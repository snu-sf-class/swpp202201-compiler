#include "register_allocate.h"

#include "analysis.h"

#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

#include <map>
#include <queue>

namespace {
using namespace sc::backend;

constexpr unsigned int MAX_ARGUMENT = 16U;
constexpr unsigned int MAX_REGISTER = 32U;
constexpr unsigned int LOAD_COST = 6U;
constexpr unsigned int STORE_COST = 6U;
constexpr unsigned int UNKNOWN_LOOP_CNT = 100U;

void PostOrderRegCollect(llvm::BasicBlock &BB,
                         std::vector<llvm::Instruction *> &insts,
                         std::set<llvm::BasicBlock *> &visit) {
  visit.insert(&BB);
  llvm::Instruction *term = BB.getTerminator();
  unsigned int num = term->getNumSuccessors();
  for (unsigned int i = 0U; i < num; i++) {
    llvm::BasicBlock *next = term->getSuccessor(i);
    if (!visit.count(next))
      PostOrderRegCollect(*next, insts, visit);
  }
  for (auto it = BB.rbegin(); it != BB.rend(); it++) {
    llvm::Instruction &I = *it;
    insts.emplace_back(&I);
  }
}

void makeInterferenceGraph(
    llvm::Function &F,
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph) {
  std::vector<llvm::Instruction *> insts; // post-order DFS-traversal
  std::set<llvm::BasicBlock *> visit;
  std::map<llvm::Instruction *, std::set<llvm::Instruction *>> in, out;

  PostOrderRegCollect(F.getEntryBlock(), insts, visit);

  for (llvm::Instruction *I : insts) {
    in[I] = {};
    out[I] = {};
    for (llvm::Value *oper : I->operand_values()) {
      llvm::Instruction *inst = llvm::dyn_cast<llvm::Instruction>(oper);
      if (inst)
        in[I].insert(inst);
    }
  }
  bool updated = true;
  while (updated) {
    updated = false;
    for (llvm::Instruction *I : insts) {
      size_t before = out[I].size();
      if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I)) {
        llvm::Instruction *nonPhi = phi->getParent()->getFirstNonPHI();
        out[I].insert(in[nonPhi].begin(), in[nonPhi].end());
      } else if (llvm::Instruction *next = I->getNextNode()) {
        out[I].insert(in[next].begin(), in[next].end());
      } else {
        unsigned int num = I->getNumSuccessors();
        for (unsigned int i = 0U; i < num; i++) {
          llvm::BasicBlock *succ = I->getSuccessor(i);
          llvm::Instruction *nonPhi = succ->getFirstNonPHI();
          std::set<llvm::Instruction *> temp = in[nonPhi];
          for (llvm::PHINode &succPhi : succ->phis()) {
            temp.erase(&succPhi);
            llvm::Value *v = succPhi.getIncomingValueForBlock(I->getParent());
            if (v) {
              llvm::Instruction *mine = llvm::dyn_cast<llvm::Instruction>(v);
              assert(mine && "It should be an instruction.");
              temp.insert(mine);
            }
          }
          out[I].insert(temp.begin(), temp.end());
        }
      }
      size_t after = out[I].size();
      updated = updated || (before != after);
      before = in[I].size();
      if (out[I].count(I)) {
        out[I].erase(I);
        in[I].insert(out[I].begin(), out[I].end());
        out[I].insert(I);
      } else {
        in[I].insert(out[I].begin(), out[I].end());
      }
      after = in[I].size();
      updated = updated || (before != after);
    }
  }
  for (llvm::Instruction *I : insts)
    if (analysis::isReg(I))
      inter_graph[I] = {};
  for (llvm::Instruction *I : insts)
    if (analysis::isReg(I))
      for (llvm::Instruction *J : out[I])
        if (I != J && analysis::isReg(J)) {
          inter_graph[I].insert(J);
          inter_graph[J].insert(I);
        }
}

void coalesceMovInsts(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph) {
  std::vector<llvm::Instruction *> trash;

  for (auto &[I, S] : inter_graph) {
    llvm::Value *v = I;
    while (llvm::Instruction *inst = analysis::isMoveInst(v))
      v = inst->getOperand(0);
    llvm::Instruction *p = llvm::dyn_cast<llvm::Instruction>(v);
    assert(p && "p should be instruction.");
    if (p != I) {
      inter_graph[p].insert(S.begin(), S.end());
      for (llvm::Instruction *other : S)
        inter_graph[other].insert(p);
      trash.emplace_back(I);
    }
  }

  for (llvm::Instruction *I : trash)
    inter_graph.erase(I);
  for (auto &[I, S] : inter_graph) {
    for (llvm::Instruction *other : trash)
      S.erase(other);
    S.erase(I);
  }
}

bool resolvePHIInterference(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph,
    llvm::IntegerType *Int64Ty) {
  for (auto &[I, S] : inter_graph)
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I)) {
      std::vector<llvm::Instruction *> parent;
      std::set<llvm::Instruction *> inter = S;
      for (int i = 0; i < phi->getNumIncomingValues(); i++) {
        llvm::Value *v = phi->getIncomingValue(i);
        while (llvm::Instruction *next = analysis::isMoveInst(v))
          v = next->getOperand(0);
        llvm::Instruction *p = llvm::dyn_cast<llvm::Instruction>(v);
        assert(p && "It should be an instruction.");
        parent.push_back(p);
        inter.insert(inter_graph[p].cbegin(), inter_graph[p].cend());
      }
      size_t size = parent.size();
      for (int i = 0; i < size; i++)
        if (inter.count(parent[i]) &&
            parent[i]->getNextNode() !=
                parent[i]->getParent()->getTerminator()) {
          llvm::Instruction *t = phi->getIncomingBlock(i)->getTerminator();
          llvm::Value *v = phi->getIncomingValue(i);
          llvm::Type *type = v->getType();
          if (!type->isIntegerTy())
            v = llvm::CastInst::CreateBitOrPointerCast(v, Int64Ty, "", t);
          v = llvm::BinaryOperator::CreateMul(
              v, llvm::ConstantInt::get(v->getType(), 1UL, true), "", t);
          if (!type->isIntegerTy())
            v = llvm::CastInst::CreateBitOrPointerCast(v, type, "", t);
          phi->setIncomingValue(i, v);
          return false;
        }
    }
  return true;
}

void coalescePHINodes(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph) {
  std::vector<llvm::Instruction *> trash;

  for (auto &[I, S] : inter_graph)
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I))
      for (int i = 0; i < phi->getNumIncomingValues(); i++) {
        llvm::Value *v = phi->getIncomingValue(i);
        while (llvm::Instruction *next = analysis::isMoveInst(v))
          v = next->getOperand(0);
        llvm::Instruction *V = llvm::dyn_cast<llvm::Instruction>(v);
        assert(V && "operand of phi should be an instruction.");
        inter_graph[phi].insert(inter_graph[V].begin(), inter_graph[V].end());
        for (llvm::Instruction *other : inter_graph[V])
          inter_graph[other].insert(phi);
        trash.emplace_back(V);
      }
  for (llvm::Instruction *I : trash)
    inter_graph.erase(I);
  for (auto &[I, S] : inter_graph) {
    for (llvm::Instruction *other : trash)
      S.erase(other);
    S.erase(I);
  }
}

void PerfectEliminationOrdering(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>>
        inter_graph, // should be copied
    std::vector<llvm::Instruction *> &order) {
  std::map<llvm::Instruction *, int> C;
  std::queue<llvm::Instruction *> Q;
  std::set<llvm::Instruction *> visit;
  for (auto &[I, S] : inter_graph) {
    size_t size = S.size();
    C[I] = size * (size - 1) / 2;
    std::vector<llvm::Instruction *> neighbor;
    for (llvm::Instruction *X : S)
      neighbor.emplace_back(X);
    for (int i = 0; i < size; i++)
      for (int j = i + 1; j < size; j++)
        C[I] -= inter_graph[neighbor[i]].count(neighbor[j]);
    if (!C[I]) {
      Q.push(I);
      visit.insert(I);
    }
  }
  while (!Q.empty()) {
    llvm::Instruction *v = Q.front();
    order.emplace_back(v);
    Q.pop();
    size_t size = inter_graph[v].size();
    for (llvm::Instruction *w : inter_graph[v]) {
      inter_graph[w].erase(v);
      C[w] -= inter_graph[w].size() - (size - 1);
      if (!C[w] && !visit.count(w)) {
        visit.insert(w);
        Q.push(w);
      }
    }
  }
  if (order.size() != inter_graph.size()) {
    order.clear();
    for (auto &[I, S] : inter_graph)
      order.emplace_back(I);
  }
}

int GreedyColoring(
    std::map<llvm::Instruction *, std::set<llvm::Instruction *>> &inter_graph,
    std::vector<llvm::Instruction *> &order,
    std::map<llvm::Instruction *, int> &color) {
  std::set<int> color_used;
  int num_colors = 0;
  for (llvm::Instruction *I : order) {
    color_used.clear();
    for (llvm::Instruction *J : inter_graph[I])
      if (color.count(J))
        color_used.insert(color[J]);
    int i = 1;
    while (color_used.count(i))
      i++;
    color[I] = i;
    num_colors = std::max(num_colors, i);
  }
  return num_colors;
}

void recursivelyInsertSymbols(symbol::SymbolMap *SM,
                              std::map<llvm::Instruction *, int> &color,
                              llvm::Value *V) {
  if (SM->getSymbol(V))
    return;
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    SM->addSymbol(C, symbol::Symbol::createConstantSymbol(C->getZExtValue()));
    return;
  }
  if (llvm::isa<llvm::ConstantPointerNull>(V) || llvm::isa<llvm::UndefValue>(V)) {
    SM->addSymbol(V, symbol::Symbol::createConstantSymbol(0UL));
    return;
  }
  llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(V);
  if (CI && CI->getCalledFunction()->getName().str() == "$decr_sp") {
    SM->addSymbol(CI, symbol::Symbol::createStackPtrSymbol());
    return;
  }
  llvm::Instruction *I = llvm::dyn_cast<llvm::Instruction>(V);
  assert(I && "It should be an instruction.");
  if (color.count(I))
    SM->addSymbol(I, symbol::Symbol::createRegisterSymbol(color[I]));
  for (llvm::Value *use : I->operand_values())
    recursivelyInsertSymbols(SM, color, use);
  if (llvm::Instruction *MV = analysis::isMoveInst(I)) {
    const symbol::Symbol *sym = SM->getSymbol(MV->getOperand(0));
    std::string &&name = sym->getName();
    switch (name.at(0)) {
    case 's':
      SM->addSymbol(I, symbol::Symbol::createStackPtrSymbol());
      break;
    case 'r':
      SM->addSymbol(I, symbol::Symbol::createRegisterSymbol(std::move(name)));
      break;
    case 'a':
      SM->addSymbol(I, symbol::Symbol::createArgumentSymbol(std::move(name)));
      break;
    default:
      SM->addSymbol(I, symbol::Symbol::createConstantSymbol(std::move(name)));
      break;
    }
  }
}

void propagatePHINodeColors(std::map<llvm::Instruction *, int> &color) {
  std::vector<std::pair<llvm::Instruction *, int>> buff;

  for (auto &[I, c] : color)
    if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(I))
      for (llvm::Use &use : phi->incoming_values()) {
        llvm::Value *v = use.get();
        while (llvm::Instruction *mv = analysis::isMoveInst(v))
          v = mv->getOperand(0);
        llvm::Instruction *i = llvm::dyn_cast<llvm::Instruction>(v);
        buff.emplace_back(i, c);
      }
  for (auto [I, c] : buff)
    color[I] = c;
}

void insertSymbols(symbol::SymbolMap *SM, llvm::Function &F,
                   std::map<llvm::Instruction *, int> &color) {
  int i = 1;
  for (llvm::Argument &arg : F.args()) {
    assert(i <= MAX_ARGUMENT && "Too many arguments.");
    SM->addSymbol(&arg, symbol::Symbol::createArgumentSymbol(i++));
  }
  i = 1;
  for (llvm::BasicBlock &BB : F)
    SM->addSymbol(&BB, symbol::Symbol::createBasicBlockLabelSymbol(
                           BB.hasName() ? BB.getName().str()
                                        : "_default." + std::to_string(i++)));
  /* You should not put below inside BasicBlock loop, should be run separately.
   */
  for (llvm::BasicBlock &BB : F)
    for (llvm::Instruction &I : BB)
      recursivelyInsertSymbols(SM, color, &I);
}

void insertLoadStore(std::vector<llvm::Instruction *> &insts,
                     llvm::CallInst *SP, llvm::IntegerType *Int64Ty,
                     llvm::PointerType *Int64PtrTy) {
  std::vector<llvm::Instruction *> stores;
  std::vector<llvm::Use *> loads;

  uint64_t acc =
      llvm::dyn_cast<llvm::ConstantInt>(SP->getArgOperand(0))->getZExtValue();
  SP->setArgOperand(0, llvm::ConstantInt::get(Int64Ty, acc + 8, true));

  for (llvm::Instruction *I : insts) {
    if (!analysis::isMoveInst(I) && !llvm::isa<llvm::PHINode>(I))
      stores.emplace_back(I);
    for (llvm::Use &use : I->uses()) {
      llvm::Instruction *user =
          llvm::dyn_cast<llvm::Instruction>(use.getUser());
      assert(user && "It should be an instruction.");
      if (!analysis::isMoveInst(user) && !llvm::isa<llvm::PHINode>(user))
        loads.emplace_back(&use);
    }
  }
  for (llvm::Instruction *I : stores) {
    llvm::Instruction *V = I;
    llvm::Instruction *next = V->getNextNode();
    llvm::Instruction *ptr = SP;
    if (acc)
      ptr = llvm::BinaryOperator::CreateAdd(
          ptr, llvm::ConstantInt::get(Int64Ty, acc, true), "", next);
    ptr = llvm::CastInst::CreateBitOrPointerCast(ptr, Int64PtrTy, "", next);
    llvm::Type *type = V->getType();
    if (type->isIntegerTy()) {
      if (!type->isIntegerTy(64))
        V = llvm::CastInst::CreateIntegerCast(V, Int64Ty, false, "", next);
    } else {
      V = llvm::CastInst::CreateBitOrPointerCast(V, Int64Ty, "", next);
    }
    llvm::StoreInst *SI = new llvm::StoreInst(V, ptr, next);
  }
  for (llvm::Use *use : loads) {
    llvm::Instruction *user = llvm::dyn_cast<llvm::Instruction>(use->getUser());
    assert(user && "It should be an instruction.");
    llvm::Instruction *ptr = SP;
    if (acc)
      ptr = llvm::BinaryOperator::CreateAdd(
          ptr, llvm::ConstantInt::get(Int64Ty, acc, true), "", user);
    ptr = llvm::CastInst::CreateBitOrPointerCast(ptr, Int64PtrTy, "", user);
    llvm::LoadInst *LI = new llvm::LoadInst(Int64Ty, ptr, "", user);
    llvm::Instruction *V = LI;
    llvm::Type *type = use->get()->getType();
    if (type->isIntegerTy()) {
      if (!type->isIntegerTy(64U))
        V = llvm::CastInst::CreateIntegerCast(V, type, false, "", user);
    } else {
      V = llvm::CastInst::CreateBitOrPointerCast(V, type, "", user);
    }
    use->set(V);
  }
}

void spillColors(llvm::Function &F, int num_colors,
                 std::map<llvm::Instruction *, int> &color,
                 llvm::FunctionCallee &decr_sp, llvm::IntegerType *Int64Ty,
                 llvm::PointerType *Int64PtrTy) {
  llvm::DominatorTree DT(F);
  llvm::LoopInfo LI(DT);
  llvm::TargetLibraryInfoImpl TLIImpl;
  llvm::TargetLibraryInfo TLI(TLIImpl, &F);
  llvm::AssumptionCache AC(F);
  llvm::ScalarEvolution SCE(F, TLI, AC, DT, LI);

  std::vector<std::vector<llvm::Instruction *>> color2inst(
      num_colors + 1, std::vector<llvm::Instruction *>());
  int min_cost = 0x7fffffff, min_color;

  for (llvm::BasicBlock &BB : F)
    for (llvm::Instruction &I : BB)
      if (analysis::isReg(&I)) {
        llvm::Value *V = &I;
        while (llvm::Instruction *inst = analysis::isMoveInst(V))
          V = inst->getOperand(0);
        llvm::Instruction *P = llvm::dyn_cast<llvm::Instruction>(V);
        color2inst[color[P]].emplace_back(&I);
      }

  for (int i = 1; i <= num_colors; i++) {
    int cost = 0;
    for (llvm::Instruction *I : color2inst[i]) {
      if (!analysis::isMoveInst(I) && !llvm::isa<llvm::PHINode>(I)) {
        unsigned int cnt = 1U;
        if (llvm::Loop *L = LI.getLoopFor(I->getParent())) {
          cnt = SCE.getSmallConstantTripCount(L);
          if (!cnt)
            cnt = UNKNOWN_LOOP_CNT;
        }
        cost += STORE_COST * cnt;
      }
      for (llvm::User *user : I->users()) {
        llvm::Instruction *I = llvm::dyn_cast<llvm::Instruction>(user);
        assert(I);
        if (!analysis::isMoveInst(I) && !llvm::isa<llvm::PHINode>(I)) {
          unsigned int cnt = 1U;
          if (llvm::Loop *L = LI.getLoopFor(I->getParent())) {
            cnt = SCE.getSmallConstantTripCount(L);
            if (!cnt)
              cnt = UNKNOWN_LOOP_CNT;
          }
          cost += LOAD_COST * cnt;
        }
      }
    }
    if (min_cost > cost) {
      min_cost = cost;
      min_color = i;
    }
  }

  llvm::BasicBlock &EB = F.getEntryBlock();
  llvm::CallInst *SP = nullptr;
  for (llvm::Instruction &I : EB) {
    if (llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(&I))
      if (CI->getCalledFunction()->getName().equals("$decr_sp")) {
        SP = CI;
        break;
      }
  }
  if (!SP) {
    llvm::Instruction *FI = F.getEntryBlock().getFirstNonPHI();
    llvm::ConstantInt *Const = llvm::ConstantInt::get(Int64Ty, 0UL, true);
    llvm::Value *Args[] = {Const};
    SP = llvm::CallInst::Create(decr_sp, llvm::ArrayRef<llvm::Value *>(Args),
                                "", FI);
  }

  insertLoadStore(color2inst[min_color], SP, Int64Ty, Int64PtrTy);
}
} // namespace

namespace sc::backend::reg_alloc {
llvm::PreservedAnalyses
RegisterAllocatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  std::map<llvm::Instruction *, std::set<llvm::Instruction *>> inter_graph;
  std::vector<llvm::Instruction *> order;
  std::map<llvm::Instruction *, int> color;
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  llvm::PointerType *Int64PtrTy = llvm::Type::getInt64PtrTy(M.getContext());
  llvm::FunctionCallee decr_sp =
      M.getOrInsertFunction("$decr_sp", Int64Ty, Int64Ty);

  for (llvm::Function &F : M)
    SM->addSymbol(&F,
                  symbol::Symbol::createFunctionNameSymbol(F.getName().str()));

  for (llvm::Function &F : M) {
    if (F.isDeclaration())
      continue;
    while (true) {
      while (true) {
        inter_graph.clear();
        makeInterferenceGraph(F, inter_graph);
        coalesceMovInsts(inter_graph);
        if (resolvePHIInterference(inter_graph, Int64Ty))
          break;
      }
      coalescePHINodes(inter_graph);
      order.clear();
      color.clear();
      PerfectEliminationOrdering(inter_graph, order);
      std::reverse(order.begin(), order.end());
      int num_colors = GreedyColoring(inter_graph, order, color);
      propagatePHINodeColors(color);
      if (num_colors <= MAX_REGISTER)
        break;
      spillColors(F, num_colors, color, decr_sp, Int64Ty, Int64PtrTy);
    }
    insertSymbols(SM, F, color);
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::reg_alloc
