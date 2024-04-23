#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "UnitLoopInfo.h"

using namespace llvm;
using namespace ece479k;

void ece479k::Loop::initPreHeader() {
  if (PreHeader != nullptr)
    return;

  // // initialize the preheader and update the predecessor of the header
  // PreHeader = BasicBlock::Create(Header->getContext(), "preheader",
  //                                Header->getParent(), Header);
  // for (auto Pred : predecessors(Header))
  //   if (Blocks.find(Pred) == Blocks.end()) {
  //     // FIXME: fix the case when multiple loops have the same header
  //     // Replace the edge from Pred to Header with an edge from Pred to
  //     // PreHeader
  //     Instruction *TI = Pred->getTerminator();
  //     for (unsigned i = 0; i < TI->getNumSuccessors(); i++)
  //       if (TI->getSuccessor(i) == Header)
  //         TI->setSuccessor(i, PreHeader);
  //   }
  // // insert a branch from the preheader to the header
  // IRBuilder<> Builder(PreHeader);
  // // Builder.CreateBr(Header);

  std::vector<BasicBlock *> externalPreds;
  for (auto Pred : predecessors(Header))
    if (!findBlock(Pred))
      externalPreds.push_back(Pred);

  PreHeader = SplitBlockPredecessors(Header, externalPreds, "preheader");
}

/// Main function for running the Loop Identification analysis. This function
/// returns information about the loops in the function via the UnitLoopInfo
/// object
UnitLoopInfo UnitLoopAnalysis::run(Function &F, FunctionAnalysisManager &FAM) {
#ifndef NDEBUG
  dbgs() << "UnitLoopAnalysis running on " << F.getName() << "\n";
  // print the CFG of the function
  dbgs() << "CFG for " << F.getName() << ":\n";
  for (auto &BB : F.getBasicBlockList()) {
    BB.printAsOperand(dbgs(), false);
    dbgs() << " -> ";
    for (auto Succ : successors(&BB)) {
      Succ->printAsOperand(dbgs(), false);
      dbgs() << " ";
    }
    dbgs() << "\n";
  }
#endif
  // Acquire the dominator tree for the function
  DominatorTree DT;
  DT.recalculate(F);
#ifndef NDEBUG
  dbgs() << "Dominator Tree for " << F.getName() << ":\n";
  DT.print(dbgs());
#endif
  // Find all the backedges in the function.
  std::vector<std::pair<BasicBlock *, BasicBlock *>> BackEdges;
  for (auto &BB : F.getBasicBlockList())
    for (auto Succ : successors(&BB))
      if (DT.dominates(Succ, &BB))
        BackEdges.push_back({&BB, Succ});
#ifndef NDEBUG
  dbgs() << "Backedges in " << F.getName() << ":\n";
  for (auto &Edge : BackEdges) {
    Edge.first->printAsOperand(dbgs(), false);
    dbgs() << " -> ";
    Edge.second->printAsOperand(dbgs(), false);
    dbgs() << "\n";
  }
#endif
  UnitLoopInfo Loops;
  // For each backedge, find the loop
  for (auto &Edge : BackEdges) {
    std::shared_ptr<Loop> L = std::make_shared<Loop>(Edge.second, Edge.first);
    // Find the loop body
    std::vector<BasicBlock *> Worklist;
    Worklist.push_back(Edge.first);
    while (!Worklist.empty()) {
      BasicBlock *BB = Worklist.back();
      Worklist.pop_back();
      L->addBlock(BB);
      for (auto Pred : predecessors(BB))
        if (DT.dominates(Edge.second, Pred) && !L->findBlock(Pred))
          Worklist.push_back(Pred);
    }
    L->sortBlocks(DT);
    Loops.addLoop(L);
  }
#ifndef NDEBUG
  // Print out the loops
  dbgs() << "Loops in " << F.getName() << ":\n";
  for (auto &L : Loops.getLoops()) {
    dbgs() << "Loop starting at ";
    L->getHeader()->printAsOperand(dbgs(), false);
    dbgs() << " ending at ";
    L->getEnd()->printAsOperand(dbgs(), false);
    dbgs() << " with blocks:\n";
    for (auto &BB : L->getBlocks()) {
      BB->printAsOperand(dbgs(), false);
      dbgs() << "\n";
    }
  }
#endif

  return Loops;
}

AnalysisKey UnitLoopAnalysis::Key;
