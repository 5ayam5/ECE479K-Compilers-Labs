// Usage: opt -load-pass-plugin=libUnitProject.so -passes="unit-licm"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

#include "UnitLICM.h"
#include "UnitLoopInfo.h"

#define DEBUG_TYPE UnitLICM
// Define any statistics here

using namespace llvm;
using namespace ece479k;

/// Main function for running the LICM optimization
PreservedAnalyses UnitLICM::run(Function &F, FunctionAnalysisManager &FAM) {
  dbgs() << "UnitLICM running on " << F.getName() << "\n";
  // Acquires the UnitLoopInfo object constructed by your Loop Identification
  // (LoopAnalysis) pass
  UnitLoopInfo &Loops = FAM.getResult<UnitLoopAnalysis>(F);

  std::unordered_map<BasicBlock *, BasicBlock *> preheadedBlocks;
  std::unordered_set<Value *> allInstructions;
  std::unordered_set<Value *> loopInvariantInstructions;

  auto processBB = [&loopInvariantInstructions, &preheadedBlocks](
                       std::shared_ptr<Loop> L, BasicBlock *BB) -> bool {
    bool noChange = true;

    std::vector<Instruction *> toRemove;
    for (auto &I : *BB) {
      if (loopInvariantInstructions.find(&I) !=
              loopInvariantInstructions.end() ||
          isa<PHINode>(&I) || isa<CallInst>(&I))
        continue;
      bool allLoopInvariant = true;
      for (auto &Op : I.operands()) {
        if (loopInvariantInstructions.find(Op) ==
                loopInvariantInstructions.end() &&
            !isa<Constant>(Op)) {
          allLoopInvariant = false;
          break;
        }
      }
      if (allLoopInvariant) {
#ifndef NDEBUG
        dbgs() << "Moving loop invariant instruction ";
        I.print(dbgs());
        dbgs() << "\n";
#endif
        loopInvariantInstructions.insert(&I);
        toRemove.push_back(&I);
        noChange = false;
      }
    }

    for (auto I : toRemove)
      I->moveBefore(L->getPreHeader()->getTerminator());

    return noChange;
  };

  for (auto &Arg : F.args())
    allInstructions.insert(&Arg);
  for (auto &BB : F.getBasicBlockList())
    for (auto &I : BB)
      allInstructions.insert(&I);

  // Perform the optimization
  // For each loop in the function
  for (auto &[_, L] : Loops.getLoops()) {
    loopInvariantInstructions = allInstructions;
    for (auto &BB : L->getBlocks())
      for (auto &I : *BB)
        loopInvariantInstructions.erase(&I);
    bool noChange = false;
    while (!noChange) {
      noChange = true;
      for (auto &BB : L->getBlocks()) {
        if (preheadedBlocks.find(BB) != preheadedBlocks.end() &&
            preheadedBlocks[BB] != nullptr)
          noChange &= processBB(L, preheadedBlocks[BB]);
        noChange &= processBB(L, BB);
      }
    }
    L->initPreHeader();
    preheadedBlocks.insert({L->getHeader(), L->getPreHeader()});
  }

  // Set proper preserved analyses
  return PreservedAnalyses::none();
}
