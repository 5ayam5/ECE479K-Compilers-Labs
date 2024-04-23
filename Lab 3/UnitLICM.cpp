// Usage: opt -load-pass-plugin=libUnitProject.so -passes="unit-licm"
#include "llvm/Support/raw_ostream.h"

#include "UnitLICM.h"
#include "UnitLoopInfo.h"

#define DEBUG_TYPE UnitLICM
// Define any statistics here

using namespace llvm;
using namespace ece479k;

/// Main function for running the LICM optimization
PreservedAnalyses UnitLICM::run(Function &F, FunctionAnalysisManager &FAM)
{
  dbgs() << "UnitLICM running on " << F.getName() << "\n";
  // Acquires the UnitLoopInfo object constructed by your Loop Identification
  // (LoopAnalysis) pass
  UnitLoopInfo &Loops = FAM.getResult<UnitLoopAnalysis>(F);

  std::unordered_map<BasicBlock *, BasicBlock *> preheadedBlocks;
  std::unordered_set<Value *> allInstructions;
  std::unordered_set<Value *> loopInvariantInstructions;

  auto processBB = [&loopInvariantInstructions, &preheadedBlocks](std::shared_ptr<Loop> L, BasicBlock *BB) -> bool
  {
    for (auto &I : loopInvariantInstructions)
    {
      dbgs() << "Loop invariant instruction ";
      I->print(dbgs());
      dbgs() << "\n";
    }
    bool noChange = true;

    for (auto &I : *BB)
    {
      // Check if each operand is loop invariant
      bool allLoopInvariant = true;
      dbgs() << "Instruction ";
      I.print(dbgs());
      dbgs() << '\n';
      for (auto &Op : I.operands())
      {
        dbgs() << "\tChecking operand ";
        Op->print(dbgs());
        dbgs() << " " << isa<Constant>(Op) << " " << (loopInvariantInstructions.find(dyn_cast<Instruction>(Op)) == loopInvariantInstructions.end());
        dbgs() << "\n";
        if (loopInvariantInstructions.find(dyn_cast<Instruction>(Op)) == loopInvariantInstructions.end() &&
            !isa<Constant>(Op))
        {
          allLoopInvariant = false;
          break;
        }
      }
      dbgs() << "\tis loop invariant: " << allLoopInvariant << "\n";
      // If all operands are loop invariant, move the instruction to the loop
      // header
      if (allLoopInvariant)
      {
        BasicBlock *preHeader = L->getPreHeader();
#ifndef NDEBUG
        dbgs() << "Moving instruction ";
        I.print(dbgs());
        dbgs() << " to preheader from ";
        BB->printAsOperand(dbgs(), false);
        dbgs() << "\n";
#endif
        I.moveBefore(preHeader->getTerminator());
        loopInvariantInstructions.insert(&I);
        if (preheadedBlocks.find(L->getHeader()) == preheadedBlocks.end())
          preheadedBlocks[L->getHeader()] = preHeader;
        dbgs() << "Updated preheader:";
        preHeader->printAsOperand(dbgs(), false);
        dbgs() << "\n";
        for (auto &I : *preHeader)
        {
          dbgs() << "\tInstruction ";
          I.print(dbgs());
          dbgs() << '\n';
        }
        noChange = false;
      }
    }

    return noChange;
  };

  for (auto &Arg : F.args())
    allInstructions.insert(&Arg);
  for (auto &BB : F.getBasicBlockList())
    for (auto &I : BB)
      allInstructions.insert(&I);

  // Perform the optimization
  // For each loop in the function
  for (auto &L : Loops.getLoops())
  {
    loopInvariantInstructions = allInstructions;
    for (auto &BB : L->getBlocks())
      for (auto &I : *BB)
        loopInvariantInstructions.erase(&I);
    bool noChange = false;
    while (!noChange)
    {
      noChange = true;
      for (auto &BB : L->getBlocks())
      {
        if (preheadedBlocks.find(BB) != preheadedBlocks.end())
          noChange &= processBB(L, preheadedBlocks[BB]);
        noChange &= processBB(L, BB);
      }
    }
  }

  // Set proper preserved analyses
  return PreservedAnalyses::none();
}
