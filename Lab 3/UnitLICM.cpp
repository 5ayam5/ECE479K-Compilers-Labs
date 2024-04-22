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

  // Perform the optimization
  // For each loop in the function
  for (auto &L : Loops.getLoops())
  {
    // For each block in the loop
    for (auto &BB : L->getBlocks())
    {
      // For each instruction in the block
      for (auto &I : *BB)
      {
        // // Check if each operand is loop invariant
        // bool allLoopInvariant = true;
        // for (auto &Op : I.operands())
        // {
        //   if (Instruction *OpI = dyn_cast<Instruction>(Op))
        //   {
        //     if (L->getBlocks().count(OpI->getParent()))
        //     {
        //       allLoopInvariant = false;
        //       break;
        //     }
        //   }
        // }
        // // If all operands are loop invariant, move the instruction to the loop
        // // header
        // if (allLoopInvariant)
        // {
        //   I.moveBefore(L->getHeader()->getFirstNonPHI());
        // }
      }
    }
  }

  // Set proper preserved analyses
  return PreservedAnalyses::all();
}
