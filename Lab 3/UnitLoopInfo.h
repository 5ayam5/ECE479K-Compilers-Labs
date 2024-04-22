#ifndef INCLUDE_UNIT_LOOP_INFO_H
#define INCLUDE_UNIT_LOOP_INFO_H
#include "llvm/IR/PassManager.h"

using namespace llvm;

namespace ece479k {
/// An object holding information about the (natural) loops in an LLVM
/// function. At minimum this will need to identify the loops, may hold
/// additional information you find useful for your LICM pass
class UnitLoopInfo {
public:
  class Loop {};

  UnitLoopInfo() {}

  // Add a loop to the list of loops
  void addLoop(Loop *L) { Loops.push_back(L); }

  // Get the list of loops
  std::vector<Loop *> &getLoops() { return Loops; }

private:
  std::vector<Loop *> Loops;
};

/// Loop Identification Analysis Pass. Produces a UnitLoopInfo object which
/// should contain any information about the loops in the function which is
/// needed for your implementation of LICM
class UnitLoopAnalysis : public AnalysisInfoMixin<UnitLoopAnalysis> {
  friend AnalysisInfoMixin<UnitLoopAnalysis>;
  static AnalysisKey Key;

public:
  typedef UnitLoopInfo Result;

  UnitLoopInfo run(Function &F, FunctionAnalysisManager &AM);
};
} // namespace ece479k
#endif // INCLUDE_UNIT_LOOP_INFO_H
