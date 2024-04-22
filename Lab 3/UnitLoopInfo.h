#ifndef INCLUDE_UNIT_LOOP_INFO_H
#define INCLUDE_UNIT_LOOP_INFO_H
#include "llvm/IR/PassManager.h"
#include <unordered_set>
#include <memory>

using namespace llvm;

namespace ece479k
{
  /// An object holding information about the (natural) loops in an LLVM
  /// function. At minimum this will need to identify the loops, may hold
  /// additional information you find useful for your LICM pass
  class Loop
  {
  public:
    Loop(BasicBlock *Header, BasicBlock *End) : Header(Header), End(End) {}

    // Add a basic block to the loop
    void addBlock(BasicBlock *BB) { Blocks.insert(BB); }
    BasicBlock *getHeader() { return Header; }
    BasicBlock *getEnd() { return End; }
    // Get the list of blocks in the loop
    std::unordered_set<BasicBlock *> &getBlocks() { return Blocks; }

  private:
    BasicBlock *Header;
    BasicBlock *End;
    std::unordered_set<BasicBlock *> Blocks;
  };

  class UnitLoopInfo
  {
  public:
    UnitLoopInfo() {}

    // Add a loop to the list of loops
    void addLoop(std::shared_ptr<Loop> L) { Loops.insert(L); }

    // Get the list of loops
    std::unordered_set<std::shared_ptr<Loop>> &getLoops() { return Loops; }

  private:
    std::unordered_set<std::shared_ptr<Loop>> Loops;
  };

  /// Loop Identification Analysis Pass. Produces a UnitLoopInfo object which
  /// should contain any information about the loops in the function which is
  /// needed for your implementation of LICM
  class UnitLoopAnalysis : public AnalysisInfoMixin<UnitLoopAnalysis>
  {
    friend AnalysisInfoMixin<UnitLoopAnalysis>;
    static AnalysisKey Key;

  public:
    typedef UnitLoopInfo Result;

    UnitLoopInfo run(Function &F, FunctionAnalysisManager &AM);
  };
} // namespace ece479k
#endif // INCLUDE_UNIT_LOOP_INFO_H
