#ifndef INCLUDE_UNIT_LOOP_INFO_H
#define INCLUDE_UNIT_LOOP_INFO_H
#include "llvm/IR/Dominators.h"
#include "llvm/IR/PassManager.h"
#include <memory>
#include <unordered_set>

using namespace llvm;

namespace ece479k {
/// An object holding information about the (natural) loops in an LLVM
/// function. At minimum this will need to identify the loops, may hold
/// additional information you find useful for your LICM pass
class Loop {
public:
  Loop(BasicBlock *Header)
      : Header(Header), PreHeader(Header->getUniquePredecessor()) {}

  void addBlock(BasicBlock *BB) {
    Blocks.push_back(BB);
    AddedBlocks.insert(BB);
  }

  void addExitBlock(BasicBlock *BB) { ExitBlocks.insert(BB); }

  bool findBlock(BasicBlock *BB) {
    return AddedBlocks.find(BB) != AddedBlocks.end();
  }

  BasicBlock *getHeader() { return Header; }

  BasicBlock *getPreHeader() {
    initPreHeader();
    return PreHeader;
  }

  void sortBlocks(DominatorTree &DT) {
    std::sort(Blocks.begin(), Blocks.end(),
              [&DT](const BasicBlock *a, const BasicBlock *b) {
                return DT.dominates(a, b);
              });
  }

  std::vector<BasicBlock *> &getBlocks() { return Blocks; }

  std::unordered_set<BasicBlock *> &getExitBlocks() { return ExitBlocks; }

private:
  void initPreHeader();

  BasicBlock *Header;
  BasicBlock *PreHeader;
  std::vector<BasicBlock *> Blocks;
  std::unordered_set<BasicBlock *> AddedBlocks;
  std::unordered_set<BasicBlock *> ExitBlocks;
};

class UnitLoopInfo {
public:
  UnitLoopInfo() {}

  std::shared_ptr<Loop> getLoop(BasicBlock *BB) {
    if (Loops.find(BB) == Loops.end()) {
      Loops.insert({BB, std::make_shared<Loop>(BB)});
      LoopList.push_back(Loops[BB]);
    }
    return Loops[BB];
  }

  std::vector<std::shared_ptr<Loop>> &getLoops() { return LoopList; }

  void sortLoops(DominatorTree &DT) {
    std::sort(
        LoopList.begin(), LoopList.end(),
        [&DT](const std::shared_ptr<Loop> a, const std::shared_ptr<Loop> b) {
          return !DT.dominates(a->getHeader(), b->getHeader());
        });
  }

private:
  std::unordered_map<BasicBlock *, std::shared_ptr<Loop>> Loops;
  std::vector<std::shared_ptr<Loop>> LoopList;
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
