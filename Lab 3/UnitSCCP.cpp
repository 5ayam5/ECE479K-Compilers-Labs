// Usage: opt -load-pass-plugin=libUnitProject.so -passes="unit-sccp"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/IR/Instructions.h"
#include <unordered_set>
#include <boost/functional/hash.hpp>

#include "UnitSCCP.h"

#define DEBUG_TYPE UnitSCCP
// Define any statistics here

namespace ece479k
{
  struct pair_hash
  {
    template <class T1, class T2>
    std::size_t operator()(const std::pair<T1, T2> &p) const
    {
      auto h1 = std::hash<T1>{}(p.first);
      auto h2 = std::hash<T2>{}(p.second);

      boost::hash_combine(h1, h2);
      return h1;
    }
  };
  /// Main function for running the SCCP optimization
  llvm::PreservedAnalyses ece479k::UnitSCCP::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM)
  {
    llvm::dbgs() << "UnitSCCP running on " << F.getName() << "\n";

    std::unordered_map<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>, bool, pair_hash> ExecFlags;
    std::unordered_map<llvm::Instruction *, Lattice> LatCell;
    std::unordered_set<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>, pair_hash> FlowWL;
    std::unordered_set<std::pair<llvm::Instruction *, llvm::Instruction *>, pair_hash> SSAWL;
    std::unordered_set<llvm::BasicBlock *> visitedBBs;
    std::unordered_map<llvm::Instruction *, std::unordered_set<llvm::Instruction *>> DefUseMap;

    auto visitPhi = [&ExecFlags, &LatCell, &DefUseMap, &SSAWL](llvm::PHINode &Phi)
    {
      llvm::dbgs() << "Visiting PHI node: " << Phi << "\n";
      Lattice prevLattice = LatCell[&Phi];
      llvm::dbgs() << "\tPrevious lattice: (" << prevLattice.value << ", " << prevLattice.constant << ")\n";
      for (auto &Op : Phi.operands())
        if (isa<llvm::Instruction>(Op))
        {
          llvm::Instruction *OpI = llvm::dyn_cast<llvm::Instruction>(Op);
          if (ExecFlags[std::make_pair(OpI->getParent(), Phi.getParent())])
            LatCell[&Phi] &= LatCell[OpI];
        }
      llvm::dbgs() << "\tLattice: (" << LatCell[&Phi].value << ", " << LatCell[&Phi].constant << ")\n";
      assert(LatCell[&Phi].value != Lattice::TOP);
      if (prevLattice.value != LatCell[&Phi].value)
      {
        for (auto &I : DefUseMap[&Phi])
          SSAWL.insert(std::make_pair(dyn_cast<llvm::Instruction>(&Phi), I));
      }
    };

    auto visitInst = [&LatCell, &FlowWL, &SSAWL, &DefUseMap](llvm::Instruction *I)
    {
      llvm::dbgs() << "Visiting instruction: " << *I << "\n";
      Lattice prevLattice = LatCell[I];
      llvm::dbgs() << "\tPrevious lattice: (" << prevLattice.value << ", " << prevLattice.constant << ")\n";
      for (auto &Op : I->operands())
        if (isa<llvm::Instruction>(Op))
          LatCell[I] &= LatCell[llvm::dyn_cast<llvm::Instruction>(Op)];
      llvm::dbgs() << "\tLattice: (" << LatCell[I].value << ", " << LatCell[I].constant << ")\n";
      assert(LatCell[I].value != Lattice::TOP);
      if (prevLattice.value != LatCell[I].value)
      {
        if (!I->isTerminator())
          for (auto &U : DefUseMap[I])
            SSAWL.insert(std::make_pair(I, U));
        else
        {
          if (LatCell[I].value == Lattice::BOTTOM || LatCell[I].constant == 0)
            FlowWL.insert(std::make_pair(I->getParent(), I->getSuccessor(0)));
          else if (LatCell[I].value == Lattice::BOTTOM || LatCell[I].constant != 0)
            FlowWL.insert(std::make_pair(I->getParent(), I->getSuccessor(1)));
        }
      }
    };

    // Initialize the flags and worklists
    for (auto &BB : F)
    {
      for (auto Succ : successors(&BB))
        ExecFlags[std::make_pair(&BB, Succ)] = false;
      for (auto &I : BB)
      {
        LatCell[&I] = Lattice();
        for (auto &Op : I.operands())
          if (isa<llvm::Instruction>(Op))
            DefUseMap[llvm::dyn_cast<llvm::Instruction>(Op)].insert(&I);
      }
    }
    FlowWL.insert(std::make_pair(nullptr, &F.getEntryBlock()));

    // Main SCCP loop
    while (!FlowWL.empty() || !SSAWL.empty())
    {
      if (!FlowWL.empty())
      {
        auto pair = *FlowWL.begin();
        FlowWL.erase(pair);
        if (ExecFlags[pair])
          continue;
        ExecFlags[pair] = true;
        llvm::BasicBlock *BB = pair.second;
        for (auto &I : *BB)
          if (isa<llvm::PHINode>(I))
            visitPhi(*dyn_cast<llvm::PHINode>(&I));
        if (visitedBBs.find(BB) == visitedBBs.end())
        {
          visitedBBs.insert(BB);
          for (auto &I : *BB)
            visitInst(&I);
        }
        if (BB->getUniqueSuccessor() != nullptr)
          FlowWL.insert(std::make_pair(BB, BB->getUniqueSuccessor()));
      }
      else
      {
        auto pair = *SSAWL.begin();
        SSAWL.erase(pair);
        llvm::Instruction *I = pair.second;
        if (llvm::isa<llvm::PHINode>(*I))
          visitPhi(*dyn_cast<llvm::PHINode>(I));
        else if (!isa<llvm::AllocaInst>(*I)) // TODO: Check if this is correct
          visitInst(I);
      }
    }

    // Set proper preserved analyses
    return llvm::PreservedAnalyses::all();
  }
};
