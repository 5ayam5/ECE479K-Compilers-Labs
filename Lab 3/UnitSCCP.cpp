// Usage: opt -load-pass-plugin=libUnitProject.so -passes="unit-sccp"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <boost/functional/hash.hpp>
#include <unordered_set>

#include "UnitSCCP.h"

#define DEBUG_TYPE UnitSCCP
// Define any statistics here

namespace ece479k {
struct pair_hash {
  template <class T1, class T2>
  std::size_t operator()(const std::pair<T1, T2> &p) const {
    auto h1 = std::hash<T1>{}(p.first);
    auto h2 = std::hash<T2>{}(p.second);

    boost::hash_combine(h1, h2);
    return h1;
  }
};
/// Main function for running the SCCP optimization
llvm::PreservedAnalyses
ece479k::UnitSCCP::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
  llvm::dbgs() << "UnitSCCP running on " << F.getName() << "\n";

  std::unordered_map<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>, bool,
                     pair_hash>
      ExecFlags;
  std::unordered_map<llvm::Value *, Lattice> LatCell;
  std::unordered_set<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>,
                     pair_hash>
      FlowWL;
  std::unordered_set<std::pair<llvm::Instruction *, llvm::Instruction *>,
                     pair_hash>
      SSAWL;
  std::unordered_set<llvm::BasicBlock *> visitedBBs;
  std::unordered_map<llvm::Value *, std::unordered_set<llvm::Instruction *>>
      DefUseMap;

  auto visitPhi = [&ExecFlags, &LatCell, &DefUseMap,
                   &SSAWL](llvm::PHINode &Phi) {
    // llvm::dbgs() << "Visiting PHI node: " << Phi << "\n";
    Lattice prevLattice = LatCell[&Phi];
    // llvm::dbgs() << "\tPrevious lattice: (" << prevLattice.value << ", "
    //              << prevLattice.constant << ")\n";
    for (auto &Op : Phi.operands())
      if (isa<llvm::Instruction>(Op)) {
        llvm::Instruction *OpI = llvm::dyn_cast<llvm::Instruction>(Op);
        if (ExecFlags[std::make_pair(OpI->getParent(), Phi.getParent())])
          LatCell[&Phi] &= LatCell[OpI];
      } else if (isa<llvm::ConstantInt>(Op))
        LatCell[&Phi] &=
            Lattice(llvm::dyn_cast<llvm::ConstantInt>(Op)->getSExtValue());
      else
        LatCell[&Phi] = Lattice(Lattice::BOTTOM);
    // llvm::dbgs() << "\tLattice: (" << LatCell[&Phi].value << ", "
    //              << LatCell[&Phi].constant << ")\n";
    if (prevLattice.value != LatCell[&Phi].value) {
      for (auto &I : DefUseMap[&Phi])
        SSAWL.insert(std::make_pair(dyn_cast<llvm::Instruction>(&Phi), I));
    }
  };

  auto visitInst = [&LatCell, &FlowWL, &SSAWL,
                    &DefUseMap](llvm::Instruction *I) {
    if (isa<llvm::CallInst>(I) || isa<llvm::InvokeInst>(I) ||
        isa<llvm::AllocaInst>(I) || isa<llvm::LoadInst>(I) ||
        isa<llvm::StoreInst>(I)) {
      LatCell[I] = Lattice(Lattice::BOTTOM);
      return;
    }
    if (I->isTerminator() &&
        !(isa<llvm::BranchInst>(I) &&
          dyn_cast<llvm::BranchInst>(I)->isConditional())) {
      for (auto Succ : successors(I->getParent()))
        FlowWL.insert(std::make_pair(I->getParent(), Succ));
      return;
    }
    // llvm::dbgs() << "Visiting instruction: " << *I << "\n";
    Lattice prevLattice = LatCell[I];
    // llvm::dbgs() << "\tPrevious lattice: (" << prevLattice.value << ", "
    //              << prevLattice.constant << ")\n";
    if (I->isBinaryOp()) {
      if ((isa<llvm::ConstantInt>(I->getOperand(0)) ||
           LatCell[I->getOperand(0)].value >= Lattice::CONSTANT) &&
          (llvm::isa<llvm::ConstantInt>(I->getOperand(1)) ||
           LatCell[I->getOperand(1)].value >= Lattice::CONSTANT)) {
        int64_t Op1 = isa<llvm::ConstantInt>(I->getOperand(0))
                          ? llvm::dyn_cast<llvm::ConstantInt>(I->getOperand(0))
                                ->getSExtValue()
                          : LatCell[I->getOperand(0)].value;
        int64_t Op2 = isa<llvm::ConstantInt>(I->getOperand(1))
                          ? llvm::dyn_cast<llvm::ConstantInt>(I->getOperand(1))
                                ->getSExtValue()
                          : LatCell[I->getOperand(1)].value;
        switch (I->getOpcode()) {
        case llvm::Instruction::Add:
          LatCell[I] = Lattice(Op1 + Op2);
          break;
        case llvm::Instruction::Sub:
          LatCell[I] = Lattice(Op1 - Op2);
          break;
        case llvm::Instruction::Mul:
          LatCell[I] = Lattice(Op1 * Op2);
          break;
        case llvm::Instruction::SDiv:
          if (Op2 != 0)
            LatCell[I] = Lattice(Op1 / Op2);
          else
            LatCell[I] = Lattice(Lattice::BOTTOM);
          break;
        case llvm::Instruction::SRem:
          if (Op2 != 0)
            LatCell[I] = Lattice(Op1 % Op2);
          else
            LatCell[I] = Lattice(Lattice::BOTTOM);
          break;
        case llvm::Instruction::And:
          LatCell[I] = Lattice(Op1 & Op2);
          break;
        case llvm::Instruction::Or:
          LatCell[I] = Lattice(Op1 | Op2);
          break;
        case llvm::Instruction::Xor:
          LatCell[I] = Lattice(Op1 ^ Op2);
          break;
        default:
          LatCell[I] = Lattice(
              Lattice::BOTTOM); // TODO: implement other binary operations
          break;
        }
      } else {
        LatCell[I] = Lattice(Lattice::BOTTOM);
      }
    } else if (I->isUnaryOp()) {
      if (isa<llvm::ConstantInt>(I->getOperand(0)) ||
          LatCell[I->getOperand(0)].value >= Lattice::CONSTANT) {
        int64_t Op1 = isa<llvm::ConstantInt>(I->getOperand(0))
                          ? llvm::dyn_cast<llvm::ConstantInt>(I->getOperand(0))
                                ->getSExtValue()
                          : LatCell[I->getOperand(0)].value;
        switch (I->getOpcode()) {
        case llvm::Instruction::FNeg:
          LatCell[I] = Lattice(-Op1);
          break;
        default:
          LatCell[I] = Lattice(
              Lattice::BOTTOM); // TODO: implement other unary operations
          break;
        }
      } else {
        LatCell[I] = Lattice(Lattice::BOTTOM);
      }
    } else if (isa<llvm::BitCastInst>(I)) {
      // TODO: Implement bitcast
      LatCell[I] = Lattice(Lattice::BOTTOM);
    } else if (isa<llvm::ICmpInst>(I)) {
      if ((isa<llvm::ConstantInt>(I->getOperand(0)) ||
           LatCell[I->getOperand(0)].value >= Lattice::CONSTANT) &&
          (isa<llvm::ConstantInt>(I->getOperand(1)) ||
           LatCell[I->getOperand(1)].value >= Lattice::CONSTANT)) {
        int64_t Op1 = llvm::isa<llvm::ConstantInt>(I->getOperand(0))
                          ? llvm::dyn_cast<llvm::ConstantInt>(I->getOperand(0))
                                ->getSExtValue()
                          : LatCell[I->getOperand(0)].value;
        int64_t Op2 = isa<llvm::ConstantInt>(I->getOperand(1))
                          ? llvm::dyn_cast<llvm::ConstantInt>(I->getOperand(1))
                                ->getSExtValue()
                          : LatCell[I->getOperand(1)].value;
        switch (llvm::dyn_cast<llvm::ICmpInst>(I)->getPredicate()) {
        case llvm::CmpInst::ICMP_EQ:
          LatCell[I] = Lattice(Op1 == Op2);
          break;
        case llvm::CmpInst::ICMP_NE:
          LatCell[I] = Lattice(Op1 != Op2);
          break;
        case llvm::CmpInst::ICMP_UGT:
        case llvm::CmpInst::ICMP_SGT:
          LatCell[I] = Lattice(Op1 > Op2);
          break;
        case llvm::CmpInst::ICMP_UGE:
        case llvm::CmpInst::ICMP_SGE:
          LatCell[I] = Lattice(Op1 >= Op2);
          break;
        case llvm::CmpInst::ICMP_ULT:
        case llvm::CmpInst::ICMP_SLT:
          LatCell[I] = Lattice(Op1 < Op2);
          break;
        case llvm::CmpInst::ICMP_ULE:
        case llvm::CmpInst::ICMP_SLE:
          LatCell[I] = Lattice(Op1 <= Op2);
          break;
        default:
          LatCell[I] = Lattice(Lattice::BOTTOM);
          break;
        }
      } else {
        LatCell[I] = Lattice(Lattice::BOTTOM);
      }
    } else if (isa<llvm::FCmpInst>(I)) {
      // TODO: Implement float constants
      LatCell[I] = Lattice(Lattice::BOTTOM);
    } else if (isa<llvm::SelectInst>(I)) {
      if (isa<llvm::ConstantInt>(I->getOperand(0)) ||
          LatCell[I->getOperand(0)].value >= Lattice::CONSTANT) {
        int64_t Op1 = isa<llvm::ConstantInt>(I->getOperand(0))
                          ? llvm::dyn_cast<llvm::ConstantInt>(I->getOperand(0))
                                ->getSExtValue()
                          : LatCell[I->getOperand(0)].value;
        if (Op1 != 0)
          LatCell[I] = LatCell[I->getOperand(1)];
        else
          LatCell[I] = LatCell[I->getOperand(2)];
      } else {
        LatCell[I] = Lattice(Lattice::BOTTOM);
      }
    } else {
      LatCell[I] = Lattice(Lattice::BOTTOM);
    }
    // llvm::dbgs() << "\tLattice: (" << LatCell[I].value << ", "
    //              << LatCell[I].constant << ")\n";
    if (prevLattice.value != LatCell[I].value) {
      if (!I->isTerminator()) {
        for (auto &U : DefUseMap[I])
          SSAWL.insert(std::make_pair(I, U));
      } else {
        if (LatCell[I].value == Lattice::BOTTOM || LatCell[I].constant == 0)
          FlowWL.insert(std::make_pair(I->getParent(), I->getSuccessor(0)));
        else if (LatCell[I].value == Lattice::BOTTOM ||
                 LatCell[I].constant != 0)
          FlowWL.insert(std::make_pair(I->getParent(), I->getSuccessor(1)));
      }
    }
  };

  // Initialize the flags and worklists
  for (auto &BB : F) {
    for (auto Succ : successors(&BB))
      ExecFlags[std::make_pair(&BB, Succ)] = false;
    for (auto &I : BB) {
      if (isa<llvm::ConstantInt>(&I))
        LatCell[&I] =
            Lattice(llvm::dyn_cast<llvm::ConstantInt>(&I)->getSExtValue());
      for (auto &Op : I.operands()) {
        DefUseMap[Op].insert(&I);
        if (isa<llvm::ConstantInt>(Op) && LatCell.find(Op) == LatCell.end())
          LatCell[Op] =
              Lattice(llvm::dyn_cast<llvm::ConstantInt>(Op)->getSExtValue());
      }
    }
  }
  FlowWL.insert(std::make_pair(nullptr, &F.getEntryBlock()));

  // Main SCCP loop
  while (!FlowWL.empty() || !SSAWL.empty()) {
    if (!FlowWL.empty()) {
      auto pair = *FlowWL.begin();
      FlowWL.erase(pair);
      if (ExecFlags[pair])
        continue;
      ExecFlags[pair] = true;
      llvm::BasicBlock *BB = pair.second;
      if (BB == nullptr)
        continue;
      for (auto &I : *BB)
        if (isa<llvm::PHINode>(I))
          visitPhi(*dyn_cast<llvm::PHINode>(&I));
      if (visitedBBs.find(BB) == visitedBBs.end()) {
        visitedBBs.insert(BB);
        for (auto &I : *BB)
          visitInst(&I);
      }
      if (BB->getUniqueSuccessor() != nullptr)
        FlowWL.insert(std::make_pair(BB, BB->getUniqueSuccessor()));
    } else {
      auto pair = *SSAWL.begin();
      SSAWL.erase(pair);
      llvm::Instruction *I = pair.second;
      if (llvm::isa<llvm::PHINode>(*I))
        visitPhi(*dyn_cast<llvm::PHINode>(I));
      else if (!isa<llvm::AllocaInst>(*I)) // TODO: Check if this is correct
        visitInst(I);
    }
  }

  for (auto [I, Lat] : LatCell) {
    if (llvm::dyn_cast<llvm::Instruction>(I) &&
        Lat.value >= Lattice::CONSTANT) {
      auto Inst = llvm::dyn_cast<llvm::Instruction>(I);
      if (Inst->isTerminator())
        continue;
#ifndef NDEBUG
      llvm::dbgs() << *I << " -> (" << Lat.value << ", " << Lat.constant
                   << ")\n";
#endif // !NDEBUG
      if (Lat.value == Lattice::CONSTANT) {
        llvm::Constant *CI = llvm::ConstantInt::get(
            I->getType(), Lat.constant, I->getType()->isIntegerTy());
        Inst->replaceAllUsesWith(CI);
        Inst->eraseFromParent();
      } else if (Lat.value == Lattice::TOP) {
        Inst->replaceAllUsesWith(llvm::UndefValue::get(I->getType()));
        Inst->eraseFromParent();
      }
    }
  }
  std::unordered_set<llvm::BasicBlock *> toDelete;
  for (auto &BB : F)
    if (visitedBBs.find(&BB) == visitedBBs.end())
      toDelete.insert(&BB);

#ifndef NDEBUG
      // for (auto BB : toDelete) {
      //   llvm::dbgs() << "Deleting basic block:\n";
      //   BB->print(llvm::dbgs());
      // }
#endif // !NDEBUG

  // for (auto BB : toDelete) {
  //   // replace the terminator with unreachable
  //   llvm::Instruction *Term = BB->getTerminator();
  //   llvm::Instruction *Unreachable =
  //       new llvm::UnreachableInst(Term->getContext());
  //   llvm::ReplaceInstWithInst(Term, Unreachable);
  // }

  // Set proper preserved analyses
  return llvm::PreservedAnalyses::none();
}
}; // namespace ece479k
