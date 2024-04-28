#ifndef INCLUDE_UNIT_SCCP_H
#define INCLUDE_UNIT_SCCP_H
#include "llvm/IR/PassManager.h"
#include <boost/functional/hash.hpp>

namespace ece479k
{
  using boost::hash;
  // using boost::hash<std::pair<Instruction *, Instruction *>>;
  /// Sparse Conditional Constant Propagation Optimization Pass
  struct Lattice
  {
    enum Value
    {
      BOTTOM,
      CONSTANT,
      TOP
    } value;
    int constant;
    Lattice() : value(TOP), constant(0) {}
    Lattice(Value v) : value(v), constant(0) {}
    Lattice(Value v, int c) : value(v), constant(c) {}
    Lattice operator&(const Lattice &other) const
    {
      if (value == BOTTOM || other.value == BOTTOM)
        return Lattice(BOTTOM);
      if (value == CONSTANT && other.value == CONSTANT && constant == other.constant)
        return Lattice(CONSTANT, constant);
      return Lattice(TOP);
    }
    Lattice operator&=(const Lattice &other)
    {
      return *this = *this & other;
    }
  };
  struct UnitSCCP : llvm::PassInfoMixin<UnitSCCP>
  {
    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
  };
} // namespace

#endif // INCLUDE_UNIT_SCCP_H
