#ifndef INCLUDE_UNIT_SCCP_H
#define INCLUDE_UNIT_SCCP_H
#include "llvm/IR/PassManager.h"
#include <boost/functional/hash.hpp>

namespace ece479k {
using boost::hash;
// using boost::hash<std::pair<Instruction *, Instruction *>>;
/// Sparse Conditional Constant Propagation Optimization Pass
struct Lattice {
  enum Value { BOTTOM, CONSTANT, TOP } value;
  int64_t constant;
  Lattice() : value(TOP), constant(0) {}
  Lattice(Value v) : value(v), constant(0) {}
  Lattice(int c) : value(CONSTANT), constant(c) {}
  Lattice operator&(const Lattice &other) const {
    if (value < other.value)
      return *this;
    if (other.value < value)
      return other;
    if (value == CONSTANT) {
      if (constant == other.constant)
        return *this;
      else
        return Lattice(BOTTOM);
    }
    return *this;
  }
  Lattice operator&=(const Lattice &other) { return *this = *this & other; }
};
struct UnitSCCP : llvm::PassInfoMixin<UnitSCCP> {
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};
} // namespace ece479k

#endif // INCLUDE_UNIT_SCCP_H
