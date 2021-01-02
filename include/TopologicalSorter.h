#ifndef C2SSA_TOPOLOGICALSORTER_H
#define C2SSA_TOPOLOGICALSORTER_H

#include <llvm/ADT/Optional.h>
#include <vector>

namespace c2ssa {

class TopologicalSorter {
  int Size;
  std::vector<std::vector<int>> AdjacencyMatrix;
  enum class Mark {
    Unvisited,
    Temp,
    Permanent,
  };
  std::vector<Mark> Marks;
  std::vector<int> Result;
  bool visit(int Node); // Returns true on cycle detection

public:
  explicit TopologicalSorter(int Size);

  void addEdge(int Start, int End);
  llvm::Optional<std::vector<int>> sort(); // Returns None if there are cycles
};

} // namespace c2ssa

#endif // C2SSA_TOPOLOGICALSORTER_H
