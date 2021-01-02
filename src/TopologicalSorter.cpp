#include "TopologicalSorter.h"

namespace c2ssa {

void TopologicalSorter::addEdge(int Start, int End) {
  AdjacencyMatrix[Start].push_back(End);
}

TopologicalSorter::TopologicalSorter(int Size) {
  this->Size = Size;
  AdjacencyMatrix.resize(Size);
  Marks.resize(Size, Mark::Unvisited);
  Result.reserve(Size);
}

llvm::Optional<std::vector<int>> TopologicalSorter::sort() {
  for (int I = 0; I < Size; ++I) {
    if (visit(I)) {
      return llvm::None;
    }
  }
  return Result;
}

bool TopologicalSorter::visit(int Node) {
  if (Marks[Node] == Mark::Permanent) {
    return false;
  }
  if (Marks[Node] == Mark::Temp) { // Cycle
    return true;
  }
  Marks[Node] = Mark::Temp;
  for (const int I : AdjacencyMatrix[Node]) {
    if (visit(I)) {
      return true;
    }
  }
  Marks[Node] = Mark::Permanent;
  Result.push_back(Node);
  return false;
}

} // namespace c2ssa
