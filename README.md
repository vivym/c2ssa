# C2SSA
## 1. Compile LLVM from scrach

```bash
git clone https://github.com/llvm/llvm-project.git
cd llvm-project
mkdir build
cd build
cmake -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" -DCMAKE_BUILD_TYPE=Debug ../llvm
make -j8
```

## 2. Compile C2SSA

```bash
git clone https://github.com/vivym/c2ssa.git
cd c2ssa
mkdir build
cd build
cmake -DLLVM_ROOT_DIR=/Users/viv/Proj/llvm-project ..
make
```

## 3. Run C2SSA

```bash
./c2ssa xx.c
```
