cd debug && make -j
exit_code=$?
cd -
if [ $exit_code -ne 0 ]; then
  echo "Build failed"
  exit 1
fi

cd build && make -j
exit_code=$?
cd -
if [ $exit_code -ne 0 ]; then
  echo "Build failed"
  exit 1
fi

if [ $# -ne 1 ] && [ $# -ne 2 ]; then
  echo "Usage: $0 <filename>"
  echo "Usage: $0 <filename> 1 (for debug)"
  exit 1
fi

file=$1
filewithoutext=${file%.*}

debug=$2
if [ $debug -eq 1 ]; then
  lib="./debug/libLab3.dylib"
else
  lib="./build/libLab3.dylib"
fi

# echo "Optimizing $file"
# echo "Output: $filewithoutext.opt.ll"

clang "$file" -c -O0 -Xclang -disable-O0-optnone -emit-llvm -S -o - \
  | opt -load-pass-plugin=$lib -passes="function(mem2reg,instcombine,simplifycfg,adce),inline,globaldce,function(sroa,early-cse,unit-sccp,jump-threading,correlated-propagation,simplifycfg,instcombine,simplifycfg,reassociate,unit-licm,adce,simplifycfg,instcombine),globaldce" -S -o "$filewithoutext".opt.ll
