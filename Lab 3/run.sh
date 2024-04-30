if [ $# -ne 1 ] && [ $# -ne 2 ]; then
  echo "Usage: $0 <filename>"
  echo "Usage: $0 <filename> 1 (for debug)"
  exit 1
fi

file=$1
filewithoutext=${file%.*}
fileext=${file##*.}

debug=0
if [ $# -eq 2 ]; then
  debug=$2
fi
if [ $debug -eq 1 ]; then
  cd debug && make -j > /dev/null 2>&1
  exit_code=$?
  cd - > /dev/null 2>&1
  if [ $exit_code -ne 0 ]; then
    echo "Build failed"
    exit 1
  fi
  lib="./debug/libLab3.dylib"
  # debug_flag="-debug"
else
  cd build && make -j > /dev/null 2>&1
  exit_code=$?
  cd - > /dev/null 2>&1
  if [ $exit_code -ne 0 ]; then
    echo "Build failed"
    exit 1
  fi
  lib="./build/libLab3.dylib"
  debug_flag=""
fi

# echo "Optimizing $file"
# echo "Output: $filewithoutext.opt.ll"
if [ $fileext == "cpp" ] || [ $fileext == "c" ]; then
  clang "$file" -c -O0 -Xclang -disable-O0-optnone -emit-llvm -S -o "$filewithoutext".ll
  opt -load-pass-plugin=$lib $debug_flag -passes="function(mem2reg,instcombine,simplifycfg,adce),inline,globaldce,function(sroa,early-cse,unit-sccp,jump-threading,correlated-propagation,simplifycfg,instcombine,simplifycfg,reassociate,unit-licm,adce,simplifycfg,instcombine),globaldce" "$filewithoutext".ll -S -o "$filewithoutext".ll.opt
  # opt -load-pass-plugin=$lib $debug_flag -passes="function(mem2reg,instcombine,simplifycfg,adce),inline,globaldce,function(sroa,early-cse)" "$filewithoutext".ll -S -o "$filewithoutext".ll.presscp
  # opt -load-pass-plugin=$lib $debug_flag -passes="function(mem2reg,instcombine,simplifycfg,adce),inline,globaldce,function(sroa,early-cse,unit-sccp,jump-threading,correlated-propagation,simplifycfg,instcombine,simplifycfg,reassociate)" "$filewithoutext".ll -S -o "$filewithoutext".ll.prelicm
elif [ $fileext == "ll" ]; then
  opt -load-pass-plugin=$lib $debug_flag -passes="function(mem2reg,instcombine,simplifycfg,adce),inline,globaldce,function(sroa,early-cse,unit-sccp,jump-threading,correlated-propagation,simplifycfg,instcombine,simplifycfg,reassociate,unit-licm,adce,simplifycfg,instcombine),globaldce" "$file" -S -o "$filewithoutext".ll.opt
else
  echo "Invalid file extension"
  exit 1
fi

