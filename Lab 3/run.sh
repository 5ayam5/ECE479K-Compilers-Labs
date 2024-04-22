if [ $# -ne 1 ]; then
  echo "Usage: $0 <filename>"
  exit 1
fi

file=$1
filewithoutext=${file%.*}

# echo "Optimizing $file"
# echo "Output: $filewithoutext.opt.ll"

clang "$file" -c -O0 -Xclang -disable-O0-optnone -emit-llvm -S -o - \
  | opt -load-pass-plugin=./build/libLab3.dylib -passes="function(mem2reg,instcombine,simplifycfg,adce),inline,globaldce,function(sroa,early-cse,unit-sccp,jump-threading,correlated-propagation,simplifycfg,instcombine,simplifycfg,reassociate,unit-licm,adce,simplifycfg,instcombine),globaldce" -S -o "$filewithoutext".opt.ll
