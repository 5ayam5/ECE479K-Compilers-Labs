if [[ $# -ne 1 ]]; then
  echo "Usage : $0 <dir>"
  exit 1
fi

dir=$1

for file in $(ls $dir); do
  if [[ -f $dir/$file ]]; then
    fileext=${file##*.}
    if [[ $fileext != "cpp" ]] && [[ $fileext != "c" ]]; then
      continue
    fi
    echo $file
    ./run.sh $dir/$file 2> /dev/null
  fi
done
