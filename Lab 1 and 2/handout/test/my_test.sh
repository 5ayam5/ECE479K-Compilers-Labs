dir=$1

for file in $dir/*; do
    # if file extension is .cl
    if [[ $file == *.cl ]]; then
        # get the file name without the extension
        filename=$(basename -- "$file")
        filename="${filename%.*}"
        # echo "Testing $filename"
        # run the file
        make $dir/$filename.out > /dev/null 2>&1
        if [ $? -ne 0 ]; then
            echo "Test failed build: $filename"
            continue
        fi
        # compare the output with the expected output
        /coolc/cool $dir/$filename.cl | diff $dir/$filename.out - > /dev/null 2>&1
        # if the output is different, print the file name
        if [ $? -ne 0 ]; then
            echo "Test failed: $filename"
        fi
    fi
done