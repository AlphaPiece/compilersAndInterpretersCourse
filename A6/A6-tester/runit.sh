#!/bin/bash

# This is a bash file to test the optimizations done in A6 
BUILD_PATH="build/......"
TEST_PATH="A6-tester/test-cases"
FILES="conds.c
queen.c
hanoi.c
"

# This does not work and I am not sure why???
# for f in $FILES
#   do
#     file="${f%.*}"
#
#     ### compile the file
#     # "${BUILD_PATH}/src/minicc" "${TEST_PATH}/${f}" -o "${file}.bc"
#     ~/mac/A6/A6/build/src/minicc  "${TEST_PATH}/${f}" -o "${file}.bc"
#
#     ### unoptimized results
#     #link minicio lib and create an executable
#     clang "${file}.bc" "${BUILD_PATH}/minicio/libminicio.a" -o $file
#
#     ### optimized results
#     # run optimization passes
#     opt -O0 -load "${BUILD_PATH}/src/liballoca2reg.so" --alloca2reg -load "${BUILD_PATH}/src/libcommonSubExprElimination.so" --commonSubExprElimination "${file}.bc" -o "${file}_opt.bc"
#     # link minicio lib and create an executable
#     clang "${file}_opt.bc" "${BUILD_PATH}/minicio/libminicio.a" -o "${file}_opt"
#
#     echo 5 | ./$file >> "${file}.txt"
#     echo 5 | ./"${file}_opt" >> "${file}_opt.txt"
#
#     # results should be equal
#     diff "${file}.txt" "${file}_opt.txt"
#   done


# So instead I ran this!

### compile the file
build/src/minicc  A6-tester/test-cases/queen.c -o queen.bc
build/src/minicc  A6-tester/test-cases/hanoi.c -o hanoi.bc
build/src/minicc  A6-tester/test-cases/conds.c -o conds.bc

### unoptimized results
#link minicio lib and create an executable
clang queen.bc build/minicio/libminicio.a -o queen
clang hanoi.bc build/minicio/libminicio.a -o hanoi
clang conds.bc build/minicio/libminicio.a -o conds

### optimized results
# run optimization passes
opt -O0 -load build/src/liballoca2reg.so --alloca2reg -load build/src/libcommonSubExprElimination.so --commonSubExprElimination queen.bc -o queen_opt.bc
# link minicio lib and create an executable
clang queen_opt.bc build/minicio/libminicio.a -o queen_opt

opt -O0 -load build/src/liballoca2reg.so --alloca2reg -load build/src/libcommonSubExprElimination.so --commonSubExprElimination hanoi.bc -o hanoi_opt.bc
# link minicio lib and create an executable
clang hanoi_opt.bc build/minicio/libminicio.a -o hanoi_opt

opt -O0 -load build/src/liballoca2reg.so --alloca2reg -load build/src/libcommonSubExprElimination.so --commonSubExprElimination conds.bc -o conds_opt.bc
# link minicio lib and create an executable
clang conds_opt.bc build/minicio/libminicio.a -o conds_opt



echo 5 | ./queen >> "queen.txt"
echo 5 | ./queen_opt >> "queen_opt.txt"

# results should be equal
diff "queen.txt" "queen_opt.txt"


echo 5 | ./hanoi >> "hanoi.txt"
echo 5 | ./hanoi_opt >> "hanoi_opt.txt"

# results should be equal
diff "hanoi.txt" "hanoi_opt.txt"


echo 5 | ./conds >> "conds.txt"
echo 5 | ./conds_opt >> "conds_opt.txt"

# results should be equal
diff "conds.txt" "conds_opt.txt"
