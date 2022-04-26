#!/usr/bin/python3
#
# Assignment 6 Public tester
#
# Tests a program path and against an expected output
#

import tester

# Change the local_path to the path of minicc compiler
local_path = '/home/farzaneh/mac/MiniC/MiniC-compiler-code/build/'
bcgen_path = [local_path + 'src/minicc']
llgen_path = ['llvm-dis','output.bc','-o','output.ll']
optgen_path = ['opt', '-O0','-load', local_path + 'src/liballoca2reg.so', '--alloca2reg', 'output.bc', '-o', 'output_opt.bc']
lloptgen_path = ['llvm-dis','output_opt.bc','-o','output_opt.ll']
execgen_path = ['clang', 'output_opt.bc', local_path + 'minicio/libminicio.a', '-o', 'output_opt']
run_path = ['./output_opt']

both_optgen_path = ['opt', '-O0', '-load', local_path + 'src/liballoca2reg.so', '--alloca2reg',
                  '-load', local_path + 'src/libcommonSubExprElimination.so', '--commonSubExprElimination',
                  'output.bc', '-o', 'output_both_opt.bc']
both_execgen_path = ['clang', 'output_both_opt.bc',
                     local_path + 'minicio/libminicio.a', '-o', 'output_both_opt']
both_run_path = ['./output_both_opt']

o3_optgen_path = ['opt', '-O3', 'output.bc', '-o', 'output_o3.bc']
o3_execgen_path = ['clang', 'output_o3.bc', local_path + 'minicio/libminicio.a', '-o', 'output_o3']
o3_run_path = ['./output_o3']

test_cases = [
    ['test-cases/conds.c', 0, b'None', b'0 ', 2],
    ['test-cases/queen.c', 1, b'5', b'0 2 4 1 3 \n0 3 1 4 2 \n1 3 0 2 4 \n1 4 2 0 3 \n2 0 3 1 4 \n2 4 1 3 0 \n3 0 2 4 1 \n3 1 4 2 0 \n4 1 3 0 2 \n4 2 0 3 1 \n', 3],
    ['test-cases/hanoi.c', 1, b'5\n', b'1 0 2 \n2 0 1 \n1 2 1 \n3 0 2 \n1 1 0 \n2 1 2 \n1 0 2 \n4 0 1 \n1 2 1 \n2 2 0 \n1 1 0 \n3 2 1 \n1 0 2 \n2 0 1 \n1 2 1 \n5 0 2 \n1 1 0 \n2 1 2 \n1 0 2 \n3 1 0 \n1 2 1 \n2 2 0 \n1 1 0 \n4 1 2 \n1 0 2 \n2 0 1 \n1 2 1 \n3 0 2 \n1 1 0 \n2 1 2 \n1 0 2 \n', 3],
    ['test-cases/getput1.c', 1, b'5\n', b'5 ', 1],
    ['test-cases/getput2.c', 1, b'10\n32\n', b'10 ', 2],
    ['test-cases/getput3.c', 1, b'320\n10\n', b'111 ', 2],
    ['test-cases/quicksort.c', 1, b'5\n1\n-10\n50\n100\n-1000\n', b'-1000 -10 1 50 100 ', 5],
    ['test-cases/radixsort.c', 1, b'170\n45\n75\n90\n802\n24\n2\n66 ', b'2 24 45 66 75 90 170 802 ', 5]
]
timetest_cases = [
    ['test-cases/queen_time.c', 1, b'13', None, 0],
]
tmp_files = ['output.bc','output_opt.bc','output_opt', 'output_both_opt.ll',
            'output_both_opt.bc', 'output_both_opt',
            'output.ll','output_opt.ll', 'output_o3.bc', 'output_o3']
def main():
    test = tester.Core('Assignment 6 Tester', 23)
    # tests
    for case in test_cases:
        paths = []
        paths.append(bcgen_path + [case[0]])
        paths.append(llgen_path)
        paths.append(optgen_path)
        paths.append(lloptgen_path)
        paths.append(execgen_path)
        test.generate_exec(paths)
        datain = case[2]
        #Supply the executable and test program path
        test.exec_test(run_path, datain, case[1])
        # Check the program output against a string and assign a mark
        test.check_output(case[3], 1)
        test.opt_check_output('output.ll','output_opt.ll',case[4]-1)
        test.clean_up(tmp_files)
    # Time test
    for case in timetest_cases:
        paths = []
        paths.append(bcgen_path + [case[0]])
        paths.append(llgen_path)
        paths.append(optgen_path)
        paths.append(lloptgen_path)
        paths.append(execgen_path)

        #commonSubExprElimination solution time test path
        paths.append(both_optgen_path)
        paths.append(both_execgen_path)

        #-O3 solution time test path
        paths.append(o3_optgen_path)
        paths.append(o3_execgen_path)

        test.generate_exec(paths)
        datain = case[2]
        test.exec_timetest(50,run_path, datain, case[1])

        #commonSubExprElimination time test
        test.exec_timetest(50, both_run_path, datain, case[1])

        #-O3 time test
        test.exec_timetest(50,o3_run_path, datain, case[1])

        test.opt_check_output('output.ll','output_opt.ll',0)
        test.clean_up(tmp_files)
if __name__ == '__main__':
    main()
