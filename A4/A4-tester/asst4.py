#!/usr/bin/python3
#
# Assignment 4 Public tester
#
# Tests a program path and against an expected output
#

import tester
import re

# Change path to minicc here
exec_path = ['/home/farzaneh/mac/MiniC/MiniC-compiler-code/build/src/minicc']

test_cases = [
    ['test-cases/var.c', b'', 1],
    ['test-cases/bool_fail.c',
        b'Redefinition of variable/parameter "z" in the same scope! (4:5)\nVariable and the assignment expression do not have the same type! (9:4)\n"+" opcode must have int type operand! (11:4)\n"&&"/"||" opcode must have bool operand! (13:8)\nFunction has return type "bool", but the returned expression has type "int"! (14:8)\nConditional expression in if statement has non-bool type! (13:8)\n', 2],
    ['test-cases/index_fail.c',
        b'Indexing an non-array variable! (7:4)\nBreak statement must appear inside a for statement! (12:8)\nThe function "main()" need to return a value at its end! (6:0)\n', 2],
    ['test-cases/counting_fail1.c',
        b'Conditional expression in for statement has non-bool type! (12:11)\n', 2],
    ['test-cases/counting_fail2.c',
        b'Function dumy() is not declared before use! (18:4)\nRedefinition of function "main()"! (24:0)', 2],
    ['test-cases/easy_fail.c',
        b'Negate "-" opcode must have int operand! (9:4)\nNot "!" opcode must have bool operand! (10:4)\n', 2],
    ['test-cases/easy.c', b'', 2],
    ['test-cases/ops_fail1.c',
        b'"+" opcode must have int type operand! (7:4)\n', 2],
    ['test-cases/ops_fail2.c',
        b'Variable gamma is not declared before use! (7:4)\n', 2],
    ['test-cases/pizza_fail5.c',
        b'Array index expressions must have int operand! (12:4)', 2],
    ['test-cases/pizza_fail4.c',
        b'Function whichTop() is declared with 1 parameters but called with 0 arguments! (30:4)', 2],
    ['test-cases/pizza_fail3.c',
        b'Function has return type "int", but the returned expression has type "bool"! (29:8)', 2],
    ['test-cases/pizza_fail2.c',
        b'Function has return type "int", but the returned expression has type "bool"! (35:4)\nInteger literal must be inside the range of int! (33:8)', 2],
    ['test-cases/pizza_fail1.c',
        b'Function has return type "int", but the returned expression has type "bool"! (27:8)', 2],
]
tmp_files = ['output.bc']

def convert_bytes_to_set(b):
    s = b.decode("utf-8") 
    ss = s.split('\n')
    ss = set(ss)
    if '' in ss:
        ss.remove('')
    return ss

def main():
    test = tester.Core('Assignment 4 Tester', 27)

    # Public tests
    for case in test_cases:
        test_path = exec_path + [case[0]]
        # Supply the executable and test program path
        test.start_test(test_path)
        # Check the program output against a string and assign a mark
        test.actual = convert_bytes_to_set(test.actual)
        test.check_output(convert_bytes_to_set(case[1]), case[2])
        test.clean_up(tmp_files)

if __name__ == '__main__':
    main()
    
