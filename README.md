## **Compilers and Interpreters**

**Note**: These are the assignments that I completed in this course. That means the skeleton of the project was given to us by the instructor and I worked on completing the required tasks.
In this course, I built a compiler for a custom language called **MiniC**, which is a subset of the C language. The compiler project was split into 6 assignments, each of which focusing on one of the main stages of the compiling process. The description of each assignment’s tasks can be viewed in a pdf in this repo.

&nbsp;
        
<img src="/A1/compiler-stages.png" style="width:600px;height:300px;">

&nbsp;

The assigments 2 and 3 aimed to build the MiniC compiler's front-end and assigments 4, 5 and 6 aimed to build the MiniC compiler's back-end.

You can find the **MiniC** language specifications [*here*](/MiniC-language.pdf).

The following is each of these assignments in detail.

&nbsp;

### **`Assignment 1`**

In assignment 1, I set up a VirtualBox on my mac and installed the required technologies for this compiler project on that. Specifically, this project required `ANTLR4 version 4.9`, `LLVM 11.0`, and `Clang 11.0`.

For being able to code on my mac while I could run it on the virtual machine, I mounted a shared folder on my mac with the virtual box VM under my ownernership. To do that run:

```
sudo mount -t    vboxsf    <name of the shared folder that you specified in your shared section in the settings of the VM>  <path to the directory on your VM where you want to refer to this shared folder>  -o  uid=1000,gid=1000 
```

For example since my shared folder in the shared section was `CSC488`, I did:

```
sudo  mount  -t   vboxsf  CSC488  ~/farzaneh/shared_files    -o  uid=1000,gid=1000 
```


*ANTLR4* should be in `/usr/local/lib/` and the *LLVM* should be in `/usr/lib`. 

Specifically for ANTLR4, we did the following setups:

1) Install Java (version 1.7 or higher)

2) Download the ANTLR4 by running:

```
cd /usr/local/lib
curl -O https://www.antlr.org/download/antlr-4.9-complete.jar
```

3) Add `antlr-4.9-complete.jar` to your `CLASSPATH` by running:

```
export CLASSPATH=".:/usr/local/lib/antlr-4.9-complete.jar:$CLASSPATH"
```

It's also a good idea to put this in your `.bash_profile` or `.zshrc`.

4) Create aliases for the ANTLR `Tool`, and `TestRig`, by running:

```
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.10.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.10.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'
```

You can view the detailed completion requirements of this assignment [*here*](/A1/a1.pdf) and its tutorial [*here*](/A1/tut1.pdf).

&nbsp;

### **`Assignment 2`**

In assignment 2, I followed the MiniC language specifications to write the MiniC grammar rules in the g4 format in `grammar/MiniC.g4` file. When building, `antlr4-runtime` processes `grammar/Minic.g4` grammar and generates Minic parser source files. The parser files are under `build/grammars/Minic` folder. Then, `src/ASTChecker.cpp` and `src/ASTBuilder.cpp` use the Minic parser API to generate the parse tree and AST (AST will be implemented in assignment 3). These two files were already written for us. To see how the ANTLR4 is compiled for MiniC, you can take a look at `CMakelist.txt` in each folder.

Given the MiniC program called `var_loop.c` as below:

```
int x;
int arr[10];

int main() {
    x = 5;
    for (x = 0; x < 10; x = x + 1) {
        arr[x] = x;
    }
    return 0;
}
```

We can generate  its parse tree by following these steps:

1) Comment out the whole `@header` section and the return in `prog`

2) In `grammar` folder, run:

```
antlr4 MiniC.g4
```

Then run:

```
javac MiniC*.java
```

Then generate the parse of `var_loop.c` by running:

```
grun MiniC prog -gui var_loop.c
```

The following is the parse tree generated for it:

&nbsp;

<img src="/A2/var_loop_parse_tree.png" style="width:600px;height:300px;">

&nbsp;

**Note**

We cannot generate the parse tree when we have added the *actions* to the MiniC.g4 file. That is why for generating the parse tree always use the starter code of A2.

You can view the detailed completion requirements of this assignment [*here*](/A2/a2.pdf) and its tutorial [*here*](/A2/tut2.pdf).

&nbsp;

### **`Assignment 3`**

In assignment 3, I added the compiler *actions* in the g4 format in the `grammars/MiniC.g4` file so that I could create the `Abstract Syntax Tree (i.e. AST)` while parsing.

Given the `var_loop.c` as above, we can generate its AST by running:

```
src/build/minicc var_loop.c -o var_loop.bc  --print-ast
```

The AST for the MiniC program `var_loop.c` is as follows:

```
MiniCProg [syslibFlag=0] (3, 0)
|-VarDecl (3, 0)
  |-TypeReference: [val=int] (3, 0)
  |-VarRef (3, 4)
    |-Identifier [name=x] (3, 4)
|-VarDecl (4, 0)
  |-TypeReference: [val=int] (4, 0)
  |-VarRef (4, 4)
    |-Identifier [name=arr] (4, 4)
    |-IntLiteralExpr [val=10] (4, 4)
|-FuncDecl [hasBody=1] (6, 0)
  |-TypeReference: [val=int] (6, 0)
  |-Identifier [name=main] (6, 4)
  |-ScopeStatement (6, 11)
    |-ExprStmt (7, 4)
      |-AssignmentExpr (7, 4)
        |-VarRef (7, 4)
          |-Identifier [name=x] (7, 4)
        |-IntLiteralExpr [val=5] (7, 8)
    |-ForStmt (8, 4)
      |-AssignmentExpr (8, 9)
        |-VarRef (8, 9)
          |-Identifier [name=x] (8, 9)
        |-IntLiteralExpr [val=0] (8, 13)
      |-BinaryExpr [opcode='<'] (8, 16)
        |-VarExpr (8, 16)
          |-VarRef (8, 16)
            |-Identifier [name=x] (8, 16)
        |-IntLiteralExpr [val=10] (8, 20)
      |-AssignmentExpr (8, 24)
        |-VarRef (8, 24)
          |-Identifier [name=x] (8, 24)
        |-BinaryExpr [opcode='+'] (8, 28)
          |-VarExpr (8, 28)
            |-VarRef (8, 28)
              |-Identifier [name=x] (8, 28)
          |-IntLiteralExpr [val=1] (8, 32)
      |-ExprStmt (9, 8)
        |-AssignmentExpr (9, 8)
          |-VarRef (9, 8)
            |-Identifier [name=arr] (9, 8)
            |-VarExpr (9, 12)
              |-VarRef (9, 12)
                |-Identifier [name=x] (9, 12)
          |-VarExpr (9, 17)
            |-VarRef (9, 17)
              |-Identifier [name=x] (9, 17)
    |-ReturnStmt (10, 4)
      |-IntLiteralExpr [val=0] (10, 11)
```

You can view the detailed completion requirements of this assignment [*here*](/A3/a3.pdf) and its tutorial [*here*](/A3/tut3.pdf).

&nbsp;

### **`Assignment 4`**

In assignment 4, I did the **semantics analysis** on the AST and created the function and symbol tables as the side effects of this analysis. The AST and table creations are implemented in the `src/VerifyAndBuildSymbols.cpp` and `src/SymbolTable.h` files, respectively. The object verifier (of class `VerifyAndBuildSymbols`) visits each AST node starting from `prog`. After visiting each node, we check the node's relationship with respect to the language, then build the symbol table and report semantic errors in `src/VerifyAndBuildSymbols.cpp`. In **MiniC**, we are specifically interested in the `Function symbol table` that maps a function name to a *FuncSymbolEntry* and the `Variable symbol table` that maps a variable name to a *VarSymbolEntry*. So we have a `FuncSymbolEntry` in the `Function symbol table` for every function declaration. Similarly, we have a `VarSymbolEntry` in the `Variable symbol table` for every variable declaration. That means, in our MiniC compiler, the program AST node (i.e. `prog`) and all of the scope AST nodes have their own local `Variable symbol table`, while there is only one global `Function symbol table` which belongs to the program AST node.

Given the following MiniC program `fail_bool.c` as below:

```
int x, y, z;
bool z;

bool main() {
    x = 10;
    y = -10;
    z = true;
    10 + 4;
    true + false;
    
    if (x || y)
        return x;
    return true;
}
```
We will get the following semantic errors:

```
Redefinition of variable/parameter "z" in the same scope! (4:5)
Variable and the assignment expression do not have the same type! (9:4)
"+" opcode must have int type operand! (11:4)
"&&"/"||" opcode must have bool operand! (13:8)
Function has return type "bool", but the returned expression has type "int"! (14:8)
Conditional expression in if statement has non-bool type! (13:8)
```

You can view the detailed completion requirements of this assignment [*here*](/A4/a4.pdf) and and its tutorial [*here*](/A4/tut4.pdf).

&nbsp;

### **`Assignment 5`**

In assignment 5, I used **LLVM framework API** to generate LLVM IR code for an input MiniC program. The implementations for this assignment are done in the `src/IRGenerator.cpp` file. The code generation is done by constructing an `IRGenerator` object, and then visiting each AST node to build its LLVM module. The `IRGenerator` class is a derived class from `ASTvisitor` in order to visit each node. Finally, in `src/main.cpp`, the LLVM module outputs IR bitcode and writes to a file corresponding to LLVM IR.

Given the MiniC program called `queen.c` as below:

```
#include "minicio.h"

int N;
int a[20], c[400];

void markcolumn(int x, int y, int mark) {
    int i;
    for (i = 0; i < N; i = i + 1)
        c[i * N + y] = c[i * N + y] + mark;
}

void markdiag(int x, int y, int mark) {
    int i;
    for (i = 0; i < N; i = i + 1) {
        int x1;
        int y1;
        x1 = i;
        y1 = y + (i - x);
        if (y1 >= 0 && y1 < N)
            c[x1 * N + y1] = c[x1 * N + y1] + mark;
        y1 = y - (i - x);
        if (y1 >= 0 && y1 < N)
            c[x1 * N + y1] = c[x1 * N + y1] + mark;
    }
}

void search(int k) {
    int i;
    if (k == N) {
        for (i = 0; i < N; i = i + 1)
            putint(a[i]);
        putnewline();
        return;
    }
    for (i = 0; i < N; i = i + 1)
        if (c[k * N + i] == 0) {
            markcolumn(k, i, 1);
            markdiag(k, i, 1);
            a[k] = i;
            search(k + 1);
            markdiag(k, i, -1);
            markcolumn(k, i, -1);
        }
}

int main() {
    int i;
    N = getint();
    for (i = 0; i < N * N; i = i + 1)
        c[i] = 0;
    search(0);
    return 0;
}
```

The generated LLVM IR code for that is as follows:

```
; ModuleID = 'queen.bc'
source_filename = "queen.bc"
target triple = "x86_64-pc-linux-gnu"

@N = common global i32 0
@a = common global [20 x i32] zeroinitializer
@c = common global [400 x i32] zeroinitializer

declare i32 @getint()

define i32 @main() {
entry:
  %i = alloca i32, align 4
  %0 = call i32 @getint()
  store i32 %0, i32* @N, align 4
  store i32 0, i32* %i, align 4
  br label %cond-bb

cond-bb:                                          ; preds = %body-bb, %entry
  %1 = load i32, i32* %i, align 4
  %2 = load i32, i32* @N, align 4
  %3 = load i32, i32* @N, align 4
  %4 = mul i32 %2, %3
  %5 = icmp slt i32 %1, %4
  br i1 %5, label %body-bb, label %out-bb

body-bb:                                          ; preds = %cond-bb
  %6 = load i32, i32* %i, align 4
  %7 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %6
  store i32 0, i32* %7, align 4
  %8 = load i32, i32* %i, align 4
  %9 = add i32 %8, 1
  store i32 %9, i32* %i, align 4
  br label %cond-bb

out-bb:                                           ; preds = %cond-bb
  call void @search(i32 0)
  ret i32 0
}

define void @markcolumn(i32 %0, i32 %1, i32 %2) {
entry:
  %x = alloca i32, align 4
  store i32 %0, i32* %x, align 4
  %y = alloca i32, align 4
  store i32 %1, i32* %y, align 4
  %mark = alloca i32, align 4
  store i32 %2, i32* %mark, align 4
  %i = alloca i32, align 4
  store i32 0, i32* %i, align 4
  br label %cond-bb

cond-bb:                                          ; preds = %body-bb, %entry
  %3 = load i32, i32* %i, align 4
  %4 = load i32, i32* @N, align 4
  %5 = icmp slt i32 %3, %4
  br i1 %5, label %body-bb, label %out-bb

body-bb:                                          ; preds = %cond-bb
  %6 = load i32, i32* %i, align 4
  %7 = load i32, i32* @N, align 4
  %8 = mul i32 %6, %7
  %9 = load i32, i32* %y, align 4
  %10 = add i32 %8, %9
  %11 = load i32, i32* %i, align 4
  %12 = load i32, i32* @N, align 4
  %13 = mul i32 %11, %12
  %14 = load i32, i32* %y, align 4
  %15 = add i32 %13, %14
  %16 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %15
  %17 = load i32, i32* %16, align 4
  %18 = load i32, i32* %mark, align 4
  %19 = add i32 %17, %18
  %20 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %10
  store i32 %19, i32* %20, align 4
  %21 = load i32, i32* %i, align 4
  %22 = add i32 %21, 1
  store i32 %22, i32* %i, align 4
  br label %cond-bb

out-bb:                                           ; preds = %cond-bb
  ret void
}

define void @markdiag(i32 %0, i32 %1, i32 %2) {
entry:
  %x = alloca i32, align 4
  store i32 %0, i32* %x, align 4
  %y = alloca i32, align 4
  store i32 %1, i32* %y, align 4
  %mark = alloca i32, align 4
  store i32 %2, i32* %mark, align 4
  %i = alloca i32, align 4
  store i32 0, i32* %i, align 4
  br label %cond-bb

cond-bb:                                          ; preds = %out-bb5, %entry
  %3 = load i32, i32* %i, align 4
  %4 = load i32, i32* @N, align 4
  %5 = icmp slt i32 %3, %4
  br i1 %5, label %body-bb, label %out-bb

body-bb:                                          ; preds = %cond-bb
  %x1 = alloca i32, align 4
  %y1 = alloca i32, align 4
  %6 = load i32, i32* %i, align 4
  store i32 %6, i32* %x1, align 4
  %7 = load i32, i32* %y, align 4
  %8 = load i32, i32* %i, align 4
  %9 = load i32, i32* %x, align 4
  %10 = sub i32 %8, %9
  %11 = add i32 %7, %10
  store i32 %11, i32* %y1, align 4
  %12 = load i32, i32* %y1, align 4
  %13 = icmp sge i32 %12, 0
  %14 = icmp eq i1 %13, true
  br i1 %14, label %logical-slow-bb, label %logical-out-bb

out-bb:                                           ; preds = %cond-bb
  ret void

logical-slow-bb:                                  ; preds = %body-bb
  %15 = load i32, i32* %y1, align 4
  %16 = load i32, i32* @N, align 4
  %17 = icmp slt i32 %15, %16
  br label %logical-out-bb

logical-out-bb:                                   ; preds = %logical-slow-bb, %body-bb
  %18 = phi i1 [ false, %body-bb ], [ %17, %logical-slow-bb ]
  br i1 %18, label %then-bb, label %out-bb1

then-bb:                                          ; preds = %logical-out-bb
  %19 = load i32, i32* %x1, align 4
  %20 = load i32, i32* @N, align 4
  %21 = mul i32 %19, %20
  %22 = load i32, i32* %y1, align 4
  %23 = add i32 %21, %22
  %24 = load i32, i32* %x1, align 4
  %25 = load i32, i32* @N, align 4
  %26 = mul i32 %24, %25
  %27 = load i32, i32* %y1, align 4
  %28 = add i32 %26, %27
  %29 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %28
  %30 = load i32, i32* %29, align 4
  %31 = load i32, i32* %mark, align 4
  %32 = add i32 %30, %31
  %33 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %23
  store i32 %32, i32* %33, align 4
  br label %out-bb1

out-bb1:                                          ; preds = %then-bb, %logical-out-bb
  %34 = load i32, i32* %y, align 4
  %35 = load i32, i32* %i, align 4
  %36 = load i32, i32* %x, align 4
  %37 = sub i32 %35, %36
  %38 = sub i32 %34, %37
  store i32 %38, i32* %y1, align 4
  %39 = load i32, i32* %y1, align 4
  %40 = icmp sge i32 %39, 0
  %41 = icmp eq i1 %40, true
  br i1 %41, label %logical-slow-bb2, label %logical-out-bb3

logical-slow-bb2:                                 ; preds = %out-bb1
  %42 = load i32, i32* %y1, align 4
  %43 = load i32, i32* @N, align 4
  %44 = icmp slt i32 %42, %43
  br label %logical-out-bb3

logical-out-bb3:                                  ; preds = %logical-slow-bb2, %out-bb1
  %45 = phi i1 [ false, %out-bb1 ], [ %44, %logical-slow-bb2 ]
  br i1 %45, label %then-bb4, label %out-bb5

then-bb4:                                         ; preds = %logical-out-bb3
  %46 = load i32, i32* %x1, align 4
  %47 = load i32, i32* @N, align 4
  %48 = mul i32 %46, %47
  %49 = load i32, i32* %y1, align 4
  %50 = add i32 %48, %49
  %51 = load i32, i32* %x1, align 4
  %52 = load i32, i32* @N, align 4
  %53 = mul i32 %51, %52
  %54 = load i32, i32* %y1, align 4
  %55 = add i32 %53, %54
  %56 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %55
  %57 = load i32, i32* %56, align 4
  %58 = load i32, i32* %mark, align 4
  %59 = add i32 %57, %58
  %60 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %50
  store i32 %59, i32* %60, align 4
  br label %out-bb5

out-bb5:                                          ; preds = %then-bb4, %logical-out-bb3
  %61 = load i32, i32* %i, align 4
  %62 = add i32 %61, 1
  store i32 %62, i32* %i, align 4
  br label %cond-bb
}

declare void @putint(i32)

declare void @putnewline()

define void @search(i32 %0) {
entry:
  %k = alloca i32, align 4
  store i32 %0, i32* %k, align 4
  %i = alloca i32, align 4
  %1 = load i32, i32* %k, align 4
  %2 = load i32, i32* @N, align 4
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %then-bb, label %out-bb

then-bb:                                          ; preds = %entry
  store i32 0, i32* %i, align 4
  br label %cond-bb

out-bb:                                           ; preds = %entry
  store i32 0, i32* %i, align 4
  br label %cond-bb2

cond-bb:                                          ; preds = %body-bb, %then-bb
  %4 = load i32, i32* %i, align 4
  %5 = load i32, i32* @N, align 4
  %6 = icmp slt i32 %4, %5
  br i1 %6, label %body-bb, label %out-bb1

body-bb:                                          ; preds = %cond-bb
  %7 = load i32, i32* %i, align 4
  %8 = getelementptr [20 x i32], [20 x i32]* @a, i32 0, i32 %7
  %9 = load i32, i32* %8, align 4
  call void @putint(i32 %9)
  %10 = load i32, i32* %i, align 4
  %11 = add i32 %10, 1
  store i32 %11, i32* %i, align 4
  br label %cond-bb

out-bb1:                                          ; preds = %cond-bb
  call void @putnewline()
  ret void

cond-bb2:                                         ; preds = %out-bb6, %out-bb
  %12 = load i32, i32* %i, align 4
  %13 = load i32, i32* @N, align 4
  %14 = icmp slt i32 %12, %13
  br i1 %14, label %body-bb3, label %out-bb4

body-bb3:                                         ; preds = %cond-bb2
  %15 = load i32, i32* %k, align 4
  %16 = load i32, i32* @N, align 4
  %17 = mul i32 %15, %16
  %18 = load i32, i32* %i, align 4
  %19 = add i32 %17, %18
  %20 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %19
  %21 = load i32, i32* %20, align 4
  %22 = icmp eq i32 %21, 0
  br i1 %22, label %then-bb5, label %out-bb6

out-bb4:                                          ; preds = %cond-bb2
  ret void

then-bb5:                                         ; preds = %body-bb3
  %23 = load i32, i32* %k, align 4
  %24 = load i32, i32* %i, align 4
  call void @markcolumn(i32 %23, i32 %24, i32 1)
  %25 = load i32, i32* %k, align 4
  %26 = load i32, i32* %i, align 4
  call void @markdiag(i32 %25, i32 %26, i32 1)
  %27 = load i32, i32* %k, align 4
  %28 = load i32, i32* %i, align 4
  %29 = getelementptr [20 x i32], [20 x i32]* @a, i32 0, i32 %27
  store i32 %28, i32* %29, align 4
  %30 = load i32, i32* %k, align 4
  %31 = add i32 %30, 1
  call void @search(i32 %31)
  %32 = load i32, i32* %k, align 4
  %33 = load i32, i32* %i, align 4
  call void @markdiag(i32 %32, i32 %33, i32 -1)
  %34 = load i32, i32* %k, align 4
  %35 = load i32, i32* %i, align 4
  call void @markcolumn(i32 %34, i32 %35, i32 -1)
  br label %out-bb6

out-bb6:                                          ; preds = %then-bb5, %body-bb3
  %36 = load i32, i32* %i, align 4
  %37 = add i32 %36, 1
  store i32 %37, i32* %i, align 4
  br label %cond-bb2
}
```

You can view the detailed completion requirements of this assignment [*here*](/A5/a5.pdf) and its tutorials [*here*](/A5/tut5-1.pdf) and [*here*](/A5/tut5-2.pdf).

&nbsp;

### **`Assignment 6`**

In assignment 6, I created the optimization pass `alloca2reg` in the `src/Alloca2Reg.cpp` file to promote the memory allocations to registers as much as possible. I also created a custom pass `commonSubExprElimination` in the `src/CommonSubExprElimination.cpp` file to apply eliminating common subexpression optimization. Both of these optimizations are *function passes*. After the MiniC compiler IR bitcode is generated, PassManager in LLVM will chain multiple optimizations and analysis passes to run together. Command `opt` is a LLVM tool that directly invokes a pass on a LLVM IR bitcode.

Given the MiniC program `queen.c` as in assigment 5 above, the optimized LLVM IR of it (after both `alloca2reg` and `commonSubExprElimination` optimizations) will be as follows:
        
```
; ModuleID = 'queen_opt.bc'
source_filename = "queen.bc"
target triple = "x86_64-pc-linux-gnu"

@N = common global i32 0
@a = common global [20 x i32] zeroinitializer
@c = common global [400 x i32] zeroinitializer

declare i32 @getint()

define i32 @main() {
entry:
  %0 = call i32 @getint()
  store i32 %0, i32* @N, align 4
  br label %cond-bb

cond-bb:                                          ; preds = %body-bb, %entry
  %phi = phi i32 [ %5, %body-bb ], [ 0, %entry ]
  %1 = load i32, i32* @N, align 4
  %2 = mul i32 %1, %1
  %3 = icmp slt i32 %phi, %2
  br i1 %3, label %body-bb, label %out-bb

body-bb:                                          ; preds = %cond-bb
  %4 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %phi
  store i32 0, i32* %4, align 4
  %5 = add i32 %phi, 1
  br label %cond-bb

out-bb:                                           ; preds = %cond-bb
  call void @search(i32 0)
  ret i32 0
}

define void @markcolumn(i32 %0, i32 %1, i32 %2) {
entry:
  br label %cond-bb

cond-bb:                                          ; preds = %body-bb, %entry
  %phi3 = phi i32 [ %11, %body-bb ], [ 0, %entry ]
  %3 = load i32, i32* @N, align 4
  %4 = icmp slt i32 %phi3, %3
  br i1 %4, label %body-bb, label %out-bb

body-bb:                                          ; preds = %cond-bb
  %5 = load i32, i32* @N, align 4
  %6 = mul i32 %phi3, %5
  %7 = add i32 %6, %1
  %8 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %7
  %9 = load i32, i32* %8, align 4
  %10 = add i32 %9, %2
  store i32 %10, i32* %8, align 4
  %11 = add i32 %phi3, 1
  br label %cond-bb

out-bb:                                           ; preds = %cond-bb
  ret void
}

define void @markdiag(i32 %0, i32 %1, i32 %2) {
entry:
  br label %cond-bb

cond-bb:                                          ; preds = %out-bb5, %entry
  %phi3 = phi i32 [ %31, %out-bb5 ], [ 0, %entry ]
  %3 = load i32, i32* @N, align 4
  %4 = icmp slt i32 %phi3, %3
  br i1 %4, label %body-bb, label %out-bb

body-bb:                                          ; preds = %cond-bb
  %5 = sub i32 %phi3, %0
  %6 = add i32 %1, %5
  %7 = icmp sge i32 %6, 0
  %8 = icmp eq i1 %7, true
  br i1 %8, label %logical-slow-bb, label %logical-out-bb

out-bb:                                           ; preds = %cond-bb
  ret void

logical-slow-bb:                                  ; preds = %body-bb
  %9 = load i32, i32* @N, align 4
  %10 = icmp slt i32 %6, %9
  br label %logical-out-bb

logical-out-bb:                                   ; preds = %logical-slow-bb, %body-bb
  %11 = phi i1 [ false, %body-bb ], [ %10, %logical-slow-bb ]
  br i1 %11, label %then-bb, label %out-bb1

then-bb:                                          ; preds = %logical-out-bb
  %12 = load i32, i32* @N, align 4
  %13 = mul i32 %phi3, %12
  %14 = add i32 %13, %6
  %15 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %14
  %16 = load i32, i32* %15, align 4
  %17 = add i32 %16, %2
  store i32 %17, i32* %15, align 4
  br label %out-bb1

out-bb1:                                          ; preds = %then-bb, %logical-out-bb
  %18 = sub i32 %phi3, %0
  %19 = sub i32 %1, %18
  %20 = icmp sge i32 %19, 0
  %21 = icmp eq i1 %20, true
  br i1 %21, label %logical-slow-bb2, label %logical-out-bb3

logical-slow-bb2:                                 ; preds = %out-bb1
  %22 = load i32, i32* @N, align 4
  %23 = icmp slt i32 %19, %22
  br label %logical-out-bb3

logical-out-bb3:                                  ; preds = %logical-slow-bb2, %out-bb1
  %24 = phi i1 [ false, %out-bb1 ], [ %23, %logical-slow-bb2 ]
  br i1 %24, label %then-bb4, label %out-bb5

then-bb4:                                         ; preds = %logical-out-bb3
  %25 = load i32, i32* @N, align 4
  %26 = mul i32 %phi3, %25
  %27 = add i32 %26, %19
  %28 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %27
  %29 = load i32, i32* %28, align 4
  %30 = add i32 %29, %2
  store i32 %30, i32* %28, align 4
  br label %out-bb5

out-bb5:                                          ; preds = %then-bb4, %logical-out-bb3
  %31 = add i32 %phi3, 1
  br label %cond-bb
}

declare void @putint(i32)

declare void @putnewline()

define void @search(i32 %0) {
entry:
  %1 = load i32, i32* @N, align 4
  %2 = icmp eq i32 %0, %1
  br i1 %2, label %then-bb, label %out-bb

then-bb:                                          ; preds = %entry
  br label %cond-bb

out-bb:                                           ; preds = %entry
  br label %cond-bb2

cond-bb:                                          ; preds = %body-bb, %then-bb
  %phi5 = phi i32 [ %7, %body-bb ], [ 0, %then-bb ]
  %3 = load i32, i32* @N, align 4
  %4 = icmp slt i32 %phi5, %3
  br i1 %4, label %body-bb, label %out-bb1

body-bb:                                          ; preds = %cond-bb
  %5 = getelementptr [20 x i32], [20 x i32]* @a, i32 0, i32 %phi5
  %6 = load i32, i32* %5, align 4
  call void @putint(i32 %6)
  %7 = add i32 %phi5, 1
  br label %cond-bb

out-bb1:                                          ; preds = %cond-bb
  call void @putnewline()
  ret void

cond-bb2:                                         ; preds = %out-bb6, %out-bb
  %phi11 = phi i32 [ %18, %out-bb6 ], [ 0, %out-bb ]
  %8 = load i32, i32* @N, align 4
  %9 = icmp slt i32 %phi11, %8
  br i1 %9, label %body-bb3, label %out-bb4

body-bb3:                                         ; preds = %cond-bb2
  %10 = load i32, i32* @N, align 4
  %11 = mul i32 %0, %10
  %12 = add i32 %11, %phi11
  %13 = getelementptr [400 x i32], [400 x i32]* @c, i32 0, i32 %12
  %14 = load i32, i32* %13, align 4
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %then-bb5, label %out-bb6

out-bb4:                                          ; preds = %cond-bb2
  ret void

then-bb5:                                         ; preds = %body-bb3
  call void @markcolumn(i32 %0, i32 %phi11, i32 1)
  call void @markdiag(i32 %0, i32 %phi11, i32 1)
  %16 = getelementptr [20 x i32], [20 x i32]* @a, i32 0, i32 %0
  store i32 %phi11, i32* %16, align 4
  %17 = add i32 %0, 1
  call void @search(i32 %17)
  call void @markdiag(i32 %0, i32 %phi11, i32 -1)
  call void @markcolumn(i32 %0, i32 %phi11, i32 -1)
  br label %out-bb6

out-bb6:                                          ; preds = %then-bb5, %body-bb3
  %18 = add i32 %phi11, 1
  br label %cond-bb2
}
```

As we can see lots of unnecessary `alloca`, `load`, and `store` instructions have been optimized out, and instead we have used the `phi` instructions to make sure the control flow of the program is as desired.

You can view the detailed completion requirements of this assignment [*here*](/A6/a6.pdf) and its tutorial [*here*](/A5/tut6.pdf).

&nbsp;

## **Compilation and Optimization Example**

Given any valid MiniC program like `queen.c`, we can compile it and get its bitcode by running:

```
src/build/minicc queen.c -o output.bc
```

Now let's run the optimization passes over the `output.bc` and compare their runtime.
On a VM with ```Memory: 2GB 1867 MHz DDR3 & CPU: 2.7 GHz Dual-Core Intel Core i5```, the runtime of the `queen.c` program in 3 different stages of optimizations is as follows:

**Runtime with only the `alloca2reg` optimization:**

First, use `opt` to run the `alloca2reg` optimization on the `output.bc` file:

```
opt -O0 -load src/liballoca2reg.so --alloca2reg output.bc -o output_opt.bc
```

Then link the `output_opt.bc` file with the MiniC library by running:

```
clang output_opt.bc minicio/libminicio.a -o output_opt
```

and then run optimized executable `output_opt` as:

```
./output_opt
```

The runtime of 50 times of running the `queen.c` program with input=13, with only the `alloca2reg` optimization was **123.2542** seconds.


**Runtime with `alloca2reg` and `commonSubExprElimination` optimization:**

First, use opt to run both the `alloca2reg` and `commonSubExprElimination` optimizations on the `output.bc` file:

```
opt -O0 -load src/liballoca2reg.so --alloca2reg -load src/libcommonSubExprElimination.so --commonSubExprElimination output.bc -o output_both_opt.bc
```

Then link the `output_both_opt.bc` file with the MiniC library by running:

```
clang output_both_opt.bc minicio/libminicio.a -o output_both_opt
```

and then run optimized executable `output_both_opt` as:

```
./output_both_opt
```

The runtime of 50 times of running the `queen.c` program with input=13, with both the `alloca2reg` and `commonSubExprElimination` optimizations was **06.5221** seconds.


**Runtime with LLVM `O3` optimization pass:**

First, use opt to run the `O3` optimization on the `output.bc` file:

```
opt -O3 output.bc -o output_o3.bc
```

Then link the `output_o3.bc` file with the MiniC library by running:

```
clang output_o3.bc minicio/libminicio.a -o output_o3
```

and then run optimized executable `output_o3` as:

```
./output_o3
```

The runtime of 50 times of running the `queen.c` program with input=13, with LLVM `O3` optimization was **78.5020** seconds.

So as we can see with only `alloca2reg` and `commonSubExprElimination` optimization passes we could get close to the best LLVM's optimization pass `O3`. In particular, `O3` optimization is only around 35% better than our custom optimization passes `alloca2reg` and `commonSubExprElimination`. This clearly demonstrates the effect of promoting alloca variables to registers which has a great role in speeding up the compiled program.


## **Compiling and Using the MiniC Compiler**

To build and compile the MiniC project, `cd` inside the `build` folder and make sure it is empty. Then run:

```
cmake ..
```

This will create the required make file for the project. Then to compile the project, make sure you are in the `build` folder and run:

```
make
```

Then you can see an executable called `minicc` is generated inside the `build` folder. This is our MiniC compiler that you can compile any valid MiniC with. For exmaple, we can compile the `queen.c` by running:

```
src/build/minicc queen.c -o output.bc
```

Also note that MiniC has a static library which is located in `build/minicio/libminicio.a`
We should link this library when we compile any MiniC file with `minicc`. To so that, run:

```
clang output.bc minicio/libminicio.a -o output
```

Now the `output` is the compiled executable of the MiniC program which we can run by:

```
./output
```

Note that you can always see the LLVM IR code of your MiniC program, by the `llvm-dis` tool. For exmaple, we can get the LLVM IR of the generated `output.bc` by running:

```
llvm-dis output.bc -o output.ll
```

This is how we generated the LLVM IR for the `queen.c` in the examples above.


**Debugging with GDB:**

In `build/CMakeCache.txt`, add the word `Debug` to the line `CMAKE_BUILD_TYPE:STRING=.` Then, after recompiling, you can debug the minicc binary with GDB.

For example, given `build/src/minicc` is the executable to run, and `queen.c` is its argument, run:

```
gdb --args build/src/minicc queen.c
```

**Running the testers of assignments:**

To run the tests for the assignment N, `cd` to its corresponding folder called `AN-tester` and then run:
        
```
python3 asstN.py -v
```

**Note**

As I mentioned, this project was provided to us in parts in the form of 6 different assignments. The folder `MiniC-compiler-code` is a collection of my implementations for each of the assignments 1-6 all in one place.

