; ModuleID = 'easy.bc'
source_filename = "easy.bc"
target triple = "x86_64-pc-linux-gnu"

@alpha = common global i1 false
@beta = common global i1 false
@delta = common global i32 0
@epsilon = common global i32 0

declare i32 @getint()

define void @main() {
entry:
  store i1 true, i1* @alpha, align 1
  %0 = load i1, i1* @alpha, align 1
  store i1 %0, i1* @beta, align 1
  store i32 -1, i32* @delta, align 4
  %1 = load i32, i32* @delta, align 4
  %2 = add i32 %1, 1
  store i32 %2, i32* @epsilon, align 4
  %3 = load i32, i32* @delta, align 4
  call void @putint(i32 %3)
  call void @putnewline()
  %4 = load i32, i32* @epsilon, align 4
  call void @putint(i32 %4)
  ret void
}

declare void @putint(i32)

declare void @putnewline()
