; ModuleID = 'var_loop_opt.bc'
source_filename = "var_loop.bc"
target triple = "x86_64-pc-linux-gnu"

@x = common global i32 0
@arr = common global [10 x i32] zeroinitializer

define i32 @main() {
entry:
  store i32 5, i32* @x, align 4
  store i32 0, i32* @x, align 4
  br label %cond-bb

cond-bb:                                          ; preds = %body-bb, %entry
  %0 = load i32, i32* @x, align 4
  %1 = icmp slt i32 %0, 10
  br i1 %1, label %body-bb, label %out-bb

body-bb:                                          ; preds = %cond-bb
  %2 = load i32, i32* @x, align 4
  %3 = getelementptr [10 x i32], [10 x i32]* @arr, i32 0, i32 %2
  store i32 %2, i32* %3, align 4
  %4 = add i32 %2, 1
  store i32 %4, i32* @x, align 4
  br label %cond-bb

out-bb:                                           ; preds = %cond-bb
  ret i32 0
}
