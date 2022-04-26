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
