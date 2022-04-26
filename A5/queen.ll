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
