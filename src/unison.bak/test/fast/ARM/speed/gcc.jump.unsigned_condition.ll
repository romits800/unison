; ModuleID = 'gcc.jump.unsigned_condition.ll'
target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv6t2-pc-linux-eabi"

@.str = external hidden unnamed_addr constant [7 x i8], align 1
@__FUNCTION__.unsigned_condition = external hidden unnamed_addr constant [19 x i8], align 1
@switch.table.4 = external hidden unnamed_addr constant [10 x i32]

; Function Attrs: noreturn
declare void @fancy_abort(i8*, i32, i8*) #0

; Function Attrs: nounwind
define i32 @unsigned_condition(i32 %code) #1 {
  %switch.tableidx = add i32 %code, -102
  %1 = icmp ult i32 %switch.tableidx, 10
  br i1 %1, label %switch.lookup, label %2

; <label>:2                                       ; preds = %0
  tail call void @fancy_abort(i8* nonnull getelementptr inbounds ([7 x i8], [7 x i8]* @.str, i32 0, i32 0), i32 951, i8* nonnull getelementptr inbounds ([19 x i8], [19 x i8]* @__FUNCTION__.unsigned_condition, i32 0, i32 0)) #2
  unreachable

switch.lookup:                                    ; preds = %0
  %switch.gep = getelementptr inbounds [10 x i32], [10 x i32]* @switch.table.4, i32 0, i32 %switch.tableidx
  %switch.load = load i32, i32* %switch.gep, align 4
  ret i32 %switch.load
}

attributes #0 = { noreturn "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="arm1156t2-s" "target-features"="+dsp,+strict-align" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"min_enum_size", i32 4}
!2 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
