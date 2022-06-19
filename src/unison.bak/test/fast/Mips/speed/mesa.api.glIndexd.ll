; ModuleID = 'mesa.api.glIndexd.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.gl_context.5805 = type { %struct.gl_shared_state.5806*, %struct.api_function_table.5807, %struct.api_function_table.5807, %struct.api_function_table.5807, %struct.gl_visual.5808*, %struct.gl_frame_buffer.5809*, %struct.dd_function_table.5810, i8*, [16 x float], [16 x float], i8, i32, [32 x [16 x float]], [16 x float], i32, [32 x [16 x float]], [16 x float], i8, i32, [10 x [16 x float]], i32, i8, i8, i32, [16 x %struct.gl_attrib_node.5811*], %struct.gl_accum_attrib.5812, %struct.gl_colorbuffer_attrib.5813, %struct.gl_current_attrib.5814, %struct.gl_depthbuffer_attrib.5815, %struct.gl_eval_attrib.5816, %struct.gl_fog_attrib.5817, %struct.gl_hint_attrib.5818, %struct.gl_light_attrib.5819, %struct.gl_line_attrib.5820, %struct.gl_list_attrib.5821, %struct.gl_pixel_attrib.5822, %struct.gl_point_attrib.5823, %struct.gl_polygon_attrib.5824, [32 x i32], %struct.gl_scissor_attrib.5825, %struct.gl_stencil_attrib.5826, %struct.gl_texture_attrib.5827, %struct.gl_transform_attrib.5828, %struct.gl_viewport_attrib.5829, i32, [16 x %struct.gl_attrib_node.5811*], %struct.gl_array_attrib.5830, %struct.gl_pixelstore_attrib.5831, %struct.gl_pixelstore_attrib.5831, %struct.gl_evaluators.5832, %struct.gl_feedback.5833, %struct.gl_selection.5834, i32, i32, i32, i32, i32, i32, i32, i32, i8, float, float, float, i8, i8, i8, i8, i32, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32*, i32)*, void (%struct.gl_context.5805*, i32, i32*, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32)*, %struct.vertex_buffer.5835*, %struct.pixel_buffer.5836*, i8 }
%struct.gl_shared_state.5806 = type { i32, [7000 x %union.node.5837*], %struct.gl_texture_object.5838* }
%union.node.5837 = type opaque
%struct.gl_texture_object.5838 = type { i32, i32, i32, float, [4 x i32], i32, i32, i32, i32, i32, [11 x %struct.gl_texture_image.5839*], i8, %struct.gl_texture_object.5838* }
%struct.gl_texture_image.5839 = type { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i8* }
%struct.api_function_table.5807 = type { void (%struct.gl_context.5805*, i32, float)*, void (%struct.gl_context.5805*, i32, float)*, i8 (%struct.gl_context.5805*, i32, i32*, i8*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, float, float, float, float, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, double)*, void (%struct.gl_context.5805*, float)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, float*)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, i8, i8, i8, i8)*, void (%struct.gl_context.5805*, i8, i8, i8, i8)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32, i32*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i8)*, void (%struct.gl_context.5805*, double, double)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i8)*, void (%struct.gl_context.5805*, i32, i8*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, float)*, void (%struct.gl_context.5805*, float, float)*, void (%struct.gl_context.5805*, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, i32, float*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, double, double, double, double, double, double)*, i32 (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32*)*, void (%struct.gl_context.5805*, i32, i8*)*, void (%struct.gl_context.5805*, i32, double*)*, void (%struct.gl_context.5805*, i32, double*)*, i32 (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, i32, float*)*, void (%struct.gl_context.5805*, i32, i32*)*, i8* (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, i32, double*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, float*)*, void (%struct.gl_context.5805*, i32, i32*)*, void (%struct.gl_context.5805*, i32, i16*)*, void (%struct.gl_context.5805*, i32, i8**)*, void (%struct.gl_context.5805*, i8*)*, void (%struct.gl_context.5805*, i32, i32*, float*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, i32, double*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, float)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, i8*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, i32, i32, i8*)*, i8 (%struct.gl_context.5805*, i32)*, i8 (%struct.gl_context.5805*, i32)*, i8 (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, float*, i32)*, void (%struct.gl_context.5805*, i32, i16)*, void (%struct.gl_context.5805*, float)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, float*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, float, float, i32, i32, float*, i8)*, void (%struct.gl_context.5805*, i32, float, float, i32, i32, float, float, i32, i32, float*, i8)*, void (%struct.gl_context.5805*, i32, float, float)*, void (%struct.gl_context.5805*, i32, float, float, i32, float, float)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, float*)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, float, float, float)*, void (%struct.gl_context.5805*, float*)*, void (%struct.gl_context.5805*, i32, i32, i8*)*, void (%struct.gl_context.5805*, float)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32, float)*, void (%struct.gl_context.5805*, float, float)*, void (%struct.gl_context.5805*, float)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, float, float)*, void (%struct.gl_context.5805*, i8*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, float, float, float, float)*, i32 (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, float, float, float)*, void (%struct.gl_context.5805*, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32, i32, i32)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, i32, i32, float*)*, void (%struct.gl_context.5805*, float, float, float)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, float, float, float, float)*, void (%struct.gl_context.5805*)* }
%struct.gl_image.5840 = type { i32, i32, i32, i32, i32, i32, i8*, i8, i32 }
%struct.gl_visual.5808 = type { i8, i8, float, float, float, float, i8, float, float, float, float, i32, i32, i32, i32, i8, i8 }
%struct.gl_frame_buffer.5809 = type { %struct.gl_visual.5808*, i32, i32, i16*, i8*, i16*, i8*, i8*, i8*, i32, i32, i32, i32 }
%struct.dd_function_table.5810 = type { void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i8, i8, i8, i8)*, void (%struct.gl_context.5805*, i8, i32, i32, i32, i32)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i8, i8, i8, i8)*, i8 (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i32*, i32*)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32, i32*, i32*, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.5805*, i32, i32*, i32*, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32*, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*)*, void (%struct.gl_context.5805*, i32, i32*, i32*, i32*, i8*)*, void (%struct.gl_context.5805*, i32, i32*, i32*, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32*)*, void (%struct.gl_context.5805*, i32, i32, i32, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.5805*, i32, i32*, i32*, i32*, i8*)*, void (%struct.gl_context.5805*, i32, i32*, i32*, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, i8 (%struct.gl_context.5805*, i32)*, i8 (%struct.gl_context.5805*, i8, i8, i8, i8)*, i8 (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*, i8)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*)*, i32 (%struct.gl_context.5805*, i32, i32, i32, i16*, i8*)*, void (%struct.gl_context.5805*, i32, i32*, i32*, i16*, i8*)*, void (%struct.gl_context.5805*, i32, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32, i16*)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32)*, void (%struct.gl_context.5805*, i32, i32, i32, i32)*, i8 (%struct.gl_context.5805*, i32, i32, i32, i32, i32, i32, i8, i8*)*, i8 (%struct.gl_context.5805*, i32, i32, float, float, float, float, %struct.gl_image.5840*)*, void (%struct.gl_context.5805*, i32)*, void (%struct.gl_context.5805*)*, void (%struct.gl_context.5805*, i32, float*)*, void (%struct.gl_context.5805*, i32, i32, i32, i32, %struct.gl_texture_image.5839*)*, void (%struct.gl_context.5805*, i32, i32, i32, float*)*, void (%struct.gl_context.5805*, i32, i32)*, void (%struct.gl_context.5805*, i32)* }
%struct.gl_accum_attrib.5812 = type { [4 x float] }
%struct.gl_colorbuffer_attrib.5813 = type { i32, [4 x float], i32, i32, i8, i32, i8, i32, float, i8, i8, i32, i32, i32, [4 x float], i32, i8, i8, i8, i8 }
%struct.gl_current_attrib.5814 = type { [4 x i32], i32, [3 x float], [4 x float], [4 x float], float, [4 x float], i32, [4 x float], i8, i8 }
%struct.gl_depthbuffer_attrib.5815 = type { i32, float, i8, i8 }
%struct.gl_eval_attrib.5816 = type { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i32, float, float, i32, i32, float, float, float, float }
%struct.gl_fog_attrib.5817 = type { i8, [4 x float], float, float, float, float, i32 }
%struct.gl_hint_attrib.5818 = type { i32, i32, i32, i32, i32 }
%struct.gl_light_attrib.5819 = type { [8 x %struct.gl_light.5841], %struct.gl_lightmodel.5842, [2 x %struct.gl_material.5843], i8, i32, i32, i32, i32, i8, %struct.gl_light.5841*, i8, [4 x float] }
%struct.gl_light.5841 = type { [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], float, float, float, float, float, float, i8, %struct.gl_light.5841*, [3 x float], [3 x float], [3 x float], [512 x [2 x float]], [3 x float], [3 x float], [3 x float], float, float }
%struct.gl_lightmodel.5842 = type { [4 x float], i8, i8 }
%struct.gl_material.5843 = type { [4 x float], [4 x float], [4 x float], [4 x float], float, float, float, float, [200 x float] }
%struct.gl_line_attrib.5820 = type { i8, i8, i16, i32, float }
%struct.gl_list_attrib.5821 = type { i32 }
%struct.gl_pixel_attrib.5822 = type { i32, float, float, float, float, float, float, float, float, float, float, i32, i32, i8, i8, float, float, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, [256 x i32], [256 x i32], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float] }
%struct.gl_point_attrib.5823 = type { i8, float }
%struct.gl_polygon_attrib.5824 = type { i32, i32, i32, i8, i8, i32, i32, i8, i8, float, float, i8, i8, i8, i8 }
%struct.gl_scissor_attrib.5825 = type { i8, i32, i32, i32, i32 }
%struct.gl_stencil_attrib.5826 = type { i8, i32, i32, i32, i32, i8, i8, i8, i8 }
%struct.gl_texture_attrib.5827 = type { i32, i32, [4 x float], i32, i32, i32, i32, i32, [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], %struct.gl_texture_object.5838*, %struct.gl_texture_object.5838*, %struct.gl_texture_object.5838*, %struct.gl_texture_object.5838*, %struct.gl_texture_object.5838*, %struct.gl_texture_object.5838* }
%struct.gl_transform_attrib.5828 = type { i32, [6 x [4 x float]], [6 x i8], i8, i8 }
%struct.gl_viewport_attrib.5829 = type { i32, i32, i32, i32, float, float, float, float, float, float, float, float }
%struct.gl_attrib_node.5811 = type { i32, i8*, %struct.gl_attrib_node.5811* }
%struct.gl_array_attrib.5830 = type { i32, i32, i32, i32, i8*, i8, i32, i32, i32, i8*, i8, i32, i32, i32, i32, i8*, i8, i32, i32, i32, i8*, i8, i32, i32, i32, i32, i8*, i8, i32, i32, i8*, i8 }
%struct.gl_pixelstore_attrib.5831 = type { i32, i32, i32, i32, i32, i32, i8, i8 }
%struct.gl_evaluators.5832 = type { %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_1d_map.5844, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845, %struct.gl_2d_map.5845 }
%struct.gl_1d_map.5844 = type { i32, float, float, float*, i8 }
%struct.gl_2d_map.5845 = type { i32, i32, float, float, float, float, float*, i8 }
%struct.gl_feedback.5833 = type { i32, i32, float*, i32, i32 }
%struct.gl_selection.5834 = type { i32*, i32, i32, i32, i32, [64 x i32], i8, float, float }
%struct.vertex_buffer.5835 = type opaque
%struct.pixel_buffer.5836 = type opaque

@CC = external global %struct.gl_context.5805*, align 4

; Function Attrs: nounwind
define void @glIndexd(double %c) #0 {
  %1 = load %struct.gl_context.5805*, %struct.gl_context.5805** @CC, align 4, !tbaa !1
  %2 = getelementptr inbounds %struct.gl_context.5805, %struct.gl_context.5805* %1, i32 0, i32 1, i32 94
  %3 = load void (%struct.gl_context.5805*, float)*, void (%struct.gl_context.5805*, float)** %2, align 4, !tbaa !5
  %4 = fptrunc double %c to float
  tail call void %3(%struct.gl_context.5805* %1, float %4) #1
  ret void
}

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !2, i64 0}
!2 = !{!"any pointer", !3, i64 0}
!3 = !{!"omnipotent char", !4, i64 0}
!4 = !{!"Simple C/C++ TBAA"}
!5 = !{!6, !2, i64 380}
!6 = !{!"gl_context", !2, i64 0, !7, i64 4, !7, i64 676, !7, i64 1348, !2, i64 2020, !2, i64 2024, !8, i64 2028, !2, i64 2208, !3, i64 2212, !3, i64 2276, !3, i64 2340, !9, i64 2344, !3, i64 2348, !3, i64 4396, !9, i64 4460, !3, i64 4464, !3, i64 6512, !3, i64 6576, !9, i64 6580, !3, i64 6584, !9, i64 7224, !3, i64 7228, !3, i64 7229, !9, i64 7232, !3, i64 7236, !10, i64 7300, !11, i64 7316, !13, i64 7404, !14, i64 7512, !15, i64 7524, !16, i64 7580, !17, i64 7620, !18, i64 7640, !20, i64 43772, !22, i64 43784, !23, i64 43788, !24, i64 54132, !25, i64 54140, !3, i64 54180, !26, i64 54308, !27, i64 54328, !28, i64 54352, !29, i64 54548, !30, i64 54656, !9, i64 54704, !3, i64 54708, !31, i64 54772, !32, i64 54900, !32, i64 54928, !33, i64 54956, !36, i64 55424, !37, i64 55444, !3, i64 55732, !9, i64 55736, !3, i64 55740, !3, i64 55744, !9, i64 55748, !9, i64 55752, !9, i64 55756, !9, i64 55760, !3, i64 55764, !12, i64 55768, !12, i64 55772, !12, i64 55776, !3, i64 55780, !3, i64 55781, !3, i64 55782, !3, i64 55783, !9, i64 55784, !2, i64 55788, !2, i64 55792, !2, i64 55796, !2, i64 55800, !2, i64 55804, !2, i64 55808, !2, i64 55812, !3, i64 55816}
!7 = !{!"api_function_table", !2, i64 0, !2, i64 4, !2, i64 8, !2, i64 12, !2, i64 16, !2, i64 20, !2, i64 24, !2, i64 28, !2, i64 32, !2, i64 36, !2, i64 40, !2, i64 44, !2, i64 48, !2, i64 52, !2, i64 56, !2, i64 60, !2, i64 64, !2, i64 68, !2, i64 72, !2, i64 76, !2, i64 80, !2, i64 84, !2, i64 88, !2, i64 92, !2, i64 96, !2, i64 100, !2, i64 104, !2, i64 108, !2, i64 112, !2, i64 116, !2, i64 120, !2, i64 124, !2, i64 128, !2, i64 132, !2, i64 136, !2, i64 140, !2, i64 144, !2, i64 148, !2, i64 152, !2, i64 156, !2, i64 160, !2, i64 164, !2, i64 168, !2, i64 172, !2, i64 176, !2, i64 180, !2, i64 184, !2, i64 188, !2, i64 192, !2, i64 196, !2, i64 200, !2, i64 204, !2, i64 208, !2, i64 212, !2, i64 216, !2, i64 220, !2, i64 224, !2, i64 228, !2, i64 232, !2, i64 236, !2, i64 240, !2, i64 244, !2, i64 248, !2, i64 252, !2, i64 256, !2, i64 260, !2, i64 264, !2, i64 268, !2, i64 272, !2, i64 276, !2, i64 280, !2, i64 284, !2, i64 288, !2, i64 292, !2, i64 296, !2, i64 300, !2, i64 304, !2, i64 308, !2, i64 312, !2, i64 316, !2, i64 320, !2, i64 324, !2, i64 328, !2, i64 332, !2, i64 336, !2, i64 340, !2, i64 344, !2, i64 348, !2, i64 352, !2, i64 356, !2, i64 360, !2, i64 364, !2, i64 368, !2, i64 372, !2, i64 376, !2, i64 380, !2, i64 384, !2, i64 388, !2, i64 392, !2, i64 396, !2, i64 400, !2, i64 404, !2, i64 408, !2, i64 412, !2, i64 416, !2, i64 420, !2, i64 424, !2, i64 428, !2, i64 432, !2, i64 436, !2, i64 440, !2, i64 444, !2, i64 448, !2, i64 452, !2, i64 456, !2, i64 460, !2, i64 464, !2, i64 468, !2, i64 472, !2, i64 476, !2, i64 480, !2, i64 484, !2, i64 488, !2, i64 492, !2, i64 496, !2, i64 500, !2, i64 504, !2, i64 508, !2, i64 512, !2, i64 516, !2, i64 520, !2, i64 524, !2, i64 528, !2, i64 532, !2, i64 536, !2, i64 540, !2, i64 544, !2, i64 548, !2, i64 552, !2, i64 556, !2, i64 560, !2, i64 564, !2, i64 568, !2, i64 572, !2, i64 576, !2, i64 580, !2, i64 584, !2, i64 588, !2, i64 592, !2, i64 596, !2, i64 600, !2, i64 604, !2, i64 608, !2, i64 612, !2, i64 616, !2, i64 620, !2, i64 624, !2, i64 628, !2, i64 632, !2, i64 636, !2, i64 640, !2, i64 644, !2, i64 648, !2, i64 652, !2, i64 656, !2, i64 660, !2, i64 664, !2, i64 668}
!8 = !{!"dd_function_table", !2, i64 0, !2, i64 4, !2, i64 8, !2, i64 12, !2, i64 16, !2, i64 20, !2, i64 24, !2, i64 28, !2, i64 32, !2, i64 36, !2, i64 40, !2, i64 44, !2, i64 48, !2, i64 52, !2, i64 56, !2, i64 60, !2, i64 64, !2, i64 68, !2, i64 72, !2, i64 76, !2, i64 80, !2, i64 84, !2, i64 88, !2, i64 92, !2, i64 96, !2, i64 100, !2, i64 104, !2, i64 108, !2, i64 112, !2, i64 116, !2, i64 120, !2, i64 124, !2, i64 128, !2, i64 132, !2, i64 136, !2, i64 140, !2, i64 144, !2, i64 148, !2, i64 152, !2, i64 156, !2, i64 160, !2, i64 164, !2, i64 168, !2, i64 172, !2, i64 176}
!9 = !{!"int", !3, i64 0}
!10 = !{!"gl_accum_attrib", !3, i64 0}
!11 = !{!"gl_colorbuffer_attrib", !9, i64 0, !3, i64 4, !9, i64 20, !9, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !12, i64 44, !3, i64 48, !3, i64 49, !3, i64 52, !3, i64 56, !3, i64 60, !3, i64 64, !3, i64 80, !3, i64 84, !3, i64 85, !3, i64 86, !3, i64 87}
!12 = !{!"float", !3, i64 0}
!13 = !{!"gl_current_attrib", !3, i64 0, !9, i64 16, !3, i64 20, !3, i64 32, !3, i64 48, !12, i64 64, !3, i64 68, !9, i64 84, !3, i64 88, !3, i64 104, !3, i64 105}
!14 = !{!"gl_depthbuffer_attrib", !3, i64 0, !12, i64 4, !3, i64 8, !3, i64 9}
!15 = !{!"gl_eval_attrib", !3, i64 0, !3, i64 1, !3, i64 2, !3, i64 3, !3, i64 4, !3, i64 5, !3, i64 6, !3, i64 7, !3, i64 8, !3, i64 9, !3, i64 10, !3, i64 11, !3, i64 12, !3, i64 13, !3, i64 14, !3, i64 15, !3, i64 16, !3, i64 17, !3, i64 18, !9, i64 20, !12, i64 24, !12, i64 28, !9, i64 32, !9, i64 36, !12, i64 40, !12, i64 44, !12, i64 48, !12, i64 52}
!16 = !{!"gl_fog_attrib", !3, i64 0, !3, i64 4, !12, i64 20, !12, i64 24, !12, i64 28, !12, i64 32, !3, i64 36}
!17 = !{!"gl_hint_attrib", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16}
!18 = !{!"gl_light_attrib", !3, i64 0, !19, i64 34304, !3, i64 34324, !3, i64 36084, !3, i64 36088, !3, i64 36092, !3, i64 36096, !9, i64 36100, !3, i64 36104, !2, i64 36108, !3, i64 36112, !3, i64 36116}
!19 = !{!"gl_lightmodel", !3, i64 0, !3, i64 16, !3, i64 17}
!20 = !{!"gl_line_attrib", !3, i64 0, !3, i64 1, !21, i64 2, !9, i64 4, !12, i64 8}
!21 = !{!"short", !3, i64 0}
!22 = !{!"gl_list_attrib", !9, i64 0}
!23 = !{!"gl_pixel_attrib", !3, i64 0, !12, i64 4, !12, i64 8, !12, i64 12, !12, i64 16, !12, i64 20, !12, i64 24, !12, i64 28, !12, i64 32, !12, i64 36, !12, i64 40, !9, i64 44, !9, i64 48, !3, i64 52, !3, i64 53, !12, i64 56, !12, i64 60, !9, i64 64, !9, i64 68, !9, i64 72, !9, i64 76, !9, i64 80, !9, i64 84, !9, i64 88, !9, i64 92, !9, i64 96, !9, i64 100, !3, i64 104, !3, i64 1128, !3, i64 2152, !3, i64 3176, !3, i64 4200, !3, i64 5224, !3, i64 6248, !3, i64 7272, !3, i64 8296, !3, i64 9320}
!24 = !{!"gl_point_attrib", !3, i64 0, !12, i64 4}
!25 = !{!"gl_polygon_attrib", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 13, !3, i64 16, !9, i64 20, !3, i64 24, !3, i64 25, !12, i64 28, !12, i64 32, !3, i64 36, !3, i64 37, !3, i64 38, !3, i64 39}
!26 = !{!"gl_scissor_attrib", !3, i64 0, !9, i64 4, !9, i64 8, !9, i64 12, !9, i64 16}
!27 = !{!"gl_stencil_attrib", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !3, i64 20, !3, i64 21, !3, i64 22, !3, i64 23}
!28 = !{!"gl_texture_attrib", !9, i64 0, !3, i64 4, !3, i64 8, !9, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !3, i64 44, !3, i64 60, !3, i64 76, !3, i64 92, !3, i64 108, !3, i64 124, !3, i64 140, !3, i64 156, !2, i64 172, !2, i64 176, !2, i64 180, !2, i64 184, !2, i64 188, !2, i64 192}
!29 = !{!"gl_transform_attrib", !3, i64 0, !3, i64 4, !3, i64 100, !3, i64 106, !3, i64 107}
!30 = !{!"gl_viewport_attrib", !9, i64 0, !9, i64 4, !9, i64 8, !9, i64 12, !12, i64 16, !12, i64 20, !12, i64 24, !12, i64 28, !12, i64 32, !12, i64 36, !12, i64 40, !12, i64 44}
!31 = !{!"gl_array_attrib", !9, i64 0, !3, i64 4, !9, i64 8, !9, i64 12, !2, i64 16, !3, i64 20, !3, i64 24, !9, i64 28, !9, i64 32, !2, i64 36, !3, i64 40, !9, i64 44, !3, i64 48, !9, i64 52, !9, i64 56, !2, i64 60, !3, i64 64, !3, i64 68, !9, i64 72, !9, i64 76, !2, i64 80, !3, i64 84, !9, i64 88, !3, i64 92, !9, i64 96, !9, i64 100, !2, i64 104, !3, i64 108, !9, i64 112, !9, i64 116, !2, i64 120, !3, i64 124}
!32 = !{!"gl_pixelstore_attrib", !9, i64 0, !9, i64 4, !9, i64 8, !9, i64 12, !9, i64 16, !9, i64 20, !3, i64 24, !3, i64 25}
!33 = !{!"gl_evaluators", !34, i64 0, !34, i64 20, !34, i64 40, !34, i64 60, !34, i64 80, !34, i64 100, !34, i64 120, !34, i64 140, !34, i64 160, !35, i64 180, !35, i64 212, !35, i64 244, !35, i64 276, !35, i64 308, !35, i64 340, !35, i64 372, !35, i64 404, !35, i64 436}
!34 = !{!"gl_1d_map", !9, i64 0, !12, i64 4, !12, i64 8, !2, i64 12, !3, i64 16}
!35 = !{!"gl_2d_map", !9, i64 0, !9, i64 4, !12, i64 8, !12, i64 12, !12, i64 16, !12, i64 20, !2, i64 24, !3, i64 28}
!36 = !{!"gl_feedback", !3, i64 0, !9, i64 4, !2, i64 8, !9, i64 12, !9, i64 16}
!37 = !{!"gl_selection", !2, i64 0, !9, i64 4, !9, i64 8, !9, i64 12, !9, i64 16, !3, i64 20, !3, i64 276, !12, i64 280, !12, i64 284}
