-- This file has been generated by specsgen. Do not modify by hand!

module Unison.Target.MSP430.SpecsGen.Itinerary (itinerary) where
import Unison.Target.MSP430.SpecsGen.MSP430InstructionDecl
import Unison.Target.MSP430.SpecsGen.MSP430ItineraryDecl
itinerary i
  | i `elem`
      [ADC16mi, ADC16mm, ADC16mr, ADC16ri, ADC16rm, ADC16rr, ADC8mi,
       ADC8mm, ADC8mr, ADC8ri, ADC8rm, ADC8rr, ADD16mi, ADD16mm, ADD16mr,
       ADD16ri, ADD16rm, ADD16rm_POST, ADD16rr, ADD8mi, ADD8mm, ADD8mr,
       ADD8ri, ADD8rm, ADD8rm_POST, ADD8rr, ADJCALLSTACKDOWN,
       ADJCALLSTACKUP, AND16mi, AND16mm, AND16mr, AND16ri, AND16rm,
       AND16rm_POST, AND16rr, AND8mi, AND8mm, AND8mr, AND8ri, AND8rm,
       AND8rm_POST, AND8rr, BIC16mm, BIC16mr, BIC16rm, BIC16rr, BIC8mm,
       BIC8mr, BIC8rm, BIC8rr, BIT16mi, BIT16mm, BIT16mr, BIT16ri,
       BIT16rm, BIT16rr, BIT8mi, BIT8mm, BIT8mr, BIT8ri, BIT8rm, BIT8rr,
       BUNDLE, Bi, Bm, Br, CALLi, CALLm, CALLr, CFI_INSTRUCTION, CMP16mi,
       CMP16mr, CMP16ri, CMP16rm, CMP16rr, CMP8mi, CMP8mr, CMP8ri, CMP8rm,
       CMP8rr, COPY, COPY_TO_REGCLASS, DBG_VALUE, EH_LABEL,
       EXTRACT_SUBREG, FAULTING_LOAD_OP, GC_LABEL, IMPLICIT_DEF,
       INLINEASM, INSERT_SUBREG, JCC, JMP, KILL, LIFETIME_END,
       LIFETIME_START, LOAD_STACK_GUARD, LOCAL_ESCAPE, Load, MOV16mi,
       MOV16mm, MOV16mr, MOV16ri, MOV16rm, MOV16rm_POST, MOV16rr, MOV8mi,
       MOV8mm, MOV8mr, MOV8ri, MOV8rm, MOV8rm_POST, MOV8rr, MOVZX16rm8,
       MOVZX16rr8, Move, NOP, OR16mi, OR16mm, OR16mr, OR16ri, OR16rm,
       OR16rm_POST, OR16rr, OR8mi, OR8mm, OR8mr, OR8ri, OR8rm, OR8rm_POST,
       OR8rr, PATCHPOINT, PHI, POP16r, PUSH16r, REG_SEQUENCE, RET, RETI,
       SAR16r1, SAR16r1c, SAR8r1, SAR8r1c, SBC16mi, SBC16mm, SBC16mr,
       SBC16ri, SBC16rm, SBC16rr, SBC8mi, SBC8mm, SBC8mr, SBC8ri, SBC8rm,
       SBC8rr, SEXT16r, SHL16r1, SHL8r1, STACKMAP, STATEPOINT, SUB16mi,
       SUB16mm, SUB16mr, SUB16ri, SUB16rm, SUB16rm_POST, SUB16rr, SUB8mi,
       SUB8mm, SUB8mr, SUB8ri, SUB8rm, SUB8rm_POST, SUB8rr, SUBREG_TO_REG,
       SWPB16r, Select16, Select8, Shl16, Shl8, Sra16, Sra8, Srl16, Srl8,
       Store, XOR16mi, XOR16mm, XOR16mr, XOR16ri, XOR16rm, XOR16rm_POST,
       XOR16rr, XOR8mi, XOR8mm, XOR8mr, XOR8ri, XOR8rm, XOR8rm_POST,
       XOR8rr, ZEXT16r]
    = NoItinerary

