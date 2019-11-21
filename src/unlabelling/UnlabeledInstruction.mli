type r=LabeledInstruction.r
type t=
|Jmp of int (* JMP pos *)
|MovToRegister of (r*int) (* MOV registro [EDI+nro de var] *)
|Push of r (* PUSH registro*)
|Pop of r (* POP registro*)
|MovToMemory of (int*r) (* MOV [EDI+nro de var] registro *)
|Call of int (* call pos *)
|MovConstant of (r*string) (* MOV registro, constante*)
|Ascii of string
|Ret
|Neg of r
|Add of(r*r)
|Sub of (r*r)
|Imul of (r)
|Xchg of (r*r)
|Cdq
|Idiv of r
|Cmp of (r*r)
|Jle of int
|Jl of int
|Je of int
|Jne of int
|Jpo of int
|Jge of int
|Jg of int
|TestAl