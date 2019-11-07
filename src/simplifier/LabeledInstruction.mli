type r=Eax|Ebx|Edx
type t=
|Jmp of int (* JMP label *)
|MovToRegister of (r*int) (* MOV registro [EDI+nro de var] *)
|Push of r (* PUSH registro*)
|Pop of r (* POP registro*)
|MovToMemory of (int*r) (* MOV [EDI+nro de var] registro *)
|Call of int (* call label *)
|MovConstant of (r*string) (* MOV registro, constante*)
|CallPrintNewLine
|Ascii of string
|CallPrintString
|Ret
|CallScanfPreparation
|JumpScanfInterruption
|Position of int
|Neg of r
|Add of(r*r)
|Sub of (r*r)
|Imul of (r*r)
|Idiv of r
|Cmp of (r*r)
|Jle of int
|Jl of int
|Je of int
|Jne of int
|Jpo of int
|Jge of int
|Jg of int