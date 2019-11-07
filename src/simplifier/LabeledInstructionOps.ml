let string_of_registry(r)=
  match r with
  |LabeledInstruction.Eax->"Eax"
  |LabeledInstruction.Ebx->"Ebx"
  |LabeledInstruction.Edx->"Edx"

let to_string(i:LabeledInstruction.t):string=
  match i with
  |Jmp(l)->"Jmp "^(string_of_int l)
  |MovToRegister(r,l)->"MovToRegister "^(string_of_int l)^" "^(string_of_registry r)
  |Push(r)->"Push "^(string_of_registry r)
  |Pop(r)->"Pop "^(string_of_registry r)
  |MovToMemory(v,r)->"MovToMemory "^(string_of_int v)^" "^(string_of_registry r)
  |Call(l)->"Call "^(string_of_int l)
  |MovConstant(r,c)->"MovConstant "^(string_of_registry r)^" "^c
  |CallPrintNewLine->"CallPrintNewLine "
  |Ascii(s)->"Ascii "^s
  |CallPrintString->"CallPrintString "
  |Ret->"Ret "
  |Position(l)->"Position "^(string_of_int l)
  |CallScanfPreparation->"CallScanfPreparation"
  |JumpScanfInterruption->"JumpScanfInterruption"
  |Neg(r)->"Neg "^(string_of_registry r)
  |Add(r1,r2)->"Add "^(string_of_registry r1)^(string_of_registry r2)
  |Sub(r1,r2)->"Sub "^(string_of_registry r1)^(string_of_registry r2)
  |Imul(r1,r2)->"Imul "^(string_of_registry r1)^(string_of_registry r2)
  |Idiv(r)->"Idiv "^(string_of_registry r)
  |Cmp(r1,r2)->"CMP "^(string_of_registry r1)^" "^(string_of_registry r2)
  |Jle(l)->"Jle "^(string_of_int l)
  |Jl(l)->"Jl "^(string_of_int l)
  |Je(l)->"Je "^(string_of_int l)
  |Jne(l)->"Jne "^(string_of_int l)
  |Jpo(l)->"Jpo "^(string_of_int l)
  |Jge(l)->"Jge "^(string_of_int l)
  |Jg(l)->"Jg "^(string_of_int l)