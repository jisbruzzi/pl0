let reg=LabeledInstructionOps.string_of_registry
let to_string(ui:UnlabeledInstruction.t):string=
  match ui with
  |Push(r)->"Push "^reg(r)
  |Pop(r)->"Pop "^reg(r)
  |Neg(r)->"Neg "^reg(r)
  |Idiv(r)->"Idiv "^reg(r)
  |MovToRegister(r,p)->"MovToRegister "^string_of_int p^" "^reg(r)
  |MovToMemory(p,r)->"MovToMemory "^string_of_int p^" "^reg(r)
  |MovConstant(r,s)->"MovConstant "^reg(r)^" "^s
  |Ascii(s)->"Ascii "^s
  |Ret->"Ret"
  |Add(r1,r2)->"Add "^reg r1^" "^reg r2
  |Sub(r1,r2)->"Sub "^reg r1^" "^reg r2
  |Imul(r1)->"Imul "^reg r1
  |Xchg(r1,r2)->"Xchg "^reg r1^" "^reg r2
  |Cmp(r1,r2)->"Cmp "^reg r1^" "^reg r2
  |Cdq->"Cdq"
  |Jle(p)->"Jle "^(string_of_int p)
  |Jl(p)->"Jl "^(string_of_int p)
  |Je(p)->"Je "^(string_of_int p)
  |Jne(p)->"Jne "^(string_of_int p)
  |Jpo(p)->"Jpo "^(string_of_int p)
  |Jge(p)->"Jge "^(string_of_int p)
  |Jg(p)->"Jg "^(string_of_int p)
  |Jmp(p)->"Jmp "^(string_of_int p)
  |Call(p)->"Call "^(string_of_int p)
  |TestAl->"TestAl "