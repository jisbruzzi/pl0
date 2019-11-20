type ret_tpye={
  program:string;
  max_var_id:int option
}

let bytes_from_integer(number_of_bytes:int)(conv:int)=
  (Printf.sprintf "%x" (-20))(* LABURAR ESTOOOOO *)

let code_of_instruction(i:UnlabeledInstruction.t):string=
  let bi=bytes_from_integer 4 in
  let bs=bytes_from_string in
  let bc=bytes_from_integer 2 in
  match i with
  |Jmp(o)->(bs "E9")^(bi o)
  |Call(o)->(bs "E8")^(bi o)
  |MovToRegister(Eax,v)->(bs "8B87")^(bi v)
  |MovToMemory(v,Eax)->(bs "8987")^(bi v)
  |MovConstant(Eax,c)->(bs "B8")^(constant_encoding c)
  |MovConstant(Ecx,c)->(bs "B9")^(constant_encoding c)
  |MovConstant(Edx,c)->(bs "BA")^(constant_encoding c)
  |Push(Eax)->(bs "50")
  |Pop(Eax)->(bs "58")
  |Pop(Ebx)->(bs "5B")
  |Ascii(s)->s
  |Ret->(bs "C3")
  |Neg(Eax)->(bs "F7D8")
  |Add(Eax,Ebx)->(bs "01D8")
  |Sub(Eax,Ebx)->(bs "29D8")
  |Imul(Eax,Ebx)->(bs "F7EB")
  |Xchg(Eax,Ebx)->(bs "93")
  |Cdq->(bs "99")
  |Idiv(Ebx)->(bs "F7FB")
  |Cmp(Eax,Ebx)->(bs "39C3")
  |Jle(o)->(bs "7E")^(bc o)
  |Jl(o)->(bs "7C")^(bc o)
  |Je(o)->(bs "74")^(bc o)
  |Jne(o)->(bs "75")^(bc o)
  |Jpo(o)->(bs "7B")^(bc o)
  |Jge(o)->(bs "7D")^(bc o)
  |Jg(o)->(bs "7F")^(bc o)
  |_->"99999"

let new_max_var_id(current:int option)(i:UnlabeledInstruction.t):int option=
  let with_id=fun(v:int)->(
    match current with
    | None->Some(v)
    | Some(vv) when vv > v->Some(vv)
    | _->current
  ) in
  match i with
  |MovToRegister(r,v)->with_id v
  |MovToMemory(v,r)->with_id v
  |_->current

let summarizer(i:UnlabeledInstruction.t)(s:ret_tpye):ret_tpye=
  {
    program=s.program^code_of_instruction i;
    max_var_id=new_max_var_id s.max_var_id i
  }


(* NECESITO QUE RUN TAMBIÉN ESCUPA LA CANTIDAD DE VARIABLES Y LA POSICIÓN DE LAS VARIABLES*)
let run(i:UnlabeledInstruction.t Lazylist.gen_t):ret_tpye=
  LazylistOps.summarize summarizer i {program="";max_var_id=None}
  