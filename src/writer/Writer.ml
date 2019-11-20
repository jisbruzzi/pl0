type ret_tpye={
  program:string;
  max_var_id:int option
}

let char_part_of_int(significance:int)(conv:int):char=
  let offset=(8*significance) in
  let i = (conv land (255 lsl offset)) lsr offset in
  Char.chr i
  
  

let hex_value(c:char):int=
  match c with
  | '0'->0
  | '1'->1
  | '2'->2
  | '3'->3
  | '4'->4
  | '5'->5
  | '6'->6
  | '7'->7
  | '8'->8
  | '9'->9
  | 'A'->10
  | 'B'->11
  | 'C'->12
  | 'D'->13
  | 'E'->14
  | 'F'->15
  | _->10000000

let char_from_hexa(high:char)(low:char):char=
  let i=(((hex_value high) lsl 4) lor (hex_value low))in
  Char.chr i
  

let rec bytes_from_string(s:string):string=
  match String.length s with
  | 0->""
  | 1->"999"
  | 2->String.make 1 (char_from_hexa (String.get s 0) (String.get s 1))
  | l->(bytes_from_string (String.sub s 0 2))^(bytes_from_string (String.sub s 2 (l-2)))

let bytes_from_integer(number_of_bytes:int)(conv:int):string=
  String.concat "" (List.map 
    (fun(s:int):string->(
      String.make 1 (char_part_of_int s conv)
    ))
    (List.rev (List.init number_of_bytes (fun x -> x)))
  )

let constant_encoding(constant:string):string=
  bytes_from_integer 4 (int_of_string constant)

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


let generate_mov_edi program_length_bytes=
  let preferred_position=50 in
  (bytes_from_string "BF") ^ (bytes_from_integer 4 (preferred_position+program_length_bytes))

let generate_zeroes_for_vars(max_var_id:int option):string= 
  let bytes_num=(match max_var_id with None->0|Some(x)->x+1) in
  String.make bytes_num (Char.chr 0)

(* NECESITO QUE RUN TAMBIÉN ESCUPA LA CANTIDAD DE VARIABLES Y LA POSICIÓN DE LAS VARIABLES*)
let run(i:UnlabeledInstruction.t Lazylist.gen_t):string=
  let result = LazylistOps.summarize summarizer i {program="";max_var_id=None} in 
  String.concat "" [
    (bytes_from_string Header.header);
    generate_mov_edi (String.length result.program);
    result.program;
    generate_zeroes_for_vars result.max_var_id
  ]
    
