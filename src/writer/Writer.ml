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
    (List.init number_of_bytes (fun x -> x))
  )

let constant_encoding(constant:string):string=
  bytes_from_integer 4 (int_of_string constant)

let code_of_instruction(i:UnlabeledInstruction.t):string=
  let bi=bytes_from_integer 4 in
  let bs=bytes_from_string in
  let bc=bytes_from_integer 1 in
  match i with
  |Jmp(o)->(bs "E9")^(bi o)
  |Call(o)->(bs "E8")^(bi o)
  |MovToRegister(Eax,v)->(bs "8B87")^(bi (v*4))
  |MovToMemory(v,Eax)->(bs "8987")^(bi (v*4))
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
  |Imul(Ebx)->(bs "F7EB")
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
  |TestAl->(bs "A801")
  |_->"9999"(*UnlabeledInstructionOps.to_string i*)

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
  let program_beginning=134513792 in
  (bytes_from_string "BF") ^ (bytes_from_integer 4 (program_beginning+program_length_bytes + 5))

let generate_zeroes_for_vars(max_var_id:int option):string= 
  let bytes_num=(match max_var_id with None->0|Some(x)->x+1)*4 in
  String.make bytes_num (Char.chr 0)

(*En este momento, es necesario ajustar los campos FileSize (posiciones
68-71, o 0044-0047 en hexadecimal) y MemorySize (posiciones 72-75, o
0048-004B en hexadecimal), colocando allí el tamaño final del archivo
ejecutable.

Por último, se debe realizar el ajuste del campo Size del encabezado
de la sección text (posiciones 201-204, o 00C9-00CC en hexadecimal),
colocando allí el tamaño de la sección text.

*)

let replace(base:string)(pos:int)(replaced:string):string=
  let base_len=String.length base in
  let replaced_len=String.length replaced in
  let before=String.sub base 0 pos in
  let after=String.sub base (pos+replaced_len) (base_len-pos-replaced_len) in
  (before^replaced^after)

let correct_program(program:string)(full_size:int):string=
  let text_size=full_size-224 in
  let full_size=(bytes_from_integer 4 full_size) in
  let program = replace program 68 full_size in (* fixed_file_size *)
  let program = replace program 72 full_size in (* fixed_memory_size *)
  let program = replace program 201 (bytes_from_integer 4 text_size) in (* largo sección text *)
  program
  
let run(i:UnlabeledInstruction.t Lazylist.gen_t):string=
  let result = LazylistOps.summarize summarizer i {program="";max_var_id=None} in 
  let program = String.concat "" [
    (bytes_from_string Header.header);
    generate_mov_edi (String.length result.program);
    result.program;
    generate_zeroes_for_vars result.max_var_id
  ] in
  correct_program program (String.length program)
    
