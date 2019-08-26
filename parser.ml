type programa = 
  | ProgramaVacio
type parseo = 
  | ParseoVacio
  | ParseoConString of programa * string
type 
let crear_parseo_con_string (p:programa) (tokens:string):parseo =
  match tokens with
  | "CONST" -> (print_string "!!TENGO CONST!!!"; ParseoConString (p,""))
  | e -> (print_string "eee"; ParseoConString(p,e) )


let agregar_char parseo (c:string) =
  print_string c;
  match parseo with
  | ParseoVacio -> crear_parseo_con_string ProgramaVacio c
  | ParseoConString (p, l) -> crear_parseo_con_string p (l^c)

let rec parsear_in_channel (ic:in_channel)(p:parseo)(largo:int) = 
  if largo > 0 then
    let nuevo_parseo=really_input_string ic 1 |> agregar_char p in parsear_in_channel ic nuevo_parseo (largo-1)
  else
    p

let () =
  let ic = open_in "hola.pl0" in 
  let p = parsear_in_channel ic ParseoVacio (in_channel_length ic) in
  match p with
  | ParseoVacio ->()
  | ParseoConString (p,l) -> print_string l