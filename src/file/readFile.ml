
let rec print_file(f:string Lazylist.gen_t)=
  match (f())with
  | Empty -> ()
  | Cons(s,lst)->(print_string s);(print_file lst)

let read_lazy_file (name:string) : char Lazylist.gen_t = 
  let file = open_in name in
  let stream = Stream.of_channel file in
  let rec generator=
    fun () -> try
      Lazylist.Cons( (Stream.next stream),generator)
    with Stream.Failure -> (close_in file;Lazylist.Empty)
  in generator

let log_lines_and_pass(f:char Lazylist.gen_t):char Lazylist.gen_t=
  let file = open_out "salida.txt" in
  let escribir = output_char file in
  let escribir_nro = (fun l -> output_string file ((string_of_int l)^":")) in
  let rec generator = fun (linea:int) (f_current:char Lazylist.gen_t):char Lazylist.gen_t ->(
    match f_current() with
    |Cons('\n',lst)-> 
      (escribir '\n');
      (escribir_nro linea);
      (fun() -> Lazylist.Cons('\n',(generator (linea+1) lst)))
    |Cons(c,lst)-> 
      (escribir c);
      (fun() -> Lazylist.Cons(c,(generator linea lst)))
    |Empty->(fun ()->Lazylist.Empty)
  ) in (escribir_nro 1);generator 1 f
