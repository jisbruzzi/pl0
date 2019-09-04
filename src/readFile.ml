let read_lazy_file (name:string) : char Lazylist.gen_t = 
  let file = open_in name in
  let stream = Stream.of_channel file in
  let rec generator=
    fun () -> try
      Lazylist.Cons( (Stream.next stream),generator)
    with Stream.Failure -> (close_in file;Lazylist.Empty)
  in generator