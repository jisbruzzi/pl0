val run:('a->'s->('s*'b option))->('a Lazylist.gen_t)->'s->('b Lazylist.gen_t)
val print:('a->string)->string->('a Lazylist.gen_t)->('a Lazylist.gen_t)
val pass:('a Lazylist.gen_t)->('a Lazylist.gen_t)
val run_all:('a Lazylist.gen_t)->unit