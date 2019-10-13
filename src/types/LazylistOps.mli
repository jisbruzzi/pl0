val map:('a->'b list)->('a Lazylist.gen_t)->('b Lazylist.gen_t)
val run:('a->'s->('s*'b list*'a list))->('a Lazylist.gen_t)->'s->('b Lazylist.gen_t)
val print:('a->string)->string->('a Lazylist.gen_t)->('a Lazylist.gen_t)
val pass:('a Lazylist.gen_t)->('a Lazylist.gen_t)
val run_all:('a Lazylist.gen_t)->unit
val stateless_run:('a->'b option)->('a Lazylist.gen_t)->('b Lazylist.gen_t)