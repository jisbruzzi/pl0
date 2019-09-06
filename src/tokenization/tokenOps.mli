type token = Token.t

val string_of_token: token->string
val print_token:token->unit
val print_tokens:token Lazylist.gen_t -> unit
val get_ident_or_keyword:string->token

val print_tokens_coords:TokenWithCoords.t Lazylist.gen_t->unit
val print_token_coords:TokenWithCoords.t -> unit
val string_of_token_coords:TokenWithCoords.t -> string