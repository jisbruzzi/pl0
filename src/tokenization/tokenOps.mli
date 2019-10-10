type token = Token.t

val string_of_token: token->string
val get_ident_or_keyword:string->token
val string_of_token_coords:TokenWithCoords.t -> string