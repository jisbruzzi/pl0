type token=Token.t

type t = 
|Maybe of t
|Sequence of t list
|Match of ((token -> bool)*SyntaxError.t)
|Asterisk of t
|Or of t list
|In of (unit->t)