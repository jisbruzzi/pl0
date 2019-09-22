type token=Token.t

type t = 
|Match of ((token -> bool)*SyntaxError.t*string)
|In of ((unit->t)*string)
|Maybe of t
|Asterisk of t
|Sequence of t list
|Or of t list
|Labeled of SyntaxLabel.t*t