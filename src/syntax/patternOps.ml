open Pattern
let rec string_of_pattern(p:t):string=
  match p with
  | Maybe(p)->"mb("^ (string_of_pattern p)  ^")?"
  | Sequence(p::tl)->(string_of_pattern p)^", "^(string_of_pattern (Sequence(tl)))
  | Sequence([])->"[]"
  | Match(_,_,label)->" MATCH "^label
  | Asterisk(p)->"asterisk("^ (string_of_pattern p) ^")*"
  | Or(p::tl)->(string_of_pattern p)^"|"^(string_of_pattern (Or(tl)))
  | Or([])->"[|]"
  | In(gen,label)-> " GENP "^label
  | Nothing -> "<>"
  | NoMatch -> "><"

let print_pattern p=print_string(string_of_pattern p)