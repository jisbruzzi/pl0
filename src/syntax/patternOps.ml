open Pattern
let rec string_of_pattern(p:t):string=
  match p with
  | Maybe(p)->"mb("^ (string_of_pattern p)  ^")?"
  | Sequence(lst)->
    "lst("^String.concat ", " (List.map string_of_pattern (lst))^")lst"
  | Match(_,_,label)->" MATCH "^label
  | Asterisk(p)->"asterisk("^ (string_of_pattern p) ^")*"
  | Or(lst)->"or("^String.concat "|" (List.map string_of_pattern (lst))^")or"
  | In(gen,label)-> " GENP "^label

let print_pattern p=print_string(string_of_pattern p)