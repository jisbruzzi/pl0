type t = NoError 
|TokenExpected of Token.t
|IdentifierExpected
|StringExpected
|IntegerExpected
|NothingExpected
|NoMatch of t
|InvalidPattern
|AlternativeErrors of t list

exception SyntaxException of (t*TokenWithCoords.t)

let rec string_of_error(e:t):string=
match e with
|NoError -> "NoError"
|TokenExpected(t) -> "TokenExpected: ´"^TokenOps.string_of_token(t)^"'"
|IdentifierExpected -> "IdentifierExpected"
|StringExpected -> "StringExpected"
|IntegerExpected -> "IntegerExpected"
|NothingExpected -> "NothingExpected"
|NoMatch(e)->"NoMatch:"^string_of_error e
|InvalidPattern -> "InvalidPattern"
|AlternativeErrors(l) -> "AlternativeErrors:"^String.concat "|" (List.map string_of_error l)