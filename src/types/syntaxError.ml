type t = NoError|TokenExpected of Token.t|IdentifierExpected|StringExpected|IntegerExpected|NothingExpected|NoMatch
exception SyntaxException of (t*TokenWithCoords.t)

let string_of_error(e:t):string=
match e with
|NoError -> "NoError"
|TokenExpected(t) -> "TokenExpected: ´"^TokenOps.string_of_token(t)^"'"
|IdentifierExpected -> "IdentifierExpected"
|StringExpected -> "StringExpected"
|IntegerExpected -> "IntegerExpected"
|NothingExpected -> "NothingExpected"
|NoMatch->"NoMatch"