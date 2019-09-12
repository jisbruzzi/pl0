type t = NoError|TokenExpected of Token.t|IdentifierExpected|StringExpected|IntegerExpected|NothingExpected|NoMatch
exception SyntaxException of (t*TokenWithCoords.t)

val string_of_error:t->string