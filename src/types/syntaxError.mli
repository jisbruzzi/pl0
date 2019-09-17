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

val string_of_error:t->string