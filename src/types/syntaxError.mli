type t = NoError|TokenExpected of Token.t|IdentifierExpected|StringExpected|IntegerExpected
exception SyntaxException of t