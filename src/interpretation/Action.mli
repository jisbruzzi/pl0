type t=
|DeclareVariable of string
|DeclareConstant of string*string
|DeclareProcedure of string
|ReadVariable of string
|CallProcedure of string
|ReadVariableOrConstant of string
|WriteVariable of string
|BeginContext
|EndContext
|OkThen
|EndIfBlock
|WriteVariableFromInput of string
|EndWhileBlock