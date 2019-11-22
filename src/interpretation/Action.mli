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
|OkDo
|EndIfBlock
|WriteVariableFromInput of string
|BeginWhileBlock
|EndWhileBlock
|IntegerRead of string
|Operate of Operation.t
|PrintNewline
|PrintString of string
|PrintResult
|BeginRepeatUntilBlock
|EndRepeatUntilBlock