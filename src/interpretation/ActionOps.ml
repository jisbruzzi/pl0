let string_of_action(a:Action.t):string=
  match a with
  |DeclareVariable(v)->"DeclareVariable:"^v
  |DeclareConstant(v,i)-> "DeclareConstant:"^v^"="^i
  |DeclareProcedure(v)->"DeclareProcedure:"^v
  |ReadVariable(v)->"ReadVariable:"^v
  |ReadVariableOrConstant(v)->"ReadVariableOrConstant:"^v
  |BeginContext->"BeginContext"
  |EndContext->"EndContext"
  |CallProcedure(name)->"CallProcedure:"^name
  |WriteVariable(name)->"WriteVariable:"^name
  |OkThen->"OkThen"
  |EndIfBlock->"EndIfBlock"
  |EndWhileBlock->"EndWhileBlock"
  |WriteVariableFromInput(s)->"WriteVariableFromInput "^s
  |IntegerRead(s)->"IntegerRead "^s
  |Operate(op)->"Operate " ^ (OperationOps.string_of_operation op)
  |PrintNewline->"PrintNewline"
  |PrintResult->"PrintResult"
  |PrintString(s)->"PrintString"^s

