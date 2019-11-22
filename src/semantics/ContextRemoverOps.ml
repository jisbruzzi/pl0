
let string_of_contextualized(a:ContextualizedAction.t):string=
  let i=string_of_int in 
  match a with
  |DeclareProcedure(id)->"DeclareProcedure"^(i id)
  |ReadVariable(id)->"ReadVariable"^(i id)
  |CallProcedure(id)->"CallProcedure"^(i id)
  |WriteVariable(id)->"WriteVariable"^(i id)
  |OkThen->"OkThen"
  |OkDo->"OkDo"
  |EndIfBlock->"EndIfBlock"
  |WriteVariableFromInput(id)->"WriteVariableFromInput"^(i id)
  |BeginWhileBlock->"BeginWhileBlock"
  |EndWhileBlock->"EndWhileBlock"
  |IntegerRead(s)->"IntegerRead"^s
  |Operate(op)->"Operate "^(OperationOps.string_of_operation op)
  |PrintNewline->"PrintNewline"
  |PrintString(s)->"PrintString"
  |PrintResult->"PrintResult"
  |Return->"Return"
  |BeginRepeatUntilBlock->"BeginRepeatUntilBlock"
  |EndRepeatUntilBlock->"EndRepeatUntilBlock"