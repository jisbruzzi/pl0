let to_string(a:FlowAction.t):string=
match a with
|PrintString(str)->"PrintString "^str
|IntegerRead(str)->"IntegerRead "^str
|ReadVariable(id)->"ReadVariable "^(string_of_int id)
|WriteVariable(id)->"WriteVariable "^(string_of_int id)
|WriteVariableFromInput(id)->"WriteVariableFromInput "^(string_of_int id)
|ReadVariableFromInput(id)->"ReadVariableFromInput "^(string_of_int id)
|LabeledPosition(id)->"LabeledPosition "^(string_of_int id)
|JumpTo(id)->"JumpTo "^(string_of_int id)
|ConditionalJumpTo(id)->"ConditionalJumpTo "^(string_of_int id)
|CallJump(id)->"CallJump "^(string_of_int id)
|Operate(op)->"Operate"
|PrintResult->"PrintResult"
|PrintNewline->"PrintNewline"
|Return->"Return"