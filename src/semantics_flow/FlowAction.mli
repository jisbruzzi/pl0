type t=
|ReadVariable of int
|WriteVariable of int
|WriteVariableFromInput of int
|IntegerRead of string
|Operate of Operation.t
|PrintNewline
|PrintString of string
|PrintResult
|Return
|ReadVariableFromInput of int
|LabeledPosition of int
|JumpTo of int
|ConditionalJumpTo of int
|CallJump of int