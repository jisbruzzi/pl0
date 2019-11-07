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
|LabeledPosition of int
|JumpTo of int
|ConditionalJumpTo of int
|CallJump of int
|SkipProcedureJumpTo of int
|SkipProcedureLabeledPosition of int
|ProcedureDeclarationLabeledPosition of int