type t=
|DeclareVariable of string
|DeclareConst of string*int
|DeclareProc of string
|ReadVariable of string
|ReadVariableOrConstant of string
