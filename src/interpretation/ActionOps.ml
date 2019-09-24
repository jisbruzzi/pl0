let string_of_action(a:Action.t):string=
  match a with
  |DeclareVariable(v)->"DeclareVariable:"^v
  |DeclareConst(v,i)-> "DeclareConst:"^v
  |DeclareProc(v)->"DeclareProc:"^v
  |ReadVariable(v)->"ReadVariable:"^v
  |ReadVariableOrConstant(v)->"ReadVariableOrConstant:"^v
