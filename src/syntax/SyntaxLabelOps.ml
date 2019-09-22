let string_of_label(l:SyntaxLabel.t)=
  match l with
  |ConstantDeclarations -> "ConstantDeclarations"
  |ConstantName -> "ConstantName"
  |ConstantValue -> "ConstantValue"
  |VariableDeclarations -> "VariableDeclarations"
  |VariableName -> "VariableName"
  |ProcedureDeclaration -> "ProcedureDeclaration"
  |ProcedureName -> "ProcedureName"
  |ProcedureBlock -> "ProcedureBlock"