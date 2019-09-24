let string_of_label(l:SyntaxLabel.t)=
  match l with
  |ConstantDeclarations->"ConstantDeclarations"
  |ConstantDeclaration->"ConstantDeclaration"
  |ConstantValue->"ConstantValue"
  |VariableDeclarations->"VariableDeclarations"
  |VariableDeclaration->"VariableDeclaration"
  |ProcedureDeclaration->"ProcedureDeclaration"
  |ProgramRoot->"ProgramRoot"
  |Block->"Block"
  |Proposition->"Proposition"
  |Expression->"Expression"
  |VariableAssign->"VariableAssign"
  |ConstOrVarRead->"ConstOrVarRead"
  |VariableAssignFromReadln->"VariableAssignFromReadln"
  |ProcedureNameDeclaration->"ProcedureNameDeclaration"
  |ProcedureNameCall->"ProcedureNameCall"