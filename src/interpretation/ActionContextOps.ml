let string_of_action_context(c:ActionContext.t):string=
  match c with
  |DeclarationsContext({procedures;variables;constants;})->
  String.concat " " (List.concat [procedures;variables;constants])