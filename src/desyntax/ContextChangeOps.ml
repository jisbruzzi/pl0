let as_string(cc:ContextChange.t):string=
  match cc with
  | Arrives(sl)->"arrives:"^SyntaxLabelOps.string_of_label sl
  | Exits(sl)->"exits: "^SyntaxLabelOps.string_of_label sl
  | Passes(l,tc)->"passes: "^SyntaxLabelOps.string_of_label l^" "^TokenOps.string_of_token_coords tc