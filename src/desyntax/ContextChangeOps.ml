let as_string(cc:ContextChange.t):string=
  match cc with
  | Arrives(sl)->"arrives:"^SyntaxLabelOps.string_of_label sl
  | Exits(sl)->"exits: "^SyntaxLabelOps.string_of_label sl
  | Passes(t)->"passes: "^TokenWithLabelsOps.string_of_token_with_label t