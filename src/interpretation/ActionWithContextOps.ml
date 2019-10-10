let string_of_action_with_context(a:ActionWithContext.t):string=
  match a with
  |{action;contexts}->(
    let action=ActionOps.string_of_action action in
    let contexts=List.map ActionContextOps.string_of_action_context contexts in
    let contexts=String.concat " -- " contexts in
    action ^":"^contexts
  )