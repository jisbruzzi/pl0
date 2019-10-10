

let string_of_token_with_label(t:TokenWithLabels.t):string=
  let (labels,token)=t in
  let labels=String.concat "/" (List.map SyntaxLabelOps.string_of_label labels) in
  let token=TokenOps.string_of_token_coords token in
  labels^":"^token

let print_token_with_label(t:TokenWithLabels.t):unit=
  print_string (string_of_token_with_label t^"\n")