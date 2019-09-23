

let string_of_token_with_label(t:TokenWithLabels.t):string=
  let (labels,token)=t in
  let labels=String.concat "/" (List.map SyntaxLabelOps.string_of_label labels) in
  let token=TokenOps.string_of_token_coords token in
  labels^":"^token

let print_token_with_label(t:TokenWithLabels.t):unit=
  print_string (string_of_token_with_label t^"\n")

let rec print_lazylist(input:TokenWithLabels.t Lazylist.gen_t):TokenWithLabels.t Lazylist.gen_t=
  match input () with
  | Empty->input
  | Cons(hd,gen)->(
    print_token_with_label hd;
    print_lazylist gen
  )