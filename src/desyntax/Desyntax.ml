type desyntax_state=SyntaxLabel.t list

let generate_exits lst=
  List.map (fun (x)->ContextChange.Exits(x)) lst

let generate_arrivals lst=
  List.map (fun (x)->ContextChange.Arrives(x)) lst

let rec label_changes (new_labels:SyntaxLabel.t list)(old_labels:SyntaxLabel.t list):ContextChange.t list=
  match new_labels,old_labels with
  | (new_hd::new_tl,old_hd::old_tl) when new_hd=old_hd -> label_changes new_tl old_tl
  | _ -> List.concat [ generate_exits old_labels; generate_arrivals new_labels ]

let desyntax(tl:TokenWithLabels.t)(s:desyntax_state):(desyntax_state*ContextChange.t list*TokenWithLabels.t list)=
  let labels,token=tl in
  let context_changes=label_changes labels s in
  (labels,List.concat [ context_changes; [ContextChange.Passes(tl)] ],[])


let run(tokens_with_labels:TokenWithLabels.t Lazylist.gen_t):ContextChange.t Lazylist.gen_t=
  LazylistOps.run desyntax tokens_with_labels [] 