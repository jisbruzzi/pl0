type p_dict=(int*int) list
type general_state = NormalState|AfterLabeledPosition of int
type state=(general_state*p_dict)

let add_to_dict(l_jump:int)(l_position:int)(d:p_dict):p_dict=
  match (List.assoc_opt l_position d) with
  | None->(l_jump,l_position)::d
  | Some(previous_l_jump)->(l_jump,previous_l_jump)::(List.remove_assoc l_jump d)
let corresponding_position(d:p_dict)(l_position:int)=
  List.assoc_opt l_position d

let process(a:FlowAction.t)(s:state):(state*FlowAction.t list*FlowAction.t list)=
  (*print_string (FlowActionOps.to_string a);*)
  let gs,dict=s in
  match (a,gs,dict) with
  | (FlowAction.SkipProcedureLabeledPosition l,NormalState,d)->((AfterLabeledPosition l,d),[],[])
  | (_,NormalState,d)->((NormalState,d),[a],[])
  | (FlowAction.SkipProcedureJumpTo l_jump,AfterLabeledPosition l_position,d)->((NormalState,add_to_dict l_jump l_position d),[],[])
  | (_,AfterLabeledPosition l_position,d)->(
    match (corresponding_position dict l_position) with
    | None-> ((NormalState,d),[FlowAction.SkipProcedureLabeledPosition (l_position);a],[])
    |Some(p)->((NormalState,d),[FlowAction.SkipProcedureLabeledPosition (p);a],[])
  )
  
  

let run(s:FlowAction.t Lazylist.gen_t):FlowAction.t Lazylist.gen_t=
  LazylistOps.run process s (NormalState,[])