type skip_label=int
type proc_label=int
type p_dict=(skip_label*proc_label) list
type general_state=Normal|AfterDeclaration of int
type state=(general_state*p_dict)

let with_proc_after(lj:int)(lp:int)(d:p_dict):p_dict=
  (lj,lp)::d

let without_proc_after(lj:int)(d:p_dict):p_dict=
  List.remove_assoc lj d

let process_skip_proc_label(l:int)(d:p_dict):(state*FlowAction.t list*FlowAction.t list)=
  match List.assoc_opt l d with
  |Some(proc_after)->
  ((Normal,without_proc_after l d),[FlowAction.SkipProcedureLabeledPosition l],[FlowAction.ProcedureDeclarationLabeledPosition proc_after])
  |None->((Normal,without_proc_after l d),[FlowAction.SkipProcedureLabeledPosition l],[])

let process(a:FlowAction.t)(s:state):(state*FlowAction.t list*FlowAction.t list)=
  let gs,d=s in match (a,gs,d) with
  | (FlowAction.ProcedureDeclarationLabeledPosition l,_,d)->((AfterDeclaration l,d),[],[])
  | (FlowAction.SkipProcedureJumpTo l_jump,AfterDeclaration l_proc,d)->((Normal,with_proc_after l_jump l_proc d),[],[])
  | (a,AfterDeclaration l_proc,d)->((Normal, d),[FlowAction.ProcedureDeclarationLabeledPosition l_proc],[a])

  | (FlowAction.SkipProcedureLabeledPosition l,_,d)->(process_skip_proc_label l d)
  | (_,_,_)->((Normal,d),[a],[])

let run(s:FlowAction.t Lazylist.gen_t):FlowAction.t Lazylist.gen_t=
  LazylistOps.run process s (Normal,[])