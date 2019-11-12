let next_instructions(current:LabeledInstruction.t)(previous:LabeledInstruction.t option)=
  match previous with
  |None->[current]
  |Some(previous)->(
    match previous,current with
    | Push(Eax),Pop(Eax)->[]
    | Push(Eax),i->[Push(Eax);i]
    | _,Push(Eax)->[]
    | _,c->[c]
  )
let process(i:LabeledInstruction.t)(state:LabeledInstruction.t option):(LabeledInstruction.t option*LabeledInstruction.t list*LabeledInstruction.t list)=
  (Some(i),next_instructions i state,[])
let run(s:LabeledInstruction.t Lazylist.gen_t):LabeledInstruction.t Lazylist.gen_t=
  LazylistOps.run process s None