let length_of_instruction(s:LabeledInstruction.t):int=
  match s with
  |Jmp(_)|JumpToSkipString(_)|JumpToExit->5
  |StoreStringPositionInEcx|StoreStringLengthInEdx(_)->5
  |Push(_)->1
  |Pop(_)->1
  |MovToRegister(_)|MovToMemory(_)->6
  |Call(_)|CallPrintNewLine|CallScanf|CallPrintString|CallPrintResult->5
  |MovConstant(_)->5
  |Ascii(s)->String.length s
  |Ret->1
  |Position(_)->0
  |Neg(_)|Add(_)|Sub(_)|Imul(_)|Idiv(_)|Cmp(_)->2
  |Cdq->1
  |Xchg(_)->1
  |Jle(_)|Jl(_)|Je(_)|Jne(_)|Jge(_)|Jg(_)|Jpo(_)->2
  |TestAl->2

type indexed_instruction=int*LabeledInstruction.t
let add_counter(source:LabeledInstruction.t)(counter:int):(int*indexed_instruction list* LabeledInstruction.t list)=
  (counter + (length_of_instruction source),[(counter,source)],[])

type located_instruction = (int*(int*int)list*LabeledInstruction.t)

let label_positions_from(known:(int*int) list)(modifier:indexed_instruction):(int*int) list=
  match modifier with
  |counter,LabeledInstruction.Position(l)->(l,counter)::known
  |_->known

let indexer(source:indexed_instruction)(known_label_positions:(int*int) list):((int*int) list*located_instruction list*indexed_instruction list)=
  let counter,instruction=source in
  let label_positions=label_positions_from known_label_positions source in
  (label_positions,[(counter,label_positions,instruction)],[])


type maybe_labelled_instruction=Labelled of LabeledInstruction.t | Unlabelled of UnlabeledInstruction.t

type params={print_new_line:int;print_string:int;scanf:int;beginning:int;print_result:int;exit:int}
let known_positions:params={
  print_new_line=134513024;(* 8048180 *)
  print_string=134513008;(* 8048170 *)
  print_result=134513040;(* 8048190 *)
  beginning=5 + 134513792;(* 5 es el largo del mov edi y el otro nro es donde inicia el programa segun la preferred position (la ubicación del movedi*)
  scanf=134513424;(* 8048310 *)
  exit=134513408;(* 8048300 *)
}

let unlabel_if_possible(label_positions:(int*int) list)(position:int)(instruction:LabeledInstruction.t)=
  let known=fun (l)->List.mem_assoc l label_positions in
  let off_l=fun (l)->(
    let dif = (List.assoc l label_positions) - (position + length_of_instruction instruction) in(
      (*print_string( Printf.sprintf "salto a:%X desde:%X la diferencia es:%X" (List.assoc l label_positions) position dif);*)
      dif
    )
    ) in
  let offset=fun(p)->p - (position + length_of_instruction instruction) in
  match instruction with
  |MovToRegister(r,v)->Unlabelled(UnlabeledInstruction.MovToRegister(r,v))
  |Push(r)->Unlabelled(UnlabeledInstruction.Push(r))
  |Pop(r)->Unlabelled(UnlabeledInstruction.Pop(r))
  |MovToMemory(v,r)->Unlabelled(UnlabeledInstruction.MovToMemory(v,r))
  |MovConstant(r,s)->Unlabelled(MovConstant(r,s))
  |Ascii(s)->Unlabelled(Ascii(s))
  |Ret->Unlabelled(Ret)
  |Position(l)->Labelled(Position(l))
  |Neg(r)->Unlabelled(Neg(r))
  |Add(r1,r2)->Unlabelled(Add(r1,r2))
  |Sub(r1,r2)->Unlabelled(Sub(r1,r2))
  |Imul(r)->Unlabelled(Imul(r))
  |Xchg(r1,r2)->Unlabelled(Xchg(r1,r2))
  |TestAl->Unlabelled(TestAl)
  |Cdq->Unlabelled(Cdq)
  |Idiv(r)->Unlabelled(Idiv(r))
  |Cmp(r1,r2)->Unlabelled(Cmp(r1,r2))
  |Call(l)->if known l then Unlabelled(Call(off_l l)) else Labelled(Call(l))
  |Jmp(l)->if known l then Unlabelled(Jmp(off_l l)) else Labelled(Jmp(l))
  |Jle(l)->if known l then Unlabelled(Jle(off_l l)) else Labelled(Jle(l))
  |Jl(l)->if known l then Unlabelled(Jl(off_l l)) else Labelled(Jl(l))
  |Je(l)->if known l then Unlabelled(Je(off_l l)) else Labelled(Je(l))
  |Jne(l)->if known l then Unlabelled(Jne(off_l l)) else Labelled(Jne(l))
  |Jpo(l)->if known l then Unlabelled(Jpo(off_l l)) else Labelled(Jpo(l))
  |Jge(l)->if known l then Unlabelled(Jge(off_l l)) else Labelled(Jge(l))
  |Jg(l)->if known l then Unlabelled(Jg(off_l l)) else Labelled(Jg(l))
  |CallPrintNewLine->Unlabelled(Call(offset known_positions.print_new_line))
  |CallScanf->Unlabelled(Call(offset known_positions.scanf))
  |CallPrintString->Unlabelled(Call(offset known_positions.print_string))
  |CallPrintResult->Unlabelled(Call(offset known_positions.print_result))
  |StoreStringPositionInEcx->Unlabelled(MovConstant(Ecx,string_of_int (position + (
    length_of_instruction(MovConstant (Eax,"1")) + (* mov ecx,123123 *)
    length_of_instruction(MovConstant (Eax,"1")) + (* mov edx,0004 *)
    length_of_instruction(Call 1) + (* call print *)
    length_of_instruction(Jmp(1)) (* jmp skip string *)
  ))))
  |StoreStringLengthInEdx(s)->Unlabelled(MovConstant(Edx,string_of_int (String.length s)))
  |JumpToSkipString(s)->Unlabelled(Jmp(
    length_of_instruction(Jmp 1) + 
    String.length(s) -
    length_of_instruction(Call(1))
  ))
  |JumpToExit->Unlabelled(Jmp(offset known_positions.exit))

let label_previous(source:located_instruction):(int*maybe_labelled_instruction) list=
  let counter,label_positions,labeled_instruction=source in
  [(counter,unlabel_if_possible label_positions counter labeled_instruction)]

let skip_head l=match l with hd::tl->tl|[]->[]

let rec skip_unlabelled(l:(int*maybe_labelled_instruction) list)=
  match l with
  |[]->[]
  |hd::tl->(
    match hd with
    |(_,(Unlabelled(_)|Labelled(Position(_))))->skip_unlabelled tl
    | _ ->hd::tl
  )

let rec grab_unlabelled(l:(int*maybe_labelled_instruction) list)=
  match l with
  | []->[]
  | hd::tl->(
    match hd with
    | (_,Unlabelled(ui))->ui::(grab_unlabelled tl)
    | _->[]
  )



let unlabeled(instruction_counter:int)(labeled_instruction:LabeledInstruction.t)(label_counter:int)(label:int):UnlabeledInstruction.t option=
  let jump_offset = label_counter-(instruction_counter + length_of_instruction labeled_instruction) in
  match labeled_instruction with
  |Call(l) when l==label->Some(Call(jump_offset))
  |Jmp(l) when l==label->Some(Jmp(jump_offset))
  |Jle(l) when l==label->Some(Jle(jump_offset))
  |Jl(l) when l==label->Some(Jl(jump_offset))
  |Je(l) when l==label->Some(Je(jump_offset))
  |Jne(l) when l==label->Some(Jne(jump_offset))
  |Jpo(l) when l==label->Some(Jpo(jump_offset))
  |Jge(l) when l==label->Some(Jge(jump_offset))
  |Jg(l) when l==label->Some(Jg(jump_offset))
  |_-> None

let print_tuple(s:(int*maybe_labelled_instruction))=
  match s with
  |counter,Labelled(li)->print_string "-->"; print_int counter; print_string (LabeledInstructionOps.to_string li)
  |counter,Unlabelled(ul)->print_string "-->"; print_int counter; print_string (UnlabeledInstructionOps.to_string ul)

let really_unlabel(source:(int*maybe_labelled_instruction))(stored_instructions:(int*maybe_labelled_instruction) list):((int*maybe_labelled_instruction) list*UnlabeledInstruction.t list*(int*maybe_labelled_instruction) list)=
  let with_source(l)= List.concat [l;[source]]
  in let result_with(s:UnlabeledInstruction.t option)=
    let next_store = match s with
    | Some(ui)->[]
    | None->(skip_unlabelled  (with_source stored_instructions))
    in let unlabeled_instructions = match s with
    | Some(ui)->[ui]
    | None->(grab_unlabelled (with_source stored_instructions))
    in let next_to_analyse = match s with
    | Some(ui)->(skip_head stored_instructions)
    | None->[]
    in (next_store,unlabeled_instructions,next_to_analyse)
  in let first_stored_instruction=List.nth_opt stored_instructions 0 in
  (*
    print_string "\n\n###############\n";
    (
      match first_stored_instruction with
      | Some(some)->print_tuple some
      | None->print_string "nada"
    );
    print_string "\n - - - - -\n";
    print_tuple source;
    print_string "\n------\n";
    List.iter print_tuple stored_instructions
  );*)
  match first_stored_instruction,source with
  | Some(counter_first,Labelled(labeled_instruction)),(counter_source,Labelled(Position(l)))->result_with (unlabeled counter_first labeled_instruction counter_source l)
  |_->result_with None

let run(s:LabeledInstruction.t Lazylist.gen_t):UnlabeledInstruction.t Lazylist.gen_t=
  s
  |> (fun s->LazylistOps.run add_counter s known_positions.beginning)
  |> (fun s->LazylistOps.run indexer s [])
  |> LazylistOps.map label_previous
  |> (fun s->LazylistOps.run really_unlabel s [])