

let length_of_instruction(s:LabeledInstruction.t):int=
  match s with
  |Jmp(_)|JumpToSkipString(_)|JumpToExit->5
  |MovToRegister(_)|StoreStringPositionInEcx|StoreStringLengthInEdx(_)->6
  |Push(_)->1
  |Pop(_)->1
  |MovToMemory(_)->6
  |Call(_)|CallPrintNewLine|CallScanf|CallPrintString|CallPrintResult->5
  |MovConstant(_)->5
  |Ascii(s)->String.length s
  |Ret->1
  |Position(_)->0
  |Neg(_)|Add(_)|Sub(_)|Imul(_)|Idiv(_)|Cmp(_)->2
  |Cdq->1
  |Xchg(_)->1
  |Jle(_)|Jl(_)|Je(_)|Jne(_)|Jge(_)|Jg(_)|Jpo(_)->2

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
  print_new_line=5;
  print_string=(-10);
  scanf=(-20);
  beginning=0;
  print_result=(-50);
  exit=(-20);


}

let unlabel_if_possible(label_positions:(int*int) list)(position:int)(instruction:LabeledInstruction.t)=
  let known=fun (l)->List.mem_assoc l label_positions in
  let off_l=fun (l)->List.assoc l label_positions - position in
  let offset=fun(p)->p-position in
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
  |Imul(r1,r2)->Unlabelled(Imul(r1,r2))
  |Xchg(r1,r2)->Unlabelled(Xchg(r1,r2))
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
    String.length(s)
  ))
  |JumpToExit->Unlabelled(Jmp(offset known_positions.exit))

let label_previous(source:located_instruction):(int*maybe_labelled_instruction) list=
  let counter,label_positions,labeled_instruction=source in
  [(counter,unlabel_if_possible label_positions counter labeled_instruction)]


let run(s:LabeledInstruction.t Lazylist.gen_t):UnlabeledInstruction.t Lazylist.gen_t=
  s
  |> (fun s->LazylistOps.run add_counter s known_positions.beginning)
  |> (fun s->LazylistOps.run indexer s [])
  |> LazylistOps.map label_previous
  |> (fun s->LazylistOps.run really_unlabel s [])