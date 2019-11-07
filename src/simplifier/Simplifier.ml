let process(current:FlowAction.t)(previous:FlowAction.t option):(FlowAction.t option*LabeledInstruction.t list*FlowAction.t list)=
  let ret=fun(r)->(Some(current),r,[]) in
  ret (match current with
    | ReadVariable(v)-> [
      LabeledInstruction.MovToRegister(LabeledInstruction.Eax,v);
      LabeledInstruction.Push(LabeledInstruction.Eax)
    ]
    | WriteVariable(v)->[
      LabeledInstruction.Pop(LabeledInstruction.Eax);
      LabeledInstruction.MovToMemory(v,LabeledInstruction.Eax);
    ]
    |IntegerRead(s)->[
      LabeledInstruction.MovConstant(LabeledInstruction.Eax,s);
      LabeledInstruction.Push(LabeledInstruction.Eax)
    ]
    |Return->[
      LabeledInstruction.Ret
    ]
    |LabeledPosition(l)->[Position(l)]
    |SkipProcedureLabeledPosition(l)->[Position(l)]
    |ProcedureDeclarationLabeledPosition(l)->[Position(l)]
    |CallJump(l)->[Call(l)]
    |SkipProcedureJumpTo(l)->[Jmp(l)]
    |JumpTo(l)->[Jmp(l)]
    |ConditionalJumpTo(label)->(
      match previous with
      | Some(Operate(op))->(
        let cmp=(fun(i)->[
            LabeledInstruction.Pop Eax;
            LabeledInstruction.Pop Ebx;
            LabeledInstruction.Cmp(Eax,Ebx);
            i
          ] 
        )in
        match op with
        |LessOrEqualCheck->cmp (Jle label)
        |LessCheck->cmp (Jl label)
        |EqualsCheck->cmp (Je label)
        |DistinctCheck->cmp (Jne label)
        |OddCheck->cmp (Jpo label)
        |GreaterOrEqualCheck->cmp (Jge label)
        |GreaterCheck->cmp (Jg label)
        |_->[]
      )
      |_->[]
    )
    |Operate(op)->(
      match op with
      |NegateOperation->[
        Pop Eax;
        Neg(Eax);
        Push Eax
      ]
      |PlusOperation->[
        Pop Eax;
        Pop Ebx;
        Add(Ebx,Eax);
        Push Eax
      ]
      |MinusOperation->[
        Pop Eax;
        Pop Ebx;
        Sub(Ebx,Eax);
        Push Eax
      ]
      |TimesOperation->[
        Pop Eax;
        Pop Ebx;
        Imul(Ebx,Eax);
        Push Eax
      ]
      |DivideOperation->[
        Pop Ebx;
        Pop Eax;
        MovConstant(Edx,"0");
        Idiv(Ebx);
        Push Eax
      ]
      |_->[]
    )
    (* SYSCALLS ------ AYUDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA*)
    |PrintResult->[
      (*LabeledInstruction.CallPrintString*)
    ]
    |PrintNewline->[
      (*LabeledInstruction.CallPrintNewLine*)
    ]
    |PrintString(s)->[
      
    ]
    | FlowAction.WriteVariableFromInput(v)->[
      (*
      LabeledInstruction.CallScanf;
      LabeledInstruction.JumpScanfInterruption;
      LabeledInstruction.Pop(LabeledInstruction.Eax);
      LabeledInstruction.MovToMemory(v,LabeledInstruction.Eax);
      *)
    ]
    
  )

let run(s:FlowAction.t Lazylist.gen_t):LabeledInstruction.t Lazylist.gen_t=
  LazylistOps.run process s None