open Pattern
type pattern=Pattern.t
type pattern_gen=unit->pattern

let match_equal t_eq t_other= t_eq=t_other
let m t_eq = Pattern.Match(
  (match_equal t_eq),
  SyntaxError.TokenExpected(t_eq),
  (TokenOps.string_of_token t_eq)
)

let match_ident t=match t with Token.Ident(_)->true | _->false
let m_ident = Pattern.Match(match_ident,SyntaxError.IdentifierExpected,"identificador")

let match_string t=match t with Token.StringTerminal(_)->true | _->false
let m_string = Pattern.Match(match_string,SyntaxError.StringExpected,"string")

let match_integer t=match t with Token.Integer(_)->true | _->false
let m_integer=Pattern.Match(match_integer,SyntaxError.IntegerExpected,"entero")

let factor_fn (expression:pattern) ():pattern = Labeled(SyntaxLabel.Factor,Or([
  Labeled(SyntaxLabel.ConstOrVarRead,m_ident);
  Labeled(SyntaxLabel.LiteralInteger,m_integer);
  Sequence([
  m Token.OpenParenthesis; expression;m Token.ClosedParenthesis
])]))

let term_fn (factor:pattern) ():pattern=Labeled(SyntaxLabel.Term,Sequence([
  factor;
  Asterisk(Sequence([
    Labeled(SyntaxLabel.FactorOperation,Or([m Token.Times;m Token.Divide])); 
    factor
  ]))
]))

let rec expresion_fn ():pattern = 
  let expresion:pattern=In(expresion_fn,"expresion") in
  let factor_generator:pattern_gen=factor_fn expresion in
  let factor=In(factor_generator,"factor") in
  let term=In(term_fn factor,"termino") in 
  Labeled(SyntaxLabel.Expression,Sequence([
    Maybe(Or([m Token.Plus; Labeled(SyntaxLabel.Negation,m Token.Minus)]));
    term;
    Asterisk(Sequence([Labeled(SyntaxLabel.TermOperation,Or([m Token.Minus;m Token.Plus]));term]))
  ]))

let condition_fn (expression:pattern) ():pattern = Or([
  Sequence([m Token.Odd;expression]);
  Sequence([
    expression;
    Or([
      m Token.Equals;
      m Token.Distinct;
      m Token.Less;
      m Token.LessOrEqual;
      m Token.Greater;
      m Token.GreaterOrEqual
    ]);
    expression;
  ])
])

let rec proposition_fn (condition:pattern)(expression:pattern) ():pattern =
  let proposition=In(proposition_fn condition expression,"proposicion") in 
  Labeled(SyntaxLabel.Proposition,Maybe(Or([  
    Labeled(SyntaxLabel.AssignationProposition,Sequence([
      Labeled(SyntaxLabel.VariableAssign,m_ident);m Token.Assignation;expression
    ]));
    Sequence([m Token.Call; Labeled(SyntaxLabel.ProcedureNameCall,m_ident)]);
    (Sequence([m Token.Begin;proposition;Asterisk(Sequence([m Token.Semicolon; proposition]));m Token.End]));
    Labeled(SyntaxLabel.IfProposition,(Sequence([m Token.If;condition;m Token.Then;proposition])));
    Labeled(SyntaxLabel.WhileProposition,Sequence([m Token.While;condition;m Token.Do;proposition]));
    (Sequence([m Token.Writeln;Maybe(Sequence([
      m Token.OpenParenthesis; 
      Or([m_string;expression]);
      Asterisk(Sequence([
        m Token.Comma; Or([m_string;expression]);
      ]));
      m Token.ClosedParenthesis
    ]))]));
    (Sequence([
      m Token.Write;
      m Token.OpenParenthesis;
      Or([m_string;expression]);
      Asterisk(Sequence([
        m Token.Comma;Or([m_string;expression])
      ]));
      m Token.ClosedParenthesis
    ]));
    (Sequence([
      m Token.Readln;
      m Token.OpenParenthesis;
      m_ident;
      Asterisk(Sequence([m Token.Comma;Labeled(SyntaxLabel.VariableAssignFromReadln,m_ident)]));
      m Token.ClosedParenthesis
    ]));

  ])))

let rec block_fn (proposition:pattern) ():pattern = Labeled(SyntaxLabel.Block,Sequence([
  Maybe(Sequence([
    m Token.Const;
    Labeled(SyntaxLabel.ConstantNameDeclaration,m_ident);
    m Token.Equals;
    Labeled(SyntaxLabel.ConstantValue,m_integer);
    Asterisk(Sequence([
      m Token.Comma;
      Labeled(SyntaxLabel.ConstantNameDeclaration,m_ident);
      m Token.Equals;
      Labeled(SyntaxLabel.ConstantValue,m_integer);
    ]));
    m Token.Semicolon
  ]));
  Maybe(Sequence([
    m Token.Var; 
    Labeled(SyntaxLabel.VariableDeclaration,m_ident);
    Asterisk(Sequence([
      m Token.Comma; 
      Labeled(SyntaxLabel.VariableDeclaration,m_ident);
    ]));
    m Token.Semicolon
  ]));
  Asterisk(Sequence([
    Labeled(SyntaxLabel.ProcedureDeclaration,Sequence([
      m Token.Procedure; 
      Labeled(SyntaxLabel.ProcedureNameDeclaration,m_ident); 
      m Token.Semicolon; 
      In(block_fn proposition,"bloque")
    ])); m Token.Semicolon
  ]));
  proposition
]))

let program_fn (block:pattern)():pattern = 
Labeled(SyntaxLabel.ProgramRoot,
  Sequence( [ block; m Token.Point;m Token.EndOfFileToken ] )
)

let build ():pattern =
  let expression = In(expresion_fn,"expresion") in
  let condition = In(condition_fn expression,"condicion") in
  let proposition = In(proposition_fn condition expression,"proposicion") in
  let block = In(block_fn proposition,"bloque") in
  let program = In(program_fn block,"programa") in
  program