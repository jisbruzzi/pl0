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

let factor_fn (expression:pattern) ():pattern = Or([m_ident;m_integer;Sequence([
  m Token.OpenParenthesis; expression;m Token.ClosedParenthesis
])])

let term_fn (factor:pattern) ():pattern=Sequence([
  factor;
  Asterisk(Sequence([Or([m Token.Times;m Token.Divide]); factor]))
])

let rec expresion_fn ():pattern = 
  let expresion:pattern=In(expresion_fn,"expresion") in
  let factor_generator:pattern_gen=factor_fn expresion in
  let factor=In(factor_generator,"factor") in
  let term=In(term_fn factor,"termino") in 
  Sequence([
    Maybe(Or([m Token.Plus; m Token.Minus]));
    term;
    Asterisk(Sequence([Or([m Token.Minus;m Token.Plus]);term]))
  ])

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
  Sequence([  
    Maybe(Sequence([m_ident;m Token.Assignation;expression]));
    Maybe(Sequence([m Token.Call; m_ident]));
    Maybe(Sequence([m Token.Begin;proposition;Asterisk(Sequence([m Token.Semicolon; proposition]))]));
    Maybe(Sequence([m Token.If;condition;m Token.Then;proposition]));
    Maybe(Sequence([m Token.While;condition;m Token.Do;proposition]));
    Maybe(Sequence([m Token.Writeln;Maybe(Sequence([
      m Token.OpenParenthesis; 
      Or([m_string;expression]);
      Asterisk(Sequence([
        m Token.Comma; m_ident
      ]));
      m Token.ClosedParenthesis
    ]))]));
    Maybe(Sequence([
      m Token.Write;
      m Token.OpenParenthesis;
      Or([m_string;expression]);
      Asterisk(Sequence([
        m Token.Comma;Or([m_string;expression])
      ]))
    ]))
  ])

let rec block_fn (proposition:pattern) ():pattern = Sequence([
  Maybe(Sequence([
    m Token.Const;
    m_ident;
    m Token.Equals;
    m_integer;
    Asterisk(Sequence([m Token.Comma;m_ident;m Token.Equals;m_integer]));
    m Token.Semicolon
    ]));
  Maybe(Sequence([
    m Token.Var; m_ident;Asterisk(Sequence([m Token.Comma; m_ident]));m Token.Semicolon
  ]));
  Asterisk(Sequence([
    m Token.Procedure; m_ident; m Token.Colon; In(block_fn proposition,"bloque"); m Token.Semicolon
  ]));
  proposition
])

let program_fn (block:pattern)():pattern = Sequence( [ block; m Token.Point ] )

let build ():pattern =
  let expression = In(expresion_fn,"expresion") in
  let condition = In(condition_fn expression,"condicion") in
  let proposition = In(proposition_fn condition expression,"proposicion") in
  let block = In(block_fn proposition,"bloque") in
  let program = In(program_fn block,"programa") in
  program