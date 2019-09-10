open Pattern
type pattern=Pattern.t
type pattern_gen=unit->pattern

let match_equal t_eq t_other= t_eq=t_other
let m t_eq = Pattern.Match(
  (match_equal t_eq),
  SyntaxError.TokenExpected(t_eq)
)

let match_ident t=match t with Token.Ident(_)->true | _->false
let m_ident = Pattern.Match(match_ident,SyntaxError.IdentifierExpected)

let match_string t=match t with Token.StringTerminal(_)->true | _->false
let m_string = Pattern.Match(match_string,SyntaxError.StringExpected)

let match_integer t=match t with Token.Integer(_)->true | _->false
let m_integer=Pattern.Match(match_integer,SyntaxError.IntegerExpected)

let expresion_fn ():pattern = Sequence([
  Maybe(Or([m Token.Plus; m Token.Minus]));
  In(term_fn);
  Asterisk(Sequence([Or([m Token.Minus;m Token.Plus]);In(term_fn)]))
])

let factor_fn ():pattern = Or([m_ident;m_integer;Sequence([
  m Token.OpenParenthesis; In(expresion_fn);m Token.ClosedParenthesis
])])

let term_fn ():pattern=Sequence([
  In(factor_fn);
  Asterisk(Sequence([Or([m Token.Times;m Token.Divide]); In(factor_fn)]))
])

let expresion_fn ():pattern = Sequence([
  Maybe(Or([m Token.Plus;m Token.Minus]));
  In(term_fn);
  Asterisk(Sequence([
    Or([m Token.Plus;m Token.Minus]);
    In(term_fn)
  ]))
])

let condition_fn ():pattern = Or([
  Sequence([m Token.Odd;In(expresion_fn)]);
  Sequence([
    In(expresion_fn);
    Or([
      m Token.Equals;
      m Token.Distinct;
      m Token.Less;
      m Token.LessOrEqual;
      m Token.Greater;
      m Token.GreaterOrEqual
    ]);
    In(expresion_fn);
  ])
])

let rec proposition_fn ():pattern = Sequence([
  Maybe(Sequence([m_ident;m Token.Assignation;In(expresion_fn)]));
  Maybe(Sequence([m Token.Call; m_ident]));
  Maybe(Sequence([m Token.Begin;In(proposition_fn);Asterisk(Sequence([m Token.Semicolon; In(proposition_fn)]))]));
  Maybe(Sequence([m Token.If;In(condition_fn);m Token.Then;In(proposition_fn)]));
  Maybe(Sequence([m Token.While;In(condition_fn);m Token.Do;In(proposition_fn)]));
  Maybe(Sequence([m Token.Writeln;Maybe(Sequence([
    m Token.OpenParenthesis; 
    Or([m_string;In(expresion_fn)]);
    Asterisk(Sequence([
      m Token.Comma; m_ident
    ]));
    m Token.ClosedParenthesis
  ]))]));
  Maybe(Sequence([
    m Token.Write;
    m Token.OpenParenthesis;
    Or([m_string;In(expresion_fn)]);
    Asterisk(Sequence([
      m Token.Comma;Or([m_string;In(expresion_fn)])
    ]))
  ]))
])

let rec block_fn ():pattern = Sequence([
  Maybe(Sequence([
    m Token.Const;
    m_ident;
    m Token.Equals;
    m_integer;
    Asterisk(Sequence([m Token.Comma;m_ident;m Token.Equals;m_integer]));
    m Token.Semicolon
    ]));
  Maybe(Sequence([
    m Token.Var; m_ident;Asterisk(Sequence([m Token.Var; m_ident]))
  ]));
  Asterisk(Sequence([
    m Token.Procedure; m_ident; m Token.Colon; In(block_fn); m Token.Semicolon
  ]));
  In(proposition_fn)
])

let program_fn ():Pattern.t = Sequence( [ In(block_fn); m Token.Point ] )


let factor=In(factor_fn)
let term=In(term_fn)
let expresion=In(expresion_fn)
let condition=In(condition_fn)
let proposition=In(proposition_fn)
let block=In(block_fn)
let program=In(program_fn)