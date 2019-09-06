type token=Token.t

(* una funcion que toma una lista de funciones verificadoras que toma otra lista de funciones.... y verifica el primer elemento y devuelve el siguiente verificador 

SON FUNCIONES!!

*)
type pattern = 
|Maybe of pattern
|Sequence of pattern list
|Match of (token -> bool)
|Asterisk of pattern
|Or of pattern list

let match_equal t_eq t_other= t_eq=t_other
let m t_eq = Match(match_equal t_eq)

let match_ident t=match t with Token.Ident(_)->true | _->false
let m_ident = Match(match_ident)

let match_string t=match t with Token.StringTerminal(_)->true | _->false
let m_string = Match(match_string)

let match_integer t=match t with Token.Integer(_)->true | _->false
let m_integer=Match(match_integer)

let factor:pattern = Or([m_ident;m_integer;Sequence([
  m Token.OpenParenthesis; expresion;m Token.ClosedParenthesis
])])
let term:pattern=Sequence([
  factor;
  Asterisk(Sequence([Or([m Token.Times;m Token.Divide]); factor]))
])
let expresion:pattern = Sequence([
  Maybe(Or([m Token.Plus;m Token.Minus]));
  term;
  Asterisk(Sequence([
    Or([m Token.Plus;m Token.Minus]);
    term
  ]))
])
let condition:pattern = Or([
  Sequence([m Token.Odd;expresion]);
  Sequence([
    expresion;
    Or([
      m Token.Equals;
      m Token.Distinct;
      m Token.Less;
      m Token.LessOrEqual;
      m Token.Greater;
      m Token.GreaterOrEqual
    ]);
    expresion;
  ])
])
let rec proposition:pattern = Sequence([
  Maybe(Sequence([m_ident;m Token.Assignation;expresion]));
  Maybe(Sequence([m Token.Call; m_ident]));
  Maybe(Sequence([m Token.Begin;proposition;Asterisk(Sequence([m Token.Semicolon; proposition]))]));
  Maybe(Sequence([m Token.If;condition;m Token.Then;proposition]));
  Maybe(Sequence([m Token.While;condition;m Token.Do;proposition]));
  Maybe(Sequence([m Token.Writeln;Maybe(Sequence([
    m Token.OpenParenthesis; 
    Or([m_string;expresion]);
    Asterisk(Sequence([
      m Token.Comma; m_ident
    ]));
    m Token.ClosedParenthesis
  ]))]));
  Maybe(Sequence([
    m Token.Write;
    m Token.OpenParenthesis;
    Or([m_string;expresion]);
    Asterisk(Sequence([
      m Token.Comma;Or([m_string;expresion])
    ]))
  ]))
])
let rec block:pattern = Sequence([
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
    m Token.Procedure; m_ident; m Token.Colon; block; m Token.Semicolon
  ]));
  proposition
])
let program:pattern = Sequence( [block;m Token.Point]) 


let expected (p:pattern)(t:Token.t):bool=
  (pattern )
let run (tokens:TokenWithCoords.t Lazylist.gen_t)(tester:pattern):TokenWithCoords.t Lazylist.gen_t =


let run (tokens:TokenWithCoords.t Lazylist.gen_t):TokenWithCoords.t Lazylist.gen_t =

