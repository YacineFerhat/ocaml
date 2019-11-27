type token =
  | ET
  | OU
  | IMPLIQ
  | NEG
  | LPAR
  | RPAR
  | QUIT
  | Vrai
  | Faux
  | VAR of (string)
  | EOF

val programme :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Prop_def.proposition
