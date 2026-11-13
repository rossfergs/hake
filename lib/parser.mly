%{
open Ast
%}

%token <int> Int
%token <float> Float
%token <string> String
%token <string> Name
%token Eq
%token Func
%token Var
%token Return
%token Mult
%token Plus
%token Minus
%token Div
%token Colon
%token LParen
%token RParen
%token Comma
%token Indent
%token Dedent
%token NLine
%token Eof
%left Eq
%left Plus Minus
%left Mult Div

%type <expr> expression
%type <statement> statement

%start <statement list> prog
%%

prog:
  | NLine* ss = separated_list(NLine+, statement) Eof { ss }
  ;

eof_or_dedent:
  | Eof { () }
  | Dedent { () }
  ;

statement:
  | Var n = Name Eq e = expression {
      Var(n, e)
    }
  | Func n = Name LParen p = separated_list(Comma, Name) RParen Colon NLine Indent b = body eof_or_dedent {
      Func{ name = n; params = p; body = b }
    }
  | Return e = expression? {
      Return e
    }
  ;

body:
  | ss = separated_list(NLine+, statement) { ss }
  ;

expression:
  | le = expression Plus re = expression { Add (le, re) }
  | le = expression Minus re = expression { Sub (le, re) }
  | le = expression Mult re = expression { Mult (le, re) }
  | le = expression Div re = expression { Div (le, re) }
  | le = expression Eq re = expression { Eq (le, re) }
  | LParen e = expression RParen { e }
  | i = Int { Int i }
  | s = String { String s }
  | n = Name LParen args = separated_list(Comma, expression) RParen { Application (n, args) }
  | n = Name { Name n }
  | f = Float { Float f }
  ;

%%
