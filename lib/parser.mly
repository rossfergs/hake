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
%token LSqb
%token RSqb
%token Comma
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
  | ss = separated_list(NLine, statement) Eof { ss }
  ;

statement:
  | Var n = Name Eq e = expression { Var(n, e) }
  | Func n = Name LParen p = separated_list(Comma, Name) RParen Colon NLine b = body { Func{ name = n; params = p; body = b } }
  ;

body:
  | ss = separated_list(NLine, statement) Return e = option(expression) { (ss, e) }
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
  | n = Name { Name n }
  | f = Float { Float f }
  ;

%%
