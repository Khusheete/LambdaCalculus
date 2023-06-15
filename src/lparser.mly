/*
  Copyright 2023 Souchet Ferdinand

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
  documentation files (the “Software”), to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
  persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


%token <string> IDENT
%token FUN
%token LAZY_ASSIGN EAGER_ASSIGN
%token LPAREN RPAREN
%token EOL EOF

%start input
%type <Lambda.statement> input
%type <Lambda.statement> statement
%type <Lambda.expr> expr
%type <Lambda.expr> no_apply_expr
// %type <Lambda.expr> multiline_expr

%%

input: statement EOL    { $1 }
    | statement EOF     { $1 }
    | EOL input         { $2 }
    | EOF               { raise End_of_file }
;

statement: expr                 { Lambda.Lazy("$arg", $1) }
    // | LAZY_ASSIGN expr          { Lambda.Lazy("", $2) }
    // | EAGER_ASSIGN expr         { Lambda.Eager("", $2) }
    | IDENT LAZY_ASSIGN expr    { Lambda.Lazy($1, $3) }
    | IDENT EAGER_ASSIGN expr   { Lambda.Eager($1, $3) }
;

expr: IDENT                             { Lambda.Var($1) }          // variable
    | expr no_apply_expr                { Lambda.Apply($1, $2) }    // application
    | IDENT FUN expr                    { Lambda.Lambda($1, $3) }   // abstraction
    // | LPAREN multiline_expr RPAREN      { Lambda.Group($2) }       // grouping
    | LPAREN expr RPAREN                { $2 }                      // grouping
;

no_apply_expr: IDENT                    { Lambda.Var($1) }          // variable
    | IDENT FUN expr                    { Lambda.Lambda($1, $3) }   // abstraction
    // | LPAREN multiline_expr RPAREN      { Lambda.Group($2) }       // grouping
    | LPAREN expr RPAREN                { $2 }                      // grouping
;

// multiline_expr:
//     | IDENT                             { Lambda.Var($1) }         // variable
//     | multiline_expr multiline_expr     { Lambda.Apply($1, $2) }   // application
//     | IDENT FUN multiline_expr          { Lambda.Lambda($1, $3) }  // abstraction
//     | LPAREN multiline_expr RPAREN      { Lambda.Group($2) }       // grouping
//     | multiline_expr EOL    { $1 }
// ;

%%