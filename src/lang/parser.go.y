%{
package lang

import "github.com/macrat/simplexer"
import "strconv"
import "strings"

func Parse(tokens []*simplexer.Token) (Module, error) {
	lexer := NewTokenIterationLexer(tokens)
	yyParse(lexer)
	if lexer.error != nil {
		return Module{}, lexer.error
	}
	return lexer.result, nil
}
%}

%union{
	token  *simplexer.Token
	Module Module
	Expr   Expr
	Exprs  []Expr
	Var    Var
	Vars   []Var
	Param  Param
	Params []Param
}

%right ASSIGN
%right ARROW
%left COMMA
%left ADD

%token <token> INTEGER STRING
%token <token> IDENTIFIER
%token <token> ADD
%token <token> LBRACKET RBRACKET
%token <token> ARROW BSLASH
%token <token> MODULE

%type <Module> Root Module
%type <Vars> Vars
%type <Var> Var
%type <Expr> Expr
%type <Exprs> Args
%type <Param> Param
%type <Params> Params
%%

Root: Module {
	yylex.(*TokenIterationLexer).result = $1
	$$ = $1
}

Module: MODULE IDENTIFIER Vars { $$ = Module{ Name: $2.Literal, Vars: $3 } }

Vars
  : Var Vars { $$ = append([]Var{ $1 }, $2...) }
  | { $$ = []Var{} }

Var
  : IDENTIFIER ASSIGN Expr {
	  $$ = Var{
			Name: $1.Literal,
			Expr: $3,
			Range: NewRangeFromToken(*$1).Union($3.GetRange()),
		}
	}
	| IDENTIFIER LBRACKET Params RBRACKET ASSIGN Expr {
		f := &Func{
			Params: $3,
			Expr:   $6,
			Range:  NewRangeFromToken(*$2).Union($6.GetRange()),
		}
		$$ = Var{
			Name: $1.Literal,
			Expr: f,
			Range: NewRangeFromToken(*$1).Union($6.GetRange()),
		}
	}
	;

Expr
	: INTEGER {
		v, err := strconv.Atoi($1.Literal)
		if err != nil {
			panic(err)
		}
		$$ = &SInt{
			Value: v,
			Range: NewRangeFromToken(*$1),
		}
	}
	| STRING {
		$$ = &SString{
			Value: strings.Replace($1.Literal, "\"", "", -1),
			Range: NewRangeFromToken(*$1),
		}
	}
	| Expr ADD Expr {
		$$ = &App{
			Func: $2.Literal,
			Args: []Expr{$1, $3},
			Range: $1.GetRange().Union($3.GetRange()),
		}
	}
	| IDENTIFIER LBRACKET Args RBRACKET {
		$$ = &App{
			Func: $1.Literal,
			Args: $3,
			Range: NewRangeFromToken(*$1).Union(NewRangeFromToken(*$4)),
		}
	}
	| IDENTIFIER {
		$$ = &Ref{
			Name: $1.Literal,
			Range: NewRangeFromToken(*$1),
		}
	}
	| BSLASH LBRACKET Params RBRACKET ARROW Expr {
		$$ = &Func{
			Params: $3,
			Expr:   $6,
			Range:  NewRangeFromToken(*$1).Union($6.GetRange()),
		}
	}
	| LBRACKET Expr RBRACKET {
		$$ = $2
	}
	;

Args
  : Expr COMMA Args { $$ = append([]Expr{ $1 }, $3...)}
	| Expr { $$ = []Expr{ $1 } }
	| { $$ = []Expr{} }

Params
  : Param COMMA Params { $$ = append([]Param{ $1 }, $3...)}
	| Param { $$ = []Param{ $1 } }
	| { $$ = []Param{} }
	;

Param
  : IDENTIFIER { $$ = Param{ Name: $1.Literal, Range: NewRangeFromToken(*$1) } }
	;

%%
