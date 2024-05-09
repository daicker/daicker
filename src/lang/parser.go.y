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
	Var    Var
	Vars   []Var
}

%left ADD
%left COMMA
%right ASSIGN

%token <token> INTEGER STRING
%token <token> IDENTIFIER
%token <token> ADD
%token <token> LBRACKET RBRACKET

%type <Module> Root Module
%type <Vars> Vars
%type <Var> Var
%type <Expr> Expr
%%

Root: Module {
	yylex.(*TokenIterationLexer).result = $1
	$$ = $1
}

Module: Vars { $$ = Module{ Vars: $1 } }

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
	| IDENTIFIER {
		$$ = &Ref{
			Name: $1.Literal,
			Range: NewRangeFromToken(*$1),
		}
	}
	| Func {

	}
	;

Func
  : LBRACKET Params RBRACKET IDENTIFIER

Params
  :

Param
  : IDENTIFIER

%%
