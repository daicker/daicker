package lang

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParse(t *testing.T) {
	tests := []struct {
		input    string
		expected Module
	}{
		{
			input: "a := 1",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSInt(1, 0, 5, 0, 6),
						Range: newRange(0, 0, 0, 6),
					},
				},
			},
		},
		{
			input: "a := \"ok\"",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSString("ok", 0, 5, 0, 9),
						Range: newRange(0, 0, 0, 9),
					},
				},
			},
		},
		{
			input: "a := 1 + 2",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 5, 0, 10, newSInt(1, 0, 5, 0, 6), newSInt(2, 0, 9, 0, 10)),
						Range: newRange(0, 0, 0, 10),
					},
				},
			},
		},
		{
			input: "a := \"a\" + \"b\"",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 5, 0, 14, newSString("a", 0, 5, 0, 8), newSString("b", 0, 11, 0, 14)),
						Range: newRange(0, 0, 0, 14),
					},
				},
			},
		},
		{
			input: "a := 1 + 2\nb := \"a\" + \"b\"",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 5, 0, 10, newSInt(1, 0, 5, 0, 6), newSInt(2, 0, 9, 0, 10)),
						Range: newRange(0, 0, 0, 10),
					},
					{
						Name:  "b",
						Expr:  newApp("+", 1, 5, 1, 14, newSString("a", 1, 5, 1, 8), newSString("b", 1, 11, 1, 14)),
						Range: newRange(1, 0, 1, 14),
					},
				},
			},
		},
		{
			input: "f := \\(a, b) => a + b",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name: "f",
						Expr: newFunc(0, 5, 0, 21,
							newApp("+", 0, 16, 0, 21, newRef("a", 0, 16, 0, 17), newRef("b", 0, 20, 0, 21)),
							newParam("a", 0, 7, 0, 8), newParam("b", 0, 10, 0, 11)),
						Range: newRange(0, 0, 0, 21),
					},
				},
			},
		},
	}

	for _, test := range tests {
		tokens, err := Lex(test.input)
		assert.NoError(t, err)
		mod, err := Parse(tokens)
		assert.NoError(t, err)
		assert.Equal(t, test.expected, mod, "input is:\n%s", test.input)
	}
}

func newApp(f string, startLine, startColumn, endLine, endColumn int, es ...Expr) *App {
	return &App{
		Func:  f,
		Args:  es,
		Range: newRange(startLine, startColumn, endLine, endColumn),
	}
}

func newRange(startLine, startColumn, endLine, endColumn int) Range {
	return Range{
		Start: Position{Line: startLine, Column: startColumn},
		End:   Position{Line: endLine, Column: endColumn},
	}
}

func newParam(name string, startLine, startColumn, endLine, endColumn int) Param {
	return Param{
		Name:  name,
		Range: newRange(startLine, startColumn, endLine, endColumn),
	}
}

func newFunc(startLine, startColumn, endLine, endColumn int, e Expr, ps ...Param) *Func {
	return &Func{
		Params: ps,
		Expr:   e,
		Range:  newRange(startLine, startColumn, endLine, endColumn),
	}
}

func newRef(v string, startLine, startColumn, endLine, endColumn int) *Ref {
	return &Ref{
		Name:  v,
		Range: newRange(startLine, startColumn, endLine, endColumn),
	}
}

func newSInt(v int, startLine, startColumn, endLine, endColumn int) *SInt {
	return &SInt{
		Value: v,
		Range: newRange(startLine, startColumn, endLine, endColumn),
	}
}

func newSString(v string, startLine, startColumn, endLine, endColumn int) *SString {
	return &SString{
		Value: v,
		Range: newRange(startLine, startColumn, endLine, endColumn),
	}
}
