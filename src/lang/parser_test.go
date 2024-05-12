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
			input: `module main
a = 1`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSInt(1, 1, 4, 1, 5),
						Range: newRange(1, 0, 1, 5),
					},
				},
			},
		},
		{
			input: `module main
a = "ok"`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSString("ok", 1, 4, 1, 8),
						Range: newRange(1, 0, 1, 8),
					},
				},
			},
		},
		{
			input: `module main
a = 1 + 2`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 1, 4, 1, 9, newSInt(1, 1, 4, 1, 5), newSInt(2, 1, 8, 1, 9)),
						Range: newRange(1, 0, 1, 9),
					},
				},
			},
		},
		{
			input: `module main
a = "a" + "b"`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 1, 4, 1, 13, newSString("a", 1, 4, 1, 7), newSString("b", 1, 10, 1, 13)),
						Range: newRange(1, 0, 1, 13),
					},
				},
			},
		},
		{
			input: `module main
a = 1 + 2
b = "a" + "b"`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 1, 4, 1, 9, newSInt(1, 1, 4, 1, 5), newSInt(2, 1, 8, 1, 9)),
						Range: newRange(1, 0, 1, 9),
					},
					{
						Name:  "b",
						Expr:  newApp("+", 2, 4, 2, 13, newSString("a", 2, 4, 2, 7), newSString("b", 2, 10, 2, 13)),
						Range: newRange(2, 0, 2, 13),
					},
				},
			},
		},
		{
			input: `module main
f = \(a, b) => a + b`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name: "f",
						Expr: newFunc(1, 4, 1, 20,
							newApp("+", 1, 15, 1, 20, newRef("a", 1, 15, 1, 16), newRef("b", 1, 19, 1, 20)),
							newParam("a", 1, 6, 1, 7), newParam("b", 1, 9, 1, 10)),
						Range: newRange(1, 0, 1, 20),
					},
				},
			},
		},
		{
			input: `module main
f(a, b) = a + b`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name: "f",
						Expr: newFunc(1, 1, 1, 15,
							newApp("+", 1, 10, 1, 15, newRef("a", 1, 10, 1, 11), newRef("b", 1, 14, 1, 15)),
							newParam("a", 1, 2, 1, 3), newParam("b", 1, 5, 1, 6)),
						Range: newRange(1, 0, 1, 15),
					},
				},
			},
		},
		{
			input: `module main
f = g(1)`,
			expected: Module{
				Name: "main",
				Vars: []Var{
					{
						Name:  "f",
						Expr:  newApp("g", 1, 4, 1, 8, newSInt(1, 1, 6, 1, 7)),
						Range: newRange(1, 0, 1, 8),
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
