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
			input: "a = 1",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSInt(1, 0, 4, 0, 5),
						Range: newRange(0, 0, 0, 5),
					},
				},
			},
		},
		{
			input: "a = \"ok\"",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSString("ok", 0, 4, 0, 8),
						Range: newRange(0, 0, 0, 8),
					},
				},
			},
		},
		{
			input: "a = 1 + 2",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 4, 0, 9, newSInt(1, 0, 4, 0, 5), newSInt(2, 0, 8, 0, 9)),
						Range: newRange(0, 0, 0, 9),
					},
				},
			},
		},
		{
			input: "a = \"a\" + \"b\"",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 4, 0, 13, newSString("a", 0, 4, 0, 7), newSString("b", 0, 10, 0, 13)),
						Range: newRange(0, 0, 0, 13),
					},
				},
			},
		},
		{
			input: "a = 1 + 2\nb = \"a\" + \"b\"",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 4, 0, 9, newSInt(1, 0, 4, 0, 5), newSInt(2, 0, 8, 0, 9)),
						Range: newRange(0, 0, 0, 9),
					},
					{
						Name:  "b",
						Expr:  newApp("+", 1, 4, 1, 13, newSString("a", 1, 4, 1, 7), newSString("b", 1, 10, 1, 13)),
						Range: newRange(1, 0, 1, 13),
					},
				},
			},
		},
		{
			input: "f = \\(a, b) => a + b",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name: "f",
						Expr: newFunc(0, 4, 0, 20,
							newApp("+", 0, 15, 0, 20, newRef("a", 0, 15, 0, 16), newRef("b", 0, 19, 0, 20)),
							newParam("a", 0, 6, 0, 7), newParam("b", 0, 9, 0, 10)),
						Range: newRange(0, 0, 0, 20),
					},
				},
			},
		},
		{
			input: "f(a, b) = a + b",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name: "f",
						Expr: newFunc(0, 1, 0, 15,
							newApp("+", 0, 10, 0, 15, newRef("a", 0, 10, 0, 11), newRef("b", 0, 14, 0, 15)),
							newParam("a", 0, 2, 0, 3), newParam("b", 0, 5, 0, 6)),
						Range: newRange(0, 0, 0, 15),
					},
				},
			},
		},
		{
			input: "f = g(1)",
			expected: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "f",
						Expr:  newApp("g", 0, 4, 0, 8, newSInt(1, 0, 6, 0, 7)),
						Range: newRange(0, 0, 0, 8),
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
