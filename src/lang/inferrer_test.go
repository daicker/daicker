package lang

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInfer(t *testing.T) {
	tests := map[string]struct {
		input         Module
		expected      Type
		expectedError error
	}{
		"int": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSInt(1, 0, 0, 0, 1),
						Range: newRange(0, 0, 0, 2),
					},
				},
			},
			expected: TInt,
		},
		"string": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newSString("test", 0, 0, 0, 1),
						Range: newRange(0, 0, 0, 2),
					},
				},
			},
			expected: TString,
		},
		"string + string": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 0, 0, 1, newSString("test", 0, 0, 0, 2), newSString("test", 0, 0, 0, 3)),
						Range: newRange(0, 0, 0, 4),
					},
				},
			},
			expected: TString,
		},
		"int + int": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 0, 0, 1, newSInt(1, 0, 0, 0, 2), newSInt(2, 0, 0, 0, 3)),
						Range: newRange(0, 0, 0, 4),
					},
				},
			},
			expected: TInt,
		},
		"int + string": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("+", 0, 0, 0, 3, newSInt(1, 0, 0, 0, 1), newSString("test", 0, 2, 0, 3)),
						Range: newRange(0, 0, 0, 4),
					},
				},
			},
			expectedError: NewCodeError(newRange(0, 0, 0, 3), "+(int, string) is not defined"),
		},
		"\\(a, b) => a + b": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name: "f",
						Expr: newFunc(0, 0, 0, 0,
							newApp("+", 0, 0, 0, 1, newRef("a", 0, 0, 0, 2), newRef("b", 0, 0, 0, 3)),
							newParam("a", 0, 0, 0, 4), newParam("b", 0, 0, 0, 5)),
						Range: newRange(0, 0, 0, 6),
					},
				},
			},
			expected: newFuncType(TAny, TAny, TAny),
		},
		"\\(a) => a + 1": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name: "f",
						Expr: newFunc(0, 0, 0, 0,
							newApp("+", 0, 0, 0, 1, newRef("a", 0, 0, 0, 2), newSInt(1, 0, 0, 0, 3)),
							newParam("a", 0, 0, 0, 4)),
						Range: newRange(0, 0, 0, 5),
					},
				},
			},
			expected: newFuncType(TAny, TInt),
		},
		"f = \\(a) => a + 1\na = f(2)": {
			input: Module{
				Name: "",
				Vars: []Var{
					{
						Name:  "a",
						Expr:  newApp("f", 0, 0, 0, 1, newSInt(2, 0, 0, 0, 2)),
						Range: newRange(0, 0, 0, 3),
					},
					{
						Name: "f",
						Expr: newFunc(0, 0, 0, 4,
							newApp("+", 0, 0, 0, 5, newRef("a", 0, 0, 0, 6), newSInt(1, 0, 0, 0, 7)),
							newParam("a", 0, 0, 0, 8)),
						Range: newRange(0, 0, 0, 9),
					},
				},
			},
			expected: TInt,
		},
	}

	for name, test := range tests {
		typ, err := Infer(test.input.Vars[0].Expr, test.input)
		if test.expectedError != nil {
			assert.Equal(t, test.expectedError, err, "test is:\n%s", name)
		} else {
			assert.NoError(t, err, "test is:\n%s", name)
			assert.Equal(t, test.expected, typ, "test case is:\n%s", name)
		}
	}
}
