package lang

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestEval(t *testing.T) {
	m := Module{
		Name: "main",
		Vars: []Var{
			{
				Name: "main",
				Expr: App{
					Func: "+",
					Args: []Expr{SInt{Value: 1}, SInt{Value: 2}},
				},
			},
		},
	}
	res, err := Eval("main", m)
	assert.NoError(t, err)
	assert.Equal(t, 3, res)
}
