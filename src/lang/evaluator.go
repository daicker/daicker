package lang

import (
	"fmt"
)

type evaluator struct {
	target     string
	module     Module
	scopedVars []Var
}

func Eval(target string, module Module) (any, error) {
	eval := &evaluator{
		target:     target,
		module:     module,
		scopedVars: []Var{},
	}
	v, ok := FindVar(target, module.Vars)
	if !ok {
		return nil, fmt.Errorf("%s is not defined", target)
	}
	return v.Expr.Accept(eval)
}

func (eval *evaluator) VisitFunc(e Func) (any, error) {
	return func(a []any) {

	}, nil
}

func (eval *evaluator) VisitRef(e Ref) (any, error) {
	v, ok := FindVar(e.Name, eval.module.Vars)
	if !ok {
		return nil, NewCodeError(e.Range, fmt.Sprintf("%s is not defined", e.Name))
	}
	return v.Expr.Accept(eval)
}

func (eval *evaluator) VisitApp(e App) (any, error) {
	argType := ArgsType{}
	args := []any{}
	for _, arg := range e.Args {
		t, err := Infer(e, eval.module)
		if err != nil {
			return nil, err
		}
		argType = append(argType, t)

		res, err := arg.Accept(eval)
		if err != nil {
			return nil, err
		}
		args = append(args, res)
	}
	// f, ok := FindFunc(v.Func, ts, vars)
	// if ok {
	// 	for i, p := range f.Expr.(Func).Params {
	// 		vars = append(vars, Var{
	// 			Name:  p.Name,
	// 			Expr:  e.Args[i],
	// 			Range: e.Range,
	// 		})
	// 	}
	// 	return f.Expr.Evaluate(vars)
	// }

	f, ok := FindFixtureFunctions(e.Func, argType)
	if !ok {
		return nil, NewCodeError(e.Range, fmt.Sprintf("%s(%s) is not defined", e.Func, argType.String()))
	}
	return f.Function(args)
}

func (eval *evaluator) VisitSString(e SString) (any, error) {
	return e.Value, nil
}

func (eval *evaluator) VisitSInt(e SInt) (any, error) {
	return e.Value, nil
}
