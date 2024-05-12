package lang

import (
	"fmt"
)

type inferrer struct {
	module Module
	env    map[string]Type
}

func Infer(e Expr, module Module) (Type, error) {
	infer := &inferrer{
		module: module,
		env:    map[string]Type{},
	}
	res, err := e.Accept(infer)
	if err != nil {
		return nil, err
	}
	return res.(Type), nil
}

func (infer *inferrer) VisitFunc(e Func) (any, error) {
	ts := ArgsType{}
	for _, p := range e.Params {
		infer.env[p.Name] = TAny
		ts = append(ts, TAny)
	}

	returnType, err := e.Expr.Accept(infer)
	if err != nil {
		return nil, err
	}

	return FuncType{
		Arguments: ts,
		Returns:   returnType.(Type),
	}, nil
}

func (infer *inferrer) VisitRef(e Ref) (any, error) {
	t, ok := infer.env[e.Name]
	if ok {
		return t, nil
	}
	v, ok := FindVar(e.Name, infer.module.Vars)
	if !ok {
		return nil, NewCodeError(e.Range, fmt.Sprintf("%s is not defined", e.Name))
	}
	return v.Expr.Accept(infer)
}

func (infer *inferrer) VisitApp(e App) (any, error) {
	ts := ArgsType{}
	for _, arg := range e.Args {
		t, err := arg.Accept(infer)
		if err != nil {
			return nil, err
		}
		ts = append(ts, t.(Type))
	}
	v, ok := FindVar(e.Func, infer.module.Vars)
	if ok {
		t, err := v.Expr.Accept(infer)
		if err != nil {
			return nil, err
		}
		switch t := t.(type) {
		case FuncType:
			ok, _ := t.Applicable(ts)
			if ok {
				return t.Returns, nil
			}
		default:
		}
	}
	fs := FindFixtureFunctions(e.Func, ts)
	if len(fs) == 0 {
		return nil, NewCodeError(e.Range, fmt.Sprintf("%s(%s) is not defined", e.Func, ArgsType(ts).String()))
	}
	if len(fs) > 1 {
		return TAny, nil
	}
	return fs[0].Type.Returns, nil
}

func (infer *inferrer) VisitSString(e SString) (any, error) {
	return TString, nil
}

func (infer *inferrer) VisitSInt(e SInt) (any, error) {
	return TInt, nil
}
