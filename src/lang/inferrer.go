package lang

import (
	"fmt"
)

type inferrer struct {
	module Module
}

func Infer(e Expr, module Module) (Type, error) {
	infer := &inferrer{
		module: module,
	}
	res, err := e.Accept(infer)
	return res.(Type), err
}

func (infer *inferrer) VisitFunc(e Func) (any, error) {
	returnType, err := e.Expr.Accept(infer)
	if err != nil {
		return nil, err
	}

	ts := []Type{}
	for range e.Params {
		ts = append(ts, TAny)
	}

	return FuncType{
		Arguments: ts,
		Returns:   returnType.(Type),
	}, nil
}

func (infer *inferrer) VisitRef(e Ref) (any, error) {
	v, ok := FindVar(e.Name, infer.module.Vars)
	if !ok {
		return nil, NewCodeError(e.Range, fmt.Sprintf("%s is not defined", e.Name))
	}
	return v.Expr.Accept(infer)
}

func (infer *inferrer) VisitApp(e App) (any, error) {
	ts := []Type{}
	for _, arg := range e.Args {
		t, err := arg.Accept(infer)
		if err != nil {
			return nil, err
		}
		ts = append(ts, t.(Type))
	}
	f, ok := FindFixtureFunctions(e.Func, ts)
	if !ok {
		return nil, NewCodeError(e.Range, fmt.Sprintf("%s(%s) is not defined", e.Func, ArgsType(ts).String()))
	}
	return f.Type.Returns, nil
}

func (infer *inferrer) VisitSString(e SString) (any, error) {
	return TString, nil
}

func (infer *inferrer) VisitSInt(e SInt) (any, error) {
	return TInt, nil
}
