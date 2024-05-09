package lang

func newApp(f string, startLine, startColumn, endLine, endColumn int, es ...Expr) *App {
	return &App{
		Func:  f,
		Args:  es,
		Range: newRange(startLine, startColumn, endLine, endColumn),
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
