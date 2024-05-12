package lang

func newFuncType(ts ...Type) Type {
	return FuncType{
		Arguments: ts[0 : len(ts)-1],
		Returns:   ts[len(ts)-1],
	}
}
