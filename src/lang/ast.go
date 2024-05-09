package lang

type ASTVisitor interface {
	VisitFunc(e Func) (any, error)
	VisitRef(e Ref) (any, error)
	VisitApp(e App) (any, error)
	VisitSString(e SString) (any, error)
	VisitSInt(e SInt) (any, error)
}

type Module struct {
	Name  string
	Vars  []Var
	Range Range
}

func FindVar(name string, vars []Var) (Var, bool) {
	for _, v := range vars {
		if v.Name == name {
			return v, true
		}
	}
	return Var{}, false
}

type Expr interface {
	GetRange() Range
	Accept(v ASTVisitor) (any, error)
}

// Var represents a variable assignment.
type Var struct {
	Name  string
	Expr  Expr
	Range Range
}

// Func represents a function.
type Func struct {
	Params []Param
	Expr   Expr
	Range  Range
}

type Param struct {
	Name  string
	Range Range
}

func (e Func) GetRange() Range {
	return e.Range
}

func (e Func) Accept(v ASTVisitor) (any, error) {
	return v.VisitFunc(e)
}

// Ref represents a reference of variable.
type Ref struct {
	Name  string
	Range Range
}

func (e Ref) GetRange() Range {
	return e.Range
}

func (e Ref) Accept(v ASTVisitor) (any, error) {
	return v.VisitRef(e)
}

// App represents a function application.
type App struct {
	Func  string
	Args  []Expr
	Range Range
}

func (e App) GetRange() Range {
	return e.Range
}

func (e App) Accept(v ASTVisitor) (any, error) {
	return v.VisitApp(e)
}

// Scaler Integer represents integer value
type SInt struct {
	Value int
	Range Range
}

func (e SInt) GetRange() Range {
	return e.Range
}

func (e SInt) Accept(v ASTVisitor) (any, error) {
	return v.VisitSInt(e)
}

// Scaler String represents string value
type SString struct {
	Value string
	Range Range
}

func (e SString) GetRange() Range {
	return e.Range
}

func (e SString) Accept(v ASTVisitor) (any, error) {
	return v.VisitSString(e)
}
