package lang

import (
	"fmt"
	"strings"
)

type Type interface {
	String() string
	Applicable(ArgsType) (bool, error)
	Equal(Type) bool
	Contains(Type) bool
}

type DataType string
type ArgsType []Type
type FuncType struct {
	Arguments ArgsType
	Returns   Type
}

const (
	TInt    DataType = "int"
	TString DataType = "string"
	TAny    DataType = "any"
)

func (ft FuncType) String() string {
	return fmt.Sprintf("(%s): %s", ft.Arguments.String(), ft.Returns.String())
}

func (t DataType) String() string {
	return string(t)
}

func (t DataType) Applicable(argTypes ArgsType) (bool, error) {
	return false, nil
}

func (t1 DataType) Equal(t2 Type) bool {
	return t1.String() == t2.String()
}

func (t1 DataType) Contains(t2 Type) bool {
	return t1 == TAny || t1.String() == t2.String()
}

func (args ArgsType) String() string {
	ts := []string{}
	for _, t := range args {
		ts = append(ts, t.String())
	}
	return strings.Join(ts, ", ")
}

func (ft FuncType) Applicable(argTypes ArgsType) (bool, error) {
	if len(ft.Arguments) != len(argTypes) {
		return false, fmt.Errorf("wrong number of arguments")
	}
	for i, t := range ft.Arguments {
		if !t.Contains(argTypes[i]) {
			return false, fmt.Errorf("wrong type argument")
		}
	}
	return true, nil
}

func (t1 FuncType) Equal(t2 Type) bool {
	return t1.String() == t2.String()
}

func (t1 FuncType) Contains(t2 Type) bool {
	return false
}
