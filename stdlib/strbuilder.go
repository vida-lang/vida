package stdlib

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/vida-lang/vida/vida"
)

// StringBuilder models an efficient way to build Strings.
type StringBuilder struct {
	Builder strings.Builder
}

// Interface Value
func (builder *StringBuilder) TypeName() string {
	return "StringBuilder"
}

func (builder *StringBuilder) Description() string {
	return fmt.Sprintf("StringBuilder(at:%p)", builder)
}

func (builder *StringBuilder) Equals(other vida.Value) bool {
	if value, ok := other.(*StringBuilder); ok {
		return reflect.DeepEqual(builder, value)
	}
	return false
}

func (builder *StringBuilder) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.TypeErrorInBinaryOperator(vida.KindDescription[op], builder, rhs)
}

func (builder *StringBuilder) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.TypeErrorInPrefixOperator(vida.KindDescription[op], builder)
}

func (builder *StringBuilder) IsIterable() bool {
	return false
}

func (builder *StringBuilder) MakeIterator() vida.Iterator {
	return nil
}

func (builder *StringBuilder) IsHashable() bool {
	return false
}

func (builder *StringBuilder) MakeHashKey() vida.HashKey {
	return vida.HashKey{}
}

func (builder *StringBuilder) IsValueSemantics() bool {
	return false
}

func (builder *StringBuilder) HasMethods() bool {
	return true
}

func (builder *StringBuilder) GetMethod(name string) (vida.Value, bool, error) {
	if method, ok := StringBuilderInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, vida.MethodNotDefined(name, builder)
}

func (builder *StringBuilder) Clone() vida.Value {
	var newBuilder strings.Builder
	newBuilder.WriteString(builder.Builder.String())
	return &StringBuilder{
		Builder: newBuilder,
	}
}

var StringBuilderInterface vida.Namespace

func loadStringBuilder() vida.Importable {
	StringBuilderInterface = vida.Namespace{
		"buildString": vida.GFunction{Name: "buildString", Value: buildString},
		"reset":       vida.GFunction{Name: "reset", Value: resetBuilder},
		"write":       vida.GFunction{Name: "write", Value: writeBytes},
		"writeByte":   vida.GFunction{Name: "writeByte", Value: writeByte},
		"writeString": vida.GFunction{Name: "writeString", Value: writeString},
		"writeRune":   vida.GFunction{Name: "writeRune", Value: writeRune},
	}
	gmodule := vida.GModule{Name: "stringBuilder", Namespace: vida.Namespace{
		"new": vida.GFunction{Name: "new", Value: newStringBuilder},
	}}
	return gmodule
}

func newStringBuilder(args ...vida.Value) (vida.Value, error) {
	return &StringBuilder{}, nil
}

func buildString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if builder, ok := args[0].(*StringBuilder); ok {
			return &vida.String{Value: builder.Builder.String()}, nil
		}
		return nil, vida.ExpectedTypeAndGotOtherType("StringBuilder", args[0].TypeName())
	}
	return nil, vida.ArityError(1, vida.UInt32(len(args)))
}

func resetBuilder(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if builder, ok := args[0].(*StringBuilder); ok {
			builder.Builder.Reset()
			return vida.NilValue, nil
		}
		return nil, vida.ExpectedTypeAndGotOtherType("StringBuilder", args[0].TypeName())
	}
	return nil, vida.ArityError(1, vida.UInt32(len(args)))
}

func writeBytes(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if builder, ok := args[0].(*StringBuilder); ok {
			if bbyte, ok := args[1].(*vida.Bytes); ok {
				builder.Builder.Write(bbyte.Value)
				return vida.NilValue, nil
			}
			return nil, vida.ExpectedTypeAndGotOtherType("Bytes", args[1].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("StringBuilder", args[0].TypeName())
	}
	return nil, vida.ArityError(1, vida.UInt32(len(args)))
}

func writeByte(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if builder, ok := args[0].(*StringBuilder); ok {
			if bbyte, ok := args[1].(vida.Byte); ok {
				builder.Builder.WriteByte(byte(bbyte))
				return vida.NilValue, nil
			}
			return nil, vida.ExpectedTypeAndGotOtherType("Byte", args[1].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("StringBuilder", args[0].TypeName())
	}
	return nil, vida.ArityError(1, vida.UInt32(len(args)))
}

func writeString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if builder, ok := args[0].(*StringBuilder); ok {
			if bbyte, ok := args[1].(*vida.String); ok {
				builder.Builder.WriteString(bbyte.Value)
				return vida.NilValue, nil
			}
			return nil, vida.ExpectedTypeAndGotOtherType("String", args[1].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("StringBuilder", args[0].TypeName())
	}
	return nil, vida.ArityError(1, vida.UInt32(len(args)))
}

func writeRune(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if builder, ok := args[0].(*StringBuilder); ok {
			if bbyte, ok := args[1].(vida.Rune); ok {
				builder.Builder.WriteRune(rune(bbyte))
				return vida.NilValue, nil
			}
			return nil, vida.ExpectedTypeAndGotOtherType("Rune", args[1].TypeName())
		}
		return nil, vida.ExpectedTypeAndGotOtherType("StringBuilder", args[0].TypeName())
	}
	return nil, vida.ArityError(1, vida.UInt32(len(args)))
}
