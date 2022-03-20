package stdlib

import (
	"bytes"
	"fmt"
	"reflect"

	"github.com/vida-lang/vida-lang/vida"
)

// BytesBuffer models a bytes buffer with operations to read and write aribitrary bytes.
type BytesBuffer struct {
	Buffer bytes.Buffer
}

// Interface Value
func (buffer *BytesBuffer) TypeName() string {
	return "BytesBuffer"
}

func (buffer *BytesBuffer) Description() string {
	return fmt.Sprintf("BytesBuffer(at:%p)", buffer)
}

func (buffer *BytesBuffer) Equals(other vida.Value) bool {
	if value, ok := other.(*BytesBuffer); ok {
		return reflect.DeepEqual(buffer, value)
	}
	return false
}

func (buffer *BytesBuffer) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.TypeErrorInBinaryOperator(vida.KindDescription[op], buffer, rhs)
}

func (buffer *BytesBuffer) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.TypeErrorInPrefixOperator(vida.KindDescription[op], buffer)
}

func (buffer *BytesBuffer) IndexGet(index vida.Value) (vida.Value, error) {
	return vida.NilValue, vida.ValueIsNotAnIndexError(buffer)
}

func (buffer *BytesBuffer) IndexSet(index, value vida.Value) error {
	return vida.ValueDoesNotSupportSubscription(buffer)
}

func (buffer *BytesBuffer) IsIterable() bool {
	return false
}

func (buffer *BytesBuffer) MakeIterator() vida.Iterator {
	return nil
}

func (buffer *BytesBuffer) IsHashable() bool {
	return false
}

func (buffer *BytesBuffer) MakeHashKey() vida.HashKey {
	return vida.HashKey{}
}

func (buffer *BytesBuffer) IsValueSemantics() bool {
	return false
}

func (buffer *BytesBuffer) HasMethods() bool {
	return true
}

func (buffer *BytesBuffer) GetMethod(name string) (vida.Value, bool, error) {
	if method, ok := BytesBufferInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, vida.MethodNotDefined(name, buffer)
}

func (buffer *BytesBuffer) Clone() vida.Value {
	var newBuffer = bytes.NewBuffer(buffer.Buffer.Bytes())
	return &BytesBuffer{
		Buffer: *newBuffer,
	}
}

// BytesBufferInterface is the collection of methods for the type Bytes.
var BytesBufferInterface vida.Namespace

func loadBytesBuffer() vida.Importable {
	BytesBufferInterface = vida.Namespace{
		"isEmpty":     vida.GFunction{Name: "isEmpty", Value: bytesIsEmpty},
		"length":      vida.GFunction{Name: "length", Value: bytesLength},
		"bytes":       vida.GFunction{Name: "bytes", Value: bytesGetBytes},
		"reset":       vida.GFunction{Name: "reset", Value: bytesReset},
		"write":       vida.GFunction{Name: "write", Value: bytesWrite},
		"writeByte":   vida.GFunction{Name: "writeByte", Value: bytesWriteByte},
		"writeRune":   vida.GFunction{Name: "writeRune", Value: bytesWriteRune},
		"writeString": vida.GFunction{Name: "writeString", Value: bytesWriteString},
		"buildString": vida.GFunction{Name: "buildString", Value: bytesToString},
	}
	gmodule := vida.GModule{Name: "bytesBuffer", Namespace: vida.Namespace{
		"newBuffer": vida.GFunction{Name: "newBuffer", Value: newBytesBuffer},
	}}
	return gmodule
}

func newBytesBuffer(args ...vida.Value) (vida.Value, error) {
	return &BytesBuffer{}, nil
}

func bytesIsEmpty(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		return vida.Bool(args[0].(*BytesBuffer).Buffer.Len() == 0), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesLength(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		return vida.Int(args[0].(*BytesBuffer).Buffer.Len()), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesGetBytes(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		return &vida.Bytes{Value: args[0].(*BytesBuffer).Buffer.Bytes()}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesReset(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		args[0].(*BytesBuffer).Buffer.Reset()
		return vida.NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesWrite(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		buffer := args[0].(*BytesBuffer)
		if other, ok := args[1].(*vida.Bytes); ok {
			if n, err := buffer.Buffer.Write(other.Value); err == nil {
				return vida.Int(n), nil
			} else {
				return nil, err
			}
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func bytesWriteByte(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		bytes := args[0].(*BytesBuffer)
		if other, ok := args[1].(vida.IntegerNumber); ok {
			if err := bytes.Buffer.WriteByte(byte(other.ToInt())); err == nil {
				return vida.NilValue, nil
			} else {
				return nil, err
			}
		}
		return nil, fmt.Errorf("expected a list of bytes as argument, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))

}

func bytesWriteRune(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		bytes := args[0].(*BytesBuffer)
		if other, ok := args[1].(vida.Rune); ok {
			if n, err := bytes.Buffer.WriteRune(rune(other)); err == nil {
				return vida.Int(n), nil
			} else {
				return nil, err
			}
		}
		return nil, fmt.Errorf("expected a list of bytes as argument, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))

}

func bytesWriteString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		bytes := args[0].(*BytesBuffer)
		if other, ok := args[1].(*vida.String); ok {
			if n, err := bytes.Buffer.WriteString(other.Value); err == nil {
				return vida.Int(n), nil
			} else {
				return nil, err
			}
		}
		return nil, fmt.Errorf("expected a list of bytes as argument, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))

}

func bytesToString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		bytes := args[0].(*BytesBuffer)
		return &vida.String{Value: bytes.Buffer.String()}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}
