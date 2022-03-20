package vida

import (
	"bytes"
	"fmt"
	"math/rand"
	"time"
)

// Bytes models a dynamic mutable array of bytes.
type Bytes struct {
	Value []byte
}

// Value Interface
func (b *Bytes) TypeName() string {
	return "Bytes"
}

func (b *Bytes) Description() string {
	return string(b.Value)
}

func (b *Bytes) Equals(other Value) bool {
	if value, ok := other.(*Bytes); ok {
		return bytes.Equal(b.Value, value.Value)
	}
	return false
}

func (b *Bytes) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case *Bytes:
		switch op {
		case TKAdd:
			return &Bytes{Value: append(b.Value, rhs.Value...)}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
	}
}

func (b *Bytes) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, b)
}

func (b *Bytes) IsIterable() bool {
	return true
}

func (b *Bytes) MakeIterator() Iterator {
	return NewBytesIterator(b, false)
}

func (b *Bytes) IsHashable() bool {
	return false
}

func (b *Bytes) MakeHashKey() HashKey {
	return HashKey{}
}

func (b *Bytes) IsValueSemantics() bool {
	return false
}

func (b *Bytes) HasMethods() bool {
	return true
}

func (b *Bytes) GetMethod(name string) (Value, bool, error) {
	if method, ok := BytesInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, MethodNotDefined(name, b)
}

func (b *Bytes) Clone() Value {
	return &Bytes{Value: append([]byte{}, b.Value...)}
}

// Interface subscript operator.
func (b *Bytes) SubscriptGet(index Value) (Value, error) {
	switch idx := index.(type) {
	case IntegerNumber:
		i, length := idx.ToInt(), Int(len(b.Value))
		switch {
		case i >= -length && i < length:
			switch {
			case i < 0:
				return Byte(b.Value[i+length]), nil
			default:
				return Byte(b.Value[i]), nil
			}
		default:
			return nil, IndexOutOfRangeError(length, i)
		}
	default:
		return nil, ValueIsNotAnIndexError(index)
	}
}

func (b *Bytes) SubscriptSet(index, value Value) error {
	if numericValue, ok := value.(IntegerNumber); ok {
		switch idx := index.(type) {
		case IntegerNumber:
			i, length := idx.ToInt(), Int(len(b.Value))
			switch {
			case i >= -length && i < length:
				switch {
				case i < 0:
					b.Value[i+length] = byte(numericValue.ToInt())
				default:
					b.Value[i] = byte(numericValue.ToInt())
				}
				return nil
			default:
				return IndexOutOfRangeError(length, i)
			}
		default:
			return ValueIsNotAnIndexError(index)
		}
	}
	return BytesChangeMustBeWithNumericTypesOnly()
}

// Interface Slice operator.
func (b *Bytes) SliceGet(sliceType UInt32, low, high Value) (Value, error) {
	lidx, okL := low.(IntegerNumber)
	hidx, okH := high.(IntegerNumber)
	if !okL || !okH {
		if !okL {
			return nil, ValueIsNotAnIndexError(low)
		}
		return nil, ValueIsNotAnIndexError(high)
	}
	var l, h Int
	length := Int(len(b.Value))
	switch sliceType {
	case ExprColon:
		l, h = lidx.ToInt(), length
	case ColonExpr:
		h = hidx.ToInt()
	case ExprColonExpr:
		l, h = lidx.ToInt(), hidx.ToInt()
	case OnlyColon:
		h = length
	default:
		return nil, NeverShouldHaveHappened("wrong sliceType in Bytes SliceGet")
	}
	if l >= -length && l <= length && h >= -length && h <= length {
		if l < 0 {
			l += length
		}
		if h < 0 {
			h += length
		}
		if l <= h {
			return &Bytes{Value: b.Value[l:h]}, nil
		} else {
			return nil, IndexOutOfRangeError(length, l)
		}
	} else {
		if !(l >= -length && l <= length) {
			return nil, IndexOutOfRangeError(length, l)
		} else {
			return nil, IndexOutOfRangeError(length, h)
		}
	}
}

func (b *Bytes) SliceSet(sliceType UInt32, low, high, value Value) error {
	if numericValue, ok := value.(IntegerNumber); ok {
		lidx, okL := low.(IntegerNumber)
		hidx, okH := high.(IntegerNumber)
		if !okL || !okH {
			if !okL {
				return ValueIsNotAnIndexError(low)
			}
			return ValueIsNotAnIndexError(high)
		}
		var l, h Int
		length := Int(len(b.Value))
		switch sliceType {
		case ExprColon:
			l, h = lidx.ToInt(), length
		case ColonExpr:
			h = hidx.ToInt()
		case ExprColonExpr:
			l, h = lidx.ToInt(), hidx.ToInt()
		case OnlyColon:
			h = length
		default:
			return NeverShouldHaveHappened("wrong sliceType in Bytes SliceSet")
		}
		if l >= -length && l <= length && h >= -length && h <= length {
			if l < 0 {
				l += length
			}
			if h < 0 {
				h += length
			}
			if l <= h {
				for i := l; i < h; i++ {
					b.Value[i] = byte(numericValue.ToInt())
				}
				return nil
			} else {
				return IndexOutOfRangeError(length, l)
			}
		} else {
			if !(l >= -length && l <= length) {
				return IndexOutOfRangeError(length, l)
			} else {
				return IndexOutOfRangeError(length, h)
			}
		}
	}
	return BytesChangeMustBeWithNumericTypesOnly()
}

// BytesInterface is the collection of methods for the type Bytes.
var BytesInterface = Namespace{
	"isEmpty":       GFunction{Name: "isEmpty", Value: bytesIsEmpty},
	"length":        GFunction{Name: "length", Value: bytesLength},
	"clear":         GFunction{Name: "clear", Value: bytesClear},
	"contains":      GFunction{Name: "contains", Value: bytesContains},
	"clone":         GFunction{Name: "clone", Value: bytesClone},
	"remove":        GFunction{Name: "remove", Value: bytesRemove},
	"randomElement": GFunction{Name: "randomElement", Value: bytesRandomElement},
	"makeIterator":  GFunction{Name: "makeIterator", Value: bytesMakeIterator},
}

func bytesIsEmpty(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Bool(len(args[0].(*Bytes).Value) == 0), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesLength(args ...Value) (Value, error) {
	if len(args) == 1 {
		b := args[0].(*Bytes)
		return Int(len(b.Value)), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesClear(args ...Value) (Value, error) {
	if len(args) == 1 {
		b := args[0].(*Bytes)
		b.Value = b.Value[:0]
		return b, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesContains(args ...Value) (Value, error) {
	if len(args) == 2 {
		b := args[0].(*Bytes)
		if numeric, ok := args[1].(IntegerNumber); ok {
			value := byte(numeric.ToInt())
			for _, v := range b.Value {
				if v == value {
					return True, nil
				}
			}
			return False, nil
		}
		return False, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func bytesClone(args ...Value) (Value, error) {
	if len(args) == 1 {
		b := args[0].(*Bytes)
		return b.Clone(), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesRemove(args ...Value) (Value, error) {
	if len(args) == 2 {
		b := args[0].(*Bytes)
		if numeric, ok := args[1].(IntegerNumber); ok {
			length, elem, index, found := len(b.Value), byte(numeric.ToInt()), 0, false
			for i, v := range b.Value {
				if v == elem {
					found, index = true, i
					break
				}
			}
			if found {
				copy(b.Value[index:], b.Value[index+1:])
				b.Value = b.Value[:length-1]
			}
			return b, nil
		}
		return b, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func bytesRandomElement(args ...Value) (Value, error) {
	if len(args) == 1 {
		b := args[0].(*Bytes)
		if len(b.Value) == 0 {
			return NilValue, nil
		}
		rand.Seed(time.Now().UnixNano())
		return Byte(b.Value[rand.Int63n(int64(len(b.Value)))]), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func bytesMakeIterator(args ...Value) (Value, error) {
	if len(args) == 1 {
		b := args[0].(*Bytes)
		return NewBytesIterator(b, false), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}
