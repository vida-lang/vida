package vida

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
	"unicode/utf8"
)

type String struct {
	Value string
	Runes []rune
}

// Interface Value
func (s *String) TypeName() string {
	return "String"
}

func (s *String) Description() string {
	return s.Value
}

func (s *String) Equals(other Value) bool {
	if value, ok := other.(*String); ok {
		return s.Value == value.Value
	}
	return false
}

func (s *String) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case *String:
		switch op {
		case TKAdd:
			var builder strings.Builder
			builder.WriteString(s.Value)
			builder.WriteString(rhs.Value)
			return &String{Value: builder.String()}, nil
		case TKGT:
			return Bool(s.Value > rhs.Value), nil
		case TKGE:
			return Bool(s.Value >= rhs.Value), nil
		case TKLT:
			return Bool(s.Value < rhs.Value), nil
		case TKLE:
			return Bool(s.Value <= rhs.Value), nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], s, rhs)
		}
	case Int:
		switch op {
		case TKMul:
			var builder strings.Builder
			for i := Int(0); i < rhs; i++ {
				builder.WriteString(s.Value)
			}
			return &String{Value: builder.String()}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], s, rhs)
		}
	case UInt:
		switch op {
		case TKMul:
			var builder strings.Builder
			for i := UInt(0); i < rhs; i++ {
				builder.WriteString(s.Value)
			}
			return &String{Value: builder.String()}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], s, rhs)
		}
	case Byte:
		switch op {
		case TKMul:
			var builder strings.Builder
			for i := Byte(0); i < rhs; i++ {
				builder.WriteString(s.Value)
			}
			return &String{Value: builder.String()}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], s, rhs)
		}
	case Rune:
		switch op {
		case TKAdd:
			var builder strings.Builder
			builder.WriteString(s.Value)
			builder.WriteRune(rune(rhs))
			return &String{Value: builder.String()}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], s, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], s, rhs)
	}
}

func (s *String) PrefixOp(op byte) (Value, error) {
	return nil, TypeErrorInPrefixOperator(KindDescription[op], s)
}

func (s *String) IsIterable() bool {
	return true
}

func (s *String) MakeIterator() Iterator {
	return NewStringIterator(s, false)
}

func (s *String) IsHashable() bool {
	return true
}

func (s *String) MakeHashKey() HashKey {
	return HashKey{
		Type:             s.TypeName(),
		ValueDescription: s.Value,
	}
}

func (s *String) IsValueSemantics() bool {
	return true
}

func (s *String) HasMethods() bool {
	return true
}

func (s *String) GetMethod(name string) (Value, bool, error) {
	if method, ok := StringInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, MethodNotDefined(name, s)
}

func (s *String) Clone() Value {
	return &String{Value: s.Value}
}

// Interface SubscriptOperable.
func (s *String) SubscriptGet(index Value) (Value, error) {
	switch idx := index.(type) {
	case IntegerNumber:
		if s.Runes == nil {
			s.Runes = []rune(s.Value)
		}
		i, length := idx.ToInt(), Int(len(s.Runes))
		switch {
		case i >= -length && i < length:
			switch {
			case i < 0:
				return Rune(s.Runes[i+length]), nil
			default:
				return Rune(s.Runes[i]), nil
			}
		default:
			return nil, IndexOutOfRangeError(length, i)
		}
	default:
		return nil, ValueIsNotAnIndexError(index)
	}
}

func (s *String) SubscriptSet(index, value Value) error {
	return ValueIsImmutable(s)
}

// Interface Slice operator.
func (s *String) SliceGet(sliceType UInt32, low, high Value) (Value, error) {
	lidx, okL := low.(IntegerNumber)
	hidx, okH := high.(IntegerNumber)
	if !okL || !okH {
		if !okL {
			return nil, ValueIsNotAnIndexError(low)
		}
		return nil, ValueIsNotAnIndexError(high)
	}
	if s.Runes == nil {
		s.Runes = []rune(s.Value)
	}
	var l, h Int
	length := Int(len(s.Runes))
	switch sliceType {
	case exprColon:
		l, h = lidx.ToInt(), length
	case colonExpr:
		h = hidx.ToInt()
	case exprColonExpr:
		l, h = lidx.ToInt(), hidx.ToInt()
	case onlyColon:
		h = length
	default:
		return nil, NeverShouldHaveHappened("wrong sliceType in String SliceGet")
	}
	if l >= -length && l <= length && h >= -length && h <= length {
		if l < 0 {
			l += length
		}
		if h < 0 {
			h += length
		}
		if l <= h {
			cp := s.Runes[l:h]
			return &String{Value: string(cp), Runes: cp}, nil
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

func (s *String) SliceSet(sliceType UInt32, low, high, value Value) error {
	return ValueIsImmutable(s)
}

var StringInterface = Namespace{
	"isEmpty":       GFunction{Name: "isEmpty", Value: stringIsEmpty},
	"length":        GFunction{Name: "length", Value: stringLength},
	"contains":      GFunction{Name: "contains", Value: stringContains},
	"clone":         GFunction{Name: "clone", Value: stringClone},
	"randomElement": GFunction{Name: "randomElement", Value: stringRandomElement},
	"makeIterator":  GFunction{Name: "makeIterator", Value: stringMakeIterator},
	"join":          GFunction{Name: "join", Value: stringJoin},
	"containsAny":   GFunction{Name: "containsAny", Value: stringContainsAny},
	"fields":        GFunction{Name: "fields", Value: stringFields},
	"trim":          GFunction{Name: "trim", Value: stringTrim},
	"trimLeft":      GFunction{Name: "trimLeft", Value: stringTrimLeft},
	"trimRight":     GFunction{Name: "trimRight", Value: stringTrimRight},
	"toLower":       GFunction{Name: "toLower", Value: stringToLower},
	"toUpper":       GFunction{Name: "toUpper", Value: stringToUpper},
	"toTitle":       GFunction{Name: "toTittle", Value: stringToTitle},
	"split":         GFunction{Name: "split", Value: stringSplit},
	"repeat":        GFunction{Name: "repeat", Value: stringRepeat},
	"replace":       GFunction{Name: "replace", Value: stringReplace},
	"hasPrefix":     GFunction{Name: "hasPrefix", Value: stringHasPrefix},
	"hasSuffix":     GFunction{Name: "hasSuffix", Value: stringHasSuffix},
	"count":         GFunction{Name: "count", Value: stringCount},
	"runes":         GFunction{Name: "runes", Value: stringGetRunes},
	"bytes":         GFunction{Name: "runes", Value: stringGetBytes},
}

// Checks if a collection is empty.
func stringIsEmpty(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Bool(Int(len(args[0].(*String).Value)) == 0), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns the current self length.
func stringLength(args ...Value) (Value, error) {
	if len(args) == 1 {
		str, _ := args[0].(*String)
		return Int(utf8.RuneCountInString(str.Value)), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Joins the elements of an iterable to the end of the string.
func stringJoin(args ...Value) (Value, error) {
	if len(args) == 2 {
		if list, ok := args[1].(*List); ok {
			str, _ := args[0].(*String)
			var builder strings.Builder
			for _, item := range list.Elements {
				builder.WriteString(item.Description())
				builder.WriteString(str.Value)
			}
			result := builder.String()
			return &String{Value: strings.TrimRight(result, str.Value)}, nil
		}
		return nil, fmt.Errorf("the argument must be value of type List")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringContains(args ...Value) (Value, error) {
	if len(args) == 2 {
		if other, ok := args[1].(*String); ok {
			str := args[0].(*String)
			return Bool(strings.Contains(str.Value, other.Value)), nil
		}
		return nil, fmt.Errorf("the argument must be of type String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringContainsAny(args ...Value) (Value, error) {
	if len(args) == 2 {
		if other, ok := args[1].(*String); ok {
			str := args[0].(*String)
			return Bool(strings.ContainsAny(str.Value, other.Value)), nil
		}
		return nil, fmt.Errorf("the argument must be of type String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Splits a string around whitespaces.
func stringFields(args ...Value) (Value, error) {
	if len(args) == 1 {
		str, _ := args[0].(*String)
		result := strings.Fields(str.Value)
		newList := make([]Value, 0, len(result))
		for _, item := range result {
			newList = append(newList, &String{Value: item})
		}
		return &List{Elements: newList}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringTrim(args ...Value) (Value, error) {
	if len(args) == 2 {
		if other, ok := args[1].(*String); ok {
			str, _ := args[0].(*String)
			return &String{Value: strings.Trim(str.Value, other.Value)}, nil
		}
		return nil, fmt.Errorf("the argument must be of type String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringToLower(args ...Value) (Value, error) {
	if len(args) == 1 {
		str, _ := args[0].(*String)
		return &String{Value: strings.ToLower(str.Value)}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringToUpper(args ...Value) (Value, error) {
	if len(args) == 1 {
		str, _ := args[0].(*String)
		return &String{Value: strings.ToUpper(str.Value)}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringToTitle(args ...Value) (Value, error) {
	if len(args) == 1 {
		str, _ := args[0].(*String)
		return &String{Value: strings.ToTitle(str.Value)}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringTrimLeft(args ...Value) (Value, error) {
	if len(args) == 2 {
		if other, ok := args[1].(*String); ok {
			str, _ := args[0].(*String)
			return &String{Value: strings.TrimLeft(str.Value, other.Value)}, nil
		}
		return nil, fmt.Errorf("the argument must be of type String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringTrimRight(args ...Value) (Value, error) {
	if len(args) == 2 {
		if other, ok := args[1].(*String); ok {
			str, _ := args[0].(*String)
			return &String{Value: strings.TrimRight(str.Value, other.Value)}, nil
		}
		return nil, fmt.Errorf("the argument must be of type String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringSplit(args ...Value) (Value, error) {
	if len(args) == 2 {
		if sep, ok := args[1].(*String); ok {
			str, _ := args[0].(*String)
			result := strings.Split(str.Value, sep.Value)
			newList := make([]Value, 0, len(result))
			for _, item := range result {
				newList = append(newList, &String{Value: item})
			}
			return &List{Elements: newList}, nil
		}
		return nil, fmt.Errorf("the argument must be of type String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringRepeat(args ...Value) (Value, error) {
	if len(args) == 2 {
		if times, ok := args[1].(Int); ok {
			str := args[0].(*String)
			return &String{Value: strings.Repeat(str.Value, int(times))}, nil
		}
		return nil, fmt.Errorf("the argument must be a value of tyoe Int")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringReplace(args ...Value) (Value, error) {
	if len(args) == 4 {
		if times, okTimes := args[3].(Int); okTimes {
			if new, okNew := args[2].(*String); okNew {
				if old, okOld := args[1].(*String); okOld {
					str, _ := args[0].(*String)
					return &String{Value: strings.Replace(str.Value, old.Value, new.Value, int(times))}, nil
				}
				return nil, fmt.Errorf("the first argument must be a value of tyoe String")
			}
			return nil, fmt.Errorf("the second argument must be a value of tyoe String")
		}
		return nil, fmt.Errorf("the third argument must be a value of tyoe Int")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 3, len(args))
}

func stringHasPrefix(args ...Value) (Value, error) {
	if len(args) == 2 {
		if prefix, ok := args[1].(*String); ok {
			str := args[0].(*String)
			return Bool(strings.HasPrefix(str.Value, prefix.Value)), nil
		}
		return nil, fmt.Errorf("the argument must be a value of tyoe String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func stringHasSuffix(args ...Value) (Value, error) {
	if len(args) == 2 {
		if suffix, ok := args[1].(*String); ok {
			str, _ := args[0].(*String)
			return Bool(strings.HasSuffix(str.Value, suffix.Value)), nil
		}
		return nil, fmt.Errorf("the argument must be a value of tyoe String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns an iterator object ready to iterate over self.
// The iteration goes over runes.
func stringMakeIterator(args ...Value) (Value, error) {
	if len(args) == 1 {
		str := args[0].(*String)
		return NewStringIterator(str, false), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringGetRunes(args ...Value) (Value, error) {
	if len(args) == 1 {
		str := args[0].(*String)
		result := make([]Value, 0, utf8.RuneCountInString(str.Value))
		for _, r := range str.Value {
			result = append(result, Rune(r))
		}
		return &List{Elements: result}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringGetBytes(args ...Value) (Value, error) {
	if len(args) == 1 {
		str := args[0].(*String)
		return &Bytes{Value: []byte(str.Value)}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringClone(args ...Value) (Value, error) {
	if len(args) == 1 {
		str := args[0].(*String)
		return &String{Value: str.Value}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringRandomElement(args ...Value) (Value, error) {
	if len(args) == 1 {
		str := args[0].(*String)
		rand.Seed(time.Now().UnixNano())
		if str.Runes == nil {
			str.Runes = []rune(str.Value)
		}
		return &String{Value: string(str.Runes[rand.Int63n(int64(len(str.Runes)))])}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func stringCount(args ...Value) (Value, error) {
	if len(args) == 2 {
		if substr, ok := args[1].(*String); ok {
			str := args[0].(*String).Value
			return Int(strings.Count(str, substr.Value)), nil
		}
		return nil, fmt.Errorf("the argument must be a value of tyoe String")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}
