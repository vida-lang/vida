package vida

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
	"unsafe"
)

// Map is a hash table or dictionary.
type Map map[HashKey]Pair

// Interface Value
func (m Map) TypeName() string {
	return "Map"
}

func (m Map) Description() string {
	var builder strings.Builder
	builder.WriteString("map[ ")
	for _, pair := range m {
		builder.WriteString(fmt.Sprintf("%v:%v ", pair.key.Description(), pair.value.Description()))
	}
	builder.WriteString("]")
	return builder.String()
}

func (m Map) Equals(other Value) bool {
	if value, ok := other.(Map); ok {
		if unsafe.Pointer(&m) == unsafe.Pointer(&value) {
			return true
		}
		if len(m) != len(value) {
			return false
		}
		for k, v := range m {
			if value, ok := value[k]; !(ok && v.Equals(value)) {
				return false
			}
		}
		return true
	}
	return false
}

func (m Map) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case Map:
		switch op {
		case TKAdd:
			newMap := make(Map)
			for key, value := range rhs {
				newMap[key] = value
			}
			for key, value := range m {
				newMap[key] = value
			}
			return newMap, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], m, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], m, rhs)
	}
}

func (m Map) PrefixOp(op byte) (Value, error) {
	return nil, TypeErrorInPrefixOperator(KindDescription[op], m)
}

func (m Map) IsIterable() bool {
	return true
}

func (m Map) MakeIterator() Iterator {
	return NewMapIterator(m, false)
}

func (m Map) IsHashable() bool {
	return false
}

func (m Map) MakeHashKey() HashKey {
	return HashKey{}
}

func (m Map) IsValueSemantics() bool {
	return false
}

func (m Map) HasMethods() bool {
	return true
}

func (m Map) GetMethod(name string) (Value, bool, error) {
	if method, ok := MapInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, MethodNotDefined(name, m)
}

func (m Map) Clone() Value {
	newMap := make(Map)
	for k, v := range m {
		newMap[k] = v.Clone().(Pair)
	}
	return newMap
}

// Interface subscription operator.
func (m Map) SubscriptGet(index Value) (Value, error) {
	if index.IsHashable() {
		if value, ok := m[index.MakeHashKey()]; ok {
			return value.value, nil
		} else {
			return NilValue, nil
		}
	} else {
		return nil, ValueNotHashableError(index)
	}
}

func (m Map) SubscriptSet(index, value Value) error {
	if index.IsHashable() {
		m[index.MakeHashKey()] = Pair{key: index, value: value}
		return nil
	}
	return ValueNotHashableError(index)
}

var MapInterface = Namespace{
	"isEmpty":       GFunction{Name: "isEmpty", Value: mapIsEmpty},
	"length":        GFunction{Name: "length", Value: mapLength},
	"clear":         GFunction{Name: "clear", Value: mapClear},
	"contains":      GFunction{Name: "contains", Value: mapContains},
	"clone":         GFunction{Name: "clone", Value: mapClone},
	"randomElement": GFunction{Name: "randomElement", Value: mapRandomElement},
	"makeIterator":  GFunction{Name: "makeIterator", Value: mapMakeIterator},
	"remove":        GFunction{Name: "remove", Value: mapDelete},
	"keys":          GFunction{Name: "keys", Value: mapKeys},
	"values":        GFunction{Name: "values", Value: mapValues},
	"pairs":         GFunction{Name: "pairs", Value: mapItems},
}

// Checks if a collection is empty.
func mapIsEmpty(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Bool(len(args[0].(Map)) == 0), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Sets the length of self to zero
func mapClear(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		for key := range mmap {
			delete(mmap, key)
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns the current self length.
func mapLength(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		return Int(len(mmap)), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a list with the keys of the map.
func mapKeys(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		keys := make([]Value, 0, len(mmap))
		for _, v := range mmap {
			keys = append(keys, v.key)
		}
		return &List{Elements: keys}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a list with the values of the map.
func mapValues(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		values := make([]Value, 0, len(mmap))
		for _, v := range mmap {
			values = append(values, v.value)
		}
		return &List{Elements: values}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a list of lists of pairs v, k
func mapItems(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		list := make([]Value, 0, len(mmap))
		for _, v := range mmap {
			list = append(list, v)
		}
		return &List{Elements: list}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Deletes the given entry of the map.
func mapDelete(args ...Value) (Value, error) {
	if len(args) == 2 {
		mmap, _ := args[0].(Map)
		if value, ok := mmap[args[1].MakeHashKey()]; ok {
			delete(mmap, args[1].MakeHashKey())
			return value, nil
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns a shallow copy of self.
func mapClone(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		return mmap.Clone(), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a random element from self without removing it.
func mapRandomElement(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		length := int64(len(mmap))
		if length == 0 {
			return NilValue, nil
		}
		rand.Seed(time.Now().UnixNano())
		randomIndex := rand.Int63n(length)
		index := int64(0)
		for key := range mmap {
			if randomIndex == index {
				return mmap[key], nil
			}
			index++
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns an iterator object ready to iterate over self.
func mapMakeIterator(args ...Value) (Value, error) {
	if len(args) == 1 {
		mmap, _ := args[0].(Map)
		return NewMapIterator(mmap, false), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func mapContains(args ...Value) (Value, error) {
	if len(args) == 2 {
		mmap, _ := args[0].(Map)
		if args[1].IsHashable() {
			_, ok := mmap[args[1].MakeHashKey()]
			return Bool(ok), nil
		}
		return nil, fmt.Errorf("expected a hashable value as argument")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))

}
