package vida

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
	"unsafe"
)

// Set is a collection of unique values.
type Set map[HashKey]Value

// Interface Value
func (set Set) TypeName() string {
	return "Set"
}

func (set Set) Description() string {
	var builder strings.Builder
	builder.WriteString("set{ ")
	for _, value := range set {
		builder.WriteString(value.Description())
		builder.WriteString(" ")
	}
	builder.WriteString("}")
	return builder.String()
}

func (set Set) Equals(other Value) bool {
	if value, ok := other.(Set); ok {
		if unsafe.Pointer(&set) == unsafe.Pointer(&value) {
			return true
		}
		if len(set) != len(value) {
			return false
		}
		for k := range set {
			if _, ok := value[k]; !ok {
				return false
			}
		}
		return true
	}
	return false
}

func (set Set) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case Set:
		switch op {
		case TKAdd:
			set := make(Set)
			for key, value := range rhs {
				set[key] = value
			}
			for key, value := range set {
				set[key] = value
			}
			return set, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], set, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], set, rhs)
	}
}

func (set Set) PrefixOp(op byte) (Value, error) {
	return nil, TypeErrorInPrefixOperator(KindDescription[op], set)
}

func (set Set) IsIterable() bool {
	return true
}

func (set Set) MakeIterator() Iterator {
	return NewSetIterator(set, false)
}

func (set Set) IsHashable() bool {
	return false
}

func (set Set) MakeHashKey() HashKey {
	return HashKey{}
}

func (set Set) IsValueSemantics() bool {
	return false
}

func (set Set) HasMethods() bool {
	return true
}

func (set Set) GetMethod(name string) (Value, bool, error) {
	if method, ok := SetInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, MethodNotDefined(name, set)
}

func (set Set) Clone() Value {
	newSet := make(Set)
	for k, v := range set {
		newSet[k] = v.Clone()
	}
	return newSet
}

var SetInterface = Namespace{
	// Common collections interface:
	"isEmpty":             GFunction{Name: "isEmpty", Value: setIsEmpty},
	"length":              GFunction{Name: "length", Value: setLength},
	"clear":               GFunction{Name: "clear", Value: setClear},
	"contains":            GFunction{Name: "contains", Value: setMemebership},
	"clone":               GFunction{Name: "clone", Value: setClone},
	"randomElement":       GFunction{Name: "randomElement", Value: setRandomElement},
	"makeIterator":        GFunction{Name: "makeIterator", Value: setMakeIterator},
	"remove":              GFunction{Name: "remove", Value: setRemoveElements},
	"add":                 GFunction{Name: "add", Value: setAddElements},
	"difference":          GFunction{Name: "difference", Value: setDifference},
	"merge":               GFunction{Name: "merge", Value: setMerge},
	"merged":              GFunction{Name: "merged", Value: setMerged},
	"toList":              GFunction{Name: "toList", Value: setToList},
	"intersection":        GFunction{Name: "intersection", Value: setIntersection},
	"isDisjoint":          GFunction{Name: "isDisjoint", Value: setIsDisjoint},
	"isSubset":            GFunction{Name: "isSubset", Value: setIsSubset},
	"isSuperset":          GFunction{Name: "isSuperset", Value: setIsSuperSet},
	"symmetricDifference": GFunction{Name: "symmetricDifference", Value: setSymmetricDifference},
	"union":               GFunction{Name: "union", Value: setUnion},
	// To do ...
	// Cartesian Product
	// Power Set
	// isProperSubset
	// isProperSuperset
}

// Checks if a collection is empty.
func setIsEmpty(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Bool(len(args[0].(Set)) == 0), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Sets the length of self to zero
func setClear(args ...Value) (Value, error) {
	if len(args) == 1 {
		set, _ := args[0].(Set)
		for key := range set {
			delete(set, key.MakeHashKey())
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns the current self length.
func setLength(args ...Value) (Value, error) {
	if len(args) == 1 {
		set, _ := args[0].(Set)
		return Int(len(set)), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Removes from self the elements passed as arguments.
func setRemoveElements(args ...Value) (Value, error) {
	if len(args) >= 2 {
		set := args[0].(Set)
		for i := 1; i < len(args); i++ {
			if args[i].IsHashable() {
				delete(set, args[i].MakeHashKey())
			}
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

// Adds to self the elements passed as arguments.
func setAddElements(args ...Value) (Value, error) {
	if len(args) >= 2 {
		set := args[0].(Set)
		for i := 1; i < len(args); i++ {
			if args[i].IsHashable() {
				set[args[i].MakeHashKey()] = args[i]
			} else {
				return nil, fmt.Errorf("value is not hashable")
			}
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

func setMemebership(args ...Value) (Value, error) {
	if len(args) == 2 {
		set, _ := args[0].(Set)
		if args[1].IsHashable() {
			_, ok := set[args[1].MakeHashKey()]
			return Bool(ok), nil
		}
		return False, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args)-1)
}

// Returns a shallow copy of self.
func setClone(args ...Value) (Value, error) {
	if len(args) == 1 {
		set := args[0].(Set)
		return set.Clone(), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a set with the elements of self that are not in others.
func setDifference(args ...Value) (Value, error) {
	if len(args) >= 2 {
		set, _ := args[0].(Set)
		union := make(Set)
		result := make(Set)
		// Union of all sets.
		for i := 1; i < len(args); i++ {
			switch args[i].(type) {
			case Set:
				argSet, _ := args[i].(Set)
				for key, value := range argSet {
					union[key] = value
				}
			default:
				return nil, fmt.Errorf("%v is not a set", args[i])
			}
		}
		// Looking for the difference with self.
		for key, value := range set {
			if _, ok := union[key]; !ok {
				result[key] = value
			}
		}
		return result, nil
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

// Merges the other set with self.
func setMerge(args ...Value) (Value, error) {
	if len(args) >= 2 {
		set, _ := args[0].(Set)
		for i := 1; i < len(args); i++ {
			if other, ok := args[i].(Set); ok {
				for element, value := range other {
					set[element] = value
				}
			} else {
				return nil, fmt.Errorf("expected at set as argument and got %T", args[i])
			}
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

// Merges the other set and self and return the new merged set without change self.
func setMerged(args ...Value) (Value, error) {
	if len(args) >= 2 {
		merged := make(Set)
		for i := 0; i < len(args); i++ {
			if set, ok := args[i].(Set); ok {
				for element, value := range set {
					merged[element] = value
				}
			} else {
				return nil, fmt.Errorf("expected a set as argument and got %T", args[i])
			}
		}
		return merged, nil
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

// Returns a list with the elements of self.
func setToList(args ...Value) (Value, error) {
	if len(args) == 1 {
		set, _ := args[0].(Set)
		list := make([]Value, 0, len(set))
		for _, value := range set {
			list = append(list, value)
		}
		return &List{Elements: list}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a set with the elements of self and the elements of others.
func setUnion(args ...Value) (Value, error) {
	if len(args) >= 2 {
		unionSet := make(Set)
		for i := 0; i < len(args); i++ {
			switch args[i].(type) {
			case Set:
				set := args[i].(Set)
				for key, value := range set {
					unionSet[key] = value
				}
			default:
				return nil, fmt.Errorf("%v is not a set", args[i])
			}
		}
		return unionSet, nil
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

// Returns a set with the elements common to all sets.
func setIntersection(args ...Value) (Value, error) {
	length := len(args)
	if length >= 2 {
		intersection := make(Set)
		if result, err := setUnion(args...); err == nil {
			union := result.(Set)
			falseFound := false
			for element, value := range union {
			innerLoop:
				for i := 0; i < length; i++ {
					if _, isElement := args[i].(Set)[element]; !isElement {
						falseFound = true
						break innerLoop
					}
				}
				if falseFound {
					falseFound = false
				} else {
					intersection[element] = value
				}
			}
		} else {
			return nil, err
		}
		return intersection, nil
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

// Returns a boolean true when all sets are disjoint, false otherwise.
func setIsDisjoint(args ...Value) (Value, error) {
	if len(args) >= 2 {
		if result, err := setIntersection(args...); err == nil {
			intersectionSet, _ := result.(Set)
			return Bool(len(intersectionSet) == 0), nil
		} else {
			return nil, err
		}
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

// Returns a new set representing the symmetric difference between self and other.
func setSymmetricDifference(args ...Value) (Value, error) {
	if len(args) >= 2 {
		if union, uerr := setUnion(args...); uerr == nil {
			if intersection, ierr := setIntersection(args...); ierr == nil {
				result, _ := setDifference(union, intersection)
				return result, nil
			} else {
				return nil, ierr
			}
		} else {
			return nil, uerr
		}
	}
	return nil, fmt.Errorf("expected al least %v argument and got %v", 1, len(args))
}

// Returns true if self is subset of other, false otherwise.
func setIsSubset(args ...Value) (Value, error) {
	if len(args) == 2 {
		if other, ok := args[1].(Set); ok {
			set, _ := args[0].(Set)
			switch {
			case len(set) > len(other):
				return False, nil
			default:
				for element := range set {
					if _, isElement := other[element]; !isElement {
						return False, nil
					}
				}
				return True, nil
			}
		}
		return nil, fmt.Errorf("expected a set as argument and got %T", args[1])
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns true if self is superset of other, false otherwise.
func setIsSuperSet(args ...Value) (Value, error) {
	if len(args) == 2 {
		if other, ok := args[1].(Set); ok {
			set, _ := args[0].(Set)
			switch {
			case len(set) >= len(other):
				for element := range other {
					if _, isElement := set[element]; !isElement {
						return False, nil
					}
				}
				return True, nil
			default:
				return False, nil
			}
		}
		return nil, fmt.Errorf("expected a set as argument and got %T", args[1])
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns a random element from self without removing it.
func setRandomElement(args ...Value) (Value, error) {
	if len(args) == 1 {
		set, _ := args[0].(Set)
		length := int64(len(set))
		if length == 0 {
			return NilValue, nil
		}
		rand.Seed(time.Now().UnixNano())
		randomIndex := rand.Int63n(length)
		index := int64(0)
		for _, value := range set {
			if randomIndex == index {
				return value, nil
			}
			index++
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns an iterator object ready to iterate over self.
func setMakeIterator(args ...Value) (Value, error) {
	if len(args) == 1 {
		set, _ := args[0].(Set)
		return NewSetIterator(set, false), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}
