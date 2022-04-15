package vida

import (
	"fmt"
	"math/rand"
	"os"
	"sort"
	"strings"
	"time"
	"unsafe"
)

// List is an indexed-ordered random-access linear collection of values.
type List struct {
	Elements []Value
}

// Interface Value
func (list *List) TypeName() string {
	return "List"
}

func (list *List) Description() string {
	var builder strings.Builder
	builder.WriteString("[ ")
	for _, elem := range list.Elements {
		builder.WriteString(elem.Description())
		builder.WriteRune(' ')
	}
	builder.WriteString("]")
	return builder.String()
}

func (list *List) Equals(other Value) bool {
	if value, ok := other.(*List); ok {
		if unsafe.Pointer(list) == unsafe.Pointer(value) {
			return true
		}
		if len(list.Elements) != len(value.Elements) {
			return false
		}
		for i := 0; i < len(value.Elements); i++ {
			if !list.Elements[i].Equals(value.Elements[i]) {
				return false
			}
		}
		return true
	}
	return false
}

func (list *List) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case *List:
		switch op {
		case TKAdd:
			elements := make([]Value, 0, len(list.Elements)+len(rhs.Elements))
			elements = append(elements, list.Elements...)
			elements = append(elements, rhs.Elements...)
			return &List{Elements: elements}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], list, rhs)
		}
	case IntegerNumber:
		switch op {
		case TKMul:
			intVal := rhs.ToInt()
			if intVal < 0 {
				intVal = -intVal
			}
			if intVal > maxListSize {
				return nil, fmt.Errorf("max size for lists is %v", maxListSize)
			}
			xs := make([]Value, 0, Int(len(list.Elements))*intVal)
			for i := Int(0); i < intVal; i++ {
				xs = append(xs, list.Elements...)
			}
			return &List{Elements: xs}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], list, rhs.ToInt())
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], list, rhs)
	}
}

func (list *List) PrefixOp(op byte) (Value, error) {
	return nil, TypeErrorInPrefixOperator(KindDescription[op], list)
}

func (list *List) IsIterable() bool {
	return true
}

func (list *List) MakeIterator() Iterator {
	return NewListIterator(list, false)
}

func (list *List) IsHashable() bool {
	return false
}

func (list *List) MakeHashKey() HashKey {
	return HashKey{}
}

func (list *List) IsValueSemantics() bool {
	return false
}

func (list *List) HasMethods() bool {
	return true
}

func (list *List) GetMethod(name string) (Value, bool, error) {
	if method, ok := ListInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, MethodNotDefined(name, list)
}

func (list *List) Clone() Value {
	newList := &List{Elements: make([]Value, len(list.Elements))}
	for i, v := range list.Elements {
		newList.Elements[i] = v.Clone()
	}
	return newList
}

// Interface subscript operator.
func (list *List) SubscriptGet(index Value) (Value, error) {
	switch idx := index.(type) {
	case IntegerNumber:
		i, length := idx.ToInt(), Int(len(list.Elements))
		switch {
		case i >= -length && i < length:
			switch {
			case i < 0:
				return list.Elements[i+length], nil
			default:
				return list.Elements[i], nil
			}
		default:
			return nil, IndexOutOfRangeError(length, i)
		}
	default:
		return nil, ValueIsNotAnIndexError(index)
	}
}

func (list *List) SubscriptSet(index, value Value) error {
	switch idx := index.(type) {
	case IntegerNumber:
		i, length := idx.ToInt(), Int(len(list.Elements))
		switch {
		case i >= -length && i < length:
			switch {
			case i < 0:
				list.Elements[i+length] = value
			default:
				list.Elements[i] = value
			}
			return nil
		default:
			return IndexOutOfRangeError(length, i)
		}
	default:
		return ValueIsNotAnIndexError(index)
	}
}

// Interface Slice operator.
func (list *List) SliceGet(sliceType UInt32, low, high Value) (Value, error) {
	lidx, okL := low.(IntegerNumber)
	hidx, okH := high.(IntegerNumber)
	if !okL {
		return nil, ValueIsNotAnIndexError(low)
	}
	if !okH {
		return nil, ValueIsNotAnIndexError(high)
	}
	var l, h Int
	length := Int(len(list.Elements))
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
		return nil, NeverShouldHaveHappened("wrong sliceType in List SliceGet")
	}
	if l >= -length && l <= length && h >= -length && h <= length {
		if l < 0 {
			l += length
		}
		if h < 0 {
			h += length
		}
		if l <= h {
			return &List{Elements: list.Elements[l:h]}, nil
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

func (list *List) SliceSet(sliceType UInt32, low, high, value Value) error {
	lidx, okL := low.(IntegerNumber)
	hidx, okH := high.(IntegerNumber)
	if !okL {
		return ValueIsNotAnIndexError(low)
	}
	if !okH {
		return ValueIsNotAnIndexError(high)
	}
	var l, h Int
	length := Int(len(list.Elements))
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
		return NeverShouldHaveHappened("wrong sliceType in List SliceSet")
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
				list.Elements[i] = value
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

// ListSorter is a type implementing the interface Sort.
type listSorter struct {
	xs *List
	vm *VM
}

func (ls listSorter) Len() int {
	return len(ls.xs.Elements)
}

func (ls listSorter) Swap(i, j int) {
	ls.xs.Elements[i], ls.xs.Elements[j] = ls.xs.Elements[j], ls.xs.Elements[i]
}

func (ls listSorter) Less(i, j int) bool {
	ls.vm.stack[ls.vm.top] = ls.xs.Elements[i]
	ls.vm.top++
	ls.vm.stack[ls.vm.top] = ls.xs.Elements[j]
	ls.vm.top++
	if err := ls.vm.runInterpreter(ls.vm.frame.closure.Function.Name); err == nil {
		if result, isBool := ls.vm.stack[ls.vm.top-1].(Bool); isBool {
			ls.vm.frameIndex = 0
			ls.vm.frame = &ls.vm.stackFrame[ls.vm.frameIndex]
			ls.vm.top, ls.vm.frame.pc, ls.vm.frame.fp = 0, 0, 0
			return bool(result)
		} else {
			PrintError(fmt.Errorf("expected a return value of type 'Bool' in sorting function"))
			ls.vm.printStack(ls.vm.frameIndex)
			os.Exit(0)
		}
	} else {
		PrintError(err)
		ls.vm.printStack(ls.vm.frameIndex)
		os.Exit(0)
	}
	return false
}

func (fiber *Fiber) loadList() {
	modName := "List"
	module := GModule{Name: modName, Namespace: make(Namespace)}
	module.Namespace["iota"] = GFunction{Name: "iota", Value: xsIota}
	module.Namespace["repeating"] = GFunction{Name: "repeating", Value: xsRepeating}
	fiber.module.namespace[modName] = module
}

// ListInterface is the collection of methods for the type List.
var ListInterface = Namespace{
	"isEmpty":       GFunction{Name: "isEmpty", Value: xsIsEmpty},
	"length":        GFunction{Name: "length", Value: xsLength},
	"clear":         GFunction{Name: "clear", Value: xsClear},
	"contains":      GFunction{Name: "contains", Value: xsContains},
	"clone":         GFunction{Name: "clone", Value: xsClone},
	"remove":        GFunction{Name: "remove", Value: xsRemove},
	"randomElement": GFunction{Name: "randomElement", Value: xsRandomElement},
	"makeIterator":  GFunction{Name: "makeIterator", Value: xsMakeIterator},
	"append":        GFunction{Name: "append", Value: xsAppend},
	"insert":        GFunction{Name: "insert", Value: xsInsert},
	"extend":        GFunction{Name: "extend", Value: xsExtend},
	"popLast":       GFunction{Name: "popLast", Value: xsPopLast},
	"popFirst":      GFunction{Name: "popLast", Value: xsPopFirst},
	"sort":          GFunction{Name: "sort", Value: xsSort},
	"sorted":        GFunction{Name: "sorted", Value: xsSorted},
	"reverse":       GFunction{Name: "reverse", Value: xsReverse},
	"reversed":      GFunction{Name: "reversed", Value: xsReversed},
	"shuffle":       GFunction{Name: "shuffle", Value: xsShuffle},
	"shuffled":      GFunction{Name: "shuffled", Value: xsShuffled},
	"removed":       GFunction{Name: "removed", Value: xsRemoved},
	"capacity":      GFunction{Name: "capacity", Value: xsCap},
	"indexOf":       GFunction{Name: "indexOf", Value: xsIndex},
	"countOf":       GFunction{Name: "countOf", Value: xsCount},
	"midIndex":      GFunction{Name: "midIndex", Value: xsMidIndex},
}

// Checks if a collection is empty.
func xsIsEmpty(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Bool(len(args[0].(*List).Elements) == 0), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Appends some value to self
func xsAppend(args ...Value) (Value, error) {
	if len(args) == 2 {
		list := args[0].(*List)
		list.Elements = append(list.Elements, args[1])
		return list, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Inserts some value at the given position
func xsInsert(args ...Value) (Value, error) {
	if len(args) == 3 {
		list := args[0].(*List)
		length := Int(len(list.Elements))
		if index, ok := args[1].(Int); ok {
			if length == 0 && index == 0 {
				list.Elements = append(list.Elements, args[2])
				return list, nil
			} else if index >= -length && index < length {
				if index < 0 {
					index += length
				}
				list.Elements = append(list.Elements, nil)
				copy(list.Elements[index+1:], list.Elements[index:])
				list.Elements[index] = args[2]
				return list, nil
			}
			return nil, fmt.Errorf("insertion index out of range")
		}
		return nil, fmt.Errorf("expected a integer as insertion index")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

// Extends self with the elements of other list
func xsExtend(args ...Value) (Value, error) {
	if len(args) == 2 {
		list := args[0].(*List)
		if other, ok := args[1].(*List); ok {
			list.Elements = append(list.Elements, other.Elements...)
			return list, nil
		}
		return nil, fmt.Errorf("expected a list as second argument")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Pops the last element of self if self is not empty.
func xsPopLast(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		length := len(list.Elements)
		if length == 0 {
			return nil, fmt.Errorf("cannot pop an element from an empty list")
		}
		var value Value
		length--
		value, list.Elements = list.Elements[length], list.Elements[:length]
		return value, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Pops the first element of self if self is not empty.
func xsPopFirst(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		length := len(list.Elements)
		if length == 0 {
			return nil, fmt.Errorf("cannot pop an element from an empty list")
		}
		var value Value
		length--
		value, list.Elements = list.Elements[0], list.Elements[1:]
		return value, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Sets the length of self to zero
func xsClear(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		list.Elements = list.Elements[:0]
		return list, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Sorts the elements of self. It is especially useful when all elements have some built-in data types
func xsSort(args ...Value) (Value, error) {
	list := args[0].(*List)
	var err error
	if len(list.Elements) == 0 {
		return list, nil
	}
	switch len(args) {
	case 1:
		err = lessThan(list)
	case 2:
		switch fnOrPattern := args[1].(type) {
		case Closure:
			if !fnOrPattern.Function.Vararg && fnOrPattern.Function.Arity == 2 {
				fiber := fiberPool.Get().(*Fiber)
				fiber.reset(fnOrPattern)
				sorter := listSorter{xs: list}
				fiber.parentFiber = globalState.currentFiber
				fiber.state = fiberRunning
				fiber.parentFiber.state = fiberWaiting
				globalState.currentFiber = fiber
				globalState.vm.Fiber = fiber
				sorter.vm = globalState.vm
				sort.Sort(sorter)
				globalState.currentFiber = globalState.currentFiber.parentFiber
				globalState.vm.Fiber = globalState.currentFiber
				globalState.currentFiber.state = fiberRunning
				fiberPool.Put(fiber)
			} else {
				err = fmt.Errorf("invalid sorting function")
			}
		case *String:
			switch fnOrPattern.Value {
			case "<":
				err = lessThan(list)
			case ">":
				err = greaterThan(list)
			default:
				err = fmt.Errorf("unknown pattern in sorting method")
			}
		default:
			err = fmt.Errorf("expected a function or a string pattern as second argument in sort method")
		}
	default:
		err = fmt.Errorf("expected %v or %v arguments in sort method and got %v", 0, 1, len(args))
	}
	if err == nil {
		return list, nil
	}
	return nil, err
}

// Returns a new sorted list from self
func xsSorted(args ...Value) (Value, error) {
	list := args[0].(*List)
	newList := &List{}
	var err error
	if len(list.Elements) == 0 {
		newList.Elements = make([]Value, 0)
		return newList, nil
	}
	switch len(args) {
	case 1:
		newList.Elements = make([]Value, len(list.Elements))
		copy(newList.Elements, list.Elements)
		err = lessThan(newList)
	case 2:
		switch fnOrPattern := args[1].(type) {
		case Closure:
			newList.Elements = make([]Value, len(list.Elements))
			copy(newList.Elements, list.Elements)
			if !fnOrPattern.Function.Vararg && fnOrPattern.Function.Arity == 2 {
				fiber := fiberPool.Get().(*Fiber)
				fiber.reset(fnOrPattern)
				sorter := listSorter{xs: newList}
				fiber.parentFiber = globalState.currentFiber
				fiber.state = fiberRunning
				fiber.parentFiber.state = fiberWaiting
				globalState.currentFiber = fiber
				globalState.vm.Fiber = fiber
				sorter.vm = globalState.vm
				sort.Sort(sorter)
				globalState.currentFiber = globalState.currentFiber.parentFiber
				globalState.vm.Fiber = globalState.currentFiber
				globalState.currentFiber.state = fiberRunning
				fiberPool.Put(fiber)
			} else {
				err = fmt.Errorf("invalid sorting function")
			}
		case *String:
			switch fnOrPattern.Value {
			case "<":
				newList.Elements = make([]Value, len(list.Elements))
				copy(newList.Elements, list.Elements)
				err = lessThan(newList)
			case ">":
				newList.Elements = make([]Value, len(list.Elements))
				copy(newList.Elements, list.Elements)
				err = greaterThan(newList)
			default:
				err = fmt.Errorf("unknown pattern in sorting method")
			}
		default:
			err = fmt.Errorf("expected a function or a string pattern as second argument in sort method")
		}
	default:
		err = fmt.Errorf("expected %v or %v arguments in sort method and got %v", 0, 1, len(args))
	}
	if err == nil {
		return newList, nil
	}
	return nil, err
}

// Reverses in place the elements of self.
func xsReverse(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		length := len(list.Elements)
		if length == 0 {
			return list, nil
		}
		for left, right := 0, length-1; left < right; left, right = left+1, right-1 {
			list.Elements[left], list.Elements[right] = list.Elements[right], list.Elements[left]
		}
		return list, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a new list with the reversed elements of self.
func xsReversed(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		length := len(list.Elements)
		newList := &List{}
		if length == 0 {
			newList.Elements = make([]Value, 0)
			return newList, nil
		}
		newList.Elements = make([]Value, length)
		copy(newList.Elements, list.Elements)
		for left, right := 0, length-1; left < right; left, right = left+1, right-1 {
			newList.Elements[left], newList.Elements[right] = newList.Elements[right], newList.Elements[left]
		}
		return newList, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Shuffles in place the elements of self.
func xsShuffle(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		length := len(list.Elements)
		if length == 0 {
			return list, nil
		}
		rand.Seed(time.Now().UnixNano())
		rand.Shuffle(length, func(i, j int) {
			list.Elements[i], list.Elements[j] = list.Elements[j], list.Elements[i]
		})
		return list, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Retorns a new list shluffling the elements of self.
func xsShuffled(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		length := len(list.Elements)
		newList := &List{}
		if length == 0 {
			newList.Elements = make([]Value, 0)
			return newList, nil
		}
		newList.Elements = make([]Value, len(list.Elements))
		copy(newList.Elements, list.Elements)
		rand.Seed(time.Now().UnixNano())
		rand.Shuffle(length, func(i, j int) {
			newList.Elements[i], newList.Elements[j] = newList.Elements[j], newList.Elements[i]
		})
		return newList, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Removes an item from the elements of self if it is in the list.
func xsRemove(args ...Value) (Value, error) {
	if len(args) == 2 {
		list := args[0].(*List)
		length, elem, index, found := len(list.Elements), args[1], 0, false
		for i, v := range list.Elements {
			if v.Equals(elem) {
				found, index = true, i
				break
			}
		}
		if found {
			copy(list.Elements[index:], list.Elements[index+1:])
			list.Elements[length-1] = NilValue
			list.Elements = list.Elements[:length-1]
		}
		return list, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns a new list with an element removed from the original list.
func xsRemoved(args ...Value) (Value, error) {
	if len(args) == 2 {
		list := args[0].(*List)
		newList := &List{}
		length, elem, index, found := len(list.Elements), args[1], 0, false
		for i, v := range list.Elements {
			if v.Equals(elem) {
				found, index = true, i
				break
			}
		}
		newList.Elements = make([]Value, length)
		copy(newList.Elements, list.Elements)
		if found {
			copy(newList.Elements[index:], newList.Elements[index+1:])
			newList.Elements[length-1] = NilValue
			newList.Elements = newList.Elements[:length-1]
		}
		return newList, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns the current self capacity.
func xsCap(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		return Int(cap(list.Elements)), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns the current self length.
func xsLength(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		return Int(len(list.Elements)), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns the index of the given element.
func xsIndex(args ...Value) (Value, error) {
	if len(args) == 2 {
		list := args[0].(*List)
		elem, index, found := args[1], 0, false
		for i, v := range list.Elements {
			if v.Equals(elem) {
				found, index = true, i
				break
			}
		}
		if found {
			return Int(index), nil
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns the number of occurrences of a given value.
func xsCount(args ...Value) (Value, error) {
	if len(args) == 2 {
		list := args[0].(*List)
		elem, count := args[1], Int(0)
		for _, v := range list.Elements {
			if v.Equals(elem) {
				count++
			}
		}
		return count, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns the if the given argument exists in the list.
func xsContains(args ...Value) (Value, error) {
	if len(args) == 2 {
		list := args[0].(*List)
		for _, v := range list.Elements {
			if v.Equals(args[1]) {
				return True, nil
			}
		}
		return False, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns a shallow copy of the elements of self.
func xsClone(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		return list.Clone(), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns a random element from self without removing it.
func xsRandomElement(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		if len(list.Elements) == 0 {
			return NilValue, nil
		}
		rand.Seed(time.Now().UnixNano())
		return list.Elements[rand.Int63n(int64(len(list.Elements)))], nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Returns an iterator object ready to iterate over self.
func xsMakeIterator(args ...Value) (Value, error) {
	if len(args) == 1 {
		list := args[0].(*List)
		return NewListIterator(list, false), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Checks if a collection is empty.
func xsMidIndex(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Int(len(args[0].(*List).Elements) / 2), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func lessThan(list *List) (err error) {
	switch v := list.Elements[0].(type) {
	case Int:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Int); ok {
				if rhs, ok := list.Elements[j].(Int); ok {
					return lhs < rhs
				}
			}
			return false
		})
	case UInt:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(UInt); ok {
				if rhs, ok := list.Elements[j].(UInt); ok {
					return lhs < rhs
				}
			}
			return false
		})
	case Float:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Float); ok {
				if rhs, ok := list.Elements[j].(Float); ok {
					return lhs < rhs
				}
			}
			return false
		})
	case *String:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*String); ok {
				if rhs, ok := list.Elements[j].(*String); ok {
					return lhs.Value < rhs.Value
				}
			}
			return false
		})
	case *BInt:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*BInt); ok {
				if rhs, ok := list.Elements[j].(*BInt); ok {
					return lhs.Value.Cmp(rhs.Value) == -1
				}
			}
			return false
		})
	case *Rational:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*Rational); ok {
				if rhs, ok := list.Elements[j].(*Rational); ok {
					return lhs.Value.Cmp(rhs.Value) == -1
				}
			}
			return false
		})
	case *List:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*List); ok {
				if rhs, ok := list.Elements[j].(*List); ok {
					return len(lhs.Elements) < len(rhs.Elements)
				}
			}
			return false
		})
	case Map:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Map); ok {
				if rhs, ok := list.Elements[j].(Map); ok {
					return len(lhs) < len(rhs)
				}
			}
			return false
		})
	case Set:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Set); ok {
				if rhs, ok := list.Elements[j].(Set); ok {
					return len(lhs) < len(rhs)
				}
			}
			return false
		})
	case Bool:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Bool); ok {
				if rhs, ok := list.Elements[j].(Bool); ok {
					if !lhs && rhs {
						return true
					}
				}
			}
			return false
		})
	default:
		err = fmt.Errorf("expected orderable data type in sort method and got %v", v.TypeName())
	}
	return err
}

func greaterThan(list *List) (err error) {
	switch v := list.Elements[0].(type) {
	case Int:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Int); ok {
				if rhs, ok := list.Elements[j].(Int); ok {
					return lhs > rhs
				}
			}
			return false
		})
	case UInt:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(UInt); ok {
				if rhs, ok := list.Elements[j].(UInt); ok {
					return lhs > rhs
				}
			}
			return false
		})
	case Float:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Float); ok {
				if rhs, ok := list.Elements[j].(Float); ok {
					return lhs > rhs
				}
			}
			return false
		})
	case *String:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*String); ok {
				if rhs, ok := list.Elements[j].(*String); ok {
					return lhs.Value > rhs.Value
				}
			}
			return false
		})
	case *BInt:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*BInt); ok {
				if rhs, ok := list.Elements[j].(*BInt); ok {
					return lhs.Value.Cmp(rhs.Value) == 1
				}
			}
			return false
		})
	case *Rational:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*Rational); ok {
				if rhs, ok := list.Elements[j].(*Rational); ok {
					return lhs.Value.Cmp(rhs.Value) == 1
				}
			}
			return false
		})
	case *List:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(*List); ok {
				if rhs, ok := list.Elements[j].(*List); ok {
					return len(lhs.Elements) > len(rhs.Elements)
				}
			}
			return false
		})
	case Map:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Map); ok {
				if rhs, ok := list.Elements[j].(Map); ok {
					return len(lhs) > len(rhs)
				}
			}
			return false
		})
	case Set:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Set); ok {
				if rhs, ok := list.Elements[j].(Set); ok {
					return len(lhs) > len(rhs)
				}
			}
			return false
		})
	case Bool:
		sort.Slice(list.Elements, func(i, j int) bool {
			if lhs, ok := list.Elements[i].(Bool); ok {
				if rhs, ok := list.Elements[j].(Bool); ok {
					if lhs && !rhs {
						return true
					}
				}
			}
			return false
		})
	default:
		err = fmt.Errorf("expected orderable data type as argument in sort method and got %v", v.TypeName())
	}
	return err
}

// Returns a List from 1 to the given argument.
func xsIota(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch s := args[0].(type) {
		case IntegerNumber:
			size := s.ToInt()
			if size < 0 {
				xs := make([]Value, -size)
				for i, counter := Int(0), Int(size); i < size; i++ {
					xs[i] = counter
					counter++
				}
				return &List{Elements: xs}, nil
			} else {
				xs := make([]Value, size)
				for i, counter := Int(0), Int(1); i < size; i++ {
					xs[i] = counter
					counter++
				}
				return &List{Elements: xs}, nil
			}
		case Iterator:
			iter := args[0].(Iterator)
			xs := make([]Value, 0, minSliceSize)
		iterLoop:
			for {
				value := iter.Next()
				if _, ok := value.(IStop); ok {
					break iterLoop
				}
				xs = append(xs, value)
			}
			return &List{Elements: xs}, nil
		case Instance:
			if method, exists := s.Struct.Methods[iteratorNextMethod].(Closure); exists {
				if method.Function.Arity == 1 {
					xs := make([]Value, 0, minSliceSize)
				instanceLoop:
					for {
						fiber := fiberPool.Get().(*Fiber)
						fiber.reset(method)
						fiber.parentFiber = globalState.currentFiber
						fiber.state = fiberRunning
						fiber.parentFiber.state = fiberWaiting
						globalState.currentFiber = fiber
						globalState.vm.Fiber = fiber
						globalState.vm.stack[globalState.vm.top] = s
						globalState.vm.top++
						if err := globalState.vm.runInterpreter(method.Function.Name); err != nil {
							fiberPool.Put(fiber)
							return NilValue, err
						}
						nextValue := globalState.vm.stack[globalState.vm.top-1]
						globalState.currentFiber = globalState.currentFiber.parentFiber
						globalState.vm.Fiber = globalState.currentFiber
						globalState.currentFiber.state = fiberRunning
						fiberPool.Put(fiber)
						switch nextValue.(type) {
						case IStop:
							break instanceLoop
						default:
							xs = append(xs, nextValue)
						}
					}
					return &List{Elements: xs}, nil
				} else {
					return NilValue, OverloadedOperatorWithWrongArity(iteratorNextMethod, 1, method.Function.Arity)
				}
			} else {
				return NilValue, MethodNotOverloaded(iteratorNextMethod, s.Struct.Name)
			}
		case Closure:
			if s.Function.Arity == 0 {
				xs := make([]Value, 0, minSliceSize)
			generatorLoop:
				for {
					fiber := fiberPool.Get().(*Fiber)
					fiber.reset(s)
					fiber.parentFiber = globalState.currentFiber
					fiber.state = fiberRunning
					fiber.parentFiber.state = fiberWaiting
					globalState.currentFiber = fiber
					globalState.vm.Fiber = fiber
					globalState.vm.stack[globalState.vm.top] = s
					globalState.vm.top++
					if err := globalState.vm.runInterpreter(s.Function.Name); err != nil {
						fiberPool.Put(fiber)
						return NilValue, err
					}
					nextValue := globalState.vm.stack[globalState.vm.top-1]
					globalState.currentFiber = globalState.currentFiber.parentFiber
					globalState.vm.Fiber = globalState.currentFiber
					globalState.currentFiber.state = fiberRunning
					fiberPool.Put(fiber)
					switch nextValue.(type) {
					case IStop:
						break generatorLoop
					default:
						xs = append(xs, nextValue)
					}
				}
				return &List{Elements: xs}, nil
			} else {
				return NilValue, ArityGeneratorError(s.Function.Name)
			}
		default:
			return nil, fmt.Errorf("expected Integer, Iterator or Generator in function iota")
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Creates a new list with element at args[0] and count at args[1]
func xsRepeating(args ...Value) (Value, error) {
	if len(args) == 2 {
		if number, ok := args[1].(IntegerNumber); ok {
			count := number.ToInt()
			if count < 0 {
				count = -count
			}
			if count > maxListSize {
				return nil, fmt.Errorf("max size for lists is %v", maxListSize)
			}
			newList := &List{Elements: make([]Value, count)}
			for i := range newList.Elements {
				newList.Elements[i] = args[0]
			}
			return newList, nil
		}
		return nil, fmt.Errorf("second argument must be an integer number")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}
