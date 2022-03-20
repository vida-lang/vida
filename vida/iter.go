package vida

import (
	"fmt"
)

// Iterator is the interface for every iterator value.
type Iterator interface {
	Next() Value
	Exhausted() bool
}

func (fiber *Fiber) loadIterator() {
	modName := "Iter"
	module := GModule{Name: modName, Namespace: make(Namespace)}
	module.Namespace["stop"] = IStopValue
	fiber.module.namespace[modName] = module
}

// NumericIterator is the iterator over a range of integers.
type NumericIterator struct {
	Value, End, Step Int
	TowardsRight     Bool
}

// Interface Value
func (iter *NumericIterator) TypeName() string {
	return "Iterator"
}

func (iter *NumericIterator) Description() string {
	return fmt.Sprintf("Iterator(value:%v end:%v step:%v)", iter.Value, iter.End, iter.Step)
}

func (iter *NumericIterator) Equals(other Value) bool {
	if value, ok := other.(*NumericIterator); ok {
		return iter.Value == value.Value && iter.End == value.End && iter.Step == value.Step && iter.TowardsRight == value.TowardsRight
	}
	return false
}

func NewRange(value, end, step Int, towardsRight Bool) *NumericIterator {
	return &NumericIterator{Value: value, End: end, Step: step, TowardsRight: towardsRight}
}

func (iter *NumericIterator) Next() Value {
	if (iter.TowardsRight && iter.Value < iter.End) || (!iter.TowardsRight && iter.Value > iter.End) {
		nextValue := iter.Value
		iter.Value += iter.Step
		return nextValue
	}
	return IStopValue
}

func (iter *NumericIterator) Exhausted() bool {
	if (iter.TowardsRight && iter.Value < iter.End) || (!iter.TowardsRight && iter.Value > iter.End) {
		return false
	}
	return true
}

func (iter *NumericIterator) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *NumericIterator) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *NumericIterator) IsIterable() bool {
	return false
}

func (iter *NumericIterator) MakeIterator() Iterator {
	return nil
}

func (iter *NumericIterator) IsHashable() bool {
	return false
}

func (iter *NumericIterator) MakeHashKey() HashKey {
	return HashKey{}
}

func (iter *NumericIterator) IsValueSemantics() bool {
	return false
}

func (iter *NumericIterator) HasMethods() bool {
	return false
}

func (iter *NumericIterator) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, iter)
}

func (iter *NumericIterator) Clone() Value {
	return NewRange(iter.Value, iter.End, iter.Step, iter.TowardsRight)
}

// ListIterator is an object to iterate over lists and returns an item or a tuple (idx, val).
type ListIterator struct {
	Object          *List
	EndIndex, Index Int
	IsRange         bool
}

// Interface Value
func (iter *ListIterator) TypeName() string {
	return "Iterator"
}

func (iter *ListIterator) Description() string {
	return fmt.Sprintf("Iterator(index:%v endIndex:%v)", iter.Index, iter.EndIndex)
}

func (iter *ListIterator) Equals(other Value) bool {
	if value, ok := other.(*ListIterator); ok {
		return iter.IsRange == value.IsRange && iter.Index == value.Index && iter.EndIndex == value.EndIndex && iter.Object.Equals(value.Object)
	}
	return false
}

func NewListIterator(list *List, isrange bool) *ListIterator {
	return &ListIterator{Object: list, EndIndex: Int(len(list.Elements)), Index: 0, IsRange: isrange}
}

func (iter *ListIterator) Next() Value {
	if iter.IsRange {
		if iter.Index < iter.EndIndex {
			tuple := Tuple{index: iter.Index, value: iter.Object.Elements[iter.Index]}
			iter.Index++
			return tuple
		}
	} else {
		if iter.Index < iter.EndIndex {
			item := iter.Object.Elements[iter.Index]
			iter.Index++
			return item
		}
	}
	return IStopValue
}

func (iter *ListIterator) Exhausted() bool {
	return iter.Index >= iter.EndIndex
}

func (iter *ListIterator) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *ListIterator) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *ListIterator) IsIterable() bool {
	return false
}

func (iter *ListIterator) MakeIterator() Iterator {
	return nil
}

func (iter *ListIterator) IsHashable() bool {
	return false
}

func (iter *ListIterator) MakeHashKey() HashKey {
	return HashKey{}
}

func (iter *ListIterator) IsValueSemantics() bool {
	return false
}

func (iter *ListIterator) HasMethods() bool {
	return false
}

func (iter *ListIterator) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, iter)
}

func (iter *ListIterator) Clone() Value {
	return &ListIterator{
		Object:   iter.Object,
		Index:    iter.Index,
		EndIndex: iter.EndIndex,
		IsRange:  iter.IsRange,
	}
}

// String iterator is an object to iterate over runes in strings.
type StringIterator struct {
	Object              *String
	EndIndex, RuneIndex Int
	IsRange             bool
}

// Interface Value
func (iter *StringIterator) TypeName() string {
	return "Iterator"
}

func (iter *StringIterator) Description() string {
	return fmt.Sprintf("Iterator(index:%v endIndex:%v)", iter.RuneIndex, iter.EndIndex)
}

func (iter *StringIterator) Equals(other Value) bool {
	if value, ok := other.(*StringIterator); ok && iter.IsRange == value.IsRange && iter.RuneIndex == value.RuneIndex && iter.EndIndex == value.EndIndex {
		if len(iter.Object.Runes) != len(value.Object.Runes) {
			return false
		}
		for i := 0; i < len(iter.Object.Runes); i++ {
			if iter.Object.Runes[i] != value.Object.Runes[i] {
				return false
			}
		}
		return true
	}
	return false
}

func NewStringIterator(str *String, isrange bool) *StringIterator {
	if str.Runes == nil {
		str.Runes = []rune(str.Value)
	}
	return &StringIterator{Object: str, EndIndex: Int(len(str.Runes)), RuneIndex: 0, IsRange: isrange}
}

func (iter *StringIterator) Next() Value {
	if iter.IsRange {
		if iter.RuneIndex < iter.EndIndex {
			tuple := Tuple{index: Int(iter.RuneIndex), value: Rune(iter.Object.Runes[iter.RuneIndex])}
			iter.RuneIndex++
			return tuple
		}
	} else {
		if iter.RuneIndex < iter.EndIndex {
			r := Rune(iter.Object.Runes[iter.RuneIndex])
			iter.RuneIndex++
			return r
		}
	}
	return IStopValue
}

func (iter *StringIterator) Exhausted() bool {
	return iter.RuneIndex >= iter.EndIndex
}

func (iter *StringIterator) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *StringIterator) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *StringIterator) IsIterable() bool {
	return false
}

func (iter *StringIterator) MakeIterator() Iterator {
	return nil
}

func (iter *StringIterator) IsHashable() bool {
	return false
}

func (iter *StringIterator) MakeHashKey() HashKey {
	return HashKey{}
}

func (iter *StringIterator) IsValueSemantics() bool {
	return false
}

func (iter *StringIterator) HasMethods() bool {
	return false
}

func (iter *StringIterator) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, iter)
}

func (iter *StringIterator) Clone() Value {
	return &StringIterator{
		Object:    iter.Object,
		RuneIndex: iter.RuneIndex,
		EndIndex:  iter.EndIndex,
		IsRange:   iter.IsRange,
	}

}

// MapIterator is an object to iterate over maps.
type MapIterator struct {
	Object          Map
	Keys            []HashKey
	Index, EndIndex Int
	IsRange         bool
}

// Interface Value
func (iter *MapIterator) TypeName() string {
	return "Iterator"
}

func (iter *MapIterator) Description() string {
	return fmt.Sprintf("Iterator(index:%v endIndex:%v)", iter.Index, iter.EndIndex)
}

func (iter *MapIterator) Equals(other Value) bool {
	if value, ok := other.(*MapIterator); ok && iter.IsRange == value.IsRange && iter.Index == value.Index && iter.EndIndex == value.EndIndex {
		if len(iter.Keys) != len(value.Keys) {
			return false
		}
		if !(iter.Object).Equals(value.Object) {
			return false
		}
		return true
	}
	return false
}

func NewMapIterator(mmap Map, isrange bool) *MapIterator {
	keys := make([]HashKey, 0, len(mmap))
	for k := range mmap {
		keys = append(keys, k)
	}
	return &MapIterator{Object: mmap, Keys: keys, Index: 0, EndIndex: Int(len(keys)), IsRange: isrange}
}

func (iter *MapIterator) Next() Value {
	if iter.IsRange {
		if iter.Index < iter.EndIndex {
			pair := Pair{}
			key := iter.Keys[iter.Index]
			pair.key, pair.value = iter.Object[key].key, iter.Object[key].value
			iter.Index++
			return pair
		}
	} else {
		if iter.Index < iter.EndIndex {
			key := iter.Keys[iter.Index]
			iter.Index++
			return iter.Object[key].key
		}
	}
	return IStopValue
}

func (iter *MapIterator) Exhausted() bool {
	return iter.Index >= iter.EndIndex
}

func (iter *MapIterator) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *MapIterator) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *MapIterator) IsIterable() bool {
	return false
}

func (iter *MapIterator) MakeIterator() Iterator {
	return nil
}

func (iter *MapIterator) IsHashable() bool {
	return false
}

func (iter *MapIterator) MakeHashKey() HashKey {
	return HashKey{}
}

func (iter *MapIterator) IsValueSemantics() bool {
	return false
}

func (iter *MapIterator) HasMethods() bool {
	return false
}

func (iter *MapIterator) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, iter)
}

func (iter *MapIterator) Clone() Value {
	return &MapIterator{
		Object:   iter.Object,
		Keys:     iter.Keys,
		Index:    iter.Index,
		EndIndex: iter.EndIndex,
		IsRange:  iter.IsRange,
	}

}

// SetIterator is an objecto to iterate over sets.
type SetIterator struct {
	Object          Set
	Keys            []HashKey
	Index, EndIndex Int
	IsRange         bool
}

// Interface Value
func (iter *SetIterator) TypeName() string {
	return "Iterator"
}

func (iter *SetIterator) Description() string {
	return fmt.Sprintf("Iterator(index:%v endIndex:%v)", iter.Index, iter.EndIndex)
}

func (iter *SetIterator) Equals(other Value) bool {
	if value, ok := other.(*SetIterator); ok && iter.IsRange == value.IsRange && iter.Index == value.Index && iter.EndIndex == value.EndIndex {
		if len(iter.Keys) != len(value.Keys) {
			return false
		}
		for i := 0; i < len(iter.Keys); i++ {
			if !iter.Keys[i].Equals(value.Keys[i]) {
				return false
			}
		}
		return true
	}
	return false
}

func NewSetIterator(set Set, isrange bool) *SetIterator {
	keys := make([]HashKey, 0, len(set))
	for k := range set {
		keys = append(keys, k)
	}
	return &SetIterator{Object: set, Keys: keys, Index: 0, EndIndex: Int(len(keys)), IsRange: isrange}
}

func (iter *SetIterator) Next() Value {
	if iter.IsRange {
		if iter.Index < iter.EndIndex {
			pair := Pair{}
			key := iter.Keys[iter.Index]
			pair.key, pair.value = iter.Object[key], NilValue
			iter.Index++
			return pair
		}
	} else {
		if iter.Index < iter.EndIndex {
			iter.Index++
			return iter.Object[iter.Keys[iter.Index-1]]
		}
	}
	return IStopValue
}

func (iter *SetIterator) Exhausted() bool {
	return iter.Index >= iter.EndIndex
}

func (iter *SetIterator) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *SetIterator) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *SetIterator) IsIterable() bool {
	return false
}

func (iter *SetIterator) MakeIterator() Iterator {
	return nil
}

func (iter *SetIterator) IsHashable() bool {
	return false
}

func (iter *SetIterator) MakeHashKey() HashKey {
	return HashKey{}
}

func (iter *SetIterator) IsValueSemantics() bool {
	return false
}

func (iter *SetIterator) HasMethods() bool {
	return false
}

func (iter *SetIterator) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, iter)
}

func (iter *SetIterator) Clone() Value {
	return &SetIterator{
		Object:   iter.Object,
		Keys:     iter.Keys,
		Index:    iter.Index,
		EndIndex: iter.EndIndex,
		IsRange:  iter.IsRange,
	}
}

type RecordIterator struct {
	Object          *Record
	Keys            []string
	Index, EndIndex Int
	IsRange         bool
}

// Interface Value
func (iter *RecordIterator) TypeName() string {
	return "Iterator"
}

func (iter *RecordIterator) Description() string {
	return fmt.Sprintf("Iterator(index:%v endIndex:%v)", iter.Index, iter.EndIndex)
}

func (iter *RecordIterator) Equals(other Value) bool {
	if value, ok := other.(*RecordIterator); ok && iter.IsRange == value.IsRange && iter.Index == value.Index && iter.EndIndex == value.EndIndex {
		if len(iter.Keys) != len(value.Keys) {
			return false
		}
		if !(*iter.Object).Equals(*value.Object) {
			return false
		}
		return true
	}
	return false
}

func NewRecordIterator(record Record, isrange bool) *RecordIterator {
	keys := make([]string, 0, len(record.Properties))
	for k := range record.Properties {
		keys = append(keys, k)
	}
	return &RecordIterator{Object: &record, Keys: keys, Index: 0, EndIndex: Int(len(keys)), IsRange: isrange}
}

func (iter *RecordIterator) Next() Value {
	if iter.IsRange {
		if iter.Index < iter.EndIndex {
			pair := Pair{}
			key := iter.Keys[iter.Index]
			pair.key, pair.value = &String{Value: key}, iter.Object.Properties[key]
			iter.Index++
			return pair
		}
	} else {
		if iter.Index < iter.EndIndex {
			key := iter.Keys[iter.Index]
			iter.Index++
			return &String{Value: key}
		}
	}
	return IStopValue
}

func (iter *RecordIterator) Exhausted() bool {
	return iter.Index >= iter.EndIndex
}

func (iter *RecordIterator) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *RecordIterator) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *RecordIterator) IsIterable() bool {
	return false
}

func (iter *RecordIterator) MakeIterator() Iterator {
	return nil
}

func (iter *RecordIterator) IsHashable() bool {
	return false
}

func (iter *RecordIterator) MakeHashKey() HashKey {
	return HashKey{}
}

func (iter *RecordIterator) IsValueSemantics() bool {
	return false
}

func (iter *RecordIterator) HasMethods() bool {
	return false
}

func (iter *RecordIterator) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, iter)
}

func (iter *RecordIterator) Clone() Value {
	return &RecordIterator{
		Object:   iter.Object,
		Keys:     iter.Keys,
		Index:    iter.Index,
		EndIndex: iter.EndIndex,
		IsRange:  iter.IsRange,
	}
}

// BytesIterator is an object to iterate over bytes buffers and returns an item or a tuple (idx, val).
type BytesIterator struct {
	Object          *Bytes
	EndIndex, Index Int
	IsRange         bool
}

// Interface Value
func (iter *BytesIterator) TypeName() string {
	return "Iterator"
}

func (iter *BytesIterator) Description() string {
	return fmt.Sprintf("Iterator(index:%v endIndex:%v)", iter.Index, iter.EndIndex)
}

func (iter *BytesIterator) Equals(other Value) bool {
	if value, ok := other.(*BytesIterator); ok {
		return iter.IsRange == value.IsRange && iter.Index == value.Index && iter.EndIndex == value.EndIndex && iter.Object.Equals(value.Object)
	}
	return false
}

func NewBytesIterator(bytes *Bytes, isrange bool) *BytesIterator {
	return &BytesIterator{Object: bytes, EndIndex: Int(len(bytes.Value)), Index: 0, IsRange: isrange}
}

func (iter *BytesIterator) Next() Value {
	if iter.IsRange {
		if iter.Index < iter.EndIndex {
			tuple := Tuple{index: iter.Index, value: Byte(iter.Object.Value[iter.Index])}
			iter.Index++
			return tuple
		}
	} else {
		if iter.Index < iter.EndIndex {
			item := Byte(iter.Object.Value[iter.Index])
			iter.Index++
			return item
		}
	}
	return IStopValue
}

func (iter *BytesIterator) Exhausted() bool {
	return iter.Index >= iter.EndIndex
}

func (iter *BytesIterator) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *BytesIterator) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, iter)
}

func (iter *BytesIterator) IsIterable() bool {
	return false
}

func (iter *BytesIterator) MakeIterator() Iterator {
	return nil
}

func (iter *BytesIterator) IsHashable() bool {
	return false
}

func (iter *BytesIterator) MakeHashKey() HashKey {
	return HashKey{}
}

func (iter *BytesIterator) IsValueSemantics() bool {
	return false
}

func (iter *BytesIterator) HasMethods() bool {
	return false
}

func (iter *BytesIterator) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, iter)
}

func (iter *BytesIterator) Clone() Value {
	return &BytesIterator{
		Object:   iter.Object,
		Index:    iter.Index,
		EndIndex: iter.EndIndex,
		IsRange:  iter.IsRange,
	}
}
