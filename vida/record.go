package vida

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
	"unsafe"
)

// Record is a non-ordered structured {property:value} data type.
type Record struct {
	Properties Namespace
}

// Interface SelectorOperator.
func (record Record) SelectorGet(field string) (Value, error) {
	if value, ok := record.Properties[field]; ok {
		return value, nil
	}
	return nil, NameNotDefinedInCompoundDataType(field, record)
}

func (record Record) SelectorSet(property string, value Value) error {
	record.Properties[property] = value
	return nil
}

// Interface Value
func (record Record) TypeName() string {
	return "Record"
}

func (record Record) Description() string {
	var builder strings.Builder
	builder.WriteString("{ ")
	for key, value := range record.Properties {
		builder.WriteString(fmt.Sprintf("%v:%v ", key, value.Description()))
	}
	builder.WriteString("}")
	return builder.String()
}

func (record Record) Equals(other Value) bool {
	if value, ok := other.(Record); ok {
		if unsafe.Pointer(&record) == unsafe.Pointer(&value) {
			return true
		}
		if len(record.Properties) != len(value.Properties) {
			return false
		}
		for k, v := range record.Properties {
			if val, ok := value.Properties[k]; !(ok && v.Equals(val)) {
				return false
			}
		}
		return true
	}
	return false
}

func (record Record) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case Record:
		switch op {
		case TKAdd:
			record := Record{Properties: make(Namespace)}
			for key, value := range rhs.Properties {
				record.Properties[key] = value
			}
			for key, value := range record.Properties {
				record.Properties[key] = value
			}
			return record, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], record, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], record, rhs)
	}
}

func (record Record) PrefixOp(op byte) (Value, error) {
	return nil, TypeErrorInPrefixOperator(KindDescription[op], record)
}

func (record Record) IsIterable() bool {
	return true
}

func (record Record) MakeIterator() Iterator {
	return NewRecordIterator(record, false)
}

func (record Record) IsHashable() bool {
	return false
}

func (record Record) MakeHashKey() HashKey {
	return HashKey{}
}

func (record Record) IsValueSemantics() bool {
	return false
}

func (record Record) HasMethods() bool {
	return true
}

func (record Record) GetMethod(name string) (Value, bool, error) {
	if method, ok := record.Properties[name]; ok {
		return method, true, nil
	}
	return nil, false, MethodNotDefined(name, record)
}

func (record Record) Clone() Value {
	rec, _ := recordClone(record)
	return rec
}

// Interface Subscript Operator.
func (record Record) SubscriptGet(index Value) (Value, error) {
	switch key := index.(type) {
	case *String:
		if value, ok := record.Properties[key.Value]; ok {
			return value, nil
		} else {
			return nil, NameNotDefinedInCompoundDataType(key.Value, record)
		}
	default:
		return nil, RecordPropertyError(key)
	}
}

func (record Record) SubscriptSet(index, value Value) error {
	property := index.(*String).Value
	record.Properties[property] = value
	return nil
}

func (fiber *Fiber) loadRecord() {
	modName := "Record"
	module := GModule{Name: modName, Namespace: make(Namespace)}
	module.Namespace["isEmpty"] = GFunction{Name: "isEmpty", Value: recordIsEmpty}
	module.Namespace["length"] = GFunction{Name: "length", Value: recordLength}
	module.Namespace["clear"] = GFunction{Name: "clear", Value: recordClear}
	module.Namespace["randomElement"] = GFunction{Name: "randomElement", Value: recordRandomElement}
	module.Namespace["remove"] = GFunction{Name: "remove", Value: recordRemoveProperty}
	module.Namespace["addPair"] = GFunction{Name: "addPair", Value: recordAddProperty}
	module.Namespace["keys"] = GFunction{Name: "keys", Value: recordGetKeys}
	module.Namespace["clone"] = GFunction{Name: "clone", Value: recordClone}
	module.Namespace["merged"] = GFunction{Name: "merged", Value: recordMerged}
	module.Namespace["merge"] = GFunction{Name: "merge", Value: recordMerge}
	module.Namespace["difference"] = GFunction{Name: "difference", Value: recordDifference}
	module.Namespace["contains"] = GFunction{Name: "contains", Value: recordContains}
	module.Namespace["makeIterator"] = GFunction{Name: "makeIterator", Value: recordMakeIterator}
	module.Namespace["prettyPrint"] = GFunction{Name: "prettyPrint", Value: recordPrettyPrint}
	fiber.module.namespace[modName] = module
}

func recordIsEmpty(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			return Bool(len(record.Properties) == 0), nil
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func recordLength(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			return Int(len(record.Properties)), nil
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func recordClear(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			for k := range record.Properties {
				delete(record.Properties, k)
			}
			return NilValue, nil
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func recordRandomElement(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			length := int64(len(record.Properties))
			if length == 0 {
				return NilValue, nil
			}
			rand.Seed(time.Now().UnixNano())
			randomIndex := rand.Int63n(length)
			index := int64(0)
			for key := range record.Properties {
				if randomIndex == index {
					return record.Properties[key], nil
				}
				index++
			}
			return NilValue, nil
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func recordAddProperty(args ...Value) (Value, error) {
	if len(args) == 3 {
		if record, okRecord := args[0].(Record); okRecord {
			if attribute, okAttribute := args[1].(*String); okAttribute {
				record.Properties[attribute.Value] = args[2]
				return args[2], nil
			}
			return nil, fmt.Errorf("expected a string as second argument")
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 3, len(args))
}

func recordRemoveProperty(args ...Value) (Value, error) {
	if len(args) == 2 {
		if record, okRecord := args[0].(Record); okRecord {
			if attribute, okAttribute := args[1].(*String); okAttribute {
				delete(record.Properties, attribute.Value)
				return NilValue, nil
			}
			return nil, fmt.Errorf("expected a string as second argument")
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func recordGetKeys(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			keys := make([]Value, 0, len(record.Properties))
			for k := range record.Properties {
				keys = append(keys, &String{Value: k})
			}
			return &List{Elements: keys}, nil
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func recordClone(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			properties := make(Namespace)
			for k, v := range record.Properties {
				properties[k] = v.Clone()
			}
			return Record{Properties: properties}, nil
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func recordMerged(args ...Value) (Value, error) {
	if len(args) == 2 {
		if lhs, ok := args[0].(Record); ok {
			if rhs, ok := args[1].(Record); ok {
				newMap := make(Namespace)
				for key, value := range rhs.Properties {
					newMap[key] = value
				}
				for key, value := range lhs.Properties {
					newMap[key] = value
				}
				return Record{Properties: newMap}, nil
			}
			return nil, fmt.Errorf("expected a record as second argument")
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func recordMerge(args ...Value) (Value, error) {
	if len(args) == 2 {
		if dest, ok := args[0].(Record); ok {
			if source, ok := args[1].(Record); ok {
				for key, value := range source.Properties {
					if _, exists := dest.Properties[key]; !exists {
						dest.Properties[key] = value
					}
				}
				return NilValue, nil
			}
			return nil, fmt.Errorf("expected a record as second argument")
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func recordDifference(args ...Value) (Value, error) {
	if len(args) == 2 {
		if lhs, ok := args[0].(Record); ok {
			if rhs, ok := args[1].(Record); ok {
				newMap := make(Namespace)
				for key, value := range lhs.Properties {
					if _, ok := rhs.Properties[key]; !ok {
						newMap[key] = value
					}
				}
				return Record{Properties: newMap}, nil
			}
			return nil, fmt.Errorf("expected a record as second argument")
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func recordContains(args ...Value) (Value, error) {
	if len(args) == 2 {
		if record, okRecord := args[0].(Record); okRecord {
			if attribute, okAttribute := args[1].(*String); okAttribute {
				var contains bool
				_, contains = record.Properties[attribute.Value]
				return Bool(contains), nil
			}
			return nil, fmt.Errorf("expected a string as second argument")
		}
		return nil, fmt.Errorf("expected a record as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func recordPrettyPrint(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			prettyRecord(record, 1)
			return NilValue, nil
		}
		return nil, fmt.Errorf("expected a record as argument")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func recordMakeIterator(args ...Value) (Value, error) {
	if len(args) == 1 {
		if record, ok := args[0].(Record); ok {
			return NewRecordIterator(record, false), nil
		}
		return nil, fmt.Errorf("expected a record as argument")
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func spaces(level int) string {
	return strings.Repeat("  ", level)
}

func prettyRecord(record Record, level int) {
	fmt.Printf("{\n")
	for key, value := range record.Properties {
		fmt.Printf("%v%v : ", spaces(level+1), key)
		switch v := value.(type) {
		case *String:
			fmt.Printf("\"%v\",\n", v)
		case *List:
			prettyList(v, level+2)
		case Map:
			prettyMap(v, level+2)
		case Record:
			prettyRecord(v, level+2)
		default:
			fmt.Printf("%v,\n", v)
		}
	}
	fmt.Printf("%v},\n", spaces(level-1))
}

func prettyList(xs *List, level int) {
	fmt.Printf("\n%v[\n", spaces(level-1))
	for _, value := range xs.Elements {
		fmt.Printf("%v", spaces(level+1))
		switch v := value.(type) {
		case *String:
			fmt.Printf("\"%v\",\n", v)
		case *List:
			prettyList(v, level+2)
		case Map:
			prettyMap(v, level+2)
		case Record:
			prettyRecord(v, level+2)
		default:
			fmt.Printf("%v,\n", v)
		}
	}
	fmt.Printf("%v],\n", spaces(level-1))
}

func prettyMap(m Map, level int) {
	fmt.Printf("\n%v[\n", spaces(level-1))
	for _, value := range m {
		fmt.Printf("%v%v : ", spaces(level+1), value.key)
		switch v := (value.value).(type) {
		case *String:
			fmt.Printf("\"%v\",\n", v)
		case *List:
			prettyList(v, level+2)
		case Map:
			prettyMap(v, level+2)
		case Record:
			prettyRecord(v, level+2)
		default:
			fmt.Printf("%v,\n", v)
		}
	}
	fmt.Printf("%v],\n", spaces(level-1))
}
