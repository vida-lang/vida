package vida

import (
	"bytes"
	"fmt"
	"strconv"
)

// Value interface models the inteface for all Vida Values.
type Value interface {
	TypeName() string
	Description() string
	Equals(Value) bool
	BinaryOp(byte, Value) (Value, error)
	PrefixOp(byte) (Value, error)
	IsIterable() bool
	MakeIterator() Iterator
	IsHashable() bool
	MakeHashKey() HashKey
	IsValueSemantics() bool
	HasMethods() bool
	GetMethod(string) (Value, bool, error)
	Clone() Value
}

// SelectorOperable interface for Values implementing operator selector (value '.' property)
type SelectorOperable interface {
	SelectorGet(string) (Value, error)
	SelectorSet(string, Value) error
}

// SubscriptOperable interface for Values implementing operator subscript (value '[expr]')
type SubscriptOperable interface {
	SubscriptGet(Value) (Value, error)
	SubscriptSet(Value, Value) error
}

// SliceOperable interface for Values implementing slice operator subscript (value '[e:e]' | '[e:]' | '[:e]' | '[:]')
type SliceOperable interface {
	SliceGet(UInt32, Value, Value) (Value, error)
	SliceSet(UInt32, Value, Value, Value) error
}

// HashKey models a key for a Map and Set collections.
type HashKey struct {
	Type             string
	ValueDescription string
}

// Interface Value
func (hash HashKey) TypeName() string {
	return "HashKey"
}

func (hash HashKey) Description() string {
	return fmt.Sprintf("hashkey(%v)", hash.ValueDescription)
}

func (hash HashKey) Equals(other Value) bool {
	if value, ok := other.(HashKey); ok {
		return hash.Type == value.Type && hash.ValueDescription == value.ValueDescription
	}
	return false
}

func (hash HashKey) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, hash)
}

func (hash HashKey) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, hash)
}

func (hash HashKey) IsIterable() bool {
	return false
}

func (hash HashKey) MakeIterator() Iterator {
	return nil
}

func (hash HashKey) IsHashable() bool {
	return true
}

func (hash HashKey) MakeHashKey() HashKey {
	return hash
}

func (hash HashKey) IsValueSemantics() bool {
	return false
}

func (hash HashKey) HasMethods() bool {
	return false
}

func (hash HashKey) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, hash)
}

func (hash HashKey) Clone() Value {
	return HashKey{Type: hash.Type, ValueDescription: hash.ValueDescription}
}

// Type IStop is a type for signaling the exhaustation of iterators.
type IStop byte

// Interface Value
func (istop IStop) TypeName() string {
	return "<IStop>"
}

func (eof IStop) Description() string {
	return "<IStop>"
}

func (istop IStop) Equals(other Value) bool {
	_, ok := other.(IStop)
	return ok
}

func (istop IStop) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, istop)
}

func (istop IStop) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, istop)
}

func (istop IStop) IsIterable() bool {
	return false
}

func (istop IStop) MakeIterator() Iterator {
	return nil
}

func (istop IStop) IsHashable() bool {
	return true
}

func (istop IStop) MakeHashKey() HashKey {
	return HashKey{
		Type:             istop.TypeName(),
		ValueDescription: istop.Description(),
	}
}

func (istop IStop) IsValueSemantics() bool {
	return true
}

func (istop IStop) HasMethods() bool {
	return false
}

func (istop IStop) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, istop)
}

func (istop IStop) Clone() Value {
	return istop
}

// Function models the static representation of callable, executable code.
type Function struct {
	Name                  string            // The name of the function.
	Arity                 UInt32            // The arity of the function.
	Vararg                bool              // The function can receive an arbitrary number of args.
	FreeVarCount          Bytecode          // Count of free variables.
	Code                  []Bytecode        // The Bytecode of the function.
	Lines                 map[UInt32]UInt32 // A map between Bytecode to number lines.
	Constants             []Value           // The constant list of values of the function.
	ModuleName            string            // The module in this function has been defined.
	CanAccessPrivateState bool              // The function can access private state of an instance.
}

// Interface Value
func (function Function) TypeName() string {
	return "Function"
}

func (function Function) Description() string {
	if function.Vararg {
		if function.Arity == 0 {
			return fmt.Sprintf("function(%v/...)", function.Name)
		}
		return fmt.Sprintf("function(%v/%v...)", function.Name, function.Arity)
	}
	return fmt.Sprintf("function(%v/%v)", function.Name, function.Arity)
}

func (function Function) Equals(other Value) bool {
	if value, ok := other.(Function); ok {
		return fmt.Sprint(function) == fmt.Sprint(value)
	}
	return false
}

func (function Function) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, function)
}

func (function Function) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, function)
}

func (function Function) IsIterable() bool {
	return false
}

func (function Function) MakeIterator() Iterator {
	return nil
}

func (function Function) IsHashable() bool {
	return true
}

func (function Function) MakeHashKey() HashKey {
	return HashKey{
		Type:             function.TypeName(),
		ValueDescription: fmt.Sprint(function),
	}
}

func (function Function) IsValueSemantics() bool {
	return false
}

func (function Function) HasMethods() bool {
	return false
}

func (function Function) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, function)
}

func (function Function) Clone() Value {
	return function
}

// Closure is the run time representation of a function with its own captured environment.
type Closure struct {
	Function *Function // The function this closure has.
	FreeVars []Value   // The run time free variables linked to this closure.
	StructID UInt      // In case the function can access private state, this is the run time structu id linked to.
}

// Interface Value
func (closure Closure) TypeName() string {
	return "Function"
}

func (closure Closure) Description() string {
	return closure.Function.Description()
}

func (closure Closure) Equals(other Value) bool {
	if value, ok := other.(Closure); ok {
		return closure.Function == value.Function && fmt.Sprint(closure.FreeVars) == fmt.Sprint(value.FreeVars)
	}
	return false
}

func (closure Closure) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, closure)
}

func (closure Closure) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, closure)
}

func (closure Closure) IsIterable() bool {
	return false
}

func (closure Closure) MakeIterator() Iterator {
	return nil
}

func (closure Closure) IsHashable() bool {
	return true
}

func (closure Closure) MakeHashKey() HashKey {
	return HashKey{
		Type:             closure.TypeName(),
		ValueDescription: fmt.Sprint(closure.Function),
	}
}

func (closure Closure) IsValueSemantics() bool {
	return false
}

func (closure Closure) HasMethods() bool {
	return false
}

func (closure Closure) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, closure)
}

func (closure Closure) Clone() Value {
	return closure
}

// GFunction is a Value that represents a function written in Go that can be run for the Vida VM.
type GFunction struct {
	Name  string
	Value func(args ...Value) (Value, error)
}

// Interface Value
func (gf GFunction) TypeName() string {
	return "GFunction"
}

func (gf GFunction) Description() string {
	return fmt.Sprintf("gfunction%v", gf)
}

func (gf GFunction) Equals(other Value) bool {
	if _, ok := other.(GFunction); ok {
		return fmt.Sprint(gf) == fmt.Sprint(other)
	}
	return false
}

func (gf GFunction) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, gf)
}

func (gf GFunction) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, gf)
}

func (gf GFunction) IsIterable() bool {
	return false
}

func (gf GFunction) MakeIterator() Iterator {
	return nil
}

func (gf GFunction) IsHashable() bool {
	return true
}

func (gf GFunction) MakeHashKey() HashKey {
	return HashKey{
		Type:             gf.TypeName(),
		ValueDescription: gf.Description(),
	}
}

func (gf GFunction) IsValueSemantics() bool {
	return false
}

func (gf GFunction) HasMethods() bool {
	return false
}

func (gf GFunction) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, gf)
}

func (gf GFunction) Clone() Value {
	return gf
}

// Result models the result of computations that could fail.
type Result struct {
	Error Value
	Value Value
}

// Interface SelectorOperator. Result implements only get becuase it is an immutable Value.
func (result Result) SelectorGet(field string) (Value, error) {
	switch field {
	case optionValue:
		return result.Value, nil
	case optionError:
		return result.Error, nil
	default:
		return nil, NameNotDefinedInCompoundDataType(field, result)
	}
}

func (result Result) SelectorSet(string, Value) error {
	return ValueIsImmutable(result)
}

// Interface Value
func (result Result) TypeName() string {
	return "Result"
}

func (result Result) Description() string {
	return fmt.Sprintf("Result(value:%v error:%v)", result.Value.Description(), result.Error.Description())
}

func (result Result) Equals(other Value) bool {
	if value, ok := other.(Result); ok {
		return result.Value.Equals(value.Value) && result.Error.Equals(value.Error)
	}
	return false
}

func (result Result) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, result)
}

func (result Result) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, result)
}

func (result Result) IsIterable() bool {
	return false
}

func (result Result) MakeIterator() Iterator {
	return nil
}

func (result Result) IsHashable() bool {
	return false
}

func (result Result) MakeHashKey() HashKey {
	return HashKey{}
}

func (result Result) IsValueSemantics() bool {
	return false
}

func (result Result) HasMethods() bool {
	return false
}

func (result Result) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, result)
}

func (result Result) Clone() Value {
	return Result{
		Error: result.Error.Clone(),
		Value: result.Value.Clone(),
	}
}

// Struct is the stencil or blueprint of a new user-defined structured data type.
type Struct struct {
	Id      UInt
	Name    string
	Public  Namespace
	Private Namespace
	Methods Namespace
}

// Interface SelectorOperator. Struct implements only get becuase it is an immutable Value.
func (s Struct) SelectorGet(field string) (Value, error) {
	if value, ok := s.Methods[field]; ok {
		return value, nil
	}
	return nil, NameNotDefinedInCompoundDataType(field, s)
}

func (s Struct) SelectorSet(string, Value) error {
	return ValueIsImmutable(s)
}

// Interface Value
func (s Struct) TypeName() string {
	return "Struct"
}

func (s Struct) Description() string {
	return fmt.Sprintf("struct(%v{%v/%v})", s.Name, len(s.Public), len(s.Methods))
}

func (s Struct) Equals(other Value) bool {
	if value, ok := other.(Struct); ok {
		return s.Name == value.Name
	}
	return false
}

func (s Struct) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, s)
}

func (s Struct) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, s)
}

func (s Struct) IsIterable() bool {
	return false
}

func (s Struct) MakeIterator() Iterator {
	return nil
}

func (s Struct) IsHashable() bool {
	return true
}

func (s Struct) MakeHashKey() HashKey {
	return HashKey{
		Type:             s.TypeName(),
		ValueDescription: s.Description(),
	}
}

func (s Struct) IsValueSemantics() bool {
	return false
}

func (s Struct) HasMethods() bool {
	return false
}

func (s Struct) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, s)
}

func (s Struct) Clone() Value {
	return s
}

// Instance models unique objects instances of some structured data types.
type Instance struct {
	Id      UInt
	Struct  *Struct
	Public  Namespace
	Private Namespace
}

// Interface SelectorOperator.
func (instance Instance) SelectorGet(field string) (Value, error) {
	if value, ok := instance.Public[field]; ok {
		return value, nil
	}
	if value, ok := instance.Private[field]; ok && globalState.currentFiber.frame.closure.Function.CanAccessPrivateState && globalState.currentFiber.frame.closure.StructID == instance.Struct.Id {
		return value, nil
	}
	return nil, NameNotDefinedInCompoundDataType(field, instance)
}

func (instance Instance) SelectorSet(property string, value Value) error {
	if _, ok := instance.Public[property]; ok {
		instance.Public[property] = value
		return nil
	}
	if _, ok := instance.Private[property]; ok && globalState.currentFiber.frame.closure.Function.CanAccessPrivateState && globalState.currentFiber.frame.closure.StructID == instance.Struct.Id {
		instance.Private[property] = value
		return nil
	}
	return NameNotDefinedInCompoundDataType(property, instance)
}

// Interface Value
func (instance Instance) TypeName() string {
	return instance.Struct.Name
}

func (instance Instance) Description() string {
	var builder bytes.Buffer
	builder.WriteString(instance.Struct.Name)
	builder.WriteString("( ")
	for key, value := range instance.Public {
		builder.WriteString(fmt.Sprintf("%v:%v ", key, value.Description()))
	}
	builder.WriteString(")")
	return builder.String()
}

func (instance Instance) Equals(other Value) bool {
	if value, ok := other.(Instance); ok && instance.Id == value.Id && instance.Struct.Name == value.Struct.Name {
		for k, v := range instance.Public {
			if val, okProp := value.Public[k]; !(okProp && v.Equals(val)) {
				return false
			}
		}
		return true
	}
	return false
}

func (instance Instance) BinaryOp(op byte, rhs Value) (Value, error) {
	var operator string
	switch op {
	case TKAdd:
		operator = operatorAdd
	case TKMinus:
		operator = operatorSub
	case TKMul:
		operator = operatorMul
	case TKDiv:
		operator = operatorDiv
	case TKMod:
		operator = operatorMod
	case TKPercent:
		operator = operatorRem
	case TKPower:
		operator = operatorPow
	case TKGT:
		operator = operatorGT
	case TKGE:
		operator = operatorGE
	case TKLT:
		operator = operatorLT
	case TKLE:
		operator = operatorLE
	case TKAnd:
		operator = operatorAnd
	case TKOr:
		operator = operatorLOr
	default:
		return nil, OperatorNotDefined(op, instance)
	}
	if method, okMethod := instance.Struct.Methods[operator].(Closure); okMethod {
		if method.Function.Arity == 2 {
			fiber := fiberPool.Get().(*Fiber)
			fiber.reset(method)
			fiber.parentFiber = globalState.currentFiber
			fiber.state = fiberRunning
			fiber.parentFiber.state = fiberWaiting
			globalState.currentFiber = fiber
			globalState.vm.Fiber = fiber
			globalState.vm.stack[globalState.vm.top] = instance
			globalState.vm.top++
			globalState.vm.stack[globalState.vm.top] = rhs
			globalState.vm.top++
			if err := globalState.vm.runInterpreter(method.Function.Name); err != nil {
				fiberPool.Put(fiber)
				return NilValue, err
			}
			value := globalState.vm.stack[globalState.vm.top-1]
			globalState.currentFiber = globalState.currentFiber.parentFiber
			globalState.vm.Fiber = globalState.currentFiber
			globalState.currentFiber.state = fiberRunning
			fiberPool.Put(fiber)
			return value, nil
		} else {
			return nil, ArityErrorInBinaryOperatorOverload()
		}
	} else {
		return nil, MethodNotDefined(operator, instance)
	}
}

func (instance Instance) PrefixOp(op byte) (Value, error) {
	var operator string
	switch op {
	case TKNot:
		operator = operatorNot
	case TKMinus:
		operator = operatorPrefixNeg
	case TKAdd:
		operator = operatorPrefixPos
	default:
		return nil, OperatorNotDefined(op, instance)
	}
	if method, okMethod := instance.Struct.Methods[operator].(Closure); okMethod {
		if method.Function.Arity == 1 {
			fiber := fiberPool.Get().(*Fiber)
			fiber.reset(method)
			fiber.parentFiber = globalState.currentFiber
			fiber.state = fiberRunning
			fiber.parentFiber.state = fiberWaiting
			globalState.currentFiber = fiber
			globalState.vm.Fiber = fiber
			globalState.vm.stack[globalState.vm.top] = instance
			globalState.vm.top++
			if err := globalState.vm.runInterpreter(method.Function.Name); err != nil {
				fiberPool.Put(fiber)
				return NilValue, err
			}
			value := globalState.vm.stack[globalState.vm.top-1]
			globalState.currentFiber = globalState.currentFiber.parentFiber
			globalState.vm.Fiber = globalState.currentFiber
			globalState.currentFiber.state = fiberRunning
			fiberPool.Put(fiber)
			return value, nil
		} else {
			return nil, ArityErrorInUnaryOperatorOverload()
		}
	} else {
		return nil, MethodNotDefined(operator, instance)
	}
}

func (instance Instance) IsIterable() bool {
	return false
}

func (instance Instance) MakeIterator() Iterator {
	return nil
}

func (instance Instance) IsHashable() bool {
	return true
}

func (instance Instance) MakeHashKey() HashKey {
	return HashKey{
		Type:             instance.TypeName(),
		ValueDescription: strconv.FormatUint(uint64(instance.Id), 16),
	}
}

func (instance Instance) IsValueSemantics() bool {
	return false
}

func (instance Instance) HasMethods() bool {
	return true
}

func (instance Instance) GetMethod(name string) (Value, bool, error) {
	if method, ok := instance.Struct.Methods[name]; ok {
		return method, true, nil
	}
	if prop, ok := instance.Public[name]; ok {
		return prop, false, nil
	}
	return nil, false, MethodNotDefined(name, instance)
}

func (instance Instance) Clone() Value {
	id := globalInstanceUniqueID
	globalInstanceUniqueID++
	namespace := make(Namespace)
	for k, v := range instance.Public {
		namespace[k] = v.Clone()
	}
	private := make(Namespace)
	for k, v := range instance.Private {
		private[k] = v.Clone()
	}
	return Instance{
		Struct:  instance.Struct,
		Id:      id,
		Public:  namespace,
		Private: private,
	}
}

// Interface subscript operator.
func (instance Instance) SubscriptGet(index Value) (Value, error) {
	if method, okMethod := instance.Struct.Methods[operatorSubscriptGet].(Closure); okMethod {
		if method.Function.Arity == 2 {
			fiber := fiberPool.Get().(*Fiber)
			fiber.reset(method)
			fiber.parentFiber = globalState.currentFiber
			fiber.state = fiberRunning
			fiber.parentFiber.state = fiberWaiting
			globalState.currentFiber = fiber
			globalState.vm.Fiber = fiber
			globalState.vm.stack[globalState.vm.top] = instance
			globalState.vm.top++
			globalState.vm.stack[globalState.vm.top] = index
			globalState.vm.top++
			if err := globalState.vm.runInterpreter(method.Function.Name); err != nil {
				fiberPool.Put(fiber)
				return NilValue, err
			}
			value := globalState.vm.stack[globalState.vm.top-1]
			globalState.currentFiber = globalState.currentFiber.parentFiber
			globalState.vm.Fiber = globalState.currentFiber
			globalState.currentFiber.state = fiberRunning
			fiberPool.Put(fiber)
			return value, nil
		} else {
			return nil, ArityErrorInUnaryOperatorOverload()
		}
	} else {
		return nil, MethodNotDefined(operatorSubscriptGet, instance)
	}
}

func (instance Instance) SubscriptSet(index, value Value) error {
	if method, okMethod := instance.Struct.Methods[operatorSubscriptSet].(Closure); okMethod {
		if method.Function.Arity == 3 {
			fiber := fiberPool.Get().(*Fiber)
			fiber.reset(method)
			fiber.parentFiber = globalState.currentFiber
			fiber.state = fiberRunning
			fiber.parentFiber.state = fiberWaiting
			globalState.currentFiber = fiber
			globalState.vm.Fiber = fiber
			globalState.vm.stack[globalState.vm.top] = instance
			globalState.vm.top++
			globalState.vm.stack[globalState.vm.top] = index
			globalState.vm.top++
			globalState.vm.stack[globalState.vm.top] = value
			globalState.vm.top++
			if err := globalState.vm.runInterpreter(method.Function.Name); err != nil {
				fiberPool.Put(fiber)
				return err
			}
			globalState.currentFiber = globalState.currentFiber.parentFiber
			globalState.vm.Fiber = globalState.currentFiber
			globalState.currentFiber.state = fiberRunning
			fiberPool.Put(fiber)
			return nil
		} else {
			return ArityErrorInUnaryOperatorOverload()
		}
	} else {
		return MethodNotDefined(operatorSubscriptGet, instance)
	}
}

// Pair is a helper struct to hold keys and values as return value when iterating over maps and records.
type Pair struct {
	key   Value
	value Value
}

// Interface SelectorOperator. Pair implements only get becuase it is an immutable Value.
func (pair Pair) SelectorGet(field string) (Value, error) {
	switch field {
	case mapPairKey:
		return pair.key, nil
	case mapPairValue:
		return pair.value, nil
	default:
		return nil, NameNotDefinedInCompoundDataType(field, pair)
	}
}

func (pair Pair) SelectorSet(string, Value) error {
	return ValueIsImmutable(pair)
}

// Interface Value
func (pair Pair) TypeName() string {
	return "Pair"
}

func (pair Pair) Description() string {
	return fmt.Sprintf("Pair( key:%v value:%v )", pair.key.Description(), pair.value.Description())
}

func (pair Pair) Equals(other Value) bool {
	if value, ok := other.(Pair); ok {
		return pair.key.Equals(value.key) && pair.value.Equals(value.value)
	}
	return false
}

func (pair Pair) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, pair)
}

func (pair Pair) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, pair)
}

func (pair Pair) IsIterable() bool {
	return false
}

func (pair Pair) MakeIterator() Iterator {
	return nil
}

func (pair Pair) IsHashable() bool {
	return false
}

func (pair Pair) MakeHashKey() HashKey {
	return HashKey{}
}

func (pair Pair) IsValueSemantics() bool {
	return false
}

func (pair Pair) HasMethods() bool {
	return false
}

func (pair Pair) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, pair)
}

func (pair Pair) Clone() Value {
	return Pair{
		key:   pair.key.Clone(),
		value: pair.value.Clone(),
	}
}

// Tuple is a helper struct to hold indexes and values as return value when iterating over lists and strings.
type Tuple struct {
	index Value
	value Value
}

// Interface SelectorOperator. Tuple implements only get becuase it is an immutable Value.
func (tuple Tuple) SelectorGet(field string) (Value, error) {
	switch field {
	case tupleIndex:
		return tuple.index, nil
	case mapPairValue:
		return tuple.value, nil
	default:
		return nil, NameNotDefinedInCompoundDataType(field, tuple)
	}
}

func (tuple Tuple) SelectorSet(string, Value) error {
	return ValueIsImmutable(tuple)
}

// Interface Value
func (tuple Tuple) TypeName() string {
	return "Tuple"
}

func (tuple Tuple) Description() string {
	return fmt.Sprintf("Tuple( index:%v value:%v )", tuple.index.Description(), tuple.value.Description())
}

func (tuple Tuple) Equals(other Value) bool {
	if value, ok := other.(Tuple); ok {
		return tuple.index.Equals(value.index) && tuple.value.Equals(value.value)
	}
	return false
}

func (tuple Tuple) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, tuple)
}

func (tuple Tuple) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, tuple)
}

func (tuple Tuple) IsIterable() bool {
	return false
}

func (tuple Tuple) MakeIterator() Iterator {
	return nil
}

func (tuple Tuple) IsHashable() bool {
	return false
}

func (tuple Tuple) MakeHashKey() HashKey {
	return HashKey{}
}

func (tuple Tuple) IsValueSemantics() bool {
	return false
}

func (tuple Tuple) HasMethods() bool {
	return false
}

func (tuple Tuple) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, tuple)
}

func (tuple Tuple) Clone() Value {
	return Tuple{
		index: tuple.index.Clone(),
		value: tuple.value.Clone(),
	}
}

// NamedConstants models small namespace of constant values. Those values must have value semantics.
type NamedConstants struct {
	Name      string
	Constants Namespace
	Indexes   map[Bytecode]string
}

// Interface SelectorOperator. NamedConstants implements only get becuase it is an immutable Value.
func (constants NamedConstants) SelectorGet(field string) (Value, error) {
	if value, ok := constants.Constants[field]; ok {
		return value, nil
	}
	return nil, NameNotDefinedInCompoundDataType(field, constants)
}

func (constants NamedConstants) SelectorSet(string, Value) error {
	return ValueIsImmutable(constants)
}

// Interface Value
func (constants NamedConstants) TypeName() string {
	return "GroupedConstants"
}

func (constants NamedConstants) Description() string {
	return fmt.Sprintf("NamedConstants(%v/%v)", constants.Name, len(constants.Constants))
}

func (constants NamedConstants) Equals(other Value) bool {
	if value, ok := other.(NamedConstants); ok {
		return constants.Name == value.Name
	}
	return false
}

func (constants NamedConstants) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, constants)
}

func (constants NamedConstants) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, constants)
}

func (constants NamedConstants) IsIterable() bool {
	return false
}

func (constants NamedConstants) MakeIterator() Iterator {
	return nil
}

func (constants NamedConstants) IsHashable() bool {
	return true
}

func (constants NamedConstants) MakeHashKey() HashKey {
	return HashKey{
		Type:             constants.TypeName(),
		ValueDescription: constants.Description(),
	}
}

func (constants NamedConstants) IsValueSemantics() bool {
	return false
}

func (constants NamedConstants) HasMethods() bool {
	return false
}

func (constants NamedConstants) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, constants)
}

func (constants NamedConstants) Clone() Value {
	return constants
}

// Value Interface Template.
/*

// Value Interface
func () TypeName() string {}
func () Description() string {}
func () Equals(Value) bool {}

func () BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, )
}

func () PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, )
}

func () IsIterable() bool {
	return false
}

func () MakeIterator() Iterator {
	return nil
}

func () IsHashable() bool {
	return true
}

func () MakeHashKey() HashKey {
	return HashKey{
		Type:        .TypeName(),
		ValueDescription: .Description(),
	}
}

func () IsValueSemantics() bool {
	return false
}

func () HasMethods() bool {
	return false
}

func () GetMethod(name string) (vida.Value, bool, error) {
	return nil, false, MethodNotDefined(name, )
}

func ( ) Clone() Value {

}
*/
