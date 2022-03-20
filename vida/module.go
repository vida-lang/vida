package vida

import (
	"fmt"
	"path/filepath"
	"unsafe"
)

// Importable is the common interface for GModules and VModules.
type Importable interface {
	GetModuleName() string
	IsGModule() bool
	IsVModule() bool
}

// GModule is the data structure to models an importable named environment written in Go.
type GModule struct {
	Name      string
	Namespace Namespace
}

// Interface Value
func (mod GModule) TypeName() string {
	return "Module"
}

func (module GModule) Description() string {
	return fmt.Sprint("GModule(", module.Name, "/", len(module.Namespace), ")")
}

func (mod GModule) Equals(other Value) bool {
	if value, ok := other.(GModule); ok {
		return mod.Name == value.Name
	}
	return false
}

func (mod GModule) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, mod)
}

func (mod GModule) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, mod)
}

func (mod GModule) IsIterable() bool {
	return false
}

func (mod GModule) MakeIterator() Iterator {
	return nil
}

func (mod GModule) IsHashable() bool {
	return false
}

func (mod GModule) MakeHashKey() HashKey {
	return HashKey{}
}

func (mod GModule) IsValueSemantics() bool {
	return false
}

func (mod GModule) HasMethods() bool {
	return false
}

func (mod GModule) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, mod)
}

// Interface Importable
func (mod GModule) GetModuleName() string {
	return mod.Name
}

func (mod GModule) IsGModule() bool {
	return true
}

func (mod GModule) IsVModule() bool {
	return false
}

func (mod GModule) Clone() Value {
	return mod
}

// Interface SelectorOperator. GModule implements only get becuase it is an immutable Value.
func (mod GModule) SelectorGet(field string) (Value, error) {
	if value, ok := mod.Namespace[field]; ok {
		return value, nil
	}
	return nil, NameNotDefinedInCompoundDataType(field, mod)
}

func (mod GModule) SelectorSet(string, Value) error {
	return ValueIsImmutable(mod)
}

// VModule models a Vida file with its compiled sorce code which is equivalent to a Vida module.
type VModule struct {
	path         string    // The module's path
	mainFunction Closure   // The module's mainFunction
	identifiers  []string  // The module's identifiers
	namespace    Namespace // The module's global namespace. It has all core functionality available.
	global       *Global   // A pointer to the global state shared by all modules.
}

// Interface Value
func (mod *VModule) TypeName() string {
	return "Module"
}

func (mod *VModule) Description() string {
	return fmt.Sprintf("VModule(%v/%v)", filepath.Base(mod.path), len(mod.namespace))
}

func (mod *VModule) Equals(other Value) bool {
	if value, ok := other.(*VModule); ok {
		return unsafe.Pointer(mod) == unsafe.Pointer(value)
	}
	return false
}

func (mod *VModule) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, mod)
}

func (mod *VModule) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, mod)
}

func (mod *VModule) IsIterable() bool {
	return false
}

func (mod *VModule) MakeIterator() Iterator {
	return nil
}

func (mod *VModule) IsHashable() bool {
	return false
}

func (mod *VModule) MakeHashKey() HashKey {
	return HashKey{}
}

func (mod *VModule) IsValueSemantics() bool {
	return false
}

func (mod *VModule) HasMethods() bool {
	return false
}

func (mod *VModule) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, mod)
}

func (mod *VModule) Clone() Value {
	return mod
}

// Interface Importable
func (mod *VModule) GetModuleName() string {
	return mod.mainFunction.Function.ModuleName
}

func (mod *VModule) IsGModule() bool {
	return false
}

func (mod *VModule) IsVModule() bool {
	return true
}

// Interface SelectorOperator. VModule implements only get becuase it is an immutable Value.
func (mod *VModule) SelectorGet(field string) (Value, error) {
	if value, ok := mod.namespace[field]; ok {
		return value, nil
	}
	return nil, NameNotDefinedInCompoundDataType(field, mod)
}

func (mod *VModule) SelectorSet(string, Value) error {
	return ValueIsImmutable(mod)
}
