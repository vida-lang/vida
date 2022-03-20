package stdlib

import "github.com/vida-lang/vida-lang/vida"

// Stdlib is the map mapping module names to LibLoaders.
var Stdlib = map[string]vida.LibLoader{
	"math":          loadMath,
	"random":        loadRandom,
	"time":          loadTime,
	"stringBuilder": loadStringBuilder,
	"bytesBuffer":   loadBytesBuffer,
	"io":            loadIO,
	"fmt":           loadFmt,
	"regex":         loadRegex,
	"cmd":           loadCMD,
}

// Templates for help when implementing the interface Value
/*

type {{TypeName}} struct {}

// Interface vida.Value
func () TypeName() string {
	return "Default"
}

func () Description() string {
	return "Default"
}

func () Equals(other vida.Value) bool {
	return false
}

func () BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, v)
}

func () PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, v)
}

func () IsIterable() bool {
	return false
}

func () MakeIterator() vida.Iterator {
	return nil
}

func () IsHashable() bool {
	return false
}

func () MakeHashKey() vida.HashKey {
	return vida.HashKey{}
}

func () IsValueSemantics() bool {
	return false
}

func () HasMethods() bool {
	return false
}

func () GetMethod(name string) (vida.Value, bool, error) {
	return nil, false, vida.MethodNotDefined(name, v)
}

func () Clone() vida.Value {
	return vida.NilValue
}
*/
