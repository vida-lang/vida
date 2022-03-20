package vida

// Bool models binary boolean values true, false
type Bool bool

// Interface Value
func (b Bool) TypeName() string {
	return "Bool"
}

func (b Bool) Description() string {
	if b {
		return "true"
	}
	return "false"
}

func (b Bool) Equals(other Value) bool {
	if value, ok := other.(Bool); ok {
		return b == value
	}
	return false
}

func (b Bool) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case Bool:
		switch op {
		case TKAnd:
			return b && rhs, nil
		case TKOr:
			return b || rhs, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
	}
}

func (b Bool) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKNot:
		return !b, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], b)
	}
}

func (b Bool) IsIterable() bool {
	return false
}

func (b Bool) MakeIterator() Iterator {
	return nil
}

func (b Bool) IsHashable() bool {
	return true
}

func (b Bool) MakeHashKey() HashKey {
	return HashKey{
		Type:             b.TypeName(),
		ValueDescription: b.Description(),
	}
}

func (b Bool) IsValueSemantics() bool {
	return true
}

func (b Bool) HasMethods() bool {
	return false
}

func (b Bool) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, b)
}

func (b Bool) Clone() Value {
	return b
}

// Nil is the value representing the absence of value.
type Nil byte

// Interface Value
func (n Nil) TypeName() string {
	return "Nil"
}

func (n Nil) Description() string {
	return "nil"
}

func (n Nil) Equals(other Value) bool {
	_, ok := other.(Nil)
	return ok
}

func (n Nil) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, TypeErrorInBinaryOperator(KindDescription[op], n, rhs)
}

func (n Nil) PrefixOp(op byte) (Value, error) {
	return nil, TypeErrorInPrefixOperator(KindDescription[op], n)
}

func (n Nil) IsIterable() bool {
	return false
}

func (n Nil) MakeIterator() Iterator {
	return nil
}

func (n Nil) IsHashable() bool {
	return true
}

func (n Nil) MakeHashKey() HashKey {
	return HashKey{
		Type:             n.TypeName(),
		ValueDescription: n.Description(),
	}
}

func (n Nil) IsValueSemantics() bool {
	return true
}

func (n Nil) HasMethods() bool {
	return false
}

func (n Nil) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, n)
}

func (n Nil) Clone() Value {
	return n
}
