package vida

import (
	"fmt"
	"math"
	"math/big"
	"math/cmplx"
	"strings"
	"unicode/utf8"
)

// IntegerNumber is the common interface for numeric values used as indexes in List, Strings or Bytes: Int, UInt, Byte and BInt.
type IntegerNumber interface {
	ToInt() Int
}

// Int models a signed integer of 64 bits machine independent.
type Int int64

// Interface IntegerNumber
func (integer Int) ToInt() Int {
	return integer
}

// Interface Value
func (integer Int) TypeName() string {
	return "Int"
}

func (integer Int) Description() string {
	return fmt.Sprint(integer)
}

func (integer Int) Equals(other Value) bool {
	if value, ok := other.(Int); ok {
		return integer == value
	}
	return false
}

// Iterface Operable
func (integer Int) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case Int:
		switch op {
		case TKAdd:
			return integer + rhs, nil
		case TKMinus:
			return integer - rhs, nil
		case TKMul:
			return integer * rhs, nil
		case TKDiv:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return integer / rhs, nil
		case TKMod:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			modulus := integer % rhs
			if modulus < 0 {
				return modulus + rhs, nil
			}
			return modulus, nil
		case TKPercent:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return integer % rhs, nil
		case TKPower:
			return Int(math.Pow(float64(integer), float64(rhs))), nil
		case TKGT:
			return Bool(integer > rhs), nil
		case TKGE:
			return Bool(integer >= rhs), nil
		case TKLT:
			return Bool(integer < rhs), nil
		case TKLE:
			return Bool(integer <= rhs), nil
		case TKAmpersand:
			return integer & rhs, nil
		case TKBar:
			return integer | rhs, nil
		case TKHat:
			return integer ^ rhs, nil
		case TKLShift:
			if rhs < 0 {
				return nil, NegativeShiftError(KindDescription[op], integer, rhs)
			}
			return integer << rhs, nil
		case TKRShift:
			if rhs < 0 {
				return nil, NegativeShiftError(KindDescription[op], integer, rhs)
			}
			return integer >> rhs, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], integer, rhs)
		}
	case *String:
		switch op {
		case TKMul:
			var builder strings.Builder
			for k := Int(0); k < integer; k++ {
				builder.WriteString(rhs.Value)
			}
			return &String{Value: builder.String()}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], integer, rhs)
		}
	case *List:
		switch op {
		case TKMul:
			xs := make([]Value, 0, Int(len(rhs.Elements))*integer)
			for k := Int(0); k < integer; k++ {
				xs = append(xs, rhs.Elements...)
			}
			return &List{Elements: xs}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], integer, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], integer, rhs)
	}
}

func (integer Int) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKMinus:
		return -integer, nil
	case TKAdd:
		return integer, nil
	case TKTilde:
		return ^integer, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], integer)
	}
}

func (integer Int) IsIterable() bool {
	return false
}

func (integer Int) MakeIterator() Iterator {
	return nil
}

func (integer Int) IsHashable() bool {
	return true
}

func (integer Int) MakeHashKey() HashKey {
	return HashKey{
		Type:             integer.TypeName(),
		ValueDescription: integer.Description(),
	}
}

func (integer Int) IsValueSemantics() bool {
	return true
}

func (integer Int) HasMethods() bool {
	return false
}

func (integer Int) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, integer)
}

func (integer Int) Clone() Value {
	return integer
}

// Type UInt models unsigned inteters of 64 bits machine independent.
type UInt uint64

// Interface IntegerNumber
func (u UInt) ToInt() Int {
	return Int(u)
}

// Interface Value
func (u UInt) TypeName() string {
	return "UInt"
}

func (u UInt) Description() string {
	return fmt.Sprint(u)
}

func (u UInt) Equals(other Value) bool {
	if value, ok := other.(UInt); ok {
		return u == value
	}
	return false
}

func (u UInt) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case UInt:
		switch op {
		case TKAdd:
			return u + rhs, nil
		case TKMinus:
			return u - rhs, nil
		case TKMul:
			return u * rhs, nil
		case TKDiv:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return u / rhs, nil
		case TKMod:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return u % rhs, nil
		case TKPercent:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return u % rhs, nil
		case TKPower:
			return UInt(math.Pow(float64(u), float64(rhs))), nil
		case TKGT:
			return Bool(u > rhs), nil
		case TKGE:
			return Bool(u >= rhs), nil
		case TKLT:
			return Bool(u < rhs), nil
		case TKLE:
			return Bool(u <= rhs), nil
		case TKAmpersand:
			return u & rhs, nil
		case TKBar:
			return u | rhs, nil
		case TKHat:
			return u ^ rhs, nil
		case TKLShift:
			return u << rhs, nil
		case TKRShift:
			return u >> rhs, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], u, rhs)
		}
	case *String:
		switch op {
		case TKMul:
			var builder strings.Builder
			for k := UInt(0); k < u; k++ {
				builder.WriteString(rhs.Value)
			}
			return &String{Value: builder.String()}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], u, rhs)
		}
	case *List:
		switch op {
		case TKMul:
			xs := make([]Value, 0, UInt(len(rhs.Elements))*u)
			for k := UInt(0); k < u; k++ {
				xs = append(xs, rhs.Elements...)
			}
			return &List{Elements: xs}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], u, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], u, rhs)
	}
}

func (u UInt) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKMinus:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], u)
	case TKAdd:
		return u, nil
	case TKTilde:
		return ^u, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], u)
	}
}

func (u UInt) IsIterable() bool {
	return false
}

func (u UInt) MakeIterator() Iterator {
	return nil
}

func (u UInt) IsHashable() bool {
	return true
}

func (u UInt) MakeHashKey() HashKey {
	return HashKey{
		Type:             u.TypeName(),
		ValueDescription: u.Description(),
	}
}

func (u UInt) IsValueSemantics() bool {
	return true
}

func (u UInt) HasMethods() bool {
	return false
}

func (u UInt) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, u)
}

func (u UInt) Clone() Value {
	return u
}

// Float models a double precision floating-point number IEEE-754 of 64 bits.
type Float float64

// Interface Value
func (f Float) TypeName() string {
	return "Float"
}

func (f Float) Description() string {
	return fmt.Sprint(f)
}

func (f Float) Equals(other Value) bool {
	if value, ok := other.(Float); ok {
		return f == value
	}
	return false
}

func (f Float) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case Float:
		switch op {
		case TKAdd:
			return f + rhs, nil
		case TKMinus:
			return f - rhs, nil
		case TKMul:
			return f * rhs, nil
		case TKDiv:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return f / rhs, nil
		case TKMod:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			modulus := math.Mod(float64(f), float64(rhs))
			if modulus < 0 {
				return Float(modulus + float64(rhs)), nil
			}
			return Float(modulus), nil
		case TKPercent:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return Float(math.Remainder(float64(f), float64(rhs))), nil
		case TKPower:
			result := math.Pow(float64(f), float64(rhs))
			if math.IsNaN(result) || math.IsInf(result, 0) {
				return nil, fmt.Errorf("%v to the power of %v = %v and it is not a numeric value", f, rhs, result)
			}
			return Float(result), nil
		case TKGT:
			return Bool(f > rhs), nil
		case TKGE:
			return Bool(f >= rhs), nil
		case TKLT:
			return Bool(f < rhs), nil
		case TKLE:
			return Bool(f <= rhs), nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], f, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], f, rhs)
	}
}

func (f Float) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKMinus:
		return -f, nil
	case TKAdd:
		return +f, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], f)
	}
}

func (f Float) IsIterable() bool {
	return false
}

func (f Float) MakeIterator() Iterator {
	return nil
}

func (f Float) IsHashable() bool {
	return true
}

func (f Float) MakeHashKey() HashKey {
	return HashKey{
		Type:             f.TypeName(),
		ValueDescription: f.Description(),
	}
}
func (f Float) IsValueSemantics() bool {
	return true
}

func (f Float) HasMethods() bool {
	return false
}

func (f Float) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, f)
}

func (f Float) Clone() Value {
	return f
}

// Byte models raw machine Bytes or unisgned integers of 8 bits.
type Byte byte

// Interface IntegerNumber
func (b Byte) ToInt() Int {
	return Int(b)
}

// Interface Value
func (b Byte) TypeName() string {
	return "Byte"
}

func (b Byte) Description() string {
	return fmt.Sprintf("%#v", b)
}

func (b Byte) Equals(other Value) bool {
	if value, ok := other.(Byte); ok {
		return b == value
	}
	return false
}

func (b Byte) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case IntegerNumber:
		switch op {
		case TKAdd:
			return b + Byte(rhs.ToInt()), nil
		case TKMinus:
			return b - Byte(rhs.ToInt()), nil
		case TKMul:
			return b * Byte(rhs.ToInt()), nil
		case TKDiv:
			if rhs == Int(0) {
				return nil, DivisionByZeroError()
			}
			return b / Byte(rhs.ToInt()), nil
		case TKMod:
			if rhs == Int(0) {
				return nil, DivisionByZeroError()
			}
			return b % Byte(rhs.ToInt()), nil
		case TKPercent:
			if rhs == Int(0) {
				return nil, DivisionByZeroError()
			}
			return b % Byte(rhs.ToInt()), nil
		case TKPower:
			return Byte(math.Pow(float64(b), float64(rhs.ToInt()))), nil
		case TKGT:
			return Bool(b > Byte(rhs.ToInt())), nil
		case TKGE:
			return Bool(b >= Byte(rhs.ToInt())), nil
		case TKLT:
			return Bool(b < Byte(rhs.ToInt())), nil
		case TKLE:
			return Bool(b <= Byte(rhs.ToInt())), nil
		case TKAmpersand:
			return b & Byte(rhs.ToInt()), nil
		case TKBar:
			return b | Byte(rhs.ToInt()), nil
		case TKHat:
			return b ^ Byte(rhs.ToInt()), nil
		case TKLShift:
			return b << Byte(rhs.ToInt()), nil
		case TKRShift:
			return b >> Byte(rhs.ToInt()), nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs.ToInt())
		}
	case *String:
		switch op {
		case TKMul:
			var builder strings.Builder
			for k := Byte(0); k < b; k++ {
				builder.WriteString(rhs.Value)
			}
			return &String{Value: builder.String()}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
		}
	case *List:
		switch op {
		case TKMul:
			xs := make([]Value, 0, Byte(len(rhs.Elements))*b)
			for k := Byte(0); k < b; k++ {
				xs = append(xs, rhs.Elements...)
			}
			return &List{Elements: xs}, nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
	}
}

func (b Byte) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKMinus:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], b)
	case TKAdd:
		return b, nil
	case TKTilde:
		return ^b, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], b)
	}
}

func (b Byte) IsIterable() bool {
	return false
}

func (b Byte) MakeIterator() Iterator {
	return nil
}

func (b Byte) IsHashable() bool {
	return true
}

func (b Byte) MakeHashKey() HashKey {
	return HashKey{
		Type:             b.TypeName(),
		ValueDescription: b.Description(),
	}
}

func (b Byte) IsValueSemantics() bool {
	return true
}

func (b Byte) HasMethods() bool {
	return false
}

func (b Byte) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, b)
}

func (b Byte) Clone() Value {
	return b
}

// Type Complex models complex numbers of 128 bits.
type Complex complex128

// Interface Value
func (z Complex) TypeName() string {
	return "Complex"
}

func (z Complex) Description() string {
	return fmt.Sprint(z)
}

func (z Complex) Equals(other Value) bool {
	if value, ok := other.(Complex); ok {
		return z == value
	}
	return false
}

func (z Complex) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case Complex:
		switch op {
		case TKAdd:
			return z + rhs, nil
		case TKMinus:
			return z - rhs, nil
		case TKMul:
			return z * rhs, nil
		case TKDiv:
			if rhs == 0 {
				return nil, DivisionByZeroError()
			}
			return z / rhs, nil
		case TKPower:
			result := cmplx.Pow(complex128(z), complex128(rhs))
			if cmplx.IsNaN(result) || cmplx.IsInf(result) {
				return nil, fmt.Errorf("%v to the power of %v = %v and it is not a numeric value", z, rhs, result)
			}
			return Complex(result), nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], z, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], z, rhs)
	}
}

func (z Complex) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKMinus:
		return -z, nil
	case TKAdd:
		return +z, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], z)
	}
}

func (z Complex) IsIterable() bool {
	return false
}

func (z Complex) MakeIterator() Iterator {
	return nil
}

func (z Complex) IsHashable() bool {
	return true
}

func (z Complex) MakeHashKey() HashKey {
	return HashKey{
		Type:             z.TypeName(),
		ValueDescription: z.Description(),
	}
}

func (z Complex) IsValueSemantics() bool {
	return true
}

func (z Complex) HasMethods() bool {
	return false
}

func (z Complex) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, z)
}

func (z Complex) Clone() Value {
	return z
}

// BInt models an arbitrary precision singned integer.
type BInt struct {
	Value *big.Int
}

// Interface IntegerNumber
func (b *BInt) ToInt() Int {
	if b.Value.IsInt64() || b.Value.IsUint64() {
		return Int(b.Value.Int64())
	}
	return 0
}

// Interface Value
func (b *BInt) TypeName() string {
	return "BInt"
}

func (b *BInt) Description() string {
	return b.Value.String()
}

func (b *BInt) Equals(other Value) bool {
	if value, ok := other.(*BInt); ok {
		return b.Value.Cmp(value.Value) == 0
	}
	return false
}

func (b *BInt) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case *BInt:
		switch op {
		case TKAdd:
			return &BInt{Value: new(big.Int).Add(b.Value, rhs.Value)}, nil
		case TKMinus:
			return &BInt{Value: new(big.Int).Sub(b.Value, rhs.Value)}, nil
		case TKMul:
			return &BInt{Value: new(big.Int).Mul(b.Value, rhs.Value)}, nil
		case TKDiv:
			bigZero := new(big.Int)
			if rhs.Value.Cmp(bigZero) == 0 {
				return nil, DivisionByZeroError()
			}
			return &BInt{Value: bigZero.Div(b.Value, rhs.Value)}, nil
		case TKMod:
			bigZero := new(big.Int)
			if rhs.Value.Cmp(bigZero) == 0 {
				return nil, DivisionByZeroError()
			}
			return &BInt{Value: bigZero.Mod(b.Value, rhs.Value)}, nil
		case TKPercent:
			bigZero := new(big.Int)
			if rhs.Value.Cmp(bigZero) == 0 {
				return nil, DivisionByZeroError()
			}
			return &BInt{Value: bigZero.Rem(b.Value, rhs.Value)}, nil
		case TKPower:
			return &BInt{Value: new(big.Int).Exp(b.Value, rhs.Value, nil)}, nil
		case TKGT:
			return Bool(b.Value.Cmp(rhs.Value) == 1), nil
		case TKGE:
			return Bool(b.Value.Cmp(rhs.Value) == 1 || b.Value.Cmp(rhs.Value) == 0), nil
		case TKLT:
			return Bool(b.Value.Cmp(rhs.Value) == -1), nil
		case TKLE:
			return Bool(b.Value.Cmp(rhs.Value) == -1 || b.Value.Cmp(rhs.Value) == 0), nil
		case TKAmpersand:
			return &BInt{Value: new(big.Int).And(b.Value, rhs.Value)}, nil
		case TKBar:
			return &BInt{Value: new(big.Int).Or(b.Value, rhs.Value)}, nil
		case TKHat:
			return &BInt{Value: new(big.Int).Xor(b.Value, rhs.Value)}, nil
		case TKLShift:
			if rhs.Value.Cmp(new(big.Int)) != -1 {
				if b.Value.IsInt64() {
					return &BInt{Value: new(big.Int).Lsh(b.Value, uint(rhs.Value.Int64()))}, nil
				} else if b.Value.IsUint64() {
					return &BInt{Value: new(big.Int).Lsh(b.Value, uint(rhs.Value.Uint64()))}, nil
				}
			}
			return nil, NegativeShiftError(KindDescription[op], b, rhs)
		case TKRShift:
			if rhs.Value.Cmp(new(big.Int)) != -1 {
				if b.Value.IsInt64() {
					return &BInt{Value: new(big.Int).Rsh(b.Value, uint(rhs.Value.Int64()))}, nil
				} else if b.Value.IsUint64() {
					return &BInt{Value: new(big.Int).Rsh(b.Value, uint(rhs.Value.Uint64()))}, nil
				}
			}
			return nil, NegativeShiftError(KindDescription[op], b, rhs)
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], b, rhs)
	}
}

func (b *BInt) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKMinus:
		return &BInt{Value: new(big.Int).Neg(b.Value)}, nil
	case TKAdd:
		return &BInt{Value: new(big.Int).Set(b.Value)}, nil
	case TKTilde:
		return &BInt{Value: new(big.Int).Set(b.Value).Not(b.Value)}, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], b)
	}
}

func (b *BInt) IsIterable() bool {
	return false
}

func (b *BInt) MakeIterator() Iterator {
	return nil
}

func (b *BInt) IsHashable() bool {
	return true
}

func (b *BInt) MakeHashKey() HashKey {
	return HashKey{
		Type:             b.TypeName(),
		ValueDescription: b.Description(),
	}
}

func (b *BInt) IsValueSemantics() bool {
	return true
}

func (b *BInt) HasMethods() bool {
	return false
}

func (b *BInt) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, b)
}

func (b *BInt) Clone() Value {
	return b
}

// Rational models an arbitrary precision rational number.
type Rational struct {
	Value *big.Rat
}

// Interface Value
func (rat *Rational) TypeName() string {
	return "Rational"
}

func (rat *Rational) Description() string {
	return rat.Value.RatString()
}

func (rat *Rational) Equals(other Value) bool {
	if value, ok := other.(*Rational); ok {
		return rat.Value.Cmp(value.Value) == 0
	}
	return false
}

func (rat *Rational) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case *Rational:
		switch op {
		case TKAdd:
			return &Rational{Value: new(big.Rat).Add(rat.Value, rhs.Value)}, nil
		case TKMinus:
			return &Rational{Value: new(big.Rat).Sub(rat.Value, rhs.Value)}, nil
		case TKMul:
			return &Rational{Value: new(big.Rat).Mul(rat.Value, rhs.Value)}, nil
		case TKDiv:
			bigZero := new(big.Rat)
			if rhs.Value.Cmp(bigZero) == 0 {
				return nil, DivisionByZeroError()
			}
			return &Rational{Value: bigZero.Quo(rat.Value, rhs.Value)}, nil
		case TKGT:
			return Bool(rat.Value.Cmp(rhs.Value) == 1), nil
		case TKGE:
			return Bool(rat.Value.Cmp(rhs.Value) == 1 || rat.Value.Cmp(rhs.Value) == 0), nil
		case TKLT:
			return Bool(rat.Value.Cmp(rhs.Value) == -1), nil
		case TKLE:
			return Bool(rat.Value.Cmp(rhs.Value) == -1 || rat.Value.Cmp(rhs.Value) == 0), nil
		default:
			return nil, TypeErrorInBinaryOperator(KindDescription[op], rat, rhs)
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], rat, rhs)
	}
}

func (rat *Rational) PrefixOp(op byte) (Value, error) {
	switch op {
	case TKMinus:
		return &Rational{Value: new(big.Rat).Neg(rat.Value)}, nil
	case TKAdd:
		return &Rational{Value: new(big.Rat).Set(rat.Value)}, nil
	default:
		return nil, TypeErrorInPrefixOperator(KindDescription[op], rat)
	}
}

func (rat *Rational) IsIterable() bool {
	return false
}

func (rat *Rational) MakeIterator() Iterator {
	return nil
}

func (rat *Rational) IsHashable() bool {
	return true
}

func (rat *Rational) MakeHashKey() HashKey {
	return HashKey{
		Type:             rat.TypeName(),
		ValueDescription: rat.Description(),
	}
}

func (rat *Rational) IsValueSemantics() bool {
	return true
}

func (rat *Rational) HasMethods() bool {
	return false
}

func (rat *Rational) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, rat)
}

func (rat *Rational) Clone() Value {
	return rat
}

// Rune models a unicode codepoint value.
type Rune rune

// Interface Value
func (r Rune) TypeName() string {
	return "Rune"
}

func (r Rune) Description() string {
	return string(r)
}

func (r Rune) Equals(other Value) bool {
	if value, ok := other.(Rune); ok {
		return r == value
	}
	return false
}

func (r Rune) BinaryOp(op byte, rhs Value) (Value, error) {
	switch rhs := rhs.(type) {
	case IntegerNumber:
		switch op {
		case TKAdd:
			result := r + Rune(rhs.ToInt())
			if utf8.ValidRune(rune(result)) {
				return result, nil
			}
			return nil, RuneOutOfRangeOrIllegal()
		case TKMinus:
			result := r - Rune(rhs.ToInt())
			if utf8.ValidRune(rune(result)) {
				return result, nil
			}
			return nil, RuneOutOfRangeOrIllegal()
		default:
		}
	case Rune:
		switch op {
		case TKAdd:
			result := r + rhs
			if utf8.ValidRune(rune(result)) {
				return result, nil
			}
			return nil, RuneOutOfRangeOrIllegal()
		case TKMinus:
			result := r - rhs
			if utf8.ValidRune(rune(result)) {
				return result, nil
			}
			return nil, RuneOutOfRangeOrIllegal()
		case TKGT:
			return Bool(r > rhs), nil
		case TKGE:
			return Bool(r >= rhs), nil
		case TKLT:
			return Bool(r < rhs), nil
		case TKLE:
			return Bool(r <= rhs), nil
		default:
		}
	default:
		return nil, TypeErrorInBinaryOperator(KindDescription[op], r, rhs)
	}
	return nil, TypeErrorInBinaryOperator(KindDescription[op], r, rhs)
}

func (r Rune) PrefixOp(op byte) (Value, error) {
	return nil, TypeErrorInPrefixOperator(KindDescription[op], r)
}

func (r Rune) IsIterable() bool {
	return false
}

func (r Rune) MakeIterator() Iterator {
	return nil
}

func (r Rune) IsHashable() bool {
	return true
}

func (r Rune) MakeHashKey() HashKey {
	return HashKey{
		Type:             r.TypeName(),
		ValueDescription: r.Description(),
	}
}

func (r Rune) IsValueSemantics() bool {
	return true
}

func (r Rune) HasMethods() bool {
	return false
}

func (r Rune) GetMethod(name string) (Value, bool, error) {
	return nil, false, MethodNotDefined(name, r)
}

func (r Rune) Clone() Value {
	return r
}
