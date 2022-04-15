package vida

import "fmt"

// Numeric constants for the definition of the token types
const (
	tokEOF byte = iota
	tokComment
	tokLet
	tokConst
	tokIf
	tokElse
	tokFor
	tokIn
	tokRange
	tokBreak
	tokContinue
	tokSwitch
	tokCase
	tokDefault
	tokLabel
	tokFunction
	tokReturn
	tokIdentifier
	tokEquation
	tokInteger
	tokUInt
	tokBig
	tokFloat
	tokRational
	tokComplex
	tokTrue
	tokFalse
	tokNil
	tokRune
	tokString
	tok3Dots
	TKAdd
	TKMinus
	TKMul
	TKDiv
	TKMod
	TKPercent
	TKPower
	TKAmpersand
	TKBar
	TKHat
	TKLShift
	TKRShift
	tokEqual
	tokNEqual
	TKGT
	TKGE
	TKLT
	TKLE
	TKAnd
	TKOr
	TKNot
	TKTilde
	tokComma
	tokColon
	tokDot
	tokLParen
	tokRParen
	tokLBracket
	tokRBracket
	tokLCurly
	tokRCurly
	tokQuestion
	tokRArrow
	tokStruct
	tokPublic
	tokExtension
	tokDefer
	tokAddAssign
	tokSubAssign
	tokMulAssign
	tokDivAssign
	tokRemAssign
	tokPowAssign
	tokBitAndAssign
	tokBitOrAssign
	tokBitXorAssign
	tokBitLShiftAssign
	tokBitRShiftAssign
	tokNotDefined
)

// KindDescription is the string representation of a token type.
var KindDescription = [...]string{
	tokEOF:             "EOF",
	tokComment:         "Comment",
	tokLet:             "Let",
	tokConst:           "Const",
	tokIf:              "If",
	tokElse:            "Else",
	tokFor:             "For",
	tokIn:              "In",
	tokRange:           "Range",
	tokBreak:           "Break",
	tokContinue:        "Continue",
	tokSwitch:          "Switch",
	tokCase:            "Case",
	tokDefault:         "Default",
	tokLabel:           "Label",
	tokFunction:        "Function",
	tokReturn:          "Return",
	tokIdentifier:      "Identifier",
	tokEquation:        "=",
	tokInteger:         "Int",
	tokUInt:            "UInt",
	tokBig:             "Big",
	tokFloat:           "Float",
	tokRational:        "Rational",
	tokComplex:         "Complex",
	tokTrue:            "True",
	tokFalse:           "False",
	tokNil:             "Nil",
	tokRune:            "Rune",
	tokString:          "String",
	tok3Dots:           "...",
	TKAdd:              "+",
	TKMinus:            "-",
	TKMul:              "*",
	TKDiv:              "/",
	TKMod:              "mod",
	TKPercent:          "%",
	TKPower:            "**",
	TKAmpersand:        "&",
	TKBar:              "|",
	TKHat:              "^",
	TKLShift:           "<<",
	TKRShift:           ">>",
	tokEqual:           "==",
	tokNEqual:          "!=",
	TKGT:               ">",
	TKGE:               ">=",
	TKLT:               "<",
	TKLE:               "<=",
	TKAnd:              "and",
	TKOr:               "or",
	TKNot:              "not",
	TKTilde:            "~",
	tokComma:           ",",
	tokColon:           ":",
	tokDot:             ".",
	tokLParen:          "(",
	tokRParen:          ")",
	tokLBracket:        "[",
	tokRBracket:        "]",
	tokLCurly:          "{",
	tokRCurly:          "}",
	tokQuestion:        "?",
	tokRArrow:          "->",
	tokStruct:          "Struct",
	tokPublic:          "Public",
	tokExtension:       "Extension",
	tokDefer:           "Defer",
	tokAddAssign:       "+=",
	tokSubAssign:       "-=",
	tokMulAssign:       "*=",
	tokDivAssign:       "/=",
	tokRemAssign:       "%=",
	tokPowAssign:       "**=",
	tokBitAndAssign:    "&=",
	tokBitOrAssign:     "|=",
	tokBitXorAssign:    "^=",
	tokBitLShiftAssign: "<<=",
	tokBitRShiftAssign: ">>=",
	tokNotDefined:      "NotDefined",
}

// keywords is a map between strings -> Kind.
var keywords = map[string]byte{
	"and":       TKAnd,
	"break":     tokBreak,
	"case":      tokCase,
	"const":     tokConst,
	"continue":  tokContinue,
	"default":   tokDefault,
	"defer":     tokDefer,
	"else":      tokElse,
	"extension": tokExtension,
	"false":     tokFalse,
	"for":       tokFor,
	"fun":       tokFunction,
	"if":        tokIf,
	"in":        tokIn,
	"let":       tokLet,
	"mod":       TKMod,
	"nil":       tokNil,
	"not":       TKNot,
	"or":        TKOr,
	"pub":       tokPublic,
	"range":     tokRange,
	"return":    tokReturn,
	"struct":    tokStruct,
	"switch":    tokSwitch,
	"true":      tokTrue,
}

// token represents the lexical unit of the language.
type token struct {
	Type        byte
	Description string
	Line        UInt32
}

// String representation of a Token.
func (tok token) String() string {
	return fmt.Sprintf("   %3v  -  %v %v", tok.Line, KindDescription[tok.Type], tok.Description)
}
