package vida

import "fmt"

// Numeric constants for the definition of the token types
const (
	TKEof byte = iota
	TKComment
	TKLet
	TKConst
	TKIf
	TKElse
	TKFor
	TKIn
	TKRange
	TKBreak
	TKContinue
	TKSwitch
	TKCase
	TKDefault
	TKLabel
	TKFunction
	TKReturn
	TKIdentifier
	TKEquation
	TKInteger
	TKUInt
	TKBig
	TKFloat
	TKRational
	TKComplex
	TKTrue
	TKFalse
	TKNil
	TKRune
	TKString
	TK3Dots
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
	TKEqual
	TKNEqual
	TKGT
	TKGE
	TKLT
	TKLE
	TKAnd
	TKOr
	TKNot
	TKTilde
	TKComma
	TKColon
	TKDot
	TKLParen
	TKRParen
	TKLBracket
	TKRBracket
	TKLCurly
	TKRCurly
	TKQuestion
	TKRighArrow
	TKStruct
	TKPublic
	TKExtension
	TKDefer
	TKAddAssign
	TKSubAssign
	TKMulAssign
	TKDivAssign
	TKRemAssign
	TKPowAssign
	TKBitAndAssign
	TKBitOrAssign
	TKBitXorAssign
	TKBitLShiftAssign
	TKBitRShiftAssign
	TKUndefined
)

// KindDescription is the string representation of a token type.
var KindDescription = [...]string{
	TKEof:             "EOF",
	TKComment:         "Comment",
	TKLet:             "Let",
	TKConst:           "Const",
	TKIf:              "If",
	TKElse:            "Else",
	TKFor:             "For",
	TKIn:              "In",
	TKRange:           "Range",
	TKBreak:           "Break",
	TKContinue:        "Continue",
	TKSwitch:          "Switch",
	TKCase:            "Case",
	TKDefault:         "Default",
	TKLabel:           "Label",
	TKFunction:        "Function",
	TKReturn:          "Return",
	TKIdentifier:      "Identifier",
	TKEquation:        "=",
	TKInteger:         "Int",
	TKUInt:            "UInt",
	TKBig:             "Big",
	TKFloat:           "Float",
	TKRational:        "Rational",
	TKComplex:         "Complex",
	TKTrue:            "True",
	TKFalse:           "False",
	TKNil:             "Nil",
	TKRune:            "Rune",
	TKString:          "String",
	TK3Dots:           "...",
	TKAdd:             "+",
	TKMinus:           "-",
	TKMul:             "*",
	TKDiv:             "/",
	TKMod:             "mod",
	TKPercent:         "%",
	TKPower:           "**",
	TKAmpersand:       "&",
	TKBar:             "|",
	TKHat:             "^",
	TKLShift:          "<<",
	TKRShift:          ">>",
	TKEqual:           "==",
	TKNEqual:          "!=",
	TKGT:              ">",
	TKGE:              ">=",
	TKLT:              "<",
	TKLE:              "<=",
	TKAnd:             "and",
	TKOr:              "or",
	TKNot:             "not",
	TKTilde:           "~",
	TKComma:           ",",
	TKColon:           ":",
	TKDot:             ".",
	TKLParen:          "(",
	TKRParen:          ")",
	TKLBracket:        "[",
	TKRBracket:        "]",
	TKLCurly:          "{",
	TKRCurly:          "}",
	TKQuestion:        "?",
	TKRighArrow:       "->",
	TKStruct:          "Struct",
	TKPublic:          "Public",
	TKExtension:       "Extension",
	TKDefer:           "Defer",
	TKAddAssign:       "+=",
	TKSubAssign:       "-=",
	TKMulAssign:       "*=",
	TKDivAssign:       "/=",
	TKRemAssign:       "%=",
	TKPowAssign:       "**=",
	TKBitAndAssign:    "&=",
	TKBitOrAssign:     "|=",
	TKBitXorAssign:    "^=",
	TKBitLShiftAssign: "<<=",
	TKBitRShiftAssign: ">>=",
	TKUndefined:       "Undefined",
}

// Keywords is a map between strings -> Kind.
var Keywords = map[string]byte{
	"and":       TKAnd,
	"break":     TKBreak,
	"case":      TKCase,
	"const":     TKConst,
	"continue":  TKContinue,
	"default":   TKDefault,
	"defer":     TKDefer,
	"else":      TKElse,
	"extension": TKExtension,
	"false":     TKFalse,
	"for":       TKFor,
	"fun":       TKFunction,
	"if":        TKIf,
	"in":        TKIn,
	"let":       TKLet,
	"mod":       TKMod,
	"nil":       TKNil,
	"not":       TKNot,
	"or":        TKOr,
	"pub":       TKPublic,
	"range":     TKRange,
	"return":    TKReturn,
	"struct":    TKStruct,
	"switch":    TKSwitch,
	"true":      TKTrue,
}

// Token represents the lexical unit of the language.
type Token struct {
	Type        byte
	Description string
	Line        UInt32
}

// String representation of a Token.
func (tok *Token) String() string {
	return fmt.Sprintf("   %3v  -  %v %v", tok.Line, KindDescription[tok.Type], tok.Description)
}
