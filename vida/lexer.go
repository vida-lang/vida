package vida

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Lexer a.k.a. lexical analyzer.
type Lexer struct {
	fileName string
	line     UInt32
	rune     rune
	err      error
	builder  strings.Builder
	input    *bytes.Buffer
}

// newLexer creates a new Lexer.
func newLexer(input *bytes.Buffer, fileName string) *Lexer {
	lexer := &Lexer{
		fileName: fileName,
		line:     1,
		input:    input}
	lexer.forward()
	return lexer
}

// nextToken performs lexical analysis on demand and returns the next token available or a boolean false when fails.
func (lexer *Lexer) nextToken() Token {
init:
	for unicode.IsSpace(lexer.rune) {
		if lexer.rune == '\n' {
			lexer.line++
		}
		lexer.forward()
	}

	if unicode.IsLetter(lexer.rune) || lexer.rune == '_' {
		for unicode.IsLetter(lexer.rune) || unicode.IsNumber(lexer.rune) || lexer.rune == '_' {
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
		}
		result := lexer.builder.String()
		lexer.builder.Reset()
		if kind, isKeyword := Keywords[result]; isKeyword {
			return Token{Type: kind, Line: lexer.line}
		}
		return Token{Type: TKIdentifier, Line: lexer.line, Description: result}
	}

	if unicode.IsDigit(lexer.rune) {
		return lexer.scanNumber()
	}

	symbol := lexer.scanSymbol()
	if symbol.Type != TKComment {
		return symbol
	}
	goto init
}

// Lexical analysis of comments.
func (lexer *Lexer) scanComment() {
	for lexer.rune != '\n' {
		lexer.forward()
	}
}

// Lexical analysis for multiline comments.
func (lexer *Lexer) scanMultilineComment(line UInt32) {
again:
	for lexer.rune != '*' && lexer.rune != 0 {
		if lexer.rune == '\n' {
			lexer.line++
		}
		lexer.forward()
	}
	if lexer.rune == 0 {
		unterminatedCommentError(lexer.fileName, lexer.line, line)
	} else if lexer.forward(); lexer.rune == '/' {
		lexer.forward()
		return
	}
	goto again
}

// Moves the lexer one token at a time.
func (lexer *Lexer) forward() {
	lexer.rune, _, lexer.err = lexer.input.ReadRune()
	if lexer.err != nil {
		lexer.rune = 0
	}
}

// Unreads the last rune read.
func (lexer *Lexer) backward(backup rune) {
	lexer.input.UnreadRune()
	lexer.rune = backup
}

// Error message for not terminated strings.
func unterminatedStringError(script string, line, initLine UInt32) {
	fmt.Printf("\n   Syntax Error\n   Unterminated string\n   %v:%3v\n\n", script, initLine)
	os.Exit(0)
}

// Error message for not terminated runes.
func unterminatedRuneError(script string, line, initLine UInt32) {
	fmt.Printf("\n   Syntax Error\n   Unterminated rune\n   %v:%3v\n\n", script, initLine)
	os.Exit(0)
}

// Error message for empty runes.
func emptyRuneError(script string, line, initLine UInt32) {
	fmt.Printf("\n   Syntax Error\n   empty rune\n   %v:%3v\n\n", script, initLine)
	os.Exit(0)
}

// Error message for illegal runes.
func illegalRuneError(script string, line, initLine UInt32) {
	fmt.Printf("\n   Syntax Error\n   illegal rune\n   %v:%3v\n\n", script, initLine)
	os.Exit(0)
}

// Error message for not terminated comments.
func unterminatedCommentError(script string, line UInt32, initLine UInt32) {
	fmt.Printf("\n   Syntax Error\n   Unterminated comment\n   %v:%3v\n\n", script, initLine)
	os.Exit(0)
}

// Lexical analysis for symbols.
func (lexer *Lexer) scanSymbol() Token {
	switch lexer.rune {
	case '{':
		lexer.forward()
		return Token{Type: TKLCurly, Line: lexer.line}
	case '}':
		lexer.forward()
		return Token{Type: TKRCurly, Line: lexer.line}
	case ',':
		lexer.forward()
		return Token{Type: TKComma, Line: lexer.line}
	case '=':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKEqual, Line: lexer.line}
		}
		return Token{Type: TKEquation, Line: lexer.line}
	case '(':
		lexer.forward()
		return Token{Type: TKLParen, Line: lexer.line}
	case ')':
		lexer.forward()
		return Token{Type: TKRParen, Line: lexer.line}
	case '[':
		lexer.forward()
		return Token{Type: TKLBracket, Line: lexer.line}
	case ']':
		lexer.forward()
		return Token{Type: TKRBracket, Line: lexer.line}
	case '.':
		lexer.forward()
		if lexer.rune == '.' {
			lexer.forward()
			if lexer.rune == '.' {
				lexer.forward()
				return Token{Type: TK3Dots, Line: lexer.line}
			}
			return Token{Type: TKUndefined, Line: lexer.line}
		}
		return Token{Type: TKDot, Line: lexer.line}
	case '"':
		lexer.forward()
		if lexer.rune == '"' {
			lexer.forward()
			return Token{Type: TKString, Line: lexer.line}
		}
		initialLine := lexer.line
	str:
		for lexer.rune != '"' && lexer.rune != 0 {
			if lexer.rune == '\\' {
				lexer.forward()
				switch lexer.rune {
				case 'n':
					lexer.builder.WriteRune(10)
				case 't':
					lexer.builder.WriteRune(9)
				case 'u', 'U':
					backup := lexer.rune
					lexer.forward()
					if lexer.rune == '{' {
						lexer.forward()
						if unicode.In(lexer.rune, unicode.Hex_Digit) {
							var hex []rune
							for unicode.In(lexer.rune, unicode.Hex_Digit) {
								hex = append(hex, lexer.rune)
								lexer.forward()
							}
							if lexer.rune == '}' {
								if i, err := strconv.ParseInt(string(hex), 16, 64); err == nil {
									result := rune(i)
									if utf8.ValidRune(result) {
										lexer.builder.WriteRune(result)
									} else {
										lexer.builder.WriteRune(92)
										lexer.builder.WriteRune(backup)
										lexer.builder.WriteRune('{')
										lexer.builder.WriteString(string(hex))
										lexer.builder.WriteRune('}')
									}
								} else {
									lexer.builder.WriteRune(92)
									lexer.builder.WriteRune(backup)
									lexer.builder.WriteRune('{')
									lexer.builder.WriteString(string(hex))
									lexer.builder.WriteRune('}')
								}
							} else {
								lexer.backward(hex[len(hex)-1])
								lexer.builder.WriteRune(92)
								lexer.builder.WriteRune(backup)
								lexer.builder.WriteRune('{')
								lexer.builder.WriteString(string(hex))
							}
						} else {
							lexer.backward('{')
							lexer.builder.WriteRune(92)
							lexer.builder.WriteRune(backup)
							lexer.builder.WriteRune('{')
						}
					} else {
						lexer.backward(backup)
						lexer.builder.WriteRune(92)
						lexer.builder.WriteRune(backup)
					}
				case '"':
					lexer.builder.WriteRune('"')
				case '\'':
					lexer.builder.WriteRune('\'')
				case 0:
					break str
				case '0':
					lexer.builder.WriteRune(0)
				case 'a':
					lexer.builder.WriteRune(7)
				case 'b':
					lexer.builder.WriteRune(8)
				case 'r':
					lexer.builder.WriteRune(13)
				case 'v':
					lexer.builder.WriteRune(11)
				case '\\':
					lexer.builder.WriteRune(92)
				default:
					lexer.backward(92)
					lexer.builder.WriteRune(92)
				}
				lexer.forward()
				continue
			}
			lexer.builder.WriteRune(lexer.rune)
			if lexer.rune == '\n' {
				lexer.line++
			}
			lexer.forward()
		}
		if lexer.rune == 0 {
			unterminatedStringError(lexer.fileName, lexer.line, initialLine)
		}
		strtok := Token{Type: TKString, Line: lexer.line, Description: lexer.builder.String()}
		lexer.builder.Reset()
		lexer.forward()
		return strtok
	case '+':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKAddAssign, Line: lexer.line}
		} else if unicode.IsDigit(lexer.rune) {
			lexer.builder.WriteRune('+')
			return lexer.scanNumber()
		}
		return Token{Type: TKAdd, Line: lexer.line}
	case '-':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKSubAssign, Line: lexer.line}
		} else if lexer.rune == '>' {
			lexer.forward()
			return Token{Type: TKRighArrow, Line: lexer.line}
		} else if unicode.IsDigit(lexer.rune) {
			lexer.builder.WriteRune('-')
			return lexer.scanNumber()
		}
		return Token{Type: TKMinus, Line: lexer.line}
	case '*':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKMulAssign, Line: lexer.line}
		} else if lexer.rune == '*' {
			lexer.forward()
			if lexer.rune == '=' {
				lexer.forward()
				return Token{Type: TKPowAssign, Line: lexer.line}
			}
			return Token{Type: TKPower, Line: lexer.line}
		}
		return Token{Type: TKMul, Line: lexer.line}
	case '/':
		lexer.forward()
		if lexer.rune == '/' {
			lexer.forward()
			lexer.scanComment()
			return Token{Type: TKComment, Line: lexer.line}
		} else if lexer.rune == '*' {
			lineBackup := lexer.line
			lexer.forward()
			lexer.scanMultilineComment(lineBackup)
			return Token{Type: TKComment, Line: lineBackup}
		} else if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKDivAssign, Line: lexer.line}
		}
		return Token{Type: TKDiv, Line: lexer.line}
	case '#':
		lexer.builder.WriteRune(lexer.rune)
		lexer.forward()
		if unicode.IsLetter(lexer.rune) || lexer.rune == '_' {
			for unicode.IsLetter(lexer.rune) || unicode.IsNumber(lexer.rune) || lexer.rune == '_' {
				lexer.builder.WriteRune(lexer.rune)
				lexer.forward()
			}
			result := lexer.builder.String()
			lexer.builder.Reset()
			return Token{Type: TKLabel, Line: lexer.line, Description: result}
		}
		return Token{Type: TKUndefined, Description: string('#'), Line: lexer.line}
	case '!':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKNEqual, Line: lexer.line}
		}
		return Token{Type: TKUndefined, Description: string('!'), Line: lexer.line}
	case '<':
		lexer.forward()
		if lexer.rune == '<' {
			lexer.forward()
			if lexer.rune == '=' {
				lexer.forward()
				return Token{Type: TKBitLShiftAssign, Line: lexer.line}
			}
			return Token{Type: TKLShift, Line: lexer.line}
		} else if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKLE, Line: lexer.line}
		}
		return Token{Type: TKLT, Line: lexer.line}
	case '>':
		lexer.forward()
		if lexer.rune == '>' {
			lexer.forward()
			if lexer.rune == '=' {
				lexer.forward()
				return Token{Type: TKBitRShiftAssign, Line: lexer.line}
			}
			return Token{Type: TKRShift, Line: lexer.line}
		} else if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKGE, Line: lexer.line}
		}
		return Token{Type: TKGT, Line: lexer.line}
	case ':':
		lexer.forward()
		return Token{Type: TKColon, Line: lexer.line}
	case '?':
		lexer.forward()
		return Token{Type: TKQuestion, Line: lexer.line}
	case '~':
		lexer.forward()
		return Token{Type: TKTilde, Line: lexer.line}
	case '&':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKBitAndAssign, Line: lexer.line}
		}
		return Token{Type: TKAmpersand, Line: lexer.line}
	case '|':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKBitOrAssign, Line: lexer.line}
		}
		return Token{Type: TKBar, Line: lexer.line}
	case '^':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKBitXorAssign, Line: lexer.line}
		}
		return Token{Type: TKHat, Line: lexer.line}
	case '%':
		lexer.forward()
		if lexer.rune == '=' {
			lexer.forward()
			return Token{Type: TKRemAssign, Line: lexer.line}
		}
		return Token{Type: TKPercent, Line: lexer.line}
	case '\'':
		initLine := lexer.line
		lexer.forward()
		if lexer.rune == '\'' {
			emptyRuneError(lexer.fileName, lexer.line, initLine)
		}
		if lexer.rune == '\\' {
			lexer.forward()
			switch lexer.rune {
			case 'n':
				lexer.builder.WriteRune(10)
			case 't':
				lexer.builder.WriteRune(9)
			case 'u', 'U':
				lexer.forward()
				if lexer.rune == '{' {
					lexer.forward()
					if unicode.In(lexer.rune, unicode.Hex_Digit) {
						var hex []rune
						for unicode.In(lexer.rune, unicode.Hex_Digit) {
							hex = append(hex, lexer.rune)
							lexer.forward()
						}
						if lexer.rune == '}' {
							if i, err := strconv.ParseInt(string(hex), 16, 64); err == nil {
								result := rune(i)
								if utf8.ValidRune(result) {
									lexer.builder.WriteRune(result)
								} else {
									illegalRuneError(lexer.fileName, lexer.line, initLine)
								}
							} else {
								illegalRuneError(lexer.fileName, lexer.line, initLine)
							}
						} else {
							illegalRuneError(lexer.fileName, lexer.line, initLine)
						}
					} else {
						illegalRuneError(lexer.fileName, lexer.line, initLine)
					}
				} else {
					illegalRuneError(lexer.fileName, lexer.line, initLine)
				}
			case '"':
				lexer.builder.WriteRune('"')
			case '\'':
				lexer.builder.WriteRune('\'')
			case '0':
				lexer.builder.WriteRune(0)
			case 'a':
				lexer.builder.WriteRune(7)
			case 'b':
				lexer.builder.WriteRune(8)
			case 'r':
				lexer.builder.WriteRune(13)
			case 'v':
				lexer.builder.WriteRune(11)
			case '\\':
				lexer.builder.WriteRune(92)
			default:
				lexer.backward(92)
				lexer.builder.WriteRune(92)
			}
			lexer.forward()
		} else {
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
		}
		if lexer.rune == 0 {
			unterminatedRuneError(lexer.fileName, lexer.line, initLine)
		}
		if lexer.rune != '\'' {
			illegalRuneError(lexer.fileName, lexer.line, initLine)
		}
		runetok := Token{Type: TKRune, Line: lexer.line, Description: lexer.builder.String()}
		lexer.builder.Reset()
		lexer.forward()
		return runetok
	case '`':
		lexer.forward()
		if lexer.rune == '`' {
			lexer.forward()
			return Token{Type: TKString, Line: lexer.line}
		}
		initialLine := lexer.line
		for lexer.rune != '`' && lexer.rune != 0 {
			lexer.builder.WriteRune(lexer.rune)
			if lexer.rune == '\n' {
				lexer.line++
			}
			lexer.forward()
		}
		if lexer.rune == 0 {
			unterminatedStringError(lexer.fileName, lexer.line, initialLine)
		}
		strtok := Token{Type: TKString, Line: lexer.line, Description: lexer.builder.String()}
		lexer.builder.Reset()
		lexer.forward()
		return strtok
	case 0:
		return Token{Type: TKEof, Line: lexer.line}
	default:
		return Token{Type: TKUndefined, Description: string(lexer.rune), Line: lexer.line}
	}
}

// Lexical analysis for big or unsigned numbers suffix.
func (lexer *Lexer) checkForBigOrUInt() Token {
	if lexer.rune == 'n' {
		lexer.forward()
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKBig, Description: result, Line: lexer.line}
	} else if lexer.rune == 'u' {
		lexer.forward()
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKUInt, Description: result, Line: lexer.line}
	}
	result := lexer.builder.String()
	lexer.builder.Reset()
	return Token{Type: TKInteger, Description: result, Line: lexer.line}
}

// Lexical analysis for exponents.
func (lexer *Lexer) scanExponent() Token {
	lexer.forward()
	if unicode.IsDigit(lexer.rune) {
		for unicode.IsDigit(lexer.rune) {
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
		}
		if lexer.rune == '+' || lexer.rune == '-' {
			lexer.builder.WriteRune(lexer.rune)
			return lexer.scanImaginary()
		}
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKFloat, Description: result, Line: lexer.line}
	} else if lexer.rune == '+' || lexer.rune == '-' {
		lexer.builder.WriteRune(lexer.rune)
		lexer.forward()
		if unicode.IsDigit(lexer.rune) {
			for unicode.IsDigit(lexer.rune) {
				lexer.builder.WriteRune(lexer.rune)
				lexer.forward()
			}
			if lexer.rune == '+' || lexer.rune == '-' {
				lexer.builder.WriteRune(lexer.rune)
				return lexer.scanImaginary()
			}
			result := lexer.builder.String()
			lexer.builder.Reset()
			return Token{Type: TKFloat, Description: result, Line: lexer.line}
		}
	}
	return Token{Type: TKUndefined, Description: lexer.builder.String(), Line: lexer.line}
}

// Lexical analysis for complex exponets.
func (lexer *Lexer) scanComplexExponent() Token {
	lexer.forward()
	if unicode.IsDigit(lexer.rune) {
		for unicode.IsDigit(lexer.rune) {
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
		}
		if lexer.rune == 'i' || lexer.rune == 'j' {
			lexer.builder.WriteRune('i')
			lexer.forward()
			result := lexer.builder.String()
			lexer.builder.Reset()
			return Token{Type: TKComplex, Description: result, Line: lexer.line}
		}
	} else if lexer.rune == '+' || lexer.rune == '-' {
		lexer.builder.WriteRune(lexer.rune)
		lexer.forward()
		if unicode.IsDigit(lexer.rune) {
			for unicode.IsDigit(lexer.rune) {
				lexer.builder.WriteRune(lexer.rune)
				lexer.forward()
			}
			if lexer.rune == 'i' || lexer.rune == 'j' {
				lexer.builder.WriteRune('i')
				lexer.forward()
				result := lexer.builder.String()
				lexer.builder.Reset()
				return Token{Type: TKComplex, Description: result, Line: lexer.line}
			}
		}
	}
	return Token{Type: TKUndefined, Description: lexer.builder.String(), Line: lexer.line}
}

// Lexical analysis for imaginary part of complex numbers.
func (lexer *Lexer) scanImaginary() Token {
	lexer.forward()
	if unicode.IsDigit(lexer.rune) {
		for unicode.IsDigit(lexer.rune) {
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
		}
		switch lexer.rune {
		case 'i', 'j':
			lexer.builder.WriteRune('i')
			lexer.forward()
			result := lexer.builder.String()
			lexer.builder.Reset()
			return Token{Type: TKComplex, Description: result, Line: lexer.line}
		case '.':
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
			if unicode.IsDigit(lexer.rune) {
				for unicode.IsDigit(lexer.rune) {
					lexer.builder.WriteRune(lexer.rune)
					lexer.forward()
				}
				if lexer.rune == 'i' || lexer.rune == 'j' {
					lexer.builder.WriteRune('i')
					lexer.forward()
					result := lexer.builder.String()
					lexer.builder.Reset()
					return Token{Type: TKComplex, Description: result, Line: lexer.line}
				}
				if lexer.rune == 'e' || lexer.rune == 'E' {
					lexer.builder.WriteRune(lexer.rune)
					return lexer.scanComplexExponent()
				}
			}
		case 'e', 'E':
			lexer.builder.WriteRune(lexer.rune)
			return lexer.scanComplexExponent()
		}
	}
	return Token{Type: TKUndefined, Description: lexer.builder.String(), Line: lexer.line}
}

// Lexical analysis for numbers,
func (lexer *Lexer) scanFromDigit() Token {
	for unicode.IsDigit(lexer.rune) || lexer.rune == '_' {
		lexer.builder.WriteRune(lexer.rune)
		lexer.forward()
	}
	switch lexer.rune {
	case 'n':
		lexer.forward()
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKBig, Description: result, Line: lexer.line}
	case 'f':
		lexer.forward()
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKFloat, Description: result, Line: lexer.line}
	case 'i', 'j':
		lexer.builder.WriteRune('i')
		lexer.forward()
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKComplex, Description: result, Line: lexer.line}
	case 'c':
		lexer.forward()
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKComplex, Description: result, Line: lexer.line}
	case '.':
		backup := lexer.rune
		lexer.forward()
		if lexer.rune == '.' {
			lexer.input.UnreadRune()
			result := lexer.builder.String()
			lexer.builder.Reset()
			return Token{Type: TKInteger, Description: result, Line: lexer.line}
		}
		if unicode.IsDigit(lexer.rune) {
			lexer.builder.WriteRune(backup)
			for unicode.IsDigit(lexer.rune) {
				lexer.builder.WriteRune(lexer.rune)
				lexer.forward()
			}
			if lexer.rune == 'e' || lexer.rune == 'E' {
				lexer.builder.WriteRune(lexer.rune)
				return lexer.scanExponent()
			}
			if lexer.rune == '+' || lexer.rune == '-' {
				lexer.builder.WriteRune(lexer.rune)
				return lexer.scanImaginary()
			}
			result := lexer.builder.String()
			lexer.builder.Reset()
			return Token{Type: TKFloat, Description: result, Line: lexer.line}
		}
		lexer.backward(backup)
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKInteger, Description: result, Line: lexer.line}
	case 'e', 'E':
		lexer.builder.WriteRune(lexer.rune)
		return lexer.scanExponent()
	case '/':
		lexer.builder.WriteRune(lexer.rune)
		lexer.forward()
		if unicode.IsDigit(lexer.rune) {
			for unicode.IsDigit(lexer.rune) {
				lexer.builder.WriteRune(lexer.rune)
				lexer.forward()
			}
			result := lexer.builder.String()
			lexer.builder.Reset()
			return Token{Type: TKRational, Description: result, Line: lexer.line}
		}
		return Token{Type: TKUndefined, Description: lexer.builder.String(), Line: lexer.line}
	case '+', '-':
		lexer.builder.WriteRune(lexer.rune)
		return lexer.scanImaginary()
	case 'u':
		lexer.forward()
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKUInt, Description: result, Line: lexer.line}
	default:
		result := lexer.builder.String()
		lexer.builder.Reset()
		return Token{Type: TKInteger, Description: result, Line: lexer.line}
	}
}

// DFA for recognize five types of numbers.
// Integer, Big, Double, Rational and Complex numbers.
func (lexer *Lexer) scanNumber() Token {
	if lexer.rune == '0' {
		lexer.builder.WriteRune(lexer.rune)
		lexer.forward()
		backup := lexer.rune
		if lexer.rune == 'x' {
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
			if unicode.In(lexer.rune, unicode.Hex_Digit) || lexer.rune == '_' {
				for unicode.In(lexer.rune, unicode.Hex_Digit) || lexer.rune == '_' {
					lexer.builder.WriteRune(lexer.rune)
					lexer.forward()
				}
				return lexer.checkForBigOrUInt()
			}
			lexer.backward(backup)
			lexer.builder.Reset()
			return Token{Type: TKInteger, Description: "0", Line: lexer.line}
		} else if lexer.rune == 'b' {
			lexer.builder.WriteRune(lexer.rune)
			lexer.forward()
			if lexer.rune == '0' || lexer.rune == '1' || lexer.rune == '_' {
				for lexer.rune == '0' || lexer.rune == '1' || lexer.rune == '_' {
					lexer.builder.WriteRune(lexer.rune)
					lexer.forward()
				}
				return lexer.checkForBigOrUInt()
			}
			lexer.backward(backup)
			lexer.builder.Reset()
			return Token{Type: TKInteger, Description: "0", Line: lexer.line}
		}
	}
	return lexer.scanFromDigit()
}
