package vida

import (
	"bytes"
	"fmt"
	"math/big"
	"os"
	"strconv"
	"unicode/utf8"
)

type assignmentHelper struct {
	tokens           []Token
	expressionsCount int
	isCollection     []bool
	operatorType     map[int]byte
	indexType        map[int]UInt32
}

func (helper *assignmentHelper) reset() {
	helper.tokens = helper.tokens[:0]
	helper.expressionsCount = 0
	helper.isCollection = helper.isCollection[:0]
}

type loopInfo struct {
	scopeDepth  Int32
	label       string
	loopAddress Bytecode
	breaks      []int
	isRangeLoop bool
}

var parseRules = createRules()

var assHelper []assignmentHelper

var jumpHelper []int

var patternHelper []int

var loopHelper []loopInfo

// This global variable is for lambda and anonumous functions count.
var lambdaID = uint64(0)

// Local models local variables.
type Local struct {
	identifier string
	depth      Int32
}

// CompFreeVar represents a local or global value in some enclosing environment at compile time.
type CompFreeVar struct {
	index   UInt32
	isLocal bool
}

// ParseRule is the struct for parsing expressions.
type ParseRule struct {
	prefix     ParseFn
	infix      ParseFn
	precedence byte
}

// ParseFn is the generic function for parsing expressions.
type ParseFn func(compiler *Compiler, precedence byte)

// Operator Precedences
const (
	PrecedenceNone           byte = iota
	PrecedenceNilChoice           // ?
	PrecedenceOr                  // or
	PrecedenceAnd                 // and
	PrecedenceEquality            // == !=
	PrecedenceRelational          // > >= < <=
	PrecedenceRange               // range
	PrecedenceAdditive            // + -
	PrecedenceMultiplicative      // * / mod %
	PrecedenceBitOr               // |
	PrecedenceBitXor              // ^
	PrecedenceBitAnd              // &
	PrecedenceShift               // << >>
	PrecedenceUnary               // - not ~ try : Right to Left Associativiy
	PrecedenceExp                 // ** Right to Left Associativiy
	PrecedencePosfix              // [] . ()
	PrecedencePrimary             //
)

// Compiler is the structure for saving partial state while performing syntax analysis and emitting machine code.
type Compiler struct {
	parent        *Compiler
	lexer         *Lexer
	token         Token
	next          Token
	rules         []ParseRule
	function      Function
	kIndex        Bytecode
	gIndex        Bytecode
	gIDPos        map[string]Bytecode
	gID           []string
	locals        []Local
	scopeDepth    Int32
	freeVariables []CompFreeVar
	canAccess     bool
	hasDefer      bool
}

func newCompiler(lexer *Lexer, rules []ParseRule) *Compiler {
	return &Compiler{
		lexer:    lexer,
		rules:    rules,
		function: Function{Lines: make(map[Bytecode]UInt32), ModuleName: lexer.fileName},
		gIDPos:   make(map[string]Bytecode),
	}
}

func newChildCompiler(parent *Compiler) *Compiler {
	return &Compiler{
		parent:    parent,
		lexer:     parent.lexer,
		token:     parent.token,
		next:      parent.next,
		rules:     parent.rules,
		function:  Function{Lines: make(map[Bytecode]UInt32), ModuleName: parent.lexer.fileName},
		gIndex:    parent.gIndex,
		gIDPos:    parent.gIDPos,
		gID:       parent.gID,
		canAccess: parent.canAccess,
	}
}

func updateCompiler(parent, child *Compiler) {
	parent.gIndex = child.gIndex
	parent.gIDPos = child.gIDPos
	parent.gID = child.gID
	parent.token = child.token
	parent.next = child.next
	child.parent = nil
	child.lexer = nil
	child.rules = nil
	child.locals = nil
}

// BuildModule builds a module. A module is the code contained in one file.
func BuildModule(input *bytes.Buffer, fileName string) *VModule {
	compiler := newCompiler(newLexer(input, fileName), parseRules)
	compiler.function.Name = mainFunctionName
	forward(compiler)
	forward(compiler)
	for compiler.token.Type != TKEof {
		switch compiler.token.Type {
		case TKLet:
			compileDeclaration(compiler)
		case TKIdentifier:
			compileAssignment(compiler)
		case TKStruct:
			compileStruct(compiler)
		case TKFunction:
			compileClosure(compiler)
		case TKConst:
			compileConstNamespace(compiler)
		case TKExtension:
			compileExtension(compiler)
		case TKIf:
			compileIf(compiler, false)
		case TKFor:
			compileFor(compiler)
		case TKLabel:
			switch compiler.next.Type {
			case TKFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(compiler, false)
		case TKDefer:
			compileDefer(compiler)
		case TKLCurly:
			compileBlock(compiler, false)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	emitEndScript(compiler, compiler.token.Line)
	cleanUp(compiler)
	return &VModule{path: fileName, mainFunction: Closure{Function: &compiler.function}, identifiers: compiler.gID, namespace: make(Namespace)}
}

// This function compailes add local and global variable definitions.
func compileDeclaration(compiler *Compiler) {
	line := compiler.token.Line
	check(compiler, TKLet)
	forward(compiler)
	assHelperOffset := len(assHelper)
	assHelper = append(assHelper, assignmentHelper{})
	check(compiler, TKIdentifier)
	assHelper[assHelperOffset].tokens = append(assHelper[assHelperOffset].tokens, compiler.token)
	forward(compiler)
	for compiler.token.Type == TKComma {
		forward(compiler)
		check(compiler, TKIdentifier)
		assHelper[assHelperOffset].tokens = append(assHelper[assHelperOffset].tokens, compiler.token)
		forward(compiler)
	}
	check(compiler, TKEquation)
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	assHelper[assHelperOffset].expressionsCount++
	for compiler.token.Type == TKComma {
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		assHelper[assHelperOffset].expressionsCount++
	}
	tokensCount := len(assHelper[assHelperOffset].tokens)
	if assHelper[assHelperOffset].expressionsCount > maxRhsExpr || tokensCount > maxRhsExpr {
		maxRHSExpressionsError(compiler.lexer.fileName)
	}
	if tokensCount > assHelper[assHelperOffset].expressionsCount && assHelper[assHelperOffset].expressionsCount == 1 {
		if compiler.scopeDepth == 0 {
			emitUnpack(compiler, Bytecode(tokensCount), line)
			declareNewGlobals(compiler, assHelperOffset, tokensCount, line)
		} else {
			emitUnpack(compiler, Bytecode(tokensCount), line)
			declareNewLocals(compiler, assHelperOffset, tokensCount)
		}
	} else {
		checkSymmetry(tokensCount, assHelper[assHelperOffset].expressionsCount, line, compiler.lexer.fileName)
		if compiler.scopeDepth == 0 {
			declareNewGlobals(compiler, assHelperOffset, tokensCount, line)
		} else {
			declareNewLocals(compiler, assHelperOffset, tokensCount)
		}
	}
	assHelper[assHelperOffset].reset()
	assHelper = assHelper[:assHelperOffset]
}

// Two helper functions for variable definition compilation.
// Declare new locals
func declareNewLocals(compiler *Compiler, assHelperOffset, tokensCount int) {
	for i := 0; i < tokensCount; i++ {
		addLocal(compiler, assHelper[assHelperOffset].tokens[i])
	}
}

// Declare new globals.
func declareNewGlobals(compiler *Compiler, assHelperOffset, tokensCount int, line UInt32) {
	for j := tokensCount - 1; j >= 0; j-- {
		if pos, exists := compiler.gIDPos[assHelper[assHelperOffset].tokens[j].Description]; exists {
			emitNewGlobal(compiler, pos, line)
		} else {
			compiler.gIDPos[assHelper[assHelperOffset].tokens[j].Description] = compiler.gIndex
			compiler.gID = append(compiler.gID, assHelper[assHelperOffset].tokens[j].Description)
			emitNewGlobal(compiler, compiler.gIndex, line)
			compiler.gIndex++
		}
	}
}

// This function compiles all global and local assignments.
func compileAssignment(compiler *Compiler) {
	assHelperOffset := len(assHelper)
	assHelper = append(assHelper, assignmentHelper{operatorType: map[int]byte{}, indexType: map[int]uint32{}})
	check(compiler, TKIdentifier)
	if compiler.next.Type == TKLBracket || compiler.next.Type == TKDot || compiler.next.Type == TKLParen {
		line := compiler.token.Line
		compileExpression(compiler, PrecedenceNone)
		if compiler.token.Type == TKRParen {
			forward(compiler)
			emitPop(compiler, 1, line)
			return
		}
		operatorType := compiler.token.Type
		forward(compiler)
		lastOpcodeIndex := len(compiler.function.Code) - 1
		lineKey := compiler.function.Code[lastOpcodeIndex]
		assHelper[assHelperOffset].isCollection = append(assHelper[assHelperOffset].isCollection, true)
		assHelper[assHelperOffset].operatorType[len(assHelper[assHelperOffset].isCollection)-1] = operatorType
		assHelper[assHelperOffset].indexType[len(assHelper[assHelperOffset].isCollection)-1] = compiler.function.Code[lastOpcodeIndex] >> instructionShift
		compiler.function.Code = compiler.function.Code[:lastOpcodeIndex]
		delete(compiler.function.Lines, lineKey)
	} else {
		assHelper[assHelperOffset].tokens = append(assHelper[assHelperOffset].tokens, compiler.token)
		assHelper[assHelperOffset].isCollection = append(assHelper[assHelperOffset].isCollection, false)
		forward(compiler)
	}
	for compiler.token.Type == TKComma {
		forward(compiler)
		check(compiler, TKIdentifier)
		if compiler.next.Type == TKLBracket || compiler.next.Type == TKDot || compiler.next.Type == TKLParen {
			compileExpression(compiler, PrecedenceNone)
			if compiler.token.Type == TKRParen {
				compilerPrintError("subscription [expr] or selection (.)", compiler.token, compiler.lexer.fileName)
			}
			operatorType := compiler.token.Type
			forward(compiler)
			lastOpcodeIndex := len(compiler.function.Code) - 1
			lineKey := compiler.function.Code[lastOpcodeIndex]
			assHelper[assHelperOffset].isCollection = append(assHelper[assHelperOffset].isCollection, true)
			assHelper[assHelperOffset].operatorType[len(assHelper[assHelperOffset].isCollection)-1] = operatorType
			assHelper[assHelperOffset].indexType[len(assHelper[assHelperOffset].isCollection)-1] = compiler.function.Code[lastOpcodeIndex] >> instructionShift
			compiler.function.Code = compiler.function.Code[:lastOpcodeIndex]
			delete(compiler.function.Lines, lineKey)
		} else {
			assHelper[assHelperOffset].tokens = append(assHelper[assHelperOffset].tokens, compiler.token)
			assHelper[assHelperOffset].isCollection = append(assHelper[assHelperOffset].isCollection, false)
			forward(compiler)
		}
	}
	// Assignment statements switch. Here the compilations takes two possible paths depending on the assignment oparator: simple (-) or operational (<op>=).
	switch compiler.token.Type {
	case TKEquation:
		compileSimpleAssignment(compiler, compiler.token.Line, assHelperOffset)
	case TKAddAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKAdd)
	case TKSubAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKMinus)
	case TKMulAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKMul)
	case TKDivAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKDiv)
	case TKRemAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKPercent)
	case TKPowAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKPower)
	case TKBitAndAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKAmpersand)
	case TKBitOrAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKBar)
	case TKBitXorAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKHat)
	case TKBitLShiftAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKLShift)
	case TKBitRShiftAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKRShift)
	default:
		compilerPrintError("Assignment", compiler.token, compiler.lexer.fileName)
	}
}

// This function compiles simple assignments. A simple assignment has the form <idLst> = <exprList>
func compileSimpleAssignment(compiler *Compiler, line Bytecode, assHelperOffset int) {
	check(compiler, TKEquation)
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	assHelper[assHelperOffset].expressionsCount++
	for compiler.token.Type == TKComma {
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		assHelper[assHelperOffset].expressionsCount++
	}
	lvaluesCount, tokensIndex := len(assHelper[assHelperOffset].isCollection), len(assHelper[assHelperOffset].tokens)-1
	if assHelper[assHelperOffset].expressionsCount > maxRhsExpr || lvaluesCount > maxRhsExpr {
		maxRHSExpressionsError(compiler.lexer.fileName)
	}
	if lvaluesCount > assHelper[assHelperOffset].expressionsCount && assHelper[assHelperOffset].expressionsCount == 1 {
		if compiler.scopeDepth == 0 {
			emitUnpack(compiler, Bytecode(lvaluesCount), line)
			emitSimpleAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, line)
		} else {
			emitUnpack(compiler, Bytecode(lvaluesCount), line)
			emitSimpleLocalAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, line)
		}
	} else {
		checkSymmetry(lvaluesCount, assHelper[assHelperOffset].expressionsCount, line, compiler.lexer.fileName)
		if compiler.scopeDepth == 0 {
			emitSimpleAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, line)
		} else {
			emitSimpleLocalAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, line)
		}
	}
	assHelper[assHelperOffset].reset()
	assHelper = assHelper[:assHelperOffset]
}

// It follows three helper functions for simple assignment.

func emitSimpleAssignment(compiler *Compiler, lvaluesCount, assHelperOffset, tokensIndex int, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == TKRBracket {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], MutatingOperatorSubscript, line)
			} else {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], MutatingOperatorSelector, line)
			}
		} else {
			emitGlobalAssignment(compiler, assHelperOffset, tokensIndex, line)
			tokensIndex--
		}
	}
}

func emitSimpleLocalAssignment(compiler *Compiler, lvaluesCount, assHelperOffset, tokensIndex int, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == TKRBracket {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], MutatingOperatorSubscript, line)
			} else {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], MutatingOperatorSelector, line)
			}
		} else {
			index := resolveLocal(compiler, assHelper[assHelperOffset].tokens[tokensIndex].Description)
			if index == -1 {
				if index := resolveFreeVar(compiler, assHelper[assHelperOffset].tokens[tokensIndex].Description, line); index != -1 {
					emitSetFreeVar(compiler, Bytecode(index), line)
				} else {
					emitGlobalAssignment(compiler, assHelperOffset, tokensIndex, line)
				}
			} else {
				emitSetLocal(compiler, Bytecode(index), line)
			}
			tokensIndex--
		}
	}
}

func emitGlobalAssignment(compiler *Compiler, assHelperOffset, tokensIndex int, line UInt32) {
	if pos, exists := compiler.gIDPos[assHelper[assHelperOffset].tokens[tokensIndex].Description]; exists {
		emitSetGlobal(compiler, pos, line)
	} else {
		compiler.gIDPos[assHelper[assHelperOffset].tokens[tokensIndex].Description] = compiler.gIndex
		compiler.gID = append(compiler.gID, assHelper[assHelperOffset].tokens[tokensIndex].Description)
		emitSetGlobal(compiler, compiler.gIndex, line)
		compiler.gIndex++
	}
}

// This function compiles operational assignments. An operational assignment has the form <idLst> <op>= <exprList>
func compileCompoundAssignment(compiler *Compiler, line Bytecode, assHelperOffset int, operatorType, operator byte) {
	check(compiler, operatorType)
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	assHelper[assHelperOffset].expressionsCount++
	for compiler.token.Type == TKComma {
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		assHelper[assHelperOffset].expressionsCount++
	}
	lvaluesCount, tokensIndex := len(assHelper[assHelperOffset].isCollection), len(assHelper[assHelperOffset].tokens)-1
	if assHelper[assHelperOffset].expressionsCount > maxRhsExpr || lvaluesCount > maxRhsExpr {
		maxRHSExpressionsError(compiler.lexer.fileName)
	}
	if lvaluesCount > assHelper[assHelperOffset].expressionsCount && assHelper[assHelperOffset].expressionsCount == 1 {
		if compiler.scopeDepth == 0 {
			emitUnpack(compiler, Bytecode(lvaluesCount), line)
			emitCompoundAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, operator, line)
		} else {
			emitUnpack(compiler, Bytecode(lvaluesCount), line)
			emitCompoundLocalAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, operator, line)
		}
	} else {
		checkSymmetry(lvaluesCount, assHelper[assHelperOffset].expressionsCount, line, compiler.lexer.fileName)
		if compiler.scopeDepth == 0 {
			emitCompoundAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, operator, line)
		} else {
			emitCompoundLocalAssignment(compiler, lvaluesCount, assHelperOffset, tokensIndex, operator, line)
		}
	}
	assHelper[assHelperOffset].reset()
	assHelper = assHelper[:assHelperOffset]
}

// It follows three helper functions for compile compund assignments.

func emitCompoundAssignment(compiler *Compiler, lvaluesCount, assHelperOffset, tokensIndex int, operatorType byte, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == TKRBracket {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), MutatingOperatorSubscript, assHelper[assHelperOffset].indexType[j])
			} else {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), MutatingOperatorSelector, assHelper[assHelperOffset].indexType[j])
			}
		} else {
			emitCompoundGlobalAssignment(compiler, assHelperOffset, tokensIndex, operatorType, line)
			tokensIndex--
		}
	}
}

func emitCompoundLocalAssignment(compiler *Compiler, lvaluesCount, assHelperOffset, tokensIndex int, operatorType byte, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == TKRBracket {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), MutatingOperatorSubscript, assHelper[assHelperOffset].indexType[j])
			} else {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), MutatingOperatorSelector, assHelper[assHelperOffset].indexType[j])
			}
		} else {
			index := resolveLocal(compiler, assHelper[assHelperOffset].tokens[tokensIndex].Description)
			if index == -1 {
				if index := resolveFreeVar(compiler, assHelper[assHelperOffset].tokens[tokensIndex].Description, line); index != -1 {
					emitCompoundSetFreeVar(compiler, Bytecode(index), line, Bytecode(operatorType))
				} else {
					emitCompoundGlobalAssignment(compiler, assHelperOffset, tokensIndex, operatorType, line)
				}
			} else {
				emitCompoundSetLocal(compiler, Bytecode(index), line, Bytecode(operatorType))
			}
			tokensIndex--
		}
	}
}

func emitCompoundGlobalAssignment(compiler *Compiler, assHelperOffset, tokensIndex int, operatorType byte, line UInt32) {
	if pos, exists := compiler.gIDPos[assHelper[assHelperOffset].tokens[tokensIndex].Description]; exists {
		emitCompoundSetGlobal(compiler, pos, line, Bytecode(operatorType))
	} else {
		compiler.gIDPos[assHelper[assHelperOffset].tokens[tokensIndex].Description] = compiler.gIndex
		compiler.gID = append(compiler.gID, assHelper[assHelperOffset].tokens[tokensIndex].Description)
		emitCompoundSetGlobal(compiler, compiler.gIndex, line, Bytecode(operatorType))
		compiler.gIndex++
	}
}

func compileStruct(compiler *Compiler) {
	line := compiler.token.Line
	check(compiler, TKStruct)
	forward(compiler)
	check(compiler, TKIdentifier)
	typeNameToken := compiler.token
	forward(compiler)
	propertiesHelper := make(Namespace)
	methodsHelper := make(Namespace)
	hasDeriving := false
	derivationCount := Bytecode(0)
	if compiler.token.Type == TKLT {
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		hasDeriving = true
		derivationCount++
		for compiler.token.Type == TKComma {
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			derivationCount++
		}
	}
	check(compiler, TKLCurly)
	forward(compiler)
	newStruct := Struct{Id: globalStructUniqueID, Name: typeNameToken.Description, Methods: make(Namespace), Public: make(Namespace), Private: make(Namespace)}
	globalStructUniqueID++
	propertiesHelper[typeNameToken.Description] = NilValue
	stuffCount := Bytecode(0)
	for compiler.token.Type != TKRCurly {
		switch compiler.token.Type {
		case TKIdentifier:
			check(compiler, TKIdentifier)
			if _, exists := propertiesHelper[compiler.token.Description]; exists {
				redefinedField(compiler.token, compiler.lexer.fileName)
			}
			propertiesHelper[compiler.token.Description] = NilValue
			newStruct.Private[compiler.token.Description] = NilValue
			forward(compiler)
			for compiler.token.Type == TKIdentifier {
				check(compiler, TKIdentifier)
				if _, exists := propertiesHelper[compiler.token.Description]; exists {
					redefinedField(compiler.token, compiler.lexer.fileName)
				}
				propertiesHelper[compiler.token.Description] = NilValue
				newStruct.Private[compiler.token.Description] = NilValue
				forward(compiler)
			}
		case TKPublic:
			check(compiler, TKPublic)
			forward(compiler)
			check(compiler, TKIdentifier)
			if _, exists := propertiesHelper[compiler.token.Description]; exists {
				redefinedField(compiler.token, compiler.lexer.fileName)
			}
			propertiesHelper[compiler.token.Description] = NilValue
			newStruct.Public[compiler.token.Description] = NilValue
			forward(compiler)
			for compiler.token.Type == TKIdentifier {
				check(compiler, TKIdentifier)
				if _, exists := propertiesHelper[compiler.token.Description]; exists {
					redefinedField(compiler.token, compiler.lexer.fileName)
				}
				propertiesHelper[compiler.token.Description] = NilValue
				newStruct.Public[compiler.token.Description] = NilValue
				forward(compiler)
			}
		case TKFunction:
			check(compiler, TKFunction)
			forward(compiler)
			check(compiler, TKIdentifier)
			methodIDToken := compiler.token
			forward(compiler)
			if _, exists := methodsHelper[methodIDToken.Description]; exists {
				redefinedField(methodIDToken, compiler.lexer.fileName)
			}
			methodsHelper[methodIDToken.Description] = NilValue
			compileMethod(compiler, methodIDToken)
			stuffCount++
		default:
			compilerPrintError("type, property or method definition", compiler.token, compiler.lexer.fileName)
		}
	}
	check(compiler, TKRCurly)
	forward(compiler)
	emitConstant(compiler, newStruct, typeNameToken.Line)
	emitBuildStruct(compiler, stuffCount, typeNameToken.Line)
	if hasDeriving {
		emitDerive(compiler, derivationCount, line)
	}
	if compiler.scopeDepth > 0 {
		addLocal(compiler, typeNameToken)
	} else {
		if pos, exists := compiler.gIDPos[typeNameToken.Description]; exists {
			emitNewGlobal(compiler, pos, line)
		} else {
			compiler.gIDPos[typeNameToken.Description] = compiler.gIndex
			compiler.gID = append(compiler.gID, typeNameToken.Description)
			emitNewGlobal(compiler, compiler.gIndex, line)
			compiler.gIndex++
		}
	}
	propertiesHelper = nil
}

func compileMethod(compiler *Compiler, methodIDToken Token) {
	isvararg := false
	arity := Bytecode(0)
	paramsMap := make(map[string]struct{})
	var params []Token
	if compiler.token.Type == TK3Dots {
		check(compiler, TK3Dots)
		forward(compiler)
		check(compiler, TKIdentifier)
		params = append(params, compiler.token)
		forward(compiler)
		isvararg = true
	} else {
		for compiler.token.Type != TKLCurly && compiler.token.Type != TK3Dots {
			check(compiler, TKIdentifier)
			if _, isParam := paramsMap[compiler.token.Description]; isParam || compiler.token.Description == methodIDToken.Description {
				argumentError(compiler.token, compiler.lexer.fileName)
			} else {
				paramsMap[compiler.token.Description] = struct{}{}
				params = append(params, compiler.token)
			}
			forward(compiler)
			arity++
		}
		if compiler.token.Type == TK3Dots {
			check(compiler, TK3Dots)
			forward(compiler)
			check(compiler, TKIdentifier)
			params = append(params, compiler.token)
			forward(compiler)
			isvararg = true
		}
	}
	check(compiler, TKLCurly)
	forward(compiler)
	newFunction, freeVariables := buildMethodBody(compiler, params)
	newFunction.Name = methodIDToken.Description
	newFunction.Arity = arity
	newFunction.Vararg = isvararg
	emitBuildClosure(compiler, newFunction, freeVariables, methodIDToken.Line)
	check(compiler, TKRCurly)
	forward(compiler)
	paramsMap = nil
	params = nil
	freeVariables = nil
}

func buildMethodBody(compiler *Compiler, params []Token) (Function, []CompFreeVar) {
	childCompiler := newChildCompiler(compiler)
	childCompiler.canAccess = true
	beginScope(childCompiler)
	for _, tok := range params {
		addLocal(childCompiler, tok)
	}
	for childCompiler.token.Type != TKEof && childCompiler.token.Type != TKRCurly {
		switch childCompiler.token.Type {
		case TKLet:
			compileDeclaration(childCompiler)
		case TKIdentifier:
			compileAssignment(childCompiler)
		case TKStruct:
			compileStruct(childCompiler)
		case TKFunction:
			compileClosure(childCompiler)
		case TKIf:
			compileIf(childCompiler, false)
		case TKFor:
			compileFor(childCompiler)
		case TKLabel:
			switch childCompiler.next.Type {
			case TKFor:
				compileFor(childCompiler)
			default:
				compilerPrintError("Statement", childCompiler.next, childCompiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(childCompiler, false)
		case TKReturn:
			compileReturn(childCompiler)
		case TKDefer:
			compileDefer(childCompiler)
		case TKExtension:
			compileExtension(childCompiler)
		case TKLCurly:
			compileBlock(childCompiler, false)
		default:
			compilerPrintError("Statement", childCompiler.token, childCompiler.lexer.fileName)
		}
	}
	if childCompiler.hasDefer {
		// |- OpCode -|
		instruction := OPRunDefer
		childCompiler.function.Code = append(childCompiler.function.Code, instruction)
		childCompiler.function.Lines[Bytecode(len(childCompiler.function.Code)-1)] = childCompiler.token.Line
	}
	emitNilReturn(childCompiler, childCompiler.token.Line)
	endScope(childCompiler)
	updateCompiler(compiler, childCompiler)
	childCompiler.function.CanAccessPrivateState = true
	return childCompiler.function, childCompiler.freeVariables
}

func compileExtension(compiler *Compiler) {
	line := compiler.token.Line
	check(compiler, TKExtension)
	forward(compiler)
	typeCount := Bytecode(0)
	fnNames := make(Namespace)
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	typeCount++
	for compiler.token.Type == TKComma {
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		typeCount++
	}
	check(compiler, TKLCurly)
	forward(compiler)
	methodCount := Bytecode(0)
	for compiler.token.Type == TKFunction {
		check(compiler, TKFunction)
		forward(compiler)
		check(compiler, TKIdentifier)
		methodIDToken := compiler.token
		forward(compiler)
		if _, exists := fnNames[methodIDToken.Description]; exists {
			redefinedField(compiler.token, compiler.lexer.fileName)
		}
		fnNames[methodIDToken.Description] = NilValue
		compileMethod(compiler, methodIDToken)
		methodCount++
	}
	check(compiler, TKRCurly)
	forward(compiler)
	if typeCount > maxTypeExtensions {
		maxTypeExtensionError(compiler.lexer.fileName)
	}
	emitExtension(compiler, methodCount, typeCount, line)
	fnNames = nil
}

func compileConstNamespace(compiler *Compiler) {
	line := compiler.token.Line
	forward(compiler)
	check(compiler, TKIdentifier)
	name := compiler.token.Description
	forward(compiler)
	check(compiler, TKLCurly)
	forward(compiler)
	indexer := Bytecode(0)
	constNamespace := NamedConstants{Name: name, Constants: make(Namespace), Indexes: make(map[Bytecode]string)}
	if compiler.token.Type == TKIdentifier {
		switch compiler.next.Type {
		case TKIdentifier:
			check(compiler, TKIdentifier)
			constId := compiler.token
			forward(compiler)
			if compiler.token.Description == constInit {
				forward(compiler)
				check(compiler, TKEquation)
				forward(compiler)
				check(compiler, TKInteger)
				ordinal := Int(0)
				if value, err := strconv.ParseInt(compiler.token.Description, 0, 64); err == nil {
					ordinal = Int(value)
				} else {
					numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
				}
				constNamespace.Constants[constId.Description] = ordinal
				forward(compiler)
				for compiler.token.Type == TKIdentifier {
					ordinal++
					constNamespace.Constants[compiler.token.Description] = ordinal
					forward(compiler)
				}
			} else {
				ordinal := Int(0)
				constNamespace.Constants[constId.Description] = ordinal
				for compiler.token.Type == TKIdentifier {
					ordinal++
					constNamespace.Constants[compiler.token.Description] = ordinal
					forward(compiler)
				}
			}
			check(compiler, TKRCurly)
			forward(compiler)
			emitConstant(compiler, constNamespace, line)
		case TKEquation:
			constId := compiler.token
			forward(compiler)
			check(compiler, TKEquation)
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			if _, exists := constNamespace.Constants[constId.Description]; exists || constId.Description == name {
				redefinedField(constId, compiler.lexer.fileName)
			} else {
				indexer++
				constNamespace.Constants[constId.Description] = NilValue
				constNamespace.Indexes[indexer] = constId.Description
			}
			for compiler.token.Type == TKIdentifier {
				constId := compiler.token
				forward(compiler)
				check(compiler, TKEquation)
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				if _, exists := constNamespace.Constants[constId.Description]; exists || constId.Description == name {
					redefinedField(constId, compiler.lexer.fileName)
				} else {
					indexer++
					constNamespace.Constants[constId.Description] = NilValue
					constNamespace.Indexes[indexer] = constId.Description
				}
			}
			check(compiler, TKRCurly)
			forward(compiler)
			emitConstant(compiler, constNamespace, line)
			emitConstantNamespace(compiler, indexer, line)
		case TKRCurly:
			constNamespace.Constants[compiler.token.Description] = Int(0)
			forward(compiler)
			check(compiler, TKRCurly)
			forward(compiler)
			emitConstant(compiler, constNamespace, line)
		default:
			compilerPrintError("Identifier or }", compiler.next, compiler.lexer.fileName)
		}
	} else {
		check(compiler, TKRCurly)
		forward(compiler)
		emitConstant(compiler, constNamespace, line)
	}
	if pos, exists := compiler.gIDPos[name]; exists {
		emitNewGlobal(compiler, pos, line)
	} else {
		compiler.gIDPos[name] = compiler.gIndex
		compiler.gID = append(compiler.gID, name)
		emitNewGlobal(compiler, compiler.gIndex, line)
		compiler.gIndex++
	}
}

func compileClosure(compiler *Compiler) {
	line := compiler.token.Line
	check(compiler, TKFunction)
	forward(compiler)
	check(compiler, TKIdentifier)
	functionIDToken := compiler.token
	forward(compiler)
	isvararg := false
	arity := Bytecode(0)
	paramsMap := make(map[string]struct{})
	var params []Token
	if compiler.token.Type == TK3Dots {
		check(compiler, TK3Dots)
		forward(compiler)
		check(compiler, TKIdentifier)
		params = append(params, compiler.token)
		forward(compiler)
		isvararg = true
	} else {
		for compiler.token.Type != TKLCurly && compiler.token.Type != TK3Dots {
			check(compiler, TKIdentifier)
			if _, isParam := paramsMap[compiler.token.Description]; isParam || compiler.token.Description == functionIDToken.Description {
				argumentError(compiler.token, compiler.lexer.fileName)
			} else {
				paramsMap[compiler.token.Description] = struct{}{}
				params = append(params, compiler.token)
			}
			forward(compiler)
			arity++
		}
		if compiler.token.Type == TK3Dots {
			check(compiler, TK3Dots)
			forward(compiler)
			check(compiler, TKIdentifier)
			params = append(params, compiler.token)
			forward(compiler)
			isvararg = true
		}
	}
	check(compiler, TKLCurly)
	forward(compiler)
	newFunction, freeVariables := buildFunctionBody(compiler, params)
	newFunction.Name = functionIDToken.Description
	newFunction.Arity = arity
	newFunction.Vararg = isvararg
	emitBuildClosure(compiler, newFunction, freeVariables, functionIDToken.Line)
	check(compiler, TKRCurly)
	forward(compiler)
	if compiler.scopeDepth > 0 {
		addLocal(compiler, functionIDToken)
	} else {
		if pos, exists := compiler.gIDPos[functionIDToken.Description]; exists {
			emitNewGlobal(compiler, pos, line)
		} else {
			compiler.gIDPos[functionIDToken.Description] = compiler.gIndex
			compiler.gID = append(compiler.gID, functionIDToken.Description)
			emitNewGlobal(compiler, compiler.gIndex, line)
			compiler.gIndex++
		}
	}
	paramsMap = nil
	params = nil
	freeVariables = nil
}

func buildFunctionBody(compiler *Compiler, params []Token) (Function, []CompFreeVar) {
	childCompiler := newChildCompiler(compiler)
	beginScope(childCompiler)
	for _, tok := range params {
		addLocal(childCompiler, tok)
	}
	for childCompiler.token.Type != TKEof && childCompiler.token.Type != TKRCurly {
		switch childCompiler.token.Type {
		case TKLet:
			compileDeclaration(childCompiler)
		case TKIdentifier:
			compileAssignment(childCompiler)
		case TKStruct:
			compileStruct(childCompiler)
		case TKFunction:
			compileClosure(childCompiler)
		case TKIf:
			compileIf(childCompiler, false)
		case TKFor:
			compileFor(childCompiler)
		case TKLabel:
			switch childCompiler.next.Type {
			case TKFor:
				compileFor(childCompiler)
			default:
				compilerPrintError("Statement", childCompiler.next, childCompiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(childCompiler, false)
		case TKReturn:
			compileReturn(childCompiler)
		case TKDefer:
			compileDefer(childCompiler)
		case TKExtension:
			compileExtension(childCompiler)
		case TKLCurly:
			compileBlock(childCompiler, false)
		default:
			compilerPrintError("Statement", childCompiler.token, childCompiler.lexer.fileName)
		}
	}
	if childCompiler.hasDefer {
		// |- OpCode -|
		instruction := OPRunDefer
		childCompiler.function.Code = append(childCompiler.function.Code, instruction)
		childCompiler.function.Lines[Bytecode(len(childCompiler.function.Code)-1)] = childCompiler.token.Line
	}
	emitNilReturn(childCompiler, childCompiler.token.Line)
	endScope(childCompiler)
	updateCompiler(compiler, childCompiler)
	if childCompiler.canAccess {
		childCompiler.function.CanAccessPrivateState = true
	}
	return childCompiler.function, childCompiler.freeVariables
}

// This function emits the instructions for creating closures at run time with or without taking free variables.
func emitBuildClosure(compiler *Compiler, newFunction Function, freeVariables []CompFreeVar, line UInt32) {
	if newFunction.FreeVarCount == 0 {
		emitClosure(compiler, appendValue(compiler, newFunction), line, false)
	} else {
		emitClosure(compiler, appendValue(compiler, newFunction), line, true)
		for i := Bytecode(0); i < newFunction.FreeVarCount; i++ {
			emitFreeVar(compiler, freeVariables[i].index, line, freeVariables[i].isLocal)
		}
	}
}

func compileReturn(compiler *Compiler) {
	line := compiler.token.Line
	if compiler.hasDefer {
		// |- OpCode -|
		instruction := OPRunDefer
		compiler.function.Code = append(compiler.function.Code, instruction)
		compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	}
	returnCount := Bytecode(1)
	check(compiler, TKReturn)
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	for compiler.token.Type == TKComma {
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		returnCount++
	}
	emitReturn(compiler, returnCount, line)
}

func compileDefer(compiler *Compiler) {
	check(compiler, TKDefer)
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	index := len(compiler.function.Code) - 1
	if compiler.function.Code[index]&opcodeMask == OPCall && compiler.token.Type == TKRParen {
		forward(compiler)
		oldInstr := compiler.function.Code[index]
		instruction := OPDefer
		instruction |= (oldInstr >> instructionShift) << instructionShift
		compiler.function.Code[index] = instruction
		compiler.hasDefer = true
	} else if compiler.function.Code[index]&opcodeMask == OPInvokeMethod && compiler.token.Type == TKRParen {
		forward(compiler)
		oldInstr := compiler.function.Code[index]
		instruction := OPDeferInvoke
		instruction |= (oldInstr >> instructionShift) << instructionShift
		compiler.function.Code[index] = instruction
		compiler.hasDefer = true
	} else {
		compilerPrintError("function/method call", compiler.token, compiler.lexer.fileName)
	}
}

func compileIf(compiler *Compiler, insideLoop bool) {
	offset := len(jumpHelper)
	line := compiler.token.Line
	check(compiler, TKIf)
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	jumpAddress := emitJumpIfFalse(compiler, line)
	check(compiler, TKLCurly)
	forward(compiler)
	compileIFBlock(compiler, insideLoop)
	check(compiler, TKRCurly)
	forward(compiler)
	jumpHelper = append(jumpHelper, emitJump(compiler, line))
	compiler.function.Code[jumpAddress] |= Bytecode(len(compiler.function.Code)) << instructionShift
	for compiler.token.Type == TKElse && compiler.next.Type == TKIf {
		line = compiler.token.Line
		check(compiler, TKElse)
		forward(compiler)
		check(compiler, TKIf)
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		jumpAddress := emitJumpIfFalse(compiler, line)
		check(compiler, TKLCurly)
		forward(compiler)
		compileIFBlock(compiler, insideLoop)
		check(compiler, TKRCurly)
		forward(compiler)
		jumpHelper = append(jumpHelper, emitJump(compiler, line))
		compiler.function.Code[jumpAddress] |= Bytecode(len(compiler.function.Code)) << instructionShift
	}
	if compiler.token.Type == TKElse {
		check(compiler, TKElse)
		forward(compiler)
		check(compiler, TKLCurly)
		forward(compiler)
		compileIFBlock(compiler, insideLoop)
		check(compiler, TKRCurly)
		forward(compiler)
	}
	instrAddress := Bytecode(len(compiler.function.Code))
	length := len(jumpHelper)
	for i := offset; i < length; i++ {
		compiler.function.Code[jumpHelper[i]] |= instrAddress << instructionShift
	}
	jumpHelper = jumpHelper[:offset]
}

func compileIFBlock(compiler *Compiler, insideLoop bool) {
	beginScope(compiler)
	for compiler.token.Type != TKEof &&
		compiler.token.Type != TKRCurly &&
		compiler.token.Type != TKElse {
		switch compiler.token.Type {
		case TKLet:
			compileDeclaration(compiler)
		case TKIdentifier:
			compileAssignment(compiler)
		case TKStruct:
			compileStruct(compiler)
		case TKFunction:
			compileClosure(compiler)
		case TKIf:
			compileIf(compiler, insideLoop)
		case TKFor:
			compileFor(compiler)
		case TKLabel:
			switch compiler.next.Type {
			case TKFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(compiler, insideLoop)
		case TKBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKReturn:
			compileReturn(compiler)
		case TKDefer:
			compileDefer(compiler)
		case TKExtension:
			compileExtension(compiler)
		case TKLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileFor(compiler *Compiler) {
	variableCount := Bytecode(0)
	loopOffset := len(loopHelper)
	loopHelper = append(loopHelper, loopInfo{})
	loopHelper[loopOffset].scopeDepth = compiler.scopeDepth
	if compiler.token.Type == TKLabel {
		loopHelper[loopOffset].label = compiler.token.Description
		forward(compiler)
	}
	line := compiler.token.Line
	check(compiler, TKFor)
	forward(compiler)
	if compiler.token.Type == TKLCurly {
		check(compiler, TKLCurly)
		forward(compiler)
		loopHelper[loopOffset].loopAddress = Bytecode(len(compiler.function.Code))
		compileLoopBlock(compiler)
		check(compiler, TKRCurly)
		forward(compiler)
		compiler.function.Code[emitJump(compiler, line)] |= loopHelper[loopOffset].loopAddress << instructionShift
		afterLoopAddress := Bytecode(len(compiler.function.Code))
		for i := 0; i < len(loopHelper[loopOffset].breaks); i++ {
			compiler.function.Code[loopHelper[loopOffset].breaks[i]] |= afterLoopAddress << instructionShift
		}
	} else if compiler.next.Type == TKIn || compiler.next.Type == TKComma {
		loopHelper[loopOffset].isRangeLoop = true
		beginScope(compiler)
		check(compiler, TKIdentifier)
		compileNil(compiler, PrecedenceNone)
		addLocal(compiler, compiler.token)
		forward(compiler)
		variableCount++
		for compiler.token.Type == TKComma {
			forward(compiler)
			check(compiler, TKIdentifier)
			compileNil(compiler, PrecedenceNone)
			addLocal(compiler, compiler.token)
			forward(compiler)
			variableCount++
		}
		check(compiler, TKIn)
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		addLocal(compiler, Token{Type: TKIdentifier, Description: iteratorID, Line: compiler.token.Line})
		loopHelper[loopOffset].loopAddress = Bytecode(len(compiler.function.Code))
		jumpAddressCheck := emitNext(compiler, line)
		if variableCount > 1 {
			emitUnpackFor(compiler, variableCount, line)
		}
		check(compiler, TKLCurly)
		forward(compiler)
		compileLoopBlock(compiler)
		line = compiler.token.Line
		check(compiler, TKRCurly)
		forward(compiler)
		compiler.function.Code[emitJump(compiler, line)] |= loopHelper[loopOffset].loopAddress << instructionShift
		afterLoopAddress := Bytecode(len(compiler.function.Code))
		compiler.function.Code[jumpAddressCheck] |= afterLoopAddress << instructionShift
		for i := 0; i < len(loopHelper[loopOffset].breaks); i++ {
			compiler.function.Code[loopHelper[loopOffset].breaks[i]] |= afterLoopAddress << instructionShift
		}
		if count := endScope(compiler); count != 0 {
			emitPop(compiler, count, line)
		}
	} else {
		jumpWhenFalseOrNil := 0
		loopHelper[loopOffset].loopAddress = Bytecode(len(compiler.function.Code))
		compileExpression(compiler, PrecedenceNone)
		jumpWhenFalseOrNil = emitJumpIfFalse(compiler, line)
		forward(compiler)
		check(compiler, TKLCurly)
		forward(compiler)
		compileLoopBlock(compiler)
		line = compiler.token.Line
		check(compiler, TKRCurly)
		forward(compiler)
		compiler.function.Code[emitJump(compiler, line)] |= loopHelper[loopOffset].loopAddress << instructionShift
		afterLoopAddress := Bytecode(len(compiler.function.Code))
		compiler.function.Code[jumpWhenFalseOrNil] |= afterLoopAddress << instructionShift
		for i := 0; i < len(loopHelper[loopOffset].breaks); i++ {
			compiler.function.Code[loopHelper[loopOffset].breaks[i]] |= afterLoopAddress << instructionShift
		}
	}
	loopHelper[loopOffset].breaks = nil
	loopHelper = loopHelper[:loopOffset]
}

func compileLoopBlock(compiler *Compiler) {
	beginScope(compiler)
	for compiler.token.Type != TKEof && compiler.token.Type != TKRCurly {
		switch compiler.token.Type {
		case TKLet:
			compileDeclaration(compiler)
		case TKIdentifier:
			compileAssignment(compiler)
		case TKStruct:
			compileStruct(compiler)
		case TKFunction:
			compileClosure(compiler)
		case TKIf:
			compileIf(compiler, true)
		case TKFor:
			compileFor(compiler)
		case TKLabel:
			switch compiler.next.Type {
			case TKFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(compiler, true)
		case TKBreak:
			compileBreak(compiler)
		case TKContinue:
			compileContinue(compiler)
		case TKReturn:
			compileReturn(compiler)
		case TKDefer:
			compileDefer(compiler)
		case TKExtension:
			compileExtension(compiler)
		case TKLCurly:
			compileBlock(compiler, true)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileSwitch(compiler *Compiler, insideLoop bool) {
	check(compiler, TKSwitch)
	forward(compiler)
	switch compiler.token.Type {
	case TKLCurly:
		offset := len(jumpHelper)
		check(compiler, TKLCurly)
		forward(compiler)
		for compiler.token.Type == TKCase {
			line := compiler.token.Line
			check(compiler, TKCase)
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			jumpIfFalseInstructionIndex := emitJumpIfFalse(compiler, line)
			check(compiler, TKColon)
			forward(compiler)
			compileSwitchCaseBlock(compiler, insideLoop)
			jumpHelper = append(jumpHelper, emitJump(compiler, line))
			compiler.function.Code[jumpIfFalseInstructionIndex] |= Bytecode(len(compiler.function.Code)) << instructionShift
		}
		compileSwitchDefaultBlock(compiler, insideLoop)
		check(compiler, TKRCurly)
		forward(compiler)
		exitAddress := Bytecode(len(compiler.function.Code))
		length := len(jumpHelper)
		for i := offset; i < length; i++ {
			compiler.function.Code[jumpHelper[i]] |= exitAddress << instructionShift
		}
		jumpHelper = jumpHelper[:offset]
	default:
		offset := len(jumpHelper)
		patternOffset := len(patternHelper)
		beginScope(compiler)
		line := compiler.token.Line
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		addLocal(compiler, Token{Type: TKIdentifier, Description: switchID, Line: line})
		switchLocalIndex := resolveLocal(compiler, switchID)
		check(compiler, TKLCurly)
		forward(compiler)
		for compiler.token.Type == TKCase {
			line := compiler.token.Line
			check(compiler, TKCase)
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			emitGetLocal(compiler, Bytecode(switchLocalIndex), line)
			patternHelper = append(patternHelper, emitMatch(compiler, line))
			for compiler.token.Type == TKComma {
				check(compiler, TKComma)
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				emitGetLocal(compiler, Bytecode(switchLocalIndex), line)
				patternHelper = append(patternHelper, emitMatch(compiler, line))
			}
			check(compiler, TKColon)
			forward(compiler)
			nextPatternAddress := emitJump(compiler, line)
			for i := patternOffset; i < len(patternHelper); i++ {
				compiler.function.Code[patternHelper[i]] |= Bytecode(len(compiler.function.Code)) << instructionShift
			}
			compileSwitchCaseBlock(compiler, insideLoop)
			jumpHelper = append(jumpHelper, emitJump(compiler, line))
			compiler.function.Code[nextPatternAddress] |= Bytecode(len(compiler.function.Code)) << instructionShift
			patternHelper = patternHelper[:patternOffset]
		}
		compileSwitchDefaultBlock(compiler, insideLoop)
		line = compiler.token.Line
		check(compiler, TKRCurly)
		forward(compiler)
		for i := offset; i < len(jumpHelper); i++ {
			compiler.function.Code[jumpHelper[i]] |= Bytecode(len(compiler.function.Code)) << instructionShift
		}
		if count := endScope(compiler); count != 0 {
			emitPop(compiler, count, line)
		}
		jumpHelper = jumpHelper[:offset]
	}
}

func compileSwitchCaseBlock(compiler *Compiler, insideLoop bool) {
	beginScope(compiler)
	for compiler.token.Type != TKEof && compiler.token.Type != TKCase && compiler.token.Type != TKDefault {
		switch compiler.token.Type {
		case TKLet:
			compileDeclaration(compiler)
		case TKIdentifier:
			compileAssignment(compiler)
		case TKStruct:
			compileStruct(compiler)
		case TKFunction:
			compileClosure(compiler)
		case TKIf:
			compileIf(compiler, insideLoop)
		case TKFor:
			compileFor(compiler)
		case TKLabel:
			switch compiler.next.Type {
			case TKFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(compiler, insideLoop)
		case TKBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKReturn:
			compileReturn(compiler)
		case TKDefer:
			compileDefer(compiler)
		case TKExtension:
			compileExtension(compiler)
		case TKLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileSwitchDefaultBlock(compiler *Compiler, insideLoop bool) {
	beginScope(compiler)
	check(compiler, TKDefault)
	forward(compiler)
	check(compiler, TKColon)
	forward(compiler)
	for compiler.token.Type != TKEof && compiler.token.Type != TKRCurly {
		switch compiler.token.Type {
		case TKLet:
			compileDeclaration(compiler)
		case TKIdentifier:
			compileAssignment(compiler)
		case TKStruct:
			compileStruct(compiler)
		case TKFunction:
			compileClosure(compiler)
		case TKIf:
			compileIf(compiler, insideLoop)
		case TKFor:
			compileFor(compiler)
		case TKLabel:
			switch compiler.next.Type {
			case TKFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(compiler, insideLoop)
		case TKBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKReturn:
			compileReturn(compiler)
		case TKDefer:
			compileDefer(compiler)
		case TKExtension:
			compileExtension(compiler)
		case TKLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileBreak(compiler *Compiler) {
	line := compiler.token.Line
	check(compiler, TKBreak)
	forward(compiler)
	if length := len(loopHelper); compiler.token.Type == TKLabel {
		var loopHelperIndex int
		var found bool
		for loopHelperIndex = 0; loopHelperIndex < length; loopHelperIndex++ {
			if loopHelper[loopHelperIndex].label == compiler.token.Description {
				found = true
				break
			}
		}
		if !found {
			labelError(compiler.token, compiler.lexer.fileName)
		}
		if loopHelper[loopHelperIndex].isRangeLoop {
			if count := countLocalsUpTo(compiler, loopHelper[loopHelperIndex].scopeDepth+1); count != 0 {
				emitPop(compiler, count, line)
			}
		} else if count := countLocalsUpTo(compiler, loopHelper[loopHelperIndex].scopeDepth); count != 0 {
			emitPop(compiler, count, line)
		}
		loopHelper[loopHelperIndex].breaks = append(loopHelper[loopHelperIndex].breaks, emitJump(compiler, line))
		forward(compiler)
	} else {
		if loopHelper[length-1].isRangeLoop {
			if count := countLocalsUpTo(compiler, loopHelper[length-1].scopeDepth+1); count != 0 {
				emitPop(compiler, count, line)
			}
		} else if count := countLocalsUpTo(compiler, loopHelper[length-1].scopeDepth); count != 0 {
			emitPop(compiler, count, line)
		}
		loopHelper[length-1].breaks = append(loopHelper[length-1].breaks, emitJump(compiler, line))
	}
}

func compileContinue(compiler *Compiler) {
	line := compiler.token.Line
	check(compiler, TKContinue)
	forward(compiler)
	if length := len(loopHelper); compiler.token.Type == TKLabel {
		var found bool
		var loopHelperIndex int
		for loopHelperIndex = 0; loopHelperIndex < length; loopHelperIndex++ {
			if loopHelper[loopHelperIndex].label == compiler.token.Description {
				found = true
				break
			}
		}
		if !found {
			labelError(compiler.token, compiler.lexer.fileName)
		}
		if loopHelper[loopHelperIndex].isRangeLoop {
			if count := countLocalsUpTo(compiler, loopHelper[loopHelperIndex].scopeDepth+1); count != 0 {
				emitPop(compiler, count, line)
			}
		} else if count := countLocalsUpTo(compiler, loopHelper[loopHelperIndex].scopeDepth); count != 0 {
			emitPop(compiler, count, line)
		}
		compiler.function.Code[emitJump(compiler, line)] |= loopHelper[loopHelperIndex].loopAddress << instructionShift
		forward(compiler)
	} else {
		if loopHelper[length-1].isRangeLoop {
			if count := countLocalsUpTo(compiler, loopHelper[length-1].scopeDepth+1); count != 0 {
				emitPop(compiler, count, line)
			}
		} else if count := countLocalsUpTo(compiler, loopHelper[length-1].scopeDepth); count != 0 {
			emitPop(compiler, count, line)
		}
		compiler.function.Code[emitJump(compiler, line)] |= loopHelper[length-1].loopAddress << instructionShift
	}
}

func compileBlock(compiler *Compiler, insideLoop bool) {
	check(compiler, TKLCurly)
	forward(compiler)
	beginScope(compiler)
	for compiler.token.Type != TKEof && compiler.token.Type != TKRCurly {
		switch compiler.token.Type {
		case TKLet:
			compileDeclaration(compiler)
		case TKIdentifier:
			compileAssignment(compiler)
		case TKStruct:
			compileStruct(compiler)
		case TKFunction:
			compileClosure(compiler)
		case TKIf:
			compileIf(compiler, insideLoop)
		case TKFor:
			compileFor(compiler)
		case TKLabel:
			switch compiler.next.Type {
			case TKFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case TKSwitch:
			compileSwitch(compiler, insideLoop)
		case TKBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case TKReturn:
			compileReturn(compiler)
		case TKDefer:
			compileDefer(compiler)
		case TKExtension:
			compileExtension(compiler)
		case TKLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	check(compiler, TKRCurly)
	forward(compiler)
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileExpression(compiler *Compiler, precedence byte) {
	prefixRule := compiler.rules[compiler.token.Type].prefix
	if prefixRule == nil {
		compilerPrintError("Expression", compiler.token, compiler.lexer.fileName)
	}
	prefixRule(compiler, precedence)
	for precedence <= compiler.rules[compiler.next.Type].precedence && compiler.rules[compiler.next.Type].precedence != PrecedenceNone {
		forward(compiler)
		infixRule := compiler.rules[compiler.token.Type].infix
		infixRule(compiler, precedence)
	}
}

func compileInteger(compiler *Compiler, precedence byte) {
	if value, err := strconv.ParseInt(string(compiler.token.Description), 0, 64); err == nil {
		emitConstant(compiler, Int(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileUInt(compiler *Compiler, precedence byte) {
	if value, err := strconv.ParseUint(string(compiler.token.Description), 0, 64); err == nil {
		emitConstant(compiler, UInt(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileFloat(compiler *Compiler, precedence byte) {
	if value, err := strconv.ParseFloat(compiler.token.Description, 64); err == nil {
		emitConstant(compiler, Float(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileString(compiler *Compiler, precedence byte) {
	emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
}

func compileBig(compiler *Compiler, precedence byte) {
	big := new(big.Int)
	if _, success := big.SetString(compiler.token.Description, 0); success {
		emitConstant(compiler, &BInt{Value: big}, compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileRational(compiler *Compiler, precedence byte) {
	rat := new(big.Rat)
	if _, success := rat.SetString(compiler.token.Description); success {
		emitConstant(compiler, &Rational{Value: rat}, compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileComplex(compiler *Compiler, precedence byte) {
	if value, err := strconv.ParseComplex(compiler.token.Description, 128); err == nil {
		emitConstant(compiler, Complex(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileTrue(compiler *Compiler, precedence byte) {
	// |- OPCode -|
	compiler.function.Code = append(compiler.function.Code, OPTrue)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = compiler.token.Line
}

func compileFalse(compiler *Compiler, precedence byte) {
	// |- OPCode -|
	compiler.function.Code = append(compiler.function.Code, OPFalse)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = compiler.token.Line
}

func compileNil(compiler *Compiler, precedence byte) {
	// |- OPCode -|
	compiler.function.Code = append(compiler.function.Code, OPNil)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = compiler.token.Line
}

func compileRune(compiler *Compiler, precedence byte) {
	r, _ := utf8.DecodeLastRuneInString(compiler.token.Description)
	emitConstant(compiler, Rune(r), compiler.token.Line)
}

func compileIdentifier(compiler *Compiler, precedence byte) {
	if compiler.scopeDepth != 0 {
		localIndex := resolveLocal(compiler, compiler.token.Description)
		if localIndex == -1 {
			if freeVarIndex := resolveFreeVar(compiler, compiler.token.Description, compiler.token.Line); freeVarIndex != -1 {
				emitGetFreeVar(compiler, Bytecode(freeVarIndex), compiler.token.Line)
			} else {
				if pos, exists := compiler.gIDPos[compiler.token.Description]; exists {
					emitGetGlobal(compiler, pos, compiler.token.Line)
				} else {
					compiler.gIDPos[compiler.token.Description] = compiler.gIndex
					compiler.gID = append(compiler.gID, compiler.token.Description)
					emitGetGlobal(compiler, compiler.gIndex, compiler.token.Line)
					compiler.gIndex++
				}
			}
		} else {
			emitGetLocal(compiler, Bytecode(localIndex), compiler.token.Line)
		}
	} else {
		if pos, exists := compiler.gIDPos[compiler.token.Description]; exists {
			emitGetGlobal(compiler, pos, compiler.token.Line)
		} else {
			compiler.gIDPos[compiler.token.Description] = compiler.gIndex
			compiler.gID = append(compiler.gID, compiler.token.Description)
			emitGetGlobal(compiler, compiler.gIndex, compiler.token.Line)
			compiler.gIndex++
		}
	}
}

func buildLambda(compiler *Compiler, params []Token) (Function, []CompFreeVar) {
	line := compiler.token.Line
	childCompiler := newChildCompiler(compiler)
	beginScope(childCompiler)
	for _, tok := range params {
		addLocal(childCompiler, tok)
	}
	compileExpression(childCompiler, PrecedenceNone)
	emitReturn(childCompiler, 1, line)
	endScope(childCompiler)
	updateCompiler(compiler, childCompiler)
	if childCompiler.canAccess {
		childCompiler.function.CanAccessPrivateState = true
	}
	return childCompiler.function, childCompiler.freeVariables
}

func compileAnonymousFunction(compiler *Compiler, precedence byte) {
	var isLambdaExpression bool
	line := compiler.token.Line
	forward(compiler)
	isvararg := false
	arity := Bytecode(0)
	paramsMap := make(map[string]struct{})
	var params []Token
	if compiler.token.Type == TK3Dots {
		check(compiler, TK3Dots)
		forward(compiler)
		check(compiler, TKIdentifier)
		params = append(params, compiler.token)
		forward(compiler)
		isvararg = true
	} else {
		for compiler.token.Type != TKColon && compiler.token.Type != TKLCurly && compiler.token.Type != TK3Dots {
			check(compiler, TKIdentifier)
			if _, isParam := paramsMap[compiler.token.Description]; isParam {
				argumentError(compiler.token, compiler.lexer.fileName)
			} else {
				paramsMap[compiler.token.Description] = struct{}{}
				params = append(params, compiler.token)
			}
			forward(compiler)
			arity++
		}
		if compiler.token.Type == TK3Dots {
			check(compiler, TK3Dots)
			forward(compiler)
			check(compiler, TKIdentifier)
			params = append(params, compiler.token)
			forward(compiler)
			isvararg = true
		}
	}
	if compiler.token.Type == TKColon {
		isLambdaExpression = true
		check(compiler, TKColon)
	} else {
		check(compiler, TKLCurly)
	}
	forward(compiler)
	var newLambda Function
	var freeVariables []CompFreeVar
	if isLambdaExpression {
		newLambda, freeVariables = buildLambda(compiler, params)
	} else {
		newLambda, freeVariables = buildFunctionBody(compiler, params)
	}
	newLambda.Name = fmt.Sprintf("anonymous(%v)", lambdaID)
	newLambda.Arity = arity
	newLambda.Vararg = isvararg
	emitBuildClosure(compiler, newLambda, freeVariables, line)
	if !isLambdaExpression {
		check(compiler, TKRCurly)
	}
	paramsMap = nil
	params = nil
	freeVariables = nil
	lambdaID++
}

func compileRange(compiler *Compiler, precedence byte) {
	line := compiler.token.Line
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	argCount := Bytecode(1)
	if compiler.next.Type == TKComma {
		forward(compiler)
		check(compiler, TKComma)
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		argCount++
		if compiler.next.Type == TKComma {
			forward(compiler)
			check(compiler, TKComma)
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			argCount++
		}
	}
	emitRange(compiler, argCount, line)
}

func compileRecord(compiler *Compiler, precedence byte) {
	switch compiler.next.Type {
	case TKRCurly:
		forward(compiler)
		emitRecord(compiler, 0, compiler.token.Line)
	default:
		line := compiler.token.Line
		length := Bytecode(0)
		forward(compiler)
		check(compiler, TKIdentifier)
		emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
		forward(compiler)
		check(compiler, TKColon)
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		length += 2
		for compiler.token.Type == TKComma {
			forward(compiler)
			check(compiler, TKIdentifier)
			emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
			forward(compiler)
			check(compiler, TKColon)
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			length += 2
		}
		check(compiler, TKRCurly)
		emitRecord(compiler, length, line)
	}
}

func compilePrefix(compiler *Compiler, precedence byte) {
	operator := compiler.token.Type
	line := compiler.token.Line
	forward(compiler)
	compileExpression(compiler, PrecedenceUnary)
	instruction := OPPrefix
	instruction |= Bytecode(operator) << instructionShift
	// |- OpCode -|- Operator -|
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileBinary(compiler *Compiler, precedence byte) {
	// Left associative operators.
	operator := compiler.token.Type
	line := compiler.token.Line
	operatorPrecedence := compiler.rules[operator].precedence + 1
	forward(compiler)
	compileExpression(compiler, operatorPrecedence)
	// |- OpCode -|- operator -|
	instruction := OPBinop
	instruction |= Bytecode(operator) << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileEqual(compiler *Compiler, precedence byte) {
	// Left associative operators.
	operator := compiler.token.Type
	line := compiler.token.Line
	operatorPrecedence := compiler.rules[operator].precedence + 1
	forward(compiler)
	compileExpression(compiler, operatorPrecedence)
	// |- OpCode -|
	if operator == TKEqual {
		compiler.function.Code = append(compiler.function.Code, OPEqual)
	} else {
		compiler.function.Code = append(compiler.function.Code, OPNotEqual)
	}
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileNilChoice(compiler *Compiler, precedence byte) {
	// Left associative operators.
	operator := compiler.token.Type
	line := compiler.token.Line
	operatorPrecedence := compiler.rules[operator].precedence + 1
	forward(compiler)
	compileExpression(compiler, operatorPrecedence)
	// |- OpCode -|
	instruction := OPNilChoice
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compilePower(compiler *Compiler, precedence byte) {
	// Right assosiative operators.
	line := compiler.token.Line
	operatorPrecedende := compiler.rules[compiler.token.Type].precedence
	forward(compiler)
	compileExpression(compiler, operatorPrecedende)
	// |- OpCode -|- operator -|
	instruction := OPBinop
	instruction |= Bytecode(TKPower) << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileCall(compiler *Compiler, precedence byte) {
	line := compiler.token.Line
	forward(compiler)
	argCount := Bytecode(0)
	spread := Bytecode(0)
	if compiler.token.Type != TKRParen {
		if compiler.token.Type == TKIdentifier && compiler.next.Type == TKColon {
			check(compiler, TKIdentifier)
			emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
			forward(compiler)
			check(compiler, TKColon)
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			argCount += 2
			for compiler.token.Type == TKComma {
				forward(compiler)
				check(compiler, TKIdentifier)
				emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
				forward(compiler)
				check(compiler, TKColon)
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				argCount += 2
			}
		} else {
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			argCount++
			if compiler.token.Type == TK3Dots {
				check(compiler, TK3Dots)
				forward(compiler)
				spread = 1
				goto emitCall
			}
			for compiler.token.Type == TKComma {
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				argCount++
				if compiler.token.Type == TK3Dots {
					check(compiler, TK3Dots)
					forward(compiler)
					spread = 1
					goto emitCall
				}
			}
		}
	}
emitCall:
	check(compiler, TKRParen)
	emitCall(compiler, argCount, spread, line)
}

func compileSelector(compiler *Compiler, precedence byte) {
	// |- OpCode -|
	line := compiler.token.Line
	check(compiler, TKDot)
	forward(compiler)
	check(compiler, TKIdentifier)
	emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
	spread := Bytecode(0)
	if compiler.next.Type == TKLParen {
		forward(compiler)
		line := compiler.token.Line
		forward(compiler)
		argCount := Bytecode(0)
		if compiler.token.Type != TKRParen {
			if compiler.token.Type == TKIdentifier && compiler.next.Type == TKColon {
				check(compiler, TKIdentifier)
				emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
				forward(compiler)
				check(compiler, TKColon)
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				argCount += 2
				for compiler.token.Type == TKComma {
					forward(compiler)
					check(compiler, TKIdentifier)
					emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
					forward(compiler)
					check(compiler, TKColon)
					forward(compiler)
					compileExpression(compiler, PrecedenceNone)
					forward(compiler)
					argCount += 2
				}
			} else {
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				argCount++
				if compiler.token.Type == TK3Dots {
					check(compiler, TK3Dots)
					forward(compiler)
					spread = 1
					goto emitInvoke
				}
				for compiler.token.Type == TKComma {
					forward(compiler)
					compileExpression(compiler, PrecedenceNone)
					forward(compiler)
					argCount++
					if compiler.token.Type == TK3Dots {
						check(compiler, TK3Dots)
						forward(compiler)
						spread = 1
						goto emitInvoke
					}
				}
			}
		}
	emitInvoke:
		check(compiler, TKRParen)
		emitInvoke(compiler, argCount, spread, line)
	} else {
		instruction := OPSelect
		instruction |= OnlyExpression << instructionShift // Flag to be used in mutating operations.
		compiler.function.Code = append(compiler.function.Code, instruction)
		compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	}
}

func compileSubscript(compiler *Compiler, precedence byte) {
	// |- OpCode -|- Flag -|
	// Where Flag means:
	// OnlyExpression = [e]
	// ExprColonExpr = [e:e]
	// ExprColon = [e:]
	// ColonExpr = [:e]
	// OnlyColon = [:]
	instruction := OPSubscript
	line := compiler.token.Line
	check(compiler, TKLBracket)
	forward(compiler)
	if compiler.token.Type == TKColon {
		check(compiler, TKColon)
		forward(compiler)
		if compiler.token.Type == TKRBracket {
			instruction |= OnlyColon << instructionShift // Flag indicating [:] operations.
			goto assembleInst
		}
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		instruction |= ColonExpr << instructionShift // Flag indicating [:e] operations.
		goto assembleInst
	}
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	if compiler.token.Type == TKColon {
		check(compiler, TKColon)
		forward(compiler)
		if compiler.token.Type == TKRBracket {
			instruction |= ExprColon << instructionShift // Flag idicating [e:] operations.
			goto assembleInst
		}
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		instruction |= ExprColonExpr << instructionShift // Flag idicating [e:e] operations.
		goto assembleInst
	}
	instruction |= OnlyExpression << instructionShift // Flag indicatign [e] operations.
assembleInst:
	check(compiler, TKRBracket)
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileGroup(compiler *Compiler, precedence byte) {
	check(compiler, TKLParen)
	forward(compiler)
	compileExpression(compiler, PrecedenceNone)
	forward(compiler)
	check(compiler, TKRParen)
}

func compileListOrMap(compiler *Compiler, precedence byte) {
	check(compiler, TKLBracket)
	switch compiler.next.Type {
	case TKRBracket:
		// Empty List
		forward(compiler)
		emitList(compiler, 0, compiler.token.Line)
	case TKColon:
		// Empty Map
		forward(compiler)
		forward(compiler)
		check(compiler, TKRBracket)
		emitMap(compiler, 0, compiler.token.Line)
	case TKFor:
		line := compiler.token.Line
		childCompiler := newChildCompiler(compiler)
		beginScope(childCompiler)
		emitList(childCompiler, 0, line)
		addLocal(childCompiler, Token{Type: TKIdentifier, Description: comprehensionID, Line: line})
		forward(childCompiler)
		check(childCompiler, TKFor)
		forward(childCompiler)
		var jumpAddressCheck []int
		var loopOffset []int
		var localLoopHelper []loopInfo
		variableCount := Bytecode(0)
		loopOffset = append(loopOffset, len(localLoopHelper))
		localLoopHelper = append(localLoopHelper, loopInfo{})
		localLoopHelper[loopOffset[len(loopOffset)-1]].scopeDepth = childCompiler.scopeDepth
		beginScope(childCompiler)
		check(childCompiler, TKIdentifier)
		compileNil(childCompiler, PrecedenceNone)
		addLocal(childCompiler, childCompiler.token)
		forward(childCompiler)
		variableCount++
		for childCompiler.token.Type == TKComma {
			forward(childCompiler)
			check(childCompiler, TKIdentifier)
			compileNil(childCompiler, PrecedenceNone)
			addLocal(childCompiler, childCompiler.token)
			forward(childCompiler)
			variableCount++
		}
		check(childCompiler, TKIn)
		forward(childCompiler)
		compileExpression(childCompiler, PrecedenceNone)
		forward(childCompiler)
		addLocal(childCompiler, Token{Type: TKIdentifier, Description: iteratorID, Line: childCompiler.token.Line})
		localLoopHelper[loopOffset[len(loopOffset)-1]].loopAddress = Bytecode(len(childCompiler.function.Code))
		jumpAddressCheck = append(jumpAddressCheck, emitNext(childCompiler, line))
		if variableCount > 1 {
			emitUnpackFor(childCompiler, variableCount, line)
		}
		for childCompiler.token.Type == TKFor {
			check(childCompiler, TKFor)
			forward(childCompiler)
			variableCount := Bytecode(0)
			loopOffset = append(loopOffset, len(localLoopHelper))
			localLoopHelper = append(localLoopHelper, loopInfo{})
			localLoopHelper[loopOffset[len(loopOffset)-1]].scopeDepth = childCompiler.scopeDepth
			beginScope(childCompiler)
			check(childCompiler, TKIdentifier)
			compileNil(childCompiler, PrecedenceNone)
			addLocal(childCompiler, childCompiler.token)
			forward(childCompiler)
			variableCount++
			for childCompiler.token.Type == TKComma {
				forward(childCompiler)
				check(childCompiler, TKIdentifier)
				compileNil(childCompiler, PrecedenceNone)
				addLocal(childCompiler, childCompiler.token)
				forward(childCompiler)
				variableCount++
			}
			check(childCompiler, TKIn)
			forward(childCompiler)
			compileExpression(childCompiler, PrecedenceNone)
			forward(childCompiler)
			addLocal(childCompiler, Token{Type: TKIdentifier, Description: iteratorID, Line: childCompiler.token.Line})
			localLoopHelper[loopOffset[len(loopOffset)-1]].loopAddress = Bytecode(len(childCompiler.function.Code))
			jumpAddressCheck = append(jumpAddressCheck, emitNext(childCompiler, line))
			if variableCount > 1 {
				emitUnpackFor(childCompiler, variableCount, line)
			}
		}
		if childCompiler.token.Type == TKIf {
			check(childCompiler, TKIf)
			forward(childCompiler)
			compileExpression(childCompiler, PrecedenceNone)
			forward(childCompiler)
			jumpAddress := emitJumpIfFalse(childCompiler, line)
			// Compile expression
			check(childCompiler, TKRighArrow)
			forward(childCompiler)
			compileExpression(childCompiler, PrecedenceNone)
			forward(childCompiler)
			emitAppendList(childCompiler, Bytecode(resolveLocal(childCompiler, comprehensionID)), line)
			childCompiler.function.Code[jumpAddress] |= Bytecode(len(childCompiler.function.Code)) << instructionShift
			for i := len(jumpAddressCheck) - 1; i >= 0; i-- {
				childCompiler.function.Code[emitJump(childCompiler, line)] |= localLoopHelper[loopOffset[len(loopOffset)-1]].loopAddress << instructionShift
				afterLoopAddress := Bytecode(len(childCompiler.function.Code))
				childCompiler.function.Code[jumpAddressCheck[i]] |= afterLoopAddress << instructionShift
				if count := endScope(childCompiler); count != 0 {
					emitPop(childCompiler, count, line)
				}
				localLoopHelper = localLoopHelper[:loopOffset[len(loopOffset)-1]]
				loopOffset = loopOffset[:loopOffset[len(loopOffset)-1]]
			}
			emitGetLocal(childCompiler, Bytecode(resolveLocal(childCompiler, comprehensionID)), line)
			emitReturn(childCompiler, 1, line)
			endScope(childCompiler)
			updateCompiler(compiler, childCompiler)
			newLambda := childCompiler.function
			newLambda.Name = fmt.Sprintf("anonymous(%v)", lambdaID)
			newLambda.Arity = 0
			if childCompiler.canAccess {
				newLambda.CanAccessPrivateState = true
			}
			emitBuildClosure(compiler, newLambda, childCompiler.freeVariables, line)
			lambdaID++
		} else {
			check(childCompiler, TKRighArrow)
			forward(childCompiler)
			compileExpression(childCompiler, PrecedenceNone)
			forward(childCompiler)
			emitAppendList(childCompiler, Bytecode(resolveLocal(childCompiler, comprehensionID)), line)
			for i := len(jumpAddressCheck) - 1; i >= 0; i-- {
				childCompiler.function.Code[emitJump(childCompiler, line)] |= localLoopHelper[loopOffset[len(loopOffset)-1]].loopAddress << instructionShift
				afterLoopAddress := Bytecode(len(childCompiler.function.Code))
				childCompiler.function.Code[jumpAddressCheck[i]] |= afterLoopAddress << instructionShift
				if count := endScope(childCompiler); count != 0 {
					emitPop(childCompiler, count, line)
				}
				localLoopHelper = localLoopHelper[:loopOffset[len(loopOffset)-1]]
				loopOffset = loopOffset[:loopOffset[len(loopOffset)-1]]
			}
			emitGetLocal(childCompiler, Bytecode(resolveLocal(childCompiler, comprehensionID)), line)
			emitReturn(childCompiler, 1, line)
			endScope(childCompiler)
			updateCompiler(compiler, childCompiler)
			newLambda := childCompiler.function
			newLambda.Name = fmt.Sprintf("anonymous(%v)", lambdaID)
			newLambda.Arity = 0
			if childCompiler.canAccess {
				newLambda.CanAccessPrivateState = true
			}
			emitBuildClosure(compiler, newLambda, childCompiler.freeVariables, line)
			lambdaID++
		}
		check(compiler, TKRBracket)
		emitCall(compiler, 0, 0, line)
		jumpAddressCheck = nil
		loopOffset = nil
		localLoopHelper = nil
	default:
		line := compiler.token.Line
		length := Bytecode(0)
		forward(compiler)
		compileExpression(compiler, PrecedenceNone)
		forward(compiler)
		length++
		switch compiler.token.Type {
		case TKComma:
			for compiler.token.Type == TKComma {
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				length++
			}
			check(compiler, TKRBracket)
			emitList(compiler, length, line)
		case TKRBracket:
			check(compiler, TKRBracket)
			emitList(compiler, length, line)
		case TKColon:
			forward(compiler)
			compileExpression(compiler, PrecedenceNone)
			forward(compiler)
			for compiler.token.Type == TKComma {
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				check(compiler, TKColon)
				forward(compiler)
				compileExpression(compiler, PrecedenceNone)
				forward(compiler)
				length++
			}
			check(compiler, TKRBracket)
			emitMap(compiler, length, line)
		default:
			compilerPrintError("Expression", compiler.token, compiler.lexer.fileName)
		}
	}
}

func check(compiler *Compiler, tKind byte) {
	if compiler.token.Type != tKind {
		unexpectedTokenError(tKind, compiler.token, compiler.lexer.fileName)
	}
}

// forward update the compiler's state with the next token ahead.
func forward(compiler *Compiler) {
	compiler.token = compiler.next
	compiler.next = compiler.lexer.nextToken()
}

func unexpectedTokenError(expected byte, found Token, script string) {
	if len(found.Description) == 0 {
		fmt.Printf("\n   Syntax Error\n   Expected '%v' but got Token '%v'\n   %v:%3v\n\n", KindDescription[expected], KindDescription[found.Type], script, found.Line)
	} else {
		fmt.Printf("\n   Syntax Error\n   Expected '%v' but got Token of type '%v' with value '%v'\n   %v:%3v\n\n", KindDescription[expected], KindDescription[found.Type], found.Description, script, found.Line)
	}
	os.Exit(0)
}

func compilerPrintError(expected string, found Token, script string) {
	if len(found.Description) == 0 {
		fmt.Printf("\n   Syntax Error\n   Expected %v but got Token '%v'\n   %v:%3v\n\n", expected, KindDescription[found.Type], script, found.Line)
	} else {
		fmt.Printf("\n   Syntax Error\n   Expected %v but got Token type '%v' with value '%v'\n   %v:%3v\n\n", expected, KindDescription[found.Type], found.Description, script, found.Line)
	}
	os.Exit(0)
}

func argumentError(found Token, script string) {
	fmt.Printf("\n   Syntax Error\n   Function and param identifiers must be different\n   %v:%3v\n\n", script, found.Line)
	os.Exit(0)
}

func redefinedField(found Token, script string) {
	fmt.Printf("\n   Semantic Error\n   The name '%v' has already been defined in the Type/Extension context\n   %v:%3v\n\n", found.Description, script, found.Line)
	os.Exit(0)
}

func redefinedLocalError(found Token, script string) {
	fmt.Printf("\n   Syntax Error\n   Redefined local variable '%v'\n   %v:%3v\n\n", found.Description, script, found.Line)
	os.Exit(0)
}

func labelError(found Token, script string) {
	fmt.Printf("\n   Syntax Error\n   Label '%v' not defined\n   %v:%3v\n\n", found.Description, script, found.Line)
	os.Exit(0)
}

func checkSymmetry(idCount, exprCount int, line UInt32, script string) {
	if idCount != exprCount {
		fmt.Printf("\nSyntax Error\nCount of identifiers and expressions must match\n%v:%3v\n\n", script, line)
		os.Exit(0)
	}
}

func maxConstantsError(script string) {
	fmt.Printf("\n   Syntax Error\n   Too many constants. Max number of constants is %v\n   %v\n\n", maxConstants, script)
	os.Exit(0)
}

func maxGlobalsError(script string) {
	fmt.Printf("\n   Syntax Error\n   Too many globals. Max number of globals is %v\n   %v\n\n", maxGlobals, script)
	os.Exit(0)
}

func maxLocalsError(script string) {
	fmt.Printf("\n   Syntax Error\n   Too many locals. Max number of locals is %v\n   %v\n\n", maxLocals, script)
	os.Exit(0)
}

func maxFreeVarsError(script string) {
	fmt.Printf("\n   Syntax Error\n   Too many free variables. Max number of free vars is %v\n   %v\n\n", maxFreeVars, script)
	os.Exit(0)
}

func maxRHSExpressionsError(script string) {
	fmt.Printf("\n   Syntax Error\n   Too many rhs expressions. Max number of rhs expressions is %v\n   %v\n\n", maxRhsExpr, script)
	os.Exit(0)
}

func maxTypeExtensionError(script string) {
	fmt.Printf("\n   Semantic Error\n   Too many types in type extension declaration. Max number of types to extend at once is %v\n   %v\n\n", maxTypeExtensions, script)
	os.Exit(0)
}

func numberError(numberDescription string, line UInt32, script string) {
	fmt.Printf("\n   Syntax Error\n   Numeric literal %v is not a valid Number\n   %v:%3v\n\n", numberDescription, script, line)
	os.Exit(0)
}

func emitConstant(compiler *Compiler, value Value, line UInt32) {
	// |- OpCode -|- kIndex -|
	if compiler.kIndex < maxConstants {
		instruction := OPConst
		instruction |= compiler.kIndex << instructionShift
		compiler.function.Constants = append(compiler.function.Constants, value)
		compiler.function.Code = append(compiler.function.Code, instruction)
		compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
		compiler.kIndex++
	} else {
		maxConstantsError(compiler.lexer.fileName)
	}
}

func appendValue(compiler *Compiler, value Value) Bytecode {
	if compiler.kIndex < maxConstants {
		compiler.function.Constants = append(compiler.function.Constants, value)
		index := compiler.kIndex
		compiler.kIndex++
		return index
	}
	maxConstantsError(compiler.lexer.fileName)
	return 0
}

func emitClosure(compiler *Compiler, index Bytecode, line UInt32, collect bool) {
	// |- OpCode -|- kIndex -|- Collect FreeVars -|
	instruction := OPClosure
	instruction |= index << instructionShift
	if collect {
		instruction |= 1 << upperShift
	} else {
		instruction |= 0 << upperShift
	}
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitBuildStruct(compiler *Compiler, stuffCount Bytecode, line UInt32) {
	// |- OpCode -|- stuffCount -|
	instruction := OPStruct
	instruction |= stuffCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitExtension(compiler *Compiler, methodsCount, StructCount Bytecode, line UInt32) {
	// |- OpCode -|- methodsCount -|- StructCount -|
	instruction := OPExtension
	instruction |= methodsCount << instructionShift
	instruction |= StructCount << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitDerive(compiler *Compiler, deriveCount Bytecode, line UInt32) {
	// |- OpCode -|- DeriveCount -|
	instruction := OPDerive
	instruction |= deriveCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitList(compiler *Compiler, length, line UInt32) {
	// |- OpCode -|- Length -|
	instruction := OPList
	instruction |= length << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitMap(compiler *Compiler, length, line UInt32) {
	// |- OpCode -|- Length -|
	instruction := OPMap
	instruction |= (length * 2) << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitRecord(compiler *Compiler, length, line UInt32) {
	// |- OpCode -|- Length -|
	instruction := OPRecord
	instruction |= length << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitConstantNamespace(compiler *Compiler, constCount, line UInt32) {
	// |- OpCode -|- ConstCount -|
	instruction := OPConstNamespace
	instruction |= constCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitNewGlobal(compiler *Compiler, gIndex Bytecode, line UInt32) {
	// |- OpCode -|- Global Index-|
	if gIndex < maxGlobals {
		instruction := OPNewGlobal
		instruction |= gIndex << instructionShift
		compiler.function.Code = append(compiler.function.Code, instruction)
		compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	} else {
		maxGlobalsError(compiler.lexer.fileName)
	}
}

func emitGetGlobal(compiler *Compiler, gIndex, line UInt32) {
	// |- OpCode -|- Global Index -|
	instruction := OPGetGlobal
	instruction |= gIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitSetGlobal(compiler *Compiler, gIndex, line UInt32) {
	// |- OpCode -|- Global Index -|
	instruction := OPSetGlobal
	instruction |= gIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitCompoundSetGlobal(compiler *Compiler, gIndex, line UInt32, operatorCode Bytecode) {
	// |- OpCode -|- Global Index -|- operator -|
	instruction := OPCompSetGlobal
	instruction |= gIndex << instructionShift
	instruction |= operatorCode << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitMutDataStructure(compiler *Compiler, relativeIndex, flag, mutatingOperatorType, line UInt32) {
	// |- OpCode -|- Relative Expr Index -|- Flag -|- mutatingOperatorType -|
	// 1 - [e]
	// 2 - [e:e]
	// 3 - [e:]
	// 4 - [:e]
	// 5 - [:]
	instruction := OPMutDataStructure
	instruction |= relativeIndex << instructionShift
	flag |= mutatingOperatorType << 4
	instruction |= flag << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitCompoundMutDataStructure(compiler *Compiler, relativeIndex, line UInt32, operatorCode, selectorType, flag Bytecode) {
	// |- OpCode 8 bits -|- Relative Expr Index 16 bits -|- operator 8 bits tokens(19-29) (add, sub, etc) -|
	// |- Flag 8 bits -|- Selector Type 1 bit (0-1) ([], .) -|
	// Where Flag means:
	// 		OnlyExpression	= val[e] | val.prop
	// 		ExprColonExpr 	= [e:e]
	// 		ExprColon 		= [e:]
	// 		ColonExpr 		= [:e]
	// 		OnlyColon 		= [:]
	instruction := OPCompMutDataStructure
	instruction |= relativeIndex << instructionShift
	instruction |= operatorCode << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	instruction = flag
	instruction |= selectorType << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitJumpIfFalse(compiler *Compiler, line UInt32) int {
	// |- OpCode -|- Jump Address -|
	instruction := OPJumpIfFalse
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func emitJump(compiler *Compiler, line UInt32) int {
	// |- OpCode -|- Jump Address -|
	instruction := OPGoto
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func beginScope(compiler *Compiler) {
	compiler.scopeDepth++
}

func endScope(compiler *Compiler) Bytecode {
	compiler.scopeDepth--
	offset := 0
	for i := len(compiler.locals) - 1; i >= 0; i-- {
		if compiler.locals[i].depth > compiler.scopeDepth {
			offset++
		}
	}
	compiler.locals = compiler.locals[:len(compiler.locals)-offset]
	return Bytecode(offset)
}

func countLocalsUpTo(compiler *Compiler, depth Int32) Bytecode {
	offset := 0
	for i := len(compiler.locals) - 1; i >= 0; i-- {
		if compiler.locals[i].depth > depth {
			offset++
		}
	}
	return Bytecode(offset)
}

func addLocal(compiler *Compiler, token Token) {
	if len(compiler.locals) < maxLocals {
		for i := len(compiler.locals) - 1; i >= 0; i-- {
			if compiler.locals[i].depth != -1 && compiler.locals[i].depth < compiler.scopeDepth {
				break
			}
			if token.Description == compiler.locals[i].identifier {
				redefinedLocalError(token, compiler.lexer.fileName)
			}
		}
		compiler.locals = append(compiler.locals, Local{identifier: token.Description, depth: compiler.scopeDepth})
		return
	}
	maxLocalsError(compiler.lexer.fileName)
}

func emitGetLocal(compiler *Compiler, localIndex, line UInt32) {
	// |- OpCode -|- Local Index-|
	instruction := OPGetLocal
	instruction |= localIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitSetLocal(compiler *Compiler, localIndex, line UInt32) {
	// |- OpCode -|- Local Index-|
	instruction := OPSetLocal
	instruction |= localIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitCompoundSetLocal(compiler *Compiler, localIndex, line UInt32, operatorCode Bytecode) {
	// |- OpCode -|- Local Index-|- operator -|
	instruction := OPCompSetLocal
	instruction |= localIndex << instructionShift
	instruction |= operatorCode << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func resolveLocal(compiler *Compiler, identifier string) Int32 {
	for i := len(compiler.locals) - 1; i >= 0; i-- {
		if compiler.locals[i].identifier == identifier {
			return Int32(i)
		}
	}
	return -1
}

func resolveFreeVar(compiler *Compiler, identifier string, line UInt32) Int32 {
	if compiler.parent == nil {
		return -1
	}
	if index := resolveLocal(compiler.parent, identifier); index != -1 {
		return addFreeVar(compiler, Bytecode(index), line, true)
	}
	if freeVarIndex := resolveFreeVar(compiler.parent, identifier, line); freeVarIndex != -1 {
		return addFreeVar(compiler, Bytecode(freeVarIndex), line, false)
	}
	return -1
}

func addFreeVar(compiler *Compiler, index, line UInt32, isLocal bool) Int32 {
	if len(compiler.freeVariables) < maxFreeVars {
		for i := Bytecode(0); i < compiler.function.FreeVarCount; i++ {
			freeVar := compiler.freeVariables[i]
			if freeVar.index == index && freeVar.isLocal == isLocal {
				return Int32(i)
			}
		}
		upIndex := len(compiler.freeVariables)
		compiler.freeVariables = append(compiler.freeVariables, CompFreeVar{index: index, isLocal: isLocal})
		compiler.function.FreeVarCount++
		return Int32(upIndex)
	}
	maxFreeVarsError(compiler.lexer.fileName)
	return 0
}

func emitFreeVar(compiler *Compiler, index, line UInt32, isLocal bool) {
	// |- OpCode -|- index -|- isLocal -|
	instruction := OPFreeVariable
	instruction |= index << instructionShift
	if isLocal {
		instruction |= 1 << upperShift
	} else {
		instruction |= 0 << upperShift
	}
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitGetFreeVar(compiler *Compiler, index, line UInt32) {
	// |- OpCode -|- Local Index -|
	instruction := OPGetFreeVariable
	instruction |= index << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitSetFreeVar(compiler *Compiler, index, line UInt32) {
	// |- OpCode -|- FreeVar Index -|
	instruction := OPSetFreeVariable
	instruction |= index << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitCompoundSetFreeVar(compiler *Compiler, index, line UInt32, operatorCode Bytecode) {
	// |- OpCode -|- FreeVar Index -|- operator code -|
	instruction := OPCompSetFreeVariable
	instruction |= index << instructionShift
	instruction |= operatorCode << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitRange(compiler *Compiler, argCount, line Bytecode) {
	// |- OpCode -|- ArgCount -|
	instruction := OPRange
	instruction |= argCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitNext(compiler *Compiler, line UInt32) int {
	// |- OpCode -|- JumpAddress -|
	instruction := OPNext
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func emitMatch(compiler *Compiler, line UInt32) int {
	// |- OpCode -|- Jumpt Address if match -|
	instruction := OPMatch
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func emitCall(compiler *Compiler, argCount, spread, line UInt32) {
	// |- OpCode -|- ArgCount -|- SpreadFlag -|
	instruction := OPCall
	instruction |= argCount << instructionShift
	instruction |= spread << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitInvoke(compiler *Compiler, argCount, spread, line UInt32) {
	// |- OpCode -|- ArgCount -|- SpreadFlag -|
	instruction := OPInvokeMethod
	instruction |= argCount << instructionShift
	instruction |= spread << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitReturn(compiler *Compiler, lengthReturnedvalues, line UInt32) {
	// |- OpCode -|- Number elements to return -|
	instruction := OPReturn
	instruction |= lengthReturnedvalues << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitUnpack(compiler *Compiler, idCount, line UInt32) {
	// |- OpCode -|- TokensCount -|
	instruction := OPUnpack
	instruction |= idCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitUnpackFor(compiler *Compiler, idCount, line UInt32) {
	// |- OpCode -|- TokensCount -|
	instruction := OPUnpackFor
	instruction |= idCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitNilReturn(compiler *Compiler, line UInt32) {
	// |- OpCode -|- Number elements to return -|
	compiler.function.Code = append(compiler.function.Code, OPNil)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	instruction := OPReturn
	instruction |= 1 << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitAppendList(compiler *Compiler, listAddress Bytecode, line UInt32) {
	// |- OpCode -|- ListAddress -|
	instruction := OPAppend
	instruction |= listAddress << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitEndScript(compiler *Compiler, line UInt32) {
	if compiler.hasDefer {
		// |- OpCode -|
		instruction := OPRunDefer
		compiler.function.Code = append(compiler.function.Code, instruction)
		compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	}
	// |- OpCode -|
	compiler.function.Code = append(compiler.function.Code, OPEnd)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = compiler.token.Line
}

func emitPop(compiler *Compiler, count, line UInt32) {
	// |- OpCode -|
	instruction := OPPop
	instruction |= count << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func newDeferLambda(argCount Bytecode, modName string, line UInt32, opcode Bytecode) Closure {
	newLambda := Function{Lines: make(map[Bytecode]UInt32), ModuleName: modName}
	instruction := opcode
	instruction |= argCount << instructionShift
	newLambda.Code = append(newLambda.Code, instruction)
	newLambda.Lines[Bytecode(len(newLambda.Code)-1)] = line
	instruction = OPReturn
	instruction |= 1 << instructionShift
	newLambda.Code = append(newLambda.Code, instruction)
	newLambda.Lines[Bytecode(len(newLambda.Code)-1)] = line
	newLambda.Name = fmt.Sprintf("deferGFunction %v", lambdaID)
	lambdaID++
	return Closure{Function: &newLambda}
}

func cleanUp(compiler *Compiler) {
	compiler.lexer.input = nil
	compiler.lexer = nil
	compiler.gIDPos = nil
	compiler.locals = nil
	compiler.rules = nil
}

func createRules() []ParseRule {
	return []ParseRule{
		TKEof:             {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKComment:         {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKLet:             {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKIf:              {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKElse:            {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKFor:             {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKIn:              {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKRange:           {prefix: compileRange, infix: nil, precedence: PrecedenceNone},
		TKBreak:           {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKContinue:        {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKSwitch:          {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKCase:            {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKDefault:         {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKLabel:           {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKFunction:        {prefix: compileAnonymousFunction, infix: nil, precedence: PrecedenceNone},
		TKReturn:          {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKIdentifier:      {prefix: compileIdentifier, infix: nil, precedence: PrecedenceNone},
		TKEquation:        {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKInteger:         {prefix: compileInteger, infix: nil, precedence: PrecedenceNone},
		TKUInt:            {prefix: compileUInt, infix: nil, precedence: PrecedenceNone},
		TKBig:             {prefix: compileBig, infix: nil, precedence: PrecedenceNone},
		TKFloat:           {prefix: compileFloat, infix: nil, precedence: PrecedenceNone},
		TKRational:        {prefix: compileRational, infix: nil, precedence: PrecedenceNone},
		TKComplex:         {prefix: compileComplex, infix: nil, precedence: PrecedenceNone},
		TKTrue:            {prefix: compileTrue, infix: nil, precedence: PrecedenceNone},
		TKFalse:           {prefix: compileFalse, infix: nil, precedence: PrecedenceNone},
		TKNil:             {prefix: compileNil, infix: nil, precedence: PrecedenceNone},
		TKRune:            {prefix: compileRune, infix: nil, precedence: PrecedenceNone},
		TKString:          {prefix: compileString, infix: nil, precedence: PrecedenceNone},
		TK3Dots:           {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKAdd:             {prefix: compilePrefix, infix: compileBinary, precedence: PrecedenceAdditive},
		TKMinus:           {prefix: compilePrefix, infix: compileBinary, precedence: PrecedenceAdditive},
		TKMul:             {prefix: nil, infix: compileBinary, precedence: PrecedenceMultiplicative},
		TKDiv:             {prefix: nil, infix: compileBinary, precedence: PrecedenceMultiplicative},
		TKMod:             {prefix: nil, infix: compileBinary, precedence: PrecedenceMultiplicative},
		TKPercent:         {prefix: nil, infix: compileBinary, precedence: PrecedenceMultiplicative},
		TKPower:           {prefix: nil, infix: compilePower, precedence: PrecedenceExp},
		TKAmpersand:       {prefix: nil, infix: compileBinary, precedence: PrecedenceBitAnd},
		TKBar:             {prefix: nil, infix: compileBinary, precedence: PrecedenceBitOr},
		TKHat:             {prefix: nil, infix: compileBinary, precedence: PrecedenceBitXor},
		TKLShift:          {prefix: nil, infix: compileBinary, precedence: PrecedenceShift},
		TKRShift:          {prefix: nil, infix: compileBinary, precedence: PrecedenceShift},
		TKEqual:           {prefix: nil, infix: compileEqual, precedence: PrecedenceEquality},
		TKNEqual:          {prefix: nil, infix: compileEqual, precedence: PrecedenceEquality},
		TKGT:              {prefix: nil, infix: compileBinary, precedence: PrecedenceRelational},
		TKGE:              {prefix: nil, infix: compileBinary, precedence: PrecedenceRelational},
		TKLT:              {prefix: nil, infix: compileBinary, precedence: PrecedenceRelational},
		TKLE:              {prefix: nil, infix: compileBinary, precedence: PrecedenceRelational},
		TKAnd:             {prefix: nil, infix: compileBinary, precedence: PrecedenceAnd},
		TKOr:              {prefix: nil, infix: compileBinary, precedence: PrecedenceOr},
		TKNot:             {prefix: compilePrefix, infix: nil, precedence: PrecedenceNone},
		TKTilde:           {prefix: compilePrefix, infix: nil, precedence: PrecedenceNone},
		TKComma:           {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKColon:           {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKDot:             {prefix: nil, infix: compileSelector, precedence: PrecedencePosfix},
		TKLParen:          {prefix: compileGroup, infix: compileCall, precedence: PrecedencePosfix},
		TKRParen:          {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKLBracket:        {prefix: compileListOrMap, infix: compileSubscript, precedence: PrecedencePosfix},
		TKRBracket:        {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKLCurly:          {prefix: compileRecord, infix: nil, precedence: PrecedenceNone},
		TKRCurly:          {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKQuestion:        {prefix: nil, infix: compileNilChoice, precedence: PrecedenceNilChoice},
		TKRighArrow:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKStruct:          {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKPublic:          {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKExtension:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKDefer:           {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKAddAssign:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKSubAssign:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKMulAssign:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKDivAssign:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKRemAssign:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKPowAssign:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKBitAndAssign:    {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKBitOrAssign:     {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKBitXorAssign:    {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKBitLShiftAssign: {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKBitRShiftAssign: {prefix: nil, infix: nil, precedence: PrecedenceNone},
		TKUndefined:       {prefix: nil, infix: nil, precedence: PrecedenceNone},
	}
}
