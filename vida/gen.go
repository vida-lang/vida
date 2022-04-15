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
	tokens           []token
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

// localVariable models local variables.
type localVariable struct {
	identifier string
	depth      Int32
}

// freeVar represents a local or global value in some enclosing environment at compile time.
type freeVar struct {
	index   UInt32
	isLocal bool
}

// parseRule is the struct for parsing expressions.
type parseRule struct {
	prefix     parseFn
	infix      parseFn
	precedence byte
}

// parseFn is the generic function for parsing expressions.
type parseFn func(compiler *compiler, precedence byte)

// Operator Precedences
const (
	precedenceNone           byte = iota
	precedenceNilChoice           // ?
	precedenceOr                  // or
	precedenceAnd                 // and
	precedenceEquality            // == !=
	precedenceRelational          // > >= < <=
	precedenceRange               // range
	precedenceAdditive            // + -
	precedenceMultiplicative      // * / mod %
	precedenceBitOr               // |
	precedenceBitXor              // ^
	precedenceBitAnd              // &
	precedenceShift               // << >>
	precedenceUnary               // - not ~ try : Right to Left Associativiy
	precedenceExp                 // ** Right to Left Associativiy
	precedencePosfix              // [] . ()
	precedencePrimary             //
)

// compiler is the structure for saving partial state while performing syntax analysis and emitting machine code.
type compiler struct {
	parent        *compiler
	lexer         *lexer
	token         token
	next          token
	rules         []parseRule
	function      Function
	kIndex        Bytecode
	gIndex        Bytecode
	gIDPos        map[string]Bytecode
	gID           []string
	locals        []localVariable
	scopeDepth    Int32
	freeVariables []freeVar
	canAccess     bool
	hasDefer      bool
}

func newCompiler(lexer *lexer, rules []parseRule) *compiler {
	return &compiler{
		lexer:    lexer,
		rules:    rules,
		function: Function{Lines: make(map[Bytecode]UInt32), ModuleName: lexer.fileName},
		gIDPos:   make(map[string]Bytecode),
	}
}

func newChildCompiler(parent *compiler) *compiler {
	return &compiler{
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

func newReplCompiler(moduleName string, buffer *bytes.Buffer) *compiler {
	compiler := &compiler{
		lexer:    newLexer(buffer, moduleName),
		rules:    parseRules,
		function: Function{Name: mainFunctionName, Lines: make(map[Bytecode]UInt32), ModuleName: moduleName},
		gIDPos:   make(map[string]Bytecode),
	}
	return compiler
}

func (c *compiler) getFunctionPointer() *Function {
	return &c.function
}

func (c *compiler) getIdentifiers() []string {
	return c.gID
}

func compileCodeForRepl(compiler *compiler) {
	for compiler.token.Type != tokEOF {
		switch compiler.token.Type {
		case tokLet:
			compileDeclaration(compiler)
		case tokIdentifier:
			if compiler.next.Type == tokEquation || compiler.next.Type == tokComma {
				compileAssignment(compiler)
			} else {
				compileREPLExpression(compiler)
			}
		case tokStruct:
			compileStruct(compiler)
		case tokFunction:
			compileClosure(compiler)
		case tokConst:
			compileConstNamespace(compiler)
		case tokExtension:
			compileExtension(compiler)
		case tokIf:
			compileIf(compiler, false)
		case tokFor:
			compileFor(compiler)
		case tokLabel:
			switch compiler.next.Type {
			case tokFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(compiler, false)
		case tokDefer:
			compileDefer(compiler)
		case tokLCurly:
			compileBlock(compiler, false)
		default:
			compileREPLExpression(compiler)
		}
	}
	emitEndScript(compiler, compiler.token.Line)
}

func compileREPLExpression(compiler *compiler) {
	wildcard, printDesc := "_", "print"
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	pos, _ := compiler.gIDPos[wildcard]
	emitSetGlobal(compiler, pos, compiler.lexer.line)
	if ppos, exists := compiler.gIDPos[printDesc]; exists {
		emitGetGlobal(compiler, ppos, compiler.lexer.line)
	} else {
		compiler.gIDPos[printDesc] = compiler.gIndex
		compiler.gID = append(compiler.gID, printDesc)
		emitGetGlobal(compiler, compiler.gIndex, compiler.lexer.line)
		compiler.gIndex++
	}
	pos, _ = compiler.gIDPos[wildcard]
	emitGetGlobal(compiler, pos, compiler.lexer.line)
	emitCall(compiler, 1, 0, compiler.lexer.line)
	emitPop(compiler, 1, compiler.lexer.line)
}

func updateCompiler(parent, child *compiler) {
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
func buildModule(input *bytes.Buffer, fileName string) *VModule {
	compiler := newCompiler(newLexer(input, fileName), parseRules)
	compiler.function.Name = mainFunctionName
	forward(compiler)
	forward(compiler)
	for compiler.token.Type != tokEOF {
		switch compiler.token.Type {
		case tokLet:
			compileDeclaration(compiler)
		case tokIdentifier:
			compileAssignment(compiler)
		case tokStruct:
			compileStruct(compiler)
		case tokFunction:
			compileClosure(compiler)
		case tokConst:
			compileConstNamespace(compiler)
		case tokExtension:
			compileExtension(compiler)
		case tokIf:
			compileIf(compiler, false)
		case tokFor:
			compileFor(compiler)
		case tokLabel:
			switch compiler.next.Type {
			case tokFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(compiler, false)
		case tokDefer:
			compileDefer(compiler)
		case tokLCurly:
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
func compileDeclaration(compiler *compiler) {
	line := compiler.token.Line
	check(compiler, tokLet)
	forward(compiler)
	assHelperOffset := len(assHelper)
	assHelper = append(assHelper, assignmentHelper{})
	check(compiler, tokIdentifier)
	assHelper[assHelperOffset].tokens = append(assHelper[assHelperOffset].tokens, compiler.token)
	forward(compiler)
	for compiler.token.Type == tokComma {
		forward(compiler)
		check(compiler, tokIdentifier)
		assHelper[assHelperOffset].tokens = append(assHelper[assHelperOffset].tokens, compiler.token)
		forward(compiler)
	}
	check(compiler, tokEquation)
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	assHelper[assHelperOffset].expressionsCount++
	for compiler.token.Type == tokComma {
		forward(compiler)
		compileExpression(compiler, precedenceNone)
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
func declareNewLocals(compiler *compiler, assHelperOffset, tokensCount int) {
	for i := 0; i < tokensCount; i++ {
		addLocal(compiler, assHelper[assHelperOffset].tokens[i])
	}
}

// Declare new globals.
func declareNewGlobals(compiler *compiler, assHelperOffset, tokensCount int, line UInt32) {
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
func compileAssignment(compiler *compiler) {
	assHelperOffset := len(assHelper)
	assHelper = append(assHelper, assignmentHelper{operatorType: map[int]byte{}, indexType: map[int]uint32{}})
	check(compiler, tokIdentifier)
	if compiler.next.Type == tokLBracket || compiler.next.Type == tokDot || compiler.next.Type == tokLParen {
		line := compiler.token.Line
		compileExpression(compiler, precedenceNone)
		if compiler.token.Type == tokRParen {
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
	for compiler.token.Type == tokComma {
		forward(compiler)
		check(compiler, tokIdentifier)
		if compiler.next.Type == tokLBracket || compiler.next.Type == tokDot || compiler.next.Type == tokLParen {
			compileExpression(compiler, precedenceNone)
			if compiler.token.Type == tokRParen {
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
	case tokEquation:
		compileSimpleAssignment(compiler, compiler.token.Line, assHelperOffset)
	case tokAddAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKAdd)
	case tokSubAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKMinus)
	case tokMulAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKMul)
	case tokDivAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKDiv)
	case tokRemAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKPercent)
	case tokPowAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKPower)
	case tokBitAndAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKAmpersand)
	case tokBitOrAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKBar)
	case tokBitXorAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKHat)
	case tokBitLShiftAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKLShift)
	case tokBitRShiftAssign:
		compileCompoundAssignment(compiler, compiler.token.Line, assHelperOffset, compiler.token.Type, TKRShift)
	default:
		compilerPrintError("Assignment", compiler.token, compiler.lexer.fileName)
	}
}

// This function compiles simple assignments. A simple assignment has the form <idLst> = <exprList>
func compileSimpleAssignment(compiler *compiler, line Bytecode, assHelperOffset int) {
	check(compiler, tokEquation)
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	assHelper[assHelperOffset].expressionsCount++
	for compiler.token.Type == tokComma {
		forward(compiler)
		compileExpression(compiler, precedenceNone)
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

func emitSimpleAssignment(compiler *compiler, lvaluesCount, assHelperOffset, tokensIndex int, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == tokRBracket {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], mutatingOperatorSubscript, line)
			} else {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], mutatingOperatorSelector, line)
			}
		} else {
			emitGlobalAssignment(compiler, assHelperOffset, tokensIndex, line)
			tokensIndex--
		}
	}
}

func emitSimpleLocalAssignment(compiler *compiler, lvaluesCount, assHelperOffset, tokensIndex int, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == tokRBracket {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], mutatingOperatorSubscript, line)
			} else {
				emitMutDataStructure(compiler, Bytecode(j), assHelper[assHelperOffset].indexType[j], mutatingOperatorSelector, line)
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

func emitGlobalAssignment(compiler *compiler, assHelperOffset, tokensIndex int, line UInt32) {
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
func compileCompoundAssignment(compiler *compiler, line Bytecode, assHelperOffset int, operatorType, operator byte) {
	check(compiler, operatorType)
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	assHelper[assHelperOffset].expressionsCount++
	for compiler.token.Type == tokComma {
		forward(compiler)
		compileExpression(compiler, precedenceNone)
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

func emitCompoundAssignment(compiler *compiler, lvaluesCount, assHelperOffset, tokensIndex int, operatorType byte, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == tokRBracket {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), mutatingOperatorSubscript, assHelper[assHelperOffset].indexType[j])
			} else {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), mutatingOperatorSelector, assHelper[assHelperOffset].indexType[j])
			}
		} else {
			emitCompoundGlobalAssignment(compiler, assHelperOffset, tokensIndex, operatorType, line)
			tokensIndex--
		}
	}
}

func emitCompoundLocalAssignment(compiler *compiler, lvaluesCount, assHelperOffset, tokensIndex int, operatorType byte, line UInt32) {
	for j := lvaluesCount - 1; j >= 0; j-- {
		if assHelper[assHelperOffset].isCollection[j] {
			if assHelper[assHelperOffset].operatorType[j] == tokRBracket {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), mutatingOperatorSubscript, assHelper[assHelperOffset].indexType[j])
			} else {
				emitCompoundMutDataStructure(compiler, Bytecode(j), line, Bytecode(operatorType), mutatingOperatorSelector, assHelper[assHelperOffset].indexType[j])
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

func emitCompoundGlobalAssignment(compiler *compiler, assHelperOffset, tokensIndex int, operatorType byte, line UInt32) {
	if pos, exists := compiler.gIDPos[assHelper[assHelperOffset].tokens[tokensIndex].Description]; exists {
		emitCompoundSetGlobal(compiler, pos, line, Bytecode(operatorType))
	} else {
		compiler.gIDPos[assHelper[assHelperOffset].tokens[tokensIndex].Description] = compiler.gIndex
		compiler.gID = append(compiler.gID, assHelper[assHelperOffset].tokens[tokensIndex].Description)
		emitCompoundSetGlobal(compiler, compiler.gIndex, line, Bytecode(operatorType))
		compiler.gIndex++
	}
}

func compileStruct(compiler *compiler) {
	line := compiler.token.Line
	check(compiler, tokStruct)
	forward(compiler)
	check(compiler, tokIdentifier)
	typeNameToken := compiler.token
	forward(compiler)
	propertiesHelper := make(Namespace)
	methodsHelper := make(Namespace)
	hasDeriving := false
	derivationCount := Bytecode(0)
	if compiler.token.Type == TKLT {
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		hasDeriving = true
		derivationCount++
		for compiler.token.Type == tokComma {
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			derivationCount++
		}
	}
	check(compiler, tokLCurly)
	forward(compiler)
	newStruct := Struct{Id: globalStructUniqueID, Name: typeNameToken.Description, Methods: make(Namespace), Public: make(Namespace), Private: make(Namespace)}
	globalStructUniqueID++
	propertiesHelper[typeNameToken.Description] = NilValue
	stuffCount := Bytecode(0)
	for compiler.token.Type != tokRCurly {
		switch compiler.token.Type {
		case tokIdentifier:
			check(compiler, tokIdentifier)
			if _, exists := propertiesHelper[compiler.token.Description]; exists {
				redefinedField(compiler.token, compiler.lexer.fileName)
			}
			propertiesHelper[compiler.token.Description] = NilValue
			newStruct.Private[compiler.token.Description] = NilValue
			forward(compiler)
			for compiler.token.Type == tokIdentifier {
				check(compiler, tokIdentifier)
				if _, exists := propertiesHelper[compiler.token.Description]; exists {
					redefinedField(compiler.token, compiler.lexer.fileName)
				}
				propertiesHelper[compiler.token.Description] = NilValue
				newStruct.Private[compiler.token.Description] = NilValue
				forward(compiler)
			}
		case tokPublic:
			check(compiler, tokPublic)
			forward(compiler)
			check(compiler, tokIdentifier)
			if _, exists := propertiesHelper[compiler.token.Description]; exists {
				redefinedField(compiler.token, compiler.lexer.fileName)
			}
			propertiesHelper[compiler.token.Description] = NilValue
			newStruct.Public[compiler.token.Description] = NilValue
			forward(compiler)
			for compiler.token.Type == tokIdentifier {
				check(compiler, tokIdentifier)
				if _, exists := propertiesHelper[compiler.token.Description]; exists {
					redefinedField(compiler.token, compiler.lexer.fileName)
				}
				propertiesHelper[compiler.token.Description] = NilValue
				newStruct.Public[compiler.token.Description] = NilValue
				forward(compiler)
			}
		case tokFunction:
			check(compiler, tokFunction)
			forward(compiler)
			check(compiler, tokIdentifier)
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
	check(compiler, tokRCurly)
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

func compileMethod(compiler *compiler, methodIDToken token) {
	isvararg := false
	arity := Bytecode(0)
	paramsMap := make(map[string]struct{})
	var params []token
	if compiler.token.Type == tok3Dots {
		check(compiler, tok3Dots)
		forward(compiler)
		check(compiler, tokIdentifier)
		params = append(params, compiler.token)
		forward(compiler)
		isvararg = true
	} else {
		for compiler.token.Type != tokLCurly && compiler.token.Type != tok3Dots {
			check(compiler, tokIdentifier)
			if _, isParam := paramsMap[compiler.token.Description]; isParam || compiler.token.Description == methodIDToken.Description {
				argumentError(compiler.token, compiler.lexer.fileName)
			} else {
				paramsMap[compiler.token.Description] = struct{}{}
				params = append(params, compiler.token)
			}
			forward(compiler)
			arity++
		}
		if compiler.token.Type == tok3Dots {
			check(compiler, tok3Dots)
			forward(compiler)
			check(compiler, tokIdentifier)
			params = append(params, compiler.token)
			forward(compiler)
			isvararg = true
		}
	}
	check(compiler, tokLCurly)
	forward(compiler)
	newFunction, freeVariables := buildMethodBody(compiler, params)
	newFunction.Name = methodIDToken.Description
	newFunction.Arity = arity
	newFunction.Vararg = isvararg
	emitBuildClosure(compiler, newFunction, freeVariables, methodIDToken.Line)
	check(compiler, tokRCurly)
	forward(compiler)
	paramsMap = nil
	params = nil
	freeVariables = nil
}

func buildMethodBody(compiler *compiler, params []token) (Function, []freeVar) {
	childCompiler := newChildCompiler(compiler)
	childCompiler.canAccess = true
	beginScope(childCompiler)
	for _, tok := range params {
		addLocal(childCompiler, tok)
	}
	for childCompiler.token.Type != tokEOF && childCompiler.token.Type != tokRCurly {
		switch childCompiler.token.Type {
		case tokLet:
			compileDeclaration(childCompiler)
		case tokIdentifier:
			compileAssignment(childCompiler)
		case tokStruct:
			compileStruct(childCompiler)
		case tokFunction:
			compileClosure(childCompiler)
		case tokIf:
			compileIf(childCompiler, false)
		case tokFor:
			compileFor(childCompiler)
		case tokLabel:
			switch childCompiler.next.Type {
			case tokFor:
				compileFor(childCompiler)
			default:
				compilerPrintError("Statement", childCompiler.next, childCompiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(childCompiler, false)
		case tokReturn:
			compileReturn(childCompiler)
		case tokDefer:
			compileDefer(childCompiler)
		case tokExtension:
			compileExtension(childCompiler)
		case tokLCurly:
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

func compileExtension(compiler *compiler) {
	line := compiler.token.Line
	check(compiler, tokExtension)
	forward(compiler)
	typeCount := Bytecode(0)
	fnNames := make(Namespace)
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	typeCount++
	for compiler.token.Type == tokComma {
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		typeCount++
	}
	check(compiler, tokLCurly)
	forward(compiler)
	methodCount := Bytecode(0)
	for compiler.token.Type == tokFunction {
		check(compiler, tokFunction)
		forward(compiler)
		check(compiler, tokIdentifier)
		methodIDToken := compiler.token
		forward(compiler)
		if _, exists := fnNames[methodIDToken.Description]; exists {
			redefinedField(compiler.token, compiler.lexer.fileName)
		}
		fnNames[methodIDToken.Description] = NilValue
		compileMethod(compiler, methodIDToken)
		methodCount++
	}
	check(compiler, tokRCurly)
	forward(compiler)
	if typeCount > maxTypeExtensions {
		maxTypeExtensionError(compiler.lexer.fileName)
	}
	emitExtension(compiler, methodCount, typeCount, line)
	fnNames = nil
}

func compileConstNamespace(compiler *compiler) {
	line := compiler.token.Line
	forward(compiler)
	check(compiler, tokIdentifier)
	name := compiler.token.Description
	forward(compiler)
	check(compiler, tokLCurly)
	forward(compiler)
	indexer := Bytecode(0)
	constNamespace := NamedConstants{Name: name, Constants: make(Namespace), Indexes: make(map[Bytecode]string)}
	if compiler.token.Type == tokIdentifier {
		switch compiler.next.Type {
		case tokIdentifier:
			check(compiler, tokIdentifier)
			constId := compiler.token
			forward(compiler)
			if compiler.token.Description == constInit {
				forward(compiler)
				check(compiler, tokEquation)
				forward(compiler)
				check(compiler, tokInteger)
				ordinal := Int(0)
				if value, err := strconv.ParseInt(compiler.token.Description, 0, 64); err == nil {
					ordinal = Int(value)
				} else {
					numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
				}
				constNamespace.Constants[constId.Description] = ordinal
				forward(compiler)
				for compiler.token.Type == tokIdentifier {
					ordinal++
					constNamespace.Constants[compiler.token.Description] = ordinal
					forward(compiler)
				}
			} else {
				ordinal := Int(0)
				constNamespace.Constants[constId.Description] = ordinal
				for compiler.token.Type == tokIdentifier {
					ordinal++
					constNamespace.Constants[compiler.token.Description] = ordinal
					forward(compiler)
				}
			}
			check(compiler, tokRCurly)
			forward(compiler)
			emitConstant(compiler, constNamespace, line)
		case tokEquation:
			constId := compiler.token
			forward(compiler)
			check(compiler, tokEquation)
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			if _, exists := constNamespace.Constants[constId.Description]; exists || constId.Description == name {
				redefinedField(constId, compiler.lexer.fileName)
			} else {
				indexer++
				constNamespace.Constants[constId.Description] = NilValue
				constNamespace.Indexes[indexer] = constId.Description
			}
			for compiler.token.Type == tokIdentifier {
				constId := compiler.token
				forward(compiler)
				check(compiler, tokEquation)
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				if _, exists := constNamespace.Constants[constId.Description]; exists || constId.Description == name {
					redefinedField(constId, compiler.lexer.fileName)
				} else {
					indexer++
					constNamespace.Constants[constId.Description] = NilValue
					constNamespace.Indexes[indexer] = constId.Description
				}
			}
			check(compiler, tokRCurly)
			forward(compiler)
			emitConstant(compiler, constNamespace, line)
			emitConstantNamespace(compiler, indexer, line)
		case tokRCurly:
			constNamespace.Constants[compiler.token.Description] = Int(0)
			forward(compiler)
			check(compiler, tokRCurly)
			forward(compiler)
			emitConstant(compiler, constNamespace, line)
		default:
			compilerPrintError("Identifier or }", compiler.next, compiler.lexer.fileName)
		}
	} else {
		check(compiler, tokRCurly)
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

func compileClosure(compiler *compiler) {
	line := compiler.token.Line
	check(compiler, tokFunction)
	forward(compiler)
	check(compiler, tokIdentifier)
	functionIDToken := compiler.token
	forward(compiler)
	isvararg := false
	arity := Bytecode(0)
	paramsMap := make(map[string]struct{})
	var params []token
	if compiler.token.Type == tok3Dots {
		check(compiler, tok3Dots)
		forward(compiler)
		check(compiler, tokIdentifier)
		params = append(params, compiler.token)
		forward(compiler)
		isvararg = true
	} else {
		for compiler.token.Type != tokLCurly && compiler.token.Type != tok3Dots {
			check(compiler, tokIdentifier)
			if _, isParam := paramsMap[compiler.token.Description]; isParam || compiler.token.Description == functionIDToken.Description {
				argumentError(compiler.token, compiler.lexer.fileName)
			} else {
				paramsMap[compiler.token.Description] = struct{}{}
				params = append(params, compiler.token)
			}
			forward(compiler)
			arity++
		}
		if compiler.token.Type == tok3Dots {
			check(compiler, tok3Dots)
			forward(compiler)
			check(compiler, tokIdentifier)
			params = append(params, compiler.token)
			forward(compiler)
			isvararg = true
		}
	}
	check(compiler, tokLCurly)
	forward(compiler)
	newFunction, freeVariables := buildFunctionBody(compiler, params)
	newFunction.Name = functionIDToken.Description
	newFunction.Arity = arity
	newFunction.Vararg = isvararg
	emitBuildClosure(compiler, newFunction, freeVariables, functionIDToken.Line)
	check(compiler, tokRCurly)
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

func buildFunctionBody(compiler *compiler, params []token) (Function, []freeVar) {
	childCompiler := newChildCompiler(compiler)
	beginScope(childCompiler)
	for _, tok := range params {
		addLocal(childCompiler, tok)
	}
	for childCompiler.token.Type != tokEOF && childCompiler.token.Type != tokRCurly {
		switch childCompiler.token.Type {
		case tokLet:
			compileDeclaration(childCompiler)
		case tokIdentifier:
			compileAssignment(childCompiler)
		case tokStruct:
			compileStruct(childCompiler)
		case tokFunction:
			compileClosure(childCompiler)
		case tokIf:
			compileIf(childCompiler, false)
		case tokFor:
			compileFor(childCompiler)
		case tokLabel:
			switch childCompiler.next.Type {
			case tokFor:
				compileFor(childCompiler)
			default:
				compilerPrintError("Statement", childCompiler.next, childCompiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(childCompiler, false)
		case tokReturn:
			compileReturn(childCompiler)
		case tokDefer:
			compileDefer(childCompiler)
		case tokExtension:
			compileExtension(childCompiler)
		case tokLCurly:
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
func emitBuildClosure(compiler *compiler, newFunction Function, freeVariables []freeVar, line UInt32) {
	if newFunction.FreeVarCount == 0 {
		emitClosure(compiler, appendValue(compiler, newFunction), line, false)
	} else {
		emitClosure(compiler, appendValue(compiler, newFunction), line, true)
		for i := Bytecode(0); i < newFunction.FreeVarCount; i++ {
			emitFreeVar(compiler, freeVariables[i].index, line, freeVariables[i].isLocal)
		}
	}
}

func compileReturn(compiler *compiler) {
	line := compiler.token.Line
	if compiler.hasDefer {
		// |- OpCode -|
		instruction := OPRunDefer
		compiler.function.Code = append(compiler.function.Code, instruction)
		compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	}
	returnCount := Bytecode(1)
	check(compiler, tokReturn)
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	for compiler.token.Type == tokComma {
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		returnCount++
	}
	emitReturn(compiler, returnCount, line)
}

func compileDefer(compiler *compiler) {
	check(compiler, tokDefer)
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	index := len(compiler.function.Code) - 1
	if compiler.function.Code[index]&opcodeMask == OPCall && compiler.token.Type == tokRParen {
		forward(compiler)
		oldInstr := compiler.function.Code[index]
		instruction := OPDefer
		instruction |= (oldInstr >> instructionShift) << instructionShift
		compiler.function.Code[index] = instruction
		compiler.hasDefer = true
	} else if compiler.function.Code[index]&opcodeMask == OPInvokeMethod && compiler.token.Type == tokRParen {
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

func compileIf(compiler *compiler, insideLoop bool) {
	offset := len(jumpHelper)
	line := compiler.token.Line
	check(compiler, tokIf)
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	jumpAddress := emitJumpIfFalse(compiler, line)
	check(compiler, tokLCurly)
	forward(compiler)
	compileIFBlock(compiler, insideLoop)
	check(compiler, tokRCurly)
	forward(compiler)
	jumpHelper = append(jumpHelper, emitJump(compiler, line))
	compiler.function.Code[jumpAddress] |= Bytecode(len(compiler.function.Code)) << instructionShift
	for compiler.token.Type == tokElse && compiler.next.Type == tokIf {
		line = compiler.token.Line
		check(compiler, tokElse)
		forward(compiler)
		check(compiler, tokIf)
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		jumpAddress := emitJumpIfFalse(compiler, line)
		check(compiler, tokLCurly)
		forward(compiler)
		compileIFBlock(compiler, insideLoop)
		check(compiler, tokRCurly)
		forward(compiler)
		jumpHelper = append(jumpHelper, emitJump(compiler, line))
		compiler.function.Code[jumpAddress] |= Bytecode(len(compiler.function.Code)) << instructionShift
	}
	if compiler.token.Type == tokElse {
		check(compiler, tokElse)
		forward(compiler)
		check(compiler, tokLCurly)
		forward(compiler)
		compileIFBlock(compiler, insideLoop)
		check(compiler, tokRCurly)
		forward(compiler)
	}
	instrAddress := Bytecode(len(compiler.function.Code))
	length := len(jumpHelper)
	for i := offset; i < length; i++ {
		compiler.function.Code[jumpHelper[i]] |= instrAddress << instructionShift
	}
	jumpHelper = jumpHelper[:offset]
}

func compileIFBlock(compiler *compiler, insideLoop bool) {
	beginScope(compiler)
	for compiler.token.Type != tokEOF &&
		compiler.token.Type != tokRCurly &&
		compiler.token.Type != tokElse {
		switch compiler.token.Type {
		case tokLet:
			compileDeclaration(compiler)
		case tokIdentifier:
			compileAssignment(compiler)
		case tokStruct:
			compileStruct(compiler)
		case tokFunction:
			compileClosure(compiler)
		case tokIf:
			compileIf(compiler, insideLoop)
		case tokFor:
			compileFor(compiler)
		case tokLabel:
			switch compiler.next.Type {
			case tokFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(compiler, insideLoop)
		case tokBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokReturn:
			compileReturn(compiler)
		case tokDefer:
			compileDefer(compiler)
		case tokExtension:
			compileExtension(compiler)
		case tokLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileFor(compiler *compiler) {
	variableCount := Bytecode(0)
	loopOffset := len(loopHelper)
	loopHelper = append(loopHelper, loopInfo{})
	loopHelper[loopOffset].scopeDepth = compiler.scopeDepth
	if compiler.token.Type == tokLabel {
		loopHelper[loopOffset].label = compiler.token.Description
		forward(compiler)
	}
	line := compiler.token.Line
	check(compiler, tokFor)
	forward(compiler)
	if compiler.token.Type == tokLCurly {
		check(compiler, tokLCurly)
		forward(compiler)
		loopHelper[loopOffset].loopAddress = Bytecode(len(compiler.function.Code))
		compileLoopBlock(compiler)
		check(compiler, tokRCurly)
		forward(compiler)
		compiler.function.Code[emitJump(compiler, line)] |= loopHelper[loopOffset].loopAddress << instructionShift
		afterLoopAddress := Bytecode(len(compiler.function.Code))
		for i := 0; i < len(loopHelper[loopOffset].breaks); i++ {
			compiler.function.Code[loopHelper[loopOffset].breaks[i]] |= afterLoopAddress << instructionShift
		}
	} else if compiler.next.Type == tokIn || compiler.next.Type == tokComma {
		loopHelper[loopOffset].isRangeLoop = true
		beginScope(compiler)
		check(compiler, tokIdentifier)
		compileNil(compiler, precedenceNone)
		addLocal(compiler, compiler.token)
		forward(compiler)
		variableCount++
		for compiler.token.Type == tokComma {
			forward(compiler)
			check(compiler, tokIdentifier)
			compileNil(compiler, precedenceNone)
			addLocal(compiler, compiler.token)
			forward(compiler)
			variableCount++
		}
		check(compiler, tokIn)
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		addLocal(compiler, token{Type: tokIdentifier, Description: iteratorID, Line: compiler.token.Line})
		loopHelper[loopOffset].loopAddress = Bytecode(len(compiler.function.Code))
		jumpAddressCheck := emitNext(compiler, line)
		if variableCount > 1 {
			emitUnpackFor(compiler, variableCount, line)
		}
		check(compiler, tokLCurly)
		forward(compiler)
		compileLoopBlock(compiler)
		line = compiler.token.Line
		check(compiler, tokRCurly)
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
		compileExpression(compiler, precedenceNone)
		jumpWhenFalseOrNil = emitJumpIfFalse(compiler, line)
		forward(compiler)
		check(compiler, tokLCurly)
		forward(compiler)
		compileLoopBlock(compiler)
		line = compiler.token.Line
		check(compiler, tokRCurly)
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

func compileLoopBlock(compiler *compiler) {
	beginScope(compiler)
	for compiler.token.Type != tokEOF && compiler.token.Type != tokRCurly {
		switch compiler.token.Type {
		case tokLet:
			compileDeclaration(compiler)
		case tokIdentifier:
			compileAssignment(compiler)
		case tokStruct:
			compileStruct(compiler)
		case tokFunction:
			compileClosure(compiler)
		case tokIf:
			compileIf(compiler, true)
		case tokFor:
			compileFor(compiler)
		case tokLabel:
			switch compiler.next.Type {
			case tokFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(compiler, true)
		case tokBreak:
			compileBreak(compiler)
		case tokContinue:
			compileContinue(compiler)
		case tokReturn:
			compileReturn(compiler)
		case tokDefer:
			compileDefer(compiler)
		case tokExtension:
			compileExtension(compiler)
		case tokLCurly:
			compileBlock(compiler, true)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileSwitch(compiler *compiler, insideLoop bool) {
	check(compiler, tokSwitch)
	forward(compiler)
	switch compiler.token.Type {
	case tokLCurly:
		offset := len(jumpHelper)
		check(compiler, tokLCurly)
		forward(compiler)
		for compiler.token.Type == tokCase {
			line := compiler.token.Line
			check(compiler, tokCase)
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			jumpIfFalseInstructionIndex := emitJumpIfFalse(compiler, line)
			check(compiler, tokColon)
			forward(compiler)
			compileSwitchCaseBlock(compiler, insideLoop)
			jumpHelper = append(jumpHelper, emitJump(compiler, line))
			compiler.function.Code[jumpIfFalseInstructionIndex] |= Bytecode(len(compiler.function.Code)) << instructionShift
		}
		compileSwitchDefaultBlock(compiler, insideLoop)
		check(compiler, tokRCurly)
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
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		addLocal(compiler, token{Type: tokIdentifier, Description: switchID, Line: line})
		switchLocalIndex := resolveLocal(compiler, switchID)
		check(compiler, tokLCurly)
		forward(compiler)
		for compiler.token.Type == tokCase {
			line := compiler.token.Line
			check(compiler, tokCase)
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			emitGetLocal(compiler, Bytecode(switchLocalIndex), line)
			patternHelper = append(patternHelper, emitMatch(compiler, line))
			for compiler.token.Type == tokComma {
				check(compiler, tokComma)
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				emitGetLocal(compiler, Bytecode(switchLocalIndex), line)
				patternHelper = append(patternHelper, emitMatch(compiler, line))
			}
			check(compiler, tokColon)
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
		check(compiler, tokRCurly)
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

func compileSwitchCaseBlock(compiler *compiler, insideLoop bool) {
	beginScope(compiler)
	for compiler.token.Type != tokEOF && compiler.token.Type != tokCase && compiler.token.Type != tokDefault {
		switch compiler.token.Type {
		case tokLet:
			compileDeclaration(compiler)
		case tokIdentifier:
			compileAssignment(compiler)
		case tokStruct:
			compileStruct(compiler)
		case tokFunction:
			compileClosure(compiler)
		case tokIf:
			compileIf(compiler, insideLoop)
		case tokFor:
			compileFor(compiler)
		case tokLabel:
			switch compiler.next.Type {
			case tokFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(compiler, insideLoop)
		case tokBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokReturn:
			compileReturn(compiler)
		case tokDefer:
			compileDefer(compiler)
		case tokExtension:
			compileExtension(compiler)
		case tokLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileSwitchDefaultBlock(compiler *compiler, insideLoop bool) {
	beginScope(compiler)
	check(compiler, tokDefault)
	forward(compiler)
	check(compiler, tokColon)
	forward(compiler)
	for compiler.token.Type != tokEOF && compiler.token.Type != tokRCurly {
		switch compiler.token.Type {
		case tokLet:
			compileDeclaration(compiler)
		case tokIdentifier:
			compileAssignment(compiler)
		case tokStruct:
			compileStruct(compiler)
		case tokFunction:
			compileClosure(compiler)
		case tokIf:
			compileIf(compiler, insideLoop)
		case tokFor:
			compileFor(compiler)
		case tokLabel:
			switch compiler.next.Type {
			case tokFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(compiler, insideLoop)
		case tokBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokReturn:
			compileReturn(compiler)
		case tokDefer:
			compileDefer(compiler)
		case tokExtension:
			compileExtension(compiler)
		case tokLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileBreak(compiler *compiler) {
	line := compiler.token.Line
	check(compiler, tokBreak)
	forward(compiler)
	if length := len(loopHelper); compiler.token.Type == tokLabel {
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

func compileContinue(compiler *compiler) {
	line := compiler.token.Line
	check(compiler, tokContinue)
	forward(compiler)
	if length := len(loopHelper); compiler.token.Type == tokLabel {
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

func compileBlock(compiler *compiler, insideLoop bool) {
	check(compiler, tokLCurly)
	forward(compiler)
	beginScope(compiler)
	for compiler.token.Type != tokEOF && compiler.token.Type != tokRCurly {
		switch compiler.token.Type {
		case tokLet:
			compileDeclaration(compiler)
		case tokIdentifier:
			compileAssignment(compiler)
		case tokStruct:
			compileStruct(compiler)
		case tokFunction:
			compileClosure(compiler)
		case tokIf:
			compileIf(compiler, insideLoop)
		case tokFor:
			compileFor(compiler)
		case tokLabel:
			switch compiler.next.Type {
			case tokFor:
				compileFor(compiler)
			default:
				compilerPrintError("Statement", compiler.next, compiler.lexer.fileName)
			}
		case tokSwitch:
			compileSwitch(compiler, insideLoop)
		case tokBreak:
			if insideLoop {
				compileBreak(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokContinue:
			if insideLoop {
				compileContinue(compiler)
			} else {
				compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
			}
		case tokReturn:
			compileReturn(compiler)
		case tokDefer:
			compileDefer(compiler)
		case tokExtension:
			compileExtension(compiler)
		case tokLCurly:
			compileBlock(compiler, insideLoop)
		default:
			compilerPrintError("Statement", compiler.token, compiler.lexer.fileName)
		}
	}
	check(compiler, tokRCurly)
	forward(compiler)
	if count := endScope(compiler); count != 0 {
		emitPop(compiler, count, compiler.token.Line)
	}
}

func compileExpression(compiler *compiler, precedence byte) {
	prefixRule := compiler.rules[compiler.token.Type].prefix
	if prefixRule == nil {
		compilerPrintError("Expression", compiler.token, compiler.lexer.fileName)
	}
	prefixRule(compiler, precedence)
	for precedence <= compiler.rules[compiler.next.Type].precedence && compiler.rules[compiler.next.Type].precedence != precedenceNone {
		forward(compiler)
		infixRule := compiler.rules[compiler.token.Type].infix
		infixRule(compiler, precedence)
	}
}

func compileInteger(compiler *compiler, precedence byte) {
	if value, err := strconv.ParseInt(string(compiler.token.Description), 0, 64); err == nil {
		emitConstant(compiler, Int(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileUInt(compiler *compiler, precedence byte) {
	if value, err := strconv.ParseUint(string(compiler.token.Description), 0, 64); err == nil {
		emitConstant(compiler, UInt(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileFloat(compiler *compiler, precedence byte) {
	if value, err := strconv.ParseFloat(compiler.token.Description, 64); err == nil {
		emitConstant(compiler, Float(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileString(compiler *compiler, precedence byte) {
	emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
}

func compileBig(compiler *compiler, precedence byte) {
	big := new(big.Int)
	if _, success := big.SetString(compiler.token.Description, 0); success {
		emitConstant(compiler, &BInt{Value: big}, compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileRational(compiler *compiler, precedence byte) {
	rat := new(big.Rat)
	if _, success := rat.SetString(compiler.token.Description); success {
		emitConstant(compiler, &Rational{Value: rat}, compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileComplex(compiler *compiler, precedence byte) {
	if value, err := strconv.ParseComplex(compiler.token.Description, 128); err == nil {
		emitConstant(compiler, Complex(value), compiler.token.Line)
	} else {
		numberError(compiler.token.Description, compiler.token.Line, compiler.lexer.fileName)
	}
}

func compileTrue(compiler *compiler, precedence byte) {
	// |- OPCode -|
	compiler.function.Code = append(compiler.function.Code, OPTrue)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = compiler.token.Line
}

func compileFalse(compiler *compiler, precedence byte) {
	// |- OPCode -|
	compiler.function.Code = append(compiler.function.Code, OPFalse)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = compiler.token.Line
}

func compileNil(compiler *compiler, precedence byte) {
	// |- OPCode -|
	compiler.function.Code = append(compiler.function.Code, OPNil)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = compiler.token.Line
}

func compileRune(compiler *compiler, precedence byte) {
	r, _ := utf8.DecodeLastRuneInString(compiler.token.Description)
	emitConstant(compiler, Rune(r), compiler.token.Line)
}

func compileIdentifier(compiler *compiler, precedence byte) {
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

func buildLambda(compiler *compiler, params []token) (Function, []freeVar) {
	line := compiler.token.Line
	childCompiler := newChildCompiler(compiler)
	beginScope(childCompiler)
	for _, tok := range params {
		addLocal(childCompiler, tok)
	}
	compileExpression(childCompiler, precedenceNone)
	emitReturn(childCompiler, 1, line)
	endScope(childCompiler)
	updateCompiler(compiler, childCompiler)
	if childCompiler.canAccess {
		childCompiler.function.CanAccessPrivateState = true
	}
	return childCompiler.function, childCompiler.freeVariables
}

func compileAnonymousFunction(compiler *compiler, precedence byte) {
	var isLambdaExpression bool
	line := compiler.token.Line
	forward(compiler)
	isvararg := false
	arity := Bytecode(0)
	paramsMap := make(map[string]struct{})
	var params []token
	if compiler.token.Type == tok3Dots {
		check(compiler, tok3Dots)
		forward(compiler)
		check(compiler, tokIdentifier)
		params = append(params, compiler.token)
		forward(compiler)
		isvararg = true
	} else {
		for compiler.token.Type != tokColon && compiler.token.Type != tokLCurly && compiler.token.Type != tok3Dots {
			check(compiler, tokIdentifier)
			if _, isParam := paramsMap[compiler.token.Description]; isParam {
				argumentError(compiler.token, compiler.lexer.fileName)
			} else {
				paramsMap[compiler.token.Description] = struct{}{}
				params = append(params, compiler.token)
			}
			forward(compiler)
			arity++
		}
		if compiler.token.Type == tok3Dots {
			check(compiler, tok3Dots)
			forward(compiler)
			check(compiler, tokIdentifier)
			params = append(params, compiler.token)
			forward(compiler)
			isvararg = true
		}
	}
	if compiler.token.Type == tokColon {
		isLambdaExpression = true
		check(compiler, tokColon)
	} else {
		check(compiler, tokLCurly)
	}
	forward(compiler)
	var newLambda Function
	var freeVariables []freeVar
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
		check(compiler, tokRCurly)
	}
	paramsMap = nil
	params = nil
	freeVariables = nil
	lambdaID++
}

func compileRange(compiler *compiler, precedence byte) {
	line := compiler.token.Line
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	argCount := Bytecode(1)
	if compiler.next.Type == tokComma {
		forward(compiler)
		check(compiler, tokComma)
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		argCount++
		if compiler.next.Type == tokComma {
			forward(compiler)
			check(compiler, tokComma)
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			argCount++
		}
	}
	emitRange(compiler, argCount, line)
}

func compileRecord(compiler *compiler, precedence byte) {
	switch compiler.next.Type {
	case tokRCurly:
		forward(compiler)
		emitRecord(compiler, 0, compiler.token.Line)
	default:
		line := compiler.token.Line
		length := Bytecode(0)
		forward(compiler)
		check(compiler, tokIdentifier)
		emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
		forward(compiler)
		check(compiler, tokColon)
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		length += 2
		for compiler.token.Type == tokComma {
			forward(compiler)
			check(compiler, tokIdentifier)
			emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
			forward(compiler)
			check(compiler, tokColon)
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			length += 2
		}
		check(compiler, tokRCurly)
		emitRecord(compiler, length, line)
	}
}

func compilePrefix(compiler *compiler, precedence byte) {
	operator := compiler.token.Type
	line := compiler.token.Line
	forward(compiler)
	compileExpression(compiler, precedenceUnary)
	instruction := OPPrefix
	instruction |= Bytecode(operator) << instructionShift
	// |- OpCode -|- Operator -|
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileBinary(compiler *compiler, precedence byte) {
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

func compileEqual(compiler *compiler, precedence byte) {
	// Left associative operators.
	operator := compiler.token.Type
	line := compiler.token.Line
	operatorPrecedence := compiler.rules[operator].precedence + 1
	forward(compiler)
	compileExpression(compiler, operatorPrecedence)
	// |- OpCode -|
	if operator == tokEqual {
		compiler.function.Code = append(compiler.function.Code, OPEqual)
	} else {
		compiler.function.Code = append(compiler.function.Code, OPNotEqual)
	}
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileNilChoice(compiler *compiler, precedence byte) {
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

func compilePower(compiler *compiler, precedence byte) {
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

func compileCall(compiler *compiler, precedence byte) {
	line := compiler.token.Line
	forward(compiler)
	argCount := Bytecode(0)
	spread := Bytecode(0)
	if compiler.token.Type != tokRParen {
		if compiler.token.Type == tokIdentifier && compiler.next.Type == tokColon {
			check(compiler, tokIdentifier)
			emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
			forward(compiler)
			check(compiler, tokColon)
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			argCount += 2
			for compiler.token.Type == tokComma {
				forward(compiler)
				check(compiler, tokIdentifier)
				emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
				forward(compiler)
				check(compiler, tokColon)
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				argCount += 2
			}
		} else {
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			argCount++
			if compiler.token.Type == tok3Dots {
				check(compiler, tok3Dots)
				forward(compiler)
				spread = 1
				goto emitCall
			}
			for compiler.token.Type == tokComma {
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				argCount++
				if compiler.token.Type == tok3Dots {
					check(compiler, tok3Dots)
					forward(compiler)
					spread = 1
					goto emitCall
				}
			}
		}
	}
emitCall:
	check(compiler, tokRParen)
	emitCall(compiler, argCount, spread, line)
}

func compileSelector(compiler *compiler, precedence byte) {
	// |- OpCode -|
	line := compiler.token.Line
	check(compiler, tokDot)
	forward(compiler)
	check(compiler, tokIdentifier)
	emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
	spread := Bytecode(0)
	if compiler.next.Type == tokLParen {
		forward(compiler)
		line := compiler.token.Line
		forward(compiler)
		argCount := Bytecode(0)
		if compiler.token.Type != tokRParen {
			if compiler.token.Type == tokIdentifier && compiler.next.Type == tokColon {
				check(compiler, tokIdentifier)
				emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
				forward(compiler)
				check(compiler, tokColon)
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				argCount += 2
				for compiler.token.Type == tokComma {
					forward(compiler)
					check(compiler, tokIdentifier)
					emitConstant(compiler, &String{Value: compiler.token.Description}, compiler.token.Line)
					forward(compiler)
					check(compiler, tokColon)
					forward(compiler)
					compileExpression(compiler, precedenceNone)
					forward(compiler)
					argCount += 2
				}
			} else {
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				argCount++
				if compiler.token.Type == tok3Dots {
					check(compiler, tok3Dots)
					forward(compiler)
					spread = 1
					goto emitInvoke
				}
				for compiler.token.Type == tokComma {
					forward(compiler)
					compileExpression(compiler, precedenceNone)
					forward(compiler)
					argCount++
					if compiler.token.Type == tok3Dots {
						check(compiler, tok3Dots)
						forward(compiler)
						spread = 1
						goto emitInvoke
					}
				}
			}
		}
	emitInvoke:
		check(compiler, tokRParen)
		emitInvoke(compiler, argCount, spread, line)
	} else {
		instruction := OPSelect
		instruction |= onlyExpression << instructionShift // Flag to be used in mutating operations.
		compiler.function.Code = append(compiler.function.Code, instruction)
		compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	}
}

func compileSubscript(compiler *compiler, precedence byte) {
	// |- OpCode -|- Flag -|
	// Where Flag means:
	// OnlyExpression = [e]
	// ExprColonExpr = [e:e]
	// ExprColon = [e:]
	// ColonExpr = [:e]
	// OnlyColon = [:]
	instruction := OPSubscript
	line := compiler.token.Line
	check(compiler, tokLBracket)
	forward(compiler)
	if compiler.token.Type == tokColon {
		check(compiler, tokColon)
		forward(compiler)
		if compiler.token.Type == tokRBracket {
			instruction |= onlyColon << instructionShift // Flag indicating [:] operations.
			goto assembleInst
		}
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		instruction |= colonExpr << instructionShift // Flag indicating [:e] operations.
		goto assembleInst
	}
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	if compiler.token.Type == tokColon {
		check(compiler, tokColon)
		forward(compiler)
		if compiler.token.Type == tokRBracket {
			instruction |= exprColon << instructionShift // Flag idicating [e:] operations.
			goto assembleInst
		}
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		instruction |= exprColonExpr << instructionShift // Flag idicating [e:e] operations.
		goto assembleInst
	}
	instruction |= onlyExpression << instructionShift // Flag indicatign [e] operations.
assembleInst:
	check(compiler, tokRBracket)
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func compileGroup(compiler *compiler, precedence byte) {
	check(compiler, tokLParen)
	forward(compiler)
	compileExpression(compiler, precedenceNone)
	forward(compiler)
	check(compiler, tokRParen)
}

func compileListOrMap(compiler *compiler, precedence byte) {
	check(compiler, tokLBracket)
	switch compiler.next.Type {
	case tokRBracket:
		// Empty List
		forward(compiler)
		emitList(compiler, 0, compiler.token.Line)
	case tokColon:
		// Empty Map
		forward(compiler)
		forward(compiler)
		check(compiler, tokRBracket)
		emitMap(compiler, 0, compiler.token.Line)
	case tokFor:
		line := compiler.token.Line
		childCompiler := newChildCompiler(compiler)
		beginScope(childCompiler)
		emitList(childCompiler, 0, line)
		addLocal(childCompiler, token{Type: tokIdentifier, Description: comprehensionID, Line: line})
		forward(childCompiler)
		check(childCompiler, tokFor)
		forward(childCompiler)
		var jumpAddressCheck []int
		var loopOffset []int
		var localLoopHelper []loopInfo
		variableCount := Bytecode(0)
		loopOffset = append(loopOffset, len(localLoopHelper))
		localLoopHelper = append(localLoopHelper, loopInfo{})
		localLoopHelper[loopOffset[len(loopOffset)-1]].scopeDepth = childCompiler.scopeDepth
		beginScope(childCompiler)
		check(childCompiler, tokIdentifier)
		compileNil(childCompiler, precedenceNone)
		addLocal(childCompiler, childCompiler.token)
		forward(childCompiler)
		variableCount++
		for childCompiler.token.Type == tokComma {
			forward(childCompiler)
			check(childCompiler, tokIdentifier)
			compileNil(childCompiler, precedenceNone)
			addLocal(childCompiler, childCompiler.token)
			forward(childCompiler)
			variableCount++
		}
		check(childCompiler, tokIn)
		forward(childCompiler)
		compileExpression(childCompiler, precedenceNone)
		forward(childCompiler)
		addLocal(childCompiler, token{Type: tokIdentifier, Description: iteratorID, Line: childCompiler.token.Line})
		localLoopHelper[loopOffset[len(loopOffset)-1]].loopAddress = Bytecode(len(childCompiler.function.Code))
		jumpAddressCheck = append(jumpAddressCheck, emitNext(childCompiler, line))
		if variableCount > 1 {
			emitUnpackFor(childCompiler, variableCount, line)
		}
		for childCompiler.token.Type == tokFor {
			check(childCompiler, tokFor)
			forward(childCompiler)
			variableCount := Bytecode(0)
			loopOffset = append(loopOffset, len(localLoopHelper))
			localLoopHelper = append(localLoopHelper, loopInfo{})
			localLoopHelper[loopOffset[len(loopOffset)-1]].scopeDepth = childCompiler.scopeDepth
			beginScope(childCompiler)
			check(childCompiler, tokIdentifier)
			compileNil(childCompiler, precedenceNone)
			addLocal(childCompiler, childCompiler.token)
			forward(childCompiler)
			variableCount++
			for childCompiler.token.Type == tokComma {
				forward(childCompiler)
				check(childCompiler, tokIdentifier)
				compileNil(childCompiler, precedenceNone)
				addLocal(childCompiler, childCompiler.token)
				forward(childCompiler)
				variableCount++
			}
			check(childCompiler, tokIn)
			forward(childCompiler)
			compileExpression(childCompiler, precedenceNone)
			forward(childCompiler)
			addLocal(childCompiler, token{Type: tokIdentifier, Description: iteratorID, Line: childCompiler.token.Line})
			localLoopHelper[loopOffset[len(loopOffset)-1]].loopAddress = Bytecode(len(childCompiler.function.Code))
			jumpAddressCheck = append(jumpAddressCheck, emitNext(childCompiler, line))
			if variableCount > 1 {
				emitUnpackFor(childCompiler, variableCount, line)
			}
		}
		if childCompiler.token.Type == tokIf {
			check(childCompiler, tokIf)
			forward(childCompiler)
			compileExpression(childCompiler, precedenceNone)
			forward(childCompiler)
			jumpAddress := emitJumpIfFalse(childCompiler, line)
			// Compile expression
			check(childCompiler, tokRArrow)
			forward(childCompiler)
			compileExpression(childCompiler, precedenceNone)
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
			check(childCompiler, tokRArrow)
			forward(childCompiler)
			compileExpression(childCompiler, precedenceNone)
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
		check(compiler, tokRBracket)
		emitCall(compiler, 0, 0, line)
		jumpAddressCheck = nil
		loopOffset = nil
		localLoopHelper = nil
	default:
		line := compiler.token.Line
		length := Bytecode(0)
		forward(compiler)
		compileExpression(compiler, precedenceNone)
		forward(compiler)
		length++
		switch compiler.token.Type {
		case tokComma:
			for compiler.token.Type == tokComma {
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				length++
			}
			check(compiler, tokRBracket)
			emitList(compiler, length, line)
		case tokRBracket:
			check(compiler, tokRBracket)
			emitList(compiler, length, line)
		case tokColon:
			forward(compiler)
			compileExpression(compiler, precedenceNone)
			forward(compiler)
			for compiler.token.Type == tokComma {
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				check(compiler, tokColon)
				forward(compiler)
				compileExpression(compiler, precedenceNone)
				forward(compiler)
				length++
			}
			check(compiler, tokRBracket)
			emitMap(compiler, length, line)
		default:
			compilerPrintError("Expression", compiler.token, compiler.lexer.fileName)
		}
	}
}

func check(compiler *compiler, tKind byte) {
	if compiler.token.Type != tKind {
		unexpectedTokenError(tKind, compiler.token, compiler.lexer.fileName)
	}
}

// forward update the compiler's state with the next token ahead.
func forward(compiler *compiler) {
	compiler.token = compiler.next
	compiler.next = compiler.lexer.nextToken()
}

func unexpectedTokenError(expected byte, found token, script string) {
	if len(found.Description) == 0 {
		fmt.Printf("\n   Syntax Error\n   Expected '%v' but got Token '%v'\n   %v:%3v\n\n", KindDescription[expected], KindDescription[found.Type], script, found.Line)
	} else {
		fmt.Printf("\n   Syntax Error\n   Expected '%v' but got Token of type '%v' with value '%v'\n   %v:%3v\n\n", KindDescription[expected], KindDescription[found.Type], found.Description, script, found.Line)
	}
	os.Exit(0)
}

func compilerPrintError(expected string, found token, script string) {
	if len(found.Description) == 0 {
		fmt.Printf("\n   Syntax Error\n   Expected %v but got Token '%v'\n   %v:%3v\n\n", expected, KindDescription[found.Type], script, found.Line)
	} else {
		fmt.Printf("\n   Syntax Error\n   Expected %v but got Token type '%v' with value '%v'\n   %v:%3v\n\n", expected, KindDescription[found.Type], found.Description, script, found.Line)
	}
	os.Exit(0)
}

func argumentError(found token, script string) {
	fmt.Printf("\n   Syntax Error\n   Function and param identifiers must be different\n   %v:%3v\n\n", script, found.Line)
	os.Exit(0)
}

func redefinedField(found token, script string) {
	fmt.Printf("\n   Semantic Error\n   The name '%v' has already been defined in the Type/Extension context\n   %v:%3v\n\n", found.Description, script, found.Line)
	os.Exit(0)
}

func redefinedLocalError(found token, script string) {
	fmt.Printf("\n   Syntax Error\n   Redefined local variable '%v'\n   %v:%3v\n\n", found.Description, script, found.Line)
	os.Exit(0)
}

func labelError(found token, script string) {
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

func emitConstant(compiler *compiler, value Value, line UInt32) {
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

func appendValue(compiler *compiler, value Value) Bytecode {
	if compiler.kIndex < maxConstants {
		compiler.function.Constants = append(compiler.function.Constants, value)
		index := compiler.kIndex
		compiler.kIndex++
		return index
	}
	maxConstantsError(compiler.lexer.fileName)
	return 0
}

func emitClosure(compiler *compiler, index Bytecode, line UInt32, collect bool) {
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

func emitBuildStruct(compiler *compiler, stuffCount Bytecode, line UInt32) {
	// |- OpCode -|- stuffCount -|
	instruction := OPStruct
	instruction |= stuffCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitExtension(compiler *compiler, methodsCount, StructCount Bytecode, line UInt32) {
	// |- OpCode -|- methodsCount -|- StructCount -|
	instruction := OPExtension
	instruction |= methodsCount << instructionShift
	instruction |= StructCount << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitDerive(compiler *compiler, deriveCount Bytecode, line UInt32) {
	// |- OpCode -|- DeriveCount -|
	instruction := OPDerive
	instruction |= deriveCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitList(compiler *compiler, length, line UInt32) {
	// |- OpCode -|- Length -|
	instruction := OPList
	instruction |= length << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitMap(compiler *compiler, length, line UInt32) {
	// |- OpCode -|- Length -|
	instruction := OPMap
	instruction |= (length * 2) << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitRecord(compiler *compiler, length, line UInt32) {
	// |- OpCode -|- Length -|
	instruction := OPRecord
	instruction |= length << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitConstantNamespace(compiler *compiler, constCount, line UInt32) {
	// |- OpCode -|- ConstCount -|
	instruction := OPConstNamespace
	instruction |= constCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitNewGlobal(compiler *compiler, gIndex Bytecode, line UInt32) {
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

func emitGetGlobal(compiler *compiler, gIndex, line UInt32) {
	// |- OpCode -|- Global Index -|
	instruction := OPGetGlobal
	instruction |= gIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitSetGlobal(compiler *compiler, gIndex, line UInt32) {
	// |- OpCode -|- Global Index -|
	instruction := OPSetGlobal
	instruction |= gIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitCompoundSetGlobal(compiler *compiler, gIndex, line UInt32, operatorCode Bytecode) {
	// |- OpCode -|- Global Index -|- operator -|
	instruction := OPCompSetGlobal
	instruction |= gIndex << instructionShift
	instruction |= operatorCode << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitMutDataStructure(compiler *compiler, relativeIndex, flag, mutatingOperatorType, line UInt32) {
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

func emitCompoundMutDataStructure(compiler *compiler, relativeIndex, line UInt32, operatorCode, selectorType, flag Bytecode) {
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

func emitJumpIfFalse(compiler *compiler, line UInt32) int {
	// |- OpCode -|- Jump Address -|
	instruction := OPJumpIfFalse
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func emitJump(compiler *compiler, line UInt32) int {
	// |- OpCode -|- Jump Address -|
	instruction := OPGoto
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func beginScope(compiler *compiler) {
	compiler.scopeDepth++
}

func endScope(compiler *compiler) Bytecode {
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

func countLocalsUpTo(compiler *compiler, depth Int32) Bytecode {
	offset := 0
	for i := len(compiler.locals) - 1; i >= 0; i-- {
		if compiler.locals[i].depth > depth {
			offset++
		}
	}
	return Bytecode(offset)
}

func addLocal(compiler *compiler, token token) {
	if len(compiler.locals) < maxLocals {
		for i := len(compiler.locals) - 1; i >= 0; i-- {
			if compiler.locals[i].depth != -1 && compiler.locals[i].depth < compiler.scopeDepth {
				break
			}
			if token.Description == compiler.locals[i].identifier {
				redefinedLocalError(token, compiler.lexer.fileName)
			}
		}
		compiler.locals = append(compiler.locals, localVariable{identifier: token.Description, depth: compiler.scopeDepth})
		return
	}
	maxLocalsError(compiler.lexer.fileName)
}

func emitGetLocal(compiler *compiler, localIndex, line UInt32) {
	// |- OpCode -|- Local Index-|
	instruction := OPGetLocal
	instruction |= localIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitSetLocal(compiler *compiler, localIndex, line UInt32) {
	// |- OpCode -|- Local Index-|
	instruction := OPSetLocal
	instruction |= localIndex << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitCompoundSetLocal(compiler *compiler, localIndex, line UInt32, operatorCode Bytecode) {
	// |- OpCode -|- Local Index-|- operator -|
	instruction := OPCompSetLocal
	instruction |= localIndex << instructionShift
	instruction |= operatorCode << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func resolveLocal(compiler *compiler, identifier string) Int32 {
	for i := len(compiler.locals) - 1; i >= 0; i-- {
		if compiler.locals[i].identifier == identifier {
			return Int32(i)
		}
	}
	return -1
}

func resolveFreeVar(compiler *compiler, identifier string, line UInt32) Int32 {
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

func addFreeVar(compiler *compiler, index, line UInt32, isLocal bool) Int32 {
	if len(compiler.freeVariables) < maxFreeVars {
		for i := Bytecode(0); i < compiler.function.FreeVarCount; i++ {
			freeVar := compiler.freeVariables[i]
			if freeVar.index == index && freeVar.isLocal == isLocal {
				return Int32(i)
			}
		}
		upIndex := len(compiler.freeVariables)
		compiler.freeVariables = append(compiler.freeVariables, freeVar{index: index, isLocal: isLocal})
		compiler.function.FreeVarCount++
		return Int32(upIndex)
	}
	maxFreeVarsError(compiler.lexer.fileName)
	return 0
}

func emitFreeVar(compiler *compiler, index, line UInt32, isLocal bool) {
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

func emitGetFreeVar(compiler *compiler, index, line UInt32) {
	// |- OpCode -|- Local Index -|
	instruction := OPGetFreeVariable
	instruction |= index << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitSetFreeVar(compiler *compiler, index, line UInt32) {
	// |- OpCode -|- FreeVar Index -|
	instruction := OPSetFreeVariable
	instruction |= index << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitCompoundSetFreeVar(compiler *compiler, index, line UInt32, operatorCode Bytecode) {
	// |- OpCode -|- FreeVar Index -|- operator code -|
	instruction := OPCompSetFreeVariable
	instruction |= index << instructionShift
	instruction |= operatorCode << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitRange(compiler *compiler, argCount, line Bytecode) {
	// |- OpCode -|- ArgCount -|
	instruction := OPRange
	instruction |= argCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitNext(compiler *compiler, line UInt32) int {
	// |- OpCode -|- JumpAddress -|
	instruction := OPNext
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func emitMatch(compiler *compiler, line UInt32) int {
	// |- OpCode -|- Jumpt Address if match -|
	instruction := OPMatch
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	return len(compiler.function.Code) - 1
}

func emitCall(compiler *compiler, argCount, spread, line UInt32) {
	// |- OpCode -|- ArgCount -|- SpreadFlag -|
	instruction := OPCall
	instruction |= argCount << instructionShift
	instruction |= spread << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitInvoke(compiler *compiler, argCount, spread, line UInt32) {
	// |- OpCode -|- ArgCount -|- SpreadFlag -|
	instruction := OPInvokeMethod
	instruction |= argCount << instructionShift
	instruction |= spread << upperShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitReturn(compiler *compiler, lengthReturnedvalues, line UInt32) {
	// |- OpCode -|- Number elements to return -|
	instruction := OPReturn
	instruction |= lengthReturnedvalues << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitUnpack(compiler *compiler, idCount, line UInt32) {
	// |- OpCode -|- TokensCount -|
	instruction := OPUnpack
	instruction |= idCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitUnpackFor(compiler *compiler, idCount, line UInt32) {
	// |- OpCode -|- TokensCount -|
	instruction := OPUnpackFor
	instruction |= idCount << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitNilReturn(compiler *compiler, line UInt32) {
	// |- OpCode -|- Number elements to return -|
	compiler.function.Code = append(compiler.function.Code, OPNil)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
	instruction := OPReturn
	instruction |= 1 << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitAppendList(compiler *compiler, listAddress Bytecode, line UInt32) {
	// |- OpCode -|- ListAddress -|
	instruction := OPAppend
	instruction |= listAddress << instructionShift
	compiler.function.Code = append(compiler.function.Code, instruction)
	compiler.function.Lines[Bytecode(len(compiler.function.Code)-1)] = line
}

func emitEndScript(compiler *compiler, line UInt32) {
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

func emitPop(compiler *compiler, count, line UInt32) {
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

func cleanUp(compiler *compiler) {
	compiler.lexer.input = nil
	compiler.lexer = nil
	compiler.gIDPos = nil
	compiler.locals = nil
	compiler.rules = nil
}

func createRules() []parseRule {
	return []parseRule{
		tokEOF:             {prefix: nil, infix: nil, precedence: precedenceNone},
		tokComment:         {prefix: nil, infix: nil, precedence: precedenceNone},
		tokLet:             {prefix: nil, infix: nil, precedence: precedenceNone},
		tokIf:              {prefix: nil, infix: nil, precedence: precedenceNone},
		tokElse:            {prefix: nil, infix: nil, precedence: precedenceNone},
		tokFor:             {prefix: nil, infix: nil, precedence: precedenceNone},
		tokIn:              {prefix: nil, infix: nil, precedence: precedenceNone},
		tokRange:           {prefix: compileRange, infix: nil, precedence: precedenceNone},
		tokBreak:           {prefix: nil, infix: nil, precedence: precedenceNone},
		tokContinue:        {prefix: nil, infix: nil, precedence: precedenceNone},
		tokSwitch:          {prefix: nil, infix: nil, precedence: precedenceNone},
		tokCase:            {prefix: nil, infix: nil, precedence: precedenceNone},
		tokDefault:         {prefix: nil, infix: nil, precedence: precedenceNone},
		tokLabel:           {prefix: nil, infix: nil, precedence: precedenceNone},
		tokFunction:        {prefix: compileAnonymousFunction, infix: nil, precedence: precedenceNone},
		tokReturn:          {prefix: nil, infix: nil, precedence: precedenceNone},
		tokIdentifier:      {prefix: compileIdentifier, infix: nil, precedence: precedenceNone},
		tokEquation:        {prefix: nil, infix: nil, precedence: precedenceNone},
		tokInteger:         {prefix: compileInteger, infix: nil, precedence: precedenceNone},
		tokUInt:            {prefix: compileUInt, infix: nil, precedence: precedenceNone},
		tokBig:             {prefix: compileBig, infix: nil, precedence: precedenceNone},
		tokFloat:           {prefix: compileFloat, infix: nil, precedence: precedenceNone},
		tokRational:        {prefix: compileRational, infix: nil, precedence: precedenceNone},
		tokComplex:         {prefix: compileComplex, infix: nil, precedence: precedenceNone},
		tokTrue:            {prefix: compileTrue, infix: nil, precedence: precedenceNone},
		tokFalse:           {prefix: compileFalse, infix: nil, precedence: precedenceNone},
		tokNil:             {prefix: compileNil, infix: nil, precedence: precedenceNone},
		tokRune:            {prefix: compileRune, infix: nil, precedence: precedenceNone},
		tokString:          {prefix: compileString, infix: nil, precedence: precedenceNone},
		tok3Dots:           {prefix: nil, infix: nil, precedence: precedenceNone},
		TKAdd:              {prefix: compilePrefix, infix: compileBinary, precedence: precedenceAdditive},
		TKMinus:            {prefix: compilePrefix, infix: compileBinary, precedence: precedenceAdditive},
		TKMul:              {prefix: nil, infix: compileBinary, precedence: precedenceMultiplicative},
		TKDiv:              {prefix: nil, infix: compileBinary, precedence: precedenceMultiplicative},
		TKMod:              {prefix: nil, infix: compileBinary, precedence: precedenceMultiplicative},
		TKPercent:          {prefix: nil, infix: compileBinary, precedence: precedenceMultiplicative},
		TKPower:            {prefix: nil, infix: compilePower, precedence: precedenceExp},
		TKAmpersand:        {prefix: nil, infix: compileBinary, precedence: precedenceBitAnd},
		TKBar:              {prefix: nil, infix: compileBinary, precedence: precedenceBitOr},
		TKHat:              {prefix: nil, infix: compileBinary, precedence: precedenceBitXor},
		TKLShift:           {prefix: nil, infix: compileBinary, precedence: precedenceShift},
		TKRShift:           {prefix: nil, infix: compileBinary, precedence: precedenceShift},
		tokEqual:           {prefix: nil, infix: compileEqual, precedence: precedenceEquality},
		tokNEqual:          {prefix: nil, infix: compileEqual, precedence: precedenceEquality},
		TKGT:               {prefix: nil, infix: compileBinary, precedence: precedenceRelational},
		TKGE:               {prefix: nil, infix: compileBinary, precedence: precedenceRelational},
		TKLT:               {prefix: nil, infix: compileBinary, precedence: precedenceRelational},
		TKLE:               {prefix: nil, infix: compileBinary, precedence: precedenceRelational},
		TKAnd:              {prefix: nil, infix: compileBinary, precedence: precedenceAnd},
		TKOr:               {prefix: nil, infix: compileBinary, precedence: precedenceOr},
		TKNot:              {prefix: compilePrefix, infix: nil, precedence: precedenceNone},
		TKTilde:            {prefix: compilePrefix, infix: nil, precedence: precedenceNone},
		tokComma:           {prefix: nil, infix: nil, precedence: precedenceNone},
		tokColon:           {prefix: nil, infix: nil, precedence: precedenceNone},
		tokDot:             {prefix: nil, infix: compileSelector, precedence: precedencePosfix},
		tokLParen:          {prefix: compileGroup, infix: compileCall, precedence: precedencePosfix},
		tokRParen:          {prefix: nil, infix: nil, precedence: precedenceNone},
		tokLBracket:        {prefix: compileListOrMap, infix: compileSubscript, precedence: precedencePosfix},
		tokRBracket:        {prefix: nil, infix: nil, precedence: precedenceNone},
		tokLCurly:          {prefix: compileRecord, infix: nil, precedence: precedenceNone},
		tokRCurly:          {prefix: nil, infix: nil, precedence: precedenceNone},
		tokQuestion:        {prefix: nil, infix: compileNilChoice, precedence: precedenceNilChoice},
		tokRArrow:          {prefix: nil, infix: nil, precedence: precedenceNone},
		tokStruct:          {prefix: nil, infix: nil, precedence: precedenceNone},
		tokPublic:          {prefix: nil, infix: nil, precedence: precedenceNone},
		tokExtension:       {prefix: nil, infix: nil, precedence: precedenceNone},
		tokDefer:           {prefix: nil, infix: nil, precedence: precedenceNone},
		tokAddAssign:       {prefix: nil, infix: nil, precedence: precedenceNone},
		tokSubAssign:       {prefix: nil, infix: nil, precedence: precedenceNone},
		tokMulAssign:       {prefix: nil, infix: nil, precedence: precedenceNone},
		tokDivAssign:       {prefix: nil, infix: nil, precedence: precedenceNone},
		tokRemAssign:       {prefix: nil, infix: nil, precedence: precedenceNone},
		tokPowAssign:       {prefix: nil, infix: nil, precedence: precedenceNone},
		tokBitAndAssign:    {prefix: nil, infix: nil, precedence: precedenceNone},
		tokBitOrAssign:     {prefix: nil, infix: nil, precedence: precedenceNone},
		tokBitXorAssign:    {prefix: nil, infix: nil, precedence: precedenceNone},
		tokBitLShiftAssign: {prefix: nil, infix: nil, precedence: precedenceNone},
		tokBitRShiftAssign: {prefix: nil, infix: nil, precedence: precedenceNone},
		tokNotDefined:      {prefix: nil, infix: nil, precedence: precedenceNone},
	}
}
