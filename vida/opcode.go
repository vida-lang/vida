package vida

import (
	"fmt"
)

// Opcodes for the VM.
const (
	OPConst Bytecode = iota
	OPTrue
	OPFalse
	OPNil
	OPClosure
	OPStruct
	OPExtension
	OPDerive
	OPBinop
	OPEqual
	OPNotEqual
	OPNilChoice
	OPList
	OPMap
	OPRecord
	OPConstNamespace
	OPSubscript
	OPSelect
	OPPrefix
	OPNewGlobal
	OPGetGlobal
	OPSetGlobal
	OPCompSetGlobal
	OPGetLocal
	OPSetLocal
	OPCompSetLocal
	OPMutDataStructure
	OPCompMutDataStructure
	OPGetFreeVariable
	OPSetFreeVariable
	OPCompSetFreeVariable
	OPFreeVariable
	OPJumpIfFalse
	OPGoto
	OPRange
	OPNext
	OPMatch
	OPCall
	OPDefer
	OPDeferInvoke
	OPRunDefer
	OPReturn
	OPUnpack
	OPUnpackFor
	OPInvokeMethod
	OPAppend
	OPPop
	OPEnd
)

// string representation of the opcodes.
var opCodeDescription = [...]string{
	OPConst:                "Const",          // |- OpCode -|- kIndex -|
	OPTrue:                 "True",           // |- OPCode -|
	OPFalse:                "False",          // |- OPCode -|
	OPNil:                  "Nil",            // |- OPCode -|
	OPClosure:              "Closure",        // |- OpCode -|- kIndex -|
	OPStruct:               "Struct",         // |- OpCode -|- stuffCount -|
	OPExtension:            "Extension",      // |- OpCode -|- methodsCount -|- typeCount -|
	OPDerive:               "Derive",         // |- OpCode -|- DeriveCount -|
	OPBinop:                "Binop",          // |- OPCode -|- operator -|
	OPEqual:                "Equal",          // |- OpCode -|
	OPNotEqual:             "NotEqual",       // |- OpCode -|
	OPNilChoice:            "NilChoice",      // |- OpCode -|
	OPList:                 "List",           // |- OpCode -|- Length -|
	OPMap:                  "Map",            // |- OpCode -|- Length -|
	OPRecord:               "Record",         // |- OpCode -|- Length -|
	OPConstNamespace:       "Constants",      // |- OpCode -|- ConstCount -|
	OPSubscript:            "Subscript",      // |- OpCode -|- Flag -|
	OPSelect:               "Select",         // |- OpCode -|- Flag -|
	OPPrefix:               "Prefix",         // |- OpCode -|- Operator -|
	OPNewGlobal:            "NewGlobal",      // |- OpCode -|- Global Index -|
	OPGetGlobal:            "GetGlobal",      // |- OpCode -|- Global Index -|
	OPSetGlobal:            "SetGlobal",      // |- OpCode -|- Global Index -|
	OPCompSetGlobal:        "CompGlobal",     // |- OpCode -|- Global Index -|- operator -|
	OPGetLocal:             "GetLocal",       // |- OpCode -|- Local Index -|
	OPSetLocal:             "SetLocal",       // |- OpCode -|- Local Index -|
	OPCompSetLocal:         "CompSetLocal",   // |- OpCode -|- Relative Index -|- operator -|
	OPMutDataStructure:     "MutDStruct",     // |- OpCode -|- Relative Index -|
	OPCompMutDataStructure: "CompMutDStruct", // |- OpCode -|- Relative Index -|- operator -|
	OPGetFreeVariable:      "GetFreeVar",     // |- OpCode -|- Local Index -|
	OPSetFreeVariable:      "SetFreeVar",     // |- OpCode -|- FreeVar Index -|
	OPCompSetFreeVariable:  "CompSetFreeVar", // |- OpCode -|- Index -|- operator -|
	OPFreeVariable:         "FreeVar",        // |- OpCode -|- index -|- isLocal -|
	OPJumpIfFalse:          "JumpFalse",      // |- OpCode -|- Jump Address -|
	OPGoto:                 "Goto",           // |- OpCode -|- Jump Address -|
	OPRange:                "Range",          // |- OpCode -|- ArgCount -|
	OPNext:                 "Next",           // |- OpCode -|- Jump Address -|
	OPMatch:                "Match",          // |- OpCode -|- Jump Address -|
	OPCall:                 "Call",           // |- OpCode -|- ArgCount -|- SpreadFlag -|
	OPDefer:                "Defer",          // |- OpCode -|- ArgCount -|
	OPDeferInvoke:          "Defer",          // |- OpCode -|- ArgCount -|
	OPRunDefer:             "RunDefer",       // |- OpCode -|
	OPReturn:               "Return",         // |- OpCode -|- Number elements to return -|
	OPUnpack:               "Unpack",         // |- OpCode -|- idCount -|
	OPUnpackFor:            "Unpack",         // |- OpCode -|- idCount -|
	OPInvokeMethod:         "Invoke",         // |- OpCode -|- ArgCount -|- SpreadFlag -|
	OPAppend:               "Append",         // |- OpCode -|- List Index -|
	OPPop:                  "Pop",            // |- OpCode -|- Count -|
	OPEnd:                  "End",            // |- OpCode -|
}

func printInstructions(function Function) {
	fmt.Printf("\n _____________________________________\n")
	fmt.Printf("\n Function %v\n", function.Name)
	fmt.Printf("%4v %5v %17v %7v\n", "Idx", "Line", "Instr", "Args")
	fmt.Printf(" _____________________________________\n\n\n")
	for i := Bytecode(0); i < Bytecode(len(function.Code)); i++ {
		instr := function.Code[i]
		switch instr & opcodeMask {
		// Instr. with 1 field.
		case OPConst, OPStruct, OPList, OPPrefix,
			OPNewGlobal, OPSetGlobal, OPGetGlobal, OPBinop, OPNext,
			OPMap, OPRecord, OPGetLocal, OPSetLocal, OPGetFreeVariable, OPSetFreeVariable,
			OPJumpIfFalse, OPGoto, OPMatch, OPSubscript, OPSelect,
			OPDefer, OPDeferInvoke, OPReturn, OPUnpack, OPUnpackFor,
			OPPop, OPAppend, OPRange, OPDerive:
			// Instr. with 2 fields.
			fmt.Printf("%4v %5v %17v %4v\n", i, function.Lines[i], opCodeDescription[instr&opcodeMask], instr>>instructionShift)
		case OPFreeVariable, OPExtension, OPCompSetGlobal, OPCompSetFreeVariable, OPCompSetLocal, OPClosure, OPMutDataStructure, OPCall, OPInvokeMethod:
			// Instr. with 3 fields.
			fmt.Printf("%4v %5v %17v %4v %4v\n", i, function.Lines[i], opCodeDescription[instr&opcodeMask], instr>>instructionShift&operandMask, instr>>upperShift)
		case OPCompMutDataStructure:
			// Instr. wit 3 fileds and 2 Bytecodes.
			fmt.Printf("%4v %5v %17v %4v %4v\n", i, function.Lines[i], opCodeDescription[instr&opcodeMask], instr>>instructionShift&operandMask, instr>>upperShift)
			i++
			instr = function.Code[i]
			fmt.Printf("%4v %5v %17v %4v %4v\n", i, function.Lines[i], "FSelector", instr&opcodeMask, instr>>instructionShift)
		default:
			// Instr. with 0 fields.
			fmt.Printf("%4v %5v %17v\n", i, function.Lines[i], opCodeDescription[instr&opcodeMask])
		}
	}
	fmt.Println()
}

// Prints a slice of machine code on the screen.
func printBytecode(function Function) {
	printInstructions(function)
	for i := 0; i < len(function.Constants); i++ {
		switch function.Constants[i].(type) {
		case Function:
			function := function.Constants[i].(Function)
			printBytecode(function)
		}
	}
}
