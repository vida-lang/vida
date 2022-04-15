package vida

import (
	"fmt"
	"runtime/debug"
)

func (vm *VM) RunDebugger(fiberFunctionName string) {
	vm.printBytecodeBeforeDebuggin()
	if err := vm.runStepByStep(fiberFunctionName); err != nil {
		globalState.vmFailure = true
		fiber := vm.runPendingDeferStatements()
		PrintError(err)
		vm.Fiber = fiber
		vm.Fiber.printStack(vm.Fiber.frameIndex)
	}
	if globalState.vmFailure {
		vm.printFinalFailureVMState()
	} else {
		vm.printVMStateBeforeLeavingDebugger()
	}
}

func (vm *VM) printBytecodeBeforeDebuggin() {
	fmt.Printf("\u001B[H")
	fmt.Printf("\u001B[2J")
	fmt.Printf("\nModule %#v\n", vm.module.global.mainModuleName)
	fmt.Printf("Bytecode\n")
	printBytecode(*vm.frame.closure.Function)
	fmt.Print("\nPress 'enter' to start step execution   ~~>   ")
	fmt.Scanf(" ")
}

func (vm *VM) printVMState() {
	// Fetch and decode instruction.
	instruction := vm.frame.closure.Function.Code[vm.frame.pc]
	opcode := instruction & opcodeMask
	operand := instruction >> instructionShift
	//vm.frame.pc++
	// Print debug information.
	fmt.Printf("\u001B[H")
	fmt.Printf("\u001B[2J")
	fmt.Printf("\n\n   VM Step Trace\n")
	fmt.Printf("   Current Fiber   : %v\n", vm.Fiber.Description())
	if vm.Fiber.parentFiber != nil {
		fmt.Printf("   Parent  Fiber   : %v\n", vm.Fiber.parentFiber.Description())
	} else {
		fmt.Printf("   Parent  Fiber   : %v\n", NilValue.Description())
	}
	fmt.Printf("   Opcode          : %v\n", opCodeDescription[opcode])
	fmt.Printf("   Operand         : %v\n", operand)
	if instruction&opcodeMask == OPFreeVariable {
		fmt.Printf("   IsLocal         : %v\n", instruction>>upperShift)
	} else if instruction&opcodeMask == OPExtension {
		fmt.Printf("   Types           : %v\n", instruction>>upperShift)
	}
	fmt.Printf("   -------------------\n")
	fmt.Printf("   Module Globals\n")
	for _, key := range vm.module.identifiers {
		if value, exists := (*vm.module.global.modules[vm.module.global.mainModuleName]).namespace[key]; exists {
			fmt.Printf("%27v : %v\n", key, value.Description())
		}
	}
	fmt.Printf("   -------------------\n")
	fmt.Printf("   FrameIdx        : %v\n", vm.frameIndex)
	fmt.Printf("   FunName         : %v\n", vm.frame.closure.Function.Name)
	fmt.Printf("   Pc              : %v\n", vm.frame.pc)
	fmt.Printf("   Fp              : %v\n", vm.frame.fp)
	fmt.Printf("   Top             : %v\n", vm.top)
	fmt.Printf("   FreeVarsCount   : %v\n", vm.frame.closure.Function.FreeVarCount)
	fmt.Printf("   FreVars         : %v\n", vm.frame.closure.FreeVars)
	fmt.Printf("   DeferCount      : %v\n", len(vm.frame.deferStack))
	fmt.Printf("   DeferStack      : %v\n", vm.frame.deferStack)
	fmt.Printf("   Line            : %v\n", vm.frame.closure.Function.Lines[vm.frame.pc])
	fmt.Printf("   StackSize       : %v\n", len(vm.stack))
	fmt.Printf("   -------------------\n\n\n")
	vm.stack[vm.top] = &String{Value: "|TOP|"}
	for i := Bytecode(0); i < vm.top+1; i++ {
		if i == vm.frame.fp {
			fmt.Printf("   FP( %v ) ", vm.stack[i].Description())
		} else {
			fmt.Printf("   %v ", vm.stack[i].Description())
		}
	}
	fmt.Printf("\n\n\n   Press 'enter' to run opcode '%v'   ~~>   ", opCodeDescription[opcode])
	fmt.Scanf(" ")
	vm.stack[vm.top] = nil
}

func (vm *VM) printVMStateBeforeLeavingDebugger() {
	fmt.Printf("   Current Fiber   : %v\n", vm.Fiber.Description())
	if vm.Fiber.parentFiber != nil {
		fmt.Printf("   Parent  Fiber   : %v\n", vm.Fiber.parentFiber.Description())
	} else {
		fmt.Printf("   Parent  Fiber   : %v\n", NilValue.Description())
	}
	fmt.Printf("   Successfull VM execution.\n\n\n")
}

func (vm *VM) printFinalFailureVMState() {
	fmt.Printf("\n\n   VM Final State:\n   Unsuccessfull VM execution\n\n\n")
}

// Trace runs the VM showing its state at each cycle of the VM.
func (vm *VM) runStepByStep(fiberFunctionName string) error {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Golang %v\n", r)
			debug.PrintStack()
		}
	}()
	for {
		vm.printVMState()
		// Fetchs the first instruction.
		instruction := vm.frame.closure.Function.Code[vm.frame.pc]
		opcode, operand := instruction&opcodeMask, instruction>>instructionShift
		vm.frame.pc++
		switch opcode {
		case OPConst:
			// |- kIndex -|- OpCode -|
			vm.stack[vm.top] = vm.frame.closure.Function.Constants[operand]
			vm.top++
		case OPTrue:
			// |- OPCode -|
			vm.stack[vm.top] = True
			vm.top++
		case OPFalse:
			// |- OPCode -|
			vm.stack[vm.top] = False
			vm.top++
		case OPNil:
			// |- OPCode -|
			vm.stack[vm.top] = NilValue
			vm.top++
		case OPClosure:
			// |- kIndex -|- OpCode -|
			fn := vm.frame.closure.Function.Constants[operand&operandMask].(Function)
			closure := Closure{Function: &fn}
			if instruction>>upperShift == 1 {
				for i := Bytecode(0); i < fn.FreeVarCount; i++ {
					// |- OpCode -|- index -|- isLocal -|
					freeVarInstr := vm.frame.closure.Function.Code[vm.frame.pc]
					if freeVarInstr>>upperShift == 1 {
						closure.FreeVars = append(closure.FreeVars, vm.stack[vm.frame.fp+(freeVarInstr>>instructionShift&operandMask)])
					} else {
						closure.FreeVars = append(closure.FreeVars, vm.frame.closure.FreeVars[freeVarInstr>>instructionShift&operandMask])
					}
					vm.frame.pc++
				}
			}
			if vm.frame.closure.Function.CanAccessPrivateState {
				closure.StructID = vm.frame.closure.StructID
			}
			vm.stack[vm.top] = closure
			vm.top++
		case OPStruct:
			// |- stuffCount -|- OpCode -|
			vm.top--
			structure := vm.stack[vm.top].(Struct)
			for i := Bytecode(0); i < operand; i++ {
				vm.top--
				value := vm.stack[vm.top].(Closure)
				value.StructID = structure.Id
				structure.Methods[value.Function.Name] = value
			}
			vm.stack[vm.top] = structure
			vm.top++
		case OPExtension:
			// |- OpCode -|- methodsCount -|- StructCount -|
			methodsCount, StructCount := operand&operandMask, instruction>>upperShift
			firstTopIndexMethods := vm.top - methodsCount
			for metaIndex := vm.top - (methodsCount + StructCount); metaIndex < firstTopIndexMethods; metaIndex++ {
				if Struct, ok := vm.stack[metaIndex].(Struct); ok {
					for j := firstTopIndexMethods; j < vm.top; j++ {
						method := vm.stack[j].(Closure)
						method.StructID = Struct.Id
						Struct.Methods[method.Function.Name] = method
					}
				} else {
					return ValueDoesNotSupportExtension(vm.stack[metaIndex])
				}
			}
			vm.top = vm.top - (methodsCount + StructCount)
		case OPDerive:
			// |- OPCode -|- DeriveCount -|
			vm.top--
			dest, _ := vm.stack[vm.top].(Struct)
			for i := vm.top - operand; i < vm.top; i++ {
				if source, ok := vm.stack[i].(Struct); ok {
					for key, value := range source.Public {
						if _, exists := dest.Public[key]; !exists {
							dest.Public[key] = value
						}
					}
					for key, value := range source.Private {
						if _, exists := dest.Private[key]; !exists {
							dest.Private[key] = value
						}
					}
					for key, value := range source.Methods {
						if _, exists := dest.Methods[key]; !exists {
							closure := value.(Closure)
							closure.StructID = dest.Id
							dest.Methods[key] = closure
						}
					}
				} else {
					return CannotDeriveFromValue(source)
				}
			}
			vm.stack[vm.top-operand] = dest
			vm.top -= operand - 1
		case OPEqual:
			// |- OpCode -|
			if vm.stack[vm.top-2].Equals(vm.stack[vm.top-1]) {
				vm.stack[vm.top-2] = True
			} else {
				vm.stack[vm.top-2] = False
			}
			vm.top--
		case OPNotEqual:
			// |- OpCode -|
			if vm.stack[vm.top-2].Equals(vm.stack[vm.top-1]) {
				vm.stack[vm.top-2] = False
			} else {
				vm.stack[vm.top-2] = True
			}
			vm.top--
		case OPNilChoice:
			// |- OpCode -|
			if _, ok := vm.stack[vm.top-2].(Nil); ok {
				vm.stack[vm.top-2] = vm.stack[vm.top-1]
			}
			vm.top--
		case OPBinop:
			// |- OpCode -|- Operator -|
			if value, err := vm.stack[vm.top-2].BinaryOp(byte(operand), vm.stack[vm.top-1]); err == nil {
				vm.stack[vm.top-2] = value
				vm.top--
			} else {
				return err
			}
		case OPPrefix:
			// |- OpCode -|- Operator -|
			if value, err := vm.stack[vm.top-1].PrefixOp(byte(operand)); err == nil {
				vm.stack[vm.top-1] = value
			} else {
				return err
			}
		case OPSubscript:
			// |- OpCode -|- Flag -|
			// Where Flag means:
			// OnlyExpression = [e]
			// ExprColonExpr = [e:e]
			// ExprColon = [e:]
			// ColonExpr = [:e]
			// OnlyColon = [:]
			switch operand {
			case onlyExpression:
				dataStrucIndex := vm.top - 2
				switch dataStruct := vm.stack[dataStrucIndex].(type) {
				case SubscriptOperable:
					if value, err := dataStruct.SubscriptGet(vm.stack[vm.top-1]); err == nil {
						vm.stack[dataStrucIndex] = value
						vm.top--
					} else {
						return err
					}
				default:
					return ValueDoesNotSupportSubscription(dataStruct)
				}
			case exprColon:
				dsIndex, low := vm.top-2, vm.stack[vm.top-1]
				switch dataStructure := vm.stack[dsIndex].(type) {
				case SliceOperable:
					if value, err := dataStructure.SliceGet(operand, low, Int(0)); err == nil {
						vm.stack[dsIndex] = value
						vm.top--
					} else {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(dataStructure)
				}
			case colonExpr:
				dsIndex, high := vm.top-2, vm.stack[vm.top-1]
				switch dataStructure := vm.stack[dsIndex].(type) {
				case SliceOperable:
					if value, err := dataStructure.SliceGet(operand, Int(0), high); err == nil {
						vm.stack[dsIndex] = value
						vm.top--
					} else {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(dataStructure)
				}
			case exprColonExpr:
				dsIndex, low, high := vm.top-3, vm.stack[vm.top-2], vm.stack[vm.top-1]
				switch dataStructure := vm.stack[dsIndex].(type) {
				case SliceOperable:
					if value, err := dataStructure.SliceGet(operand, low, high); err == nil {
						vm.stack[dsIndex] = value
						vm.top -= 2
					} else {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(dataStructure)
				}
			case onlyColon:
				dsIndex := vm.top - 1
				switch dataStructure := vm.stack[dsIndex].(type) {
				case SliceOperable:
					if value, err := dataStructure.SliceGet(operand, Int(0), Int(0)); err == nil {
						vm.stack[dsIndex] = value
					} else {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(dataStructure)
				}
			default:
				return NeverShouldHaveHappened("Wrong flag in OPSubscript")
			}
		case OPSelect:
			// |- OpCode -|
			objectIndex, property := vm.top-2, vm.stack[vm.top-1].(*String).Value
			switch object := vm.stack[objectIndex].(type) {
			case SelectorOperable:
				if value, err := object.SelectorGet(property); err == nil {
					vm.stack[objectIndex] = value
					vm.top--
				} else {
					return err
				}
			default:
				return SelectionOperationNotSupported(object)
			}
		case OPList:
			// |- Length -|- OpCode -|
			list := &List{Elements: make([]Value, operand)}
			vm.top = vm.top - operand
			resultRegister := vm.top
			for i := Bytecode(0); i < operand; i++ {
				list.Elements[i] = vm.stack[vm.top]
				vm.top++
			}
			vm.stack[resultRegister] = list
			resultRegister++
			vm.top = resultRegister
		case OPMap:
			// |- Length -|- OpCode -|
			vm.top = vm.top - operand
			resultRegister := vm.top
			dictionary := make(Map)
			for i := Bytecode(0); i < operand; i++ {
				key := vm.stack[vm.top]
				i++
				vm.top++
				value := vm.stack[vm.top]
				vm.top++
				if key.IsHashable() {
					dictionary[key.MakeHashKey()] = Pair{key: key, value: value}
				} else {
					return ValueNotHashableError(key)
				}
			}
			vm.stack[resultRegister] = dictionary
			resultRegister++
			vm.top = resultRegister
		case OPRecord:
			// |- Length -|- OpCode -|
			record := Record{Properties: make(Namespace)}
			for i := vm.top - operand; i < vm.top; i++ {
				key := vm.stack[i].(*String)
				i++
				record.Properties[key.Value] = vm.stack[i]
			}
			vm.top -= operand
			vm.stack[vm.top] = record
			vm.top++
		case OPConstNamespace:
			// |- OpCode -| ConstCount -|
			vm.top--
			constants := vm.stack[vm.top].(NamedConstants)
			for i, index := vm.top-operand, Bytecode(1); i < vm.top; i++ {
				value := vm.stack[i]
				if value.IsValueSemantics() {
					constants.Constants[constants.Indexes[index]] = value
					index++
				} else {
					return ValueIsNotValueSemantics(value)
				}
			}
			vm.top -= operand
			vm.stack[vm.top] = constants
			vm.top++
		case OPNewGlobal:
			// |- Global Index-|- OpCode -|
			vm.top--
			if _, exists := (*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).namespace[(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand]]; exists {
				return VariableAlreadyDefined((*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand])
			} else {
				(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).namespace[(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand]] = vm.stack[vm.top]
			}
		case OPGetGlobal:
			// |- Global Index-|- OpCode -|
			if value, exists := (*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).namespace[(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand]]; exists {
				vm.stack[vm.top] = value
				vm.top++
			} else {
				return VariableNotDefined((*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand])
			}
		case OPSetGlobal:
			// |- Global Index -|- OpCode -|
			if _, exists := (*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).namespace[(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand]]; exists {
				vm.top--
				(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).namespace[(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand]] = vm.stack[vm.top]
			} else {
				return VariableNotDefined((*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[operand])
			}
		case OPCompSetGlobal:
			// |- OpCode -|- Global Index -|- operator -|
			globalIndex, operator := operand&operandMask, instruction>>upperShift
			var err error
			var value Value
			var exists bool
			if value, exists = (*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).namespace[(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[globalIndex]]; exists {
				vm.stack[vm.top], vm.stack[vm.top-1] = vm.stack[vm.top-1], value
				vm.top++
				value, err = vm.stack[vm.top-2].BinaryOp(byte(operator), vm.stack[vm.top-1])
			} else {
				return VariableNotDefined((*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[globalIndex])
			}
			if err == nil {
				vm.top -= 2
				(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).namespace[(*vm.module.global.modules[vm.frame.closure.Function.ModuleName]).identifiers[globalIndex]] = value
			} else {
				return err
			}
		case OPGetLocal:
			// |- Local Index-|- OpCode -|
			vm.stack[vm.top] = vm.stack[vm.frame.fp+operand]
			vm.top++
		case OPSetLocal:
			// |- Local Index-|- OpCode -|
			vm.top--
			vm.stack[vm.frame.fp+operand] = vm.stack[vm.top]
		case OPCompSetLocal:
			// |- OpCode -|- Local Index-|- operator -|
			localIndex, operator := operand&operandMask, instruction>>upperShift
			vm.stack[vm.top], vm.stack[vm.top-1] = vm.stack[vm.top-1], vm.stack[vm.frame.fp+localIndex]
			vm.top++
			if value, err := vm.stack[vm.top-2].BinaryOp(byte(operator), vm.stack[vm.top-1]); err == nil {
				vm.top -= 2
				vm.stack[vm.frame.fp+localIndex] = value
			} else {
				return err
			}
		case OPMutDataStructure:
			// |- OpCode -|- Relative Expr Index -|- Flag -|- mutatingOperatorType -|
			// Where Flag means:
			// 		OnlyExpression	= val[e] | val.prop
			// 		ExprColonExpr 	= [e:e]
			// 		ExprColon 		= [e:]
			// 		ColonExpr 		= [:e]
			// 		OnlyColon 		= [:]
			// And mutating operator type means:
			//		MutatingOperatorSubscript	= []
			//		MutatingOperatorSelect 		= .
			operand = operand & operandMask
			flag := instruction >> upperShift
			mutatingOperatorType := flag >> 4
			mask := ^Bytecode(1 << 4)
			flag &= mask
			var err error
			switch flag {
			case onlyExpression:
				vm.top--
				indexIndex, value := vm.top-(operand+1), vm.stack[vm.top]
				dataStructIndex, index := indexIndex-1, vm.stack[indexIndex]
				if mutatingOperatorType == mutatingOperatorSubscript {
					switch ds := vm.stack[dataStructIndex].(type) {
					case SubscriptOperable:
						if err = ds.SubscriptSet(index, value); err != nil {
							return err
						}
					default:
						return ValueIsImmutable(ds)
					}
				} else {
					switch ds := vm.stack[dataStructIndex].(type) {
					case SelectorOperable:
						if err = ds.SelectorSet(index.(*String).Value, value); err != nil {
							return err
						}
					default:
						return ValueIsImmutable(ds)
					}
				}
				if err == nil {
					pos := vm.top - operand
					for i := Bytecode(0); i < operand; i++ {
						vm.stack[dataStructIndex] = vm.stack[pos]
						dataStructIndex++
						pos++
					}
					vm.top = dataStructIndex
				}
			case exprColon:
				vm.top--
				index, value := vm.top-(operand+1), vm.stack[vm.top]
				dataStructIndex := index - 1
				switch ds := vm.stack[dataStructIndex].(type) {
				case SliceOperable:
					if err = ds.SliceSet(flag, vm.stack[index], Int(0), value); err != nil {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					pos := vm.top - operand
					for i := Bytecode(0); i < operand; i++ {
						vm.stack[dataStructIndex] = vm.stack[pos]
						dataStructIndex++
						pos++
					}
					vm.top = dataStructIndex
				}
			case colonExpr:
				vm.top--
				index, value := vm.top-(operand+1), vm.stack[vm.top]
				dataStructIndex := index - 1
				switch ds := vm.stack[dataStructIndex].(type) {
				case SliceOperable:
					if err = ds.SliceSet(flag, Int(0), vm.stack[index], value); err != nil {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					pos := vm.top - operand
					for i := Bytecode(0); i < operand; i++ {
						vm.stack[dataStructIndex] = vm.stack[pos]
						dataStructIndex++
						pos++
					}
					vm.top = dataStructIndex
				}
			case exprColonExpr:
				vm.top--
				upperBound, value := vm.top-(operand+1), vm.stack[vm.top]
				lowerBound := upperBound - 1
				dataStructIndex := lowerBound - 1
				switch ds := vm.stack[dataStructIndex].(type) {
				case SliceOperable:
					if err = ds.SliceSet(flag, vm.stack[lowerBound], vm.stack[upperBound], value); err != nil {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					if operand != 0 {
						pos := vm.top - operand
						for i := Bytecode(0); i < operand; i++ {
							vm.stack[dataStructIndex] = vm.stack[pos]
							dataStructIndex++
							pos++
						}
					}
					vm.top = dataStructIndex
				}
			case onlyColon:
				vm.top--
				dataStructIndex, value := vm.top-(operand+1), vm.stack[vm.top]
				switch ds := vm.stack[dataStructIndex].(type) {
				case SliceOperable:
					if err = ds.SliceSet(flag, Int(0), Int(0), value); err != nil {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					if operand != 0 {
						pos := vm.top - operand
						for i := Bytecode(0); i < operand; i++ {
							vm.stack[dataStructIndex] = vm.stack[pos]
							dataStructIndex++
							pos++
						}
					}
					vm.top--
				}
			default:
				return NeverShouldHaveHappened("Unknown flag in OPMutDS")
			}
		case OPCompMutDataStructure:
			// |- OpCode 8 bits -|- Relative Expr Index 16 bits -|- operator 8 bits tokens(19-29) (add, sub, etc) -|
			// |- Flag 8 bits -|- Selector Operator Type 1 bit (0-1) ([], .) -|
			// Where Flag means:
			// 		OnlyExpression	= val[e] | val.prop
			// 		ExprColonExpr 	= [e:e]
			// 		ExprColon 		= [e:]
			// 		ColonExpr 		= [:e]
			// 		OnlyColon 		= [:]
			relativeIndex, operator := operand&operandMask, instruction>>upperShift
			instruction = vm.frame.closure.Function.Code[vm.frame.pc]
			vm.frame.pc++
			flag, selectorOperator := instruction&opcodeMask, instruction>>instructionShift
			var err error
			var value Value
			switch flag {
			case onlyExpression:
				index := (vm.top - 1) - (relativeIndex + 1)
				dataStructureIndex := index - 1
				dataStructure := vm.stack[dataStructureIndex]
				vm.stack[vm.top] = vm.stack[vm.top-1]
				if selectorOperator == mutatingOperatorSubscript {
					switch dataStruct := vm.stack[dataStructureIndex].(type) {
					case SubscriptOperable:
						if value, err = dataStruct.SubscriptGet(vm.stack[index]); err == nil {
							vm.stack[dataStructureIndex] = value
							vm.top--
						} else {
							return err
						}
					default:
						return ValueDoesNotSupportSubscription(dataStruct)
					}
				} else {
					property := vm.stack[index].(*String).Value
					switch object := vm.stack[dataStructureIndex].(type) {
					case SelectorOperable:
						if value, err = object.SelectorGet(property); err == nil {
							vm.stack[dataStructureIndex] = value
							vm.top--
						} else {
							return err
						}
					default:
						return SelectionOperationNotSupported(object)
					}
				}
				if err == nil {
					vm.stack[vm.top], vm.stack[dataStructureIndex] = vm.stack[dataStructureIndex], dataStructure
					vm.top += 2
					if value, err = vm.stack[vm.top-2].BinaryOp(byte(operator), vm.stack[vm.top-1]); err == nil {
						vm.top--
						vm.stack[vm.top-1] = value
						///---------- Mutate ds
						if selectorOperator == mutatingOperatorSubscript {
							switch ds := dataStructure.(type) {
							case SubscriptOperable:
								if err = ds.SubscriptSet(vm.stack[index], value); err != nil {
									return err
								}
							default:
								return ValueIsImmutable(ds)
							}
						} else {
							switch ds := dataStructure.(type) {
							case SelectorOperable:
								if err = ds.SelectorSet(vm.stack[index].(*String).Value, value); err != nil {
									return err
								}
							default:
								return ValueIsImmutable(ds)
							}
						}
						if err == nil {
							pos := vm.top - relativeIndex - 1
							for i := Bytecode(0); i < relativeIndex; i++ {
								vm.stack[dataStructureIndex] = vm.stack[pos]
								dataStructureIndex++
								pos++
							}
							vm.top = dataStructureIndex
						}
					} else {
						return err
					}
				}
			case exprColon:
				low := (vm.top - 1) - (relativeIndex + 1)
				dataStructureIndex := low - 1
				vm.top--
				value = vm.stack[vm.top]
				switch ds := vm.stack[dataStructureIndex].(type) {
				case SliceOperable:
					switch low := vm.stack[low].(type) {
					case IntegerNumber:
						if err = vm.compoundMutateSlice(vm.stack[dataStructureIndex], low.ToInt(), 0, operator, flag, value); err != nil {
							return err
						}
					default:
						return ValueIsNotAnIndexError(low)
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					if relativeIndex != 0 {
						pos := vm.top - relativeIndex
						for i := Bytecode(0); i < relativeIndex; i++ {
							vm.stack[dataStructureIndex] = vm.stack[pos]
							dataStructureIndex++
							pos++
						}
					}
					vm.top = dataStructureIndex
				}
			case colonExpr:
				high := (vm.top - 1) - (relativeIndex + 1)
				dataStructureIndex := high - 1
				vm.top--
				value = vm.stack[vm.top]
				switch ds := vm.stack[dataStructureIndex].(type) {
				case SliceOperable:
					switch high := vm.stack[high].(type) {
					case IntegerNumber:
						if err = vm.compoundMutateSlice(vm.stack[dataStructureIndex], 0, high.ToInt(), operator, flag, value); err != nil {
							return err
						}
					default:
						return ValueIsNotAnIndexError(high)
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					if relativeIndex != 0 {
						pos := vm.top - relativeIndex
						for i := Bytecode(0); i < relativeIndex; i++ {
							vm.stack[dataStructureIndex] = vm.stack[pos]
							dataStructureIndex++
							pos++
						}
					}
					vm.top = dataStructureIndex
				}
			case exprColonExpr:
				high := (vm.top - 1) - (relativeIndex + 1)
				low := high - 1
				dataStructureIndex := low - 1
				vm.top--
				value = vm.stack[vm.top]
				switch ds := vm.stack[dataStructureIndex].(type) {
				case SliceOperable:
					switch low := vm.stack[low].(type) {
					case IntegerNumber:
						switch high := vm.stack[high].(type) {
						case IntegerNumber:
							if err = vm.compoundMutateSlice(vm.stack[dataStructureIndex], low.ToInt(), high.ToInt(), operator, flag, value); err != nil {
								return err
							}
						default:
							return ValueIsNotAnIndexError(high)
						}
					default:
						return ValueIsNotAnIndexError(low)
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					if relativeIndex != 0 {
						pos := vm.top - relativeIndex
						for i := Bytecode(0); i < relativeIndex; i++ {
							vm.stack[dataStructureIndex] = vm.stack[pos]
							dataStructureIndex++
							pos++
						}
					}
					vm.top = dataStructureIndex
				}
			case onlyColon:
				dataStructureIndex := (vm.top - 1) - (relativeIndex + 1)
				vm.top--
				value = vm.stack[vm.top]
				switch ds := vm.stack[dataStructureIndex].(type) {
				case SliceOperable:
					if err = vm.compoundMutateSlice(vm.stack[dataStructureIndex], 0, 0, operator, flag, value); err != nil {
						return err
					}
				default:
					return ValueDoesNotSupportSlicing(ds)
				}
				if err == nil {
					if relativeIndex != 0 {
						pos := vm.top - relativeIndex
						for i := Bytecode(0); i < relativeIndex; i++ {
							vm.stack[dataStructureIndex] = vm.stack[pos]
							dataStructureIndex++
							pos++
						}
					}
					vm.top = dataStructureIndex
				}
			default:
				return NeverShouldHaveHappened("Wrong slice operation in OPCompMutDS")
			}
		case OPGetFreeVariable:
			// |- Local Index -|- OpCode -|
			vm.stack[vm.top] = vm.frame.closure.FreeVars[operand]
			vm.top++
		case OPSetFreeVariable:
			// |- FreeVar Index -|- OpCode -|
			vm.top--
			vm.frame.closure.FreeVars[operand] = vm.stack[vm.top]
		case OPCompSetFreeVariable:
			// |- OpCode -|- Relative Index -|- operator -|
			freeVarIndex, operator := operand&operandMask, instruction>>upperShift
			vm.stack[vm.top], vm.stack[vm.top-1] = vm.stack[vm.top-1], vm.frame.closure.FreeVars[freeVarIndex]
			vm.top++
			if value, err := vm.stack[vm.top-2].BinaryOp(byte(operator), vm.stack[vm.top-1]); err == nil {
				vm.top -= 2
				vm.frame.closure.FreeVars[freeVarIndex] = value
			} else {
				return err
			}
		case OPJumpIfFalse:
			// |- Jump Address -|- OpCode -|
			vm.top--
			if condition, ok := vm.stack[vm.top].(Bool); Bool(ok) && !condition {
				vm.frame.pc = operand
			} else if _, ok := vm.stack[vm.top].(Nil); ok {
				vm.frame.pc = operand
			}
		case OPGoto:
			// |- Jump Address -|- OpCode -|
			vm.frame.pc = operand
		case OPRange:
			// |- OpCode -|- ArgCount -|
			switch operand {
			case 1:
				switch value := vm.stack[vm.top-1].(type) {
				case Int:
					if value < 0 {
						vm.stack[vm.top-1] = NewRange(value, 0, 1, true)
					} else {
						vm.stack[vm.top-1] = NewRange(0, value, 1, true)
					}
				case *List:
					vm.stack[vm.top-1] = NewListIterator(value, true)
				case Map:
					vm.stack[vm.top-1] = NewMapIterator(value, true)
				case *String:
					vm.stack[vm.top-1] = NewStringIterator(value, true)
				case Set:
					vm.stack[vm.top-1] = NewSetIterator(value, true)
				case Record:
					vm.stack[vm.top-1] = NewRecordIterator(value, true)
				case *Bytes:
					vm.stack[vm.top-1] = NewBytesIterator(value, true)
				case Closure, Instance:
					continue
				default:
					return RangeExpressionError(value)
				}
			case 2:
				switch low := vm.stack[vm.top-2].(type) {
				case Int:
					switch high := vm.stack[vm.top-1].(type) {
					case Int:
						if low <= high {
							vm.stack[vm.top-2] = NewRange(low, high, 1, true)
						} else {
							vm.stack[vm.top-2] = NewRange(low, high, -1, false)
						}
						vm.top--
					default:
						return RangeExpressionError(high)
					}
				default:
					return RangeExpressionError(low)
				}
			case 3:
				switch low := vm.stack[vm.top-3].(type) {
				case Int:
					switch high := vm.stack[vm.top-2].(type) {
					case Int:
						switch step := vm.stack[vm.top-1].(type) {
						case Int:
							if low <= high {
								if step > 0 {
									vm.stack[vm.top-3] = NewRange(low, high, step, true)
									vm.top -= 2
								} else {
									return RangeExpectedPositiveValue(step)
								}
							} else {
								if step > 0 {
									vm.stack[vm.top-3] = NewRange(low, high, -step, false)
									vm.top -= 2
								} else {
									return RangeExpectedPositiveValue(step)
								}
							}
						default:
							return RangeExpectedIntegerValue(step)
						}
					default:
						return RangeExpectedIntegerValue(high)
					}
				default:
					return RangeExpectedIntegerValue(low)
				}
			}
		case OPNext:
			// |- OpCode -|- Jump Address -|
			iteratorAddress := vm.top - 1
			iterable := vm.stack[iteratorAddress]
			if iterable.IsIterable() {
				iterator := iterable.MakeIterator()
				nextValue := iterator.Next()
				switch nextValue.(type) {
				case IStop:
					vm.frame.pc = operand
					continue
				default:
					vm.stack[vm.top-2] = nextValue
					vm.stack[iteratorAddress] = iterator.(Value)
					if vm.frame.closure.Function.Code[vm.frame.pc]&opcodeMask == OPUnpackFor {
						idCount := vm.frame.closure.Function.Code[vm.frame.pc] >> instructionShift
						switch collection := vm.stack[vm.top-2].(type) {
						case *List:
							if len(collection.Elements) == int(idCount) {
								for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
									vm.stack[i] = collection.Elements[index]
									index++
								}
							} else {
								return UnpackCountDoesNotMatchError()
							}
						case Pair:
							if idCount == 2 {
								vm.stack[vm.top-idCount-1] = collection.key
								vm.stack[vm.top-idCount] = collection.value
							} else {
								return UnpackCountDoesNotMatchError()
							}
						case Tuple:
							if idCount == 2 {
								vm.stack[vm.top-idCount-1] = collection.index
								vm.stack[vm.top-idCount] = collection.value
							} else {
								return UnpackCountDoesNotMatchError()
							}
						case Result:
							if idCount == 2 {
								vm.stack[vm.top-idCount-1] = collection.Value
								vm.stack[vm.top-idCount] = collection.Error
							} else {
								return UnpackCountDoesNotMatchError()
							}
						case IStop:
							for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
								vm.stack[i] = IStopValue
								index++
							}
							continue
						default:
							return ExpectedUnpackableValueError(vm.stack[vm.top-2])
						}
						vm.frame.pc++
					}
				}
			} else {
				switch iterable := iterable.(type) {
				case Iterator:
					nextValue := iterable.Next()
					switch nextValue.(type) {
					case IStop:
						vm.frame.pc = operand
						continue
					default:
						vm.stack[vm.top-2] = nextValue
						if vm.frame.closure.Function.Code[vm.frame.pc]&opcodeMask == OPUnpackFor {
							idCount := vm.frame.closure.Function.Code[vm.frame.pc] >> instructionShift
							switch collection := vm.stack[vm.top-2].(type) {
							case *List:
								if len(collection.Elements) == int(idCount) {
									for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
										vm.stack[i] = collection.Elements[index]
										index++
									}
								} else {
									return UnpackCountDoesNotMatchError()
								}
							case Pair:
								if idCount == 2 {
									vm.stack[vm.top-idCount-1] = collection.key
									vm.stack[vm.top-idCount] = collection.value
								} else {
									return UnpackCountDoesNotMatchError()
								}
							case Tuple:
								if idCount == 2 {
									vm.stack[vm.top-idCount-1] = collection.index
									vm.stack[vm.top-idCount] = collection.value
								} else {
									return UnpackCountDoesNotMatchError()
								}
							case Result:
								if idCount == 2 {
									vm.stack[vm.top-idCount-1] = collection.Value
									vm.stack[vm.top-idCount] = collection.Error
								} else {
									return UnpackCountDoesNotMatchError()
								}
							case IStop:
								for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
									vm.stack[i] = IStopValue
									index++
								}
								continue
							default:
								return ExpectedUnpackableValueError(vm.stack[vm.top-2])
							}
							vm.frame.pc++
						}
					}
				case Instance:
					if method, exists := iterable.Struct.Methods[iteratorNextMethod].(Closure); exists {
						if method.Function.Arity == 1 {
							fiber := fiberPool.Get().(*Fiber)
							fiber.reset(method)
							fiber.parentFiber = globalState.currentFiber
							fiber.state = fiberRunning
							fiber.parentFiber.state = fiberWaiting
							globalState.currentFiber = fiber
							globalState.vm.Fiber = fiber
							globalState.vm.stack[globalState.vm.top] = iterable
							globalState.vm.top++
							if err := globalState.vm.runInterpreter(method.Function.Name); err != nil {
								fiberPool.Put(fiber)
								return err
							}
							nextValue := globalState.vm.stack[globalState.vm.top-1]
							globalState.currentFiber = globalState.currentFiber.parentFiber
							globalState.vm.Fiber = globalState.currentFiber
							globalState.currentFiber.state = fiberRunning
							fiberPool.Put(fiber)
							switch nextValue.(type) {
							case IStop:
								vm.frame.pc = operand
								continue
							default:
								vm.stack[vm.top-2] = nextValue
								if vm.frame.closure.Function.Code[vm.frame.pc]&opcodeMask == OPUnpackFor {
									idCount := vm.frame.closure.Function.Code[vm.frame.pc] >> instructionShift
									switch collection := vm.stack[vm.top-2].(type) {
									case *List:
										if len(collection.Elements) == int(idCount) {
											for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
												vm.stack[i] = collection.Elements[index]
												index++
											}
										} else {
											return UnpackCountDoesNotMatchError()
										}
									case Pair:
										if idCount == 2 {
											vm.stack[vm.top-idCount-1] = collection.key
											vm.stack[vm.top-idCount] = collection.value
										} else {
											return UnpackCountDoesNotMatchError()
										}
									case Tuple:
										if idCount == 2 {
											vm.stack[vm.top-idCount-1] = collection.index
											vm.stack[vm.top-idCount] = collection.value
										} else {
											return UnpackCountDoesNotMatchError()
										}
									case Result:
										if idCount == 2 {
											vm.stack[vm.top-idCount-1] = collection.Value
											vm.stack[vm.top-idCount] = collection.Error
										} else {
											return UnpackCountDoesNotMatchError()
										}
									case IStop:
										for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
											vm.stack[i] = IStopValue
											index++
										}
										continue
									default:
										return ExpectedUnpackableValueError(vm.stack[vm.top-2])
									}
									vm.frame.pc++
								}
							}
						} else {
							return OverloadedOperatorWithWrongArity(iteratorNextMethod, 1, method.Function.Arity)
						}
					} else {
						return MethodNotOverloaded(iteratorNextMethod, iterable.Struct.Name)
					}
				case Closure:
					if iterable.Function.Arity == 0 {
						fiber := fiberPool.Get().(*Fiber)
						fiber.reset(iterable)
						fiber.parentFiber = globalState.currentFiber
						fiber.state = fiberRunning
						fiber.parentFiber.state = fiberWaiting
						globalState.currentFiber = fiber
						globalState.vm.Fiber = fiber
						globalState.vm.stack[globalState.vm.top] = iterable
						globalState.vm.top++
						if err := globalState.vm.runInterpreter(iterable.Function.Name); err != nil {
							fiberPool.Put(fiber)
							return err
						}
						nextValue := globalState.vm.stack[globalState.vm.top-1]
						globalState.currentFiber = globalState.currentFiber.parentFiber
						globalState.vm.Fiber = globalState.currentFiber
						globalState.currentFiber.state = fiberRunning
						fiberPool.Put(fiber)
						switch nextValue.(type) {
						case IStop:
							vm.frame.pc = operand
							continue
						default:
							vm.stack[vm.top-2] = nextValue
							if vm.frame.closure.Function.Code[vm.frame.pc]&opcodeMask == OPUnpackFor {
								idCount := vm.frame.closure.Function.Code[vm.frame.pc] >> instructionShift
								switch collection := vm.stack[vm.top-2].(type) {
								case *List:
									if len(collection.Elements) == int(idCount) {
										for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
											vm.stack[i] = collection.Elements[index]
											index++
										}
									} else {
										return UnpackCountDoesNotMatchError()
									}
								case Pair:
									if idCount == 2 {
										vm.stack[vm.top-idCount-1] = collection.key
										vm.stack[vm.top-idCount] = collection.value
									} else {
										return UnpackCountDoesNotMatchError()
									}
								case Tuple:
									if idCount == 2 {
										vm.stack[vm.top-idCount-1] = collection.index
										vm.stack[vm.top-idCount] = collection.value
									} else {
										return UnpackCountDoesNotMatchError()
									}
								case Result:
									if idCount == 2 {
										vm.stack[vm.top-idCount-1] = collection.Value
										vm.stack[vm.top-idCount] = collection.Error
									} else {
										return UnpackCountDoesNotMatchError()
									}
								case IStop:
									for i, index := vm.top-idCount-1, 0; i < vm.top-1; i++ {
										vm.stack[i] = IStopValue
										index++
									}
									continue
								default:
									return ExpectedUnpackableValueError(vm.stack[vm.top-2])
								}
								vm.frame.pc++
							}
						}
					} else {
						return ArityGeneratorError(iterable.Function.Name)
					}
				default:
					return ExpectedIterableValueError(iterable)
				}
			}
		case OPMatch:
			// |- Jumpt Address if Match -|- OpCode -|
			value, pattern := vm.top-1, vm.top-2
			switch iterable := vm.stack[pattern].(type) {
			case *NumericIterator:
				if number, ok := vm.stack[value].(Int); ok {
					if number >= iterable.Value && number <= iterable.End {
						vm.frame.pc = operand
					}
				} else {
					return fmt.Errorf("type error in switch range pattern")
				}
			default:
				if vm.stack[pattern].Equals(vm.stack[value]) {
					vm.frame.pc = operand
				}
			}
			vm.top -= 2
		case OPCall:
			// |- ArgCount -|- OpCode -|- SpreadFlag -|
			operand &= operandMask
			if instruction>>upperShift == 1 {
				switch dataStructure := vm.stack[vm.top-1].(type) {
				case *List:
					vm.top--
					for _, v := range dataStructure.Elements {
						vm.stack[vm.top] = v
						vm.top++
					}
					operand += Bytecode(len(dataStructure.Elements) - 1)
				default:
					return ExpectedListToSpreadError(dataStructure)
				}
			}
			switch callable := vm.stack[vm.top-operand-1].(type) {
			case Closure:
				if vm.frameIndex == stackOverflowLimit {
					return StackOverfloError()
				}
				if callable.Function.Vararg && callable.Function.Arity <= operand {
					if callable.Function == vm.frame.closure.Function {
						nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
						if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
							elements := make([]Value, 0, operand-callable.Function.Arity)
							for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
								elements = append(elements, vm.stack[i])
							}
							vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
							zeroPos := vm.frame.fp
							vm.top = vm.top - operand + callable.Function.Arity
							for i := vm.top - callable.Function.Arity; i < vm.top; i++ {
								vm.stack[zeroPos] = vm.stack[i]
								zeroPos++
							}
							vm.stack[zeroPos] = vm.stack[vm.top]
							vm.top = vm.frame.fp + callable.Function.Arity + 1
							vm.frame.pc = 0
							continue
						}
					}
					//===============================
					// Caller Frame
					//===============================
					elements := make([]Value, 0, operand-callable.Function.Arity)
					for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
						elements = append(elements, vm.stack[i])
					}
					vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
					vm.frame.ret = vm.top - operand - 1
					vm.top = vm.frame.ret + callable.Function.Arity + 2
					//===============================
					// Callee Frame
					//===============================
					vm.frameIndex++
					vm.frame = &vm.stackFrame[vm.frameIndex]
					vm.frame.closure = callable
					vm.frame.pc = 0
					vm.frame.fp = vm.top - callable.Function.Arity - 1
				} else if callable.Function.Arity == operand {
					if callable.Function == vm.frame.closure.Function {
						nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
						if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
							zeroPos := vm.frame.fp
							for i := vm.top - operand; i < vm.top; i++ {
								vm.stack[zeroPos] = vm.stack[i]
								zeroPos++
							}
							vm.top = zeroPos
							vm.frame.pc = 0
							continue
						}
					}
					//===============================
					// Caller Frame
					//===============================
					vm.frame.ret = vm.top - operand - 1
					//===============================
					// Callee Frame
					//===============================
					vm.frameIndex++
					vm.frame = &vm.stackFrame[vm.frameIndex]
					vm.frame.closure = callable
					vm.frame.pc = 0
					vm.frame.fp = vm.top - operand
				} else {
					return fmt.Errorf(fmt.Sprintf("expected at least %v args and got %v", callable.Function.Arity, operand))
				}
			case GFunction:
				runningFiberPointer := globalState.currentFiber
				if result, err := callable.Value(vm.stack[vm.top-operand : vm.top]...); err == nil {
					if runningFiberPointer == globalState.currentFiber {
						vm.stack[vm.top-operand-1] = result
						vm.top -= operand
					} else {
						runningFiberPointer.frame.ret = runningFiberPointer.top - operand - 1
						runningFiberPointer.top = runningFiberPointer.frame.ret + 1
					}
				} else {
					return err
				}
			case Struct:
				if operand <= Bytecode(len(callable.Public)+len(callable.Private))*2 && operand%2 == 0 {
					newInstance := Instance{Id: globalInstanceUniqueID, Struct: &callable, Public: make(Namespace), Private: make(Namespace)}
					globalInstanceUniqueID++
					for k, v := range callable.Public {
						newInstance.Public[k] = v
					}
					for k, v := range callable.Private {
						newInstance.Private[k] = v
					}
					everythingOk := true
					var err error
				innerLoop2:
					for i := vm.top - operand; i < vm.top; i++ {
						if propertyName, ok := vm.stack[i].(*String); ok {
							i++
							if _, exists := callable.Public[propertyName.Value]; exists {
								newInstance.Public[propertyName.Value] = vm.stack[i]
							} else if _, exists := callable.Private[propertyName.Value]; exists {
								newInstance.Private[propertyName.Value] = vm.stack[i]
							} else {
								err = IsNotMethodProperty(propertyName.Value, callable.Name)
								everythingOk = false
								globalInstanceUniqueID--
								break innerLoop2
							}
						} else {
							err = IsNotMethodProperty(vm.stack[i].Description(), callable.Name)
							everythingOk = false
							globalInstanceUniqueID--
							break innerLoop2
						}
					}
					if everythingOk {
						vm.stack[vm.top-operand-1] = newInstance
						vm.top -= operand
					} else {
						return err
					}
				} else {
					return fmt.Errorf(fmt.Sprintf("expected %v pairs (property:value) args in type constructor", len(callable.Public)))
				}
			case Instance:
				if vm.frameIndex == stackOverflowLimit {
					return StackOverfloError()
				}
				if method, okMethod := callable.Struct.Methods[operatorCall].(Closure); okMethod {
					if method.Function.Vararg && method.Function.Arity-1 <= operand {
						if method.Function == vm.frame.closure.Function {
							nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
							if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
								elements := make([]Value, 0, operand-method.Function.Arity+1)
								for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
									elements = append(elements, vm.stack[i])
								}
								vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
								zeroPos := vm.frame.fp + 1
								vm.top = vm.top - operand + method.Function.Arity - 1
								for i := vm.top - method.Function.Arity + 1; i < vm.top; i++ {
									vm.stack[zeroPos] = vm.stack[i]
									zeroPos++
								}
								vm.stack[zeroPos] = vm.stack[vm.top]
								zeroPos++
								vm.top = zeroPos
								vm.frame.pc = 0
								continue
							}
						}
						//===============================
						// Caller Frame
						//===============================
						elements := make([]Value, 0, operand-method.Function.Arity+1)
						for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
							elements = append(elements, vm.stack[i])
						}
						vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
						vm.frame.ret = vm.top - operand - 1
						vm.top = vm.frame.ret + method.Function.Arity + 1
						//===============================
						// Callee Frame
						//===============================
						vm.frameIndex++
						vm.frame = &vm.stackFrame[vm.frameIndex]
						vm.frame.closure = method
						vm.frame.pc = 0
						vm.frame.fp = vm.top - method.Function.Arity - 1
					} else if method.Function.Arity-1 == operand {
						if method.Function == vm.frame.closure.Function {
							nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
							if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
								zeroPos := vm.frame.fp + 1
								for i := vm.top - operand; i < vm.top; i++ {
									vm.stack[zeroPos] = vm.stack[i]
									zeroPos++
								}
								vm.top = zeroPos
								vm.frame.pc = 0
								continue
							}
						}
						//===============================
						// Caller Frame
						//===============================
						vm.frame.ret = vm.top - operand - 1
						//===============================
						// Callee Frame
						//===============================
						vm.frameIndex++
						vm.frame = &vm.stackFrame[vm.frameIndex]
						vm.frame.closure = method
						vm.frame.pc = 0
						vm.frame.fp = vm.top - operand - 1
					} else {
						return fmt.Errorf(fmt.Sprintf("expected at %v args and got %v", method.Function.Arity-1, operand))
					}
				} else {
					return IsNotMethodProperty(operatorCall, callable.TypeName())
				}
			default:
				return fmt.Errorf(fmt.Sprintf("calling a no callable value of type %v", callable.TypeName()))
			}
		case OPDefer:
			// |- ArgCount -|- OpCode -|
			fnIndex := vm.top - operand - 1
			var df deferInfo
			switch callable := vm.stack[fnIndex].(type) {
			case Closure:
				df = deferInfo{callable: callable, typeofCallable: typeFunction}
			case GFunction:
				df = deferInfo{callable: callable, typeofCallable: typeGFunction}
			case Struct:
				df = deferInfo{callable: callable, typeofCallable: typeStruct}
			case Instance:
				df = deferInfo{callable: callable, typeofCallable: typeInstance}
			default:
				return ExpectedCallableValueInDeferError(vm.stack[fnIndex])
			}
			offset := operand
			for i := Bytecode(0); i < operand; i++ {
				df.args = append(df.args, vm.stack[vm.top-offset])
				offset--
			}
			vm.top = fnIndex
			df.line = vm.frame.closure.Function.Lines[vm.frame.pc-1]
			vm.frame.deferStack = append(vm.frame.deferStack, df)
			vm.frame.hasDefer = true
		case OPDeferInvoke:
			// |- ArgCount -|- OpCode -|
			attributeIndex := vm.top - (operand + 1)
			objectIndex := attributeIndex - 1
			deferInfo := deferInfo{attribute: vm.stack[attributeIndex].(*String).Value, object: vm.stack[objectIndex], typeofCallable: typeInvoke}
			offset := operand
			for i := Bytecode(0); i < operand; i++ {
				deferInfo.args = append(deferInfo.args, vm.stack[vm.top-offset])
				offset--
			}
			vm.top = objectIndex
			deferInfo.line = vm.frame.closure.Function.Lines[vm.frame.pc-1]
			vm.frame.deferStack = append(vm.frame.deferStack, deferInfo)
			vm.frame.hasDefer = true
		case OPRunDefer:
			// |- OpCode -|
			vm.performDefer()
		case OPReturn:
			// |- Number elements to return -|- OpCode -|
			// The result of the function is always at stack[top - 1]
			var result Value
			if operand == 1 {
				result = vm.stack[vm.top-1]
			} else {
				// When there are two or more values returned, the last one starts at stack[top - 1]
				xs := make([]Value, 0, operand)
				for i := vm.top - operand; i < vm.top; i++ {
					xs = append(xs, vm.stack[i])
				}
				result = &List{Elements: xs}
			}
			if vm.frameIndex == 0 && vm.stackFrame[0].closure.Function.Name == fiberFunctionName {
				// The main function of the current fiber is returning, so the thread must end here.
				vm.stack[vm.top-1] = result
				return nil
			} else if vm.frameIndex == 0 {
				// A Fiber is terminating here, so a context switch must happen.
				globalState.currentFiber.state = fiberTerminated
				globalState.currentFiber = globalState.currentFiber.parentFiber
				globalState.currentFiber.state = fiberRunning
				globalState.vm.Fiber = globalState.currentFiber
				vm.stack[vm.frame.ret] = result
				vm.top = vm.frame.ret + 1
			} else {
				// ===============================
				// Caller Frame
				vm.frameIndex--
				vm.frame = &vm.stackFrame[vm.frameIndex]
				vm.stack[vm.frame.ret] = result
				vm.top = vm.frame.ret + 1
			}
		case OPUnpack:
			// |- idCount -|- OpCode -|
			vm.top--
			switch unpackable := vm.stack[vm.top].(type) {
			case *List:
				if len(unpackable.Elements) >= int(operand) {
					for i := 0; i < int(operand); i++ {
						vm.stack[vm.top] = unpackable.Elements[i]
						vm.top++
					}
				} else {
					return UnpackCountDoesNotMatchError()
				}
			case Result:
				if operand == 2 {
					vm.stack[vm.top] = unpackable.Value
					vm.top++
					vm.stack[vm.top] = unpackable.Error
					vm.top++
				} else {
					return UnpackCountDoesNotMatchError()
				}
			case Pair:
				if operand == 2 {
					vm.stack[vm.top] = unpackable.key
					vm.top++
					vm.stack[vm.top] = unpackable.value
					vm.top++
				} else {
					return UnpackCountDoesNotMatchError()
				}
			case Tuple:
				if operand == 2 {
					vm.stack[vm.top] = unpackable.index
					vm.top++
					vm.stack[vm.top] = unpackable.value
					vm.top++
				} else {
					return UnpackCountDoesNotMatchError()
				}
			default:
				return ExpectedUnpackableValueError(unpackable)
			}
		case OPUnpackFor:
			// |- idCount -|- OpCode -|
			switch collection := vm.stack[vm.top-2].(type) {
			case *List:
				if len(collection.Elements) == int(operand) {
					for i, index := vm.top-operand-1, 0; i < vm.top-1; i++ {
						vm.stack[i] = collection.Elements[index]
						index++
					}
				} else {
					return UnpackCountDoesNotMatchError()
				}
			case Pair:
				if operand == 2 {
					vm.stack[vm.top-operand-1] = collection.key
					vm.stack[vm.top-operand] = collection.value
				} else {
					return UnpackCountDoesNotMatchError()
				}
			case Tuple:
				if operand == 2 {
					vm.stack[vm.top-operand-1] = collection.index
					vm.stack[vm.top-operand] = collection.value
				} else {
					return UnpackCountDoesNotMatchError()
				}
			case Result:
				if operand == 2 {
					vm.stack[vm.top-operand-1] = collection.Value
					vm.stack[vm.top-operand] = collection.Error
				} else {
					return UnpackCountDoesNotMatchError()
				}
			case IStop:
				for i, index := vm.top-operand-1, 0; i < vm.top-1; i++ {
					vm.stack[i] = IStopValue
					index++
				}
				continue
			default:
				return ExpectedUnpackableValueError(vm.stack[vm.top-2])
			}
		case OPInvokeMethod:
			// |- ArgCount -|- OpCode -|- SpreadFlag -|
			operand &= operandMask
			methodAddr := int(vm.top - (operand + 1))
			objectAddr := int(methodAddr - 1)
			object := vm.stack[objectAddr]
			methodName := vm.stack[methodAddr].(*String)
			if object.HasMethods() {
				if method, isMethod, err := object.GetMethod(methodName.Value); err == nil {
					if isMethod {
						vm.stack[objectAddr], vm.stack[methodAddr] = method, object
						operand++
						if instruction>>upperShift == 1 {
							switch dataStructure := vm.stack[vm.top-1].(type) {
							case *List:
								vm.top--
								for _, v := range dataStructure.Elements {
									vm.stack[vm.top] = v
									vm.top++
								}
								operand += Bytecode(len(dataStructure.Elements) - 1)
							default:
								return ExpectedListToSpreadError(dataStructure)
							}
						}
						switch callable := method.(type) {
						case Closure:
							if vm.frameIndex == stackOverflowLimit {
								return StackOverfloError()
							}
							if callable.Function.Vararg && callable.Function.Arity <= operand {
								if callable.Function == vm.frame.closure.Function {
									nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
									if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
										elements := make([]Value, 0, operand-callable.Function.Arity)
										for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
											elements = append(elements, vm.stack[i])
										}
										vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
										zeroPos := vm.frame.fp
										vm.top = vm.top - operand + callable.Function.Arity
										for i := vm.top - callable.Function.Arity; i < vm.top; i++ {
											vm.stack[zeroPos] = vm.stack[i]
											zeroPos++
										}
										vm.stack[zeroPos] = vm.stack[vm.top]
										vm.top = vm.frame.fp + callable.Function.Arity + 1
										vm.frame.pc = 0
										continue
									}
								}
								//===============================
								// Caller Frame
								//===============================
								elements := make([]Value, 0, operand-callable.Function.Arity)
								for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
									elements = append(elements, vm.stack[i])
								}
								vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
								vm.frame.ret = vm.top - operand - 1
								vm.top = vm.frame.ret + callable.Function.Arity + 2
								//===============================
								// Callee Frame
								//===============================
								vm.frameIndex++
								vm.frame = &vm.stackFrame[vm.frameIndex]
								vm.frame.closure = callable
								vm.frame.pc = 0
								vm.frame.fp = vm.top - callable.Function.Arity - 1
							} else if callable.Function.Arity == operand {
								if callable.Function == vm.frame.closure.Function {
									nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
									if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
										zeroPos := vm.frame.fp
										for i := vm.top - operand; i < vm.top; i++ {
											vm.stack[zeroPos] = vm.stack[i]
											zeroPos++
										}
										vm.top = zeroPos
										vm.frame.pc = 0
										continue
									}
								}
								//===============================
								// Caller Frame
								//===============================
								vm.frame.ret = vm.top - operand - 1
								//===============================
								// Callee Frame
								//===============================
								vm.frameIndex++
								vm.frame = &vm.stackFrame[vm.frameIndex]
								vm.frame.closure = callable
								vm.frame.pc = 0
								vm.frame.fp = vm.top - operand
							} else {
								return fmt.Errorf(fmt.Sprintf("expected at least %v args and got %v", callable.Function.Arity, operand))
							}
						case GFunction:
							runningFiberPointer := globalState.currentFiber
							if result, err := callable.Value(vm.stack[vm.top-operand : vm.top]...); err == nil {
								if runningFiberPointer == globalState.currentFiber {
									vm.stack[vm.top-operand-1] = result
									vm.top -= operand
								} else {
									runningFiberPointer.frame.ret = runningFiberPointer.top - operand - 1
									runningFiberPointer.top = runningFiberPointer.frame.ret + 1
								}
							} else {
								return err
							}
						case Struct:
							if operand <= Bytecode(len(callable.Public)+len(callable.Private))*2 && operand%2 == 0 {
								newInstance := Instance{Id: globalInstanceUniqueID, Struct: &callable, Public: make(Namespace), Private: make(Namespace)}
								globalInstanceUniqueID++
								for k, v := range callable.Public {
									newInstance.Public[k] = v
								}
								for k, v := range callable.Private {
									newInstance.Private[k] = v
								}
								everythingOk := true
								var err error
							innerLoop3:
								for i := vm.top - operand; i < vm.top; i++ {
									if propertyName, ok := vm.stack[i].(*String); ok {
										i++
										if _, exists := callable.Public[propertyName.Value]; exists {
											newInstance.Public[propertyName.Value] = vm.stack[i]
										} else if _, exists := callable.Private[propertyName.Value]; exists {
											newInstance.Private[propertyName.Value] = vm.stack[i]
										} else {
											err = IsNotMethodProperty(propertyName.Value, callable.Name)
											everythingOk = false
											globalInstanceUniqueID--
											break innerLoop3
										}
									} else {
										err = IsNotMethodProperty(vm.stack[i].Description(), callable.Name)
										everythingOk = false
										globalInstanceUniqueID--
										break innerLoop3
									}
								}
								if everythingOk {
									vm.stack[vm.top-operand-1] = newInstance
									vm.top -= operand
								} else {
									return err
								}
							} else {
								return fmt.Errorf(fmt.Sprintf("expected %v pairs (property:value) args in type constructor", len(callable.Public)))
							}
						case Instance:
							if vm.frameIndex == stackOverflowLimit {
								return StackOverfloError()
							}
							if method, okMethod := callable.Struct.Methods[operatorCall].(Closure); okMethod {
								if method.Function.Vararg && method.Function.Arity-1 <= operand {
									if method.Function == vm.frame.closure.Function {
										nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
										if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
											elements := make([]Value, 0, operand-method.Function.Arity+1)
											for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
												elements = append(elements, vm.stack[i])
											}
											vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
											zeroPos := vm.frame.fp + 1
											vm.top = vm.top - operand + method.Function.Arity - 1
											for i := vm.top - method.Function.Arity + 1; i < vm.top; i++ {
												vm.stack[zeroPos] = vm.stack[i]
												zeroPos++
											}
											vm.stack[zeroPos] = vm.stack[vm.top]
											zeroPos++
											vm.top = zeroPos
											vm.frame.pc = 0
											continue
										}
									}
									//===============================
									// Caller Frame
									//===============================
									elements := make([]Value, 0, operand-method.Function.Arity+1)
									for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
										elements = append(elements, vm.stack[i])
									}
									vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
									vm.frame.ret = vm.top - operand - 1
									vm.top = vm.frame.ret + method.Function.Arity + 1
									//===============================
									// Callee Frame
									//===============================
									vm.frameIndex++
									vm.frame = &vm.stackFrame[vm.frameIndex]
									vm.frame.closure = method
									vm.frame.pc = 0
									vm.frame.fp = vm.top - method.Function.Arity - 1
								} else if method.Function.Arity-1 == operand {
									if method.Function == vm.frame.closure.Function {
										nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
										if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
											zeroPos := vm.frame.fp + 1
											for i := vm.top - operand; i < vm.top; i++ {
												vm.stack[zeroPos] = vm.stack[i]
												zeroPos++
											}
											vm.top = zeroPos
											vm.frame.pc = 0
											continue
										}
									}
									//===============================
									// Caller Frame
									//===============================
									vm.frame.ret = vm.top - operand - 1
									//===============================
									// Callee Frame
									//===============================
									vm.frameIndex++
									vm.frame = &vm.stackFrame[vm.frameIndex]
									vm.frame.closure = method
									vm.frame.pc = 0
									vm.frame.fp = vm.top - operand - 1
								} else {
									return fmt.Errorf(fmt.Sprintf("expected at %v args and got %v", method.Function.Arity-1, operand))
								}
							} else {
								return IsNotMethodProperty(operatorCall, callable.TypeName())
							}
						default:
							return fmt.Errorf(fmt.Sprintf("calling a no callable value of type %v", callable.TypeName()))
						}
					} else {
						vm.stack[objectAddr] = method
						for i := methodAddr + 1; i < int(vm.top); i++ {
							vm.stack[i-1] = vm.stack[i]
						}
						vm.top--
						if instruction>>upperShift == 1 {
							switch dataStructure := vm.stack[vm.top-1].(type) {
							case *List:
								vm.top--
								for _, v := range dataStructure.Elements {
									vm.stack[vm.top] = v
									vm.top++
								}
								operand += Bytecode(len(dataStructure.Elements) - 1)
							default:
								return ExpectedListToSpreadError(dataStructure)
							}
						}
						switch callable := method.(type) {
						case Closure:
							if vm.frameIndex == stackOverflowLimit {
								return StackOverfloError()
							}
							if callable.Function.Vararg && callable.Function.Arity <= operand {
								if callable.Function == vm.frame.closure.Function {
									nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
									if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
										elements := make([]Value, 0, operand-callable.Function.Arity)
										for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
											elements = append(elements, vm.stack[i])
										}
										vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
										zeroPos := vm.frame.fp
										vm.top = vm.top - operand + callable.Function.Arity
										for i := vm.top - callable.Function.Arity; i < vm.top; i++ {
											vm.stack[zeroPos] = vm.stack[i]
											zeroPos++
										}
										vm.stack[zeroPos] = vm.stack[vm.top]
										vm.top = vm.frame.fp + callable.Function.Arity + 1
										vm.frame.pc = 0
										continue
									}
								}
								//===============================
								// Caller Frame
								//===============================
								elements := make([]Value, 0, operand-callable.Function.Arity)
								for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
									elements = append(elements, vm.stack[i])
								}
								vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
								vm.frame.ret = vm.top - operand - 1
								vm.top = vm.frame.ret + callable.Function.Arity + 2
								//===============================
								// Callee Frame
								//===============================
								vm.frameIndex++
								vm.frame = &vm.stackFrame[vm.frameIndex]
								vm.frame.closure = callable
								vm.frame.pc = 0
								vm.frame.fp = vm.top - callable.Function.Arity - 1
							} else if callable.Function.Arity == operand {
								if callable.Function == vm.frame.closure.Function {
									nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
									if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
										zeroPos := vm.frame.fp
										for i := vm.top - operand; i < vm.top; i++ {
											vm.stack[zeroPos] = vm.stack[i]
											zeroPos++
										}
										vm.top = zeroPos
										vm.frame.pc = 0
										continue
									}
								}
								//===============================
								// Caller Frame
								//===============================
								vm.frame.ret = vm.top - operand - 1
								//===============================
								// Callee Frame
								//===============================
								vm.frameIndex++
								vm.frame = &vm.stackFrame[vm.frameIndex]
								vm.frame.closure = callable
								vm.frame.pc = 0
								vm.frame.fp = vm.top - operand
							} else {
								return fmt.Errorf(fmt.Sprintf("expected at least %v args and got %v", callable.Function.Arity, operand))
							}
						case GFunction:
							runningFiberPointer := globalState.currentFiber
							if result, err := callable.Value(vm.stack[vm.top-operand : vm.top]...); err == nil {
								if runningFiberPointer == globalState.currentFiber {
									vm.stack[vm.top-operand-1] = result
									vm.top -= operand
								} else {
									runningFiberPointer.frame.ret = runningFiberPointer.top - operand - 1
									runningFiberPointer.top = runningFiberPointer.frame.ret + 1
								}
							} else {
								return err
							}
						case Struct:
							if operand <= Bytecode(len(callable.Public)+len(callable.Private))*2 && operand%2 == 0 {
								newInstance := Instance{Id: globalInstanceUniqueID, Struct: &callable, Public: make(Namespace), Private: make(Namespace)}
								globalInstanceUniqueID++
								for k, v := range callable.Public {
									newInstance.Public[k] = v
								}
								for k, v := range callable.Private {
									newInstance.Private[k] = v
								}
								everythingOk := true
								var err error
							innerLoop:
								for i := vm.top - operand; i < vm.top; i++ {
									if propertyName, ok := vm.stack[i].(*String); ok {
										i++
										if _, exists := callable.Public[propertyName.Value]; exists {
											newInstance.Public[propertyName.Value] = vm.stack[i]
										} else if _, exists := callable.Private[propertyName.Value]; exists {
											newInstance.Private[propertyName.Value] = vm.stack[i]
										} else {
											err = IsNotMethodProperty(propertyName.Value, callable.Name)
											everythingOk = false
											globalInstanceUniqueID--
											break innerLoop
										}
									} else {
										err = IsNotMethodProperty(vm.stack[i].Description(), callable.Name)
										everythingOk = false
										globalInstanceUniqueID--
										break innerLoop
									}
								}
								if everythingOk {
									vm.stack[vm.top-operand-1] = newInstance
									vm.top -= operand
								} else {
									return err
								}
							} else {
								return fmt.Errorf(fmt.Sprintf("expected %v pairs (property:value) args in type constructor", len(callable.Public)))
							}
						case Instance:
							if vm.frameIndex == stackOverflowLimit {
								return StackOverfloError()
							}
							if method, okMethod := callable.Struct.Methods[operatorCall].(Closure); okMethod {
								if method.Function.Vararg && method.Function.Arity-1 <= operand {
									if method.Function == vm.frame.closure.Function {
										nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
										if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
											elements := make([]Value, 0, operand-method.Function.Arity+1)
											for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
												elements = append(elements, vm.stack[i])
											}
											vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
											zeroPos := vm.frame.fp + 1
											vm.top = vm.top - operand + method.Function.Arity - 1
											for i := vm.top - method.Function.Arity + 1; i < vm.top; i++ {
												vm.stack[zeroPos] = vm.stack[i]
												zeroPos++
											}
											vm.stack[zeroPos] = vm.stack[vm.top]
											zeroPos++
											vm.top = zeroPos
											vm.frame.pc = 0
											continue
										}
									}
									//===============================
									// Caller Frame
									//===============================
									elements := make([]Value, 0, operand-method.Function.Arity+1)
									for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
										elements = append(elements, vm.stack[i])
									}
									vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
									vm.frame.ret = vm.top - operand - 1
									vm.top = vm.frame.ret + method.Function.Arity + 1
									//===============================
									// Callee Frame
									//===============================
									vm.frameIndex++
									vm.frame = &vm.stackFrame[vm.frameIndex]
									vm.frame.closure = method
									vm.frame.pc = 0
									vm.frame.fp = vm.top - method.Function.Arity - 1
								} else if method.Function.Arity-1 == operand {
									if method.Function == vm.frame.closure.Function {
										nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
										if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
											zeroPos := vm.frame.fp + 1
											for i := vm.top - operand; i < vm.top; i++ {
												vm.stack[zeroPos] = vm.stack[i]
												zeroPos++
											}
											vm.top = zeroPos
											vm.frame.pc = 0
											continue
										}
									}
									//===============================
									// Caller Frame
									//===============================
									vm.frame.ret = vm.top - operand - 1
									//===============================
									// Callee Frame
									//===============================
									vm.frameIndex++
									vm.frame = &vm.stackFrame[vm.frameIndex]
									vm.frame.closure = method
									vm.frame.pc = 0
									vm.frame.fp = vm.top - operand - 1
								} else {
									return fmt.Errorf(fmt.Sprintf("expected at %v args and got %v", method.Function.Arity-1, operand))
								}
							} else {
								return IsNotMethodProperty(operatorCall, callable.TypeName())
							}
						default:
							return fmt.Errorf(fmt.Sprintf("calling a no callable value of type %v", callable.TypeName()))
						}
					}
				} else {
					return IsNotMethodProperty(methodName.Value, object.TypeName())
				}
			} else {
				var err error
				var method Value
				switch object := object.(type) {
				case Struct:
					if value, ok := object.Methods[methodName.Value]; ok {
						method = value
					} else {
						err = IsNotMethodProperty(methodName.Value, object.TypeName())
					}
				case GModule:
					if value, ok := object.Namespace[methodName.Value]; ok {
						method = value
					} else {
						err = IsNotMethodProperty(methodName.Value, object.TypeName())
					}
				case *VModule:
					if value, ok := object.namespace[methodName.Value]; ok {
						method = value
					} else {
						err = IsNotMethodProperty(methodName.Value, object.TypeName())
					}
				case Iterator:
					switch methodName.Value {
					case iteratorNext:
						method = GFunction{Name: methodName.Value, Value: func(args ...Value) (Value, error) {
							if len(args) == 0 {
								return object.Next(), nil
							}
							return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
						}}
					case iteratorExhausted:
						method = GFunction{Name: methodName.Value, Value: func(args ...Value) (Value, error) {
							if len(args) == 0 {
								return Bool(object.Exhausted()), nil
							}
							return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
						}}
					case iteratorNotExhausted:
						method = GFunction{Name: methodName.Value, Value: func(args ...Value) (Value, error) {
							if len(args) == 0 {
								return !Bool(object.Exhausted()), nil
							}
							return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
						}}
					default:
						err = fmt.Errorf("'%v' is not a method/property of the type '%v'", methodName.Value, object.(Value).TypeName())
					}
				default:
					return fmt.Errorf(fmt.Sprintf("method call or selection not supported by type %v", object.TypeName()))
				}
				if err == nil {
					vm.stack[objectAddr] = method
					for i := methodAddr + 1; i < int(vm.top); i++ {
						vm.stack[i-1] = vm.stack[i]
					}
					vm.top--
					if instruction>>upperShift == 1 {
						switch dataStructure := vm.stack[vm.top-1].(type) {
						case *List:
							vm.top--
							for _, v := range dataStructure.Elements {
								vm.stack[vm.top] = v
								vm.top++
							}
							operand += Bytecode(len(dataStructure.Elements) - 1)
						default:
							return ExpectedListToSpreadError(dataStructure)
						}
					}
					switch callable := method.(type) {
					case Closure:
						if vm.frameIndex == stackOverflowLimit {
							return StackOverfloError()
						}
						if callable.Function.Vararg && callable.Function.Arity <= operand {
							if callable.Function == vm.frame.closure.Function {
								nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
								if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
									elements := make([]Value, 0, operand-callable.Function.Arity)
									for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
										elements = append(elements, vm.stack[i])
									}
									vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
									zeroPos := vm.frame.fp
									vm.top = vm.top - operand + callable.Function.Arity
									for i := vm.top - callable.Function.Arity; i < vm.top; i++ {
										vm.stack[zeroPos] = vm.stack[i]
										zeroPos++
									}
									vm.stack[zeroPos] = vm.stack[vm.top]
									vm.top = vm.frame.fp + callable.Function.Arity + 1
									vm.frame.pc = 0
									continue
								}
							}
							//===============================
							// Caller Frame
							//===============================
							elements := make([]Value, 0, operand-callable.Function.Arity)
							for i := vm.top - operand + callable.Function.Arity; i < vm.top; i++ {
								elements = append(elements, vm.stack[i])
							}
							vm.stack[vm.top-operand+callable.Function.Arity] = &List{Elements: elements}
							vm.frame.ret = vm.top - operand - 1
							vm.top = vm.frame.ret + callable.Function.Arity + 2
							//===============================
							// Callee Frame
							//===============================
							vm.frameIndex++
							vm.frame = &vm.stackFrame[vm.frameIndex]
							vm.frame.closure = callable
							vm.frame.pc = 0
							vm.frame.fp = vm.top - callable.Function.Arity - 1
						} else if callable.Function.Arity == operand {
							if callable.Function == vm.frame.closure.Function {
								nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
								if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
									zeroPos := vm.frame.fp
									for i := vm.top - operand; i < vm.top; i++ {
										vm.stack[zeroPos] = vm.stack[i]
										zeroPos++
									}
									vm.top = zeroPos
									vm.frame.pc = 0
									continue
								}
							}
							//===============================
							// Caller Frame
							//===============================
							vm.frame.ret = vm.top - operand - 1
							//===============================
							// Callee Frame
							//===============================
							vm.frameIndex++
							vm.frame = &vm.stackFrame[vm.frameIndex]
							vm.frame.closure = callable
							vm.frame.pc = 0
							vm.frame.fp = vm.top - operand
						} else {
							return fmt.Errorf(fmt.Sprintf("expected at least %v args and got %v", callable.Function.Arity, operand))
						}
					case GFunction:
						runningFiberPointer := globalState.currentFiber
						if result, err := callable.Value(vm.stack[vm.top-operand : vm.top]...); err == nil {
							if runningFiberPointer == globalState.currentFiber {
								vm.stack[vm.top-operand-1] = result
								vm.top -= operand
							} else {
								runningFiberPointer.frame.ret = runningFiberPointer.top - operand - 1
								runningFiberPointer.top = runningFiberPointer.frame.ret + 1
							}
						} else {
							return err
						}
					case Struct:
						if operand <= Bytecode(len(callable.Public)+len(callable.Private))*2 && operand%2 == 0 {
							newInstance := Instance{Id: globalInstanceUniqueID, Struct: &callable, Public: make(Namespace), Private: make(Namespace)}
							globalInstanceUniqueID++
							for k, v := range callable.Public {
								newInstance.Public[k] = v
							}
							for k, v := range callable.Private {
								newInstance.Private[k] = v
							}
							everythingOk := true
							var err error
						innerLoop1:
							for i := vm.top - operand; i < vm.top; i++ {
								if propertyName, ok := vm.stack[i].(*String); ok {
									i++
									if _, exists := callable.Public[propertyName.Value]; exists {
										newInstance.Public[propertyName.Value] = vm.stack[i]
									} else if _, exists := callable.Private[propertyName.Value]; exists {
										newInstance.Private[propertyName.Value] = vm.stack[i]
									} else {
										err = IsNotMethodProperty(propertyName.Value, callable.Name)
										everythingOk = false
										globalInstanceUniqueID--
										break innerLoop1
									}
								} else {
									err = IsNotMethodProperty(vm.stack[i].Description(), callable.Name)
									everythingOk = false
									globalInstanceUniqueID--
									break innerLoop1
								}
							}
							if everythingOk {
								vm.stack[vm.top-operand-1] = newInstance
								vm.top -= operand
							} else {
								return err
							}
						} else {
							return fmt.Errorf(fmt.Sprintf("expected %v pairs (property:value) args in type constructor", len(callable.Public)))
						}
					case Instance:
						if vm.frameIndex == stackOverflowLimit {
							return StackOverfloError()
						}
						if method, okMethod := callable.Struct.Methods[operatorCall].(Closure); okMethod {
							if method.Function.Vararg && method.Function.Arity-1 <= operand {
								if method.Function == vm.frame.closure.Function {
									nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
									if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
										elements := make([]Value, 0, operand-method.Function.Arity+1)
										for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
											elements = append(elements, vm.stack[i])
										}
										vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
										zeroPos := vm.frame.fp + 1
										vm.top = vm.top - operand + method.Function.Arity - 1
										for i := vm.top - method.Function.Arity + 1; i < vm.top; i++ {
											vm.stack[zeroPos] = vm.stack[i]
											zeroPos++
										}
										vm.stack[zeroPos] = vm.stack[vm.top]
										zeroPos++
										vm.top = zeroPos
										vm.frame.pc = 0
										continue
									}
								}
								//===============================
								// Caller Frame
								//===============================
								elements := make([]Value, 0, operand-method.Function.Arity+1)
								for i := vm.top - operand + method.Function.Arity - 1; i < vm.top; i++ {
									elements = append(elements, vm.stack[i])
								}
								vm.stack[vm.top-operand+method.Function.Arity-1] = &List{Elements: elements}
								vm.frame.ret = vm.top - operand - 1
								vm.top = vm.frame.ret + method.Function.Arity + 1
								//===============================
								// Callee Frame
								//===============================
								vm.frameIndex++
								vm.frame = &vm.stackFrame[vm.frameIndex]
								vm.frame.closure = method
								vm.frame.pc = 0
								vm.frame.fp = vm.top - method.Function.Arity - 1
							} else if method.Function.Arity-1 == operand {
								if method.Function == vm.frame.closure.Function {
									nextOpcode := vm.frame.closure.Function.Code[vm.frame.pc] & opcodeMask
									if nextOpcode == OPReturn || (nextOpcode == OPPop && vm.frame.closure.Function.Code[vm.frame.pc+1]&opcodeMask == OPNil && vm.frame.closure.Function.Code[vm.frame.pc+2]&opcodeMask == OPReturn) {
										zeroPos := vm.frame.fp + 1
										for i := vm.top - operand; i < vm.top; i++ {
											vm.stack[zeroPos] = vm.stack[i]
											zeroPos++
										}
										vm.top = zeroPos
										vm.frame.pc = 0
										continue
									}
								}
								//===============================
								// Caller Frame
								//===============================
								vm.frame.ret = vm.top - operand - 1
								//===============================
								// Callee Frame
								//===============================
								vm.frameIndex++
								vm.frame = &vm.stackFrame[vm.frameIndex]
								vm.frame.closure = method
								vm.frame.pc = 0
								vm.frame.fp = vm.top - operand - 1
							} else {
								return fmt.Errorf(fmt.Sprintf("expected at %v args and got %v", method.Function.Arity-1, operand))
							}
						} else {
							return IsNotMethodProperty(operatorCall, callable.TypeName())
						}
					default:
						return fmt.Errorf(fmt.Sprintf("calling a no callable value of type %v", callable.TypeName()))
					}
				} else {
					return err
				}
			}
		case OPAppend:
			// |- ListAddress -|- OpCode -|
			vm.top--
			list, _ := vm.stack[vm.frame.fp+operand].(*List)
			list.Elements = append(list.Elements, vm.stack[vm.top])
		case OPPop:
			// |- Count -|- OpCode -|
			vm.top = vm.top - operand
		case OPEnd:
			// |- OpCode -|
			return nil
		}
	}
}
