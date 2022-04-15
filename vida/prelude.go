package vida

import (
	"bufio"
	"bytes"
	"fmt"
	"io/ioutil"
	"math/big"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"unicode/utf8"
)

// Loads the core functionality of the language.
func (fiber *Fiber) loadPrelude() {
	fiber.module.namespace["print"] = GFunction{Name: "print", Value: gPrint}
	fiber.module.namespace["input"] = GFunction{Name: "input", Value: gInput}
	fiber.module.namespace["panic"] = GFunction{Name: "panic", Value: gAbort}
	fiber.module.namespace["exit"] = GFunction{Name: "exit", Value: gExit}
	fiber.module.namespace["typeof"] = GFunction{Name: "typeof", Value: gTypeof}
	fiber.module.namespace["import"] = GFunction{Name: "import", Value: gImportModule}
	fiber.module.namespace["importRemote"] = GFunction{Name: "importRemote", Value: gImportRemoteModule}
	fiber.module.namespace["argv"] = GFunction{Name: "args", Value: gArgs}
	fiber.module.namespace["real"] = GFunction{Name: "real", Value: gGetReal}
	fiber.module.namespace["imag"] = GFunction{Name: "imag", Value: gGetImag}
	fiber.module.namespace["toSameTypeOf"] = GFunction{Name: "toSameTypeOf", Value: gToSameTypeOf}
	fiber.module.namespace["clone"] = GFunction{Name: "clone", Value: gClone}
	fiber.module.namespace["assert"] = GFunction{Name: "assert", Value: gAssert}
	// Data Type Constructors
	fiber.module.namespace["Set"] = GFunction{Name: "Set", Value: gSetConstructor}
	fiber.module.namespace["String"] = GFunction{Name: "String", Value: gToString}
	fiber.module.namespace["Rune"] = GFunction{Name: "Rune", Value: gRune}
	fiber.module.namespace["Int"] = GFunction{Name: "Int", Value: gToInt}
	fiber.module.namespace["UInt"] = GFunction{Name: "UInt", Value: gToUInt}
	fiber.module.namespace["Byte"] = GFunction{Name: "Byte", Value: gToByte}
	fiber.module.namespace["Complex"] = GFunction{Name: "Complex", Value: gToComplex}
	fiber.module.namespace["Float"] = GFunction{Name: "Float", Value: gToFloat}
	fiber.module.namespace["BInt"] = GFunction{Name: "BInt", Value: gToBigInt}
	fiber.module.namespace["Rat"] = GFunction{Name: "Rat", Value: gToRat}
	fiber.module.namespace["Bool"] = GFunction{Name: "Bool", Value: gToBool}
	fiber.module.namespace["Bytes"] = GFunction{Name: "Bytes", Value: gNewBytes}
	fiber.module.namespace["Nil"] = GFunction{Name: "Nil", Value: gToNil}
	fiber.module.namespace["Map"] = GFunction{Name: "Map", Value: gToMap}
	fiber.module.namespace["Struct"] = GFunction{Name: "Struct", Value: gToStruct}
	fiber.module.namespace["Function"] = GFunction{Name: "Function", Value: gNotImplemented}
	fiber.module.namespace["GFunction"] = GFunction{Name: "GFunction", Value: gNotImplemented}
	fiber.module.namespace["Result"] = GFunction{Name: "Result", Value: gCreateResult}
	fiber.module.namespace["Ok"] = GFunction{Name: "Ok", Value: gOk}
	fiber.module.namespace["Error"] = GFunction{Name: "Error", Value: gError}
	fiber.module.namespace["Pair"] = GFunction{Name: "Pair", Value: gNotImplemented}
	fiber.module.namespace["Tuple"] = GFunction{Name: "Tuple", Value: gNotImplemented}
}

// Simple print functionality.
func gPrint(args ...Value) (Value, error) {
	VidaFprintln(os.Stdout, args...)
	return NilValue, nil
}

// Reads from the input stream and returns a string with data.
func gInput(args ...Value) (Value, error) {
	length := len(args)
	if length <= 1 {
		if length == 1 {
			fmt.Print(args[0].Description())
		}
		scanner := bufio.NewScanner(os.Stdin)
		for scanner.Scan() {
			return &String{Value: scanner.Text()}, nil
		}
	}
	return nil, fmt.Errorf("expected 0 or 1 argument and got %v", len(args))
}

// Unconditionally terminates the execution of a module.
func gAbort(args ...Value) (Value, error) {
	fiber := globalState.vm.runPendingDeferStatements()
	globalState.currentFiber = fiber
	globalState.vm.Fiber = fiber
	globalState.vm.printStack(globalState.currentFiber.frameIndex)
	if len(args) == 0 {
		fmt.Printf("   Aborting fiber...\n   %v\n   Message: %v\n\n\n", fiber.Description(), NilValue.Description())
	} else if len(args) >= 1 {
		fmt.Printf("   Aborting fiber...\n   %v\n   Message: %v\n\n\n", fiber.Description(), args[0].Description())
	}
	os.Exit(0)
	return NilValue, nil
}

func gExit(args ...Value) (Value, error) {
	os.Exit(0)
	return NilValue, nil
}

// Returns the type of a Value.
func gTypeof(args ...Value) (Value, error) {
	if len(args) == 1 {
		return &String{Value: args[0].TypeName()}, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns a string representation of a Value.
func gToString(args ...Value) (Value, error) {
	if len(args) == 0 {
		return &String{Value: ""}, nil
	}
	if len(args) == 1 {
		return &String{Value: args[0].Description()}, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

// Returns the Unicode codepoint of a given argument.
func gRune(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case *String:
			if utf8.RuneCountInString(value.Value) == 1 {
				r, _ := utf8.DecodeLastRuneInString(value.Value)
				if utf8.ValidRune(r) {
					return Rune(r), nil
				}
				return nil, fmt.Errorf("rune '%v' is not valid", r)
			} else if utf8.RuneCountInString(value.Value) == 2 {
				r, _ := utf8.DecodeRuneInString(value.Value)
				if utf8.ValidRune(r) {
					return Rune(r), nil
				}
				return nil, fmt.Errorf("rune '%v' is not valid", r)
			}
			return nil, fmt.Errorf("expected a string with one unicode character as argument")
		case Int:
			r := rune(value)
			if utf8.ValidRune(r) {
				return Rune(r), nil
			}
			return nil, fmt.Errorf("rune '%v' is not valid", r)
		case UInt:
			r := rune(value)
			if utf8.ValidRune(r) {
				return Rune(r), nil
			}
			return nil, fmt.Errorf("rune '%v' is not valid", r)
		case Byte:
			r := rune(value)
			if utf8.ValidRune(r) {
				return Rune(r), nil
			}
			return nil, fmt.Errorf("rune '%v' is not valid", r)
		default:
			return nil, fmt.Errorf("expected a string or an integer type as argument")
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToInt(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return value, nil
		case UInt:
			return Int(value), nil
		case Float:
			return Int(value), nil
		case Byte:
			return Int(value), nil
		case *BInt:
			if value.Value.IsInt64() {
				return Int(value.Value.Int64()), nil
			} else if value.Value.IsUint64() {
				return Int(value.Value.Uint64()), nil
			}
			return nil, fmt.Errorf("big integer too large to fit in a type Int")
		case *Rational:
			if f, exact := value.Value.Float64(); exact {
				return Int(f), nil
			}
			return nil, fmt.Errorf("not possible to convert Rational to Int")
		case Nil:
			return Int(0), nil
		case Bool:
			if value {
				return Int(1), nil
			}
			return Int(0), nil
		case Rune:
			return Int(value), nil
		case *String:
			if v, err := strconv.ParseInt(value.Value, 0, 64); err == nil {
				return Int(v), nil
			} else {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to Int", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToUInt(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return UInt(value), nil
		case UInt:
			return value, nil
		case Float:
			return UInt(value), nil
		case Byte:
			return UInt(value), nil
		case *BInt:
			if value.Value.IsInt64() {
				return UInt(value.Value.Int64()), nil
			} else if value.Value.IsUint64() {
				return UInt(value.Value.Uint64()), nil
			}
			return nil, fmt.Errorf("big integer too large to fit in a type UInt")
		case *Rational:
			if f, exact := value.Value.Float64(); exact {
				return UInt(f), nil
			}
			return nil, fmt.Errorf("not possible to convert Rational to UInt")
		case Nil:
			return UInt(0), nil
		case Bool:
			if value {
				return UInt(1), nil
			}
			return UInt(0), nil
		case Rune:
			return UInt(value), nil
		case *String:
			if v, err := strconv.ParseUint(value.Value, 0, 64); err == nil {
				return UInt(v), nil
			} else {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to UInt", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToByte(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return Byte(value), nil
		case UInt:
			return Byte(value), nil
		case Float:
			return Byte(value), nil
		case Byte:
			return Byte(value), nil
		case *BInt:
			if value.Value.IsInt64() {
				return Byte(value.Value.Int64()), nil
			} else if value.Value.IsUint64() {
				return Byte(value.Value.Uint64()), nil
			}
			return nil, fmt.Errorf("big integer too large to fit in a type Byte")
		case *Rational:
			if f, exact := value.Value.Float64(); exact {
				return Byte(f), nil
			}
			return nil, fmt.Errorf("not possible to convert Rational to Byte")
		case Nil:
			return Byte(0), nil
		case Bool:
			if value {
				return Byte(1), nil
			}
			return Byte(0), nil
		case Rune:
			return Byte(value), nil
		case *String:
			if v, err := strconv.ParseUint(value.Value, 0, 64); err == nil {
				return Byte(v), nil
			} else {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to Byte", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToComplex(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return Complex(complex(Float(value), 0)), nil
		case UInt:
			return Complex(complex(Float(value), 0)), nil
		case Float:
			return Complex(complex(Float(value), 0)), nil
		case Byte:
			return Complex(complex(Float(value), 0)), nil
		case *BInt:
			if value.Value.IsInt64() {
				return Complex(complex(Float(value.Value.Int64()), 0)), nil
			} else if value.Value.IsUint64() {
				return Complex(complex(Float(value.Value.Uint64()), 0)), nil
			}
			return nil, fmt.Errorf("big integer too large to fit in a type Complex")
		case *Rational:
			if f, exact := value.Value.Float64(); exact {
				return Complex(complex(f, 0)), nil
			}
			return nil, fmt.Errorf("not possible to convert Rational to Complex")
		case Nil:
			return Complex(complex(0, 0)), nil
		case Bool:
			if value {
				return Complex(complex(1, 0)), nil
			}
			return Complex(complex(0, 0)), nil
		case Rune:
			return Complex(complex(Float(value), 0)), nil
		case *String:
			if value.Value == "i" || value.Value == "j" {
				return Complex(complex(0, 1)), nil
			}
			if cm, err := strconv.ParseComplex(value.Value, 128); err == nil {
				return Complex(cm), nil
			} else {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to Complex", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToFloat(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return Float(value), nil
		case UInt:
			return Float(value), nil
		case Float:
			return value, nil
		case Byte:
			return Float(value), nil
		case *BInt:
			if value.Value.IsInt64() {
				return Float(value.Value.Int64()), nil
			} else if value.Value.IsUint64() {
				return Float(value.Value.Uint64()), nil
			}
			return nil, fmt.Errorf("big integer too large to fit in a type Float")
		case *Rational:
			if f, exact := value.Value.Float64(); exact {
				return Float(f), nil
			}
			return nil, fmt.Errorf("not possible to convert Rational to Float")
		case Nil:
			return Float(0.0), nil
		case Bool:
			if value {
				return Float(0.0), nil
			}
			return Float(1.0), nil
		case Rune:
			return Float(value), nil
		case *String:
			if f, err := strconv.ParseFloat(value.Value, 64); err == nil {
				return Float(f), nil
			} else {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to Float", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToBigInt(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return &BInt{Value: new(big.Int).SetInt64(int64(value))}, nil
		case UInt:
			return &BInt{Value: new(big.Int).SetUint64(uint64(value))}, nil
		case Float:
			return &BInt{Value: new(big.Int).SetUint64(uint64(value))}, nil
		case Byte:
			return &BInt{Value: new(big.Int).SetInt64(int64(value))}, nil
		case *BInt:
			return &BInt{Value: new(big.Int).Set(value.Value)}, nil
		case *Rational:
			if f, exact := value.Value.Float64(); exact {
				return &BInt{Value: new(big.Int).SetUint64(uint64(f))}, nil
			}
			return nil, fmt.Errorf("not possible to convert Rational to BInt")
		case Nil:
			return &BInt{Value: new(big.Int)}, nil
		case Bool:
			if value {
				return &BInt{Value: new(big.Int).SetInt64(1)}, nil
			}
			return &BInt{Value: new(big.Int)}, nil
		case Rune:
			return &BInt{Value: new(big.Int).SetUint64(uint64(value))}, nil
		case *String:
			if v, ok := new(big.Int).SetString(value.Value, 0); ok {
				return &BInt{Value: v}, nil
			} else {
				return nil, fmt.Errorf("not possible to convert type '%v' to BInt", value.TypeName())
			}
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to BInt", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToRat(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return &Rational{Value: new(big.Rat).SetInt64(int64(value))}, nil
		case UInt:
			return &Rational{Value: new(big.Rat).SetUint64(uint64(value))}, nil
		case Float:
			return &Rational{Value: new(big.Rat).SetFloat64(float64(value))}, nil
		case Byte:
			return &Rational{Value: new(big.Rat).SetInt64(int64(value))}, nil
		case *BInt:
			if rat, ok := new(big.Rat).SetString(value.Value.String()); ok {
				return &Rational{Value: rat}, nil
			}
			return nil, fmt.Errorf("not possible to convert type '%v' to Rational", value.TypeName())
		case *Rational:
			if rat, ok := new(big.Rat).SetString(value.Value.String()); ok {
				return &Rational{Value: rat}, nil
			}
			return nil, fmt.Errorf("not possible to convert type '%v' to Rational", value.TypeName())
		case Nil:
			return &Rational{Value: new(big.Rat)}, nil
		case Bool:
			if value {
				return &Rational{Value: new(big.Rat).SetInt64(1)}, nil
			}
			return &Rational{Value: new(big.Rat)}, nil
		case Rune:
			return &Rational{Value: new(big.Rat).SetUint64(uint64(value))}, nil
		case *String:
			if v, ok := new(big.Rat).SetString(value.Value); ok {
				return &Rational{Value: v}, nil
			} else {
				return nil, fmt.Errorf("not possible to convert type '%v' to Rational", value.TypeName())
			}
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to Rational", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gImportModule(args ...Value) (Value, error) {
	if len(args) == 1 {
		if givenPath, ok := args[0].(*String); ok {
			// Import VModule.
			if strings.HasSuffix(givenPath.Value, ModuleExtension) {
				//abspath := filepath.Join(filepath.Dir(globalState.currentFiber.module.path), givenPath.Value)
				if !filepath.IsAbs(givenPath.Value) {
					givenPath.Value = filepath.Join(filepath.Dir(globalState.currentFiber.module.path), givenPath.Value)
				}
				if _, exists := cycleDetector[givenPath.Value]; exists {
					return nil, fmt.Errorf(fmt.Sprintf("import cycle ocurred when importing the module '%v'", givenPath.Value))
				} else {
					cycleDetector[givenPath.Value] = NilValue
				}
				if moduleImported, alreadyImported := globalState.modules[givenPath.Value]; alreadyImported {
					return moduleImported, nil
				}
				if input, err := LoadModule(givenPath.Value); err == nil {
					input.WriteRune(10)
					module := buildModule(input, givenPath.Value)
					module.global = &globalState
					globalState.modules[givenPath.Value] = module
					//moduleFiber := &Fiber{stackFrame: make([]Frame, frameStackSize), stack: make([]Value, fiberStackSize)}
					moduleFiber := fiberPool.Get().(*Fiber)
					//moduleFiber.frame = &moduleFiber.stackFrame[moduleFiber.frameIndex]
					moduleFiber.reset(Closure{Function: module.mainFunction.Function})
					moduleFiber.parentFiber = globalState.currentFiber
					moduleFiber.state = fiberRunning
					moduleFiber.module = module
					moduleFiber.parentFiber.state = fiberWaiting
					globalState.currentFiber = moduleFiber
					globalState.vm.Fiber = moduleFiber
					//moduleFiber.frame.closure =
					moduleFiber.loadAllBuiltinFunctionality()
					if err := globalState.vm.runInterpreter(moduleFiber.frame.closure.Function.Name); err != nil {
						fiberPool.Put(moduleFiber)
						return NilValue, err
					}
					delete(cycleDetector, givenPath.Value)
					globalState.currentFiber = globalState.currentFiber.parentFiber
					globalState.vm.Fiber = globalState.currentFiber
					globalState.currentFiber.state = fiberRunning
					fiberPool.Put(moduleFiber)
					return module, nil
				} else {
					fmt.Printf("%v", err)
					os.Exit(0)
				}
			} else {
				// Import GModule.
				if gmodule, exists := globalState.stdlib[givenPath.Value]; exists {
					return gmodule().(Value), nil
				} else {
					return nil, fmt.Errorf("the module '%v' has not beed defined in the stdlib", givenPath.Value)
				}
			}
		} else {
			return nil, fmt.Errorf("expected a string as argument")
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gImportRemoteModule(args ...Value) (Value, error) {
	if len(args) == 1 {
		if givenPath, ok := args[0].(*String); ok {
			if strings.HasSuffix(givenPath.Value, ModuleExtension) {
				if response, err := http.Get(givenPath.Value); err == nil {
					defer response.Body.Close()
					if content, contentError := ioutil.ReadAll(response.Body); contentError == nil {
						if utf8.Valid(content) {
							if _, exists := cycleDetector[givenPath.Value]; exists {
								return nil, fmt.Errorf(fmt.Sprintf("import cycle ocurred when importing the module '%v'", givenPath))
							} else {
								cycleDetector[givenPath.Value] = NilValue
							}
							if moduleImported, alreadyImported := globalState.modules[givenPath.Value]; alreadyImported {
								return moduleImported, nil
							}
							input := bytes.NewBuffer(content)
							input.WriteRune(10)
							module := buildModule(input, givenPath.Value)
							module.global = &globalState
							globalState.modules[givenPath.Value] = module
							//moduleFiber := &Fiber{stackFrame: make([]Frame, frameStackSize), stack: make([]Value, fiberStackSize)}
							moduleFiber := fiberPool.Get().(*Fiber)
							//moduleFiber.frame = &moduleFiber.stackFrame[moduleFiber.frameIndex]
							moduleFiber.reset(Closure{Function: module.mainFunction.Function})
							moduleFiber.parentFiber = globalState.currentFiber
							moduleFiber.state = fiberRunning
							moduleFiber.module = module
							moduleFiber.parentFiber.state = fiberWaiting
							globalState.currentFiber = moduleFiber
							globalState.vm.Fiber = moduleFiber
							//moduleFiber.frame.closure = Closure{Function: module.mainFunction.Function}
							moduleFiber.loadAllBuiltinFunctionality()
							if err := globalState.vm.runInterpreter(moduleFiber.frame.closure.Function.Name); err != nil {
								fiberPool.Put(moduleFiber)
								return NilValue, err
							}
							delete(cycleDetector, givenPath.Value)
							globalState.currentFiber = globalState.currentFiber.parentFiber
							globalState.vm.Fiber = globalState.currentFiber
							globalState.currentFiber.state = fiberRunning
							fiberPool.Put(moduleFiber)
							return module, nil
						} else {
							return nil, fmt.Errorf("the resource located at '%v' has no valid utf-8 content", givenPath)
						}
					} else {
						return nil, contentError
					}
				} else {
					return nil, fmt.Errorf("cannot access the resource located at '%v' because %v", givenPath, err)
				}
			} else {
				info := fmt.Sprintf("\n\nThe file '%v' is not a Vida script\n\n\n", givenPath)
				fmt.Printf("%v", fmt.Errorf(info))
				os.Exit(0)
			}
		} else {
			return nil, fmt.Errorf("expected a string as argument")
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gSetConstructor(args ...Value) (Value, error) {
	if len(args) == 1 {
		if list, isList := args[0].(*List); isList {
			set := make(Set)
			for i := 0; i < len(list.Elements); i++ {
				if list.Elements[i].IsHashable() {
					set[list.Elements[i].MakeHashKey()] = list.Elements[i]
				} else {
					return nil, fmt.Errorf("the value '%v' is not hashable. It cannot be a set element", list.Elements[i])
				}
			}
			return set, nil
		} else {
			set := make(Set)
			for i := 0; i < len(args); i++ {
				if args[i].IsHashable() {
					set[args[i].MakeHashKey()] = args[i]
				} else {
					return nil, fmt.Errorf("the value '%v' is not hashable. It cannot be a set element", args[i])
				}
			}
			return set, nil
		}
	}
	set := make(Set)
	for i := 0; i < len(args); i++ {
		if args[i].IsHashable() {
			set[args[i].MakeHashKey()] = args[i]
		} else {
			return nil, fmt.Errorf("the value '%v' is not hashable. It cannot be a set element", args[i])
		}
	}
	return set, nil
}

func gGetReal(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Complex:
			return Float(real(value)), nil
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to the real part of a Complex number", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gGetImag(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Complex:
			return Float(imag(value)), nil
		default:
			return nil, fmt.Errorf("not possible to convert type '%v' to the imaginary part of a Complex number", value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gToSameTypeOf(args ...Value) (Value, error) {
	if len(args) == 2 {
		switch valueType := args[0].(type) {
		case Int:
			return gToInt(args[1])
		case UInt:
			return gToUInt(args[1])
		case Byte:
			return gToByte(args[1])
		case Float:
			return gToFloat(args[1])
		case Complex:
			return gToComplex(args[1])
		case *BInt:
			return gToBigInt(args[1])
		case *Rational:
			return gToRat(args[1])
		case *String:
			return gToString(args[1])
		case Bool:
			return gToBool(args[1])
		case Nil:
			return NilValue, nil
		case Rune:
			return gRune(args[1])
		case IStop:
			return IStopValue, nil
		default:
			return nil, fmt.Errorf("type comversion from '%v' to '%v' not supported", valueType.TypeName(), args[1].TypeName())
		}
	}
	return nil, fmt.Errorf("expected 2 arguments and got %v", len(args))
}

func gToBool(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Int:
			return Bool(value != 0), nil
		case UInt:
			return Bool(value != 0), nil
		case Byte:
			return Bool(value != 0), nil
		case Float:
			return Bool(value != 0), nil
		case Complex:
			return Bool(value != 0), nil
		case *BInt:
			return Bool(value.Value.Int64() != 0), nil
		case Rune:
			return Bool(value != 0), nil
		case *String:
			return Bool(len(value.Value) != 0), nil
		case Nil:
			return False, nil
		case Iterator:
			return Bool(!value.Exhausted()), nil
		case *List:
			return Bool(len(value.Elements) != 0), nil
		case Map:
			return Bool(len(value) != 0), nil
		case Set:
			return Bool(len(value) != 0), nil
		case Record:
			return Bool(len(value.Properties) != 0), nil
		default:
			return False, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gNewBytes(args ...Value) (Value, error) {
	switch len(args) {
	case 0:
		return &Bytes{}, nil
	case 1:
		switch value := args[0].(type) {
		case IntegerNumber:
			return &Bytes{Value: make([]byte, value.ToInt())}, nil
		case *String:
			return &Bytes{Value: append([]byte{}, value.Value...)}, nil
		case *List:
			newBytes := make([]byte, len(value.Elements))
			for i, v := range value.Elements {
				switch v := v.(type) {
				case IntegerNumber:
					newBytes[i] = byte(v.ToInt())
				default:
					return nil, fmt.Errorf("all content of the list must be a numeric integer")
				}
			}
			return &Bytes{Value: newBytes}, nil
		case *Bytes:
			return &Bytes{Value: append([]byte{}, value.Value...)}, nil
		default:
			return nil, fmt.Errorf("argument must be a numeric integer, String, List or Bytes")
		}
	case 2:
		switch value := args[0].(type) {
		case IntegerNumber:
			switch size := args[1].(type) {
			case IntegerNumber:
				newBytes := make([]byte, size.ToInt())
				for i := range newBytes {
					newBytes[i] = byte(value.ToInt())
				}
				return &Bytes{Value: newBytes}, nil
			default:
				return nil, fmt.Errorf("second argument must be a numeric integer")
			}
		default:
			return nil, fmt.Errorf("first argument must be a numeric integer")
		}
	default:
		return nil, fmt.Errorf("expected at most %v arguments and got %v", 2, len(args))
	}
}

func gToNil(args ...Value) (Value, error) {
	return NilValue, nil
}

func gToMap(args ...Value) (Value, error) {
	if len(args) == 0 {
		return new(Map), nil
	}
	if len(args) == 1 {
		switch iterable := args[0].(type) {
		case *List:
			newMap := make(Map)
			for _, v := range iterable.Elements {
				if v.IsHashable() {
					newMap[v.MakeHashKey()] = Pair{key: v, value: NilValue}
				}
			}
			return newMap, nil
		case Set:
			newMap := make(Map)
			for k, v := range iterable {
				newMap[k] = Pair{key: v, value: NilValue}
			}
			return newMap, nil
		case Map:
			newMap := make(Map)
			for k, v := range iterable {
				newMap[k] = v
			}
			return newMap, nil
		case Record:
			newMap := make(Map)
			for k, v := range iterable.Properties {
				newMap[HashKey{Type: "String", ValueDescription: k}] = Pair{key: &String{Value: k}, value: v}
			}
			return newMap, nil
		case *String:
			newMap := make(Map)
			fields := strings.Fields(iterable.Value)
			for _, k := range fields {
				newMap[HashKey{Type: "String", ValueDescription: k}] = Pair{key: &String{Value: k}, value: NilValue}
			}
			return newMap, nil
		default:
			return nil, fmt.Errorf("cannot build a Map from '%v'", iterable.TypeName())
		}
	}
	return nil, fmt.Errorf("unexpected error from Map function")
}

func gToStruct(args ...Value) (Value, error) {
	if len(args) == 2 {
		if structName, ok := args[0].(*String); ok {
			if record, ok := args[1].(Record); ok {
				newStruct := Struct{Id: globalStructUniqueID, Name: structName.Value, Public: make(Namespace), Private: make(Namespace), Methods: make(Namespace)}
				globalStructUniqueID++
				for k, v := range record.Properties {
					switch v := v.(type) {
					case Closure:
						newStruct.Methods[k] = v
					default:
						newStruct.Public[k] = NilValue
					}
				}
				return newStruct, nil
			}
			return nil, fmt.Errorf("expected a Record as second argument")
		}
		return nil, fmt.Errorf("expected a String as first argument")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func gClone(args ...Value) (Value, error) {
	if len(args) == 1 {
		return args[0].Clone(), nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gCreateResult(args ...Value) (Value, error) {
	if len(args) == 2 {
		return Result{
			Value: args[0],
			Error: args[1],
		}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func gOk(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Result{
			Value: args[0],
			Error: NilValue,
		}, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gError(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Result{
			Value: NilValue,
			Error: args[0],
		}, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func gAssert(args ...Value) (Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case Bool:
			if value {
				return True, nil
			} else {
				return False, AssertionFailure("Assertion Failure")
			}
		default:
			return nil, ExpectedTypeAndGotOtherType(True.TypeName(), value.TypeName())
		}
	} else if len(args) == 2 {
		switch value := args[0].(type) {
		case Bool:
			if value {
				return True, nil
			} else {
				return False, AssertionFailure(fmt.Sprintf("Assertion Failure\n   %v", args[1].Description()))
			}
		default:
			return nil, ExpectedTypeAndGotOtherType(True.TypeName(), value.TypeName())
		}
	}
	return nil, fmt.Errorf("expected %v at least argument and got %v", 1, len(args))
}

func gArgs(args ...Value) (Value, error) {
	if len(args) == 0 {
		args := os.Args
		length := len(args)
		if length-1 >= 1 {
			if strings.HasSuffix(args[1], ModuleExtension) {
				xs := make([]Value, length-1)
				for i, v := range args[1:] {
					xs[i] = &String{Value: v}
				}
				return &List{Elements: xs}, nil
			} else {
				xs := make([]Value, length-2)
				for i, v := range args[2:] {
					xs[i] = &String{Value: v}
				}
				return &List{Elements: xs}, nil
			}
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func gNotImplemented(args ...Value) (Value, error) {
	return NilValue, nil
}
