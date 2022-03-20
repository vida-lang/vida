package vida

import (
	"fmt"
	"path/filepath"
	"sync"
	"unsafe"
)

// fiberPool
var fiberPool = sync.Pool{
	New: func() interface{} {
		fiber, _ := FiberNew(Closure{})
		return fiber.(*Fiber)
	},
}

// Fiber states.
const (
	fiberReady byte = iota
	fiberRunning
	fiberSuspended
	fiberWaiting
	fiberTerminated
)

// Description of the fiber states.
var fiberStateDescription = []string{
	fiberReady:      "ready",      // This is the default state when a fiber is created.
	fiberRunning:    "running",    // This is the state when a fiber is running in the VM.
	fiberSuspended:  "suspended",  // This is te state when a fiber after being suspended.
	fiberWaiting:    "waiting",    // This the state when a fiber runs another fiber.
	fiberTerminated: "terminated", // This is the state when a fiber is done.
}

// Fiber is the basic unit of execution and model a computational sequential process.
type Fiber struct {
	stackFrame  []Frame  // The particular state of a function stack call.
	stack       []Value  // The underlying stack for every frame.
	frame       *Frame   // A pointer to one of the frames in the stackFrame. It is the current frame.
	module      *VModule // A pointer to the module containing the function currently running in the fiber.
	parentFiber *Fiber   // The parent or caller Fiber in a graph of Fibers
	frameIndex  int      // The index of the current frame.
	state       byte     // Fiber state
	top         Bytecode // Top of the stack a.k.a. stack pointer.
}

// Interface Value
func (fiber Fiber) TypeName() string {
	return "Fiber"
}

func (fiber Fiber) Description() string {
	return fmt.Sprintf("Fiber(module:%v fn:%v at:%p state:%v)", filepath.Base(fiber.module.path), fiber.stackFrame[0].closure.Function.Name, &fiber.stackFrame[0].closure, fiberStateDescription[fiber.state])
}

func (fiber *Fiber) Equals(other Value) bool {
	if value, ok := other.(*Fiber); ok {
		return unsafe.Pointer(fiber) == unsafe.Pointer(value)
	}
	return false
}

func (fiber *Fiber) BinaryOp(op byte, rhs Value) (Value, error) {
	return nil, OperatorNotDefined(op, fiber)
}

func (fiber *Fiber) PrefixOp(op byte) (Value, error) {
	return nil, OperatorNotDefined(op, fiber)
}

func (fiber *Fiber) IsIterable() bool {
	return false
}

func (fiber *Fiber) MakeIterator() Iterator {
	return nil
}

func (fiber *Fiber) IsHashable() bool {
	return false
}

func (fiber *Fiber) MakeHashKey() HashKey {
	return HashKey{}
}

func (fiber *Fiber) IsValueSemantics() bool {
	return false
}

func (fiber *Fiber) HasMethods() bool {
	return true
}

func (fiber *Fiber) GetMethod(name string) (Value, bool, error) {
	if method, ok := FiberInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, MethodNotDefined(name, fiber)
}

func (fiber *Fiber) Clone() Value {
	return fiber
}

// mainFiber creates the main sequence of execution. It creates the shared global state and loads the corelib as well.
func MainFiber(mainScriptName string, module *VModule) *Fiber {
	globalState = Global{modules: make(map[string]*VModule), mainModuleName: mainScriptName}
	globalState.modules[mainScriptName] = module
	module.global = &globalState
	cycleDetector[mainScriptName] = NilValue
	mainFiber := &Fiber{stackFrame: make([]Frame, frameStackSize), stack: make([]Value, stackSize)}
	mainFiber.frame = &mainFiber.stackFrame[mainFiber.frameIndex]
	mainFiber.frame.closure = Closure{Function: module.mainFunction.Function}
	mainFiber.module = module
	mainFiber.state = fiberRunning
	globalState.mainFiber = mainFiber
	globalState.currentFiber = mainFiber
	mainFiber.loadAll()
	return mainFiber
}

// debugFiber creates a new sequence of execution, but with just 32 slots in its stack for debbugging purposes.
func debugFiber(mainScriptName string, module *VModule) *Fiber {
	globalState = Global{modules: make(map[string]*VModule), mainModuleName: mainScriptName}
	globalState.modules[mainScriptName] = module
	module.global = &globalState
	cycleDetector[mainScriptName] = NilValue
	debugFiber := &Fiber{stackFrame: make([]Frame, frameStackSize), stack: make([]Value, debugStackSize)}
	debugFiber.frame = &debugFiber.stackFrame[debugFiber.frameIndex]
	debugFiber.frame.closure = Closure{Function: module.mainFunction.Function}
	debugFiber.module = module
	debugFiber.state = fiberRunning
	globalState.mainFiber = debugFiber
	globalState.currentFiber = debugFiber
	debugFiber.loadAll()
	return debugFiber
}

// stackTrace prints the stack frames to show the source of an error.
func (fiber *Fiber) printStack(fromFrameIndex int) {
	fmt.Printf("\n\n   Stack\n\n")
	var frame *Frame
	for current := fiber; current != nil; current = current.parentFiber {
		fmt.Printf("   %v\n", current.Description())
		for i := fromFrameIndex; i >= 0; i-- {
			frame = &current.stackFrame[i]
			fmt.Printf("       [%v] %v:%v in function %v\n", i, filepath.Base(frame.closure.Function.ModuleName), frame.closure.Function.Lines[frame.pc-1], frame.closure.Function.Name)
		}
		fmt.Print("\n")
	}
	fmt.Printf("\n")
}

func (fiber *Fiber) runPendingDeferStatements() *Fiber {
	for current := fiber; current != nil; current = current.parentFiber {
		globalState.currentFiber = current
		globalState.vm.Fiber = current
		for i := current.frameIndex; i >= 0; i-- {
			globalState.vm.frame = &current.stackFrame[i]
			if globalState.vm.frame.hasDefer {
				globalState.vm.performDefer()
			}
		}
	}
	return fiber
}

func (fiber *Fiber) loadAll() {
	fiber.loadPrelude()
	fiber.loadFiber()
	fiber.loadRecord()
	fiber.loadList()
	fiber.loadIterator()
	fiber.loadVida()
}

// loadFiber loads the Fiber GModule.
func (fiber *Fiber) loadFiber() {
	modName := "Fiber"
	module := GModule{Name: modName, Namespace: make(Namespace)}
	module.Namespace["new"] = GFunction{Name: "new", Value: FiberNew}
	module.Namespace["wrap"] = GFunction{Name: "wrap", Value: FiberWrap}
	module.Namespace["running"] = GFunction{Name: "running", Value: FiberCurrent}
	module.Namespace["suspend"] = GFunction{Name: "suspend", Value: FiberSuspend}
	module.Namespace["run"] = GFunction{Name: "run", Value: FiberRun}
	module.Namespace["recycle"] = GFunction{Name: "recycle", Value: FiberRecycle}
	fiber.module.namespace[modName] = module
}

func (fiber *Fiber) reset(closure Closure) {
	fiber.frameIndex, fiber.frame = 0, &fiber.stackFrame[0]
	fiber.frame.closure, fiber.state = closure, fiberReady
	fiber.frame.fp, fiber.top, fiber.frame.pc = 0, 0, 0
}

// Creates a new Fiber with a function as its unique argument.
func FiberNew(args ...Value) (Value, error) {
	if len(args) == 1 {
		if closure, ok := args[0].(Closure); ok {
			newFiber := &Fiber{stackFrame: make([]Frame, frameStackSize), stack: make([]Value, fiberStackSize)}
			newFiber.frame = &newFiber.stackFrame[newFiber.frameIndex]
			newFiber.frame.closure = closure
			newFiber.module = globalState.mainFiber.module
			return newFiber, nil
		}
		return nil, fmt.Errorf("expected a function as argument")
	}
	return nil, fmt.Errorf("expected 1 argument and got %v", len(args))
}

func FiberWrap(args ...Value) (Value, error) {
	if len(args) == 1 {
		if closure, ok := args[0].(Closure); ok {
			if fib, err := FiberNew(closure); err == nil {
				newFiber, _ := fib.(*Fiber)
				return GFunction{Name: "GFunction Wrapped " + newFiber.Description(), Value: func(args ...Value) (Value, error) {
					args = append(args, nil)
					copy(args[1:], args[0:])
					args[0] = newFiber
					return FiberRun(args...)
				}}, nil
			} else {
				return nil, err
			}
		}
		return nil, fmt.Errorf("expected a function as argument")
	}
	return nil, fmt.Errorf("expected 1 argument and got %v", len(args))
}

// Returns the current running a.k.a. resumed fiber.
func FiberCurrent(args ...Value) (Value, error) {
	if len(args) == 0 {
		return globalState.currentFiber, nil
	}
	return nil, fmt.Errorf("expected 0 arguments and got %v", len(args))
}

// Returns the current running a.k.a. resumed fiber's caller fiber.
func FiberParent(args ...Value) (Value, error) {
	if len(args) == 0 {
		if globalState.currentFiber.parentFiber == nil {
			return NilValue, nil
		}
		return globalState.currentFiber.parentFiber, nil
	}
	return nil, fmt.Errorf("expected 0 arguments and got %v", len(args))
}

// Suspends the fiber in which it is called and returns some value to the caller fiber. A.k.a. fiber yield.
func FiberSuspend(args ...Value) (Value, error) {
	if globalState.currentFiber.parentFiber != nil {
		argCount := Bytecode(len(args))
		switch globalState.currentFiber.state {
		case fiberRunning:
			var data Value
			if argCount == 0 {
				data = NilValue
			} else if argCount == 1 {
				data = globalState.currentFiber.stack[globalState.currentFiber.top-1]
			} else {
				xs := make([]Value, 0, argCount)
				for i := globalState.currentFiber.top - argCount; i < globalState.currentFiber.top; i++ {
					xs = append(xs, globalState.currentFiber.stack[i])
				}
				data = &List{Elements: xs}
			}
			globalState.currentFiber.state = fiberSuspended
			globalState.currentFiber = globalState.currentFiber.parentFiber
			globalState.vm.Fiber = globalState.currentFiber
			globalState.currentFiber.state = fiberRunning
			globalState.currentFiber.stack[globalState.currentFiber.frame.ret] = data
		case fiberSuspended:
			return nil, fmt.Errorf("cannot suspend a suspended fiber")
		case fiberReady:
			return nil, fmt.Errorf("cannot suspend a ready fiber")
		case fiberWaiting:
			return nil, fmt.Errorf("cannot suspend a waiting fiber")
		case fiberTerminated:
			return nil, fmt.Errorf("cannot suspend a terminated fiber")
		}
		return NilValue, nil
	}
	return nil, fmt.Errorf("suspending a function must be done from a fiber")
}

// Activates or re-activates the fiber given as its first argument.
func FiberRun(args ...Value) (Value, error) {
	if len(args) >= 1 {
		if fiber, ok := args[0].(*Fiber); ok {
			length := len(args)
			argCount := Bytecode(length - 1)
			switch fiber.state {
			case fiberReady:
				if fiber.stackFrame[fiber.frameIndex].closure.Function.Vararg {
					if fiber.stackFrame[fiber.frameIndex].closure.Function.Arity == 0 {
						elements := make([]Value, 0, argCount)
						for i := 1; i < length; i++ {
							elements = append(elements, args[i])
						}
						fiber.stack[fiber.top] = &List{Elements: elements}
						fiber.top++
						fiber.parentFiber = globalState.currentFiber
						fiber.state = fiberRunning
						fiber.parentFiber.state = fiberWaiting
						globalState.currentFiber = fiber
						globalState.vm.Fiber = fiber
					} else if fiber.stackFrame[fiber.frameIndex].closure.Function.Arity <= argCount {
						for i := Bytecode(1); i < fiber.stackFrame[fiber.frameIndex].closure.Function.Arity+1; i++ {
							fiber.stack[fiber.top] = args[i]
							fiber.top++
						}
						elements := make([]Value, 0, argCount-fiber.stackFrame[fiber.frameIndex].closure.Function.Arity)
						for i := fiber.stackFrame[fiber.frameIndex].closure.Function.Arity + 1; i < Bytecode(length); i++ {
							elements = append(elements, args[i])
						}
						fiber.stack[fiber.top] = &List{Elements: elements}
						fiber.top++
						fiber.parentFiber = globalState.currentFiber
						fiber.state = fiberRunning
						fiber.parentFiber.state = fiberWaiting
						globalState.currentFiber = fiber
						globalState.vm.Fiber = fiber
					} else {
						return nil, fmt.Errorf("expected at least %v args and got %v", fiber.stackFrame[0].closure.Function.Arity, argCount)
					}
				} else {
					if fiber.stackFrame[fiber.frameIndex].closure.Function.Arity == argCount {
						for i := 1; i < length; i++ {
							fiber.stack[fiber.top] = args[i]
							fiber.top++
						}
						fiber.parentFiber = globalState.currentFiber
						fiber.state = fiberRunning
						fiber.parentFiber.state = fiberWaiting
						globalState.currentFiber = fiber
						globalState.vm.Fiber = fiber
					} else {
						return nil, fmt.Errorf("expected %v args and got %v", fiber.stackFrame[0].closure.Function.Arity, argCount)
					}
				}
			case fiberSuspended:
				if argCount == 0 {
					fiber.stack[fiber.frame.ret] = NilValue
				} else if argCount == 1 {
					fiber.stack[fiber.frame.ret] = args[1]
				} else {
					elements := make([]Value, 0, argCount)
					for i := 1; i < length; i++ {
						elements = append(elements, args[i])
					}
					fiber.stack[fiber.frame.ret] = &List{Elements: elements}
				}
				fiber.parentFiber = globalState.currentFiber
				fiber.state = fiberRunning
				globalState.currentFiber.state = fiberWaiting
				globalState.currentFiber = fiber
				globalState.vm.Fiber = fiber
			case fiberRunning:
				return nil, fmt.Errorf("cannot run a running fiber")
			case fiberWaiting:
				return nil, fmt.Errorf("cannot run a waiting fiber")
			case fiberTerminated:
				return nil, fmt.Errorf("cannot run a terminated fiber")
			}
		} else {
			return nil, fmt.Errorf("the first argument must be a fiber")
		}
	} else {
		return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
	}
	return NilValue, nil
}

// FiberInterface is the set of methods for fiber instances.
var FiberInterface = Namespace{
	"isAlive":   GFunction{Name: "isAlive", Value: FiberIsAlive},
	"state":     GFunction{Name: "state", Value: FiberState},
	"run":       GFunction{Name: "run", Value: FiberRun},
	"terminate": GFunction{Name: "terminate", Value: FiberTerminate},
}

// Checks if a fiber is alive or terminated.
func FiberIsAlive(args ...Value) (Value, error) {
	if len(args) == 1 {
		return Bool(args[0].(*Fiber).state != fiberTerminated), nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

// Prints the stack trace for a given fiber.
func FiberPrintStack(args ...Value) (Value, error) {
	if len(args) == 1 {
		fiber := args[0].(*Fiber)
		fmt.Printf("\n\n")
		fiber.printStack(fiber.frameIndex)
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func FiberState(args ...Value) (Value, error) {
	if len(args) == 1 {
		return &String{Value: fiberStateDescription[args[0].(*Fiber).state]}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func FiberTerminate(args ...Value) (Value, error) {
	if len(args) == 1 {
		args[0].(*Fiber).state = fiberTerminated
		return NilValue, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func FiberRecycle(args ...Value) (Value, error) {
	if len(args) == 2 {
		if fiber, ok := args[0].(*Fiber); ok {
			if closure, ok := args[1].(Closure); ok {
				fiber.reset(closure)
				return fiber, nil
			}
			return nil, fmt.Errorf("the second argument must be a function")
		}
		return nil, fmt.Errorf("the first argument must be a fiber")
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}
