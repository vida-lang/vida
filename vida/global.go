package vida

import (
	"bytes"
	"fmt"
	"unicode/utf8"
)

// Namespace is a type alias for a map between variable names and arbitrary Values.
type Namespace = map[string]Value

// LibLoader is a function when called produces and returns a GModule.
type LibLoader func() Importable

// Global is the unique shared state accesible for every fiber and for every module.
type Global struct {
	modules        map[string]*VModule  // The global environment of modules.
	mainModuleName string               // The file name of the main module.
	stdlib         map[string]LibLoader // Loader functions for the stadlib.
	currentFiber   *Fiber               // A pointer the running fiber.
	mainFiber      *Fiber               // A pointer to the main or root fiber.
	vm             *VM                  // A pointer to the Main global VM.
	vmFailure      bool                 // It indicates wether the vm finished in a failure state.
}

// Frame is a structure to support function calls and preserve its state.
type Frame struct {
	closure    Closure     // The function running in this frame.
	pc         Bytecode    // The register pointing to the next instruction.
	fp         Bytecode    // The register keeping the address of the current frame pointer.
	ret        Bytecode    // The register keeping the return address.
	deferStack []deferInfo // The stack of defer calls information.
	hasDefer   bool        // A signal to indicate that this frame has pending defer calls.
}

// deferInfo is the structure to keep information about defer calls.
type deferInfo struct {
	callable       Value   // The callable value to be called in a defer statement.
	object         Value   // This is the instance to be called in case of deferring a method call.
	attribute      string  // This is the attribute in case of method call.
	args           []Value // This is the slice of arguments to pass to the callable.
	typeofCallable byte    // This is a flag to signal de type of callable: Function (0), GFunction (1), Struct (2), InvokeMethod(3), Instance (4)
	line           UInt32  // This is the line for error signaling purposes.
}

// Repl models an iterative updated computation structure.
type Repl struct {
	replModuleName string
	replFiber      *Fiber
	compiler       *compiler
	vm             *VM
}

func NewRepl(moduleName string, stdlib map[string]LibLoader) *Repl {
	compiler := newReplCompiler(moduleName, new(bytes.Buffer))
	replFiber := replFiber(moduleName, &VModule{path: moduleName, mainFunction: Closure{Function: compiler.getFunctionPointer()}, namespace: make(Namespace)})
	vm := VM{Fiber: replFiber}
	globalState.vm = &vm
	repl := &Repl{
		replModuleName: moduleName,
		replFiber:      replFiber,
		compiler:       compiler,
		vm:             &vm,
	}
	repl.compileCode("let _ = nil", false)
	globalState.vm.runInterpreter(repl.replModuleName)
	return repl
}

func (repl *Repl) addCode(code string) {
	repl.compiler.lexer.input.Reset()
	repl.compiler.lexer.input.WriteString(code)
	repl.compiler.lexer.input.WriteRune(10)
	repl.compiler.lexer.forward()
	forward(repl.compiler)
	forward(repl.compiler)
}

func (repl *Repl) resetFiber() {
	repl.vm.Fiber.frameIndex, repl.vm.Fiber.frame = 0, &repl.vm.Fiber.stackFrame[0]
	repl.vm.Fiber.state, repl.compiler.lexer.line = fiberRunning, 1
	repl.compiler.function.Code = repl.compiler.function.Code[:0]
	repl.vm.Fiber.frame.fp, repl.vm.Fiber.top, repl.vm.Fiber.frame.pc = 0, 0, 0
}

func (repl *Repl) updateIdentifiers() {
	globalState.modules[repl.replModuleName].identifiers = repl.compiler.gID
}

func (repl *Repl) compileCode(code string, printCode bool) {
	repl.addCode(code)
	repl.resetFiber()
	compileCodeForRepl(repl.compiler)
	repl.updateIdentifiers()
	if printCode {
		printBytecode(repl.compiler.function)
	}
}

func (repl *Repl) runCode() error {
	return globalState.vm.runInterpreter(repl.replModuleName)
}

func (repl *Repl) Eval(code string, showCode bool) (err error) {
	if len(code) == 0 {
		return err
	}
	if !utf8.Valid([]byte(code)) {
		err = fmt.Errorf("It is not a valid utf-8 string")
		return err
	}
	repl.compileCode(code, showCode)
	err = repl.runCode()
	return err
}
