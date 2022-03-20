package vida

import (
	"bytes"
	"fmt"
	"os"
	"time"
	"unicode/utf8"
)

const (
	ModuleExtension    = ".vida"
	CompiledExtension  = ".vo"
	AssemblerExtension = ".vi"
	LanguageVersion    = "0.7.0"
	VMEngineVersion    = "0.5.0"
	STDLibVersion      = "0.3.0"
	LangName           = "Vida 🌻"
	LangHeader         = "A general-purpose programming language."
	LangWebSite        = "https://www.vida-lang.org"
)

// LoadModule loads a script file and returns a buffer of bytes.
func LoadModule(moduleName string) (*bytes.Buffer, error) {
	if bin, err := os.ReadFile(moduleName); err == nil && utf8.Valid(bin) {
		return bytes.NewBuffer(bin), nil
	} else if err != nil {
		return nil, fmt.Errorf(fmt.Sprintf("\n\nError reading file '%v'\n%v\n\n\n", moduleName, err))
	} else {
		return nil, fmt.Errorf(fmt.Sprintf("\n\nThe file '%v' does not contain a valid utf-8 sequence of bytes\n\n\n", moduleName))
	}
}

// RunModule runs the given Vida Script
func RunModule(input *bytes.Buffer, script string, stdlib map[string]LibLoader) {
	input.WriteRune(10)
	vm := VM{MainFiber(script, BuildModule(input, script))}
	globalState.vm = &vm
	globalState.stdlib = stdlib
	vm.Run(mainFunctionName)
}

// DebugModule runs the given Vida script in step by step computation fashion.
func DebugModule(input *bytes.Buffer, script string, stdlib map[string]LibLoader) {
	input.WriteRune(10)
	vm := VM{debugFiber(script, BuildModule(input, script))}
	globalState.vm = &vm
	globalState.stdlib = stdlib
	vm.RunDebugger(mainFunctionName)
}

// TimeModule runs the given Vida Script
func TimeModule(input *bytes.Buffer, script string, stdlib map[string]LibLoader) {
	input.WriteRune(10)
	init := time.Now()
	vm := VM{MainFiber(script, BuildModule(input, script))}
	frontendTime := time.Since(init)
	globalState.vm, globalState.stdlib = &vm, stdlib
	init = time.Now()
	vm.Run(mainFunctionName)
	runTime := time.Since(init)
	fmt.Printf("\n\n\n   Time Elapsed\n\n")
	fmt.Printf("   Compiling : %v\n", frontendTime)
	fmt.Printf("   Running   : %v\n\n", runTime)
	fmt.Printf("   Total     : %v\n\n\n", runTime+frontendTime)
}

// PrintCode prints the machine code in human readable fashion.
func PrintCode(input *bytes.Buffer, script string) {
	input.WriteRune(10)
	vm := VM{MainFiber(script, BuildModule(input, script))}
	fmt.Printf("\n _____________________________________\n")
	fmt.Printf("\n\n     Human Readable Machine Code\n")
	fmt.Printf("\n _____________________________________\n")
	fmt.Printf("\n Module : '%v'", script)
	printBytecode(*vm.frame.closure.Function)
}

// loadVida loads the Vida GModule.
func (fiber *Fiber) loadVida() {
	modName := "Vida"
	module := GModule{Name: modName, Namespace: make(Namespace)}
	module.Namespace["version"] = &String{Value: LangName + " version " + LanguageVersion}
	module.Namespace["vmVersion"] = &String{Value: "Vida VMEngine version " + VMEngineVersion}
	module.Namespace["fext"] = &String{Value: "Vida file extension " + ModuleExtension}
	module.Namespace["header"] = &String{Value: LangHeader}
	module.Namespace["printStack"] = GFunction{Name: "printStack", Value: printStack}
	module.Namespace["breakPoint"] = GFunction{Name: "breakPoint", Value: setBreakPoint}
	fiber.module.namespace[modName] = module
}

// print the current fiber call stack.
func printStack(args ...Value) (Value, error) {
	globalState.currentFiber.printStack(globalState.currentFiber.frameIndex)
	return NilValue, nil
}

// Sets up a point in the code when the VM is run in debug mode.
func setBreakPoint(args ...Value) (Value, error) {
	if err := globalState.vm.runStepByStep(globalState.vm.stackFrame[0].closure.Function.Name); err != nil {
		globalState.vmFailure = true
		fiber := globalState.vm.runPendingDeferStatements()
		PrintError(err)
		globalState.vm.Fiber = fiber
		globalState.vm.Fiber.printStack(globalState.vm.Fiber.frameIndex)
	}
	if globalState.vmFailure {
		globalState.vm.printFinalFailureVMState()
	} else {
		globalState.vm.printVMStateBeforeLeavingDebugger()
	}
	os.Exit(0)
	return NilValue, nil
}
