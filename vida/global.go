package vida

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
	deferStack []DeferInfo // The stack of defer calls information.
	hasDefer   bool        // A signal to indicate that this frame has pending defer calls.
}

// DeferInfo is the structure to keep information about defer calls.
type DeferInfo struct {
	callable       Value   // The callable value to be called in a defer statement.
	object         Value   // This is the instance to be called in case of deferring a method call.
	attribute      string  // This is the attribute in case of method call.
	args           []Value // This is the slice of arguments to pass to the callable.
	typeofCallable byte    // This is a flag to signal de type of callable: Function (0), GFunction (1), Struct (2), InvokeMethod(3), Instance (4)
	line           UInt32  // This is the line for error signaling purposes.
}
