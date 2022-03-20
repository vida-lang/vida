package stdlib

import (
	"fmt"
	"os/exec"
	"runtime"

	"github.com/vida-lang/vida-lang/vida"
)

// Cmd is a wrapper for Golang *exec.Cmd.
type Cmd struct {
	Value *exec.Cmd
}

// Interface vida.Value
func (cmd Cmd) TypeName() string {
	return "Cmd"
}

func (cmd Cmd) Description() string {
	return cmd.Value.String()
}

func (cmd Cmd) Equals(other vida.Value) bool {
	if value, ok := other.(Cmd); ok {
		return cmd.Value == value.Value
	}
	return false
}

func (cmd Cmd) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, cmd)
}

func (cmd Cmd) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, cmd)
}

func (cmd Cmd) IsIterable() bool {
	return false
}

func (cmd Cmd) MakeIterator() vida.Iterator {
	return nil
}

func (cmd Cmd) IsHashable() bool {
	return false
}

func (cmd Cmd) MakeHashKey() vida.HashKey {
	return vida.HashKey{}
}

func (cmd Cmd) IsValueSemantics() bool {
	return false
}

func (cmd Cmd) HasMethods() bool {
	return true
}

func (cmd Cmd) GetMethod(name string) (vida.Value, bool, error) {
	if method, ok := CMDInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, vida.MethodNotDefined(name, cmd)
}

func (cmd Cmd) Clone() vida.Value {
	return Cmd{Value: cmd.Value}
}

var CMDInterface vida.Namespace

func loadCMD() vida.Importable {
	CMDInterface = vida.Namespace{
		"run":            vida.GFunction{Name: "run", Value: cmdRun},
		"output":         vida.GFunction{Name: "output", Value: cmdOutput},
		"combinedOutput": vida.GFunction{Name: "combinedOutput", Value: cmdCombinedOutput},
		"start":          vida.GFunction{Name: "start", Value: cmdStart},
		"wait":           vida.GFunction{Name: "wait", Value: cmdWait},
	}
	gmodule := vida.GModule{Name: "cmd", Namespace: vida.Namespace{
		"new":      vida.GFunction{Name: "new", Value: cmdNew},
		"os":       &vida.String{Value: runtime.GOOS},
		"lookPath": vida.GFunction{Name: "lookPath", Value: cmdLookPath},
	}}
	return gmodule
}

func cmdNew(args ...vida.Value) (vida.Value, error) {
	length := len(args)
	if length > 0 {
		switch length {
		case 1:
			if command, ok := args[0].(*vida.String); ok {
				return vida.Result{Value: Cmd{Value: exec.Command(command.Value)}, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: "arguments must be of type String"}}, nil
			}
		default:
			slice := make([]string, length)
			for i, v := range args {
				if str, ok := v.(*vida.String); ok {
					slice[i] = str.Value
				} else {
					return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: "arguments must be of type String"}}, nil
				}
			}
			return vida.Result{Value: Cmd{Value: exec.Command(slice[0], slice[1:]...)}, Error: vida.NilValue}, nil
		}

	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

func cmdLookPath(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if file, ok := args[0].(*vida.String); ok {
			if path, err := exec.LookPath(file.Value); err == nil {
				return vida.Result{Value: &vida.String{Value: path}, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		} else {
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: "argument must be a String"}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func cmdRun(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		cmd, _ := args[0].(Cmd)
		if err := cmd.Value.Run(); err == nil {
			return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
		} else {
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func cmdOutput(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		cmd, _ := args[0].(Cmd)
		if out, err := cmd.Value.Output(); err == nil {
			return vida.Result{Value: &vida.Bytes{Value: out}, Error: vida.NilValue}, nil
		} else {
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func cmdCombinedOutput(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		cmd, _ := args[0].(Cmd)
		if out, err := cmd.Value.CombinedOutput(); err == nil {
			return vida.Result{Value: &vida.Bytes{Value: out}, Error: vida.NilValue}, nil
		} else {
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func cmdStart(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		cmd, _ := args[0].(Cmd)
		if err := cmd.Value.Start(); err == nil {
			return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
		} else {
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func cmdWait(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		cmd, _ := args[0].(Cmd)
		if err := cmd.Value.Wait(); err == nil {
			return vida.Result{Value: vida.NilValue, Error: vida.NilValue}, nil
		} else {
			return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}
