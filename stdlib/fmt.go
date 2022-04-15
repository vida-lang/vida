package stdlib

import (
	"fmt"

	"github.com/vida-lang/vida/vida"
)

func loadFmt() vida.Importable {
	gmodule := vida.GModule{Name: "fmt", Namespace: vida.Namespace{
		"printf":        vida.GFunction{Name: "printf", Value: fmtPrintf},
		"sprintf":       vida.GFunction{Name: "sprintf", Value: fmtSPrintf},
		"sprint":        vida.GFunction{Name: "sprint", Value: fmtSPrint},
		"sprintln":      vida.GFunction{Name: "sprintln", Value: fmtSPrintln},
		"fprintf":       vida.GFunction{Name: "fprintf", Value: fmtFprintf},
		"fprint":        vida.GFunction{Name: "fprint", Value: fmtFprint},
		"fprintln":      vida.GFunction{Name: "fprintln", Value: fmtFprintln},
		"clearTerminal": vida.GFunction{Name: "clearTerminal", Value: fmtClearScreen},
	}}
	return gmodule
}

func fmtPrintf(args ...vida.Value) (vida.Value, error) {
	if len(args) >= 1 {
		if format, ok := args[0].(*vida.String); ok {
			vida.VidaPrintf(format.Value, args[1:]...)
			return vida.NilValue, nil
		}
		return nil, fmt.Errorf("expected a format string as first argument and got %v", args[0])
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

func fmtSPrintf(args ...vida.Value) (vida.Value, error) {
	if len(args) >= 1 {
		if format, ok := args[0].(*vida.String); ok {
			return &vida.String{Value: vida.VidaSprintf(format.Value, args[1:]...)}, nil
		}
		return nil, fmt.Errorf("expected a format string as first argument and got %v", args[0])
	}
	return nil, fmt.Errorf("expected at least %v argument and got %v", 1, len(args))
}

func fmtSPrint(args ...vida.Value) (vida.Value, error) {
	return &vida.String{Value: vida.VidaSprint(args...)}, nil
}

func fmtSPrintln(args ...vida.Value) (vida.Value, error) {
	return &vida.String{Value: vida.VidaSprintln(args...)}, nil
}

func fmtFprintf(args ...vida.Value) (vida.Value, error) {
	if len(args) >= 2 {
		switch file := args[0].(type) {
		case *File:
			if format, ok := args[1].(*vida.String); ok {
				if n, e := vida.VidaFprintf(file.Value, format.Value, args[2:]...); e == nil {
					return vida.Int(n), nil
				} else {
					return nil, e
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("String", args[1].TypeName())
		case *BytesBuffer:
			if format, ok := args[1].(*vida.String); ok {
				if n, e := vida.VidaFprintf(&file.Buffer, format.Value, args[2:]...); e == nil {
					return vida.Int(n), nil
				} else {
					return nil, e
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("String", args[1].TypeName())
		case *StringBuilder:
			if format, ok := args[1].(*vida.String); ok {
				if n, e := vida.VidaFprintf(&file.Builder, format.Value, args[2:]...); e == nil {
					return vida.Int(n), nil
				} else {
					return nil, e
				}
			}
			return nil, vida.ExpectedTypeAndGotOtherType("String", args[1].TypeName())
		default:
			return nil, vida.ExpectedTypeAndGotOtherType("File/BytesBuffer/StringBuilder", args[0].TypeName())
		}
	}
	return nil, vida.VarArgArityError(2, vida.UInt32(len(args)))
}

func fmtFprint(args ...vida.Value) (vida.Value, error) {
	if len(args) >= 2 {
		switch file := args[0].(type) {
		case *File:
			if n, e := vida.VidaFprint(file.Value, args[1:]...); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		case *StringBuilder:
			if n, e := vida.VidaFprint(&file.Builder, args[1:]...); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		case *BytesBuffer:
			if n, e := vida.VidaFprint(&file.Buffer, args[1:]...); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		default:
			return nil, vida.ExpectedTypeAndGotOtherType("File/BytesBuffer/StringBuilder", args[0].TypeName())
		}
	}
	return nil, vida.VarArgArityError(2, vida.UInt32(len(args)))
}

func fmtFprintln(args ...vida.Value) (vida.Value, error) {
	if len(args) >= 2 {
		switch file := args[0].(type) {
		case *File:
			if n, e := vida.VidaFprintln(file.Value, args[1:]...); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		case *StringBuilder:
			if n, e := vida.VidaFprintln(&file.Builder, args[1:]...); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		case *BytesBuffer:
			if n, e := vida.VidaFprintln(&file.Buffer, args[1:]...); e == nil {
				return vida.Int(n), nil
			} else {
				return nil, e
			}
		default:
			return nil, vida.ExpectedTypeAndGotOtherType("File/BytesBuffer/StringBuilder", args[0].TypeName())
		}
	}
	return nil, vida.VarArgArityError(2, vida.UInt32(len(args)))
}

func fmtClearScreen(args ...vida.Value) (vida.Value, error) {
	vida.VidaPrintf("\u001B[H")
	vida.VidaPrintf("\u001B[2J")
	return vida.NilValue, nil
}
