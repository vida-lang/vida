package stdlib

import (
	"fmt"

	"github.com/vida-lang/vida/vida"
)

// GFunctionFromFloatToFloat wraps a Go function type func(float64)float64
func GFunctionFromFloatToFloat(functionName string, fn func(float64) float64) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 1 {
			if data, ok := args[0].(vida.Float); ok {
				return vida.Float(fn(float64(data))), nil
			}
			return nil, fmt.Errorf("expected argument of type Float")
		}
		return nil, fmt.Errorf("exptected %v arguments and got %v", 1, len(args))
	}}
}

// GFunctionFromFloatToFloat wraps a Go function type func(float64, float64)float64
func GFunctionFromFloatFloatToFloat(functionName string, fn func(float64, float64) float64) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 2 {
			if lhs, ok := args[0].(vida.Float); ok {
				if rhs, ok := args[1].(vida.Float); ok {
					return vida.Float(fn(float64(lhs), float64(rhs))), nil
				}
				return nil, fmt.Errorf("second argument must be a Float")
			}
			return nil, fmt.Errorf("first argument must be a Float")
		}
		return nil, fmt.Errorf("exptected %v arguments and got %v", 2, len(args))
	}}
}

// GFunctionFromVoidToString wraps a Go function type func()string
func GFunctionFromVoidToString(functionName string, fn func() string) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 0 {
			return &vida.String{Value: fn()}, nil
		}
		return nil, fmt.Errorf("exptected %v arguments and got %v", 0, len(args))
	}}
}

// GFunctionFromVoidToInt wraps a Go function type func()int64
func GFunctionFromVoidToInt(functionName string, fn func() int64) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 0 {
			return vida.Int(fn()), nil
		}
		return nil, fmt.Errorf("exptected %v arguments and got %v", 0, len(args))
	}}
}

// GFunctionFromVoidToFloat wraps a Go function type func()float64
func GFunctionFromVoidToFloat(functionName string, fn func() float64) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 0 {
			return vida.Float(fn()), nil
		}
		return nil, fmt.Errorf("exptected %v arguments and got %v", 0, len(args))
	}}
}

// GFunctionFromVoidToUInt wraps a Go function type func()uint64
func GFunctionFromVoidToUInt(functionName string, fn func() uint64) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 0 {
			return vida.UInt(fn()), nil
		}
		return nil, fmt.Errorf("exptected %v arguments and got %v", 0, len(args))
	}}
}

// GFunctionFromIntToInt wraps a Go function type func(int64)int64
func GFunctionFromIntToInt(functionName string, fn func(int64) int64) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 1 {
			if value, ok := args[0].(vida.Int); ok {
				return vida.Int(fn(int64(value))), nil
			} else {
				return nil, fmt.Errorf("expected an Int as argument")
			}
		}
		return nil, fmt.Errorf("exptected %v argument and got %v", 1, len(args))
	}}
}

// GFunctionFromStringToBool wraps a Go function type func(string)bool
func GFunctionFromStringToBool(functionName string, fn func(string) bool) vida.GFunction {
	return vida.GFunction{Name: functionName, Value: func(args ...vida.Value) (vida.Value, error) {
		if len(args) == 1 {
			if data, ok := args[0].(*vida.String); ok {
				return vida.Bool(fn(data.Value)), nil
			}
			return nil, fmt.Errorf("expected a String as argument")
		}
		return nil, fmt.Errorf("exptected %v arguments and got %v", 1, len(args))
	}}
}
