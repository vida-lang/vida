package stdlib

import (
	"fmt"
	"math"
	"math/cmplx"

	"github.com/vida-lang/vida/vida"
)

// loadMath loads the stdlib for mathematical computations over Floats.
func loadMath() vida.Importable {
	gmodule := vida.GModule{Name: "math", Namespace: vida.Namespace{
		"e":         vida.Float(math.E),
		"pi":        vida.Float(math.Pi),
		"tau":       vida.Float(math.Pi * 2.0),
		"phi":       vida.Float(math.Phi),
		"sqrt2":     vida.Float(math.Sqrt2),
		"sqrtE":     vida.Float(math.SqrtE),
		"sqrtPi":    vida.Float(math.SqrtPi),
		"sqrtPhi":   vida.Float(math.SqrtPhi),
		"ln2":       vida.Float(math.Ln2),
		"log2e":     vida.Float(1 / math.Ln2),
		"ln10":      vida.Float(math.Ln10),
		"log10e":    vida.Float(math.Log10E),
		"inf":       vida.Float(math.Inf(0)),
		"nan":       vida.Float(math.NaN()),
		"isnan":     vida.GFunction{Name: "isnan", Value: gIsNan},
		"acos":      GFunctionFromFloatToFloat("acos", math.Acos),
		"acosh":     GFunctionFromFloatToFloat("acosh", math.Acosh),
		"asin":      GFunctionFromFloatToFloat("asin", math.Asin),
		"asinh":     GFunctionFromFloatToFloat("ashih", math.Asinh),
		"atan":      GFunctionFromFloatToFloat("atan", math.Atan),
		"atan2":     GFunctionFromFloatFloatToFloat("atan2", math.Atan2),
		"atanh":     GFunctionFromFloatToFloat("atanh", math.Atanh),
		"cbrt":      GFunctionFromFloatToFloat("cbrt", math.Cbrt),
		"ceil":      GFunctionFromFloatToFloat("ceil", math.Ceil),
		"cos":       GFunctionFromFloatToFloat("cos", math.Cos),
		"cosh":      GFunctionFromFloatToFloat("cosh", math.Cosh),
		"exp":       GFunctionFromFloatToFloat("exp", math.Exp),
		"exp2":      GFunctionFromFloatToFloat("exp2", math.Exp2),
		"floor":     GFunctionFromFloatToFloat("floor", math.Floor),
		"gamma":     GFunctionFromFloatToFloat("gamma", math.Gamma),
		"hypot":     GFunctionFromFloatFloatToFloat("hypot", math.Hypot),
		"log":       GFunctionFromFloatToFloat("log", math.Log),
		"log10":     GFunctionFromFloatToFloat("log10", math.Log10),
		"log2":      GFunctionFromFloatToFloat("log2", math.Log2),
		"max":       GFunctionFromFloatFloatToFloat("max", math.Max),
		"min":       GFunctionFromFloatFloatToFloat("min", math.Min),
		"pow":       GFunctionFromFloatFloatToFloat("pow", math.Pow),
		"rem":       GFunctionFromFloatFloatToFloat("rem", math.Remainder),
		"round":     GFunctionFromFloatToFloat("round", math.Round),
		"sin":       GFunctionFromFloatToFloat("sin", math.Sin),
		"sinh":      GFunctionFromFloatToFloat("sinh", math.Sinh),
		"sqrt":      GFunctionFromFloatToFloat("sqrt", math.Sqrt),
		"tan":       GFunctionFromFloatToFloat("tan", math.Tan),
		"tanh":      GFunctionFromFloatToFloat("tanh", math.Tanh),
		"trunc":     GFunctionFromFloatToFloat("trunc", math.Trunc),
		"toDegrees": GFunctionFromFloatToFloat("toDegrees", func(f float64) float64 { return math.Mod(f*(180.0/math.Pi), 360.0) }),
		"toRadians": GFunctionFromFloatToFloat("toRadians", func(f float64) float64 { return math.Mod(f*(math.Pi/180.0), math.Pi*2.0) }),
	}}
	return gmodule
}

func gIsNan(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case vida.Float:
			return vida.Bool(math.IsNaN(float64(value))), nil
		case vida.Complex:
			return vida.Bool(cmplx.IsNaN(complex128(value))), nil
		default:
			return vida.False, nil
		}
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}
