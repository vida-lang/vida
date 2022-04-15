package stdlib

import (
	"fmt"
	"math/rand"
	"time"

	"github.com/vida-lang/vida/vida"
)

// loadRandom loads the stdlib for random functionality.
func loadRandom() vida.Importable {
	rand.Seed(time.Now().UnixNano())
	gmodule := vida.GModule{Name: "random", Namespace: vida.Namespace{
		"randInt":   GFunctionFromIntToInt("randInt", randomRandInt),
		"randFloat": GFunctionFromVoidToFloat("randFloat", rand.Float64),
		"expDist":   GFunctionFromVoidToFloat("expDist", rand.ExpFloat64),
		"normDist":  GFunctionFromVoidToFloat("normDist", rand.NormFloat64),
		"randUInt":  GFunctionFromVoidToUInt("randUInt", rand.Uint64),
		"resetSeed": vida.GFunction{Name: "resetSeed", Value: randomResetSeed}}}
	return gmodule
}

func randomRandInt(upperBound int64) int64 {
	if upperBound > 0 {
		return rand.Int63n(upperBound)
	} else if upperBound < 0 {
		return rand.Int63n(-upperBound)
	}
	return 0
}

func randomResetSeed(args ...vida.Value) (vida.Value, error) {
	if len(args) == 0 {
		rand.Seed(time.Now().UnixNano())
		return vida.NilValue, nil
	}
	return nil, fmt.Errorf("exptected %v arguments and got %v", 0, len(args))
}
