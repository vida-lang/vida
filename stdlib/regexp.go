package stdlib

import (
	"fmt"
	"regexp"
	"unsafe"

	"github.com/vida-lang/vida-lang/vida"
)

type Regex struct {
	Value *regexp.Regexp
}

// Interface vida.Value
func (re *Regex) TypeName() string {
	return "Regex"
}

func (re *Regex) Description() string {
	return fmt.Sprintf("Regex(pattern:%v at:%v)", re.Value.String(), unsafe.Pointer(re))
}

func (re *Regex) Equals(other vida.Value) bool {
	if value, ok := other.(*Regex); ok {
		return unsafe.Pointer(re) == unsafe.Pointer(value)
	}
	return false
}

func (re *Regex) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, re)
}

func (re *Regex) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, re)
}

func (re *Regex) IsIterable() bool {
	return false
}

func (re *Regex) MakeIterator() vida.Iterator {
	return nil
}

func (re *Regex) IsHashable() bool {
	return false
}

func (re *Regex) MakeHashKey() vida.HashKey {
	return vida.HashKey{}
}

func (re *Regex) IsValueSemantics() bool {
	return false
}

func (re *Regex) HasMethods() bool {
	return true
}

func (re *Regex) GetMethod(name string) (vida.Value, bool, error) {
	if method, ok := RegexInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, vida.MethodNotDefined(name, re)
}

func (re *Regex) Clone() vida.Value {
	return &Regex{Value: re.Value}
}

var RegexInterface vida.Namespace

func loadRegex() vida.Importable {
	RegexInterface = vida.Namespace{
		"find":                       vida.GFunction{Name: "find", Value: regexFind},
		"findAll":                    vida.GFunction{Name: "findAll", Value: regexFindAll},
		"findAllSubmatch":            vida.GFunction{Name: "findAllSubmatch", Value: regexFindAllSubmatch},
		"findAllSubmatchIndex":       vida.GFunction{Name: "findAllSubmatchIndex", Value: regexFindAllSubmatchIndex},
		"findSubmatch":               vida.GFunction{Name: "findSubmatch", Value: regexFindSubmatch},
		"findSubmatchIndex":          vida.GFunction{Name: "findSubmatchIndex", Value: regexFindSubmatchIndex},
		"findIndex":                  vida.GFunction{Name: "findIndex", Value: regexFindIndex},
		"findAllIndex":               vida.GFunction{Name: "findAllIndex", Value: regexFindAllIndex},
		"findString":                 vida.GFunction{Name: "findString", Value: regexFindString},
		"findStringIndex":            vida.GFunction{Name: "findStringIndex", Value: regexFindStringIndex},
		"findStringSubmatch":         vida.GFunction{Name: "findStringSubmatch", Value: regexFindStringSubmatch},
		"findStringSubmatchIndex":    vida.GFunction{Name: "findStringSubmatchIndex", Value: regexFindStringSubmatchIndex},
		"findAllString":              vida.GFunction{Name: "findAllString", Value: regexFindAllString},
		"findAllStringIndex":         vida.GFunction{Name: "findAllStringIndex", Value: regexFindAllStringIndex},
		"findAllStringSubmatch":      vida.GFunction{Name: "findAllStringSubmatch", Value: regexFindAllStringSubmatch},
		"findAllStringSubmatchIndex": vida.GFunction{Name: "findAllStringSubmatchIndex", Value: regexFindAllStringSubmatchIndex},
		"match":                      vida.GFunction{Name: "match", Value: regexMatch},
		"matchString":                vida.GFunction{Name: "matchString", Value: regexMatchString},
		"longest":                    vida.GFunction{Name: "longest", Value: regexLongest},
		"expression":                 vida.GFunction{Name: "expression", Value: regexExpression},
	}
	gmodule := vida.GModule{Name: "regex", Namespace: vida.Namespace{
		"new": vida.GFunction{Name: "new", Value: regexNew},
	}}
	return gmodule
}

func regexNew(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if str, ok := args[0].(*vida.String); ok {
			if regex, err := regexp.Compile(str.Value); err == nil {
				return vida.Result{Value: &Regex{Value: regex}, Error: vida.NilValue}, nil
			} else {
				return vida.Result{Value: vida.NilValue, Error: &vida.String{Value: err.Error()}}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[0].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexMatchString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if str, ok := args[1].(*vida.String); ok {
			regex := args[0].(*Regex).Value
			return vida.Bool(regex.MatchString(str.Value)), nil
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexFindString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if str, ok := args[1].(*vida.String); ok {
			regex := args[0].(*Regex).Value
			return &vida.String{Value: regex.FindString(str.Value)}, nil
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexMatch(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if b, ok := args[1].(*vida.Bytes); ok {
			regex := args[0].(*Regex).Value
			return vida.Bool(regex.Match(b.Value)), nil
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexFind(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if b, ok := args[1].(*vida.Bytes); ok {
			result := args[0].(*Regex).Value.Find(b.Value)
			if result == nil {
				return vida.NilValue, nil
			} else {
				return &vida.Bytes{Value: result}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexFindAll(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.Bytes); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAll(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						xs[i] = &vida.Bytes{Value: v}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if b, ok := args[1].(*vida.Bytes); ok {
			result := args[0].(*Regex).Value.FindIndex(b.Value)
			if result == nil {
				return vida.NilValue, nil
			} else {
				slice := make([]vida.Value, 2)
				slice[0], slice[1] = vida.Int(result[0]), vida.Int(result[1])
				return &vida.List{Elements: slice}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexFindAllIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.Bytes); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAllIndex(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						l := make([]vida.Value, len(v))
						for j, w := range v {
							l[j] = vida.Int(w)
						}
						xs[i] = &vida.List{Elements: l}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindAllString(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.String); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAllString(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						xs[i] = &vida.String{Value: v}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindAllStringIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.String); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAllStringIndex(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						l := make([]vida.Value, len(v))
						for j, w := range v {
							l[j] = vida.Int(w)
						}
						xs[i] = &vida.List{Elements: l}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindAllStringSubmatch(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.String); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAllStringSubmatch(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						l := make([]vida.Value, len(v))
						for j, w := range v {
							l[j] = &vida.String{Value: w}
						}
						xs[i] = &vida.List{Elements: l}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindAllStringSubmatchIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.String); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAllStringSubmatchIndex(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						l := make([]vida.Value, len(v))
						for j, w := range v {
							l[j] = vida.Int(w)
						}
						xs[i] = &vida.List{Elements: l}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindAllSubmatch(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.Bytes); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAllSubmatch(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						l := make([]vida.Value, len(v))
						for j, w := range v {
							l[j] = &vida.Bytes{Value: w}
						}
						xs[i] = &vida.List{Elements: l}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindAllSubmatchIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 3 {
		if b, ok := args[1].(*vida.Bytes); ok {
			if i, ok := args[2].(vida.Int); ok {
				result := args[0].(*Regex).Value.FindAllSubmatchIndex(b.Value, int(i))
				if result == nil {
					return vida.NilValue, nil
				} else {
					xs := make([]vida.Value, len(result))
					for i, v := range result {
						l := make([]vida.Value, len(v))
						for j, w := range v {
							l[j] = vida.Int(w)
						}
						xs[i] = &vida.List{Elements: l}
					}
					return &vida.List{Elements: xs}, nil
				}
			}
			return nil, fmt.Errorf("expected a value of type Int, and got value of type '%v'", args[2].TypeName())
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 2, len(args))
}

func regexFindStringIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if str, ok := args[1].(*vida.String); ok {
			regex := args[0].(*Regex).Value
			result := regex.FindStringIndex(str.Value)
			if result == nil {
				return vida.NilValue, nil
			} else {
				xs := make([]vida.Value, len(result))
				for i, v := range result {
					xs[i] = vida.Int(v)
				}
				return &vida.List{Elements: xs}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexFindStringSubmatch(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if b, ok := args[1].(*vida.String); ok {
			result := args[0].(*Regex).Value.FindStringSubmatch(b.Value)
			if result == nil {
				return vida.NilValue, nil
			} else {
				xs := make([]vida.Value, len(result))
				for i, v := range result {
					xs[i] = &vida.String{Value: v}
				}
				return &vida.List{Elements: xs}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func regexFindStringSubmatchIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if str, ok := args[1].(*vida.String); ok {
			regex := args[0].(*Regex).Value
			result := regex.FindStringSubmatchIndex(str.Value)
			if result == nil {
				return vida.NilValue, nil
			} else {
				xs := make([]vida.Value, len(result))
				for i, v := range result {
					xs[i] = vida.Int(v)
				}
				return &vida.List{Elements: xs}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type String, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexFindSubmatch(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if b, ok := args[1].(*vida.Bytes); ok {
			result := args[0].(*Regex).Value.FindSubmatch(b.Value)
			if result == nil {
				return vida.NilValue, nil
			} else {
				xs := make([]vida.Value, len(result))
				for i, v := range result {
					xs[i] = &vida.Bytes{Value: v}
				}
				return &vida.List{Elements: xs}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}

func regexFindSubmatchIndex(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if b, ok := args[1].(*vida.Bytes); ok {
			result := args[0].(*Regex).Value.FindSubmatchIndex(b.Value)
			if result == nil {
				return vida.NilValue, nil
			} else {
				xs := make([]vida.Value, len(result))
				for i, v := range result {
					xs[i] = vida.Int(v)
				}
				return &vida.List{Elements: xs}, nil
			}
		}
		return nil, fmt.Errorf("expected a value of type Bytes, and got value of type '%v'", args[1].TypeName())
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 1, len(args))
}

func regexExpression(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		return &vida.String{Value: args[0].(*Regex).Value.String()}, nil
	}
	return nil, fmt.Errorf("expected %v arguments and got %v", 0, len(args))
}

func regexLongest(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		args[0].(*Regex).Value.Longest()
		return vida.NilValue, nil
	}
	return nil, fmt.Errorf("expected %v argument and got %v", 1, len(args))
}
