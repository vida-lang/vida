package stdlib

import (
	"fmt"
	"time"

	"github.com/vida-lang/vida/vida"
)

// Durations models Go's Duration type derived from int64
type Duration struct {
	Value time.Duration
}

// Interface Value
func (d Duration) TypeName() string {
	return "Duration"
}

func (d Duration) Description() string {
	return fmt.Sprint(d.Value)
}

func (d Duration) Equals(other vida.Value) bool {
	if value, ok := other.(Duration); ok {
		return d.Value == value.Value
	}
	return false
}

func (d Duration) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	switch rhs := rhs.(type) {
	case Duration:
		switch op {
		case vida.TKAdd:
			return Duration{Value: d.Value + rhs.Value}, nil
		case vida.TKMul:
			return Duration{Value: d.Value * rhs.Value}, nil
		default:
			return nil, vida.TypeErrorInBinaryOperator(vida.KindDescription[op], d, rhs)
		}
	default:
		return nil, vida.TypeErrorInBinaryOperator(vida.KindDescription[op], d, rhs)
	}
}

func (d Duration) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, d)
}

func (d Duration) IsIterable() bool {
	return false
}

func (d Duration) MakeIterator() vida.Iterator {
	return nil
}

func (d Duration) IsHashable() bool {
	return true
}

func (d Duration) MakeHashKey() vida.HashKey {
	return vida.HashKey{
		Type:             d.TypeName(),
		ValueDescription: d.Description(),
	}
}

func (d Duration) IsValueSemantics() bool {
	return true
}

func (d Duration) HasMethods() bool {
	return false
}

func (d Duration) GetMethod(name string) (vida.Value, bool, error) {
	return nil, false, vida.MethodNotDefined(name, d)
}

func (d Duration) Clone() vida.Value {
	return Duration{Value: d.Value}
}

// Time models Go's Time struct for wall and monotonic clock.
type Time struct {
	Value time.Time
}

// Interface Value
func (t Time) TypeName() string {
	return "Time"
}

func (t Time) Description() string {
	return fmt.Sprint(t.Value)
}

func (t Time) Equals(other vida.Value) bool {
	if value, ok := other.(Time); ok {
		return t.Value == value.Value
	}
	return false
}

func (t Time) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, t)
}

func (t Time) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, t)
}

func (t Time) IsIterable() bool {
	return false
}

func (t Time) MakeIterator() vida.Iterator {
	return nil
}

func (t Time) IsHashable() bool {
	return true
}

func (t Time) MakeHashKey() vida.HashKey {
	return vida.HashKey{
		Type:             t.TypeName(),
		ValueDescription: t.Description(),
	}
}

func (t Time) IsValueSemantics() bool {
	return true
}

func (t Time) HasMethods() bool {
	return true
}

func (t Time) GetMethod(name string) (vida.Value, bool, error) {
	if method, ok := timeMethodsInterface[name]; ok {
		return method, true, nil
	}
	return nil, false, vida.MethodNotDefined(name, t)
}

func (t Time) Clone() vida.Value {
	return Time{Value: t.Value}
}

// WeekDay models weekdays.
type WeekDay struct {
	Value time.Weekday
}

// Interface vida.Value
func (day WeekDay) TypeName() string {
	return "WeekDay"
}

func (day WeekDay) Description() string {
	return day.Value.String()
}

func (day WeekDay) Equals(other vida.Value) bool {
	if value, ok := other.(WeekDay); ok {
		return day.Value == value.Value
	}
	return false
}
func (day WeekDay) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, day)
}

func (day WeekDay) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, day)
}

func (day WeekDay) IsIterable() bool {
	return false
}

func (day WeekDay) MakeIterator() vida.Iterator {
	return nil
}

func (day WeekDay) IsHashable() bool {
	return true
}

func (day WeekDay) MakeHashKey() vida.HashKey {
	return vida.HashKey{
		Type:             day.TypeName(),
		ValueDescription: day.Description(),
	}
}
func (day WeekDay) IsValueSemantics() bool {
	return true
}

func (day WeekDay) HasMethods() bool {
	return false
}

func (day WeekDay) GetMethod(name string) (vida.Value, bool, error) {
	return nil, false, vida.MethodNotDefined(name, day)
}

func (day WeekDay) Clone() vida.Value {
	return day
}

// Month models weekdays.
type Month struct {
	Value time.Month
}

// Interface vida.Value
func (m Month) TypeName() string {
	return "Month"
}

func (m Month) Description() string {
	return m.Value.String()
}

func (m Month) Equals(other vida.Value) bool {
	if value, ok := other.(Month); ok {
		return m.Value == value.Value
	}
	return false
}

func (m Month) BinaryOp(op byte, rhs vida.Value) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, m)
}

func (m Month) PrefixOp(op byte) (vida.Value, error) {
	return nil, vida.OperatorNotDefined(op, m)
}

func (m Month) IsIterable() bool {
	return false
}

func (m Month) MakeIterator() vida.Iterator {
	return nil
}

func (m Month) IsHashable() bool {
	return true
}

func (m Month) MakeHashKey() vida.HashKey {
	return vida.HashKey{
		Type:             m.TypeName(),
		ValueDescription: m.Description(),
	}
}

func (m Month) IsValueSemantics() bool {
	return true
}

func (m Month) HasMethods() bool {
	return false
}

func (m Month) GetMethod(name string) (vida.Value, bool, error) {
	return nil, false, vida.MethodNotDefined(name, m)
}

func (m Month) Clone() vida.Value {
	return m
}

// timeMethodsInterface is the collection of methods for the type Time.
var timeMethodsInterface vida.Namespace

// loadTime loads the time lib from the stdlib to a module wirtten in Vida.
func loadTime() vida.Importable {
	// Method inteface for the value Time.
	timeMethodsInterface = vida.Namespace{
		"unixMilli": vida.GFunction{Name: "unixMilli", Value: timeUnixMilli},
		"unixMicro": vida.GFunction{Name: "unixMicro", Value: timeUnixMicro},
		"unixNano":  vida.GFunction{Name: "unixNano", Value: timeUnixNano},
		"add":       vida.GFunction{Name: "add", Value: timeAdd},
		"sub":       vida.GFunction{Name: "sub", Value: timeSub},
	}
	// GModule for the stdlib time.
	gmodule := vida.GModule{Name: "time", Namespace: vida.Namespace{
		"now":         vida.GFunction{Name: "now", Value: timeNow},
		"since":       vida.GFunction{Name: "since", Value: timeSince},
		"sleep":       vida.GFunction{Name: "sleep", Value: timeSleep},
		"hour":        Duration{Value: time.Hour},
		"minute":      Duration{Value: time.Minute},
		"second":      Duration{Value: time.Second},
		"microsecond": Duration{Value: time.Microsecond},
		"millisecond": Duration{Value: time.Millisecond},
		"nanosecond":  Duration{Value: time.Nanosecond},
		"Monday":      WeekDay{Value: time.Monday},
		"Tuesday":     WeekDay{Value: time.Tuesday},
		"Wednesday":   WeekDay{Value: time.Wednesday},
		"Thursday":    WeekDay{Value: time.Thursday},
		"Friday":      WeekDay{Value: time.Friday},
		"Saturday":    WeekDay{Value: time.Saturday},
		"Sunday":      WeekDay{Value: time.Sunday},
		"January":     Month{Value: time.January},
		"February":    Month{Value: time.February},
		"March":       Month{Value: time.March},
		"April":       Month{Value: time.April},
		"May":         Month{Value: time.May},
		"June":        Month{Value: time.June},
		"July":        Month{Value: time.July},
		"August":      Month{Value: time.August},
		"September":   Month{Value: time.September},
		"October":     Month{Value: time.October},
		"November":    Month{Value: time.November},
		"December":    Month{Value: time.December},
		"Duration":    vida.GFunction{Name: "Duration", Value: timeDuration}}}
	return gmodule
}

func timeNow(args ...vida.Value) (vida.Value, error) {
	if len(args) == 0 {
		return Time{Value: time.Now()}, nil
	}
	return nil, fmt.Errorf("exptected %v arguments and got %v", 0, len(args))
}

func timeSince(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if givenTime, ok := args[0].(Time); ok {
			return Duration{Value: time.Since(givenTime.Value)}, nil
		} else {
			return nil, fmt.Errorf("expected a value of type Time as argument")
		}
	}
	return nil, fmt.Errorf("exptected %v argument and got %v", 1, len(args))
}

func timeSleep(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if duration, ok := args[0].(Duration); ok {
			time.Sleep(duration.Value)
			return vida.NilValue, nil
		}
		return nil, fmt.Errorf("exptected a value of type Duration as argument")
	}
	return nil, fmt.Errorf("exptected %v argument and got %v", 1, len(args))
}

func timeDuration(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		switch value := args[0].(type) {
		case vida.Int:
			return Duration{Value: time.Duration(value)}, nil
		case vida.UInt:
			return Duration{Value: time.Duration(value)}, nil
		case vida.Byte:
			return Duration{Value: time.Duration(value)}, nil
		case Duration:
			return value, nil
		default:
			return nil, fmt.Errorf("exptected an Integer type as argument")
		}
	}
	return nil, fmt.Errorf("exptected %v argument and got %v", 1, len(args))
}

func timeAdd(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if tLhs, ok := args[0].(Time); ok {
			if tRhs, ok := args[1].(Duration); ok {
				return Time{Value: tLhs.Value.Add(tRhs.Value)}, nil
			}
			return nil, fmt.Errorf("exptected a Duration value as second argument")
		}
		return nil, fmt.Errorf("exptected an Time value as first argument")
	}
	return nil, fmt.Errorf("exptected %v arguments and got %v", 2, len(args))
}

func timeSub(args ...vida.Value) (vida.Value, error) {
	if len(args) == 2 {
		if tLhs, ok := args[0].(Time); ok {
			if tRhs, ok := args[1].(Time); ok {
				return Duration{Value: tLhs.Value.Sub(tRhs.Value)}, nil
			}
			return nil, fmt.Errorf("exptected an Time value as second argument")
		}
		return nil, fmt.Errorf("exptected an Time value as first argument")
	}
	return nil, fmt.Errorf("exptected %v arguments and got %v", 2, len(args))
}

func timeUnixNano(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if t, ok := args[0].(Time); ok {
			return vida.Int(t.Value.UnixNano()), nil
		}
		return nil, fmt.Errorf("exptected a value of type Time as argument")
	}
	return nil, fmt.Errorf("exptected %v argument and got %v", 1, len(args))
}

func timeUnixMilli(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if t, ok := args[0].(Time); ok {
			return vida.Int(t.Value.UnixMilli()), nil
		}
		return nil, fmt.Errorf("exptected a value of type Time as argument")
	}
	return nil, fmt.Errorf("exptected %v argument and got %v", 1, len(args))
}

func timeUnixMicro(args ...vida.Value) (vida.Value, error) {
	if len(args) == 1 {
		if t, ok := args[0].(Time); ok {
			return vida.Int(t.Value.UnixMicro()), nil
		}
		return nil, fmt.Errorf("exptected a value of type Time as argument")
	}
	return nil, fmt.Errorf("exptected %v argument and got %v", 1, len(args))
}
