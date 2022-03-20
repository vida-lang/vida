package vida

import (
	"fmt"
)

// PrintError shows a given error in the terminal.
func PrintError(err error) {
	fmt.Printf("\n\n\n   %v:\n   %v\n\n", runTimeError, err.Error())
}

// Error messages for type errors in binary operations.
func TypeErrorInBinaryOperator(op string, lhs, rhs Value) error {
	return fmt.Errorf("type error with operator '%v' : (%v %v %v)", op, lhs.TypeName(), op, rhs.TypeName())
}

func NegativeShiftError(op string, lhs, rhs Value) error {
	return fmt.Errorf("attempt to perform a shift bits operation with a negative shift amount '%v' : (%v %v %v)", op, lhs, op, rhs)
}

// Division by zero error.
func DivisionByZeroError() error {
	return fmt.Errorf("attempt to perform a division by zero")
}

// Arity error in binary operator overload.
func ArityErrorInBinaryOperatorOverload() error {
	return fmt.Errorf("the arity of a prefix operator overloading must be 2")
}

// Arity error in unary operator overload.
func ArityErrorInUnaryOperatorOverload() error {
	return fmt.Errorf("the arity of a prefix operator overloading must be 1")
}

// Method not defined error.
func MethodNotDefined(method string, value Value) error {
	return fmt.Errorf("the method '%v' not defined in the struct '%v'", method, value.TypeName())
}

// Error message for type errors in unary operations.
func TypeErrorInPrefixOperator(op string, value Value) error {
	return fmt.Errorf("type error in prefix operator '%v' : (%v%v)", op, op, value.TypeName())
}

// Error messages for data structure subscriptions.
func IndexOutOfRangeError(length, index Int) error {
	return fmt.Errorf("subscript index is out of range with length %v and index [%v]", length, index)
}

// Error message when using a value not hashable as key in a map or a set.
func ValueNotHashableError(key Value) error {
	return fmt.Errorf("a value of type '%v' is not hashable", key.TypeName())
}

// Error message when using a non-string value as key in a record.
func RecordPropertyError(key Value) error {
	return fmt.Errorf("the vaue of type of '%v' cannot be used as key in a value of type Record", key.TypeName())
}

// Error message when using a value that cannot be an index.
func ValueIsNotAnIndexError(value Value) error {
	return fmt.Errorf("a value of type '%v' cannot be used as an index", value.TypeName())
}

// Error message when using a value that does not support subscription operations [].
func ValueDoesNotSupportSubscription(value Value) error {
	return fmt.Errorf("a value of type '%v' does not support subscription", value.TypeName())
}

// Error message for subscription operations in instances.
func InstancesDoNotSupportSubscriptionWriting() error {
	return fmt.Errorf("instances do not support subscription writing operations")
}

// Error messages for selection operations.
func NameNotDefinedInCompoundDataType(name string, dataStructure Value) error {
	return fmt.Errorf("the property/method '%v' is not defiend in '%v'", name, dataStructure)
}

// Error messages for values that does not support selector operator (.).
func SelectionOperationNotSupported(value Value) error {
	return fmt.Errorf("a value of type '%v' does not support selector operator '.'", value.TypeName())
}

// Error message used when trying to mutate an immutable value.
func ValueIsImmutable(value Value) error {
	return fmt.Errorf("cannot change the state of a value of type '%v'", value.TypeName())
}

// Error produced when trying to extend a non-Struct Value.
func ValueDoesNotSupportExtension(value Value) error {
	return fmt.Errorf("only structures can be extended")
}

// Error produced when trying to deriving properties and methods from a non-Struct value.
func CannotDeriveFromValue(value Value) error {
	return fmt.Errorf("cannot derive properties or methods from a non-Struct value")
}

// Error when type does not support slicing.
func ValueDoesNotSupportSlicing(value Value) error {
	return fmt.Errorf("a value of type '%v' does not support slicing", value.TypeName())
}

// Error when an operator is not defined for some data type.
func OperatorNotDefined(op byte, value Value) error {
	return fmt.Errorf("the operator '%v' is not defined for the type '%v'", KindDescription[op], value.TypeName())
}

// Error message used when an unknown flag is used for subscription or selection operations. This error never should have happened.
func NeverShouldHaveHappened(what string) error {
	return fmt.Errorf("sorry 💔 this error never ever should have happened.\n   '%v'", what)
}

// Error message used when changing a byte array with wrong data type.
func BytesChangeMustBeWithNumericTypesOnly() error {
	return fmt.Errorf("bytes state can be changed with integer data types only")
}

// Error when rune is out of range or it is illegal.
func RuneOutOfRangeOrIllegal() error {
	return fmt.Errorf("rune is illigal or it is out of range")
}

// Error when type does not support value semantics.
func ValueIsNotValueSemantics(value Value) error {
	return fmt.Errorf("a value of type '%v' has reference semantics", value.TypeName())
}

// Error when declaring a declared variable.
func VariableAlreadyDefined(identifier string) error {
	return fmt.Errorf("re-declaring a variable '%v' that has already been declared", identifier)
}

// Error when changing the value of a not-declared variable.
func VariableNotDefined(identifier string) error {
	return fmt.Errorf("the variable '%v' has not been declared yet", identifier)
}

// Error when an assertion failure occurs.
func AssertionFailure(message string) error {
	return fmt.Errorf(message)
}

// Error when range expression error.
func RangeExpressionError(value Value) error {
	return fmt.Errorf("a value of type '%v' is not iterable", value.TypeName())
}

// Error when step range is negative.
func RangeExpectedPositiveValue(value Value) error {
	return fmt.Errorf("the value given as step in range expression is negative '%v'", value)
}

// Error when step range is negative.
func RangeExpectedIntegerValue(value Value) error {
	return fmt.Errorf("one of the values found in range expression is not an Int '%v'", value.TypeName())
}

// Error when an instance overloaded __next operator with wrong arity.
func OverloadedOperatorWithWrongArity(method string, foundArity, must UInt32) error {
	return fmt.Errorf("the overloaded function '%v' must have an arity of '%v', but found an arity of '%v'", method, must, foundArity)
}

// Error when not found overloaded some method.
func MethodNotOverloaded(method, structName string) error {
	return fmt.Errorf("the Struct '%v' has not implemented the method '%v'", structName, method)
}

// Error when arity of a generator function is not zero.
func ArityGeneratorError(method string) error {
	return fmt.Errorf("arity of a generator function '%v' must be 0", method)
}

// Error when expected an iterable value.
func ExpectedIterableValueError(value Value) error {
	return fmt.Errorf("expected an iterable value in for-loop statement and got a value of type '%v'", value.TypeName())
}

// Error when expected a callable value.
func ExpectedCallableValueInDeferError(value Value) error {
	return fmt.Errorf("expected a callable value in defer statement and got a value of type '%v'", value.TypeName())
}

// Error when unpacking values.
func UnpackCountDoesNotMatchError() error {
	return fmt.Errorf("count of identifiers and values to unpack does not match")
}

// Error when a unpackable value was expected.
func ExpectedUnpackableValueError(value Value) error {
	return fmt.Errorf("expected unpackable value but got '%v'", value.TypeName())
}

// Error when a method or property was not found.
func IsNotMethodProperty(method, structName string) error {
	return fmt.Errorf("the identifier '%v' is not a method/property of the type '%v'", method, structName)
}

// Error when a unpackable value was expected.
func ExpectedListToSpreadError(value Value) error {
	return fmt.Errorf("expected a List to spread its elements but got '%v'", value.TypeName())
}

// Stack Overflow Error
func StackOverfloError() error {
	return fmt.Errorf("%v", stackOverflow)
}

// Error when a method or property was not found.
func VarArgArityError(arity, argCount UInt32) error {
	return fmt.Errorf("expected at least %v arguments in function call and got %v", arity, argCount)
}

// Error when a method or property was not found.
func ArityError(arity, argCount UInt32) error {
	return fmt.Errorf("expected %v arguments in function call and got %v", arity, argCount)
}

// Error when another value was expected.
func ExpectedTypeAndGotOtherType(expected, got string) error {
	return fmt.Errorf("expected a value of type '%v' and got '%v'", expected, got)
}
