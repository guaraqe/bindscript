# bindscript

A DSL for the generation of Purescript bindings for Javascript libraries.

- [Goals](#goals)
- [Usage](#usage)
- [Data and function definitions](#data-and-function-definitions)
- [Accessory methods](#accessory-methods)

## Goals

Writing Purescript bindings for Javascript libraries is very tedious for many reasons:

- bindings depend on two files, a `.js` one and a `.purs` one;
- javascript functions and methods must be curried manually;
- the code being written very often follows a repetitive template.

While other solutions exist for some of these problems, like `Data.Function.Uncurried` or [easy-ffi](https://github.com/pelotom/purescript-easy-ffi), `bindscript` aims to give an improved experience in all of these aspects.

It works by writing a single `.bs` file containing Purescript type definitions and Javascript function bodies or accessory methods, and generate both `.js` and `.purs` files containing the bindings.

## Usage

The `bindscript` executable can be used either on single files or folder trees.

```
bindscript --input STRING [--output STRING]
```

If the output path is not given, it is derived from the input's path.
An example of bindscript file is the following:

```
module Test where

import Blabla

//BINDINGS

-- | Data documentation
data Test :: Type -> Type

-- | Function documentation
-- With comments
function :: Int -> Int
         -> ThreeEff Double
x y _ ->
  // Some comment
  return x + y;

-- Automatic property getter
propertyGetTest :: Object -> Int
property get propertyName

-- Automatic property setter
propertySetTest :: Object -> Int -> ThreeEff Unit
property set propertyName

-- Automatic constructor
constructorTest :: Int -> Three Object
constructor eff constructorName 1

-- Automatic method
methodTest :: Object -> Int -> Int -> Three Int
method eff methodName 2

-- Automatic function
functionTest :: Int -> Int -> Int
function pure functionName 2
```

Everything before `//BINDINGS` is copied as-is to the target Purescript file.
The following definitions are converted as follows.

## Data and function definitions

### Data definitions

The block:

```
-- | Data documentation
data Test :: Type -> Type
```

is transformed in Purescript to:

```
-- | Data documentation
foreign import data Test :: Type -> Type
```

and ignored in the Javascript output.

### Function definitions

The block:

```
-- | Function documentation
-- With comments
function :: Int -> Int
         -> ThreeEff Double
x y _ ->
  // Some comment
  return x + y;
```

is transformed in Purescript to:

```
-- | Function documentation
-- With comments
foreign import function ::
     Int -> Int
  -> ThreeEff Double
```

and in Javascript to:

```
exports.function = function(x) {
  return function(y) {
    return function() {
      // Some comment
      return x + y;
    };
  };
};
```

## Accessory methods

There are several accessory methods that automatically write functions for common patterns:

- `property` for object property getters and setters;
- `method` for object methods;
- `function` for curring functions.

They are used as follows.

### Object property getters

For object property getters, one only needs to give the accessor name.
The block:

```
-- Automatic property getter
propertyGetTest :: Object -> Int
property get propertyName
```

is transformed in Purescript to:

```
-- Automatic property getter
foreign import propertyGetTest :: Object -> Int
```

and in Javascript to:

```
exports.propertyGetTest = function(var0) {
  var0.propertyName;
};
```

### Object property setters

For object property setters, one only needs to give the accessor name.
The block:

```
-- Automatic property setter
propertySetTest :: Object -> Int -> ThreeEff Unit
property set propertyName
```

is transformed in Purescript to:

```
-- Automatic property setter
foreign import propertySetTest :: Object -> Int -> ThreeEff Unit
```

and in Javascript to:

```
exports.propertySetTest = function(var0) {
  return function(var1) {
    var0.propertyName = var1;
  };
};
```

### Constructors

For constructors, one needs to say if it is effectful or pure, the constructor name and its number of parameters.
The block:

```
-- Automatic constructor
constructorTest :: Int -> Three Object
constructor eff constructorName 1
```

is transformed in Purescript to:
```
-- Automatic constructor
foreign import constructorTest :: Int -> Three Object
```

and in Javascript to:

```
exports.constructorTest = function(var0) {
  return function() {
    return new constructorName(var0);
  };
};
```

### Object methods

For object methods, one needs to say if it is effectful or pure, the method name and its number of parameters.
The block:

```
-- Automatic method
methodTest :: Object -> Int -> Int -> Three Int
method eff methodName 2
```

is transformed in Purescript to:

```
-- Automatic method
foreign import methodTest :: Object -> Int -> Int -> Three Int
```

and in Javascript to:

```
exports.methodTest = function(var0) {
  return function(var1) {
    return function(var2) {
      return function() {
        return var0.methodName(var1,var2);
      };
    };
  };
};
```

### Curried functions

For functions, one needs to say if it is effectful or pure, the function name and its number of parameters.
The block:

```
-- Automatic function
functionTest :: Int -> Int -> Int
function pure functionName 2
```

is transformed in Purescript to:

```
-- Automatic function
foreign import functionTest :: Int -> Int -> Int
```

and in Javascript to:

```
exports.functionTest = function(var0) {
  return function(var1) {
    return functionName(var0,var1);
  };
};
```

### Wildcards

An undescore (`_`) can be used in order to use the same name for the Purescript and Javascript functions.
That is, the following are equivalent:

```
functionName :: Int -> Int -> Int
function pure _ 2


functionName :: Int -> Int -> Int
function pure functionName 2
```
