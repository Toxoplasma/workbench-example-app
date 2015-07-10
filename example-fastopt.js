'use strict';
/* Scala.js runtime support
 * Copyright 2013 LAMP/EPFL
 * Author: Sébastien Doeraene
 */

/* ---------------------------------- *
 * The top-level Scala.js environment *
 * ---------------------------------- */

var ScalaJS = {};

// Get the environment info
ScalaJS.env = (typeof __ScalaJSEnv === "object" && __ScalaJSEnv) ? __ScalaJSEnv : {};

// Global scope
ScalaJS.g =
  (typeof ScalaJS.env["global"] === "object" && ScalaJS.env["global"])
    ? ScalaJS.env["global"]
    : ((typeof global === "object" && global && global["Object"] === Object) ? global : this);
ScalaJS.env["global"] = ScalaJS.g;

// Where to send exports
ScalaJS.e =
  (typeof ScalaJS.env["exportsNamespace"] === "object" && ScalaJS.env["exportsNamespace"])
    ? ScalaJS.env["exportsNamespace"] : ScalaJS.g;
ScalaJS.env["exportsNamespace"] = ScalaJS.e;

// Freeze the environment info
ScalaJS.g["Object"]["freeze"](ScalaJS.env);

// Other fields
ScalaJS.d = {};         // Data for types
ScalaJS.c = {};         // Scala.js constructors
ScalaJS.h = {};         // Inheritable constructors (without initialization code)
ScalaJS.s = {};         // Static methods
ScalaJS.n = {};         // Module instances
ScalaJS.m = {};         // Module accessors
ScalaJS.is = {};        // isInstanceOf methods
ScalaJS.isArrayOf = {}; // isInstanceOfArrayOf methods

ScalaJS.as = {};        // asInstanceOf methods
ScalaJS.asArrayOf = {}; // asInstanceOfArrayOf methods

ScalaJS.lastIDHash = 0; // last value attributed to an id hash code

// Core mechanism

ScalaJS.makeIsArrayOfPrimitive = function(primitiveData) {
  return function(obj, depth) {
    return !!(obj && obj.$classData &&
      (obj.$classData.arrayDepth === depth) &&
      (obj.$classData.arrayBase === primitiveData));
  }
};


ScalaJS.makeAsArrayOfPrimitive = function(isInstanceOfFunction, arrayEncodedName) {
  return function(obj, depth) {
    if (isInstanceOfFunction(obj, depth) || (obj === null))
      return obj;
    else
      ScalaJS.throwArrayCastException(obj, arrayEncodedName, depth);
  }
};


/** Encode a property name for runtime manipulation
  *  Usage:
  *    env.propertyName({someProp:0})
  *  Returns:
  *    "someProp"
  *  Useful when the property is renamed by a global optimizer (like Closure)
  *  but we must still get hold of a string of that name for runtime
  * reflection.
  */
ScalaJS.propertyName = function(obj) {
  var result;
  for (var prop in obj)
    result = prop;
  return result;
};

// Runtime functions

ScalaJS.isScalaJSObject = function(obj) {
  return !!(obj && obj.$classData);
};


ScalaJS.throwClassCastException = function(instance, classFullName) {




  throw new ScalaJS.c.sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new ScalaJS.c.jl_ClassCastException().init___T(
      instance + " is not an instance of " + classFullName));

};

ScalaJS.throwArrayCastException = function(instance, classArrayEncodedName, depth) {
  for (; depth; --depth)
    classArrayEncodedName = "[" + classArrayEncodedName;
  ScalaJS.throwClassCastException(instance, classArrayEncodedName);
};


ScalaJS.noIsInstance = function(instance) {
  throw new ScalaJS.g["TypeError"](
    "Cannot call isInstance() on a Class representing a raw JS trait/object");
};

ScalaJS.makeNativeArrayWrapper = function(arrayClassData, nativeArray) {
  return new arrayClassData.constr(nativeArray);
};

ScalaJS.newArrayObject = function(arrayClassData, lengths) {
  return ScalaJS.newArrayObjectInternal(arrayClassData, lengths, 0);
};

ScalaJS.newArrayObjectInternal = function(arrayClassData, lengths, lengthIndex) {
  var result = new arrayClassData.constr(lengths[lengthIndex]);

  if (lengthIndex < lengths.length-1) {
    var subArrayClassData = arrayClassData.componentData;
    var subLengthIndex = lengthIndex+1;
    var underlying = result.u;
    for (var i = 0; i < underlying.length; i++) {
      underlying[i] = ScalaJS.newArrayObjectInternal(
        subArrayClassData, lengths, subLengthIndex);
    }
  }

  return result;
};

ScalaJS.checkNonNull = function(obj) {
  return obj !== null ? obj : ScalaJS.throwNullPointerException();
};

ScalaJS.throwNullPointerException = function() {
  throw new ScalaJS.c.jl_NullPointerException().init___();
};

ScalaJS.objectToString = function(instance) {
  if (instance === void 0)
    return "undefined";
  else
    return instance.toString();
};

ScalaJS.objectGetClass = function(instance) {
  switch (typeof instance) {
    case "string":
      return ScalaJS.d.T.getClassOf();
    case "number":
      var v = instance | 0;
      if (v === instance) { // is the value integral?
        if (ScalaJS.isByte(v))
          return ScalaJS.d.jl_Byte.getClassOf();
        else if (ScalaJS.isShort(v))
          return ScalaJS.d.jl_Short.getClassOf();
        else
          return ScalaJS.d.jl_Integer.getClassOf();
      } else {
        if (ScalaJS.isFloat(instance))
          return ScalaJS.d.jl_Float.getClassOf();
        else
          return ScalaJS.d.jl_Double.getClassOf();
      }
    case "boolean":
      return ScalaJS.d.jl_Boolean.getClassOf();
    case "undefined":
      return ScalaJS.d.sr_BoxedUnit.getClassOf();
    default:
      if (instance === null)
        ScalaJS.throwNullPointerException();
      else if (ScalaJS.is.sjsr_RuntimeLong(instance))
        return ScalaJS.d.jl_Long.getClassOf();
      else if (ScalaJS.isScalaJSObject(instance))
        return instance.$classData.getClassOf();
      else
        return null; // Exception?
  }
};

ScalaJS.objectClone = function(instance) {
  if (ScalaJS.isScalaJSObject(instance) || (instance === null))
    return instance.clone__O();
  else
    throw new ScalaJS.c.jl_CloneNotSupportedException().init___();
};

ScalaJS.objectNotify = function(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notify__V();
};

ScalaJS.objectNotifyAll = function(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notifyAll__V();
};

ScalaJS.objectFinalize = function(instance) {
  if (ScalaJS.isScalaJSObject(instance) || (instance === null))
    instance.finalize__V();
  // else no-op
};

ScalaJS.objectEquals = function(instance, rhs) {
  if (ScalaJS.isScalaJSObject(instance) || (instance === null))
    return instance.equals__O__Z(rhs);
  else if (typeof instance === "number")
    return typeof rhs === "number" && ScalaJS.numberEquals(instance, rhs);
  else
    return instance === rhs;
};

ScalaJS.numberEquals = function(lhs, rhs) {
  return (lhs === rhs) ? (
    // 0.0.equals(-0.0) must be false
    lhs !== 0 || 1/lhs === 1/rhs
  ) : (
    // are they both NaN?
    (lhs !== lhs) && (rhs !== rhs)
  );
};

ScalaJS.objectHashCode = function(instance) {
  switch (typeof instance) {
    case "string":
      return ScalaJS.m.sjsr_RuntimeString$().hashCode__T__I(instance);
    case "number":
      return ScalaJS.m.sjsr_Bits$().numberHashCode__D__I(instance);
    case "boolean":
      return instance ? 1231 : 1237;
    case "undefined":
      return 0;
    default:
      if (ScalaJS.isScalaJSObject(instance) || instance === null)
        return instance.hashCode__I();
      else
        return 42; // TODO?
  }
};

ScalaJS.comparableCompareTo = function(instance, rhs) {
  switch (typeof instance) {
    case "string":

      ScalaJS.as.T(rhs);

      return instance === rhs ? 0 : (instance < rhs ? -1 : 1);
    case "number":

      ScalaJS.as.jl_Number(rhs);

      return ScalaJS.m.jl_Double$().compare__D__D__I(instance, rhs);
    case "boolean":

      ScalaJS.asBoolean(rhs);

      return instance - rhs; // yes, this gives the right result
    default:
      return instance.compareTo__O__I(rhs);
  }
};

ScalaJS.charSequenceLength = function(instance) {
  if (typeof(instance) === "string")

    return ScalaJS.uI(instance["length"]);



  else
    return instance.length__I();
};

ScalaJS.charSequenceCharAt = function(instance, index) {
  if (typeof(instance) === "string")

    return ScalaJS.uI(instance["charCodeAt"](index)) & 0xffff;



  else
    return instance.charAt__I__C(index);
};

ScalaJS.charSequenceSubSequence = function(instance, start, end) {
  if (typeof(instance) === "string")

    return ScalaJS.as.T(instance["substring"](start, end));



  else
    return instance.subSequence__I__I__jl_CharSequence(start, end);
};

ScalaJS.booleanBooleanValue = function(instance) {
  if (typeof instance === "boolean") return instance;
  else                               return instance.booleanValue__Z();
};

ScalaJS.numberByteValue = function(instance) {
  if (typeof instance === "number") return (instance << 24) >> 24;
  else                              return instance.byteValue__B();
};
ScalaJS.numberShortValue = function(instance) {
  if (typeof instance === "number") return (instance << 16) >> 16;
  else                              return instance.shortValue__S();
};
ScalaJS.numberIntValue = function(instance) {
  if (typeof instance === "number") return instance | 0;
  else                              return instance.intValue__I();
};
ScalaJS.numberLongValue = function(instance) {
  if (typeof instance === "number")
    return ScalaJS.m.sjsr_RuntimeLong$().fromDouble__D__sjsr_RuntimeLong(instance);
  else
    return instance.longValue__J();
};
ScalaJS.numberFloatValue = function(instance) {
  if (typeof instance === "number") return ScalaJS.fround(instance);
  else                              return instance.floatValue__F();
};
ScalaJS.numberDoubleValue = function(instance) {
  if (typeof instance === "number") return instance;
  else                              return instance.doubleValue__D();
};

ScalaJS.isNaN = function(instance) {
  return instance !== instance;
};

ScalaJS.isInfinite = function(instance) {
  return !ScalaJS.g["isFinite"](instance) && !ScalaJS.isNaN(instance);
};

ScalaJS.propertiesOf = function(obj) {
  var result = [];
  for (var prop in obj)
    result["push"](prop);
  return result;
};

ScalaJS.systemArraycopy = function(src, srcPos, dest, destPos, length) {
  var srcu = src.u;
  var destu = dest.u;
  if (srcu !== destu || destPos < srcPos || srcPos + length < destPos) {
    for (var i = 0; i < length; i++)
      destu[destPos+i] = srcu[srcPos+i];
  } else {
    for (var i = length-1; i >= 0; i--)
      destu[destPos+i] = srcu[srcPos+i];
  }
};

ScalaJS.systemIdentityHashCode = function(obj) {
  if (ScalaJS.isScalaJSObject(obj)) {
    var hash = obj["$idHashCode$0"];
    if (hash !== void 0) {
      return hash;
    } else {
      hash = (ScalaJS.lastIDHash + 1) | 0;
      ScalaJS.lastIDHash = hash;
      obj["$idHashCode$0"] = hash;
      return hash;
    }
  } else if (obj === null) {
    return 0;
  } else {
    return ScalaJS.objectHashCode(obj);
  }
};

// is/as for hijacked boxed classes (the non-trivial ones)

ScalaJS.isByte = function(v) {
  return (v << 24 >> 24) === v && 1/v !== 1/-0;
};

ScalaJS.isShort = function(v) {
  return (v << 16 >> 16) === v && 1/v !== 1/-0;
};

ScalaJS.isInt = function(v) {
  return (v | 0) === v && 1/v !== 1/-0;
};

ScalaJS.isFloat = function(v) {
  return v !== v || ScalaJS.fround(v) === v;
};


ScalaJS.asUnit = function(v) {
  if (v === void 0)
    return v;
  else
    ScalaJS.throwClassCastException(v, "scala.runtime.BoxedUnit");
};

ScalaJS.asBoolean = function(v) {
  if (typeof v === "boolean" || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Boolean");
};

ScalaJS.asByte = function(v) {
  if (ScalaJS.isByte(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Byte");
};

ScalaJS.asShort = function(v) {
  if (ScalaJS.isShort(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Short");
};

ScalaJS.asInt = function(v) {
  if (ScalaJS.isInt(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Integer");
};

ScalaJS.asFloat = function(v) {
  if (ScalaJS.isFloat(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Float");
};

ScalaJS.asDouble = function(v) {
  if (typeof v === "number" || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Double");
};


// Unboxes


ScalaJS.uZ = function(value) {
  return !!ScalaJS.asBoolean(value);
};
ScalaJS.uB = function(value) {
  return ScalaJS.asByte(value) | 0;
};
ScalaJS.uS = function(value) {
  return ScalaJS.asShort(value) | 0;
};
ScalaJS.uI = function(value) {
  return ScalaJS.asInt(value) | 0;
};
ScalaJS.uJ = function(value) {
  return null === value ? ScalaJS.m.sjsr_RuntimeLong$().Zero$1
                        : ScalaJS.as.sjsr_RuntimeLong(value);
};
ScalaJS.uF = function(value) {
  /* Here, it is fine to use + instead of fround, because asFloat already
   * ensures that the result is either null or a float. 
   */
  return +ScalaJS.asFloat(value);
};
ScalaJS.uD = function(value) {
  return +ScalaJS.asDouble(value);
};






// TypeArray conversions

ScalaJS.byteArray2TypedArray = function(value) { return new Int8Array(value.u); };
ScalaJS.shortArray2TypedArray = function(value) { return new Int16Array(value.u); };
ScalaJS.charArray2TypedArray = function(value) { return new Uint16Array(value.u); };
ScalaJS.intArray2TypedArray = function(value) { return new Int32Array(value.u); };
ScalaJS.floatArray2TypedArray = function(value) { return new Float32Array(value.u); };
ScalaJS.doubleArray2TypedArray = function(value) { return new Float64Array(value.u); };

ScalaJS.typedArray2ByteArray = function(value) {
  var arrayClassData = ScalaJS.d.B.getArrayOf();
  return new arrayClassData.constr(new Int8Array(value));
};
ScalaJS.typedArray2ShortArray = function(value) {
  var arrayClassData = ScalaJS.d.S.getArrayOf();
  return new arrayClassData.constr(new Int16Array(value));
};
ScalaJS.typedArray2CharArray = function(value) {
  var arrayClassData = ScalaJS.d.C.getArrayOf();
  return new arrayClassData.constr(new Uint16Array(value));
};
ScalaJS.typedArray2IntArray = function(value) {
  var arrayClassData = ScalaJS.d.I.getArrayOf();
  return new arrayClassData.constr(new Int32Array(value));
};
ScalaJS.typedArray2FloatArray = function(value) {
  var arrayClassData = ScalaJS.d.F.getArrayOf();
  return new arrayClassData.constr(new Float32Array(value));
};
ScalaJS.typedArray2DoubleArray = function(value) {
  var arrayClassData = ScalaJS.d.D.getArrayOf();
  return new arrayClassData.constr(new Float64Array(value));
};

/* We have to force a non-elidable *read* of ScalaJS.e, otherwise Closure will
 * eliminate it altogether, along with all the exports, which is ... er ...
 * plain wrong.
 */
this["__ScalaJSExportsNamespace"] = ScalaJS.e;

// Type data constructors

/** @constructor */
ScalaJS.PrimitiveTypeData = function(zero, arrayEncodedName, displayName) {
  // Runtime support
  this.constr = undefined;
  this.parentData = undefined;
  this.ancestors = {};
  this.componentData = null;
  this.zero = zero;
  this.arrayEncodedName = arrayEncodedName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isArrayOf = function(obj, depth) { return false; };

  // java.lang.Class support
  this["name"] = displayName;
  this["isPrimitive"] = true;
  this["isInterface"] = false;
  this["isArrayClass"] = false;
  this["isInstance"] = function(obj) { return false; };
};

/** @constructor */
ScalaJS.ClassTypeData = function(internalNameObj, isInterface, fullName,
                                 ancestors, parentData, isInstance, isArrayOf) {
  var internalName = ScalaJS.propertyName(internalNameObj);

  isInstance = isInstance || function(obj) {
    return !!(obj && obj.$classData && obj.$classData.ancestors[internalName]);
  };

  isArrayOf = isArrayOf || function(obj, depth) {
    return !!(obj && obj.$classData && (obj.$classData.arrayDepth === depth)
      && obj.$classData.arrayBase.ancestors[internalName])
  };

  // Runtime support
  this.constr = undefined;
  this.parentData = parentData;
  this.ancestors = ancestors;
  this.componentData = null;
  this.zero = null;
  this.arrayEncodedName = "L"+fullName+";";
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isArrayOf = isArrayOf;

  // java.lang.Class support
  this["name"] = fullName;
  this["isPrimitive"] = false;
  this["isInterface"] = isInterface;
  this["isArrayClass"] = false;
  this["isInstance"] = isInstance;
};

/** @constructor */
ScalaJS.ArrayTypeData = function(componentData) {
  // The constructor

  var componentZero = componentData.zero;

  // The zero for the Long runtime representation
  // is a special case here, since the class has not
  // been defined yet, when this file is read
  if (componentZero == "longZero")
    componentZero = ScalaJS.m.sjsr_RuntimeLong$().Zero$1;

  /** @constructor */
  var ArrayClass = function(arg) {
    if (typeof(arg) === "number") {
      // arg is the length of the array
      this.u = new Array(arg);
      for (var i = 0; i < arg; i++)
        this.u[i] = componentZero;
    } else {
      // arg is a native array that we wrap
      this.u = arg;
    }
  }
  ArrayClass.prototype = new ScalaJS.h.O;
  ArrayClass.prototype.constructor = ArrayClass;
  ArrayClass.prototype.$classData = this;

  ArrayClass.prototype.clone__O = function() {
    if (this.u instanceof Array)
      return new ArrayClass(this.u["slice"](0));
    else
      // The underlying Array is a TypedArray
      return new ArrayClass(this.u.constructor(this.u));
  };

  // Don't generate reflective call proxies. The compiler special cases
  // reflective calls to methods on scala.Array

  // The data

  var encodedName = "[" + componentData.arrayEncodedName;
  var componentBase = componentData.arrayBase || componentData;
  var componentDepth = componentData.arrayDepth || 0;
  var arrayDepth = componentDepth + 1;

  var isInstance = function(obj) {
    return componentBase.isArrayOf(obj, arrayDepth);
  }

  // Runtime support
  this.constr = ArrayClass;
  this.parentData = ScalaJS.d.O;
  this.ancestors = {O: 1};
  this.componentData = componentData;
  this.arrayBase = componentBase;
  this.arrayDepth = arrayDepth;
  this.zero = null;
  this.arrayEncodedName = encodedName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isArrayOf = undefined;

  // java.lang.Class support
  this["name"] = encodedName;
  this["isPrimitive"] = false;
  this["isInterface"] = false;
  this["isArrayClass"] = true;
  this["isInstance"] = isInstance;
};

ScalaJS.ClassTypeData.prototype.getClassOf = function() {
  if (!this._classOf)
    this._classOf = new ScalaJS.c.jl_Class().init___jl_ScalaJSClassData(this);
  return this._classOf;
};

ScalaJS.ClassTypeData.prototype.getArrayOf = function() {
  if (!this._arrayOf)
    this._arrayOf = new ScalaJS.ArrayTypeData(this);
  return this._arrayOf;
};

// java.lang.Class support

ScalaJS.ClassTypeData.prototype["getFakeInstance"] = function() {
  if (this === ScalaJS.d.T)
    return "some string";
  else if (this === ScalaJS.d.jl_Boolean)
    return false;
  else if (this === ScalaJS.d.jl_Byte ||
           this === ScalaJS.d.jl_Short ||
           this === ScalaJS.d.jl_Integer ||
           this === ScalaJS.d.jl_Float ||
           this === ScalaJS.d.jl_Double)
    return 0;
  else if (this === ScalaJS.d.jl_Long)
    return ScalaJS.m.sjsr_RuntimeLong$().Zero$1;
  else if (this === ScalaJS.d.sr_BoxedUnit)
    return void 0;
  else
    return {$classData: this};
};

ScalaJS.ClassTypeData.prototype["getSuperclass"] = function() {
  return this.parentData ? this.parentData.getClassOf() : null;
};

ScalaJS.ClassTypeData.prototype["getComponentType"] = function() {
  return this.componentData ? this.componentData.getClassOf() : null;
};

ScalaJS.ClassTypeData.prototype["newArrayOfThisClass"] = function(lengths) {
  var arrayClassData = this;
  for (var i = 0; i < lengths.length; i++)
    arrayClassData = arrayClassData.getArrayOf();
  return ScalaJS.newArrayObject(arrayClassData, lengths);
};

ScalaJS.PrimitiveTypeData.prototype = ScalaJS.ClassTypeData.prototype;
ScalaJS.ArrayTypeData.prototype = ScalaJS.ClassTypeData.prototype;

// Create primitive types

ScalaJS.d.V = new ScalaJS.PrimitiveTypeData(undefined, "V", "void");
ScalaJS.d.Z = new ScalaJS.PrimitiveTypeData(false, "Z", "boolean");
ScalaJS.d.C = new ScalaJS.PrimitiveTypeData(0, "C", "char");
ScalaJS.d.B = new ScalaJS.PrimitiveTypeData(0, "B", "byte");
ScalaJS.d.S = new ScalaJS.PrimitiveTypeData(0, "S", "short");
ScalaJS.d.I = new ScalaJS.PrimitiveTypeData(0, "I", "int");
ScalaJS.d.J = new ScalaJS.PrimitiveTypeData("longZero", "J", "long");
ScalaJS.d.F = new ScalaJS.PrimitiveTypeData(0.0, "F", "float");
ScalaJS.d.D = new ScalaJS.PrimitiveTypeData(0.0, "D", "double");

// Instance tests for array of primitives

ScalaJS.isArrayOf.Z = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.Z);
ScalaJS.d.Z.isArrayOf = ScalaJS.isArrayOf.Z;

ScalaJS.isArrayOf.C = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.C);
ScalaJS.d.C.isArrayOf = ScalaJS.isArrayOf.C;

ScalaJS.isArrayOf.B = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.B);
ScalaJS.d.B.isArrayOf = ScalaJS.isArrayOf.B;

ScalaJS.isArrayOf.S = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.S);
ScalaJS.d.S.isArrayOf = ScalaJS.isArrayOf.S;

ScalaJS.isArrayOf.I = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.I);
ScalaJS.d.I.isArrayOf = ScalaJS.isArrayOf.I;

ScalaJS.isArrayOf.J = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.J);
ScalaJS.d.J.isArrayOf = ScalaJS.isArrayOf.J;

ScalaJS.isArrayOf.F = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.F);
ScalaJS.d.F.isArrayOf = ScalaJS.isArrayOf.F;

ScalaJS.isArrayOf.D = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.D);
ScalaJS.d.D.isArrayOf = ScalaJS.isArrayOf.D;


// asInstanceOfs for array of primitives
ScalaJS.asArrayOf.Z = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.Z, "Z");
ScalaJS.asArrayOf.C = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.C, "C");
ScalaJS.asArrayOf.B = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.B, "B");
ScalaJS.asArrayOf.S = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.S, "S");
ScalaJS.asArrayOf.I = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.I, "I");
ScalaJS.asArrayOf.J = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.J, "J");
ScalaJS.asArrayOf.F = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.F, "F");
ScalaJS.asArrayOf.D = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.D, "D");


// Polyfills

ScalaJS.imul = ScalaJS.g["Math"]["imul"] || (function(a, b) {
  // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
  var ah = (a >>> 16) & 0xffff;
  var al = a & 0xffff;
  var bh = (b >>> 16) & 0xffff;
  var bl = b & 0xffff;
  // the shift by 0 fixes the sign on the high part
  // the final |0 converts the unsigned value into a signed value
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0);
});

ScalaJS.fround = ScalaJS.g["Math"]["fround"] ||









  (function(v) {
    return +v;
  });

ScalaJS.is.Ljava_io_Closeable = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Ljava_io_Closeable)))
});
ScalaJS.as.Ljava_io_Closeable = (function(obj) {
  return ((ScalaJS.is.Ljava_io_Closeable(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.io.Closeable"))
});
ScalaJS.isArrayOf.Ljava_io_Closeable = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Ljava_io_Closeable)))
});
ScalaJS.asArrayOf.Ljava_io_Closeable = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Ljava_io_Closeable(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.io.Closeable;", depth))
});
/** @constructor */
ScalaJS.c.O = (function() {
  /*<skip>*/
});
/** @constructor */
ScalaJS.h.O = (function() {
  /*<skip>*/
});
ScalaJS.h.O.prototype = ScalaJS.c.O.prototype;
ScalaJS.c.O.prototype.init___ = (function() {
  return this
});
ScalaJS.c.O.prototype.equals__O__Z = (function(that) {
  return (this === that)
});
ScalaJS.c.O.prototype.toString__T = (function() {
  var jsx$2 = ScalaJS.objectGetClass(this).getName__T();
  var i = this.hashCode__I();
  var x = ScalaJS.uD((i >>> 0));
  var jsx$1 = x["toString"](16);
  return ((jsx$2 + "@") + ScalaJS.as.T(jsx$1))
});
ScalaJS.c.O.prototype.hashCode__I = (function() {
  return ScalaJS.systemIdentityHashCode(this)
});
ScalaJS.c.O.prototype["toString"] = (function() {
  return this.toString__T()
});
ScalaJS.is.O = (function(obj) {
  return (obj !== null)
});
ScalaJS.as.O = (function(obj) {
  return obj
});
ScalaJS.isArrayOf.O = (function(obj, depth) {
  var data = (obj && obj.$classData);
  if ((!data)) {
    return false
  } else {
    var arrayDepth = (data.arrayDepth || 0);
    return ((!(arrayDepth < depth)) && ((arrayDepth > depth) || (!data.arrayBase["isPrimitive"])))
  }
});
ScalaJS.asArrayOf.O = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.O(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Object;", depth))
});
ScalaJS.d.O = new ScalaJS.ClassTypeData({
  O: 0
}, false, "java.lang.Object", {
  O: 1
}, (void 0), ScalaJS.is.O, ScalaJS.isArrayOf.O);
ScalaJS.c.O.prototype.$classData = ScalaJS.d.O;
ScalaJS.is.jl_CharSequence = (function(obj) {
  return (!(!(((obj && obj.$classData) && obj.$classData.ancestors.jl_CharSequence) || ((typeof obj) === "string"))))
});
ScalaJS.as.jl_CharSequence = (function(obj) {
  return ((ScalaJS.is.jl_CharSequence(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.lang.CharSequence"))
});
ScalaJS.isArrayOf.jl_CharSequence = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_CharSequence)))
});
ScalaJS.asArrayOf.jl_CharSequence = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_CharSequence(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.CharSequence;", depth))
});
ScalaJS.is.ju_Formattable = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.ju_Formattable)))
});
ScalaJS.as.ju_Formattable = (function(obj) {
  return ((ScalaJS.is.ju_Formattable(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.util.Formattable"))
});
ScalaJS.isArrayOf.ju_Formattable = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.ju_Formattable)))
});
ScalaJS.asArrayOf.ju_Formattable = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.ju_Formattable(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.util.Formattable;", depth))
});
ScalaJS.is.sc_GenTraversableOnce = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_GenTraversableOnce)))
});
ScalaJS.as.sc_GenTraversableOnce = (function(obj) {
  return ((ScalaJS.is.sc_GenTraversableOnce(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.GenTraversableOnce"))
});
ScalaJS.isArrayOf.sc_GenTraversableOnce = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_GenTraversableOnce)))
});
ScalaJS.asArrayOf.sc_GenTraversableOnce = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_GenTraversableOnce(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.GenTraversableOnce;", depth))
});
ScalaJS.is.scm_HashEntry = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_HashEntry)))
});
ScalaJS.as.scm_HashEntry = (function(obj) {
  return ((ScalaJS.is.scm_HashEntry(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.HashEntry"))
});
ScalaJS.isArrayOf.scm_HashEntry = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_HashEntry)))
});
ScalaJS.asArrayOf.scm_HashEntry = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_HashEntry(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.HashEntry;", depth))
});
ScalaJS.d.scm_HashEntry = new ScalaJS.ClassTypeData({
  scm_HashEntry: 0
}, true, "scala.collection.mutable.HashEntry", {
  scm_HashEntry: 1
});
/** @constructor */
ScalaJS.c.Lenemies_Gun = (function() {
  ScalaJS.c.O.call(this);
  this.firingSpeed$1 = 0;
  this.damage$1 = 0;
  this.range$1 = 0;
  this.ammoPerShot$1 = 0;
  this.owner$1 = null;
  this.shotCountdown$1 = 0;
  this.ammo$1 = 0
});
ScalaJS.c.Lenemies_Gun.prototype = new ScalaJS.h.O();
ScalaJS.c.Lenemies_Gun.prototype.constructor = ScalaJS.c.Lenemies_Gun;
/** @constructor */
ScalaJS.h.Lenemies_Gun = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Gun.prototype = ScalaJS.c.Lenemies_Gun.prototype;
ScalaJS.c.Lenemies_Gun.prototype.shoot__Lobjects_Actor__Lgame_Game__V = (function(target, g) {
  if ((this.ammo$1 >= this.ammoPerShot$1)) {
    this.ammo$1 = ((this.ammo$1 - this.ammoPerShot$1) | 0);
    target.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(this.owner$1, this.damage$1, 1.0, g);
    this.shotCountdown$1 = this.firingSpeed$1;
    g.linesToDraw$1.$$plus$eq__O__scm_Buffer(new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(this.owner$1.center__Lglobalvars_Pt(), target.center__Lglobalvars_Pt(), "black"))
  }
});
ScalaJS.c.Lenemies_Gun.prototype.canShoot__Z = (function() {
  return ((this.shotCountdown$1 === 0) && (this.ammo$1 >= this.ammoPerShot$1))
});
ScalaJS.c.Lenemies_Gun.prototype.init___I__I__I__I__Lobjects_Actor = (function(firingSpeed_, damage_, range_, ammoPerShot_, owner_) {
  this.firingSpeed$1 = firingSpeed_;
  this.damage$1 = damage_;
  this.range$1 = range_;
  this.ammoPerShot$1 = ammoPerShot_;
  this.owner$1 = owner_;
  this.shotCountdown$1 = 0;
  this.ammo$1 = ScalaJS.m.Lglobalvars_GV$().BASE$undAMMO$1;
  return this
});
ScalaJS.c.Lenemies_Gun.prototype.tick__V = (function() {
  if ((this.shotCountdown$1 > 0)) {
    this.shotCountdown$1 = (((-1) + this.shotCountdown$1) | 0)
  }
});
ScalaJS.d.Lenemies_Gun = new ScalaJS.ClassTypeData({
  Lenemies_Gun: 0
}, false, "enemies.Gun", {
  Lenemies_Gun: 1,
  O: 1
});
ScalaJS.c.Lenemies_Gun.prototype.$classData = ScalaJS.d.Lenemies_Gun;
/** @constructor */
ScalaJS.c.Lexample_ScalaJSExample$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.Lexample_ScalaJSExample$.prototype = new ScalaJS.h.O();
ScalaJS.c.Lexample_ScalaJSExample$.prototype.constructor = ScalaJS.c.Lexample_ScalaJSExample$;
/** @constructor */
ScalaJS.h.Lexample_ScalaJSExample$ = (function() {
  /*<skip>*/
});
ScalaJS.h.Lexample_ScalaJSExample$.prototype = ScalaJS.c.Lexample_ScalaJSExample$.prototype;
ScalaJS.c.Lexample_ScalaJSExample$.prototype.main__Lorg_scalajs_dom_raw_HTMLCanvasElement__V = (function(canvas) {
  ScalaJS.g["console"]["log"]("butts butts butts");
  var keysDown = ScalaJS.as.scm_Set(ScalaJS.m.scm_Set$().apply__sc_Seq__sc_GenTraversable(ScalaJS.m.sci_Nil$()));
  var ctx = canvas["getContext"]("2d");
  var g = new ScalaJS.c.Lgame_Game().init___I__I__Lorg_scalajs_dom_raw_CanvasRenderingContext2D(ScalaJS.m.Lglobalvars_GV$().GAMEX$1, ScalaJS.m.Lglobalvars_GV$().GAMEY$1, ctx);
  g.genMap__V();
  new ScalaJS.c.Lenemies_Spitter().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D((((-10) + ((ScalaJS.m.Lglobalvars_GV$().GAMEX$1 / 2) | 0)) | 0), (((-50) + ScalaJS.m.Lglobalvars_GV$().GAMEY$1) | 0)));
  var puddle = new ScalaJS.c.Lobjects_FirePatch().init___Lglobalvars_Pt__I__I(new ScalaJS.c.Lglobalvars_Pt().init___D__D(100.0, 100.0), 100, 5000);
  g.addActor__Lobjects_Actor__scm_Set(puddle);
  new ScalaJS.c.Lenemies_Human().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D((50 + g.player$1.loc$1.x$1), g.player$1.loc$1.y$1));
  var thegun = new ScalaJS.c.Lenemies_Gun().init___I__I__I__I__Lobjects_Actor(ScalaJS.m.Lglobalvars_GV$().SNIPER$undFIRETIME$1, ScalaJS.m.Lglobalvars_GV$().SNIPER$undDAMAGE$1, ScalaJS.m.Lglobalvars_GV$().SNIPER$undRANGE$1, ScalaJS.m.Lglobalvars_GV$().SNIPER$undAPS$1, null);
  var grounded = new ScalaJS.c.Lobjects_GroundGun().init___Lglobalvars_Pt__Lenemies_Gun__T(new ScalaJS.c.Lglobalvars_Pt().init___D__D((50 + g.player$1.loc$1.x$1), g.player$1.loc$1.y$1), thegun, "ak47");
  g.addActor__Lobjects_Actor__scm_Set(grounded);
  new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(200.0, 400.0), new ScalaJS.c.Lglobalvars_Pt().init___D__D(200.0, 10.0));
  var zedSpawner = new ScalaJS.c.Lobjects_ZombieSpawner().init___Lglobalvars_Pt__I(new ScalaJS.c.Lglobalvars_Pt().init___D__D(((((ScalaJS.m.Lglobalvars_GV$().GAMEX$1 / 2) | 0) - ((ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1 / 2) | 0)) | 0), ScalaJS.m.Lglobalvars_GV$().GAMEY$1), 200);
  g.addActor__Lobjects_Actor__scm_Set(zedSpawner);
  this.clear$1__p1__Lorg_scalajs_dom_raw_CanvasRenderingContext2D__V(ctx);
  ScalaJS.g["onkeydown"] = (function(keysDown$1) {
    return (function(e$2) {
      var elem = ScalaJS.uI(e$2["keyCode"]);
      return ScalaJS.s.scm_FlatHashTable$class__addElem__scm_FlatHashTable__O__Z(keysDown$1, elem)
    })
  })(keysDown);
  ScalaJS.g["onkeyup"] = (function(keysDown$1$1) {
    return (function(e$2$1) {
      var elem$1 = ScalaJS.uI(e$2$1["keyCode"]);
      return ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(keysDown$1$1, elem$1)
    })
  })(keysDown);
  ScalaJS.g["setInterval"]((function(keysDown$1$2, ctx$1, g$1) {
    return (function() {
      ScalaJS.m.Lexample_ScalaJSExample$().example$ScalaJSExample$$run$1__scm_Set__Lorg_scalajs_dom_raw_CanvasRenderingContext2D__Lgame_Game__V(keysDown$1$2, ctx$1, g$1)
    })
  })(keysDown, ctx, g), 25.0)
});
ScalaJS.c.Lexample_ScalaJSExample$.prototype.$$js$exported$meth$main__Lorg_scalajs_dom_raw_HTMLCanvasElement__O = (function(canvas) {
  this.main__Lorg_scalajs_dom_raw_HTMLCanvasElement__V(canvas)
});
ScalaJS.c.Lexample_ScalaJSExample$.prototype.example$ScalaJSExample$$run$1__scm_Set__Lorg_scalajs_dom_raw_CanvasRenderingContext2D__Lgame_Game__V = (function(keysDown$1, ctx$1, g$1) {
  if ((g$1.player$1.hp$2 <= 0)) {
    ctx$1["font"] = "75px sans-serif";
    ctx$1["fillStyle"] = "black";
    ctx$1["fillText"]("It's over!", ((ScalaJS.m.Lglobalvars_GV$().GAMEX$1 / 2) | 0), ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 / 2) | 0))
  } else {
    if ((g$1.player$1.loc$1.y$1 < 0)) {
      g$1.score$1 = ((g$1.score$1 + ScalaJS.imul(10, g$1.difficulty$1)) | 0);
      ScalaJS.g["console"]["log"]("Generating new map");
      var this$3 = g$1.acts$1;
      var this$4 = new ScalaJS.c.scm_HashSet().init___();
      var oldActs = ScalaJS.as.scm_HashSet(ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this$4, this$3));
      g$1.genMap__V();
      var jsx$1 = ScalaJS.g["console"];
      var s = (("Copying " + oldActs.tableSize$5) + " actors");
      jsx$1["log"](s);
      g$1.delayedActs$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(g$1$1) {
        return (function(delAct$2) {
          var delAct = ScalaJS.as.Lobjects_DelayedActor(delAct$2);
          delAct.time$3 = ((delAct.time$3 + ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 / delAct.speed$2) | 0)) | 0);
          if ((delAct.time$3 > ScalaJS.m.Lglobalvars_GV$().OFFMAPCUTOFF$1)) {
            var this$6 = g$1$1.delayedActs$1;
            return ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(this$6, delAct)
          } else {
            return (void 0)
          }
        })
      })(g$1)));
      var i = 0;
      var len = oldActs.table$5.u["length"];
      while ((i < len)) {
        var curEntry = oldActs.table$5.u[i];
        if ((curEntry !== null)) {
          var arg1 = ScalaJS.s.scm_FlatHashTable$HashUtils$class__entryToElem__scm_FlatHashTable$HashUtils__O__O(oldActs, curEntry);
          var a = ScalaJS.as.Lobjects_Actor(arg1);
          if ((!ScalaJS.is.Lobjects_ProjectileActor(a))) {
            var x$2 = g$1.player$1;
            if ((!(a === x$2))) {
              var time = ((a.loc$1.y$1 / a.speed$2) | 0);
              a.loc$1.y$1 = ScalaJS.m.Lglobalvars_GV$().GAMEY$1;
              a.moveToNewMap__Lgame_Game__V(g$1);
              var delayedAct = new ScalaJS.c.Lobjects_DelayedActor().init___Lobjects_Actor__I(a, time);
              var this$7 = g$1.delayedActs$1;
              this$7.$$plus$eq__O__scm_HashSet(delayedAct)
            }
          }
        };
        i = ((1 + i) | 0)
      }
    };
    this.handlePlayerMovement__Lenemies_Player__scm_Set__Lgame_Game__O(g$1.player$1, keysDown$1, g$1);
    g$1.runAllAIs__V();
    this.clear$1__p1__Lorg_scalajs_dom_raw_CanvasRenderingContext2D__V(ctx$1);
    g$1.drawAll__V();
    var s$1 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(200, 200, 200)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
    ctx$1["fillStyle"] = s$1;
    ctx$1["fillRect"](ScalaJS.m.Lglobalvars_GV$().GAMEX$1, 0.0, ((ScalaJS.m.Lglobalvars_GV$().FULLX$1 - ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0), ScalaJS.m.Lglobalvars_GV$().FULLY$1);
    var elem$1 = 0;
    elem$1 = 0;
    var s$2 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(200, 0, 0)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
    ctx$1["fillStyle"] = s$2;
    var jsx$3 = ScalaJS.m.Lglobalvars_GV$().GAMEX$1;
    var jsx$2 = elem$1;
    var x = ((((ScalaJS.m.Lglobalvars_GV$().FULLX$1 - ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0) * g$1.player$1.hp$2) / g$1.player$1.maxHp$2);
    ctx$1["fillRect"](jsx$3, jsx$2, ((x > 0.0) ? x : 0.0), 80.0);
    ctx$1["fillStyle"] = "black";
    ctx$1["font"] = "12px sans-serif";
    ctx$1["fillText"]("health", ScalaJS.m.Lglobalvars_GV$().GAMEX$1, ((10 + elem$1) | 0));
    elem$1 = ((80 + elem$1) | 0);
    var s$3 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(0, 0, 200)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
    ctx$1["fillStyle"] = s$3;
    var jsx$5 = ScalaJS.m.Lglobalvars_GV$().GAMEX$1;
    var jsx$4 = elem$1;
    var x$1 = ((g$1.player$1.gun$3.ammo$1 / 10) | 0);
    var y = ((ScalaJS.m.Lglobalvars_GV$().FULLX$1 - ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0);
    ctx$1["fillRect"](jsx$5, jsx$4, ((x$1 < y) ? x$1 : y), 80.0);
    ctx$1["fillStyle"] = "black";
    ctx$1["font"] = "12px sans-serif";
    ctx$1["fillText"]("ammo", ScalaJS.m.Lglobalvars_GV$().GAMEX$1, ((10 + elem$1) | 0));
    elem$1 = ((80 + elem$1) | 0);
    ctx$1["fillStyle"] = "black";
    ctx$1["font"] = "12px sans-serif";
    ctx$1["fillText"]("score", ScalaJS.m.Lglobalvars_GV$().GAMEX$1, ((10 + elem$1) | 0));
    ctx$1["font"] = "50px sans-serif";
    var this$19 = g$1.score$1;
    ctx$1["fillText"](("" + this$19), ScalaJS.m.Lglobalvars_GV$().GAMEX$1, ((50 + elem$1) | 0));
    elem$1 = ((80 + elem$1) | 0);
    ctx$1["font"] = "12px sans-serif";
    var this$21 = g$1.player$1.usableItems$4;
    var this$22 = this$21.elems$5;
    var this$23 = new ScalaJS.c.sc_LinearSeqLike$$anon$1().init___sc_LinearSeqLike(this$22);
    while (this$23.hasNext__Z()) {
      var arg1$1 = this$23.next__O();
      var item = ScalaJS.as.Lobjects_UsableItem(arg1$1);
      var img = g$1.images$1.apply__O__O(item.displayName$1);
      g$1.ctx$1["drawImage"](img, ((5 + ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0), elem$1, ScalaJS.imul(2, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), ScalaJS.imul(2, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1));
      ctx$1["fillText"](item.name$1, ((30 + ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0), ((12 + elem$1) | 0));
      elem$1 = ((elem$1 + ((5 + ScalaJS.imul(2, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1)) | 0)) | 0)
    }
  }
});
ScalaJS.c.Lexample_ScalaJSExample$.prototype.clear$1__p1__Lorg_scalajs_dom_raw_CanvasRenderingContext2D__V = (function(ctx$1) {
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(200, 200, 200)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  ctx$1["fillStyle"] = s;
  ctx$1["fillRect"](0.0, 0.0, ScalaJS.m.Lglobalvars_GV$().FULLX$1, ScalaJS.m.Lglobalvars_GV$().FULLY$1)
});
ScalaJS.c.Lexample_ScalaJSExample$.prototype.handlePlayerMovement__Lenemies_Player__scm_Set__Lgame_Game__O = (function(player, keys, g) {
  var dx = 0;
  var dy = 0;
  if (ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z(keys, 38)) {
    dy = (((-2) + dy) | 0)
  };
  if (ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z(keys, 37)) {
    dx = (((-2) + dx) | 0)
  };
  if (ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z(keys, 39)) {
    dx = ((2 + dx) | 0)
  };
  if (ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z(keys, 40)) {
    dy = ((2 + dy) | 0)
  };
  player.moveLoc__D__D__Lgame_Game__Z(dx, dy, g);
  return (ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z(keys, 32) ? (player.useItem__Lgame_Game__V(g), ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(keys, 32)) : (void 0))
});
ScalaJS.c.Lexample_ScalaJSExample$.prototype["main"] = (function(arg$1) {
  var preparg$1 = arg$1;
  return this.$$js$exported$meth$main__Lorg_scalajs_dom_raw_HTMLCanvasElement__O(preparg$1)
});
ScalaJS.d.Lexample_ScalaJSExample$ = new ScalaJS.ClassTypeData({
  Lexample_ScalaJSExample$: 0
}, false, "example.ScalaJSExample$", {
  Lexample_ScalaJSExample$: 1,
  O: 1
});
ScalaJS.c.Lexample_ScalaJSExample$.prototype.$classData = ScalaJS.d.Lexample_ScalaJSExample$;
ScalaJS.n.Lexample_ScalaJSExample$ = (void 0);
ScalaJS.m.Lexample_ScalaJSExample$ = (function() {
  if ((!ScalaJS.n.Lexample_ScalaJSExample$)) {
    ScalaJS.n.Lexample_ScalaJSExample$ = new ScalaJS.c.Lexample_ScalaJSExample$().init___()
  };
  return ScalaJS.n.Lexample_ScalaJSExample$
});
ScalaJS.e["example"] = (ScalaJS.e["example"] || {});
ScalaJS.e["example"]["ScalaJSExample"] = ScalaJS.m.Lexample_ScalaJSExample$;
/** @constructor */
ScalaJS.c.Lgame_Game = (function() {
  ScalaJS.c.O.call(this);
  this.difficulty$1 = 0;
  this.score$1 = 0;
  this.r$1 = null;
  this.mapSizeX$1 = 0;
  this.mapSizeY$1 = 0;
  this.ctx$1 = null;
  this.linesToDraw$1 = null;
  this.images$1 = null;
  this.objs$1 = null;
  this.acts$1 = null;
  this.delayedActs$1 = null;
  this.headTexts$1 = null;
  this.player$1 = null
});
ScalaJS.c.Lgame_Game.prototype = new ScalaJS.h.O();
ScalaJS.c.Lgame_Game.prototype.constructor = ScalaJS.c.Lgame_Game;
/** @constructor */
ScalaJS.h.Lgame_Game = (function() {
  /*<skip>*/
});
ScalaJS.h.Lgame_Game.prototype = ScalaJS.c.Lgame_Game.prototype;
ScalaJS.c.Lgame_Game.prototype.spawnItemFromDummy__Lobjects_DummyItem__scm_Set = (function(d) {
  var ammopack = new ScalaJS.c.Lobjects_MediumAmmoPack().init___Lglobalvars_Pt(d.loc$1);
  var healthpack = new ScalaJS.c.Lobjects_MediumHealthPack().init___Lglobalvars_Pt(d.loc$1);
  var mine = new ScalaJS.c.Lobjects_GroundUsableItem().init___Lglobalvars_Pt__Lobjects_UsableItem__T(d.loc$1, new ScalaJS.c.Lobjects_UsableLandMine().init___Lobjects_Actor(null), "item_landmine");
  var spit = new ScalaJS.c.Lobjects_GroundUsableItem().init___Lglobalvars_Pt__Lobjects_UsableItem__T(d.loc$1, new ScalaJS.c.Lobjects_UsableSpitterAcid().init___Lobjects_Actor(null), "item_acid");
  var pipe = new ScalaJS.c.Lobjects_GroundUsableItem().init___Lglobalvars_Pt__Lobjects_UsableItem__T(d.loc$1, new ScalaJS.c.Lobjects_UsablePipeBomb().init___Lobjects_Actor(null), "item_pipebomb");
  var grenade = new ScalaJS.c.Lobjects_GroundUsableItem().init___Lglobalvars_Pt__Lobjects_UsableItem__T(d.loc$1, new ScalaJS.c.Lobjects_UsableGrenade().init___Lobjects_Actor(null), "item_pipebomb");
  ScalaJS.m.sc_Seq$();
  var n = ScalaJS.m.Lglobalvars_GV$().AMMO$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b = new ScalaJS.c.scm_ListBuffer().init___();
  var i = 0;
  while ((i < n)) {
    b.$$plus$eq__O__scm_ListBuffer(ammopack);
    i = ((1 + i) | 0)
  };
  var ammopackL = b.toList__sci_List();
  ScalaJS.m.sc_Seq$();
  var n$1 = ScalaJS.m.Lglobalvars_GV$().HEALTH$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b$1 = new ScalaJS.c.scm_ListBuffer().init___();
  var i$1 = 0;
  while ((i$1 < n$1)) {
    b$1.$$plus$eq__O__scm_ListBuffer(healthpack);
    i$1 = ((1 + i$1) | 0)
  };
  var healthpackL = b$1.toList__sci_List();
  ScalaJS.m.sc_Seq$();
  var n$2 = ScalaJS.m.Lglobalvars_GV$().LANDMINE$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b$2 = new ScalaJS.c.scm_ListBuffer().init___();
  var i$2 = 0;
  while ((i$2 < n$2)) {
    b$2.$$plus$eq__O__scm_ListBuffer(mine);
    i$2 = ((1 + i$2) | 0)
  };
  var mineL = b$2.toList__sci_List();
  ScalaJS.m.sc_Seq$();
  var n$3 = ScalaJS.m.Lglobalvars_GV$().SPITTERACID$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b$3 = new ScalaJS.c.scm_ListBuffer().init___();
  var i$3 = 0;
  while ((i$3 < n$3)) {
    b$3.$$plus$eq__O__scm_ListBuffer(spit);
    i$3 = ((1 + i$3) | 0)
  };
  var spitL = b$3.toList__sci_List();
  ScalaJS.m.sc_Seq$();
  var n$4 = ScalaJS.m.Lglobalvars_GV$().PIPEBOMB$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b$4 = new ScalaJS.c.scm_ListBuffer().init___();
  var i$4 = 0;
  while ((i$4 < n$4)) {
    b$4.$$plus$eq__O__scm_ListBuffer(pipe);
    i$4 = ((1 + i$4) | 0)
  };
  var pipeL = b$4.toList__sci_List();
  ScalaJS.m.sc_Seq$();
  var n$5 = ScalaJS.m.Lglobalvars_GV$().GRENADE$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b$5 = new ScalaJS.c.scm_ListBuffer().init___();
  var i$5 = 0;
  while ((i$5 < n$5)) {
    b$5.$$plus$eq__O__scm_ListBuffer(grenade);
    i$5 = ((1 + i$5) | 0)
  };
  var grenadeL = b$5.toList__sci_List();
  var this$13 = ScalaJS.m.sc_Seq$();
  var jsx$4 = ScalaJS.as.sc_TraversableLike(ammopackL.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(healthpackL, this$13.ReusableCBFInstance$2));
  var this$14 = ScalaJS.m.sc_Seq$();
  var jsx$3 = ScalaJS.as.sc_TraversableLike(jsx$4.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(mineL, this$14.ReusableCBFInstance$2));
  var this$15 = ScalaJS.m.sc_Seq$();
  var jsx$2 = ScalaJS.as.sc_TraversableLike(jsx$3.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(spitL, this$15.ReusableCBFInstance$2));
  var this$16 = ScalaJS.m.sc_Seq$();
  var jsx$1 = ScalaJS.as.sc_TraversableLike(jsx$2.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(pipeL, this$16.ReusableCBFInstance$2));
  var this$17 = ScalaJS.m.sc_Seq$();
  var choices = ScalaJS.as.sc_Seq(jsx$1.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(grenadeL, this$17.ReusableCBFInstance$2));
  var this$18 = this.r$1;
  var n$6 = choices.length__I();
  var choice = ScalaJS.as.Lobjects_Item(choices.apply__I__O(this$18.self$1.nextInt__I__I(n$6)));
  return this.addActor__Lobjects_Actor__scm_Set(choice)
});
ScalaJS.c.Lgame_Game.prototype.spawnMainWalls__scm_Set = (function() {
  var leftWall = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0), new ScalaJS.c.Lglobalvars_Pt().init___D__D(5.0, ScalaJS.m.Lglobalvars_GV$().GAMEY$1));
  var rightWall = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D((((-5) + ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0), 0.0), new ScalaJS.c.Lglobalvars_Pt().init___D__D(5.0, ScalaJS.m.Lglobalvars_GV$().GAMEY$1));
  var this$1 = this.objs$1;
  this$1.$$plus$eq__O__scm_HashSet(leftWall);
  var this$2 = this.objs$1;
  this$2.$$plus$eq__O__scm_HashSet(rightWall);
  var topTopBound = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, (-6.0)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().GAMEX$1, 4.0));
  var topLeftBound = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, (-6.0)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(5.0, 6.0));
  var topRightBound = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D((((-5) + ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0), (-6.0)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(5.0, 6.0));
  var botBotBound = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, ((20 + ScalaJS.m.Lglobalvars_GV$().GAMEY$1) | 0)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().GAMEX$1, 5.0));
  var botLeftBound = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, ScalaJS.m.Lglobalvars_GV$().GAMEY$1), new ScalaJS.c.Lglobalvars_Pt().init___D__D(5.0, 20.0));
  var botRightBound = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D((((-5) + ScalaJS.m.Lglobalvars_GV$().GAMEX$1) | 0), ScalaJS.m.Lglobalvars_GV$().GAMEY$1), new ScalaJS.c.Lglobalvars_Pt().init___D__D(5.0, 20.0));
  var this$3 = this.objs$1;
  this$3.$$plus$eq__O__scm_HashSet(topTopBound);
  var this$4 = this.objs$1;
  this$4.$$plus$eq__O__scm_HashSet(topLeftBound);
  var this$5 = this.objs$1;
  this$5.$$plus$eq__O__scm_HashSet(topRightBound);
  var this$6 = this.objs$1;
  this$6.$$plus$eq__O__scm_HashSet(botBotBound);
  var this$7 = this.objs$1;
  this$7.$$plus$eq__O__scm_HashSet(botLeftBound);
  var this$8 = this.objs$1;
  return this$8.$$plus$eq__O__scm_HashSet(botRightBound)
});
ScalaJS.c.Lgame_Game.prototype.loadAllImages__scm_HashMap = (function() {
  var img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("ak47.png");
  var this$3 = this.images$1;
  var y = img;
  var e = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$3, "ak47", y));
  if ((e !== null)) {
    e.value$1 = y
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("item_landmine.png");
  var this$6 = this.images$1;
  var y$1 = img;
  var e$1 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$6, "item_landmine", y$1));
  if ((e$1 !== null)) {
    e$1.value$1 = y$1
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("item health kit.png");
  var this$9 = this.images$1;
  var y$2 = img;
  var e$2 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$9, "item_healthkit", y$2));
  if ((e$2 !== null)) {
    e$2.value$1 = y$2
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("item ammo box.png");
  var this$12 = this.images$1;
  var y$3 = img;
  var e$3 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$12, "item_ammobox", y$3));
  if ((e$3 !== null)) {
    e$3.value$1 = y$3
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("item molotov.png");
  var this$15 = this.images$1;
  var y$4 = img;
  var e$4 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$15, "item_molotov", y$4));
  if ((e$4 !== null)) {
    e$4.value$1 = y$4
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("item pipe bomb.png");
  var this$18 = this.images$1;
  var y$5 = img;
  var e$5 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$18, "item_pipebomb", y$5));
  if ((e$5 !== null)) {
    e$5.value$1 = y$5
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("item acid.png");
  var this$21 = this.images$1;
  var y$6 = img;
  var e$6 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$21, "item_acid", y$6));
  if ((e$6 !== null)) {
    e$6.value$1 = y$6
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("character 1.png");
  var this$24 = this.images$1;
  var y$7 = img;
  var e$7 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$24, "char_human_1", y$7));
  if ((e$7 !== null)) {
    e$7.value$1 = y$7
  };
  img = this.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement("character 1 big.png");
  var this$27 = this.images$1;
  var y$8 = img;
  var e$8 = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$27, "char_human_1_big", y$8));
  if ((e$8 !== null)) {
    e$8.value$1 = y$8
  };
  return this$27
});
ScalaJS.c.Lgame_Game.prototype.spawnSpecial__O = (function() {
  var this$1 = this.r$1;
  var n = ScalaJS.m.Lglobalvars_GV$().GAMEX$1;
  var x = this$1.self$1.nextInt__I__I(n);
  var this$2 = this.r$1;
  var n$1 = ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 - ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 / 4) | 0)) | 0);
  var y = this$2.self$1.nextInt__I__I(n$1);
  var charger = new ScalaJS.c.Lenemies_Charger().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(x, y));
  var spitter = new ScalaJS.c.Lenemies_Spitter().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(x, y));
  var tank = new ScalaJS.c.Lenemies_Tank().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(x, y));
  ScalaJS.m.sc_Seq$();
  var n$2 = ScalaJS.m.Lglobalvars_GV$().CHARGER$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b = new ScalaJS.c.scm_ListBuffer().init___();
  var i = 0;
  while ((i < n$2)) {
    b.$$plus$eq__O__scm_ListBuffer(charger);
    i = ((1 + i) | 0)
  };
  var chargerL = b.toList__sci_List();
  ScalaJS.m.sc_Seq$();
  var n$3 = ScalaJS.m.Lglobalvars_GV$().SPITTER$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b$1 = new ScalaJS.c.scm_ListBuffer().init___();
  var i$1 = 0;
  while ((i$1 < n$3)) {
    b$1.$$plus$eq__O__scm_ListBuffer(spitter);
    i$1 = ((1 + i$1) | 0)
  };
  var spitterL = b$1.toList__sci_List();
  ScalaJS.m.sc_Seq$();
  var n$4 = ScalaJS.m.Lglobalvars_GV$().TANK$undCHANCE$1;
  ScalaJS.m.sci_Seq$();
  var b$2 = new ScalaJS.c.scm_ListBuffer().init___();
  var i$2 = 0;
  while ((i$2 < n$4)) {
    b$2.$$plus$eq__O__scm_ListBuffer(tank);
    i$2 = ((1 + i$2) | 0)
  };
  var tankL = b$2.toList__sci_List();
  var this$9 = ScalaJS.m.sc_Seq$();
  var jsx$1 = ScalaJS.as.sc_TraversableLike(chargerL.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(spitterL, this$9.ReusableCBFInstance$2));
  var this$10 = ScalaJS.m.sc_Seq$();
  var choices = ScalaJS.as.sc_Seq(jsx$1.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(tankL, this$10.ReusableCBFInstance$2));
  var this$11 = this.r$1;
  var n$5 = choices.length__I();
  var choice = ScalaJS.as.Lobjects_Actor(choices.apply__I__O(this$11.self$1.nextInt__I__I(n$5)));
  return ((!this.collision__Lobjects_Obj__Z(choice)) ? this.addActor__Lobjects_Actor__scm_Set(choice) : (void 0))
});
ScalaJS.c.Lgame_Game.prototype.drawAllCond__F1__V = (function(cond) {
  this.objs$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, cond$1) {
    return (function(o$2) {
      var o = ScalaJS.as.Lobjects_Obj(o$2);
      if (ScalaJS.uZ(cond$1.apply__O__O(o))) {
        o.draw__Lgame_Game__V(arg$outer)
      }
    })
  })(this, cond)))
});
ScalaJS.c.Lgame_Game.prototype.runAllAIs__V = (function() {
  var this$2 = this.acts$1;
  var f = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer) {
    return (function(x$1$2) {
      var x$1 = ScalaJS.as.Lobjects_Actor(x$1$2);
      x$1.handleMomentum__Lgame_Game__V(arg$outer)
    })
  })(this));
  var this$1 = ScalaJS.m.scm_Set$();
  var bf = new ScalaJS.c.scg_GenSetFactory$$anon$1().init___scg_GenSetFactory(this$1);
  ScalaJS.s.sc_TraversableLike$class__map__sc_TraversableLike__F1__scg_CanBuildFrom__O(this$2, f, bf);
  var this$4 = this.acts$1;
  var f$1 = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer$1) {
    return (function(x$2$2) {
      var x$2 = ScalaJS.as.Lobjects_Actor(x$2$2);
      x$2.aiMove__Lgame_Game__V(arg$outer$1)
    })
  })(this));
  var this$3 = ScalaJS.m.scm_Set$();
  var bf$1 = new ScalaJS.c.scg_GenSetFactory$$anon$1().init___scg_GenSetFactory(this$3);
  ScalaJS.s.sc_TraversableLike$class__map__sc_TraversableLike__F1__scg_CanBuildFrom__O(this$4, f$1, bf$1);
  var this$6 = this.delayedActs$1;
  var f$2 = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer$2) {
    return (function(x$3$2) {
      var x$3 = ScalaJS.as.Lobjects_DelayedActor(x$3$2);
      x$3.aiMove__Lgame_Game__V(arg$outer$2)
    })
  })(this));
  var this$5 = ScalaJS.m.scm_Set$();
  var bf$2 = new ScalaJS.c.scg_GenSetFactory$$anon$1().init___scg_GenSetFactory(this$5);
  ScalaJS.s.sc_TraversableLike$class__map__sc_TraversableLike__F1__scg_CanBuildFrom__O(this$6, f$2, bf$2);
  var this$7 = this.headTexts$1;
  var p = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(check$ifrefutable$1$2) {
    var check$ifrefutable$1 = ScalaJS.as.T2(check$ifrefutable$1$2);
    return (check$ifrefutable$1 !== null)
  }));
  new ScalaJS.c.sc_TraversableLike$WithFilter().init___sc_TraversableLike__F1(this$7, p).foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer$3) {
    return (function(x$4$2) {
      var x$4 = ScalaJS.as.T2(x$4$2);
      if ((x$4 !== null)) {
        var o = ScalaJS.as.Lobjects_Obj(x$4.$$und1__O());
        var ht = ScalaJS.as.Lglobalvars_HeadText(x$4.$$und2__O());
        return ((ht.time$1 <= 0) ? arg$outer$3.headTexts$1.remove__O__s_Option(o) : (ht.tick__V(), (void 0)))
      } else {
        throw new ScalaJS.c.s_MatchError().init___O(x$4)
      }
    })
  })(this)))
});
ScalaJS.c.Lgame_Game.prototype.drawShadows__V = (function() {
  this.objs$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer) {
    return (function(o$2) {
      var o = ScalaJS.as.Lobjects_Obj(o$2);
      var x$2 = arg$outer.player$1;
      if (((!(o === x$2)) && o.blocksLos$1)) {
        var p1 = o.loc$1.cloone__Lglobalvars_Pt();
        var p2 = o.upperRight__Lglobalvars_Pt();
        if ((((arg$outer.player$1.center__Lglobalvars_Pt().x$1 > o.loc$1.x$1) && (arg$outer.player$1.center__Lglobalvars_Pt().x$1 <= (o.loc$1.x$1 + o.size$1.x$1))) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 <= o.loc$1.y$1))) {
          p1 = o.loc$1.cloone__Lglobalvars_Pt();
          p2 = o.upperRight__Lglobalvars_Pt()
        } else if ((((arg$outer.player$1.center__Lglobalvars_Pt().x$1 > o.loc$1.x$1) && (arg$outer.player$1.center__Lglobalvars_Pt().x$1 <= (o.loc$1.x$1 + o.size$1.x$1))) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 > o.loc$1.y$1))) {
          p1 = o.lowerLeft__Lglobalvars_Pt();
          p2 = o.lowerRight__Lglobalvars_Pt()
        } else if ((((arg$outer.player$1.center__Lglobalvars_Pt().x$1 > (o.loc$1.x$1 + o.size$1.x$1)) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 > o.loc$1.y$1)) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 <= (o.loc$1.y$1 + o.size$1.y$1)))) {
          p1 = o.upperRight__Lglobalvars_Pt();
          p2 = o.lowerRight__Lglobalvars_Pt()
        } else if ((((arg$outer.player$1.center__Lglobalvars_Pt().x$1 < o.loc$1.x$1) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 > o.loc$1.y$1)) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 <= (o.loc$1.y$1 + o.size$1.y$1)))) {
          p1 = o.loc$1.cloone__Lglobalvars_Pt();
          p2 = o.lowerLeft__Lglobalvars_Pt()
        } else if (((arg$outer.player$1.center__Lglobalvars_Pt().x$1 > (o.loc$1.x$1 + o.size$1.x$1)) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 <= o.loc$1.y$1))) {
          p1 = o.loc$1.cloone__Lglobalvars_Pt();
          p2 = o.lowerRight__Lglobalvars_Pt()
        } else if (((arg$outer.player$1.center__Lglobalvars_Pt().x$1 > (o.loc$1.x$1 + o.size$1.x$1)) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 > (o.loc$1.y$1 + o.size$1.y$1)))) {
          p1 = o.upperRight__Lglobalvars_Pt();
          p2 = o.lowerLeft__Lglobalvars_Pt()
        } else if (((arg$outer.player$1.center__Lglobalvars_Pt().x$1 <= o.loc$1.x$1) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 > (o.loc$1.y$1 + o.size$1.y$1)))) {
          p1 = o.loc$1.cloone__Lglobalvars_Pt();
          p2 = o.lowerRight__Lglobalvars_Pt()
        } else if (((arg$outer.player$1.center__Lglobalvars_Pt().x$1 <= o.loc$1.x$1) && (arg$outer.player$1.center__Lglobalvars_Pt().y$1 <= o.loc$1.y$1))) {
          p1 = o.lowerLeft__Lglobalvars_Pt();
          p2 = o.upperRight__Lglobalvars_Pt()
        };
        arg$outer.drawLineShadow__Lglobalvars_Pt__Lglobalvars_Pt__V(p1, p2)
      }
    })
  })(this)))
});
ScalaJS.c.Lgame_Game.prototype.init___I__I__Lorg_scalajs_dom_raw_CanvasRenderingContext2D = (function(mSizeX, mSizeY, ctx_) {
  this.difficulty$1 = 0;
  this.score$1 = 0;
  this.r$1 = new ScalaJS.c.s_util_Random().init___();
  this.mapSizeX$1 = mSizeX;
  this.mapSizeY$1 = mSizeY;
  this.ctx$1 = ctx_;
  this.linesToDraw$1 = ScalaJS.as.scm_Buffer(ScalaJS.m.scm_Buffer$().apply__sc_Seq__sc_GenTraversable(ScalaJS.m.sci_Nil$()));
  this.images$1 = ScalaJS.as.scm_HashMap(ScalaJS.m.scm_HashMap$().apply__sc_Seq__sc_GenMap(ScalaJS.m.sci_Nil$()));
  this.loadAllImages__scm_HashMap();
  this.objs$1 = ScalaJS.as.scm_Set(ScalaJS.m.scm_Set$().apply__sc_Seq__sc_GenTraversable(ScalaJS.m.sci_Nil$()));
  this.acts$1 = ScalaJS.as.scm_Set(ScalaJS.m.scm_Set$().apply__sc_Seq__sc_GenTraversable(ScalaJS.m.sci_Nil$()));
  this.delayedActs$1 = ScalaJS.as.scm_Set(ScalaJS.m.scm_Set$().apply__sc_Seq__sc_GenTraversable(ScalaJS.m.sci_Nil$()));
  this.headTexts$1 = ScalaJS.as.scm_HashMap(ScalaJS.m.scm_HashMap$().apply__sc_Seq__sc_GenMap(ScalaJS.m.sci_Nil$()));
  this.player$1 = new ScalaJS.c.Lenemies_Player().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(((ScalaJS.m.Lglobalvars_GV$().GAMEX$1 / 2) | 0), ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 / 2) | 0)));
  this.player$1.name$2 = "Tim Jones";
  this.addActor__Lobjects_Actor__scm_Set(this.player$1);
  return this
});
ScalaJS.c.Lgame_Game.prototype.genMap__V = (function() {
  var jsx$1 = ScalaJS.g["console"];
  var s = ("Generating map with difficulty " + this.difficulty$1);
  jsx$1["log"](s);
  var this$2 = this.objs$1;
  ScalaJS.s.scm_FlatHashTable$class__clearTable__scm_FlatHashTable__V(this$2);
  var this$3 = this.acts$1;
  ScalaJS.s.scm_FlatHashTable$class__clearTable__scm_FlatHashTable__V(this$3);
  if ((this.difficulty$1 !== 0)) {
    var this$4 = this.player$1;
    var newLoc = new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.player$1.loc$1.x$1, ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 - ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1) | 0));
    this$4.loc$1 = newLoc.cloone__Lglobalvars_Pt()
  };
  this.addActor__Lobjects_Actor__scm_Set(this.player$1);
  this.spawnMainWalls__scm_Set();
  var i = 1;
  var count = 0;
  while ((i !== 11)) {
    var v1 = i;
    this.genFarmhouse__V();
    count = ((1 + count) | 0);
    i = ((1 + i) | 0)
  };
  this.spawnItemsFromDummies__V();
  var end = ScalaJS.imul(3, this.difficulty$1);
  var isEmpty$4 = (end < 1);
  var numRangeElements$4 = (isEmpty$4 ? 0 : end);
  var lastElement$4 = (isEmpty$4 ? 0 : end);
  var terminalElement$4 = ((1 + lastElement$4) | 0);
  if ((numRangeElements$4 < 0)) {
    ScalaJS.m.sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(1, end, 1, true)
  };
  var i$1 = 1;
  var count$1 = 0;
  while ((i$1 !== terminalElement$4)) {
    var arg1 = i$1;
    this.spawnZombie__O();
    count$1 = ((1 + count$1) | 0);
    i$1 = ((1 + i$1) | 0)
  };
  var end$1 = this.difficulty$1;
  var isEmpty$4$1 = (end$1 < 1);
  var numRangeElements$4$1 = (isEmpty$4$1 ? 0 : end$1);
  var lastElement$4$1 = (isEmpty$4$1 ? 0 : end$1);
  var terminalElement$4$1 = ((1 + lastElement$4$1) | 0);
  if ((numRangeElements$4$1 < 0)) {
    ScalaJS.m.sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(1, end$1, 1, true)
  };
  var i$2 = 1;
  var count$2 = 0;
  while ((i$2 !== terminalElement$4$1)) {
    var arg1$1 = i$2;
    this.spawnSpecial__O();
    count$2 = ((1 + count$2) | 0);
    i$2 = ((1 + i$2) | 0)
  };
  var i$3 = 1;
  var count$3 = 0;
  while ((i$3 !== 2)) {
    var arg1$2 = i$3;
    this.spawnFriendly__O();
    count$3 = ((1 + count$3) | 0);
    i$3 = ((1 + i$3) | 0)
  };
  this.difficulty$1 = ((1 + this.difficulty$1) | 0)
});
ScalaJS.c.Lgame_Game.prototype.addHeadText__Lobjects_Obj__T__I__V = (function(o, text, time) {
  var ht = new ScalaJS.c.Lglobalvars_HeadText().init___T__I(text, time);
  var this$3 = this.headTexts$1;
  var e = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this$3, o, ht));
  if ((e !== null)) {
    e.value$1 = ht
  }
});
ScalaJS.c.Lgame_Game.prototype.genFarmhouse__V = (function() {
  var this$1 = this.r$1;
  var sWidth = ((22 + this$1.self$1.nextInt__I__I(78)) | 0);
  var this$2 = this.r$1;
  var sHeight = ((22 + this$2.self$1.nextInt__I__I(78)) | 0);
  var this$3 = this.r$1;
  var n = ((ScalaJS.m.Lglobalvars_GV$().GAMEX$1 - sWidth) | 0);
  var jsx$1 = this$3.self$1.nextInt__I__I(n);
  var this$4 = this.r$1;
  var n$1 = ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 - sHeight) | 0);
  var starter = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(jsx$1, this$4.self$1.nextInt__I__I(n$1)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(sWidth, sHeight));
  if ((!this.collision__Lobjects_Obj__Z(starter))) {
    ScalaJS.m.scm_Buffer$();
    var array = [new ScalaJS.c.T2().init___O__O(starter, "none")];
    if ((ScalaJS.uI(array["length"]) === 0)) {
      var rooms = new ScalaJS.c.sjs_js_WrappedArray().init___()
    } else {
      var b = new ScalaJS.c.sjs_js_WrappedArray().init___();
      matchEnd4: {
        var i = 0;
        var len = ScalaJS.uI(array["length"]);
        while ((i < len)) {
          var index = i;
          var arg1 = array[index];
          b.array$6["push"](arg1);
          i = ((1 + i) | 0)
        };
        break matchEnd4
      };
      var rooms = b
    };
    var last = new ScalaJS.c.sr_ObjectRef().init___O(starter);
    var lastRelation = new ScalaJS.c.sr_ObjectRef().init___O("");
    var f = new ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1().init___Lgame_Game__I__I__I__scm_Buffer__sr_ObjectRef__sr_ObjectRef(this, 5, 22, 100, rooms, last, lastRelation);
    var i$1 = 1;
    var count = 0;
    while ((i$1 !== 9)) {
      f.apply$mcVI$sp__I__V(i$1);
      count = ((1 + count) | 0);
      i$1 = ((1 + i$1) | 0)
    };
    var end = (((-1) + ScalaJS.uI(rooms.array$6["length"])) | 0);
    var isEmpty$4 = (end < 0);
    var numRangeElements$4 = (isEmpty$4 ? 0 : ((end > 2147483646) ? (-1) : ((1 + end) | 0)));
    var lastElement$4 = (isEmpty$4 ? (-1) : end);
    var terminalElement$4 = ((1 + lastElement$4) | 0);
    var f$1 = new ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2().init___Lgame_Game__I__scm_Buffer(this, 5, rooms);
    if ((numRangeElements$4 < 0)) {
      ScalaJS.m.sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(0, end, 1, true)
    };
    var i$2 = 0;
    var count$1 = 0;
    while ((i$2 !== terminalElement$4)) {
      var v1 = i$2;
      f$1.apply__I__scm_Set(v1);
      count$1 = ((1 + count$1) | 0);
      i$2 = ((1 + i$2) | 0)
    }
  }
});
ScalaJS.c.Lgame_Game.prototype.drawLineShadow__Lglobalvars_Pt__Lglobalvars_Pt__V = (function(p1, p2) {
  var x1 = this.extendLinesOffMap__Lglobalvars_Pt__Lglobalvars_Pt__Lglobalvars_Pt__Lglobalvars_Pt__T2(p1, p1.$$minus__Lglobalvars_Pt__Lglobalvars_Pt(this.player$1.center__Lglobalvars_Pt()), p2, p2.$$minus__Lglobalvars_Pt__Lglobalvars_Pt(this.player$1.center__Lglobalvars_Pt()));
  if ((x1 !== null)) {
    var extp1 = ScalaJS.as.Lglobalvars_Pt(x1.$$und1__O());
    var extp2 = ScalaJS.as.Lglobalvars_Pt(x1.$$und2__O());
    var x$5_$_$$und1$f = extp1;
    var x$5_$_$$und2$f = extp2
  } else {
    var x$5;
    throw new ScalaJS.c.s_MatchError().init___O(x1)
  };
  var extp1$2 = ScalaJS.as.Lglobalvars_Pt(x$5_$_$$und1$f);
  var extp2$2 = ScalaJS.as.Lglobalvars_Pt(x$5_$_$$und2$f);
  this.ctx$1["beginPath"]();
  this.ctx$1["moveTo"](p1.x$1, p1.y$1);
  this.ctx$1["lineTo"](extp1$2.x$1, extp1$2.y$1);
  this.ctx$1["lineTo"](extp2$2.x$1, extp2$2.y$1);
  this.ctx$1["lineTo"](p2.x$1, p2.y$1);
  var jsx$1 = this.ctx$1;
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(100, 100, 100)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  jsx$1["fillStyle"] = s;
  this.ctx$1["fill"]()
});
ScalaJS.c.Lgame_Game.prototype.spawnZombie__O = (function() {
  var this$1 = this.r$1;
  var n = ((ScalaJS.m.Lglobalvars_GV$().GAMEX$1 - ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1) | 0);
  var x = this$1.self$1.nextInt__I__I(n);
  var this$2 = this.r$1;
  var n$1 = ((((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 - ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1) | 0) - ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 / 4) | 0)) | 0);
  var y = this$2.self$1.nextInt__I__I(n$1);
  var randDude = new ScalaJS.c.Lenemies_Zombie().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(x, y));
  return ((!this.collision__Lobjects_Obj__Z(randDude)) ? this.addActor__Lobjects_Actor__scm_Set(randDude) : (void 0))
});
ScalaJS.c.Lgame_Game.prototype.spawnItemsFromDummies__V = (function() {
  var tot = new ScalaJS.c.sr_IntRef().init___I(0);
  var this$3 = this.acts$1;
  var pf = new ScalaJS.c.Lgame_Game$$anonfun$1().init___Lgame_Game(this);
  var this$2 = ScalaJS.m.scm_Set$();
  var bf = new ScalaJS.c.scg_GenSetFactory$$anon$1().init___scg_GenSetFactory(this$2);
  var dummies = ScalaJS.as.scm_Set(ScalaJS.s.sc_TraversableLike$class__collect__sc_TraversableLike__s_PartialFunction__scg_CanBuildFrom__O(this$3, pf, bf));
  dummies.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, tot$1) {
    return (function(d$2) {
      var d = ScalaJS.as.Lobjects_DummyItem(d$2);
      if ((d !== null)) {
        arg$outer.spawnItemFromDummy__Lobjects_DummyItem__scm_Set(d);
        tot$1.elem$1 = ((1 + tot$1.elem$1) | 0)
      }
    })
  })(this, tot)));
  dummies.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer$1) {
    return (function(d$2$1) {
      var d$1 = ScalaJS.as.Lobjects_DummyItem(d$2$1);
      return ((d$1 !== null) ? arg$outer$1.removeActor__Lobjects_Actor__s_Option(d$1) : (void 0))
    })
  })(this)))
});
ScalaJS.c.Lgame_Game.prototype.addActor__Lobjects_Actor__scm_Set = (function(newAct) {
  var this$1 = this.acts$1;
  this$1.$$plus$eq__O__scm_HashSet(newAct);
  var this$2 = this.objs$1;
  return this$2.$$plus$eq__O__scm_HashSet(newAct)
});
ScalaJS.c.Lgame_Game.prototype.drawHeadTexts__V = (function() {
  this.ctx$1["fillStyle"] = "black";
  this.ctx$1["font"] = "12px sans-serif";
  var this$2 = this.headTexts$1;
  var p = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(check$ifrefutable$2$2) {
    var check$ifrefutable$2 = ScalaJS.as.T2(check$ifrefutable$2$2);
    return (check$ifrefutable$2 !== null)
  }));
  new ScalaJS.c.sc_TraversableLike$WithFilter().init___sc_TraversableLike__F1(this$2, p).foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer) {
    return (function(x$6$2) {
      var x$6 = ScalaJS.as.T2(x$6$2);
      if ((x$6 !== null)) {
        var o = ScalaJS.as.Lobjects_Obj(x$6.$$und1__O());
        var ht = ScalaJS.as.Lglobalvars_HeadText(x$6.$$und2__O());
        if (arg$outer.player$1.hasLosTo__Lobjects_Obj__Lgame_Game__Z(o, arg$outer)) {
          var jsx$3 = arg$outer.ctx$1;
          var jsx$2 = ht.text$1;
          var jsx$1 = o.loc$1.x$1;
          var thiz = ht.text$1;
          jsx$3["fillText"](jsx$2, (jsx$1 - ScalaJS.imul(2, ScalaJS.uI(thiz["length"]))), ((-6.0) + o.loc$1.y$1))
        }
      } else {
        throw new ScalaJS.c.s_MatchError().init___O(x$6)
      }
    })
  })(this)))
});
ScalaJS.c.Lgame_Game.prototype.drawAll__V = (function() {
  this.drawAllCond__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(o$2) {
    var o = ScalaJS.as.Lobjects_Obj(o$2);
    return o.lowPriority$1
  })));
  while ((!this.linesToDraw$1.isEmpty__Z())) {
    var line = ScalaJS.as.Lglobalvars_SimpleLine(this.linesToDraw$1.remove__I__O(0));
    var s = line.start$1;
    var e = line.end$1;
    this.ctx$1["beginPath"]();
    this.ctx$1["moveTo"](s.x$1, s.y$1);
    var jsx$1 = this.ctx$1;
    var s$1 = line.color$1;
    jsx$1["fillStyle"] = s$1;
    this.ctx$1["lineTo"](e.x$1, e.y$1);
    this.ctx$1["stroke"]()
  };
  this.drawAllCond__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(o$2$1) {
    var o$1 = ScalaJS.as.Lobjects_Obj(o$2$1);
    return ((!o$1.lowPriority$1) && (!o$1.alwaysVisible$1))
  })));
  this.drawAllCond__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(o$2$2) {
    var o$3 = ScalaJS.as.Lobjects_Obj(o$2$2);
    return o$3.alwaysVisible$1
  })));
  this.drawShadows__V();
  var wally = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0), new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0));
  this.drawAllCond__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(wally$1) {
    return (function(o$2$3) {
      var o$4 = ScalaJS.as.Lobjects_Obj(o$2$3);
      var x = ScalaJS.objectGetClass(o$4);
      var x$2 = ScalaJS.objectGetClass(wally$1);
      return (x === x$2)
    })
  })(wally)));
  this.drawHeadTexts__V()
});
ScalaJS.c.Lgame_Game.prototype.extendLinesOffMap__Lglobalvars_Pt__Lglobalvars_Pt__Lglobalvars_Pt__Lglobalvars_Pt__T2 = (function(p1_, s1, p2_, s2) {
  var dum = new ScalaJS.c.Lobjects_Obj().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0), new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().GAMEX$1, ScalaJS.m.Lglobalvars_GV$().GAMEY$1));
  var p1 = p1_.cloone__Lglobalvars_Pt();
  var p2 = p2_.cloone__Lglobalvars_Pt();
  var lineBetween = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(p1, p2, "black");
  while (dum.collidesLine__Lglobalvars_SimpleLine__Z(lineBetween)) {
    p1 = p1.$$plus__Lglobalvars_Pt__Lglobalvars_Pt(s1);
    p2 = p2.$$plus__Lglobalvars_Pt__Lglobalvars_Pt(s2);
    lineBetween = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(p1, p2, "black")
  };
  return new ScalaJS.c.T2().init___O__O(p1, p2)
});
ScalaJS.c.Lgame_Game.prototype.loadImage__T__Lorg_scalajs_dom_raw_HTMLImageElement = (function(path) {
  var _image = ScalaJS.g["document"]["createElement"]("img");
  _image["src"] = path;
  return _image
});
ScalaJS.c.Lgame_Game.prototype.removeActor__Lobjects_Actor__s_Option = (function(act) {
  var this$1 = this.acts$1;
  ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(this$1, act);
  var this$2 = this.objs$1;
  ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(this$2, act);
  return this.headTexts$1.remove__O__s_Option(act)
});
ScalaJS.c.Lgame_Game.prototype.spawnFriendly__O = (function() {
  var this$1 = this.r$1;
  var n = ((ScalaJS.m.Lglobalvars_GV$().GAMEX$1 - ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1) | 0);
  var x = this$1.self$1.nextInt__I__I(n);
  var this$2 = this.r$1;
  var n$1 = ((ScalaJS.m.Lglobalvars_GV$().GAMEY$1 - ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1) | 0);
  var y = this$2.self$1.nextInt__I__I(n$1);
  var randDude = new ScalaJS.c.Lenemies_Human().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(x, y));
  return ((!this.collision__Lobjects_Obj__Z(randDude)) ? this.addActor__Lobjects_Actor__scm_Set(randDude) : (void 0))
});
ScalaJS.c.Lgame_Game.prototype.collision__Lobjects_Obj__Z = (function(ob) {
  var nonLocalReturnKey1 = new ScalaJS.c.O().init___();
  try {
    this.objs$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(ob$1, nonLocalReturnKey1$1) {
      return (function(o$2) {
        var o = ScalaJS.as.Lobjects_Obj(o$2);
        if ((((o !== ob$1) && o.blocksMovement$1) && o.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(ob$1.loc$1, ob$1.size$1))) {
          throw new ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp().init___O__Z(nonLocalReturnKey1$1, true)
        }
      })
    })(ob, nonLocalReturnKey1)));
    return false
  } catch (e) {
    if (ScalaJS.is.sr_NonLocalReturnControl(e)) {
      var ex = ScalaJS.as.sr_NonLocalReturnControl(e);
      if ((ex.key$2 === nonLocalReturnKey1)) {
        return ex.value$mcZ$sp$f
      } else {
        throw ex
      }
    } else {
      throw e
    }
  }
});
ScalaJS.d.Lgame_Game = new ScalaJS.ClassTypeData({
  Lgame_Game: 0
}, false, "game.Game", {
  Lgame_Game: 1,
  O: 1
});
ScalaJS.c.Lgame_Game.prototype.$classData = ScalaJS.d.Lgame_Game;
/** @constructor */
ScalaJS.c.Lglobalvars_GV$ = (function() {
  ScalaJS.c.O.call(this);
  this.GAMEX$1 = 0;
  this.GAMEY$1 = 0;
  this.FULLX$1 = 0;
  this.FULLY$1 = 0;
  this.OFFMAPCUTOFF$1 = 0;
  this.NORMUNITSIZE$1 = 0;
  this.BIGUNITSIZE$1 = 0;
  this.HUGEUNITSIZE$1 = 0;
  this.NORMMOMENTUMFACTOR$1 = 0.0;
  this.BIGMOMENTUMFACTOR$1 = 0.0;
  this.HUGEMOMENTUMFACTOR$1 = 0;
  this.PLAYER$undHEALTH$1 = 0;
  this.PLAYER$undTHROWDISTANCE$1 = 0;
  this.PLAYER$undTHROWSPEED$1 = 0;
  this.HUMAN$undHEALTH$1 = 0;
  this.HUMAN$undAMMO$1 = 0;
  this.BASE$undAMMO$1 = 0;
  this.ITEM$undCHANCE$1 = 0;
  this.AMMO$undCHANCE$1 = 0;
  this.HEALTH$undCHANCE$1 = 0;
  this.LANDMINE$undCHANCE$1 = 0;
  this.SPITTERACID$undCHANCE$1 = 0;
  this.PIPEBOMB$undCHANCE$1 = 0;
  this.GRENADE$undCHANCE$1 = 0;
  this.PISTOL$undDAMAGE$1 = 0;
  this.PISTOL$undRANGE$1 = 0;
  this.PISTOL$undAPS$1 = 0;
  this.PISTOL$undFIRETIME$1 = 0;
  this.AK47$undDAMAGE$1 = 0;
  this.AK47$undRANGE$1 = 0;
  this.AK47$undAPS$1 = 0;
  this.AK47$undFIRETIME$1 = 0;
  this.SNIPER$undDAMAGE$1 = 0;
  this.SNIPER$undRANGE$1 = 0;
  this.SNIPER$undAPS$1 = 0;
  this.SNIPER$undFIRETIME$1 = 0;
  this.AMMOPACK$undAMOUNT$1 = 0;
  this.HEALTHPACK$undAMOUNT$1 = 0;
  this.LANDMINE$undRADIUS$1 = 0;
  this.LANDMINE$undDAMAGE$1 = 0;
  this.LANDMINE$undDELAY$1 = 0;
  this.PIPEBOMB$undTIME$1 = 0;
  this.PIPEBOMB$undDAMAGE$1 = 0;
  this.PIPEBOMB$undRADIUS$1 = 0;
  this.GRENADE$undDAMAGE$1 = 0;
  this.GRENADE$undRADIUS$1 = 0;
  this.MOLOTOV$undRADIUS$1 = 0;
  this.FIRE$undDAMAGE$1 = 0.0;
  this.CHARGER$undCHANCE$1 = 0;
  this.SPITTER$undCHANCE$1 = 0;
  this.TANK$undCHANCE$1 = 0;
  this.CHARGER$undCHARGECOOLDOWN$1 = 0;
  this.CHARGER$undCHARGERANGE$1 = 0;
  this.CHARGER$undCHARGESPEED$1 = 0;
  this.CHARGER$undCHARGINGMOMENTUMFACTOR$1 = 0.0;
  this.SPITTER$undSPITRATE$1 = 0;
  this.SPITTER$undSPITRADIUS$1 = 0;
  this.SPITTER$undSPITREDUCERATE$1 = 0;
  this.SPITTER$undSPITRANGE$1 = 0;
  this.TANK$undHEALTH$1 = 0;
  this.TANK$undDAMAGE$1 = 0
});
ScalaJS.c.Lglobalvars_GV$.prototype = new ScalaJS.h.O();
ScalaJS.c.Lglobalvars_GV$.prototype.constructor = ScalaJS.c.Lglobalvars_GV$;
/** @constructor */
ScalaJS.h.Lglobalvars_GV$ = (function() {
  /*<skip>*/
});
ScalaJS.h.Lglobalvars_GV$.prototype = ScalaJS.c.Lglobalvars_GV$.prototype;
ScalaJS.c.Lglobalvars_GV$.prototype.init___ = (function() {
  ScalaJS.n.Lglobalvars_GV$ = this;
  this.GAMEX$1 = 600;
  this.GAMEY$1 = 600;
  this.FULLX$1 = 800;
  this.FULLY$1 = 800;
  this.OFFMAPCUTOFF$1 = 600;
  this.NORMUNITSIZE$1 = 10;
  this.BIGUNITSIZE$1 = 14;
  this.HUGEUNITSIZE$1 = 20;
  this.NORMMOMENTUMFACTOR$1 = 0.6;
  this.BIGMOMENTUMFACTOR$1 = 0.4;
  this.HUGEMOMENTUMFACTOR$1 = 0;
  this.PLAYER$undHEALTH$1 = 100;
  this.PLAYER$undTHROWDISTANCE$1 = 50;
  this.PLAYER$undTHROWSPEED$1 = 6;
  this.HUMAN$undHEALTH$1 = 50;
  this.HUMAN$undAMMO$1 = 500;
  this.BASE$undAMMO$1 = 1000;
  this.ITEM$undCHANCE$1 = 1;
  this.AMMO$undCHANCE$1 = 14;
  this.HEALTH$undCHANCE$1 = 6;
  this.LANDMINE$undCHANCE$1 = 5;
  this.SPITTERACID$undCHANCE$1 = 5;
  this.PIPEBOMB$undCHANCE$1 = 5;
  this.GRENADE$undCHANCE$1 = 5;
  this.PISTOL$undDAMAGE$1 = 10;
  this.PISTOL$undRANGE$1 = 100;
  this.PISTOL$undAPS$1 = 10;
  this.PISTOL$undFIRETIME$1 = 20;
  this.AK47$undDAMAGE$1 = 7;
  this.AK47$undRANGE$1 = 200;
  this.AK47$undAPS$1 = 4;
  this.AK47$undFIRETIME$1 = 7;
  this.SNIPER$undDAMAGE$1 = 50;
  this.SNIPER$undRANGE$1 = 600;
  this.SNIPER$undAPS$1 = 20;
  this.SNIPER$undFIRETIME$1 = 50;
  this.AMMOPACK$undAMOUNT$1 = 500;
  this.HEALTHPACK$undAMOUNT$1 = 25;
  this.LANDMINE$undRADIUS$1 = 50;
  this.LANDMINE$undDAMAGE$1 = 70;
  this.LANDMINE$undDELAY$1 = 100;
  this.PIPEBOMB$undTIME$1 = 200;
  this.PIPEBOMB$undDAMAGE$1 = 50;
  this.PIPEBOMB$undRADIUS$1 = 50;
  this.GRENADE$undDAMAGE$1 = 100;
  this.GRENADE$undRADIUS$1 = 50;
  this.MOLOTOV$undRADIUS$1 = 40;
  this.FIRE$undDAMAGE$1 = 0.5;
  this.CHARGER$undCHANCE$1 = 5;
  this.SPITTER$undCHANCE$1 = 5;
  this.TANK$undCHANCE$1 = 1;
  this.CHARGER$undCHARGECOOLDOWN$1 = 300;
  this.CHARGER$undCHARGERANGE$1 = 200;
  this.CHARGER$undCHARGESPEED$1 = 6;
  this.CHARGER$undCHARGINGMOMENTUMFACTOR$1 = 0.2;
  this.SPITTER$undSPITRATE$1 = 400;
  this.SPITTER$undSPITRADIUS$1 = 40;
  this.SPITTER$undSPITREDUCERATE$1 = 4;
  this.SPITTER$undSPITRANGE$1 = 300;
  this.TANK$undHEALTH$1 = 400;
  this.TANK$undDAMAGE$1 = 25;
  return this
});
ScalaJS.d.Lglobalvars_GV$ = new ScalaJS.ClassTypeData({
  Lglobalvars_GV$: 0
}, false, "globalvars.GV$", {
  Lglobalvars_GV$: 1,
  O: 1
});
ScalaJS.c.Lglobalvars_GV$.prototype.$classData = ScalaJS.d.Lglobalvars_GV$;
ScalaJS.n.Lglobalvars_GV$ = (void 0);
ScalaJS.m.Lglobalvars_GV$ = (function() {
  if ((!ScalaJS.n.Lglobalvars_GV$)) {
    ScalaJS.n.Lglobalvars_GV$ = new ScalaJS.c.Lglobalvars_GV$().init___()
  };
  return ScalaJS.n.Lglobalvars_GV$
});
/** @constructor */
ScalaJS.c.Lglobalvars_HeadText = (function() {
  ScalaJS.c.O.call(this);
  this.text$1 = null;
  this.time$1 = 0
});
ScalaJS.c.Lglobalvars_HeadText.prototype = new ScalaJS.h.O();
ScalaJS.c.Lglobalvars_HeadText.prototype.constructor = ScalaJS.c.Lglobalvars_HeadText;
/** @constructor */
ScalaJS.h.Lglobalvars_HeadText = (function() {
  /*<skip>*/
});
ScalaJS.h.Lglobalvars_HeadText.prototype = ScalaJS.c.Lglobalvars_HeadText.prototype;
ScalaJS.c.Lglobalvars_HeadText.prototype.tick__V = (function() {
  this.time$1 = (((-1) + this.time$1) | 0)
});
ScalaJS.c.Lglobalvars_HeadText.prototype.init___T__I = (function(text_, time_) {
  this.text$1 = text_;
  this.time$1 = time_;
  return this
});
ScalaJS.is.Lglobalvars_HeadText = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lglobalvars_HeadText)))
});
ScalaJS.as.Lglobalvars_HeadText = (function(obj) {
  return ((ScalaJS.is.Lglobalvars_HeadText(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "globalvars.HeadText"))
});
ScalaJS.isArrayOf.Lglobalvars_HeadText = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lglobalvars_HeadText)))
});
ScalaJS.asArrayOf.Lglobalvars_HeadText = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lglobalvars_HeadText(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lglobalvars.HeadText;", depth))
});
ScalaJS.d.Lglobalvars_HeadText = new ScalaJS.ClassTypeData({
  Lglobalvars_HeadText: 0
}, false, "globalvars.HeadText", {
  Lglobalvars_HeadText: 1,
  O: 1
});
ScalaJS.c.Lglobalvars_HeadText.prototype.$classData = ScalaJS.d.Lglobalvars_HeadText;
/** @constructor */
ScalaJS.c.Lglobalvars_Pt = (function() {
  ScalaJS.c.O.call(this);
  this.x$1 = 0.0;
  this.y$1 = 0.0
});
ScalaJS.c.Lglobalvars_Pt.prototype = new ScalaJS.h.O();
ScalaJS.c.Lglobalvars_Pt.prototype.constructor = ScalaJS.c.Lglobalvars_Pt;
/** @constructor */
ScalaJS.h.Lglobalvars_Pt = (function() {
  /*<skip>*/
});
ScalaJS.h.Lglobalvars_Pt.prototype = ScalaJS.c.Lglobalvars_Pt.prototype;
ScalaJS.c.Lglobalvars_Pt.prototype.$$plus__Lglobalvars_Pt__Lglobalvars_Pt = (function(p) {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.x$1 + p.x$1), (this.y$1 + p.y$1))
});
ScalaJS.c.Lglobalvars_Pt.prototype.pythagLength__D = (function() {
  var x = ((this.x$1 * this.x$1) + (this.y$1 * this.y$1));
  return ScalaJS.uD(ScalaJS.g["Math"]["sqrt"](x))
});
ScalaJS.c.Lglobalvars_Pt.prototype.$$times__I__Lglobalvars_Pt = (function(n) {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.x$1 * n), (this.y$1 * n))
});
ScalaJS.c.Lglobalvars_Pt.prototype.toString__T = (function() {
  return (((("(" + this.x$1) + ", ") + this.y$1) + ")")
});
ScalaJS.c.Lglobalvars_Pt.prototype.unitStep__Lglobalvars_Pt = (function() {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.x$1 / this.pythagLength__D()), (this.y$1 / this.pythagLength__D()))
});
ScalaJS.c.Lglobalvars_Pt.prototype.init___D__D = (function(x_, y_) {
  this.x$1 = x_;
  this.y$1 = y_;
  return this
});
ScalaJS.c.Lglobalvars_Pt.prototype.is$undzero__Z = (function() {
  return ((this.x$1 === 0.0) && (this.y$1 === 0.0))
});
ScalaJS.c.Lglobalvars_Pt.prototype.$$minus__Lglobalvars_Pt__Lglobalvars_Pt = (function(p) {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.x$1 - p.x$1), (this.y$1 - p.y$1))
});
ScalaJS.c.Lglobalvars_Pt.prototype.cloone__Lglobalvars_Pt = (function() {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.x$1, this.y$1)
});
ScalaJS.c.Lglobalvars_Pt.prototype.is$undneg$undone__Z = (function() {
  return ((this.x$1 === (-1.0)) && (this.y$1 === (-1.0)))
});
ScalaJS.is.Lglobalvars_Pt = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lglobalvars_Pt)))
});
ScalaJS.as.Lglobalvars_Pt = (function(obj) {
  return ((ScalaJS.is.Lglobalvars_Pt(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "globalvars.Pt"))
});
ScalaJS.isArrayOf.Lglobalvars_Pt = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lglobalvars_Pt)))
});
ScalaJS.asArrayOf.Lglobalvars_Pt = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lglobalvars_Pt(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lglobalvars.Pt;", depth))
});
ScalaJS.d.Lglobalvars_Pt = new ScalaJS.ClassTypeData({
  Lglobalvars_Pt: 0
}, false, "globalvars.Pt", {
  Lglobalvars_Pt: 1,
  O: 1
});
ScalaJS.c.Lglobalvars_Pt.prototype.$classData = ScalaJS.d.Lglobalvars_Pt;
/** @constructor */
ScalaJS.c.Lobjects_Obj = (function() {
  ScalaJS.c.O.call(this);
  this.loc$1 = null;
  this.size$1 = null;
  this.blocksMovement$1 = false;
  this.blocksLos$1 = false;
  this.alwaysVisible$1 = false;
  this.lowPriority$1 = false
});
ScalaJS.c.Lobjects_Obj.prototype = new ScalaJS.h.O();
ScalaJS.c.Lobjects_Obj.prototype.constructor = ScalaJS.c.Lobjects_Obj;
/** @constructor */
ScalaJS.h.Lobjects_Obj = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_Obj.prototype = ScalaJS.c.Lobjects_Obj.prototype;
ScalaJS.c.Lobjects_Obj.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt = (function(loc_, size_) {
  this.loc$1 = loc_;
  this.size$1 = size_;
  this.blocksMovement$1 = true;
  this.blocksLos$1 = false;
  this.alwaysVisible$1 = false;
  this.lowPriority$1 = false;
  return this
});
ScalaJS.c.Lobjects_Obj.prototype.lowerRight__Lglobalvars_Pt = (function() {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.loc$1.x$1 + this.size$1.x$1), (this.loc$1.y$1 + this.size$1.y$1))
});
ScalaJS.c.Lobjects_Obj.prototype.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z = (function(lo, s) {
  if (((((this.loc$1.x$1 < (lo.x$1 + s.x$1)) && ((this.loc$1.x$1 + this.size$1.x$1) > lo.x$1)) && (this.loc$1.y$1 < (lo.y$1 + s.y$1))) && ((this.loc$1.y$1 + this.size$1.y$1) > lo.y$1))) {
    return true
  };
  return false
});
ScalaJS.c.Lobjects_Obj.prototype.hasLosTo__Lobjects_Obj__Lgame_Game__Z = (function(o, g) {
  return this.hasCenterLosTo__Lglobalvars_Pt__Lobjects_Obj__Lgame_Game__Z(o.center__Lglobalvars_Pt(), o, g)
});
ScalaJS.c.Lobjects_Obj.prototype.upperRight__Lglobalvars_Pt = (function() {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.loc$1.x$1 + this.size$1.x$1), this.loc$1.y$1)
});
ScalaJS.c.Lobjects_Obj.prototype.collidesLine__Lglobalvars_SimpleLine__Z = (function(l) {
  if ((((((l.start$1.x$1 <= this.loc$1.x$1) && (l.end$1.x$1 <= this.loc$1.x$1)) || ((l.start$1.y$1 <= this.loc$1.y$1) && (l.end$1.y$1 <= this.loc$1.y$1))) || ((l.start$1.x$1 >= (this.loc$1.x$1 + this.size$1.x$1)) && (l.end$1.x$1 >= (this.loc$1.x$1 + this.size$1.x$1)))) || ((l.start$1.y$1 >= (this.loc$1.y$1 + this.size$1.y$1)) && (l.end$1.y$1 >= (this.loc$1.y$1 + this.size$1.y$1))))) {
    return false
  };
  var m = ((l.end$1.y$1 - l.start$1.y$1) / (l.end$1.x$1 - l.start$1.x$1));
  var y1 = ((m * (this.loc$1.x$1 - l.start$1.x$1)) + l.start$1.y$1);
  if (((y1 > this.loc$1.y$1) && (y1 < (this.loc$1.y$1 + this.size$1.y$1)))) {
    return true
  };
  var y2 = ((m * ((this.loc$1.x$1 + this.size$1.x$1) - l.start$1.x$1)) + l.start$1.y$1);
  if (((y2 > this.loc$1.y$1) && (y2 < (this.loc$1.y$1 + this.size$1.y$1)))) {
    return true
  };
  var x1 = (((this.loc$1.y$1 - l.start$1.y$1) / m) + l.start$1.x$1);
  if (((x1 > this.loc$1.x$1) && (x1 < (this.loc$1.x$1 + this.size$1.x$1)))) {
    return true
  };
  var x2 = ((((this.loc$1.y$1 + this.size$1.y$1) - l.start$1.y$1) / m) + l.start$1.x$1);
  if (((x2 > this.loc$1.x$1) && (x2 < (this.loc$1.x$1 + this.size$1.x$1)))) {
    return true
  };
  return false
});
ScalaJS.c.Lobjects_Obj.prototype.hasCenterLosTo__Lglobalvars_Pt__Lobjects_Obj__Lgame_Game__Z = (function(p, o, g) {
  var elem = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(this.center__Lglobalvars_Pt(), p, "black");
  var l = new ScalaJS.c.sr_ObjectRef().init___O(elem);
  var losBroken = new ScalaJS.c.sr_BooleanRef().init___Z(false);
  g.objs$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, o$2, l$2, losBroken$2) {
    return (function(ob$2) {
      var ob = ScalaJS.as.Lobjects_Obj(ob$2);
      if (((((ob !== arg$outer) && (ob !== o$2)) && ob.blocksLos$1) && ob.collidesLine__Lglobalvars_SimpleLine__Z(ScalaJS.as.Lglobalvars_SimpleLine(l$2.elem$1)))) {
        losBroken$2.elem$1 = true
      }
    })
  })(this, o, l, losBroken)));
  if ((!losBroken.elem$1)) {
    return true
  };
  return false
});
ScalaJS.c.Lobjects_Obj.prototype.lowerLeft__Lglobalvars_Pt = (function() {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.loc$1.x$1, (this.loc$1.y$1 + this.size$1.y$1))
});
ScalaJS.c.Lobjects_Obj.prototype.distanceTo__Lobjects_Obj__I = (function(o) {
  var x = (o.loc$1.x$1 - this.loc$1.x$1);
  var jsx$1 = ScalaJS.uD(ScalaJS.g["Math"]["pow"](x, 2.0));
  var x$1 = (o.loc$1.y$1 - this.loc$1.y$1);
  var x$2 = (jsx$1 + ScalaJS.uD(ScalaJS.g["Math"]["pow"](x$1, 2.0)));
  return (ScalaJS.uD(ScalaJS.g["Math"]["sqrt"](x$2)) | 0)
});
ScalaJS.c.Lobjects_Obj.prototype.distanceTo__Lglobalvars_Pt__I = (function(p) {
  var x = (p.x$1 - this.loc$1.x$1);
  var jsx$1 = ScalaJS.uD(ScalaJS.g["Math"]["pow"](x, 2.0));
  var x$1 = (p.y$1 - this.loc$1.y$1);
  var x$2 = (jsx$1 + ScalaJS.uD(ScalaJS.g["Math"]["pow"](x$1, 2.0)));
  return (ScalaJS.uD(ScalaJS.g["Math"]["sqrt"](x$2)) | 0)
});
ScalaJS.c.Lobjects_Obj.prototype.draw__Lgame_Game__V = (function(g) {
  /*<skip>*/
});
ScalaJS.c.Lobjects_Obj.prototype.center__Lglobalvars_Pt = (function() {
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.loc$1.x$1 + (this.size$1.x$1 / 2)), (this.loc$1.y$1 + (this.size$1.y$1 / 2)))
});
ScalaJS.is.Lobjects_Obj = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_Obj)))
});
ScalaJS.as.Lobjects_Obj = (function(obj) {
  return ((ScalaJS.is.Lobjects_Obj(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.Obj"))
});
ScalaJS.isArrayOf.Lobjects_Obj = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_Obj)))
});
ScalaJS.asArrayOf.Lobjects_Obj = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_Obj(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.Obj;", depth))
});
ScalaJS.d.Lobjects_Obj = new ScalaJS.ClassTypeData({
  Lobjects_Obj: 0
}, false, "objects.Obj", {
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_Obj.prototype.$classData = ScalaJS.d.Lobjects_Obj;
/** @constructor */
ScalaJS.c.Lobjects_UsableItem = (function() {
  ScalaJS.c.O.call(this);
  this.owner$1 = null;
  this.name$1 = null;
  this.displayName$1 = null
});
ScalaJS.c.Lobjects_UsableItem.prototype = new ScalaJS.h.O();
ScalaJS.c.Lobjects_UsableItem.prototype.constructor = ScalaJS.c.Lobjects_UsableItem;
/** @constructor */
ScalaJS.h.Lobjects_UsableItem = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_UsableItem.prototype = ScalaJS.c.Lobjects_UsableItem.prototype;
ScalaJS.c.Lobjects_UsableItem.prototype.init___Lobjects_Actor__T__T = (function(owner_, name_, displayName_) {
  this.owner$1 = owner_;
  this.name$1 = name_;
  this.displayName$1 = displayName_;
  return this
});
ScalaJS.is.Lobjects_UsableItem = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_UsableItem)))
});
ScalaJS.as.Lobjects_UsableItem = (function(obj) {
  return ((ScalaJS.is.Lobjects_UsableItem(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.UsableItem"))
});
ScalaJS.isArrayOf.Lobjects_UsableItem = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_UsableItem)))
});
ScalaJS.asArrayOf.Lobjects_UsableItem = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_UsableItem(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.UsableItem;", depth))
});
/** @constructor */
ScalaJS.c.jl_Character$ = (function() {
  ScalaJS.c.O.call(this);
  this.TYPE$1 = null;
  this.MIN$undVALUE$1 = 0;
  this.MAX$undVALUE$1 = 0;
  this.SIZE$1 = 0;
  this.MIN$undRADIX$1 = 0;
  this.MAX$undRADIX$1 = 0;
  this.MIN$undHIGH$undSURROGATE$1 = 0;
  this.MAX$undHIGH$undSURROGATE$1 = 0;
  this.MIN$undLOW$undSURROGATE$1 = 0;
  this.MAX$undLOW$undSURROGATE$1 = 0;
  this.MIN$undSURROGATE$1 = 0;
  this.MAX$undSURROGATE$1 = 0;
  this.MIN$undCODE$undPOINT$1 = 0;
  this.MAX$undCODE$undPOINT$1 = 0;
  this.MIN$undSUPPLEMENTARY$undCODE$undPOINT$1 = 0;
  this.HighSurrogateMask$1 = 0;
  this.HighSurrogateID$1 = 0;
  this.LowSurrogateMask$1 = 0;
  this.LowSurrogateID$1 = 0;
  this.SurrogateUsefulPartMask$1 = 0;
  this.reUnicodeIdentStart$1 = null;
  this.reUnicodeIdentPartExcl$1 = null;
  this.reIdentIgnorable$1 = null;
  this.bitmap$0$1 = 0
});
ScalaJS.c.jl_Character$.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_Character$.prototype.constructor = ScalaJS.c.jl_Character$;
/** @constructor */
ScalaJS.h.jl_Character$ = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Character$.prototype = ScalaJS.c.jl_Character$.prototype;
ScalaJS.c.jl_Character$.prototype.digit__C__I__I = (function(c, radix) {
  return (((radix > 36) || (radix < 2)) ? (-1) : ((((c >= 48) && (c <= 57)) && ((((-48) + c) | 0) < radix)) ? (((-48) + c) | 0) : ((((c >= 65) && (c <= 90)) && ((((-65) + c) | 0) < (((-10) + radix) | 0))) ? (((-55) + c) | 0) : ((((c >= 97) && (c <= 122)) && ((((-97) + c) | 0) < (((-10) + radix) | 0))) ? (((-87) + c) | 0) : ((((c >= 65313) && (c <= 65338)) && ((((-65313) + c) | 0) < (((-10) + radix) | 0))) ? (((-65303) + c) | 0) : ((((c >= 65345) && (c <= 65370)) && ((((-65345) + c) | 0) < (((-10) + radix) | 0))) ? (((-65303) + c) | 0) : (-1)))))))
});
ScalaJS.c.jl_Character$.prototype.isUpperCase__C__Z = (function(c) {
  return (this.toUpperCase__C__C(c) === c)
});
ScalaJS.c.jl_Character$.prototype.toUpperCase__C__C = (function(c) {
  var thiz = ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c));
  var $$this = ScalaJS.as.T(thiz["toUpperCase"]());
  return (65535 & ScalaJS.uI($$this["charCodeAt"](0)))
});
ScalaJS.d.jl_Character$ = new ScalaJS.ClassTypeData({
  jl_Character$: 0
}, false, "java.lang.Character$", {
  jl_Character$: 1,
  O: 1
});
ScalaJS.c.jl_Character$.prototype.$classData = ScalaJS.d.jl_Character$;
ScalaJS.n.jl_Character$ = (void 0);
ScalaJS.m.jl_Character$ = (function() {
  if ((!ScalaJS.n.jl_Character$)) {
    ScalaJS.n.jl_Character$ = new ScalaJS.c.jl_Character$().init___()
  };
  return ScalaJS.n.jl_Character$
});
/** @constructor */
ScalaJS.c.jl_Class = (function() {
  ScalaJS.c.O.call(this);
  this.data$1 = null
});
ScalaJS.c.jl_Class.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_Class.prototype.constructor = ScalaJS.c.jl_Class;
/** @constructor */
ScalaJS.h.jl_Class = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Class.prototype = ScalaJS.c.jl_Class.prototype;
ScalaJS.c.jl_Class.prototype.getName__T = (function() {
  return ScalaJS.as.T(this.data$1["name"])
});
ScalaJS.c.jl_Class.prototype.isPrimitive__Z = (function() {
  return ScalaJS.uZ(this.data$1["isPrimitive"])
});
ScalaJS.c.jl_Class.prototype.toString__T = (function() {
  return ((this.isInterface__Z() ? "interface " : (this.isPrimitive__Z() ? "" : "class ")) + this.getName__T())
});
ScalaJS.c.jl_Class.prototype.isAssignableFrom__jl_Class__Z = (function(that) {
  return ((this.isPrimitive__Z() || that.isPrimitive__Z()) ? ((this === that) || ((this === ScalaJS.d.S.getClassOf()) ? (that === ScalaJS.d.B.getClassOf()) : ((this === ScalaJS.d.I.getClassOf()) ? ((that === ScalaJS.d.B.getClassOf()) || (that === ScalaJS.d.S.getClassOf())) : ((this === ScalaJS.d.F.getClassOf()) ? (((that === ScalaJS.d.B.getClassOf()) || (that === ScalaJS.d.S.getClassOf())) || (that === ScalaJS.d.I.getClassOf())) : ((this === ScalaJS.d.D.getClassOf()) && ((((that === ScalaJS.d.B.getClassOf()) || (that === ScalaJS.d.S.getClassOf())) || (that === ScalaJS.d.I.getClassOf())) || (that === ScalaJS.d.F.getClassOf()))))))) : this.isInstance__O__Z(that.getFakeInstance__p1__O()))
});
ScalaJS.c.jl_Class.prototype.isInstance__O__Z = (function(obj) {
  return ScalaJS.uZ(this.data$1["isInstance"](obj))
});
ScalaJS.c.jl_Class.prototype.getFakeInstance__p1__O = (function() {
  return this.data$1["getFakeInstance"]()
});
ScalaJS.c.jl_Class.prototype.init___jl_ScalaJSClassData = (function(data) {
  this.data$1 = data;
  return this
});
ScalaJS.c.jl_Class.prototype.isArray__Z = (function() {
  return ScalaJS.uZ(this.data$1["isArrayClass"])
});
ScalaJS.c.jl_Class.prototype.isInterface__Z = (function() {
  return ScalaJS.uZ(this.data$1["isInterface"])
});
ScalaJS.d.jl_Class = new ScalaJS.ClassTypeData({
  jl_Class: 0
}, false, "java.lang.Class", {
  jl_Class: 1,
  O: 1
});
ScalaJS.c.jl_Class.prototype.$classData = ScalaJS.d.jl_Class;
/** @constructor */
ScalaJS.c.jl_Double$ = (function() {
  ScalaJS.c.O.call(this);
  this.TYPE$1 = null;
  this.POSITIVE$undINFINITY$1 = 0.0;
  this.NEGATIVE$undINFINITY$1 = 0.0;
  this.NaN$1 = 0.0;
  this.MAX$undVALUE$1 = 0.0;
  this.MIN$undVALUE$1 = 0.0;
  this.MAX$undEXPONENT$1 = 0;
  this.MIN$undEXPONENT$1 = 0;
  this.SIZE$1 = 0;
  this.doubleStrPat$1 = null;
  this.bitmap$0$1 = false
});
ScalaJS.c.jl_Double$.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_Double$.prototype.constructor = ScalaJS.c.jl_Double$;
/** @constructor */
ScalaJS.h.jl_Double$ = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Double$.prototype = ScalaJS.c.jl_Double$.prototype;
ScalaJS.c.jl_Double$.prototype.compare__D__D__I = (function(a, b) {
  if ((a !== a)) {
    return ((b !== b) ? 0 : 1)
  } else if ((b !== b)) {
    return (-1)
  } else if ((a === b)) {
    if ((a === 0.0)) {
      var ainf = (1.0 / a);
      return ((ainf === (1.0 / b)) ? 0 : ((ainf < 0) ? (-1) : 1))
    } else {
      return 0
    }
  } else {
    return ((a < b) ? (-1) : 1)
  }
});
ScalaJS.d.jl_Double$ = new ScalaJS.ClassTypeData({
  jl_Double$: 0
}, false, "java.lang.Double$", {
  jl_Double$: 1,
  O: 1
});
ScalaJS.c.jl_Double$.prototype.$classData = ScalaJS.d.jl_Double$;
ScalaJS.n.jl_Double$ = (void 0);
ScalaJS.m.jl_Double$ = (function() {
  if ((!ScalaJS.n.jl_Double$)) {
    ScalaJS.n.jl_Double$ = new ScalaJS.c.jl_Double$().init___()
  };
  return ScalaJS.n.jl_Double$
});
/** @constructor */
ScalaJS.c.jl_Integer$ = (function() {
  ScalaJS.c.O.call(this);
  this.TYPE$1 = null;
  this.MIN$undVALUE$1 = 0;
  this.MAX$undVALUE$1 = 0;
  this.SIZE$1 = 0
});
ScalaJS.c.jl_Integer$.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_Integer$.prototype.constructor = ScalaJS.c.jl_Integer$;
/** @constructor */
ScalaJS.h.jl_Integer$ = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Integer$.prototype = ScalaJS.c.jl_Integer$.prototype;
ScalaJS.c.jl_Integer$.prototype.fail$1__p1__T__sr_Nothing$ = (function(s$1) {
  throw new ScalaJS.c.jl_NumberFormatException().init___T(new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["For input string: \"", "\""])).s__sc_Seq__T(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([s$1])))
});
ScalaJS.c.jl_Integer$.prototype.parseInt__T__I__I = (function(s, radix) {
  if ((s === null)) {
    var jsx$1 = true
  } else {
    var this$2 = new ScalaJS.c.sci_StringOps().init___T(s);
    var $$this = this$2.repr$1;
    var jsx$1 = (ScalaJS.uI($$this["length"]) === 0)
  };
  if (((jsx$1 || (radix < 2)) || (radix > 36))) {
    this.fail$1__p1__T__sr_Nothing$(s)
  } else {
    var i = ((((65535 & ScalaJS.uI(s["charCodeAt"](0))) === 45) || ((65535 & ScalaJS.uI(s["charCodeAt"](0))) === 43)) ? 1 : 0);
    var this$12 = new ScalaJS.c.sci_StringOps().init___T(s);
    var $$this$1 = this$12.repr$1;
    if ((ScalaJS.uI($$this$1["length"]) <= i)) {
      this.fail$1__p1__T__sr_Nothing$(s)
    } else {
      while (true) {
        var jsx$2 = i;
        var this$16 = new ScalaJS.c.sci_StringOps().init___T(s);
        var $$this$2 = this$16.repr$1;
        if ((jsx$2 < ScalaJS.uI($$this$2["length"]))) {
          var jsx$3 = ScalaJS.m.jl_Character$();
          var index = i;
          if ((jsx$3.digit__C__I__I((65535 & ScalaJS.uI(s["charCodeAt"](index))), radix) < 0)) {
            this.fail$1__p1__T__sr_Nothing$(s)
          };
          i = ((1 + i) | 0)
        } else {
          break
        }
      };
      var res = ScalaJS.uD(ScalaJS.g["parseInt"](s, radix));
      return ((((res !== res) || (res > 2147483647)) || (res < (-2147483648))) ? this.fail$1__p1__T__sr_Nothing$(s) : (res | 0))
    }
  }
});
ScalaJS.c.jl_Integer$.prototype.rotateLeft__I__I__I = (function(i, distance) {
  return ((i << distance) | ((i >>> ((-distance) | 0)) | 0))
});
ScalaJS.c.jl_Integer$.prototype.reverseBytes__I__I = (function(i) {
  var byte3 = ((i >>> 24) | 0);
  var byte2 = (65280 & ((i >>> 8) | 0));
  var byte1 = (16711680 & (i << 8));
  var byte0 = (i << 24);
  return (((byte0 | byte1) | byte2) | byte3)
});
ScalaJS.c.jl_Integer$.prototype.bitCount__I__I = (function(i) {
  var t1 = ((i - (1431655765 & (i >> 1))) | 0);
  var t2 = (((858993459 & t1) + (858993459 & (t1 >> 2))) | 0);
  return (ScalaJS.imul(16843009, (252645135 & ((t2 + (t2 >> 4)) | 0))) >> 24)
});
ScalaJS.c.jl_Integer$.prototype.numberOfLeadingZeros__I__I = (function(i) {
  var x = i;
  x = (x | ((x >>> 1) | 0));
  x = (x | ((x >>> 2) | 0));
  x = (x | ((x >>> 4) | 0));
  x = (x | ((x >>> 8) | 0));
  x = (x | ((x >>> 16) | 0));
  return ((32 - this.bitCount__I__I(x)) | 0)
});
ScalaJS.c.jl_Integer$.prototype.numberOfTrailingZeros__I__I = (function(i) {
  return this.bitCount__I__I((((-1) + (i & ((-i) | 0))) | 0))
});
ScalaJS.d.jl_Integer$ = new ScalaJS.ClassTypeData({
  jl_Integer$: 0
}, false, "java.lang.Integer$", {
  jl_Integer$: 1,
  O: 1
});
ScalaJS.c.jl_Integer$.prototype.$classData = ScalaJS.d.jl_Integer$;
ScalaJS.n.jl_Integer$ = (void 0);
ScalaJS.m.jl_Integer$ = (function() {
  if ((!ScalaJS.n.jl_Integer$)) {
    ScalaJS.n.jl_Integer$ = new ScalaJS.c.jl_Integer$().init___()
  };
  return ScalaJS.n.jl_Integer$
});
/** @constructor */
ScalaJS.c.jl_Number = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.jl_Number.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_Number.prototype.constructor = ScalaJS.c.jl_Number;
/** @constructor */
ScalaJS.h.jl_Number = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Number.prototype = ScalaJS.c.jl_Number.prototype;
ScalaJS.is.jl_Number = (function(obj) {
  return (!(!(((obj && obj.$classData) && obj.$classData.ancestors.jl_Number) || ((typeof obj) === "number"))))
});
ScalaJS.as.jl_Number = (function(obj) {
  return ((ScalaJS.is.jl_Number(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.lang.Number"))
});
ScalaJS.isArrayOf.jl_Number = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Number)))
});
ScalaJS.asArrayOf.jl_Number = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_Number(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Number;", depth))
});
/** @constructor */
ScalaJS.c.jl_System$ = (function() {
  ScalaJS.c.O.call(this);
  this.out$1 = null;
  this.err$1 = null;
  this.in$1 = null;
  this.getHighPrecisionTime$1 = null
});
ScalaJS.c.jl_System$.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_System$.prototype.constructor = ScalaJS.c.jl_System$;
/** @constructor */
ScalaJS.h.jl_System$ = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_System$.prototype = ScalaJS.c.jl_System$.prototype;
ScalaJS.c.jl_System$.prototype.init___ = (function() {
  ScalaJS.n.jl_System$ = this;
  this.out$1 = new ScalaJS.c.jl_JSConsoleBasedPrintStream().init___jl_Boolean(false);
  this.err$1 = new ScalaJS.c.jl_JSConsoleBasedPrintStream().init___jl_Boolean(true);
  this.in$1 = null;
  var x = ScalaJS.g["performance"];
  if (ScalaJS.uZ((!(!x)))) {
    var x$1 = ScalaJS.g["performance"]["now"];
    if (ScalaJS.uZ((!(!x$1)))) {
      var jsx$1 = (function(this$2$1) {
        return (function() {
          return ScalaJS.uD(ScalaJS.g["performance"]["now"]())
        })
      })(this)
    } else {
      var x$2 = ScalaJS.g["performance"]["webkitNow"];
      if (ScalaJS.uZ((!(!x$2)))) {
        var jsx$1 = (function(this$3$1) {
          return (function() {
            return ScalaJS.uD(ScalaJS.g["performance"]["webkitNow"]())
          })
        })(this)
      } else {
        var jsx$1 = (function(this$4$1) {
          return (function() {
            return ScalaJS.uD(new ScalaJS.g["Date"]()["getTime"]())
          })
        })(this)
      }
    }
  } else {
    var jsx$1 = (function(this$5$1) {
      return (function() {
        return ScalaJS.uD(new ScalaJS.g["Date"]()["getTime"]())
      })
    })(this)
  };
  this.getHighPrecisionTime$1 = jsx$1;
  return this
});
ScalaJS.d.jl_System$ = new ScalaJS.ClassTypeData({
  jl_System$: 0
}, false, "java.lang.System$", {
  jl_System$: 1,
  O: 1
});
ScalaJS.c.jl_System$.prototype.$classData = ScalaJS.d.jl_System$;
ScalaJS.n.jl_System$ = (void 0);
ScalaJS.m.jl_System$ = (function() {
  if ((!ScalaJS.n.jl_System$)) {
    ScalaJS.n.jl_System$ = new ScalaJS.c.jl_System$().init___()
  };
  return ScalaJS.n.jl_System$
});
/** @constructor */
ScalaJS.c.jl_ThreadLocal = (function() {
  ScalaJS.c.O.call(this);
  this.hasValue$1 = null;
  this.v$1 = null
});
ScalaJS.c.jl_ThreadLocal.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_ThreadLocal.prototype.constructor = ScalaJS.c.jl_ThreadLocal;
/** @constructor */
ScalaJS.h.jl_ThreadLocal = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_ThreadLocal.prototype = ScalaJS.c.jl_ThreadLocal.prototype;
ScalaJS.c.jl_ThreadLocal.prototype.init___ = (function() {
  this.hasValue$1 = false;
  return this
});
ScalaJS.c.jl_ThreadLocal.prototype.get__O = (function() {
  var x = this.hasValue$1;
  if ((!ScalaJS.uZ(x))) {
    this.set__O__V(this.initialValue__O())
  };
  return this.v$1
});
ScalaJS.c.jl_ThreadLocal.prototype.set__O__V = (function(o) {
  this.v$1 = o;
  this.hasValue$1 = true
});
/** @constructor */
ScalaJS.c.ju_Arrays$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.ju_Arrays$.prototype = new ScalaJS.h.O();
ScalaJS.c.ju_Arrays$.prototype.constructor = ScalaJS.c.ju_Arrays$;
/** @constructor */
ScalaJS.h.ju_Arrays$ = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_Arrays$.prototype = ScalaJS.c.ju_Arrays$.prototype;
ScalaJS.c.ju_Arrays$.prototype.fillImpl$mIc$sp__p1__AI__I__V = (function(a, value) {
  var i = 0;
  while ((i !== a.u["length"])) {
    a.u[i] = value;
    i = ((1 + i) | 0)
  }
});
ScalaJS.d.ju_Arrays$ = new ScalaJS.ClassTypeData({
  ju_Arrays$: 0
}, false, "java.util.Arrays$", {
  ju_Arrays$: 1,
  O: 1
});
ScalaJS.c.ju_Arrays$.prototype.$classData = ScalaJS.d.ju_Arrays$;
ScalaJS.n.ju_Arrays$ = (void 0);
ScalaJS.m.ju_Arrays$ = (function() {
  if ((!ScalaJS.n.ju_Arrays$)) {
    ScalaJS.n.ju_Arrays$ = new ScalaJS.c.ju_Arrays$().init___()
  };
  return ScalaJS.n.ju_Arrays$
});
/** @constructor */
ScalaJS.c.ju_Formatter$ = (function() {
  ScalaJS.c.O.call(this);
  this.java$util$Formatter$$RegularChunk$1 = null;
  this.java$util$Formatter$$DoublePercent$1 = null;
  this.java$util$Formatter$$EOLChunk$1 = null;
  this.java$util$Formatter$$FormattedChunk$1 = null
});
ScalaJS.c.ju_Formatter$.prototype = new ScalaJS.h.O();
ScalaJS.c.ju_Formatter$.prototype.constructor = ScalaJS.c.ju_Formatter$;
/** @constructor */
ScalaJS.h.ju_Formatter$ = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_Formatter$.prototype = ScalaJS.c.ju_Formatter$.prototype;
ScalaJS.c.ju_Formatter$.prototype.init___ = (function() {
  ScalaJS.n.ju_Formatter$ = this;
  this.java$util$Formatter$$RegularChunk$1 = new ScalaJS.c.ju_Formatter$RegExpExtractor().init___sjs_js_RegExp(new ScalaJS.g["RegExp"]("^[^\\x25]+"));
  this.java$util$Formatter$$DoublePercent$1 = new ScalaJS.c.ju_Formatter$RegExpExtractor().init___sjs_js_RegExp(new ScalaJS.g["RegExp"]("^\\x25{2}"));
  this.java$util$Formatter$$EOLChunk$1 = new ScalaJS.c.ju_Formatter$RegExpExtractor().init___sjs_js_RegExp(new ScalaJS.g["RegExp"]("^\\x25n"));
  this.java$util$Formatter$$FormattedChunk$1 = new ScalaJS.c.ju_Formatter$RegExpExtractor().init___sjs_js_RegExp(new ScalaJS.g["RegExp"]("^\\x25(?:([1-9]\\d*)\\$)?([-#+ 0,\\(<]*)(\\d*)(?:\\.(\\d+))?([A-Za-z])"));
  return this
});
ScalaJS.d.ju_Formatter$ = new ScalaJS.ClassTypeData({
  ju_Formatter$: 0
}, false, "java.util.Formatter$", {
  ju_Formatter$: 1,
  O: 1
});
ScalaJS.c.ju_Formatter$.prototype.$classData = ScalaJS.d.ju_Formatter$;
ScalaJS.n.ju_Formatter$ = (void 0);
ScalaJS.m.ju_Formatter$ = (function() {
  if ((!ScalaJS.n.ju_Formatter$)) {
    ScalaJS.n.ju_Formatter$ = new ScalaJS.c.ju_Formatter$().init___()
  };
  return ScalaJS.n.ju_Formatter$
});
/** @constructor */
ScalaJS.c.ju_Formatter$RegExpExtractor = (function() {
  ScalaJS.c.O.call(this);
  this.regexp$1 = null
});
ScalaJS.c.ju_Formatter$RegExpExtractor.prototype = new ScalaJS.h.O();
ScalaJS.c.ju_Formatter$RegExpExtractor.prototype.constructor = ScalaJS.c.ju_Formatter$RegExpExtractor;
/** @constructor */
ScalaJS.h.ju_Formatter$RegExpExtractor = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_Formatter$RegExpExtractor.prototype = ScalaJS.c.ju_Formatter$RegExpExtractor.prototype;
ScalaJS.c.ju_Formatter$RegExpExtractor.prototype.unapply__T__s_Option = (function(str) {
  return ScalaJS.m.s_Option$().apply__O__s_Option(this.regexp$1["exec"](str))
});
ScalaJS.c.ju_Formatter$RegExpExtractor.prototype.init___sjs_js_RegExp = (function(regexp) {
  this.regexp$1 = regexp;
  return this
});
ScalaJS.d.ju_Formatter$RegExpExtractor = new ScalaJS.ClassTypeData({
  ju_Formatter$RegExpExtractor: 0
}, false, "java.util.Formatter$RegExpExtractor", {
  ju_Formatter$RegExpExtractor: 1,
  O: 1
});
ScalaJS.c.ju_Formatter$RegExpExtractor.prototype.$classData = ScalaJS.d.ju_Formatter$RegExpExtractor;
/** @constructor */
ScalaJS.c.s_DeprecatedConsole = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_DeprecatedConsole.prototype = new ScalaJS.h.O();
ScalaJS.c.s_DeprecatedConsole.prototype.constructor = ScalaJS.c.s_DeprecatedConsole;
/** @constructor */
ScalaJS.h.s_DeprecatedConsole = (function() {
  /*<skip>*/
});
ScalaJS.h.s_DeprecatedConsole.prototype = ScalaJS.c.s_DeprecatedConsole.prototype;
/** @constructor */
ScalaJS.c.s_FallbackArrayBuilding = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_FallbackArrayBuilding.prototype = new ScalaJS.h.O();
ScalaJS.c.s_FallbackArrayBuilding.prototype.constructor = ScalaJS.c.s_FallbackArrayBuilding;
/** @constructor */
ScalaJS.h.s_FallbackArrayBuilding = (function() {
  /*<skip>*/
});
ScalaJS.h.s_FallbackArrayBuilding.prototype = ScalaJS.c.s_FallbackArrayBuilding.prototype;
/** @constructor */
ScalaJS.c.s_LowPriorityImplicits = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_LowPriorityImplicits.prototype = new ScalaJS.h.O();
ScalaJS.c.s_LowPriorityImplicits.prototype.constructor = ScalaJS.c.s_LowPriorityImplicits;
/** @constructor */
ScalaJS.h.s_LowPriorityImplicits = (function() {
  /*<skip>*/
});
ScalaJS.h.s_LowPriorityImplicits.prototype = ScalaJS.c.s_LowPriorityImplicits.prototype;
ScalaJS.c.s_LowPriorityImplicits.prototype.unwrapString__sci_WrappedString__T = (function(ws) {
  return ((ws !== null) ? ws.self$4 : null)
});
/** @constructor */
ScalaJS.c.s_PartialFunction$ = (function() {
  ScalaJS.c.O.call(this);
  this.scala$PartialFunction$$fallback$undpf$f = null;
  this.scala$PartialFunction$$constFalse$f = null;
  this.empty$undpf$1 = null
});
ScalaJS.c.s_PartialFunction$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_PartialFunction$.prototype.constructor = ScalaJS.c.s_PartialFunction$;
/** @constructor */
ScalaJS.h.s_PartialFunction$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_PartialFunction$.prototype = ScalaJS.c.s_PartialFunction$.prototype;
ScalaJS.c.s_PartialFunction$.prototype.init___ = (function() {
  ScalaJS.n.s_PartialFunction$ = this;
  this.scala$PartialFunction$$fallback$undpf$f = new ScalaJS.c.s_PartialFunction$$anonfun$4().init___();
  this.scala$PartialFunction$$constFalse$f = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2) {
    return (function(x$1$2) {
      return false
    })
  })(this));
  this.empty$undpf$1 = new ScalaJS.c.s_PartialFunction$$anon$1().init___();
  return this
});
ScalaJS.c.s_PartialFunction$.prototype.scala$PartialFunction$$fallbackOccurred__O__Z = (function(x) {
  return (this.scala$PartialFunction$$fallback$undpf$f === x)
});
ScalaJS.d.s_PartialFunction$ = new ScalaJS.ClassTypeData({
  s_PartialFunction$: 0
}, false, "scala.PartialFunction$", {
  s_PartialFunction$: 1,
  O: 1
});
ScalaJS.c.s_PartialFunction$.prototype.$classData = ScalaJS.d.s_PartialFunction$;
ScalaJS.n.s_PartialFunction$ = (void 0);
ScalaJS.m.s_PartialFunction$ = (function() {
  if ((!ScalaJS.n.s_PartialFunction$)) {
    ScalaJS.n.s_PartialFunction$ = new ScalaJS.c.s_PartialFunction$().init___()
  };
  return ScalaJS.n.s_PartialFunction$
});
ScalaJS.s.s_PartialFunction$class__runWith__s_PartialFunction__F1__F1 = (function($$this, action) {
  return new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1, action$1) {
    return (function(x$2) {
      var z = $$this$1.applyOrElse__O__F1__O(x$2, ScalaJS.m.s_PartialFunction$().scala$PartialFunction$$fallback$undpf$f);
      return ((!ScalaJS.m.s_PartialFunction$().scala$PartialFunction$$fallbackOccurred__O__Z(z)) && (action$1.apply__O__O(z), true))
    })
  })($$this, action))
});
ScalaJS.s.s_PartialFunction$class__applyOrElse__s_PartialFunction__O__F1__O = (function($$this, x, default$2) {
  return ($$this.isDefinedAt__O__Z(x) ? $$this.apply__O__O(x) : default$2.apply__O__O(x))
});
/** @constructor */
ScalaJS.c.s_Predef$any2stringadd$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_Predef$any2stringadd$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_Predef$any2stringadd$.prototype.constructor = ScalaJS.c.s_Predef$any2stringadd$;
/** @constructor */
ScalaJS.h.s_Predef$any2stringadd$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Predef$any2stringadd$.prototype = ScalaJS.c.s_Predef$any2stringadd$.prototype;
ScalaJS.c.s_Predef$any2stringadd$.prototype.$$plus$extension__O__T__T = (function($$this, other) {
  return (("" + ScalaJS.m.sjsr_RuntimeString$().valueOf__O__T($$this)) + other)
});
ScalaJS.d.s_Predef$any2stringadd$ = new ScalaJS.ClassTypeData({
  s_Predef$any2stringadd$: 0
}, false, "scala.Predef$any2stringadd$", {
  s_Predef$any2stringadd$: 1,
  O: 1
});
ScalaJS.c.s_Predef$any2stringadd$.prototype.$classData = ScalaJS.d.s_Predef$any2stringadd$;
ScalaJS.n.s_Predef$any2stringadd$ = (void 0);
ScalaJS.m.s_Predef$any2stringadd$ = (function() {
  if ((!ScalaJS.n.s_Predef$any2stringadd$)) {
    ScalaJS.n.s_Predef$any2stringadd$ = new ScalaJS.c.s_Predef$any2stringadd$().init___()
  };
  return ScalaJS.n.s_Predef$any2stringadd$
});
ScalaJS.s.s_Product2$class__productElement__s_Product2__I__O = (function($$this, n) {
  switch (n) {
    case 0:
      {
        return $$this.$$und1__O();
        break
      };
    case 1:
      {
        return $$this.$$und2__O();
        break
      };
    default:
      throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + n));
  }
});
ScalaJS.s.s_Proxy$class__toString__s_Proxy__T = (function($$this) {
  return ("" + $$this.self$1)
});
ScalaJS.s.s_Proxy$class__equals__s_Proxy__O__Z = (function($$this, that) {
  return ((that !== null) && (((that === $$this) || (that === $$this.self$1)) || ScalaJS.objectEquals(that, $$this.self$1)))
});
/** @constructor */
ScalaJS.c.s_math_Ordered$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_math_Ordered$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_math_Ordered$.prototype.constructor = ScalaJS.c.s_math_Ordered$;
/** @constructor */
ScalaJS.h.s_math_Ordered$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_math_Ordered$.prototype = ScalaJS.c.s_math_Ordered$.prototype;
ScalaJS.d.s_math_Ordered$ = new ScalaJS.ClassTypeData({
  s_math_Ordered$: 0
}, false, "scala.math.Ordered$", {
  s_math_Ordered$: 1,
  O: 1
});
ScalaJS.c.s_math_Ordered$.prototype.$classData = ScalaJS.d.s_math_Ordered$;
ScalaJS.n.s_math_Ordered$ = (void 0);
ScalaJS.m.s_math_Ordered$ = (function() {
  if ((!ScalaJS.n.s_math_Ordered$)) {
    ScalaJS.n.s_math_Ordered$ = new ScalaJS.c.s_math_Ordered$().init___()
  };
  return ScalaJS.n.s_math_Ordered$
});
/** @constructor */
ScalaJS.c.s_package$ = (function() {
  ScalaJS.c.O.call(this);
  this.AnyRef$1 = null;
  this.Traversable$1 = null;
  this.Iterable$1 = null;
  this.Seq$1 = null;
  this.IndexedSeq$1 = null;
  this.Iterator$1 = null;
  this.List$1 = null;
  this.Nil$1 = null;
  this.$$colon$colon$1 = null;
  this.$$plus$colon$1 = null;
  this.$$colon$plus$1 = null;
  this.Stream$1 = null;
  this.$$hash$colon$colon$1 = null;
  this.Vector$1 = null;
  this.StringBuilder$1 = null;
  this.Range$1 = null;
  this.BigDecimal$1 = null;
  this.BigInt$1 = null;
  this.Equiv$1 = null;
  this.Fractional$1 = null;
  this.Integral$1 = null;
  this.Numeric$1 = null;
  this.Ordered$1 = null;
  this.Ordering$1 = null;
  this.Either$1 = null;
  this.Left$1 = null;
  this.Right$1 = null;
  this.bitmap$0$1 = 0
});
ScalaJS.c.s_package$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_package$.prototype.constructor = ScalaJS.c.s_package$;
/** @constructor */
ScalaJS.h.s_package$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_package$.prototype = ScalaJS.c.s_package$.prototype;
ScalaJS.c.s_package$.prototype.init___ = (function() {
  ScalaJS.n.s_package$ = this;
  this.AnyRef$1 = new ScalaJS.c.s_package$$anon$1().init___();
  this.Traversable$1 = ScalaJS.m.sc_Traversable$();
  this.Iterable$1 = ScalaJS.m.sc_Iterable$();
  this.Seq$1 = ScalaJS.m.sc_Seq$();
  this.IndexedSeq$1 = ScalaJS.m.sc_IndexedSeq$();
  this.Iterator$1 = ScalaJS.m.sc_Iterator$();
  this.List$1 = ScalaJS.m.sci_List$();
  this.Nil$1 = ScalaJS.m.sci_Nil$();
  this.$$colon$colon$1 = ScalaJS.m.sci_$colon$colon$();
  this.$$plus$colon$1 = ScalaJS.m.sc_$plus$colon$();
  this.$$colon$plus$1 = ScalaJS.m.sc_$colon$plus$();
  this.Stream$1 = ScalaJS.m.sci_Stream$();
  this.$$hash$colon$colon$1 = ScalaJS.m.sci_Stream$$hash$colon$colon$();
  this.Vector$1 = ScalaJS.m.sci_Vector$();
  this.StringBuilder$1 = ScalaJS.m.scm_StringBuilder$();
  this.Range$1 = ScalaJS.m.sci_Range$();
  this.Equiv$1 = ScalaJS.m.s_math_Equiv$();
  this.Fractional$1 = ScalaJS.m.s_math_Fractional$();
  this.Integral$1 = ScalaJS.m.s_math_Integral$();
  this.Numeric$1 = ScalaJS.m.s_math_Numeric$();
  this.Ordered$1 = ScalaJS.m.s_math_Ordered$();
  this.Ordering$1 = ScalaJS.m.s_math_Ordering$();
  this.Either$1 = ScalaJS.m.s_util_Either$();
  this.Left$1 = ScalaJS.m.s_util_Left$();
  this.Right$1 = ScalaJS.m.s_util_Right$();
  return this
});
ScalaJS.d.s_package$ = new ScalaJS.ClassTypeData({
  s_package$: 0
}, false, "scala.package$", {
  s_package$: 1,
  O: 1
});
ScalaJS.c.s_package$.prototype.$classData = ScalaJS.d.s_package$;
ScalaJS.n.s_package$ = (void 0);
ScalaJS.m.s_package$ = (function() {
  if ((!ScalaJS.n.s_package$)) {
    ScalaJS.n.s_package$ = new ScalaJS.c.s_package$().init___()
  };
  return ScalaJS.n.s_package$
});
/** @constructor */
ScalaJS.c.s_reflect_ClassManifestFactory$ = (function() {
  ScalaJS.c.O.call(this);
  this.Byte$1 = null;
  this.Short$1 = null;
  this.Char$1 = null;
  this.Int$1 = null;
  this.Long$1 = null;
  this.Float$1 = null;
  this.Double$1 = null;
  this.Boolean$1 = null;
  this.Unit$1 = null;
  this.Any$1 = null;
  this.Object$1 = null;
  this.AnyVal$1 = null;
  this.Nothing$1 = null;
  this.Null$1 = null
});
ScalaJS.c.s_reflect_ClassManifestFactory$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_reflect_ClassManifestFactory$.prototype.constructor = ScalaJS.c.s_reflect_ClassManifestFactory$;
/** @constructor */
ScalaJS.h.s_reflect_ClassManifestFactory$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ClassManifestFactory$.prototype = ScalaJS.c.s_reflect_ClassManifestFactory$.prototype;
ScalaJS.c.s_reflect_ClassManifestFactory$.prototype.init___ = (function() {
  ScalaJS.n.s_reflect_ClassManifestFactory$ = this;
  this.Byte$1 = ScalaJS.m.s_reflect_ManifestFactory$().Byte$1;
  this.Short$1 = ScalaJS.m.s_reflect_ManifestFactory$().Short$1;
  this.Char$1 = ScalaJS.m.s_reflect_ManifestFactory$().Char$1;
  this.Int$1 = ScalaJS.m.s_reflect_ManifestFactory$().Int$1;
  this.Long$1 = ScalaJS.m.s_reflect_ManifestFactory$().Long$1;
  this.Float$1 = ScalaJS.m.s_reflect_ManifestFactory$().Float$1;
  this.Double$1 = ScalaJS.m.s_reflect_ManifestFactory$().Double$1;
  this.Boolean$1 = ScalaJS.m.s_reflect_ManifestFactory$().Boolean$1;
  this.Unit$1 = ScalaJS.m.s_reflect_ManifestFactory$().Unit$1;
  this.Any$1 = ScalaJS.m.s_reflect_ManifestFactory$().Any$1;
  this.Object$1 = ScalaJS.m.s_reflect_ManifestFactory$().Object$1;
  this.AnyVal$1 = ScalaJS.m.s_reflect_ManifestFactory$().AnyVal$1;
  this.Nothing$1 = ScalaJS.m.s_reflect_ManifestFactory$().Nothing$1;
  this.Null$1 = ScalaJS.m.s_reflect_ManifestFactory$().Null$1;
  return this
});
ScalaJS.d.s_reflect_ClassManifestFactory$ = new ScalaJS.ClassTypeData({
  s_reflect_ClassManifestFactory$: 0
}, false, "scala.reflect.ClassManifestFactory$", {
  s_reflect_ClassManifestFactory$: 1,
  O: 1
});
ScalaJS.c.s_reflect_ClassManifestFactory$.prototype.$classData = ScalaJS.d.s_reflect_ClassManifestFactory$;
ScalaJS.n.s_reflect_ClassManifestFactory$ = (void 0);
ScalaJS.m.s_reflect_ClassManifestFactory$ = (function() {
  if ((!ScalaJS.n.s_reflect_ClassManifestFactory$)) {
    ScalaJS.n.s_reflect_ClassManifestFactory$ = new ScalaJS.c.s_reflect_ClassManifestFactory$().init___()
  };
  return ScalaJS.n.s_reflect_ClassManifestFactory$
});
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$ = (function() {
  ScalaJS.c.O.call(this);
  this.Byte$1 = null;
  this.Short$1 = null;
  this.Char$1 = null;
  this.Int$1 = null;
  this.Long$1 = null;
  this.Float$1 = null;
  this.Double$1 = null;
  this.Boolean$1 = null;
  this.Unit$1 = null;
  this.scala$reflect$ManifestFactory$$ObjectTYPE$1 = null;
  this.scala$reflect$ManifestFactory$$NothingTYPE$1 = null;
  this.scala$reflect$ManifestFactory$$NullTYPE$1 = null;
  this.Any$1 = null;
  this.Object$1 = null;
  this.AnyRef$1 = null;
  this.AnyVal$1 = null;
  this.Null$1 = null;
  this.Nothing$1 = null
});
ScalaJS.c.s_reflect_ManifestFactory$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_reflect_ManifestFactory$.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$.prototype = ScalaJS.c.s_reflect_ManifestFactory$.prototype;
ScalaJS.c.s_reflect_ManifestFactory$.prototype.init___ = (function() {
  ScalaJS.n.s_reflect_ManifestFactory$ = this;
  this.Byte$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$6().init___();
  this.Short$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$7().init___();
  this.Char$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$8().init___();
  this.Int$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$9().init___();
  this.Long$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$10().init___();
  this.Float$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$11().init___();
  this.Double$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$12().init___();
  this.Boolean$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$13().init___();
  this.Unit$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$14().init___();
  this.scala$reflect$ManifestFactory$$ObjectTYPE$1 = ScalaJS.d.O.getClassOf();
  this.scala$reflect$ManifestFactory$$NothingTYPE$1 = ScalaJS.d.sr_Nothing$.getClassOf();
  this.scala$reflect$ManifestFactory$$NullTYPE$1 = ScalaJS.d.sr_Null$.getClassOf();
  this.Any$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$1().init___();
  this.Object$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$2().init___();
  this.AnyRef$1 = this.Object$1;
  this.AnyVal$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$3().init___();
  this.Null$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$4().init___();
  this.Nothing$1 = new ScalaJS.c.s_reflect_ManifestFactory$$anon$5().init___();
  return this
});
ScalaJS.d.s_reflect_ManifestFactory$ = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$: 0
}, false, "scala.reflect.ManifestFactory$", {
  s_reflect_ManifestFactory$: 1,
  O: 1
});
ScalaJS.c.s_reflect_ManifestFactory$.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$;
ScalaJS.n.s_reflect_ManifestFactory$ = (void 0);
ScalaJS.m.s_reflect_ManifestFactory$ = (function() {
  if ((!ScalaJS.n.s_reflect_ManifestFactory$)) {
    ScalaJS.n.s_reflect_ManifestFactory$ = new ScalaJS.c.s_reflect_ManifestFactory$().init___()
  };
  return ScalaJS.n.s_reflect_ManifestFactory$
});
/** @constructor */
ScalaJS.c.s_reflect_package$ = (function() {
  ScalaJS.c.O.call(this);
  this.ClassManifest$1 = null;
  this.Manifest$1 = null
});
ScalaJS.c.s_reflect_package$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_reflect_package$.prototype.constructor = ScalaJS.c.s_reflect_package$;
/** @constructor */
ScalaJS.h.s_reflect_package$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_package$.prototype = ScalaJS.c.s_reflect_package$.prototype;
ScalaJS.c.s_reflect_package$.prototype.init___ = (function() {
  ScalaJS.n.s_reflect_package$ = this;
  this.ClassManifest$1 = ScalaJS.m.s_reflect_ClassManifestFactory$();
  this.Manifest$1 = ScalaJS.m.s_reflect_ManifestFactory$();
  return this
});
ScalaJS.d.s_reflect_package$ = new ScalaJS.ClassTypeData({
  s_reflect_package$: 0
}, false, "scala.reflect.package$", {
  s_reflect_package$: 1,
  O: 1
});
ScalaJS.c.s_reflect_package$.prototype.$classData = ScalaJS.d.s_reflect_package$;
ScalaJS.n.s_reflect_package$ = (void 0);
ScalaJS.m.s_reflect_package$ = (function() {
  if ((!ScalaJS.n.s_reflect_package$)) {
    ScalaJS.n.s_reflect_package$ = new ScalaJS.c.s_reflect_package$().init___()
  };
  return ScalaJS.n.s_reflect_package$
});
/** @constructor */
ScalaJS.c.s_util_DynamicVariable = (function() {
  ScalaJS.c.O.call(this);
  this.scala$util$DynamicVariable$$init$f = null;
  this.tl$1 = null
});
ScalaJS.c.s_util_DynamicVariable.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_DynamicVariable.prototype.constructor = ScalaJS.c.s_util_DynamicVariable;
/** @constructor */
ScalaJS.h.s_util_DynamicVariable = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_DynamicVariable.prototype = ScalaJS.c.s_util_DynamicVariable.prototype;
ScalaJS.c.s_util_DynamicVariable.prototype.toString__T = (function() {
  return (("DynamicVariable(" + this.tl$1.get__O()) + ")")
});
ScalaJS.c.s_util_DynamicVariable.prototype.init___O = (function(init) {
  this.scala$util$DynamicVariable$$init$f = init;
  this.tl$1 = new ScalaJS.c.s_util_DynamicVariable$$anon$1().init___s_util_DynamicVariable(this);
  return this
});
ScalaJS.d.s_util_DynamicVariable = new ScalaJS.ClassTypeData({
  s_util_DynamicVariable: 0
}, false, "scala.util.DynamicVariable", {
  s_util_DynamicVariable: 1,
  O: 1
});
ScalaJS.c.s_util_DynamicVariable.prototype.$classData = ScalaJS.d.s_util_DynamicVariable;
/** @constructor */
ScalaJS.c.s_util_Either$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_util_Either$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_Either$.prototype.constructor = ScalaJS.c.s_util_Either$;
/** @constructor */
ScalaJS.h.s_util_Either$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_Either$.prototype = ScalaJS.c.s_util_Either$.prototype;
ScalaJS.d.s_util_Either$ = new ScalaJS.ClassTypeData({
  s_util_Either$: 0
}, false, "scala.util.Either$", {
  s_util_Either$: 1,
  O: 1
});
ScalaJS.c.s_util_Either$.prototype.$classData = ScalaJS.d.s_util_Either$;
ScalaJS.n.s_util_Either$ = (void 0);
ScalaJS.m.s_util_Either$ = (function() {
  if ((!ScalaJS.n.s_util_Either$)) {
    ScalaJS.n.s_util_Either$ = new ScalaJS.c.s_util_Either$().init___()
  };
  return ScalaJS.n.s_util_Either$
});
/** @constructor */
ScalaJS.c.s_util_control_Breaks = (function() {
  ScalaJS.c.O.call(this);
  this.scala$util$control$Breaks$$breakException$1 = null
});
ScalaJS.c.s_util_control_Breaks.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_control_Breaks.prototype.constructor = ScalaJS.c.s_util_control_Breaks;
/** @constructor */
ScalaJS.h.s_util_control_Breaks = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_control_Breaks.prototype = ScalaJS.c.s_util_control_Breaks.prototype;
ScalaJS.c.s_util_control_Breaks.prototype.init___ = (function() {
  this.scala$util$control$Breaks$$breakException$1 = new ScalaJS.c.s_util_control_BreakControl().init___();
  return this
});
ScalaJS.d.s_util_control_Breaks = new ScalaJS.ClassTypeData({
  s_util_control_Breaks: 0
}, false, "scala.util.control.Breaks", {
  s_util_control_Breaks: 1,
  O: 1
});
ScalaJS.c.s_util_control_Breaks.prototype.$classData = ScalaJS.d.s_util_control_Breaks;
ScalaJS.s.s_util_control_NoStackTrace$class__fillInStackTrace__s_util_control_NoStackTrace__jl_Throwable = (function($$this) {
  var this$1 = ScalaJS.m.s_util_control_NoStackTrace$();
  if (this$1.$$undnoSuppression$1) {
    return $$this.scala$util$control$NoStackTrace$$super$fillInStackTrace__jl_Throwable()
  } else {
    return ScalaJS.as.jl_Throwable($$this)
  }
});
/** @constructor */
ScalaJS.c.s_util_hashing_MurmurHash3 = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.constructor = ScalaJS.c.s_util_hashing_MurmurHash3;
/** @constructor */
ScalaJS.h.s_util_hashing_MurmurHash3 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_hashing_MurmurHash3.prototype = ScalaJS.c.s_util_hashing_MurmurHash3.prototype;
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.mixLast__I__I__I = (function(hash, data) {
  var k = data;
  k = ScalaJS.imul((-862048943), k);
  k = ScalaJS.m.jl_Integer$().rotateLeft__I__I__I(k, 15);
  k = ScalaJS.imul(461845907, k);
  return (hash ^ k)
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.mix__I__I__I = (function(hash, data) {
  var h = this.mixLast__I__I__I(hash, data);
  h = ScalaJS.m.jl_Integer$().rotateLeft__I__I__I(h, 13);
  return (((-430675100) + ScalaJS.imul(5, h)) | 0)
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.avalanche__p1__I__I = (function(hash) {
  var h = hash;
  h = (h ^ ((h >>> 16) | 0));
  h = ScalaJS.imul((-2048144789), h);
  h = (h ^ ((h >>> 13) | 0));
  h = ScalaJS.imul((-1028477387), h);
  h = (h ^ ((h >>> 16) | 0));
  return h
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.unorderedHash__sc_TraversableOnce__I__I = (function(xs, seed) {
  var a = new ScalaJS.c.sr_IntRef().init___I(0);
  var b = new ScalaJS.c.sr_IntRef().init___I(0);
  var n = new ScalaJS.c.sr_IntRef().init___I(0);
  var c = new ScalaJS.c.sr_IntRef().init___I(1);
  xs.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2$1, a$1, b$1, n$1, c$1) {
    return (function(x$2) {
      var h = ScalaJS.m.sr_ScalaRunTime$().hash__O__I(x$2);
      a$1.elem$1 = ((a$1.elem$1 + h) | 0);
      b$1.elem$1 = (b$1.elem$1 ^ h);
      if ((h !== 0)) {
        c$1.elem$1 = ScalaJS.imul(c$1.elem$1, h)
      };
      n$1.elem$1 = ((1 + n$1.elem$1) | 0)
    })
  })(this, a, b, n, c)));
  var h$1 = seed;
  h$1 = this.mix__I__I__I(h$1, a.elem$1);
  h$1 = this.mix__I__I__I(h$1, b.elem$1);
  h$1 = this.mixLast__I__I__I(h$1, c.elem$1);
  return this.finalizeHash__I__I__I(h$1, n.elem$1)
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.productHash__s_Product__I__I = (function(x, seed) {
  var arr = x.productArity__I();
  if ((arr === 0)) {
    var this$1 = x.productPrefix__T();
    return ScalaJS.m.sjsr_RuntimeString$().hashCode__T__I(this$1)
  } else {
    var h = seed;
    var i = 0;
    while ((i < arr)) {
      h = this.mix__I__I__I(h, ScalaJS.m.sr_ScalaRunTime$().hash__O__I(x.productElement__I__O(i)));
      i = ((1 + i) | 0)
    };
    return this.finalizeHash__I__I__I(h, arr)
  }
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.finalizeHash__I__I__I = (function(hash, length) {
  return this.avalanche__p1__I__I((hash ^ length))
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.orderedHash__sc_TraversableOnce__I__I = (function(xs, seed) {
  var n = new ScalaJS.c.sr_IntRef().init___I(0);
  var h = new ScalaJS.c.sr_IntRef().init___I(seed);
  xs.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2$1, n$1, h$1) {
    return (function(x$2) {
      h$1.elem$1 = this$2$1.mix__I__I__I(h$1.elem$1, ScalaJS.m.sr_ScalaRunTime$().hash__O__I(x$2));
      n$1.elem$1 = ((1 + n$1.elem$1) | 0)
    })
  })(this, n, h)));
  return this.finalizeHash__I__I__I(h.elem$1, n.elem$1)
});
ScalaJS.c.s_util_hashing_MurmurHash3.prototype.listHash__sci_List__I__I = (function(xs, seed) {
  var n = 0;
  var h = seed;
  var elems = xs;
  while ((!elems.isEmpty__Z())) {
    var head = elems.head__O();
    var tail = ScalaJS.as.sci_List(elems.tail__O());
    h = this.mix__I__I__I(h, ScalaJS.m.sr_ScalaRunTime$().hash__O__I(head));
    n = ((1 + n) | 0);
    elems = tail
  };
  return this.finalizeHash__I__I__I(h, n)
});
/** @constructor */
ScalaJS.c.s_util_hashing_package$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_util_hashing_package$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_hashing_package$.prototype.constructor = ScalaJS.c.s_util_hashing_package$;
/** @constructor */
ScalaJS.h.s_util_hashing_package$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_hashing_package$.prototype = ScalaJS.c.s_util_hashing_package$.prototype;
ScalaJS.c.s_util_hashing_package$.prototype.byteswap32__I__I = (function(v) {
  var hc = ScalaJS.imul((-1640532531), v);
  hc = ScalaJS.m.jl_Integer$().reverseBytes__I__I(hc);
  return ScalaJS.imul((-1640532531), hc)
});
ScalaJS.d.s_util_hashing_package$ = new ScalaJS.ClassTypeData({
  s_util_hashing_package$: 0
}, false, "scala.util.hashing.package$", {
  s_util_hashing_package$: 1,
  O: 1
});
ScalaJS.c.s_util_hashing_package$.prototype.$classData = ScalaJS.d.s_util_hashing_package$;
ScalaJS.n.s_util_hashing_package$ = (void 0);
ScalaJS.m.s_util_hashing_package$ = (function() {
  if ((!ScalaJS.n.s_util_hashing_package$)) {
    ScalaJS.n.s_util_hashing_package$ = new ScalaJS.c.s_util_hashing_package$().init___()
  };
  return ScalaJS.n.s_util_hashing_package$
});
/** @constructor */
ScalaJS.c.sc_$colon$plus$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sc_$colon$plus$.prototype = new ScalaJS.h.O();
ScalaJS.c.sc_$colon$plus$.prototype.constructor = ScalaJS.c.sc_$colon$plus$;
/** @constructor */
ScalaJS.h.sc_$colon$plus$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_$colon$plus$.prototype = ScalaJS.c.sc_$colon$plus$.prototype;
ScalaJS.d.sc_$colon$plus$ = new ScalaJS.ClassTypeData({
  sc_$colon$plus$: 0
}, false, "scala.collection.$colon$plus$", {
  sc_$colon$plus$: 1,
  O: 1
});
ScalaJS.c.sc_$colon$plus$.prototype.$classData = ScalaJS.d.sc_$colon$plus$;
ScalaJS.n.sc_$colon$plus$ = (void 0);
ScalaJS.m.sc_$colon$plus$ = (function() {
  if ((!ScalaJS.n.sc_$colon$plus$)) {
    ScalaJS.n.sc_$colon$plus$ = new ScalaJS.c.sc_$colon$plus$().init___()
  };
  return ScalaJS.n.sc_$colon$plus$
});
/** @constructor */
ScalaJS.c.sc_$plus$colon$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sc_$plus$colon$.prototype = new ScalaJS.h.O();
ScalaJS.c.sc_$plus$colon$.prototype.constructor = ScalaJS.c.sc_$plus$colon$;
/** @constructor */
ScalaJS.h.sc_$plus$colon$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_$plus$colon$.prototype = ScalaJS.c.sc_$plus$colon$.prototype;
ScalaJS.d.sc_$plus$colon$ = new ScalaJS.ClassTypeData({
  sc_$plus$colon$: 0
}, false, "scala.collection.$plus$colon$", {
  sc_$plus$colon$: 1,
  O: 1
});
ScalaJS.c.sc_$plus$colon$.prototype.$classData = ScalaJS.d.sc_$plus$colon$;
ScalaJS.n.sc_$plus$colon$ = (void 0);
ScalaJS.m.sc_$plus$colon$ = (function() {
  if ((!ScalaJS.n.sc_$plus$colon$)) {
    ScalaJS.n.sc_$plus$colon$ = new ScalaJS.c.sc_$plus$colon$().init___()
  };
  return ScalaJS.n.sc_$plus$colon$
});
ScalaJS.s.sc_GenMapLike$class__liftedTree1$1__p0__sc_GenMapLike__sc_GenMap__Z = (function($$this, x2$1) {
  try {
    var this$1 = $$this.iterator__sc_Iterator();
    var res = true;
    while ((res && this$1.hasNext__Z())) {
      var arg1 = this$1.next__O();
      var x0$1 = ScalaJS.as.T2(arg1);
      if ((x0$1 !== null)) {
        var k = x0$1.$$und1__O();
        var v = x0$1.$$und2__O();
        var x1$2 = x2$1.get__O__s_Option(k);
        matchEnd6: {
          if (ScalaJS.is.s_Some(x1$2)) {
            var x2 = ScalaJS.as.s_Some(x1$2);
            var p3 = x2.x$2;
            if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(v, p3)) {
              res = true;
              break matchEnd6
            }
          };
          res = false;
          break matchEnd6
        }
      } else {
        throw new ScalaJS.c.s_MatchError().init___O(x0$1)
      }
    };
    return res
  } catch (e) {
    if (ScalaJS.is.jl_ClassCastException(e)) {
      ScalaJS.as.jl_ClassCastException(e);
      var this$3 = ScalaJS.m.s_Console$();
      var this$4 = this$3.outVar$2;
      ScalaJS.as.Ljava_io_PrintStream(this$4.tl$1.get__O()).println__O__V("class cast ");
      return false
    } else {
      throw e
    }
  }
});
ScalaJS.s.sc_GenMapLike$class__equals__sc_GenMapLike__O__Z = (function($$this, that) {
  if (ScalaJS.is.sc_GenMap(that)) {
    var x2 = ScalaJS.as.sc_GenMap(that);
    return (($$this === x2) || (($$this.size__I() === x2.size__I()) && ScalaJS.s.sc_GenMapLike$class__liftedTree1$1__p0__sc_GenMapLike__sc_GenMap__Z($$this, x2)))
  } else {
    return false
  }
});
ScalaJS.s.sc_GenSeqLike$class__equals__sc_GenSeqLike__O__Z = (function($$this, that) {
  if (ScalaJS.is.sc_GenSeq(that)) {
    var x2 = ScalaJS.as.sc_GenSeq(that);
    return $$this.sameElements__sc_GenIterable__Z(x2)
  } else {
    return false
  }
});
ScalaJS.s.sc_GenSeqLike$class__isDefinedAt__sc_GenSeqLike__I__Z = (function($$this, idx) {
  return ((idx >= 0) && (idx < $$this.length__I()))
});
ScalaJS.s.sc_GenSetLike$class__liftedTree1$1__p0__sc_GenSetLike__sc_GenSet__Z = (function($$this, x2$1) {
  try {
    return $$this.subsetOf__sc_GenSet__Z(x2$1)
  } catch (e) {
    if (ScalaJS.is.jl_ClassCastException(e)) {
      ScalaJS.as.jl_ClassCastException(e);
      return false
    } else {
      throw e
    }
  }
});
ScalaJS.s.sc_GenSetLike$class__equals__sc_GenSetLike__O__Z = (function($$this, that) {
  if (ScalaJS.is.sc_GenSet(that)) {
    var x2 = ScalaJS.as.sc_GenSet(that);
    return (($$this === x2) || (($$this.size__I() === x2.size__I()) && ScalaJS.s.sc_GenSetLike$class__liftedTree1$1__p0__sc_GenSetLike__sc_GenSet__Z($$this, x2)))
  } else {
    return false
  }
});
ScalaJS.s.sc_IndexedSeqOptimized$class__lengthCompare__sc_IndexedSeqOptimized__I__I = (function($$this, len) {
  return (($$this.length__I() - len) | 0)
});
ScalaJS.s.sc_IndexedSeqOptimized$class__slice__sc_IndexedSeqOptimized__I__I__O = (function($$this, from, until) {
  var lo = ((from > 0) ? from : 0);
  var x = ((until > 0) ? until : 0);
  var y = $$this.length__I();
  var hi = ((x < y) ? x : y);
  var x$1 = ((hi - lo) | 0);
  var elems = ((x$1 > 0) ? x$1 : 0);
  var b = $$this.newBuilder__scm_Builder();
  b.sizeHint__I__V(elems);
  var i = lo;
  while ((i < hi)) {
    b.$$plus$eq__O__scm_Builder($$this.apply__I__O(i));
    i = ((1 + i) | 0)
  };
  return b.result__O()
});
ScalaJS.s.sc_IndexedSeqOptimized$class__copyToArray__sc_IndexedSeqOptimized__O__I__I__V = (function($$this, xs, start, len) {
  var i = 0;
  var j = start;
  var $$this$1 = $$this.length__I();
  var $$this$2 = (($$this$1 < len) ? $$this$1 : len);
  var that = ((ScalaJS.m.sr_ScalaRunTime$().array$undlength__O__I(xs) - start) | 0);
  var end = (($$this$2 < that) ? $$this$2 : that);
  while ((i < end)) {
    ScalaJS.m.sr_ScalaRunTime$().array$undupdate__O__I__O__V(xs, j, $$this.apply__I__O(i));
    i = ((1 + i) | 0);
    j = ((1 + j) | 0)
  }
});
ScalaJS.s.sc_IndexedSeqOptimized$class__sameElements__sc_IndexedSeqOptimized__sc_GenIterable__Z = (function($$this, that) {
  if (ScalaJS.is.sc_IndexedSeq(that)) {
    var x2 = ScalaJS.as.sc_IndexedSeq(that);
    var len = $$this.length__I();
    if ((len === x2.length__I())) {
      var i = 0;
      while (((i < len) && ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z($$this.apply__I__O(i), x2.apply__I__O(i)))) {
        i = ((1 + i) | 0)
      };
      return (i === len)
    } else {
      return false
    }
  } else {
    return ScalaJS.s.sc_IterableLike$class__sameElements__sc_IterableLike__sc_GenIterable__Z($$this, that)
  }
});
ScalaJS.s.sc_IndexedSeqOptimized$class__foreach__sc_IndexedSeqOptimized__F1__V = (function($$this, f) {
  var i = 0;
  var len = $$this.length__I();
  while ((i < len)) {
    f.apply__O__O($$this.apply__I__O(i));
    i = ((1 + i) | 0)
  }
});
ScalaJS.s.sc_IndexedSeqOptimized$class__reverse__sc_IndexedSeqOptimized__O = (function($$this) {
  var b = $$this.newBuilder__scm_Builder();
  b.sizeHint__I__V($$this.length__I());
  var i = $$this.length__I();
  while ((i > 0)) {
    i = (((-1) + i) | 0);
    b.$$plus$eq__O__scm_Builder($$this.apply__I__O(i))
  };
  return b.result__O()
});
ScalaJS.s.sc_IndexedSeqOptimized$class__tail__sc_IndexedSeqOptimized__O = (function($$this) {
  return (ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z($$this) ? ScalaJS.s.sc_TraversableLike$class__tail__sc_TraversableLike__O($$this) : $$this.slice__I__I__O(1, $$this.length__I()))
});
ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z = (function($$this) {
  return ($$this.length__I() === 0)
});
ScalaJS.s.sc_IndexedSeqOptimized$class__head__sc_IndexedSeqOptimized__O = (function($$this) {
  return (ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z($$this) ? new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I($$this, 0, $$this.length__I()).next__O() : $$this.apply__I__O(0))
});
ScalaJS.s.sc_IterableLike$class__drop__sc_IterableLike__I__O = (function($$this, n) {
  var b = $$this.newBuilder__scm_Builder();
  var lo = ((n < 0) ? 0 : n);
  var delta = ((-lo) | 0);
  ScalaJS.s.scm_Builder$class__sizeHint__scm_Builder__sc_TraversableLike__I__V(b, $$this, delta);
  var i = 0;
  var it = $$this.iterator__sc_Iterator();
  while (((i < n) && it.hasNext__Z())) {
    it.next__O();
    i = ((1 + i) | 0)
  };
  return ScalaJS.as.scm_Builder(b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable(it)).result__O()
});
ScalaJS.s.sc_IterableLike$class__copyToArray__sc_IterableLike__O__I__I__V = (function($$this, xs, start, len) {
  var i = start;
  var $$this$1 = ((start + len) | 0);
  var that = ScalaJS.m.sr_ScalaRunTime$().array$undlength__O__I(xs);
  var end = (($$this$1 < that) ? $$this$1 : that);
  var it = $$this.iterator__sc_Iterator();
  while (((i < end) && it.hasNext__Z())) {
    ScalaJS.m.sr_ScalaRunTime$().array$undupdate__O__I__O__V(xs, i, it.next__O());
    i = ((1 + i) | 0)
  }
});
ScalaJS.s.sc_IterableLike$class__take__sc_IterableLike__I__O = (function($$this, n) {
  var b = $$this.newBuilder__scm_Builder();
  if ((n <= 0)) {
    return b.result__O()
  } else {
    b.sizeHintBounded__I__sc_TraversableLike__V(n, $$this);
    var i = 0;
    var it = $$this.iterator__sc_Iterator();
    while (((i < n) && it.hasNext__Z())) {
      b.$$plus$eq__O__scm_Builder(it.next__O());
      i = ((1 + i) | 0)
    };
    return b.result__O()
  }
});
ScalaJS.s.sc_IterableLike$class__sameElements__sc_IterableLike__sc_GenIterable__Z = (function($$this, that) {
  var these = $$this.iterator__sc_Iterator();
  var those = that.iterator__sc_Iterator();
  while ((these.hasNext__Z() && those.hasNext__Z())) {
    if ((!ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(these.next__O(), those.next__O()))) {
      return false
    }
  };
  return ((!these.hasNext__Z()) && (!those.hasNext__Z()))
});
/** @constructor */
ScalaJS.c.sc_Iterator$ = (function() {
  ScalaJS.c.O.call(this);
  this.empty$1 = null
});
ScalaJS.c.sc_Iterator$.prototype = new ScalaJS.h.O();
ScalaJS.c.sc_Iterator$.prototype.constructor = ScalaJS.c.sc_Iterator$;
/** @constructor */
ScalaJS.h.sc_Iterator$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_Iterator$.prototype = ScalaJS.c.sc_Iterator$.prototype;
ScalaJS.c.sc_Iterator$.prototype.init___ = (function() {
  ScalaJS.n.sc_Iterator$ = this;
  this.empty$1 = new ScalaJS.c.sc_Iterator$$anon$2().init___();
  return this
});
ScalaJS.d.sc_Iterator$ = new ScalaJS.ClassTypeData({
  sc_Iterator$: 0
}, false, "scala.collection.Iterator$", {
  sc_Iterator$: 1,
  O: 1
});
ScalaJS.c.sc_Iterator$.prototype.$classData = ScalaJS.d.sc_Iterator$;
ScalaJS.n.sc_Iterator$ = (void 0);
ScalaJS.m.sc_Iterator$ = (function() {
  if ((!ScalaJS.n.sc_Iterator$)) {
    ScalaJS.n.sc_Iterator$ = new ScalaJS.c.sc_Iterator$().init___()
  };
  return ScalaJS.n.sc_Iterator$
});
ScalaJS.s.sc_Iterator$class__toStream__sc_Iterator__sci_Stream = (function($$this) {
  if ($$this.hasNext__Z()) {
    var hd = $$this.next__O();
    var tl = new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function($$this$1) {
      return (function() {
        return $$this$1.toStream__sci_Stream()
      })
    })($$this));
    return new ScalaJS.c.sci_Stream$Cons().init___O__F0(hd, tl)
  } else {
    ScalaJS.m.sci_Stream$();
    return ScalaJS.m.sci_Stream$Empty$()
  }
});
ScalaJS.s.sc_Iterator$class__isEmpty__sc_Iterator__Z = (function($$this) {
  return (!$$this.hasNext__Z())
});
ScalaJS.s.sc_Iterator$class__toString__sc_Iterator__T = (function($$this) {
  return (($$this.hasNext__Z() ? "non-empty" : "empty") + " iterator")
});
ScalaJS.s.sc_Iterator$class__foreach__sc_Iterator__F1__V = (function($$this, f) {
  while ($$this.hasNext__Z()) {
    f.apply__O__O($$this.next__O())
  }
});
ScalaJS.s.sc_Iterator$class__forall__sc_Iterator__F1__Z = (function($$this, p) {
  var res = true;
  while ((res && $$this.hasNext__Z())) {
    res = ScalaJS.uZ(p.apply__O__O($$this.next__O()))
  };
  return res
});
ScalaJS.s.sc_LinearSeqOptimized$class__isDefinedAt__sc_LinearSeqOptimized__I__Z = (function($$this, x) {
  return ((x >= 0) && (ScalaJS.s.sc_LinearSeqOptimized$class__lengthCompare__sc_LinearSeqOptimized__I__I($$this, x) > 0))
});
ScalaJS.s.sc_LinearSeqOptimized$class__lengthCompare__sc_LinearSeqOptimized__I__I = (function($$this, len) {
  return ((len < 0) ? 1 : ScalaJS.s.sc_LinearSeqOptimized$class__loop$1__p0__sc_LinearSeqOptimized__I__sc_LinearSeqOptimized__I__I($$this, 0, $$this, len))
});
ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O = (function($$this, n) {
  var rest = $$this.drop__I__sc_LinearSeqOptimized(n);
  if (((n < 0) || rest.isEmpty__Z())) {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + n))
  };
  return rest.head__O()
});
ScalaJS.s.sc_LinearSeqOptimized$class__loop$1__p0__sc_LinearSeqOptimized__I__sc_LinearSeqOptimized__I__I = (function($$this, i, xs, len$1) {
  _loop: while (true) {
    if ((i === len$1)) {
      return (xs.isEmpty__Z() ? 0 : 1)
    } else if (xs.isEmpty__Z()) {
      return (-1)
    } else {
      var temp$i = ((1 + i) | 0);
      var temp$xs = ScalaJS.as.sc_LinearSeqOptimized(xs.tail__O());
      i = temp$i;
      xs = temp$xs;
      continue _loop
    }
  }
});
ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I = (function($$this) {
  var these = $$this;
  var len = 0;
  while ((!these.isEmpty__Z())) {
    len = ((1 + len) | 0);
    these = ScalaJS.as.sc_LinearSeqOptimized(these.tail__O())
  };
  return len
});
ScalaJS.s.sc_LinearSeqOptimized$class__last__sc_LinearSeqOptimized__O = (function($$this) {
  if ($$this.isEmpty__Z()) {
    throw new ScalaJS.c.ju_NoSuchElementException().init___()
  };
  var these = $$this;
  var nx = ScalaJS.as.sc_LinearSeqOptimized(these.tail__O());
  while ((!nx.isEmpty__Z())) {
    these = nx;
    nx = ScalaJS.as.sc_LinearSeqOptimized(nx.tail__O())
  };
  return these.head__O()
});
ScalaJS.s.sc_LinearSeqOptimized$class__sameElements__sc_LinearSeqOptimized__sc_GenIterable__Z = (function($$this, that) {
  if (ScalaJS.is.sc_LinearSeq(that)) {
    var x2 = ScalaJS.as.sc_LinearSeq(that);
    if (($$this === x2)) {
      return true
    } else {
      var these = $$this;
      var those = x2;
      while ((((!these.isEmpty__Z()) && (!those.isEmpty__Z())) && ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(these.head__O(), those.head__O()))) {
        these = ScalaJS.as.sc_LinearSeqOptimized(these.tail__O());
        those = ScalaJS.as.sc_LinearSeq(those.tail__O())
      };
      return (these.isEmpty__Z() && those.isEmpty__Z())
    }
  } else {
    return ScalaJS.s.sc_IterableLike$class__sameElements__sc_IterableLike__sc_GenIterable__Z($$this, that)
  }
});
ScalaJS.s.sc_MapLike$class__addString__sc_MapLike__scm_StringBuilder__T__T__T__scm_StringBuilder = (function($$this, b, start, sep, end) {
  var this$2 = $$this.iterator__sc_Iterator();
  var f = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1) {
    return (function(x0$1$2) {
      var x0$1 = ScalaJS.as.T2(x0$1$2);
      if ((x0$1 !== null)) {
        var k = x0$1.$$und1__O();
        var v = x0$1.$$und2__O();
        return (("" + ScalaJS.m.s_Predef$any2stringadd$().$$plus$extension__O__T__T(k, " -> ")) + v)
      } else {
        throw new ScalaJS.c.s_MatchError().init___O(x0$1)
      }
    })
  })($$this));
  var this$3 = new ScalaJS.c.sc_Iterator$$anon$11().init___sc_Iterator__F1(this$2, f);
  return ScalaJS.s.sc_TraversableOnce$class__addString__sc_TraversableOnce__scm_StringBuilder__T__T__T__scm_StringBuilder(this$3, b, start, sep, end)
});
ScalaJS.s.sc_MapLike$class__apply__sc_MapLike__O__O = (function($$this, key) {
  var x1 = $$this.get__O__s_Option(key);
  var x = ScalaJS.m.s_None$();
  if ((x === x1)) {
    return ScalaJS.s.sc_MapLike$class__$default__sc_MapLike__O__O($$this, key)
  } else if (ScalaJS.is.s_Some(x1)) {
    var x2 = ScalaJS.as.s_Some(x1);
    var value = x2.x$2;
    return value
  } else {
    throw new ScalaJS.c.s_MatchError().init___O(x1)
  }
});
ScalaJS.s.sc_MapLike$class__isEmpty__sc_MapLike__Z = (function($$this) {
  return ($$this.size__I() === 0)
});
ScalaJS.s.sc_MapLike$class__contains__sc_MapLike__O__Z = (function($$this, key) {
  return $$this.get__O__s_Option(key).isDefined__Z()
});
ScalaJS.s.sc_MapLike$class__$default__sc_MapLike__O__O = (function($$this, key) {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T(("key not found: " + key))
});
ScalaJS.s.sc_SeqLike$class__isEmpty__sc_SeqLike__Z = (function($$this) {
  return ($$this.lengthCompare__I__I(0) === 0)
});
ScalaJS.s.sc_SeqLike$class__$$plus$colon__sc_SeqLike__O__scg_CanBuildFrom__O = (function($$this, elem, bf) {
  var b = bf.apply__O__scm_Builder($$this.repr__O());
  b.$$plus$eq__O__scm_Builder(elem);
  b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable($$this.thisCollection__sc_Seq());
  return b.result__O()
});
ScalaJS.s.sc_SeqLike$class__reverse__sc_SeqLike__O = (function($$this) {
  var elem = ScalaJS.m.sci_Nil$();
  var xs = new ScalaJS.c.sr_ObjectRef().init___O(elem);
  $$this.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1, xs$1) {
    return (function(x$2) {
      var this$2 = ScalaJS.as.sci_List(xs$1.elem$1);
      xs$1.elem$1 = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(x$2, this$2)
    })
  })($$this, xs)));
  var b = $$this.newBuilder__scm_Builder();
  ScalaJS.s.scm_Builder$class__sizeHint__scm_Builder__sc_TraversableLike__V(b, $$this);
  var this$3 = ScalaJS.as.sci_List(xs.elem$1);
  var these = this$3;
  while ((!these.isEmpty__Z())) {
    var arg1 = these.head__O();
    b.$$plus$eq__O__scm_Builder(arg1);
    these = ScalaJS.as.sci_List(these.tail__O())
  };
  return b.result__O()
});
ScalaJS.s.sc_SeqLike$class__reverseIterator__sc_SeqLike__sc_Iterator = (function($$this) {
  return $$this.toCollection__O__sc_Seq($$this.reverse__O()).iterator__sc_Iterator()
});
ScalaJS.s.sc_SeqLike$class__$$colon$plus__sc_SeqLike__O__scg_CanBuildFrom__O = (function($$this, elem, bf) {
  var b = bf.apply__O__scm_Builder($$this.repr__O());
  b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable($$this.thisCollection__sc_Seq());
  b.$$plus$eq__O__scm_Builder(elem);
  return b.result__O()
});
ScalaJS.s.sc_SeqLike$class__lengthCompare__sc_SeqLike__I__I = (function($$this, len) {
  if ((len < 0)) {
    return 1
  } else {
    var i = 0;
    var it = $$this.iterator__sc_Iterator();
    while (it.hasNext__Z()) {
      if ((i === len)) {
        return (it.hasNext__Z() ? 1 : 0)
      };
      it.next__O();
      i = ((1 + i) | 0)
    };
    return ((i - len) | 0)
  }
});
ScalaJS.s.sc_SetLike$class__isEmpty__sc_SetLike__Z = (function($$this) {
  return ($$this.size__I() === 0)
});
ScalaJS.s.sc_TraversableLike$class__collect__sc_TraversableLike__s_PartialFunction__scg_CanBuildFrom__O = (function($$this, pf, bf) {
  var b = bf.apply__O__scm_Builder($$this.repr__O());
  $$this.foreach__F1__V(pf.runWith__F1__F1(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1, b$1) {
    return (function(x$1$2) {
      return b$1.$$plus$eq__O__scm_Builder(x$1$2)
    })
  })($$this, b))));
  return b.result__O()
});
ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O = (function($$this, cbf) {
  var b = cbf.apply__scm_Builder();
  ScalaJS.s.scm_Builder$class__sizeHint__scm_Builder__sc_TraversableLike__V(b, $$this);
  b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable($$this.thisCollection__sc_Traversable());
  return b.result__O()
});
ScalaJS.s.sc_TraversableLike$class__toString__sc_TraversableLike__T = (function($$this) {
  return $$this.mkString__T__T__T__T(($$this.stringPrefix__T() + "("), ", ", ")")
});
ScalaJS.s.sc_TraversableLike$class__flatMap__sc_TraversableLike__F1__scg_CanBuildFrom__O = (function($$this, f, bf) {
  var b = bf.apply__O__scm_Builder($$this.repr__O());
  $$this.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1, b$1, f$1) {
    return (function(x$2) {
      return ScalaJS.as.scm_Builder(b$1.$$plus$plus$eq__sc_TraversableOnce__scg_Growable(ScalaJS.as.sc_GenTraversableOnce(f$1.apply__O__O(x$2)).seq__sc_TraversableOnce()))
    })
  })($$this, b, f)));
  return b.result__O()
});
ScalaJS.s.sc_TraversableLike$class__map__sc_TraversableLike__F1__scg_CanBuildFrom__O = (function($$this, f, bf) {
  var b = ScalaJS.s.sc_TraversableLike$class__builder$1__p0__sc_TraversableLike__scg_CanBuildFrom__scm_Builder($$this, bf);
  $$this.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1, b$1, f$1) {
    return (function(x$2) {
      return b$1.$$plus$eq__O__scm_Builder(f$1.apply__O__O(x$2))
    })
  })($$this, b, f)));
  return b.result__O()
});
ScalaJS.s.sc_TraversableLike$class__tail__sc_TraversableLike__O = (function($$this) {
  if ($$this.isEmpty__Z()) {
    throw new ScalaJS.c.jl_UnsupportedOperationException().init___T("empty.tail")
  };
  return $$this.drop__I__O(1)
});
ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O = (function($$this, that, bf) {
  var b = bf.apply__O__scm_Builder($$this.repr__O());
  if (ScalaJS.is.sc_IndexedSeqLike(that)) {
    var delta = that.seq__sc_TraversableOnce().size__I();
    ScalaJS.s.scm_Builder$class__sizeHint__scm_Builder__sc_TraversableLike__I__V(b, $$this, delta)
  };
  b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable($$this.thisCollection__sc_Traversable());
  b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable(that.seq__sc_TraversableOnce());
  return b.result__O()
});
ScalaJS.s.sc_TraversableLike$class__builder$1__p0__sc_TraversableLike__scg_CanBuildFrom__scm_Builder = (function($$this, bf$1) {
  var b = bf$1.apply__O__scm_Builder($$this.repr__O());
  ScalaJS.s.scm_Builder$class__sizeHint__scm_Builder__sc_TraversableLike__V(b, $$this);
  return b
});
ScalaJS.s.sc_TraversableLike$class__stringPrefix__sc_TraversableLike__T = (function($$this) {
  var string = ScalaJS.objectGetClass($$this.repr__O()).getName__T();
  var idx1 = ScalaJS.m.sjsr_RuntimeString$().lastIndexOf__T__I__I(string, 46);
  if ((idx1 !== (-1))) {
    var thiz = string;
    var beginIndex = ((1 + idx1) | 0);
    string = ScalaJS.as.T(thiz["substring"](beginIndex))
  };
  var idx2 = ScalaJS.m.sjsr_RuntimeString$().indexOf__T__I__I(string, 36);
  if ((idx2 !== (-1))) {
    var thiz$1 = string;
    string = ScalaJS.as.T(thiz$1["substring"](0, idx2))
  };
  return string
});
ScalaJS.is.sc_TraversableOnce = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_TraversableOnce)))
});
ScalaJS.as.sc_TraversableOnce = (function(obj) {
  return ((ScalaJS.is.sc_TraversableOnce(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.TraversableOnce"))
});
ScalaJS.isArrayOf.sc_TraversableOnce = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_TraversableOnce)))
});
ScalaJS.asArrayOf.sc_TraversableOnce = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_TraversableOnce(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.TraversableOnce;", depth))
});
ScalaJS.s.sc_TraversableOnce$class__to__sc_TraversableOnce__scg_CanBuildFrom__O = (function($$this, cbf) {
  var b = cbf.apply__scm_Builder();
  b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable($$this.seq__sc_TraversableOnce());
  return b.result__O()
});
ScalaJS.s.sc_TraversableOnce$class__addString__sc_TraversableOnce__scm_StringBuilder__T__T__T__scm_StringBuilder = (function($$this, b, start, sep, end) {
  var first = new ScalaJS.c.sr_BooleanRef().init___Z(true);
  b.append__T__scm_StringBuilder(start);
  $$this.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1, first$1, b$1, sep$1) {
    return (function(x$2) {
      if (first$1.elem$1) {
        b$1.append__O__scm_StringBuilder(x$2);
        first$1.elem$1 = false;
        return (void 0)
      } else {
        b$1.append__T__scm_StringBuilder(sep$1);
        return b$1.append__O__scm_StringBuilder(x$2)
      }
    })
  })($$this, first, b, sep)));
  b.append__T__scm_StringBuilder(end);
  return b
});
ScalaJS.s.sc_TraversableOnce$class__mkString__sc_TraversableOnce__T__T__T__T = (function($$this, start, sep, end) {
  var this$1 = $$this.addString__scm_StringBuilder__T__T__T__scm_StringBuilder(new ScalaJS.c.scm_StringBuilder().init___(), start, sep, end);
  var this$2 = this$1.underlying$5;
  return this$2.content$1
});
ScalaJS.s.sc_TraversableOnce$class__nonEmpty__sc_TraversableOnce__Z = (function($$this) {
  return (!$$this.isEmpty__Z())
});
ScalaJS.s.sc_TraversableOnce$class__size__sc_TraversableOnce__I = (function($$this) {
  var result = new ScalaJS.c.sr_IntRef().init___I(0);
  $$this.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1, result$1) {
    return (function(x$2) {
      result$1.elem$1 = ((1 + result$1.elem$1) | 0)
    })
  })($$this, result)));
  return result.elem$1
});
/** @constructor */
ScalaJS.c.scg_GenMapFactory = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.scg_GenMapFactory.prototype = new ScalaJS.h.O();
ScalaJS.c.scg_GenMapFactory.prototype.constructor = ScalaJS.c.scg_GenMapFactory;
/** @constructor */
ScalaJS.h.scg_GenMapFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenMapFactory.prototype = ScalaJS.c.scg_GenMapFactory.prototype;
ScalaJS.c.scg_GenMapFactory.prototype.apply__sc_Seq__sc_GenMap = (function(elems) {
  return ScalaJS.as.sc_GenMap(ScalaJS.as.scm_Builder(this.newBuilder__scm_Builder().$$plus$plus$eq__sc_TraversableOnce__scg_Growable(elems)).result__O())
});
ScalaJS.c.scg_GenMapFactory.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_MapBuilder().init___sc_GenMap(this.empty__sc_GenMap())
});
/** @constructor */
ScalaJS.c.scg_GenericCompanion = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.scg_GenericCompanion.prototype = new ScalaJS.h.O();
ScalaJS.c.scg_GenericCompanion.prototype.constructor = ScalaJS.c.scg_GenericCompanion;
/** @constructor */
ScalaJS.h.scg_GenericCompanion = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenericCompanion.prototype = ScalaJS.c.scg_GenericCompanion.prototype;
ScalaJS.c.scg_GenericCompanion.prototype.apply__sc_Seq__sc_GenTraversable = (function(elems) {
  if (elems.isEmpty__Z()) {
    return this.empty__sc_GenTraversable()
  } else {
    var b = this.newBuilder__scm_Builder();
    b.$$plus$plus$eq__sc_TraversableOnce__scg_Growable(elems);
    return ScalaJS.as.sc_GenTraversable(b.result__O())
  }
});
ScalaJS.c.scg_GenericCompanion.prototype.empty__sc_GenTraversable = (function() {
  return ScalaJS.as.sc_GenTraversable(this.newBuilder__scm_Builder().result__O())
});
ScalaJS.is.scg_Growable = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scg_Growable)))
});
ScalaJS.as.scg_Growable = (function(obj) {
  return ((ScalaJS.is.scg_Growable(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.generic.Growable"))
});
ScalaJS.isArrayOf.scg_Growable = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scg_Growable)))
});
ScalaJS.asArrayOf.scg_Growable = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scg_Growable(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.generic.Growable;", depth))
});
ScalaJS.s.scg_Growable$class__loop$1__p0__scg_Growable__sc_LinearSeq__V = (function($$this, xs) {
  x: {
    _loop: while (true) {
      var this$1 = xs;
      if (ScalaJS.s.sc_TraversableOnce$class__nonEmpty__sc_TraversableOnce__Z(this$1)) {
        $$this.$$plus$eq__O__scg_Growable(xs.head__O());
        xs = ScalaJS.as.sc_LinearSeq(xs.tail__O());
        continue _loop
      };
      break x
    }
  }
});
ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable = (function($$this, xs) {
  if (ScalaJS.is.sc_LinearSeq(xs)) {
    var x2 = ScalaJS.as.sc_LinearSeq(xs);
    ScalaJS.s.scg_Growable$class__loop$1__p0__scg_Growable__sc_LinearSeq__V($$this, x2)
  } else {
    xs.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function($$this$1) {
      return (function(elem$2) {
        return $$this$1.$$plus$eq__O__scg_Growable(elem$2)
      })
    })($$this)))
  };
  return $$this
});
/** @constructor */
ScalaJS.c.sci_HashMap$Merger = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sci_HashMap$Merger.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_HashMap$Merger.prototype.constructor = ScalaJS.c.sci_HashMap$Merger;
/** @constructor */
ScalaJS.h.sci_HashMap$Merger = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$Merger.prototype = ScalaJS.c.sci_HashMap$Merger.prototype;
/** @constructor */
ScalaJS.c.sci_Stream$$hash$colon$colon$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sci_Stream$$hash$colon$colon$.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_Stream$$hash$colon$colon$.prototype.constructor = ScalaJS.c.sci_Stream$$hash$colon$colon$;
/** @constructor */
ScalaJS.h.sci_Stream$$hash$colon$colon$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream$$hash$colon$colon$.prototype = ScalaJS.c.sci_Stream$$hash$colon$colon$.prototype;
ScalaJS.d.sci_Stream$$hash$colon$colon$ = new ScalaJS.ClassTypeData({
  sci_Stream$$hash$colon$colon$: 0
}, false, "scala.collection.immutable.Stream$$hash$colon$colon$", {
  sci_Stream$$hash$colon$colon$: 1,
  O: 1
});
ScalaJS.c.sci_Stream$$hash$colon$colon$.prototype.$classData = ScalaJS.d.sci_Stream$$hash$colon$colon$;
ScalaJS.n.sci_Stream$$hash$colon$colon$ = (void 0);
ScalaJS.m.sci_Stream$$hash$colon$colon$ = (function() {
  if ((!ScalaJS.n.sci_Stream$$hash$colon$colon$)) {
    ScalaJS.n.sci_Stream$$hash$colon$colon$ = new ScalaJS.c.sci_Stream$$hash$colon$colon$().init___()
  };
  return ScalaJS.n.sci_Stream$$hash$colon$colon$
});
/** @constructor */
ScalaJS.c.sci_Stream$ConsWrapper = (function() {
  ScalaJS.c.O.call(this);
  this.tl$1 = null
});
ScalaJS.c.sci_Stream$ConsWrapper.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_Stream$ConsWrapper.prototype.constructor = ScalaJS.c.sci_Stream$ConsWrapper;
/** @constructor */
ScalaJS.h.sci_Stream$ConsWrapper = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream$ConsWrapper.prototype = ScalaJS.c.sci_Stream$ConsWrapper.prototype;
ScalaJS.c.sci_Stream$ConsWrapper.prototype.init___F0 = (function(tl) {
  this.tl$1 = tl;
  return this
});
ScalaJS.c.sci_Stream$ConsWrapper.prototype.$$hash$colon$colon__O__sci_Stream = (function(hd) {
  var tl = this.tl$1;
  return new ScalaJS.c.sci_Stream$Cons().init___O__F0(hd, tl)
});
ScalaJS.d.sci_Stream$ConsWrapper = new ScalaJS.ClassTypeData({
  sci_Stream$ConsWrapper: 0
}, false, "scala.collection.immutable.Stream$ConsWrapper", {
  sci_Stream$ConsWrapper: 1,
  O: 1
});
ScalaJS.c.sci_Stream$ConsWrapper.prototype.$classData = ScalaJS.d.sci_Stream$ConsWrapper;
/** @constructor */
ScalaJS.c.sci_StreamIterator$LazyCell = (function() {
  ScalaJS.c.O.call(this);
  this.st$1 = null;
  this.v$1 = null;
  this.$$outer$f = null;
  this.bitmap$0$1 = false
});
ScalaJS.c.sci_StreamIterator$LazyCell.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_StreamIterator$LazyCell.prototype.constructor = ScalaJS.c.sci_StreamIterator$LazyCell;
/** @constructor */
ScalaJS.h.sci_StreamIterator$LazyCell = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_StreamIterator$LazyCell.prototype = ScalaJS.c.sci_StreamIterator$LazyCell.prototype;
ScalaJS.c.sci_StreamIterator$LazyCell.prototype.init___sci_StreamIterator__F0 = (function($$outer, st) {
  this.st$1 = st;
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$f = $$outer
  };
  return this
});
ScalaJS.c.sci_StreamIterator$LazyCell.prototype.v$lzycompute__p1__sci_Stream = (function() {
  if ((!this.bitmap$0$1)) {
    this.v$1 = ScalaJS.as.sci_Stream(this.st$1.apply__O());
    this.bitmap$0$1 = true
  };
  this.st$1 = null;
  return this.v$1
});
ScalaJS.c.sci_StreamIterator$LazyCell.prototype.v__sci_Stream = (function() {
  return ((!this.bitmap$0$1) ? this.v$lzycompute__p1__sci_Stream() : this.v$1)
});
ScalaJS.d.sci_StreamIterator$LazyCell = new ScalaJS.ClassTypeData({
  sci_StreamIterator$LazyCell: 0
}, false, "scala.collection.immutable.StreamIterator$LazyCell", {
  sci_StreamIterator$LazyCell: 1,
  O: 1
});
ScalaJS.c.sci_StreamIterator$LazyCell.prototype.$classData = ScalaJS.d.sci_StreamIterator$LazyCell;
ScalaJS.s.sci_StringLike$class__unwrapArg__p0__sci_StringLike__O__O = (function($$this, arg) {
  if (ScalaJS.is.s_math_ScalaNumber(arg)) {
    var x2 = ScalaJS.as.s_math_ScalaNumber(arg);
    return x2.underlying__O()
  } else {
    return arg
  }
});
ScalaJS.s.sci_StringLike$class__slice__sci_StringLike__I__I__O = (function($$this, from, until) {
  var start = ((from > 0) ? from : 0);
  var that = $$this.length__I();
  var end = ((until < that) ? until : that);
  if ((start >= end)) {
    return $$this.newBuilder__scm_Builder().result__O()
  } else {
    var jsx$1 = $$this.newBuilder__scm_Builder();
    var thiz = $$this.toString__T();
    var x = ScalaJS.as.T(thiz["substring"](start, end));
    return ScalaJS.as.scm_Builder(jsx$1.$$plus$plus$eq__sc_TraversableOnce__scg_Growable(new ScalaJS.c.sci_StringOps().init___T(x))).result__O()
  }
});
/** @constructor */
ScalaJS.c.sci_StringOps$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sci_StringOps$.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_StringOps$.prototype.constructor = ScalaJS.c.sci_StringOps$;
/** @constructor */
ScalaJS.h.sci_StringOps$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_StringOps$.prototype = ScalaJS.c.sci_StringOps$.prototype;
ScalaJS.c.sci_StringOps$.prototype.equals$extension__T__O__Z = (function($$this, x$1) {
  if (ScalaJS.is.sci_StringOps(x$1)) {
    var StringOps$1 = ((x$1 === null) ? null : ScalaJS.as.sci_StringOps(x$1).repr$1);
    return ($$this === StringOps$1)
  } else {
    return false
  }
});
ScalaJS.c.sci_StringOps$.prototype.slice$extension__T__I__I__T = (function($$this, from, until) {
  var start = ((from < 0) ? 0 : from);
  if (((until <= start) || (start >= ScalaJS.uI($$this["length"])))) {
    return ""
  };
  var end = ((until > ScalaJS.uI($$this["length"])) ? ScalaJS.uI($$this["length"]) : until);
  return ScalaJS.as.T($$this["substring"](start, end))
});
ScalaJS.d.sci_StringOps$ = new ScalaJS.ClassTypeData({
  sci_StringOps$: 0
}, false, "scala.collection.immutable.StringOps$", {
  sci_StringOps$: 1,
  O: 1
});
ScalaJS.c.sci_StringOps$.prototype.$classData = ScalaJS.d.sci_StringOps$;
ScalaJS.n.sci_StringOps$ = (void 0);
ScalaJS.m.sci_StringOps$ = (function() {
  if ((!ScalaJS.n.sci_StringOps$)) {
    ScalaJS.n.sci_StringOps$ = new ScalaJS.c.sci_StringOps$().init___()
  };
  return ScalaJS.n.sci_StringOps$
});
ScalaJS.s.sci_VectorPointer$class__gotoFreshPosWritable1__sci_VectorPointer__I__I__I__V = (function($$this, oldIndex, newIndex, xor) {
  ScalaJS.s.sci_VectorPointer$class__stabilize__sci_VectorPointer__I__V($$this, oldIndex);
  ScalaJS.s.sci_VectorPointer$class__gotoFreshPosWritable0__sci_VectorPointer__I__I__I__V($$this, oldIndex, newIndex, xor)
});
ScalaJS.s.sci_VectorPointer$class__getElem__sci_VectorPointer__I__I__O = (function($$this, index, xor) {
  if ((xor < 32)) {
    return $$this.display0__AO().u[(31 & index)]
  } else if ((xor < 1024)) {
    return ScalaJS.asArrayOf.O($$this.display1__AO().u[(31 & (index >> 5))], 1).u[(31 & index)]
  } else if ((xor < 32768)) {
    return ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (index >> 10))], 1).u[(31 & (index >> 5))], 1).u[(31 & index)]
  } else if ((xor < 1048576)) {
    return ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (index >> 15))], 1).u[(31 & (index >> 10))], 1).u[(31 & (index >> 5))], 1).u[(31 & index)]
  } else if ((xor < 33554432)) {
    return ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O($$this.display4__AO().u[(31 & (index >> 20))], 1).u[(31 & (index >> 15))], 1).u[(31 & (index >> 10))], 1).u[(31 & (index >> 5))], 1).u[(31 & index)]
  } else if ((xor < 1073741824)) {
    return ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O(ScalaJS.asArrayOf.O($$this.display5__AO().u[(31 & (index >> 25))], 1).u[(31 & (index >> 20))], 1).u[(31 & (index >> 15))], 1).u[(31 & (index >> 10))], 1).u[(31 & (index >> 5))], 1).u[(31 & index)]
  } else {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___()
  }
});
ScalaJS.s.sci_VectorPointer$class__gotoNextBlockStartWritable__sci_VectorPointer__I__I__V = (function($$this, index, xor) {
  if ((xor < 1024)) {
    if (($$this.depth__I() === 1)) {
      $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
      $$this.display1__AO().u[0] = $$this.display0__AO();
      $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
    };
    $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO()
  } else if ((xor < 32768)) {
    if (($$this.depth__I() === 2)) {
      $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
      $$this.display2__AO().u[0] = $$this.display1__AO();
      $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
    };
    $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO()
  } else if ((xor < 1048576)) {
    if (($$this.depth__I() === 3)) {
      $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
      $$this.display3__AO().u[0] = $$this.display2__AO();
      $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
    };
    $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO();
    $$this.display3__AO().u[(31 & (index >> 15))] = $$this.display2__AO()
  } else if ((xor < 33554432)) {
    if (($$this.depth__I() === 4)) {
      $$this.display4$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
      $$this.display4__AO().u[0] = $$this.display3__AO();
      $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
    };
    $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO();
    $$this.display3__AO().u[(31 & (index >> 15))] = $$this.display2__AO();
    $$this.display4__AO().u[(31 & (index >> 20))] = $$this.display3__AO()
  } else if ((xor < 1073741824)) {
    if (($$this.depth__I() === 5)) {
      $$this.display5$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
      $$this.display5__AO().u[0] = $$this.display4__AO();
      $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
    };
    $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display4$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
    $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO();
    $$this.display3__AO().u[(31 & (index >> 15))] = $$this.display2__AO();
    $$this.display4__AO().u[(31 & (index >> 20))] = $$this.display3__AO();
    $$this.display5__AO().u[(31 & (index >> 25))] = $$this.display4__AO()
  } else {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___()
  }
});
ScalaJS.s.sci_VectorPointer$class__gotoPosWritable0__sci_VectorPointer__I__I__V = (function($$this, newIndex, xor) {
  var x1 = (((-1) + $$this.depth__I()) | 0);
  switch (x1) {
    case 5:
      {
        var a = $$this.display5__AO();
        $$this.display5$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a));
        var array = $$this.display5__AO();
        var index = (31 & (newIndex >> 25));
        $$this.display4$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array, index));
        var array$1 = $$this.display4__AO();
        var index$1 = (31 & (newIndex >> 20));
        $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$1, index$1));
        var array$2 = $$this.display3__AO();
        var index$2 = (31 & (newIndex >> 15));
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$2, index$2));
        var array$3 = $$this.display2__AO();
        var index$3 = (31 & (newIndex >> 10));
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$3, index$3));
        var array$4 = $$this.display1__AO();
        var index$4 = (31 & (newIndex >> 5));
        $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$4, index$4));
        break
      };
    case 4:
      {
        var a$1 = $$this.display4__AO();
        $$this.display4$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$1));
        var array$5 = $$this.display4__AO();
        var index$5 = (31 & (newIndex >> 20));
        $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$5, index$5));
        var array$6 = $$this.display3__AO();
        var index$6 = (31 & (newIndex >> 15));
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$6, index$6));
        var array$7 = $$this.display2__AO();
        var index$7 = (31 & (newIndex >> 10));
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$7, index$7));
        var array$8 = $$this.display1__AO();
        var index$8 = (31 & (newIndex >> 5));
        $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$8, index$8));
        break
      };
    case 3:
      {
        var a$2 = $$this.display3__AO();
        $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$2));
        var array$9 = $$this.display3__AO();
        var index$9 = (31 & (newIndex >> 15));
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$9, index$9));
        var array$10 = $$this.display2__AO();
        var index$10 = (31 & (newIndex >> 10));
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$10, index$10));
        var array$11 = $$this.display1__AO();
        var index$11 = (31 & (newIndex >> 5));
        $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$11, index$11));
        break
      };
    case 2:
      {
        var a$3 = $$this.display2__AO();
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$3));
        var array$12 = $$this.display2__AO();
        var index$12 = (31 & (newIndex >> 10));
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$12, index$12));
        var array$13 = $$this.display1__AO();
        var index$13 = (31 & (newIndex >> 5));
        $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$13, index$13));
        break
      };
    case 1:
      {
        var a$4 = $$this.display1__AO();
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$4));
        var array$14 = $$this.display1__AO();
        var index$14 = (31 & (newIndex >> 5));
        $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$14, index$14));
        break
      };
    case 0:
      {
        var a$5 = $$this.display0__AO();
        $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$5));
        break
      };
    default:
      throw new ScalaJS.c.s_MatchError().init___O(x1);
  }
});
ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V = (function($$this) {
  return (void 0)
});
ScalaJS.s.sci_VectorPointer$class__stabilize__sci_VectorPointer__I__V = (function($$this, index) {
  var x1 = (((-1) + $$this.depth__I()) | 0);
  switch (x1) {
    case 5:
      {
        var a = $$this.display5__AO();
        $$this.display5$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a));
        var a$1 = $$this.display4__AO();
        $$this.display4$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$1));
        var a$2 = $$this.display3__AO();
        $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$2));
        var a$3 = $$this.display2__AO();
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$3));
        var a$4 = $$this.display1__AO();
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$4));
        $$this.display5__AO().u[(31 & (index >> 25))] = $$this.display4__AO();
        $$this.display4__AO().u[(31 & (index >> 20))] = $$this.display3__AO();
        $$this.display3__AO().u[(31 & (index >> 15))] = $$this.display2__AO();
        $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO();
        $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
        break
      };
    case 4:
      {
        var a$5 = $$this.display4__AO();
        $$this.display4$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$5));
        var a$6 = $$this.display3__AO();
        $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$6));
        var a$7 = $$this.display2__AO();
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$7));
        var a$8 = $$this.display1__AO();
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$8));
        $$this.display4__AO().u[(31 & (index >> 20))] = $$this.display3__AO();
        $$this.display3__AO().u[(31 & (index >> 15))] = $$this.display2__AO();
        $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO();
        $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
        break
      };
    case 3:
      {
        var a$9 = $$this.display3__AO();
        $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$9));
        var a$10 = $$this.display2__AO();
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$10));
        var a$11 = $$this.display1__AO();
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$11));
        $$this.display3__AO().u[(31 & (index >> 15))] = $$this.display2__AO();
        $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO();
        $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
        break
      };
    case 2:
      {
        var a$12 = $$this.display2__AO();
        $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$12));
        var a$13 = $$this.display1__AO();
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$13));
        $$this.display2__AO().u[(31 & (index >> 10))] = $$this.display1__AO();
        $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
        break
      };
    case 1:
      {
        var a$14 = $$this.display1__AO();
        $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$14));
        $$this.display1__AO().u[(31 & (index >> 5))] = $$this.display0__AO();
        break
      };
    case 0:
      break;
    default:
      throw new ScalaJS.c.s_MatchError().init___O(x1);
  }
});
ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO = (function($$this, array, index) {
  var x = array.u[index];
  array.u[index] = null;
  var a = ScalaJS.asArrayOf.O(x, 1);
  return ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a)
});
ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V = (function($$this, that, depth) {
  $$this.depth$und$eq__I__V(depth);
  var x1 = (((-1) + depth) | 0);
  switch (x1) {
    case (-1):
      break;
    case 0:
      {
        $$this.display0$und$eq__AO__V(that.display0__AO());
        break
      };
    case 1:
      {
        $$this.display1$und$eq__AO__V(that.display1__AO());
        $$this.display0$und$eq__AO__V(that.display0__AO());
        break
      };
    case 2:
      {
        $$this.display2$und$eq__AO__V(that.display2__AO());
        $$this.display1$und$eq__AO__V(that.display1__AO());
        $$this.display0$und$eq__AO__V(that.display0__AO());
        break
      };
    case 3:
      {
        $$this.display3$und$eq__AO__V(that.display3__AO());
        $$this.display2$und$eq__AO__V(that.display2__AO());
        $$this.display1$und$eq__AO__V(that.display1__AO());
        $$this.display0$und$eq__AO__V(that.display0__AO());
        break
      };
    case 4:
      {
        $$this.display4$und$eq__AO__V(that.display4__AO());
        $$this.display3$und$eq__AO__V(that.display3__AO());
        $$this.display2$und$eq__AO__V(that.display2__AO());
        $$this.display1$und$eq__AO__V(that.display1__AO());
        $$this.display0$und$eq__AO__V(that.display0__AO());
        break
      };
    case 5:
      {
        $$this.display5$und$eq__AO__V(that.display5__AO());
        $$this.display4$und$eq__AO__V(that.display4__AO());
        $$this.display3$und$eq__AO__V(that.display3__AO());
        $$this.display2$und$eq__AO__V(that.display2__AO());
        $$this.display1$und$eq__AO__V(that.display1__AO());
        $$this.display0$und$eq__AO__V(that.display0__AO());
        break
      };
    default:
      throw new ScalaJS.c.s_MatchError().init___O(x1);
  }
});
ScalaJS.s.sci_VectorPointer$class__gotoNextBlockStart__sci_VectorPointer__I__I__V = (function($$this, index, xor) {
  if ((xor < 1024)) {
    $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[(31 & (index >> 5))], 1))
  } else if ((xor < 32768)) {
    $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (index >> 10))], 1));
    $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[0], 1))
  } else if ((xor < 1048576)) {
    $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (index >> 15))], 1));
    $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[0], 1));
    $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[0], 1))
  } else if ((xor < 33554432)) {
    $$this.display3$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display4__AO().u[(31 & (index >> 20))], 1));
    $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[0], 1));
    $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[0], 1));
    $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[0], 1))
  } else if ((xor < 1073741824)) {
    $$this.display4$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display5__AO().u[(31 & (index >> 25))], 1));
    $$this.display3$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display4__AO().u[0], 1));
    $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[0], 1));
    $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[0], 1));
    $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[0], 1))
  } else {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___()
  }
});
ScalaJS.s.sci_VectorPointer$class__gotoPos__sci_VectorPointer__I__I__V = (function($$this, index, xor) {
  if ((xor >= 32)) {
    if ((xor < 1024)) {
      $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[(31 & (index >> 5))], 1))
    } else if ((xor < 32768)) {
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (index >> 10))], 1));
      $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[(31 & (index >> 5))], 1))
    } else if ((xor < 1048576)) {
      $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (index >> 15))], 1));
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (index >> 10))], 1));
      $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[(31 & (index >> 5))], 1))
    } else if ((xor < 33554432)) {
      $$this.display3$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display4__AO().u[(31 & (index >> 20))], 1));
      $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (index >> 15))], 1));
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (index >> 10))], 1));
      $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[(31 & (index >> 5))], 1))
    } else if ((xor < 1073741824)) {
      $$this.display4$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display5__AO().u[(31 & (index >> 25))], 1));
      $$this.display3$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display4__AO().u[(31 & (index >> 20))], 1));
      $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (index >> 15))], 1));
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (index >> 10))], 1));
      $$this.display0$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display1__AO().u[(31 & (index >> 5))], 1))
    } else {
      throw new ScalaJS.c.jl_IllegalArgumentException().init___()
    }
  }
});
ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO = (function($$this, a) {
  if ((a === null)) {
    var this$2 = ScalaJS.m.s_Console$();
    var this$3 = this$2.outVar$2;
    ScalaJS.as.Ljava_io_PrintStream(this$3.tl$1.get__O()).println__O__V("NULL")
  };
  var b = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [a.u["length"]]);
  var length = a.u["length"];
  ScalaJS.systemArraycopy(a, 0, b, 0, length);
  return b
});
ScalaJS.s.sci_VectorPointer$class__gotoFreshPosWritable0__sci_VectorPointer__I__I__I__V = (function($$this, oldIndex, newIndex, xor) {
  if ((xor >= 32)) {
    if ((xor < 1024)) {
      if (($$this.depth__I() === 1)) {
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display1__AO().u[(31 & (oldIndex >> 5))] = $$this.display0__AO();
        $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
      };
      $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
    } else if ((xor < 32768)) {
      if (($$this.depth__I() === 2)) {
        $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display2__AO().u[(31 & (oldIndex >> 10))] = $$this.display1__AO();
        $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
      };
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (newIndex >> 10))], 1));
      if (($$this.display1__AO() === null)) {
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
    } else if ((xor < 1048576)) {
      if (($$this.depth__I() === 3)) {
        $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display3__AO().u[(31 & (oldIndex >> 15))] = $$this.display2__AO();
        $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
      };
      $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (newIndex >> 15))], 1));
      if (($$this.display2__AO() === null)) {
        $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (newIndex >> 10))], 1));
      if (($$this.display1__AO() === null)) {
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
    } else if ((xor < 33554432)) {
      if (($$this.depth__I() === 4)) {
        $$this.display4$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display4__AO().u[(31 & (oldIndex >> 20))] = $$this.display3__AO();
        $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
      };
      $$this.display3$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display4__AO().u[(31 & (newIndex >> 20))], 1));
      if (($$this.display3__AO() === null)) {
        $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (newIndex >> 15))], 1));
      if (($$this.display2__AO() === null)) {
        $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (newIndex >> 10))], 1));
      if (($$this.display1__AO() === null)) {
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
    } else if ((xor < 1073741824)) {
      if (($$this.depth__I() === 5)) {
        $$this.display5$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display5__AO().u[(31 & (oldIndex >> 25))] = $$this.display4__AO();
        $$this.display4$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]));
        $$this.depth$und$eq__I__V(((1 + $$this.depth__I()) | 0))
      };
      $$this.display4$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display5__AO().u[(31 & (newIndex >> 20))], 1));
      if (($$this.display4__AO() === null)) {
        $$this.display4$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display3$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display4__AO().u[(31 & (newIndex >> 20))], 1));
      if (($$this.display3__AO() === null)) {
        $$this.display3$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display2$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display3__AO().u[(31 & (newIndex >> 15))], 1));
      if (($$this.display2__AO() === null)) {
        $$this.display2$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display1$und$eq__AO__V(ScalaJS.asArrayOf.O($$this.display2__AO().u[(31 & (newIndex >> 10))], 1));
      if (($$this.display1__AO() === null)) {
        $$this.display1$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
      };
      $$this.display0$und$eq__AO__V(ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]))
    } else {
      throw new ScalaJS.c.jl_IllegalArgumentException().init___()
    }
  }
});
ScalaJS.s.sci_VectorPointer$class__copyRange__sci_VectorPointer__AO__I__I__AO = (function($$this, array, oldLeft, newLeft) {
  var elems = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]);
  var length = ((32 - ((newLeft > oldLeft) ? newLeft : oldLeft)) | 0);
  ScalaJS.systemArraycopy(array, oldLeft, elems, newLeft, length);
  return elems
});
ScalaJS.s.sci_VectorPointer$class__gotoPosWritable1__sci_VectorPointer__I__I__I__V = (function($$this, oldIndex, newIndex, xor) {
  if ((xor < 32)) {
    var a = $$this.display0__AO();
    $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a))
  } else if ((xor < 1024)) {
    var a$1 = $$this.display1__AO();
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$1));
    $$this.display1__AO().u[(31 & (oldIndex >> 5))] = $$this.display0__AO();
    var array = $$this.display1__AO();
    var index = (31 & (newIndex >> 5));
    $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array, index))
  } else if ((xor < 32768)) {
    var a$2 = $$this.display1__AO();
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$2));
    var a$3 = $$this.display2__AO();
    $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$3));
    $$this.display1__AO().u[(31 & (oldIndex >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (oldIndex >> 10))] = $$this.display1__AO();
    var array$1 = $$this.display2__AO();
    var index$1 = (31 & (newIndex >> 10));
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$1, index$1));
    var array$2 = $$this.display1__AO();
    var index$2 = (31 & (newIndex >> 5));
    $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$2, index$2))
  } else if ((xor < 1048576)) {
    var a$4 = $$this.display1__AO();
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$4));
    var a$5 = $$this.display2__AO();
    $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$5));
    var a$6 = $$this.display3__AO();
    $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$6));
    $$this.display1__AO().u[(31 & (oldIndex >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (oldIndex >> 10))] = $$this.display1__AO();
    $$this.display3__AO().u[(31 & (oldIndex >> 15))] = $$this.display2__AO();
    var array$3 = $$this.display3__AO();
    var index$3 = (31 & (newIndex >> 15));
    $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$3, index$3));
    var array$4 = $$this.display2__AO();
    var index$4 = (31 & (newIndex >> 10));
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$4, index$4));
    var array$5 = $$this.display1__AO();
    var index$5 = (31 & (newIndex >> 5));
    $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$5, index$5))
  } else if ((xor < 33554432)) {
    var a$7 = $$this.display1__AO();
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$7));
    var a$8 = $$this.display2__AO();
    $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$8));
    var a$9 = $$this.display3__AO();
    $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$9));
    var a$10 = $$this.display4__AO();
    $$this.display4$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$10));
    $$this.display1__AO().u[(31 & (oldIndex >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (oldIndex >> 10))] = $$this.display1__AO();
    $$this.display3__AO().u[(31 & (oldIndex >> 15))] = $$this.display2__AO();
    $$this.display4__AO().u[(31 & (oldIndex >> 20))] = $$this.display3__AO();
    var array$6 = $$this.display4__AO();
    var index$6 = (31 & (newIndex >> 20));
    $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$6, index$6));
    var array$7 = $$this.display3__AO();
    var index$7 = (31 & (newIndex >> 15));
    $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$7, index$7));
    var array$8 = $$this.display2__AO();
    var index$8 = (31 & (newIndex >> 10));
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$8, index$8));
    var array$9 = $$this.display1__AO();
    var index$9 = (31 & (newIndex >> 5));
    $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$9, index$9))
  } else if ((xor < 1073741824)) {
    var a$11 = $$this.display1__AO();
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$11));
    var a$12 = $$this.display2__AO();
    $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$12));
    var a$13 = $$this.display3__AO();
    $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$13));
    var a$14 = $$this.display4__AO();
    $$this.display4$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$14));
    var a$15 = $$this.display5__AO();
    $$this.display5$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__copyOf__sci_VectorPointer__AO__AO($$this, a$15));
    $$this.display1__AO().u[(31 & (oldIndex >> 5))] = $$this.display0__AO();
    $$this.display2__AO().u[(31 & (oldIndex >> 10))] = $$this.display1__AO();
    $$this.display3__AO().u[(31 & (oldIndex >> 15))] = $$this.display2__AO();
    $$this.display4__AO().u[(31 & (oldIndex >> 20))] = $$this.display3__AO();
    $$this.display5__AO().u[(31 & (oldIndex >> 25))] = $$this.display4__AO();
    var array$10 = $$this.display5__AO();
    var index$10 = (31 & (newIndex >> 25));
    $$this.display4$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$10, index$10));
    var array$11 = $$this.display4__AO();
    var index$11 = (31 & (newIndex >> 20));
    $$this.display3$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$11, index$11));
    var array$12 = $$this.display3__AO();
    var index$12 = (31 & (newIndex >> 15));
    $$this.display2$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$12, index$12));
    var array$13 = $$this.display2__AO();
    var index$13 = (31 & (newIndex >> 10));
    $$this.display1$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$13, index$13));
    var array$14 = $$this.display1__AO();
    var index$14 = (31 & (newIndex >> 5));
    $$this.display0$und$eq__AO__V(ScalaJS.s.sci_VectorPointer$class__nullSlotAndCopy__sci_VectorPointer__AO__I__AO($$this, array$14, index$14))
  } else {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___()
  }
});
/** @constructor */
ScalaJS.c.sci_WrappedString$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sci_WrappedString$.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_WrappedString$.prototype.constructor = ScalaJS.c.sci_WrappedString$;
/** @constructor */
ScalaJS.h.sci_WrappedString$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_WrappedString$.prototype = ScalaJS.c.sci_WrappedString$.prototype;
ScalaJS.c.sci_WrappedString$.prototype.newBuilder__scm_Builder = (function() {
  var this$3 = new ScalaJS.c.scm_StringBuilder().init___();
  var f = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2) {
    return (function(x$2) {
      var x = ScalaJS.as.T(x$2);
      return new ScalaJS.c.sci_WrappedString().init___T(x)
    })
  })(this));
  return new ScalaJS.c.scm_Builder$$anon$1().init___scm_Builder__F1(this$3, f)
});
ScalaJS.d.sci_WrappedString$ = new ScalaJS.ClassTypeData({
  sci_WrappedString$: 0
}, false, "scala.collection.immutable.WrappedString$", {
  sci_WrappedString$: 1,
  O: 1
});
ScalaJS.c.sci_WrappedString$.prototype.$classData = ScalaJS.d.sci_WrappedString$;
ScalaJS.n.sci_WrappedString$ = (void 0);
ScalaJS.m.sci_WrappedString$ = (function() {
  if ((!ScalaJS.n.sci_WrappedString$)) {
    ScalaJS.n.sci_WrappedString$ = new ScalaJS.c.sci_WrappedString$().init___()
  };
  return ScalaJS.n.sci_WrappedString$
});
ScalaJS.s.scm_Builder$class__sizeHint__scm_Builder__sc_TraversableLike__V = (function($$this, coll) {
  if (ScalaJS.is.sc_IndexedSeqLike(coll)) {
    $$this.sizeHint__I__V(coll.size__I())
  }
});
ScalaJS.s.scm_Builder$class__sizeHint__scm_Builder__sc_TraversableLike__I__V = (function($$this, coll, delta) {
  if (ScalaJS.is.sc_IndexedSeqLike(coll)) {
    $$this.sizeHint__I__V(((coll.size__I() + delta) | 0))
  }
});
ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V = (function($$this, size, boundingColl) {
  if (ScalaJS.is.sc_IndexedSeqLike(boundingColl)) {
    var that = boundingColl.size__I();
    $$this.sizeHint__I__V(((size < that) ? size : that))
  }
});
/** @constructor */
ScalaJS.c.scm_FlatHashTable$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.scm_FlatHashTable$.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_FlatHashTable$.prototype.constructor = ScalaJS.c.scm_FlatHashTable$;
/** @constructor */
ScalaJS.h.scm_FlatHashTable$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_FlatHashTable$.prototype = ScalaJS.c.scm_FlatHashTable$.prototype;
ScalaJS.c.scm_FlatHashTable$.prototype.newThreshold__I__I__I = (function(_loadFactor, size) {
  var assertion = (_loadFactor < 500);
  if ((!assertion)) {
    throw new ScalaJS.c.jl_AssertionError().init___O(("assertion failed: " + "loadFactor too large; must be < 0.5"))
  };
  return new ScalaJS.c.sjsr_RuntimeLong().init___I(size).$$times__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I(_loadFactor)).$$div__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(1000, 0, 0)).toInt__I()
});
ScalaJS.d.scm_FlatHashTable$ = new ScalaJS.ClassTypeData({
  scm_FlatHashTable$: 0
}, false, "scala.collection.mutable.FlatHashTable$", {
  scm_FlatHashTable$: 1,
  O: 1
});
ScalaJS.c.scm_FlatHashTable$.prototype.$classData = ScalaJS.d.scm_FlatHashTable$;
ScalaJS.n.scm_FlatHashTable$ = (void 0);
ScalaJS.m.scm_FlatHashTable$ = (function() {
  if ((!ScalaJS.n.scm_FlatHashTable$)) {
    ScalaJS.n.scm_FlatHashTable$ = new ScalaJS.c.scm_FlatHashTable$().init___()
  };
  return ScalaJS.n.scm_FlatHashTable$
});
ScalaJS.s.scm_FlatHashTable$HashUtils$class__improve__scm_FlatHashTable$HashUtils__I__I__I = (function($$this, hcode, seed) {
  var improved = ScalaJS.m.s_util_hashing_package$().byteswap32__I__I(hcode);
  var rotation = (seed % 32);
  var rotated = (((improved >>> rotation) | 0) | (improved << ((32 - rotation) | 0)));
  return rotated
});
ScalaJS.s.scm_FlatHashTable$HashUtils$class__entryToElem__scm_FlatHashTable$HashUtils__O__O = (function($$this, entry) {
  return ((entry === ScalaJS.m.scm_FlatHashTable$NullSentinel$()) ? null : entry)
});
ScalaJS.s.scm_FlatHashTable$HashUtils$class__elemToEntry__scm_FlatHashTable$HashUtils__O__O = (function($$this, elem) {
  return ((elem === null) ? ScalaJS.m.scm_FlatHashTable$NullSentinel$() : elem)
});
/** @constructor */
ScalaJS.c.scm_FlatHashTable$NullSentinel$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.scm_FlatHashTable$NullSentinel$.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_FlatHashTable$NullSentinel$.prototype.constructor = ScalaJS.c.scm_FlatHashTable$NullSentinel$;
/** @constructor */
ScalaJS.h.scm_FlatHashTable$NullSentinel$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_FlatHashTable$NullSentinel$.prototype = ScalaJS.c.scm_FlatHashTable$NullSentinel$.prototype;
ScalaJS.c.scm_FlatHashTable$NullSentinel$.prototype.toString__T = (function() {
  return "NullSentinel"
});
ScalaJS.c.scm_FlatHashTable$NullSentinel$.prototype.hashCode__I = (function() {
  return 0
});
ScalaJS.d.scm_FlatHashTable$NullSentinel$ = new ScalaJS.ClassTypeData({
  scm_FlatHashTable$NullSentinel$: 0
}, false, "scala.collection.mutable.FlatHashTable$NullSentinel$", {
  scm_FlatHashTable$NullSentinel$: 1,
  O: 1
});
ScalaJS.c.scm_FlatHashTable$NullSentinel$.prototype.$classData = ScalaJS.d.scm_FlatHashTable$NullSentinel$;
ScalaJS.n.scm_FlatHashTable$NullSentinel$ = (void 0);
ScalaJS.m.scm_FlatHashTable$NullSentinel$ = (function() {
  if ((!ScalaJS.n.scm_FlatHashTable$NullSentinel$)) {
    ScalaJS.n.scm_FlatHashTable$NullSentinel$ = new ScalaJS.c.scm_FlatHashTable$NullSentinel$().init___()
  };
  return ScalaJS.n.scm_FlatHashTable$NullSentinel$
});
ScalaJS.s.scm_FlatHashTable$class__growTable__p0__scm_FlatHashTable__V = (function($$this) {
  var oldtable = $$this.table$5;
  $$this.table$5 = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [ScalaJS.imul(2, $$this.table$5.u["length"])]);
  $$this.tableSize$5 = 0;
  var tableLength = $$this.table$5.u["length"];
  ScalaJS.s.scm_FlatHashTable$class__nnSizeMapReset__scm_FlatHashTable__I__V($$this, tableLength);
  $$this.seedvalue$5 = ScalaJS.s.scm_FlatHashTable$class__tableSizeSeed__scm_FlatHashTable__I($$this);
  $$this.threshold$5 = ScalaJS.m.scm_FlatHashTable$().newThreshold__I__I__I($$this.$$undloadFactor$5, $$this.table$5.u["length"]);
  var i = 0;
  while ((i < oldtable.u["length"])) {
    var entry = oldtable.u[i];
    if ((entry !== null)) {
      ScalaJS.s.scm_FlatHashTable$class__addEntry__scm_FlatHashTable__O__Z($$this, entry)
    };
    i = ((1 + i) | 0)
  }
});
ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z = (function($$this, elem) {
  var removalEntry = ScalaJS.s.scm_FlatHashTable$HashUtils$class__elemToEntry__scm_FlatHashTable$HashUtils__O__O($$this, elem);
  var hcode = ScalaJS.objectHashCode(removalEntry);
  var h = ScalaJS.s.scm_FlatHashTable$class__index__scm_FlatHashTable__I__I($$this, hcode);
  var curEntry = $$this.table$5.u[h];
  while ((curEntry !== null)) {
    if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(curEntry, removalEntry)) {
      var h0 = h;
      var h1 = (((1 + h0) | 0) % $$this.table$5.u["length"]);
      while (($$this.table$5.u[h1] !== null)) {
        var hcode$1 = ScalaJS.objectHashCode($$this.table$5.u[h1]);
        var h2 = ScalaJS.s.scm_FlatHashTable$class__index__scm_FlatHashTable__I__I($$this, hcode$1);
        if (((h2 !== h1) && ScalaJS.s.scm_FlatHashTable$class__precedes$1__p0__scm_FlatHashTable__I__I__Z($$this, h2, h0))) {
          $$this.table$5.u[h0] = $$this.table$5.u[h1];
          h0 = h1
        };
        h1 = (((1 + h1) | 0) % $$this.table$5.u["length"])
      };
      $$this.table$5.u[h0] = null;
      $$this.tableSize$5 = (((-1) + $$this.tableSize$5) | 0);
      var h$1 = h0;
      ScalaJS.s.scm_FlatHashTable$class__nnSizeMapRemove__scm_FlatHashTable__I__V($$this, h$1);
      return true
    };
    h = (((1 + h) | 0) % $$this.table$5.u["length"]);
    curEntry = $$this.table$5.u[h]
  };
  return false
});
ScalaJS.s.scm_FlatHashTable$class__calcSizeMapSize__scm_FlatHashTable__I__I = (function($$this, tableLength) {
  return ((1 + (tableLength >> 5)) | 0)
});
ScalaJS.s.scm_FlatHashTable$class__nnSizeMapAdd__scm_FlatHashTable__I__V = (function($$this, h) {
  if (($$this.sizemap$5 !== null)) {
    var p = (h >> 5);
    var ev$1 = $$this.sizemap$5;
    ev$1.u[p] = ((1 + ev$1.u[p]) | 0)
  }
});
ScalaJS.s.scm_FlatHashTable$class__nnSizeMapRemove__scm_FlatHashTable__I__V = (function($$this, h) {
  if (($$this.sizemap$5 !== null)) {
    var ev$2 = $$this.sizemap$5;
    var ev$3 = (h >> 5);
    ev$2.u[ev$3] = (((-1) + ev$2.u[ev$3]) | 0)
  }
});
ScalaJS.s.scm_FlatHashTable$class__$$init$__scm_FlatHashTable__V = (function($$this) {
  $$this.$$undloadFactor$5 = 450;
  $$this.table$5 = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [ScalaJS.s.scm_FlatHashTable$class__capacity__scm_FlatHashTable__I__I($$this, 32)]);
  $$this.tableSize$5 = 0;
  $$this.threshold$5 = ScalaJS.m.scm_FlatHashTable$().newThreshold__I__I__I($$this.$$undloadFactor$5, ScalaJS.s.scm_FlatHashTable$class__capacity__scm_FlatHashTable__I__I($$this, 32));
  $$this.sizemap$5 = null;
  $$this.seedvalue$5 = ScalaJS.s.scm_FlatHashTable$class__tableSizeSeed__scm_FlatHashTable__I($$this)
});
ScalaJS.s.scm_FlatHashTable$class__clearTable__scm_FlatHashTable__V = (function($$this) {
  var i = (((-1) + $$this.table$5.u["length"]) | 0);
  while ((i >= 0)) {
    $$this.table$5.u[i] = null;
    i = (((-1) + i) | 0)
  };
  $$this.tableSize$5 = 0;
  var tableLength = $$this.table$5.u["length"];
  ScalaJS.s.scm_FlatHashTable$class__nnSizeMapReset__scm_FlatHashTable__I__V($$this, tableLength)
});
ScalaJS.s.scm_FlatHashTable$class__findElemImpl__p0__scm_FlatHashTable__O__O = (function($$this, elem) {
  var searchEntry = ScalaJS.s.scm_FlatHashTable$HashUtils$class__elemToEntry__scm_FlatHashTable$HashUtils__O__O($$this, elem);
  var hcode = ScalaJS.objectHashCode(searchEntry);
  var h = ScalaJS.s.scm_FlatHashTable$class__index__scm_FlatHashTable__I__I($$this, hcode);
  var curEntry = $$this.table$5.u[h];
  while (((curEntry !== null) && (!ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(curEntry, searchEntry)))) {
    h = (((1 + h) | 0) % $$this.table$5.u["length"]);
    curEntry = $$this.table$5.u[h]
  };
  return curEntry
});
ScalaJS.s.scm_FlatHashTable$class__addEntry__scm_FlatHashTable__O__Z = (function($$this, newEntry) {
  var hcode = ScalaJS.objectHashCode(newEntry);
  var h = ScalaJS.s.scm_FlatHashTable$class__index__scm_FlatHashTable__I__I($$this, hcode);
  var curEntry = $$this.table$5.u[h];
  while ((curEntry !== null)) {
    if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(curEntry, newEntry)) {
      return false
    };
    h = (((1 + h) | 0) % $$this.table$5.u["length"]);
    curEntry = $$this.table$5.u[h]
  };
  $$this.table$5.u[h] = newEntry;
  $$this.tableSize$5 = ((1 + $$this.tableSize$5) | 0);
  var h$1 = h;
  ScalaJS.s.scm_FlatHashTable$class__nnSizeMapAdd__scm_FlatHashTable__I__V($$this, h$1);
  if (($$this.tableSize$5 >= $$this.threshold$5)) {
    ScalaJS.s.scm_FlatHashTable$class__growTable__p0__scm_FlatHashTable__V($$this)
  };
  return true
});
ScalaJS.s.scm_FlatHashTable$class__addElem__scm_FlatHashTable__O__Z = (function($$this, elem) {
  var newEntry = ScalaJS.s.scm_FlatHashTable$HashUtils$class__elemToEntry__scm_FlatHashTable$HashUtils__O__O($$this, elem);
  return ScalaJS.s.scm_FlatHashTable$class__addEntry__scm_FlatHashTable__O__Z($$this, newEntry)
});
ScalaJS.s.scm_FlatHashTable$class__index__scm_FlatHashTable__I__I = (function($$this, hcode) {
  var seed = $$this.seedvalue$5;
  var improved = ScalaJS.s.scm_FlatHashTable$HashUtils$class__improve__scm_FlatHashTable$HashUtils__I__I__I($$this, hcode, seed);
  var ones = (((-1) + $$this.table$5.u["length"]) | 0);
  return (((improved >>> ((32 - ScalaJS.m.jl_Integer$().bitCount__I__I(ones)) | 0)) | 0) & ones)
});
ScalaJS.s.scm_FlatHashTable$class__precedes$1__p0__scm_FlatHashTable__I__I__Z = (function($$this, i, j) {
  var d = ($$this.table$5.u["length"] >> 1);
  return ((i <= j) ? (((j - i) | 0) < d) : (((i - j) | 0) > d))
});
ScalaJS.s.scm_FlatHashTable$class__tableSizeSeed__scm_FlatHashTable__I = (function($$this) {
  return ScalaJS.m.jl_Integer$().bitCount__I__I((((-1) + $$this.table$5.u["length"]) | 0))
});
ScalaJS.s.scm_FlatHashTable$class__capacity__scm_FlatHashTable__I__I = (function($$this, expectedSize) {
  return ((expectedSize === 0) ? 1 : ScalaJS.m.scm_HashTable$().powerOfTwo__I__I(expectedSize))
});
ScalaJS.s.scm_FlatHashTable$class__nnSizeMapReset__scm_FlatHashTable__I__V = (function($$this, tableLength) {
  if (($$this.sizemap$5 !== null)) {
    var nsize = ScalaJS.s.scm_FlatHashTable$class__calcSizeMapSize__scm_FlatHashTable__I__I($$this, tableLength);
    if (($$this.sizemap$5.u["length"] !== nsize)) {
      $$this.sizemap$5 = ScalaJS.newArrayObject(ScalaJS.d.I.getArrayOf(), [nsize])
    } else {
      var this$1 = ScalaJS.m.ju_Arrays$();
      var a = $$this.sizemap$5;
      this$1.fillImpl$mIc$sp__p1__AI__I__V(a, 0)
    }
  }
});
ScalaJS.s.scm_FlatHashTable$class__initWithContents__scm_FlatHashTable__scm_FlatHashTable$Contents__V = (function($$this, c) {
  if ((c !== null)) {
    $$this.$$undloadFactor$5 = c.loadFactor__I();
    $$this.table$5 = c.table__AO();
    $$this.tableSize$5 = c.tableSize__I();
    $$this.threshold$5 = c.threshold__I();
    $$this.seedvalue$5 = c.seedvalue__I();
    $$this.sizemap$5 = c.sizemap__AI()
  }
});
ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z = (function($$this, elem) {
  return (ScalaJS.s.scm_FlatHashTable$class__findElemImpl__p0__scm_FlatHashTable__O__O($$this, elem) !== null)
});
/** @constructor */
ScalaJS.c.scm_HashTable$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.scm_HashTable$.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_HashTable$.prototype.constructor = ScalaJS.c.scm_HashTable$;
/** @constructor */
ScalaJS.h.scm_HashTable$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_HashTable$.prototype = ScalaJS.c.scm_HashTable$.prototype;
ScalaJS.c.scm_HashTable$.prototype.capacity__I__I = (function(expectedSize) {
  return ((expectedSize === 0) ? 1 : this.powerOfTwo__I__I(expectedSize))
});
ScalaJS.c.scm_HashTable$.prototype.newThreshold__I__I__I = (function(_loadFactor, size) {
  return new ScalaJS.c.sjsr_RuntimeLong().init___I(size).$$times__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I(_loadFactor)).$$div__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(1000, 0, 0)).toInt__I()
});
ScalaJS.c.scm_HashTable$.prototype.powerOfTwo__I__I = (function(target) {
  var c = (((-1) + target) | 0);
  c = (c | ((c >>> 1) | 0));
  c = (c | ((c >>> 2) | 0));
  c = (c | ((c >>> 4) | 0));
  c = (c | ((c >>> 8) | 0));
  c = (c | ((c >>> 16) | 0));
  return ((1 + c) | 0)
});
ScalaJS.d.scm_HashTable$ = new ScalaJS.ClassTypeData({
  scm_HashTable$: 0
}, false, "scala.collection.mutable.HashTable$", {
  scm_HashTable$: 1,
  O: 1
});
ScalaJS.c.scm_HashTable$.prototype.$classData = ScalaJS.d.scm_HashTable$;
ScalaJS.n.scm_HashTable$ = (void 0);
ScalaJS.m.scm_HashTable$ = (function() {
  if ((!ScalaJS.n.scm_HashTable$)) {
    ScalaJS.n.scm_HashTable$ = new ScalaJS.c.scm_HashTable$().init___()
  };
  return ScalaJS.n.scm_HashTable$
});
ScalaJS.s.scm_HashTable$HashUtils$class__improve__scm_HashTable$HashUtils__I__I__I = (function($$this, hcode, seed) {
  var i = ScalaJS.m.s_util_hashing_package$().byteswap32__I__I(hcode);
  var rotation = (seed % 32);
  var rotated = (((i >>> rotation) | 0) | (i << ((32 - rotation) | 0)));
  return rotated
});
ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$lastPopulatedIndex__scm_HashTable__I = (function($$this) {
  var idx = (((-1) + $$this.table$5.u["length"]) | 0);
  while ((($$this.table$5.u[idx] === null) && (idx > 0))) {
    idx = (((-1) + idx) | 0)
  };
  return idx
});
ScalaJS.s.scm_HashTable$class__initWithContents__scm_HashTable__scm_HashTable$Contents__V = (function($$this, c) {
  if ((c !== null)) {
    $$this.$$undloadFactor$5 = c.loadFactor__I();
    $$this.table$5 = c.table__Ascm_HashEntry();
    $$this.tableSize$5 = c.tableSize__I();
    $$this.threshold$5 = c.threshold__I();
    $$this.seedvalue$5 = c.seedvalue__I();
    $$this.sizemap$5 = c.sizemap__AI()
  }
});
ScalaJS.s.scm_HashTable$class__findEntry__scm_HashTable__O__scm_HashEntry = (function($$this, key) {
  var hcode = ScalaJS.m.sr_ScalaRunTime$().hash__O__I(key);
  return ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$findEntry0__scm_HashTable__O__I__scm_HashEntry($$this, key, ScalaJS.s.scm_HashTable$class__index__scm_HashTable__I__I($$this, hcode))
});
ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$findEntry0__scm_HashTable__O__I__scm_HashEntry = (function($$this, key, h) {
  var e = $$this.table$5.u[h];
  while (true) {
    if ((e !== null)) {
      var key1 = e.key$1;
      var jsx$1 = (!ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key1, key))
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      e = ScalaJS.as.scm_HashEntry(e.next$1)
    } else {
      break
    }
  };
  return e
});
ScalaJS.s.scm_HashTable$class__nnSizeMapAdd__scm_HashTable__I__V = (function($$this, h) {
  if (($$this.sizemap$5 !== null)) {
    var ev$1 = $$this.sizemap$5;
    var ev$2 = (h >> 5);
    ev$1.u[ev$2] = ((1 + ev$1.u[ev$2]) | 0)
  }
});
ScalaJS.s.scm_HashTable$class__nnSizeMapRemove__scm_HashTable__I__V = (function($$this, h) {
  if (($$this.sizemap$5 !== null)) {
    var ev$3 = $$this.sizemap$5;
    var ev$4 = (h >> 5);
    ev$3.u[ev$4] = (((-1) + ev$3.u[ev$4]) | 0)
  }
});
ScalaJS.s.scm_HashTable$class__calcSizeMapSize__scm_HashTable__I__I = (function($$this, tableLength) {
  return ((1 + (tableLength >> 5)) | 0)
});
ScalaJS.s.scm_HashTable$class__resize__p0__scm_HashTable__I__V = (function($$this, newSize) {
  var oldTable = $$this.table$5;
  $$this.table$5 = ScalaJS.newArrayObject(ScalaJS.d.scm_HashEntry.getArrayOf(), [newSize]);
  var tableLength = $$this.table$5.u["length"];
  ScalaJS.s.scm_HashTable$class__nnSizeMapReset__scm_HashTable__I__V($$this, tableLength);
  var i = (((-1) + oldTable.u["length"]) | 0);
  while ((i >= 0)) {
    var e = oldTable.u[i];
    while ((e !== null)) {
      var key = e.key$1;
      var hcode = ScalaJS.m.sr_ScalaRunTime$().hash__O__I(key);
      var h = ScalaJS.s.scm_HashTable$class__index__scm_HashTable__I__I($$this, hcode);
      var e1 = ScalaJS.as.scm_HashEntry(e.next$1);
      e.next$1 = $$this.table$5.u[h];
      $$this.table$5.u[h] = e;
      e = e1;
      ScalaJS.s.scm_HashTable$class__nnSizeMapAdd__scm_HashTable__I__V($$this, h)
    };
    i = (((-1) + i) | 0)
  };
  $$this.threshold$5 = ScalaJS.m.scm_HashTable$().newThreshold__I__I__I($$this.$$undloadFactor$5, newSize)
});
ScalaJS.s.scm_HashTable$class__removeEntry__scm_HashTable__O__scm_HashEntry = (function($$this, key) {
  var hcode = ScalaJS.m.sr_ScalaRunTime$().hash__O__I(key);
  var h = ScalaJS.s.scm_HashTable$class__index__scm_HashTable__I__I($$this, hcode);
  var e = $$this.table$5.u[h];
  if ((e !== null)) {
    var key1 = e.key$1;
    if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key1, key)) {
      $$this.table$5.u[h] = ScalaJS.as.scm_HashEntry(e.next$1);
      $$this.tableSize$5 = (((-1) + $$this.tableSize$5) | 0);
      ScalaJS.s.scm_HashTable$class__nnSizeMapRemove__scm_HashTable__I__V($$this, h);
      return e
    } else {
      var e1 = ScalaJS.as.scm_HashEntry(e.next$1);
      while (true) {
        if ((e1 !== null)) {
          var key1$1 = e1.key$1;
          var jsx$1 = (!ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key1$1, key))
        } else {
          var jsx$1 = false
        };
        if (jsx$1) {
          e = e1;
          e1 = ScalaJS.as.scm_HashEntry(e1.next$1)
        } else {
          break
        }
      };
      if ((e1 !== null)) {
        e.next$1 = e1.next$1;
        $$this.tableSize$5 = (((-1) + $$this.tableSize$5) | 0);
        ScalaJS.s.scm_HashTable$class__nnSizeMapRemove__scm_HashTable__I__V($$this, h);
        return e1
      }
    }
  };
  return null
});
ScalaJS.s.scm_HashTable$class__$$init$__scm_HashTable__V = (function($$this) {
  $$this.$$undloadFactor$5 = 750;
  $$this.table$5 = ScalaJS.newArrayObject(ScalaJS.d.scm_HashEntry.getArrayOf(), [ScalaJS.m.scm_HashTable$().capacity__I__I(16)]);
  $$this.tableSize$5 = 0;
  $$this.threshold$5 = ScalaJS.s.scm_HashTable$class__initialThreshold__p0__scm_HashTable__I__I($$this, $$this.$$undloadFactor$5);
  $$this.sizemap$5 = null;
  $$this.seedvalue$5 = ScalaJS.s.scm_HashTable$class__tableSizeSeed__scm_HashTable__I($$this)
});
ScalaJS.s.scm_HashTable$class__index__scm_HashTable__I__I = (function($$this, hcode) {
  var ones = (((-1) + $$this.table$5.u["length"]) | 0);
  var seed = $$this.seedvalue$5;
  var improved = ScalaJS.s.scm_HashTable$HashUtils$class__improve__scm_HashTable$HashUtils__I__I__I($$this, hcode, seed);
  var shifted = ((improved >> ((32 - ScalaJS.m.jl_Integer$().bitCount__I__I(ones)) | 0)) & ones);
  return shifted
});
ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$addEntry0__scm_HashTable__scm_HashEntry__I__V = (function($$this, e, h) {
  e.next$1 = $$this.table$5.u[h];
  $$this.table$5.u[h] = e;
  $$this.tableSize$5 = ((1 + $$this.tableSize$5) | 0);
  ScalaJS.s.scm_HashTable$class__nnSizeMapAdd__scm_HashTable__I__V($$this, h);
  if (($$this.tableSize$5 > $$this.threshold$5)) {
    ScalaJS.s.scm_HashTable$class__resize__p0__scm_HashTable__I__V($$this, ScalaJS.imul(2, $$this.table$5.u["length"]))
  }
});
ScalaJS.s.scm_HashTable$class__initialThreshold__p0__scm_HashTable__I__I = (function($$this, _loadFactor) {
  return ScalaJS.m.scm_HashTable$().newThreshold__I__I__I(_loadFactor, ScalaJS.m.scm_HashTable$().capacity__I__I(16))
});
ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry = (function($$this, key, value) {
  var hcode = ScalaJS.m.sr_ScalaRunTime$().hash__O__I(key);
  var h = ScalaJS.s.scm_HashTable$class__index__scm_HashTable__I__I($$this, hcode);
  var e = ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$findEntry0__scm_HashTable__O__I__scm_HashEntry($$this, key, h);
  return ((e !== null) ? e : (ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$addEntry0__scm_HashTable__scm_HashEntry__I__V($$this, new ScalaJS.c.scm_DefaultEntry().init___O__O(key, value), h), null))
});
ScalaJS.s.scm_HashTable$class__nnSizeMapReset__scm_HashTable__I__V = (function($$this, tableLength) {
  if (($$this.sizemap$5 !== null)) {
    var nsize = ScalaJS.s.scm_HashTable$class__calcSizeMapSize__scm_HashTable__I__I($$this, tableLength);
    if (($$this.sizemap$5.u["length"] !== nsize)) {
      $$this.sizemap$5 = ScalaJS.newArrayObject(ScalaJS.d.I.getArrayOf(), [nsize])
    } else {
      var this$1 = ScalaJS.m.ju_Arrays$();
      var a = $$this.sizemap$5;
      this$1.fillImpl$mIc$sp__p1__AI__I__V(a, 0)
    }
  }
});
ScalaJS.s.scm_HashTable$class__tableSizeSeed__scm_HashTable__I = (function($$this) {
  return ScalaJS.m.jl_Integer$().bitCount__I__I((((-1) + $$this.table$5.u["length"]) | 0))
});
ScalaJS.s.scm_ResizableArray$class__copyToArray__scm_ResizableArray__O__I__I__V = (function($$this, xs, start, len) {
  var that = ((ScalaJS.m.sr_ScalaRunTime$().array$undlength__O__I(xs) - start) | 0);
  var $$this$1 = ((len < that) ? len : that);
  var that$1 = $$this.size0$6;
  var len1 = (($$this$1 < that$1) ? $$this$1 : that$1);
  ScalaJS.m.s_Array$().copy__O__I__O__I__I__V($$this.array$6, 0, xs, start, len1)
});
ScalaJS.s.scm_ResizableArray$class__ensureSize__scm_ResizableArray__I__V = (function($$this, n) {
  var x = $$this.array$6.u["length"];
  var arrayLength = new ScalaJS.c.sjsr_RuntimeLong().init___I(x);
  if (new ScalaJS.c.sjsr_RuntimeLong().init___I(n).$$greater__sjsr_RuntimeLong__Z(arrayLength)) {
    var newSize = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(2, 0, 0).$$times__sjsr_RuntimeLong__sjsr_RuntimeLong(arrayLength);
    while (new ScalaJS.c.sjsr_RuntimeLong().init___I(n).$$greater__sjsr_RuntimeLong__Z(newSize)) {
      newSize = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(2, 0, 0).$$times__sjsr_RuntimeLong__sjsr_RuntimeLong(newSize)
    };
    if (newSize.$$greater__sjsr_RuntimeLong__Z(new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 511, 0))) {
      newSize = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 511, 0)
    };
    var newArray = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [newSize.toInt__I()]);
    var src = $$this.array$6;
    var length = $$this.size0$6;
    ScalaJS.systemArraycopy(src, 0, newArray, 0, length);
    $$this.array$6 = newArray
  }
});
ScalaJS.s.scm_ResizableArray$class__foreach__scm_ResizableArray__F1__V = (function($$this, f) {
  var i = 0;
  var top = $$this.size0$6;
  while ((i < top)) {
    f.apply__O__O($$this.array$6.u[i]);
    i = ((1 + i) | 0)
  }
});
ScalaJS.s.scm_ResizableArray$class__apply__scm_ResizableArray__I__O = (function($$this, idx) {
  if ((idx >= $$this.size0$6)) {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + idx))
  };
  return $$this.array$6.u[idx]
});
ScalaJS.s.scm_ResizableArray$class__reduceToSize__scm_ResizableArray__I__V = (function($$this, sz) {
  ScalaJS.m.s_Predef$().require__Z__V((sz <= $$this.size0$6));
  while (($$this.size0$6 > sz)) {
    $$this.size0$6 = (((-1) + $$this.size0$6) | 0);
    $$this.array$6.u[$$this.size0$6] = null
  }
});
ScalaJS.s.scm_ResizableArray$class__$$init$__scm_ResizableArray__V = (function($$this) {
  var x = $$this.initialSize$6;
  $$this.array$6 = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [((x > 1) ? x : 1)]);
  $$this.size0$6 = 0
});
/** @constructor */
ScalaJS.c.sjsr_Bits$ = (function() {
  ScalaJS.c.O.call(this);
  this.areTypedArraysSupported$1 = false;
  this.arrayBuffer$1 = null;
  this.int32Array$1 = null;
  this.float32Array$1 = null;
  this.float64Array$1 = null;
  this.areTypedArraysBigEndian$1 = false;
  this.highOffset$1 = 0;
  this.lowOffset$1 = 0
});
ScalaJS.c.sjsr_Bits$.prototype = new ScalaJS.h.O();
ScalaJS.c.sjsr_Bits$.prototype.constructor = ScalaJS.c.sjsr_Bits$;
/** @constructor */
ScalaJS.h.sjsr_Bits$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_Bits$.prototype = ScalaJS.c.sjsr_Bits$.prototype;
ScalaJS.c.sjsr_Bits$.prototype.init___ = (function() {
  ScalaJS.n.sjsr_Bits$ = this;
  var x = (((ScalaJS.g["ArrayBuffer"] && ScalaJS.g["Int32Array"]) && ScalaJS.g["Float32Array"]) && ScalaJS.g["Float64Array"]);
  this.areTypedArraysSupported$1 = ScalaJS.uZ((!(!x)));
  this.arrayBuffer$1 = (this.areTypedArraysSupported$1 ? new ScalaJS.g["ArrayBuffer"](8) : null);
  this.int32Array$1 = (this.areTypedArraysSupported$1 ? new ScalaJS.g["Int32Array"](this.arrayBuffer$1, 0, 2) : null);
  this.float32Array$1 = (this.areTypedArraysSupported$1 ? new ScalaJS.g["Float32Array"](this.arrayBuffer$1, 0, 2) : null);
  this.float64Array$1 = (this.areTypedArraysSupported$1 ? new ScalaJS.g["Float64Array"](this.arrayBuffer$1, 0, 1) : null);
  if ((!this.areTypedArraysSupported$1)) {
    var jsx$1 = true
  } else {
    this.int32Array$1[0] = 16909060;
    var jsx$1 = (ScalaJS.uB(new ScalaJS.g["Int8Array"](this.arrayBuffer$1, 0, 8)[0]) === 1)
  };
  this.areTypedArraysBigEndian$1 = jsx$1;
  this.highOffset$1 = (this.areTypedArraysBigEndian$1 ? 0 : 1);
  this.lowOffset$1 = (this.areTypedArraysBigEndian$1 ? 1 : 0);
  return this
});
ScalaJS.c.sjsr_Bits$.prototype.numberHashCode__D__I = (function(value) {
  var iv = (value | 0);
  if (((iv === value) && ((1.0 / value) !== (-Infinity)))) {
    return iv
  } else {
    var this$1 = this.doubleToLongBits__D__J(value);
    return this$1.$$up__sjsr_RuntimeLong__sjsr_RuntimeLong(this$1.$$greater$greater$greater__I__sjsr_RuntimeLong(32)).toInt__I()
  }
});
ScalaJS.c.sjsr_Bits$.prototype.doubleToLongBitsPolyfill__p1__D__J = (function(value) {
  if ((value !== value)) {
    var _3 = ScalaJS.uD(ScalaJS.g["Math"]["pow"](2.0, 51));
    var x1_$_$$und1$1 = false;
    var x1_$_$$und2$1 = 2047;
    var x1_$_$$und3$1 = _3
  } else if (((value === Infinity) || (value === (-Infinity)))) {
    var _1 = (value < 0);
    var x1_$_$$und1$1 = _1;
    var x1_$_$$und2$1 = 2047;
    var x1_$_$$und3$1 = 0.0
  } else if ((value === 0.0)) {
    var _1$1 = ((1 / value) === (-Infinity));
    var x1_$_$$und1$1 = _1$1;
    var x1_$_$$und2$1 = 0;
    var x1_$_$$und3$1 = 0.0
  } else {
    var s = (value < 0);
    var av = (s ? (-value) : value);
    if ((av >= ScalaJS.uD(ScalaJS.g["Math"]["pow"](2.0, (-1022))))) {
      var twoPowFbits = ScalaJS.uD(ScalaJS.g["Math"]["pow"](2.0, 52));
      var a = (ScalaJS.uD(ScalaJS.g["Math"]["log"](av)) / 0.6931471805599453);
      var a$1 = (ScalaJS.uD(ScalaJS.g["Math"]["floor"](a)) | 0);
      var e = ((a$1 < 1023) ? a$1 : 1023);
      var b = e;
      var n = ((av / ScalaJS.uD(ScalaJS.g["Math"]["pow"](2.0, b))) * twoPowFbits);
      var w = ScalaJS.uD(ScalaJS.g["Math"]["floor"](n));
      var f = (n - w);
      var f$1 = ((f < 0.5) ? w : ((f > 0.5) ? (1 + w) : (((w % 2) !== 0) ? (1 + w) : w)));
      if (((f$1 / twoPowFbits) >= 2)) {
        e = ((1 + e) | 0);
        f$1 = 1.0
      };
      if ((e > 1023)) {
        e = 2047;
        f$1 = 0.0
      } else {
        e = ((1023 + e) | 0);
        f$1 = (f$1 - twoPowFbits)
      };
      var _2 = e;
      var _3$1 = f$1;
      var x1_$_$$und1$1 = s;
      var x1_$_$$und2$1 = _2;
      var x1_$_$$und3$1 = _3$1
    } else {
      var n$1 = (av / ScalaJS.uD(ScalaJS.g["Math"]["pow"](2.0, (-1074))));
      var w$1 = ScalaJS.uD(ScalaJS.g["Math"]["floor"](n$1));
      var f$2 = (n$1 - w$1);
      var _3$2 = ((f$2 < 0.5) ? w$1 : ((f$2 > 0.5) ? (1 + w$1) : (((w$1 % 2) !== 0) ? (1 + w$1) : w$1)));
      var x1_$_$$und1$1 = s;
      var x1_$_$$und2$1 = 0;
      var x1_$_$$und3$1 = _3$2
    }
  };
  var s$1 = ScalaJS.uZ(x1_$_$$und1$1);
  var e$1 = ScalaJS.uI(x1_$_$$und2$1);
  var f$3 = ScalaJS.uD(x1_$_$$und3$1);
  var x$2_$_$$und1$1 = s$1;
  var x$2_$_$$und2$1 = e$1;
  var x$2_$_$$und3$1 = f$3;
  var s$2 = ScalaJS.uZ(x$2_$_$$und1$1);
  var e$2 = ScalaJS.uI(x$2_$_$$und2$1);
  var f$2$1 = ScalaJS.uD(x$2_$_$$und3$1);
  var hif = ((f$2$1 / 4.294967296E9) | 0);
  var hi = (((s$2 ? (-2147483648) : 0) | (e$2 << 20)) | hif);
  var lo = (f$2$1 | 0);
  return new ScalaJS.c.sjsr_RuntimeLong().init___I(hi).$$less$less__I__sjsr_RuntimeLong(32).$$bar__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 1023, 0).$$amp__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I(lo)))
});
ScalaJS.c.sjsr_Bits$.prototype.doubleToLongBits__D__J = (function(value) {
  if (this.areTypedArraysSupported$1) {
    this.float64Array$1[0] = value;
    return new ScalaJS.c.sjsr_RuntimeLong().init___I(ScalaJS.uI(this.int32Array$1[this.highOffset$1])).$$less$less__I__sjsr_RuntimeLong(32).$$bar__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 1023, 0).$$amp__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I(ScalaJS.uI(this.int32Array$1[this.lowOffset$1]))))
  } else {
    return this.doubleToLongBitsPolyfill__p1__D__J(value)
  }
});
ScalaJS.d.sjsr_Bits$ = new ScalaJS.ClassTypeData({
  sjsr_Bits$: 0
}, false, "scala.scalajs.runtime.Bits$", {
  sjsr_Bits$: 1,
  O: 1
});
ScalaJS.c.sjsr_Bits$.prototype.$classData = ScalaJS.d.sjsr_Bits$;
ScalaJS.n.sjsr_Bits$ = (void 0);
ScalaJS.m.sjsr_Bits$ = (function() {
  if ((!ScalaJS.n.sjsr_Bits$)) {
    ScalaJS.n.sjsr_Bits$ = new ScalaJS.c.sjsr_Bits$().init___()
  };
  return ScalaJS.n.sjsr_Bits$
});
/** @constructor */
ScalaJS.c.sjsr_RuntimeString$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sjsr_RuntimeString$.prototype = new ScalaJS.h.O();
ScalaJS.c.sjsr_RuntimeString$.prototype.constructor = ScalaJS.c.sjsr_RuntimeString$;
/** @constructor */
ScalaJS.h.sjsr_RuntimeString$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_RuntimeString$.prototype = ScalaJS.c.sjsr_RuntimeString$.prototype;
ScalaJS.c.sjsr_RuntimeString$.prototype.indexOf__T__I__I__I = (function(thiz, ch, fromIndex) {
  var str = this.fromCodePoint__p1__I__T(ch);
  return ScalaJS.uI(thiz["indexOf"](str, fromIndex))
});
ScalaJS.c.sjsr_RuntimeString$.prototype.valueOf__C__T = (function(value) {
  return ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](value))
});
ScalaJS.c.sjsr_RuntimeString$.prototype.valueOf__O__T = (function(value) {
  return ((value === null) ? "null" : ScalaJS.objectToString(value))
});
ScalaJS.c.sjsr_RuntimeString$.prototype.lastIndexOf__T__I__I = (function(thiz, ch) {
  var str = this.fromCodePoint__p1__I__T(ch);
  return ScalaJS.uI(thiz["lastIndexOf"](str))
});
ScalaJS.c.sjsr_RuntimeString$.prototype.indexOf__T__I__I = (function(thiz, ch) {
  var str = this.fromCodePoint__p1__I__T(ch);
  return ScalaJS.uI(thiz["indexOf"](str))
});
ScalaJS.c.sjsr_RuntimeString$.prototype.format__T__AO__T = (function(format, args) {
  var frm = new ScalaJS.c.ju_Formatter().init___();
  var this$1 = frm.format__T__AO__ju_Formatter(format, args);
  var res = this$1.out__jl_Appendable().toString__T();
  frm.close__V();
  return res
});
ScalaJS.c.sjsr_RuntimeString$.prototype.fromCodePoint__p1__I__T = (function(codePoint) {
  if ((((-65536) & codePoint) === 0)) {
    var array = [codePoint];
    var x = ScalaJS.g["String"];
    var jsx$4 = x["fromCharCode"];
    matchEnd5: {
      var jsx$3;
      var jsx$3 = array;
      break matchEnd5
    };
    var jsx$2 = []["concat"](jsx$3);
    var jsx$1 = jsx$4["apply"](x, jsx$2);
    return ScalaJS.as.T(jsx$1)
  } else if (((codePoint < 0) || (codePoint > 1114111))) {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___()
  } else {
    var offsetCp = (((-65536) + codePoint) | 0);
    var array$1 = [(55296 | (offsetCp >> 10)), (56320 | (1023 & offsetCp))];
    var x$1 = ScalaJS.g["String"];
    var jsx$8 = x$1["fromCharCode"];
    matchEnd5$1: {
      var jsx$7;
      var jsx$7 = array$1;
      break matchEnd5$1
    };
    var jsx$6 = []["concat"](jsx$7);
    var jsx$5 = jsx$8["apply"](x$1, jsx$6);
    return ScalaJS.as.T(jsx$5)
  }
});
ScalaJS.c.sjsr_RuntimeString$.prototype.hashCode__T__I = (function(thiz) {
  var res = 0;
  var mul = 1;
  var i = (((-1) + ScalaJS.uI(thiz["length"])) | 0);
  while ((i >= 0)) {
    var jsx$1 = res;
    var index = i;
    res = ((jsx$1 + ScalaJS.imul((65535 & ScalaJS.uI(thiz["charCodeAt"](index))), mul)) | 0);
    mul = ScalaJS.imul(31, mul);
    i = (((-1) + i) | 0)
  };
  return res
});
ScalaJS.d.sjsr_RuntimeString$ = new ScalaJS.ClassTypeData({
  sjsr_RuntimeString$: 0
}, false, "scala.scalajs.runtime.RuntimeString$", {
  sjsr_RuntimeString$: 1,
  O: 1
});
ScalaJS.c.sjsr_RuntimeString$.prototype.$classData = ScalaJS.d.sjsr_RuntimeString$;
ScalaJS.n.sjsr_RuntimeString$ = (void 0);
ScalaJS.m.sjsr_RuntimeString$ = (function() {
  if ((!ScalaJS.n.sjsr_RuntimeString$)) {
    ScalaJS.n.sjsr_RuntimeString$ = new ScalaJS.c.sjsr_RuntimeString$().init___()
  };
  return ScalaJS.n.sjsr_RuntimeString$
});
/** @constructor */
ScalaJS.c.sjsr_StackTrace$ = (function() {
  ScalaJS.c.O.call(this);
  this.isRhino$1 = false;
  this.decompressedClasses$1 = null;
  this.decompressedPrefixes$1 = null;
  this.compressedPrefixes$1 = null;
  this.bitmap$0$1 = false
});
ScalaJS.c.sjsr_StackTrace$.prototype = new ScalaJS.h.O();
ScalaJS.c.sjsr_StackTrace$.prototype.constructor = ScalaJS.c.sjsr_StackTrace$;
/** @constructor */
ScalaJS.h.sjsr_StackTrace$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_StackTrace$.prototype = ScalaJS.c.sjsr_StackTrace$.prototype;
ScalaJS.c.sjsr_StackTrace$.prototype.init___ = (function() {
  ScalaJS.n.sjsr_StackTrace$ = this;
  var dict = {
    "O": "java_lang_Object",
    "T": "java_lang_String",
    "V": "scala_Unit",
    "Z": "scala_Boolean",
    "C": "scala_Char",
    "B": "scala_Byte",
    "S": "scala_Short",
    "I": "scala_Int",
    "J": "scala_Long",
    "F": "scala_Float",
    "D": "scala_Double"
  };
  var index = 0;
  while ((index <= 22)) {
    if ((index >= 2)) {
      dict[("T" + index)] = ("scala_Tuple" + index)
    };
    dict[("F" + index)] = ("scala_Function" + index);
    index = ((1 + index) | 0)
  };
  this.decompressedClasses$1 = dict;
  this.decompressedPrefixes$1 = {
    "sjsr_": "scala_scalajs_runtime_",
    "sjs_": "scala_scalajs_",
    "sci_": "scala_collection_immutable_",
    "scm_": "scala_collection_mutable_",
    "scg_": "scala_collection_generic_",
    "sc_": "scala_collection_",
    "sr_": "scala_runtime_",
    "s_": "scala_",
    "jl_": "java_lang_",
    "ju_": "java_util_"
  };
  this.compressedPrefixes$1 = ScalaJS.g["Object"]["keys"](this.decompressedPrefixes$1);
  return this
});
ScalaJS.c.sjsr_StackTrace$.prototype.createException__p1__O = (function() {
  try {
    return this["undef"]()
  } catch (e) {
    var e$2 = ScalaJS.m.sjsr_package$().wrapJavaScriptException__O__jl_Throwable(e);
    if ((e$2 !== null)) {
      if (ScalaJS.is.sjs_js_JavaScriptException(e$2)) {
        var x5 = ScalaJS.as.sjs_js_JavaScriptException(e$2);
        var e$3 = x5.exception$4;
        return e$3
      } else {
        throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(e$2)
      }
    } else {
      throw e
    }
  }
});
ScalaJS.c.sjsr_StackTrace$.prototype.captureState__jl_Throwable__O__V = (function(throwable, e) {
  throwable["stackdata"] = e
});
ScalaJS.d.sjsr_StackTrace$ = new ScalaJS.ClassTypeData({
  sjsr_StackTrace$: 0
}, false, "scala.scalajs.runtime.StackTrace$", {
  sjsr_StackTrace$: 1,
  O: 1
});
ScalaJS.c.sjsr_StackTrace$.prototype.$classData = ScalaJS.d.sjsr_StackTrace$;
ScalaJS.n.sjsr_StackTrace$ = (void 0);
ScalaJS.m.sjsr_StackTrace$ = (function() {
  if ((!ScalaJS.n.sjsr_StackTrace$)) {
    ScalaJS.n.sjsr_StackTrace$ = new ScalaJS.c.sjsr_StackTrace$().init___()
  };
  return ScalaJS.n.sjsr_StackTrace$
});
/** @constructor */
ScalaJS.c.sjsr_package$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sjsr_package$.prototype = new ScalaJS.h.O();
ScalaJS.c.sjsr_package$.prototype.constructor = ScalaJS.c.sjsr_package$;
/** @constructor */
ScalaJS.h.sjsr_package$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_package$.prototype = ScalaJS.c.sjsr_package$.prototype;
ScalaJS.c.sjsr_package$.prototype.unwrapJavaScriptException__jl_Throwable__O = (function(th) {
  if (ScalaJS.is.sjs_js_JavaScriptException(th)) {
    var x2 = ScalaJS.as.sjs_js_JavaScriptException(th);
    var e = x2.exception$4;
    return e
  } else {
    return th
  }
});
ScalaJS.c.sjsr_package$.prototype.wrapJavaScriptException__O__jl_Throwable = (function(e) {
  if (ScalaJS.is.jl_Throwable(e)) {
    var x2 = ScalaJS.as.jl_Throwable(e);
    return x2
  } else {
    return new ScalaJS.c.sjs_js_JavaScriptException().init___O(e)
  }
});
ScalaJS.d.sjsr_package$ = new ScalaJS.ClassTypeData({
  sjsr_package$: 0
}, false, "scala.scalajs.runtime.package$", {
  sjsr_package$: 1,
  O: 1
});
ScalaJS.c.sjsr_package$.prototype.$classData = ScalaJS.d.sjsr_package$;
ScalaJS.n.sjsr_package$ = (void 0);
ScalaJS.m.sjsr_package$ = (function() {
  if ((!ScalaJS.n.sjsr_package$)) {
    ScalaJS.n.sjsr_package$ = new ScalaJS.c.sjsr_package$().init___()
  };
  return ScalaJS.n.sjsr_package$
});
ScalaJS.isArrayOf.sr_BoxedUnit = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sr_BoxedUnit)))
});
ScalaJS.asArrayOf.sr_BoxedUnit = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sr_BoxedUnit(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.runtime.BoxedUnit;", depth))
});
ScalaJS.d.sr_BoxedUnit = new ScalaJS.ClassTypeData({
  sr_BoxedUnit: 0
}, false, "scala.runtime.BoxedUnit", {
  sr_BoxedUnit: 1,
  O: 1
}, (void 0), (function(x) {
  return (x === (void 0))
}));
/** @constructor */
ScalaJS.c.sr_BoxesRunTime$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sr_BoxesRunTime$.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_BoxesRunTime$.prototype.constructor = ScalaJS.c.sr_BoxesRunTime$;
/** @constructor */
ScalaJS.h.sr_BoxesRunTime$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_BoxesRunTime$.prototype = ScalaJS.c.sr_BoxesRunTime$.prototype;
ScalaJS.c.sr_BoxesRunTime$.prototype.equalsCharObject__jl_Character__O__Z = (function(xc, y) {
  if (ScalaJS.is.jl_Character(y)) {
    var x2 = ScalaJS.as.jl_Character(y);
    return (xc.value$1 === x2.value$1)
  } else if (ScalaJS.is.jl_Number(y)) {
    var x3 = ScalaJS.as.jl_Number(y);
    if (((typeof x3) === "number")) {
      var x2$1 = ScalaJS.uD(x3);
      return (x2$1 === xc.value$1)
    } else if (ScalaJS.is.sjsr_RuntimeLong(x3)) {
      var x3$1 = ScalaJS.uJ(x3);
      return x3$1.equals__sjsr_RuntimeLong__Z(new ScalaJS.c.sjsr_RuntimeLong().init___I(xc.value$1))
    } else {
      return ((x3 === null) ? (xc === null) : ScalaJS.objectEquals(x3, xc))
    }
  } else {
    return ((xc === null) && (y === null))
  }
});
ScalaJS.c.sr_BoxesRunTime$.prototype.equalsNumObject__jl_Number__O__Z = (function(xn, y) {
  if (ScalaJS.is.jl_Number(y)) {
    var x2 = ScalaJS.as.jl_Number(y);
    return this.equalsNumNum__jl_Number__jl_Number__Z(xn, x2)
  } else if (ScalaJS.is.jl_Character(y)) {
    var x3 = ScalaJS.as.jl_Character(y);
    if (((typeof xn) === "number")) {
      var x2$1 = ScalaJS.uD(xn);
      return (x2$1 === x3.value$1)
    } else if (ScalaJS.is.sjsr_RuntimeLong(xn)) {
      var x3$1 = ScalaJS.uJ(xn);
      return x3$1.equals__sjsr_RuntimeLong__Z(new ScalaJS.c.sjsr_RuntimeLong().init___I(x3.value$1))
    } else {
      return ((xn === null) ? (x3 === null) : ScalaJS.objectEquals(xn, x3))
    }
  } else {
    return ((xn === null) ? (y === null) : ScalaJS.objectEquals(xn, y))
  }
});
ScalaJS.c.sr_BoxesRunTime$.prototype.equals__O__O__Z = (function(x, y) {
  if ((x === y)) {
    return true
  } else if (ScalaJS.is.jl_Number(x)) {
    var x2 = ScalaJS.as.jl_Number(x);
    return this.equalsNumObject__jl_Number__O__Z(x2, y)
  } else if (ScalaJS.is.jl_Character(x)) {
    var x3 = ScalaJS.as.jl_Character(x);
    return this.equalsCharObject__jl_Character__O__Z(x3, y)
  } else {
    return ((x === null) ? (y === null) : ScalaJS.objectEquals(x, y))
  }
});
ScalaJS.c.sr_BoxesRunTime$.prototype.hashFromLong__jl_Long__I = (function(n) {
  var iv = ScalaJS.uJ(n).toInt__I();
  return (new ScalaJS.c.sjsr_RuntimeLong().init___I(iv).equals__sjsr_RuntimeLong__Z(ScalaJS.uJ(n)) ? iv : ScalaJS.uJ(n).$$up__sjsr_RuntimeLong__sjsr_RuntimeLong(ScalaJS.uJ(n).$$greater$greater$greater__I__sjsr_RuntimeLong(32)).toInt__I())
});
ScalaJS.c.sr_BoxesRunTime$.prototype.hashFromNumber__jl_Number__I = (function(n) {
  if (ScalaJS.isInt(n)) {
    var x2 = ScalaJS.uI(n);
    return x2
  } else if (ScalaJS.is.sjsr_RuntimeLong(n)) {
    var x3 = ScalaJS.as.sjsr_RuntimeLong(n);
    return this.hashFromLong__jl_Long__I(x3)
  } else if (((typeof n) === "number")) {
    var x4 = ScalaJS.asDouble(n);
    return this.hashFromDouble__jl_Double__I(x4)
  } else {
    return ScalaJS.objectHashCode(n)
  }
});
ScalaJS.c.sr_BoxesRunTime$.prototype.equalsNumNum__jl_Number__jl_Number__Z = (function(xn, yn) {
  if (((typeof xn) === "number")) {
    var x2 = ScalaJS.uD(xn);
    if (((typeof yn) === "number")) {
      var x2$2 = ScalaJS.uD(yn);
      return (x2 === x2$2)
    } else if (ScalaJS.is.sjsr_RuntimeLong(yn)) {
      var x3 = ScalaJS.uJ(yn);
      return (x2 === x3.toDouble__D())
    } else if (ScalaJS.is.s_math_ScalaNumber(yn)) {
      var x4 = ScalaJS.as.s_math_ScalaNumber(yn);
      return x4.equals__O__Z(x2)
    } else {
      return false
    }
  } else if (ScalaJS.is.sjsr_RuntimeLong(xn)) {
    var x3$2 = ScalaJS.uJ(xn);
    if (ScalaJS.is.sjsr_RuntimeLong(yn)) {
      var x2$3 = ScalaJS.uJ(yn);
      return x3$2.equals__sjsr_RuntimeLong__Z(x2$3)
    } else if (((typeof yn) === "number")) {
      var x3$3 = ScalaJS.uD(yn);
      return (x3$2.toDouble__D() === x3$3)
    } else if (ScalaJS.is.s_math_ScalaNumber(yn)) {
      var x4$2 = ScalaJS.as.s_math_ScalaNumber(yn);
      return x4$2.equals__O__Z(x3$2)
    } else {
      return false
    }
  } else {
    return ((xn === null) ? (yn === null) : ScalaJS.objectEquals(xn, yn))
  }
});
ScalaJS.c.sr_BoxesRunTime$.prototype.hashFromDouble__jl_Double__I = (function(n) {
  var iv = (ScalaJS.uD(n) | 0);
  var dv = ScalaJS.uD(n);
  if ((iv === dv)) {
    return iv
  } else {
    var lv = ScalaJS.m.sjsr_RuntimeLong$().fromDouble__D__sjsr_RuntimeLong(ScalaJS.uD(n));
    return ((lv.toDouble__D() === dv) ? lv.$$up__sjsr_RuntimeLong__sjsr_RuntimeLong(lv.$$greater$greater$greater__I__sjsr_RuntimeLong(32)).toInt__I() : ScalaJS.m.sjsr_Bits$().numberHashCode__D__I(ScalaJS.uD(n)))
  }
});
ScalaJS.d.sr_BoxesRunTime$ = new ScalaJS.ClassTypeData({
  sr_BoxesRunTime$: 0
}, false, "scala.runtime.BoxesRunTime$", {
  sr_BoxesRunTime$: 1,
  O: 1
});
ScalaJS.c.sr_BoxesRunTime$.prototype.$classData = ScalaJS.d.sr_BoxesRunTime$;
ScalaJS.n.sr_BoxesRunTime$ = (void 0);
ScalaJS.m.sr_BoxesRunTime$ = (function() {
  if ((!ScalaJS.n.sr_BoxesRunTime$)) {
    ScalaJS.n.sr_BoxesRunTime$ = new ScalaJS.c.sr_BoxesRunTime$().init___()
  };
  return ScalaJS.n.sr_BoxesRunTime$
});
ScalaJS.d.sr_Null$ = new ScalaJS.ClassTypeData({
  sr_Null$: 0
}, false, "scala.runtime.Null$", {
  sr_Null$: 1,
  O: 1
});
/** @constructor */
ScalaJS.c.sr_ScalaRunTime$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sr_ScalaRunTime$.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_ScalaRunTime$.prototype.constructor = ScalaJS.c.sr_ScalaRunTime$;
/** @constructor */
ScalaJS.h.sr_ScalaRunTime$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_ScalaRunTime$.prototype = ScalaJS.c.sr_ScalaRunTime$.prototype;
ScalaJS.c.sr_ScalaRunTime$.prototype.array$undlength__O__I = (function(xs) {
  if (ScalaJS.isArrayOf.O(xs, 1)) {
    var x2 = ScalaJS.asArrayOf.O(xs, 1);
    return x2.u["length"]
  } else if (ScalaJS.isArrayOf.I(xs, 1)) {
    var x3 = ScalaJS.asArrayOf.I(xs, 1);
    return x3.u["length"]
  } else if (ScalaJS.isArrayOf.D(xs, 1)) {
    var x4 = ScalaJS.asArrayOf.D(xs, 1);
    return x4.u["length"]
  } else if (ScalaJS.isArrayOf.J(xs, 1)) {
    var x5 = ScalaJS.asArrayOf.J(xs, 1);
    return x5.u["length"]
  } else if (ScalaJS.isArrayOf.F(xs, 1)) {
    var x6 = ScalaJS.asArrayOf.F(xs, 1);
    return x6.u["length"]
  } else if (ScalaJS.isArrayOf.C(xs, 1)) {
    var x7 = ScalaJS.asArrayOf.C(xs, 1);
    return x7.u["length"]
  } else if (ScalaJS.isArrayOf.B(xs, 1)) {
    var x8 = ScalaJS.asArrayOf.B(xs, 1);
    return x8.u["length"]
  } else if (ScalaJS.isArrayOf.S(xs, 1)) {
    var x9 = ScalaJS.asArrayOf.S(xs, 1);
    return x9.u["length"]
  } else if (ScalaJS.isArrayOf.Z(xs, 1)) {
    var x10 = ScalaJS.asArrayOf.Z(xs, 1);
    return x10.u["length"]
  } else if (ScalaJS.isArrayOf.sr_BoxedUnit(xs, 1)) {
    var x11 = ScalaJS.asArrayOf.sr_BoxedUnit(xs, 1);
    return x11.u["length"]
  } else if ((xs === null)) {
    throw new ScalaJS.c.jl_NullPointerException().init___()
  } else {
    throw new ScalaJS.c.s_MatchError().init___O(xs)
  }
});
ScalaJS.c.sr_ScalaRunTime$.prototype.array$undupdate__O__I__O__V = (function(xs, idx, value) {
  if (ScalaJS.isArrayOf.O(xs, 1)) {
    var x2 = ScalaJS.asArrayOf.O(xs, 1);
    x2.u[idx] = value
  } else if (ScalaJS.isArrayOf.I(xs, 1)) {
    var x3 = ScalaJS.asArrayOf.I(xs, 1);
    x3.u[idx] = ScalaJS.uI(value)
  } else if (ScalaJS.isArrayOf.D(xs, 1)) {
    var x4 = ScalaJS.asArrayOf.D(xs, 1);
    x4.u[idx] = ScalaJS.uD(value)
  } else if (ScalaJS.isArrayOf.J(xs, 1)) {
    var x5 = ScalaJS.asArrayOf.J(xs, 1);
    x5.u[idx] = ScalaJS.uJ(value)
  } else if (ScalaJS.isArrayOf.F(xs, 1)) {
    var x6 = ScalaJS.asArrayOf.F(xs, 1);
    x6.u[idx] = ScalaJS.uF(value)
  } else if (ScalaJS.isArrayOf.C(xs, 1)) {
    var x7 = ScalaJS.asArrayOf.C(xs, 1);
    if ((value === null)) {
      var jsx$1 = 0
    } else {
      var this$2 = ScalaJS.as.jl_Character(value);
      var jsx$1 = this$2.value$1
    };
    x7.u[idx] = jsx$1
  } else if (ScalaJS.isArrayOf.B(xs, 1)) {
    var x8 = ScalaJS.asArrayOf.B(xs, 1);
    x8.u[idx] = ScalaJS.uB(value)
  } else if (ScalaJS.isArrayOf.S(xs, 1)) {
    var x9 = ScalaJS.asArrayOf.S(xs, 1);
    x9.u[idx] = ScalaJS.uS(value)
  } else if (ScalaJS.isArrayOf.Z(xs, 1)) {
    var x10 = ScalaJS.asArrayOf.Z(xs, 1);
    x10.u[idx] = ScalaJS.uZ(value)
  } else if (ScalaJS.isArrayOf.sr_BoxedUnit(xs, 1)) {
    var x11 = ScalaJS.asArrayOf.sr_BoxedUnit(xs, 1);
    x11.u[idx] = ScalaJS.asUnit(value)
  } else if ((xs === null)) {
    throw new ScalaJS.c.jl_NullPointerException().init___()
  } else {
    throw new ScalaJS.c.s_MatchError().init___O(xs)
  }
});
ScalaJS.c.sr_ScalaRunTime$.prototype.hash__O__I = (function(x) {
  return ((x === null) ? 0 : (ScalaJS.is.jl_Number(x) ? ScalaJS.m.sr_BoxesRunTime$().hashFromNumber__jl_Number__I(ScalaJS.as.jl_Number(x)) : ScalaJS.objectHashCode(x)))
});
ScalaJS.c.sr_ScalaRunTime$.prototype.$$undtoString__s_Product__T = (function(x) {
  var this$1 = x.productIterator__sc_Iterator();
  var start = (x.productPrefix__T() + "(");
  return ScalaJS.s.sc_TraversableOnce$class__mkString__sc_TraversableOnce__T__T__T__T(this$1, start, ",", ")")
});
ScalaJS.c.sr_ScalaRunTime$.prototype.array$undapply__O__I__O = (function(xs, idx) {
  if (ScalaJS.isArrayOf.O(xs, 1)) {
    var x2 = ScalaJS.asArrayOf.O(xs, 1);
    return x2.u[idx]
  } else if (ScalaJS.isArrayOf.I(xs, 1)) {
    var x3 = ScalaJS.asArrayOf.I(xs, 1);
    return x3.u[idx]
  } else if (ScalaJS.isArrayOf.D(xs, 1)) {
    var x4 = ScalaJS.asArrayOf.D(xs, 1);
    return x4.u[idx]
  } else if (ScalaJS.isArrayOf.J(xs, 1)) {
    var x5 = ScalaJS.asArrayOf.J(xs, 1);
    return x5.u[idx]
  } else if (ScalaJS.isArrayOf.F(xs, 1)) {
    var x6 = ScalaJS.asArrayOf.F(xs, 1);
    return x6.u[idx]
  } else if (ScalaJS.isArrayOf.C(xs, 1)) {
    var x7 = ScalaJS.asArrayOf.C(xs, 1);
    var c = x7.u[idx];
    return new ScalaJS.c.jl_Character().init___C(c)
  } else if (ScalaJS.isArrayOf.B(xs, 1)) {
    var x8 = ScalaJS.asArrayOf.B(xs, 1);
    return x8.u[idx]
  } else if (ScalaJS.isArrayOf.S(xs, 1)) {
    var x9 = ScalaJS.asArrayOf.S(xs, 1);
    return x9.u[idx]
  } else if (ScalaJS.isArrayOf.Z(xs, 1)) {
    var x10 = ScalaJS.asArrayOf.Z(xs, 1);
    return x10.u[idx]
  } else if (ScalaJS.isArrayOf.sr_BoxedUnit(xs, 1)) {
    var x11 = ScalaJS.asArrayOf.sr_BoxedUnit(xs, 1);
    return x11.u[idx]
  } else if ((xs === null)) {
    throw new ScalaJS.c.jl_NullPointerException().init___()
  } else {
    throw new ScalaJS.c.s_MatchError().init___O(xs)
  }
});
ScalaJS.d.sr_ScalaRunTime$ = new ScalaJS.ClassTypeData({
  sr_ScalaRunTime$: 0
}, false, "scala.runtime.ScalaRunTime$", {
  sr_ScalaRunTime$: 1,
  O: 1
});
ScalaJS.c.sr_ScalaRunTime$.prototype.$classData = ScalaJS.d.sr_ScalaRunTime$;
ScalaJS.n.sr_ScalaRunTime$ = (void 0);
ScalaJS.m.sr_ScalaRunTime$ = (function() {
  if ((!ScalaJS.n.sr_ScalaRunTime$)) {
    ScalaJS.n.sr_ScalaRunTime$ = new ScalaJS.c.sr_ScalaRunTime$().init___()
  };
  return ScalaJS.n.sr_ScalaRunTime$
});
/** @constructor */
ScalaJS.c.Lobjects_Actor = (function() {
  ScalaJS.c.Lobjects_Obj.call(this);
  this.maxHp$2 = 0.0;
  this.hp$2 = 0.0;
  this.name$2 = null;
  this.speed$2 = 0;
  this.points$2 = 0;
  this.faction$2 = null;
  this.momentum$2 = null;
  this.momentumFactor$2 = 0.0;
  this.important$2 = false;
  this.lastLoc$2 = null;
  this.effects$2 = null
});
ScalaJS.c.Lobjects_Actor.prototype = new ScalaJS.h.Lobjects_Obj();
ScalaJS.c.Lobjects_Actor.prototype.constructor = ScalaJS.c.Lobjects_Actor;
/** @constructor */
ScalaJS.h.Lobjects_Actor = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_Actor.prototype = ScalaJS.c.Lobjects_Actor.prototype;
ScalaJS.c.Lobjects_Actor.prototype.changeLocRel__D__D__V = (function(dx, dy) {
  var ev$1 = this.loc$1;
  ev$1.x$1 = (ev$1.x$1 + dx);
  var ev$2 = this.loc$1;
  ev$2.y$1 = (ev$2.y$1 + dy)
});
ScalaJS.c.Lobjects_Actor.prototype.getClosestEnemyInLOS__Lgame_Game__Lobjects_Actor = (function(g) {
  var closestAct = new ScalaJS.c.sr_ObjectRef().init___O(null);
  var distance = new ScalaJS.c.sr_IntRef().init___I(100000);
  g.acts$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, g$2, closestAct$1, distance$1) {
    return (function(a$2) {
      var a = ScalaJS.as.Lobjects_Actor(a$2);
      if ((((a.faction$2 !== "NA") && (a.faction$2 !== arg$outer.faction$2)) && arg$outer.hasLosTo__Lobjects_Obj__Lgame_Game__Z(a, g$2))) {
        var dist = arg$outer.distanceTo__Lobjects_Obj__I(a);
        if ((dist < distance$1.elem$1)) {
          closestAct$1.elem$1 = a;
          distance$1.elem$1 = dist
        }
      }
    })
  })(this, g, closestAct, distance)));
  return ScalaJS.as.Lobjects_Actor(closestAct.elem$1)
});
ScalaJS.c.Lobjects_Actor.prototype.canTakeItem__Lobjects_Actor__Z = (function(item) {
  return false
});
ScalaJS.c.Lobjects_Actor.prototype.push__Lobjects_Actor__D__Lgame_Game__V = (function(source, amount, g) {
  var x = (source.loc$1.x$1 - this.loc$1.x$1);
  var dx = (-((x > 0) ? 1.0 : ((x < 0) ? (-1.0) : x)));
  var x$1 = (source.loc$1.y$1 - this.loc$1.y$1);
  var dy = (-((x$1 > 0) ? 1.0 : ((x$1 < 0) ? (-1.0) : x$1)));
  this.momentum$2 = new ScalaJS.c.Lglobalvars_Pt().init___D__D((dx * amount), (dy * amount))
});
ScalaJS.c.Lobjects_Actor.prototype.onDeath__Lgame_Game__V = (function(g) {
  /*<skip>*/
});
ScalaJS.c.Lobjects_Actor.prototype.aiMove__Lgame_Game__V = (function(g) {
  var changeX = ((3 * this.momentum$2.x$1) / 4);
  var changeY = ((3 * this.momentum$2.y$1) / 4);
  this.moveLoc__D__D__Lgame_Game__Z(changeX, changeY, g);
  this.momentum$2 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(changeX, changeY)
});
ScalaJS.c.Lobjects_Actor.prototype.handleMomentum__Lgame_Game__V = (function(g) {
  var changeX = (this.momentum$2.x$1 * this.momentumFactor$2);
  var changeY = (this.momentum$2.y$1 * this.momentumFactor$2);
  var change = new ScalaJS.c.Lglobalvars_Pt().init___D__D(changeX, changeY);
  var end = (change.pythagLength__D() | 0);
  var isEmpty$4 = (end < 1);
  var numRangeElements$4 = (isEmpty$4 ? 0 : end);
  var lastElement$4 = (isEmpty$4 ? 0 : end);
  var terminalElement$4 = ((1 + lastElement$4) | 0);
  if ((numRangeElements$4 < 0)) {
    ScalaJS.m.sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(1, end, 1, true)
  };
  var i = 1;
  var count = 0;
  while ((i !== terminalElement$4)) {
    var arg1 = i;
    var d = change.unitStep__Lglobalvars_Pt();
    this.moveLoc__D__D__Lgame_Game__Z(d.x$1, d.y$1, g);
    count = ((1 + count) | 0);
    i = ((1 + i) | 0)
  };
  this.momentum$2 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(changeX, changeY)
});
ScalaJS.c.Lobjects_Actor.prototype.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V = (function(source, damage, pushFactor, g) {
  var jsx$1 = ScalaJS.g["console"];
  var s = ((((source.name$2 + " does ") + damage) + " damage to ") + this.name$2);
  jsx$1["log"](s);
  this.hp$2 = (this.hp$2 - damage);
  this.push__Lobjects_Actor__D__Lgame_Game__V(source, (damage * pushFactor), g);
  if ((this.hp$2 <= 0)) {
    g.removeActor__Lobjects_Actor__s_Option(this);
    this.onDeath__Lgame_Game__V(g);
    g.score$1 = ((g.score$1 + this.points$2) | 0)
  }
});
ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T = (function(loc_, size_, hp_, speed_, faction_, points_, name_) {
  ScalaJS.c.Lobjects_Obj.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt.call(this, loc_, size_);
  this.maxHp$2 = hp_;
  this.hp$2 = hp_;
  this.name$2 = name_;
  this.speed$2 = speed_;
  this.points$2 = points_;
  this.faction$2 = faction_;
  this.momentum$2 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0);
  this.momentumFactor$2 = ScalaJS.m.Lglobalvars_GV$().NORMMOMENTUMFACTOR$1;
  this.important$2 = false;
  this.lastLoc$2 = new ScalaJS.c.Lglobalvars_Pt().init___D__D((-1.0), (-1.0));
  this.effects$2 = ScalaJS.as.scm_Set(ScalaJS.m.scm_Set$().apply__sc_Seq__sc_GenTraversable(ScalaJS.m.sci_Nil$()));
  return this
});
ScalaJS.c.Lobjects_Actor.prototype.moveToNewMap__Lgame_Game__V = (function(g) {
  /*<skip>*/
});
ScalaJS.c.Lobjects_Actor.prototype.moveLoc__D__D__Lgame_Game__Z = (function(dx, dy, g) {
  var moved = false;
  var tempLoc = this.loc$1.cloone__Lglobalvars_Pt();
  if (((dx !== 0) && this.canMoveTo__Lglobalvars_Pt__Lgame_Game__Z(new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.loc$1.x$1 + dx), this.loc$1.y$1), g))) {
    var ev$3 = this.loc$1;
    ev$3.x$1 = (ev$3.x$1 + dx);
    moved = true
  };
  if (((dy !== 0) && this.canMoveTo__Lglobalvars_Pt__Lgame_Game__Z(new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.loc$1.x$1, (this.loc$1.y$1 + dy)), g))) {
    var ev$4 = this.loc$1;
    ev$4.y$1 = (ev$4.y$1 + dy);
    moved = true
  };
  if (moved) {
    this.lastLoc$2 = tempLoc
  };
  return moved
});
ScalaJS.c.Lobjects_Actor.prototype.draw__Lgame_Game__V = (function(g) {
  g.ctx$1["fillStyle"] = "red";
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.c.Lobjects_Actor.prototype.isDead__Z = (function() {
  return (this.hp$2 <= 0)
});
ScalaJS.c.Lobjects_Actor.prototype.canMoveTo__Lglobalvars_Pt__Lgame_Game__Z = (function(newLoc, g) {
  var nonLocalReturnKey1 = new ScalaJS.c.O().init___();
  try {
    g.objs$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, newLoc$1, nonLocalReturnKey1$1) {
      return (function(o$2) {
        var o = ScalaJS.as.Lobjects_Obj(o$2);
        if ((((o !== arg$outer) && o.blocksMovement$1) && o.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(newLoc$1, arg$outer.size$1))) {
          throw new ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp().init___O__Z(nonLocalReturnKey1$1, false)
        }
      })
    })(this, newLoc, nonLocalReturnKey1)));
    return true
  } catch (e) {
    if (ScalaJS.is.sr_NonLocalReturnControl(e)) {
      var ex = ScalaJS.as.sr_NonLocalReturnControl(e);
      if ((ex.key$2 === nonLocalReturnKey1)) {
        return ex.value$mcZ$sp$f
      } else {
        throw ex
      }
    } else {
      throw e
    }
  }
});
ScalaJS.is.Lobjects_Actor = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_Actor)))
});
ScalaJS.as.Lobjects_Actor = (function(obj) {
  return ((ScalaJS.is.Lobjects_Actor(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.Actor"))
});
ScalaJS.isArrayOf.Lobjects_Actor = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_Actor)))
});
ScalaJS.asArrayOf.Lobjects_Actor = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_Actor(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.Actor;", depth))
});
/** @constructor */
ScalaJS.c.Lobjects_ThrowableItem = (function() {
  ScalaJS.c.Lobjects_UsableItem.call(this);
  this.dist$2 = 0;
  this.speed$2 = 0
});
ScalaJS.c.Lobjects_ThrowableItem.prototype = new ScalaJS.h.Lobjects_UsableItem();
ScalaJS.c.Lobjects_ThrowableItem.prototype.constructor = ScalaJS.c.Lobjects_ThrowableItem;
/** @constructor */
ScalaJS.h.Lobjects_ThrowableItem = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_ThrowableItem.prototype = ScalaJS.c.Lobjects_ThrowableItem.prototype;
ScalaJS.c.Lobjects_ThrowableItem.prototype.getThrowPosition__Lglobalvars_Pt = (function() {
  var d = this.owner$1.loc$1.$$minus__Lglobalvars_Pt__Lglobalvars_Pt(this.owner$1.lastLoc$2);
  var diff = d.$$times__I__Lglobalvars_Pt(this.dist$2);
  return this.owner$1.loc$1.$$plus__Lglobalvars_Pt__Lglobalvars_Pt(diff)
});
ScalaJS.c.Lobjects_ThrowableItem.prototype.init___Lobjects_Actor__T__T = (function(owner_, name_, picture_) {
  ScalaJS.c.Lobjects_UsableItem.prototype.init___Lobjects_Actor__T__T.call(this, owner_, name_, picture_);
  this.dist$2 = ScalaJS.m.Lglobalvars_GV$().PLAYER$undTHROWDISTANCE$1;
  this.speed$2 = ScalaJS.m.Lglobalvars_GV$().PLAYER$undTHROWSPEED$1;
  return this
});
/** @constructor */
ScalaJS.c.Lobjects_UsableLandMine = (function() {
  ScalaJS.c.Lobjects_UsableItem.call(this)
});
ScalaJS.c.Lobjects_UsableLandMine.prototype = new ScalaJS.h.Lobjects_UsableItem();
ScalaJS.c.Lobjects_UsableLandMine.prototype.constructor = ScalaJS.c.Lobjects_UsableLandMine;
/** @constructor */
ScalaJS.h.Lobjects_UsableLandMine = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_UsableLandMine.prototype = ScalaJS.c.Lobjects_UsableLandMine.prototype;
ScalaJS.c.Lobjects_UsableLandMine.prototype.use__Lgame_Game__V = (function(g) {
  var mine = new ScalaJS.c.Lobjects_LandMine().init___Lglobalvars_Pt(this.owner$1.loc$1.cloone__Lglobalvars_Pt());
  g.addActor__Lobjects_Actor__scm_Set(mine)
});
ScalaJS.c.Lobjects_UsableLandMine.prototype.init___Lobjects_Actor = (function(owner_) {
  ScalaJS.c.Lobjects_UsableItem.prototype.init___Lobjects_Actor__T__T.call(this, owner_, "land mine", "item_landmine");
  return this
});
ScalaJS.d.Lobjects_UsableLandMine = new ScalaJS.ClassTypeData({
  Lobjects_UsableLandMine: 0
}, false, "objects.UsableLandMine", {
  Lobjects_UsableLandMine: 1,
  Lobjects_UsableItem: 1,
  O: 1
});
ScalaJS.c.Lobjects_UsableLandMine.prototype.$classData = ScalaJS.d.Lobjects_UsableLandMine;
/** @constructor */
ScalaJS.c.Lobjects_Wall = (function() {
  ScalaJS.c.Lobjects_Obj.call(this)
});
ScalaJS.c.Lobjects_Wall.prototype = new ScalaJS.h.Lobjects_Obj();
ScalaJS.c.Lobjects_Wall.prototype.constructor = ScalaJS.c.Lobjects_Wall;
/** @constructor */
ScalaJS.h.Lobjects_Wall = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_Wall.prototype = ScalaJS.c.Lobjects_Wall.prototype;
ScalaJS.c.Lobjects_Wall.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt = (function(loc_, size_) {
  ScalaJS.c.Lobjects_Obj.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt.call(this, loc_, size_);
  this.blocksLos$1 = true;
  this.alwaysVisible$1 = true;
  return this
});
ScalaJS.c.Lobjects_Wall.prototype.splitWithDoorAt__I__I__T2 = (function(i, doorSize) {
  if ((this.size$1.x$1 >= this.size$1.y$1)) {
    var w1 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(this.loc$1, new ScalaJS.c.Lglobalvars_Pt().init___D__D(i, this.size$1.y$1));
    var w2 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(((this.loc$1.x$1 + i) + doorSize), this.loc$1.y$1), new ScalaJS.c.Lglobalvars_Pt().init___D__D(((this.size$1.x$1 - i) - doorSize), this.size$1.y$1));
    return new ScalaJS.c.T2().init___O__O(w1, w2)
  } else {
    var w1$2 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(this.loc$1, new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.size$1.x$1, i));
    var w2$2 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.loc$1.x$1, ((this.loc$1.y$1 + i) + doorSize)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.size$1.x$1, ((this.size$1.y$1 - i) - doorSize)));
    return new ScalaJS.c.T2().init___O__O(w1$2, w2$2)
  }
});
ScalaJS.c.Lobjects_Wall.prototype.draw__Lgame_Game__V = (function(g) {
  g.ctx$1["fillStyle"] = "blue";
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.is.Lobjects_Wall = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_Wall)))
});
ScalaJS.as.Lobjects_Wall = (function(obj) {
  return ((ScalaJS.is.Lobjects_Wall(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.Wall"))
});
ScalaJS.isArrayOf.Lobjects_Wall = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_Wall)))
});
ScalaJS.asArrayOf.Lobjects_Wall = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_Wall(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.Wall;", depth))
});
ScalaJS.d.Lobjects_Wall = new ScalaJS.ClassTypeData({
  Lobjects_Wall: 0
}, false, "objects.Wall", {
  Lobjects_Wall: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_Wall.prototype.$classData = ScalaJS.d.Lobjects_Wall;
ScalaJS.isArrayOf.jl_Boolean = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Boolean)))
});
ScalaJS.asArrayOf.jl_Boolean = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_Boolean(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Boolean;", depth))
});
ScalaJS.d.jl_Boolean = new ScalaJS.ClassTypeData({
  jl_Boolean: 0
}, false, "java.lang.Boolean", {
  jl_Boolean: 1,
  O: 1,
  jl_Comparable: 1
}, (void 0), (function(x) {
  return ((typeof x) === "boolean")
}));
/** @constructor */
ScalaJS.c.jl_Character = (function() {
  ScalaJS.c.O.call(this);
  this.value$1 = 0
});
ScalaJS.c.jl_Character.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_Character.prototype.constructor = ScalaJS.c.jl_Character;
/** @constructor */
ScalaJS.h.jl_Character = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Character.prototype = ScalaJS.c.jl_Character.prototype;
ScalaJS.c.jl_Character.prototype.equals__O__Z = (function(that) {
  if (ScalaJS.is.jl_Character(that)) {
    var jsx$1 = this.value$1;
    var this$1 = ScalaJS.as.jl_Character(that);
    return (jsx$1 === this$1.value$1)
  } else {
    return false
  }
});
ScalaJS.c.jl_Character.prototype.toString__T = (function() {
  var c = this.value$1;
  return ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c))
});
ScalaJS.c.jl_Character.prototype.init___C = (function(value) {
  this.value$1 = value;
  return this
});
ScalaJS.c.jl_Character.prototype.hashCode__I = (function() {
  return this.value$1
});
ScalaJS.is.jl_Character = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.jl_Character)))
});
ScalaJS.as.jl_Character = (function(obj) {
  return ((ScalaJS.is.jl_Character(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.lang.Character"))
});
ScalaJS.isArrayOf.jl_Character = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Character)))
});
ScalaJS.asArrayOf.jl_Character = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_Character(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Character;", depth))
});
ScalaJS.d.jl_Character = new ScalaJS.ClassTypeData({
  jl_Character: 0
}, false, "java.lang.Character", {
  jl_Character: 1,
  O: 1,
  jl_Comparable: 1
});
ScalaJS.c.jl_Character.prototype.$classData = ScalaJS.d.jl_Character;
/** @constructor */
ScalaJS.c.jl_InheritableThreadLocal = (function() {
  ScalaJS.c.jl_ThreadLocal.call(this)
});
ScalaJS.c.jl_InheritableThreadLocal.prototype = new ScalaJS.h.jl_ThreadLocal();
ScalaJS.c.jl_InheritableThreadLocal.prototype.constructor = ScalaJS.c.jl_InheritableThreadLocal;
/** @constructor */
ScalaJS.h.jl_InheritableThreadLocal = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_InheritableThreadLocal.prototype = ScalaJS.c.jl_InheritableThreadLocal.prototype;
/** @constructor */
ScalaJS.c.jl_Throwable = (function() {
  ScalaJS.c.O.call(this);
  this.s$1 = null;
  this.e$1 = null;
  this.stackTrace$1 = null
});
ScalaJS.c.jl_Throwable.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_Throwable.prototype.constructor = ScalaJS.c.jl_Throwable;
/** @constructor */
ScalaJS.h.jl_Throwable = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Throwable.prototype = ScalaJS.c.jl_Throwable.prototype;
ScalaJS.c.jl_Throwable.prototype.init___ = (function() {
  ScalaJS.c.jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
ScalaJS.c.jl_Throwable.prototype.fillInStackTrace__jl_Throwable = (function() {
  var this$1 = ScalaJS.m.sjsr_StackTrace$();
  this$1.captureState__jl_Throwable__O__V(this, this$1.createException__p1__O());
  return this
});
ScalaJS.c.jl_Throwable.prototype.getMessage__T = (function() {
  return this.s$1
});
ScalaJS.c.jl_Throwable.prototype.toString__T = (function() {
  var className = ScalaJS.objectGetClass(this).getName__T();
  var message = this.getMessage__T();
  return ((message === null) ? className : ((className + ": ") + message))
});
ScalaJS.c.jl_Throwable.prototype.init___T__jl_Throwable = (function(s, e) {
  this.s$1 = s;
  this.e$1 = e;
  this.fillInStackTrace__jl_Throwable();
  return this
});
ScalaJS.is.jl_Throwable = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.jl_Throwable)))
});
ScalaJS.as.jl_Throwable = (function(obj) {
  return ((ScalaJS.is.jl_Throwable(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.lang.Throwable"))
});
ScalaJS.isArrayOf.jl_Throwable = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Throwable)))
});
ScalaJS.asArrayOf.jl_Throwable = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_Throwable(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Throwable;", depth))
});
/** @constructor */
ScalaJS.c.ju_Random = (function() {
  ScalaJS.c.O.call(this);
  this.seedHi$1 = 0;
  this.seedLo$1 = 0;
  this.nextNextGaussian$1 = 0.0;
  this.haveNextNextGaussian$1 = false
});
ScalaJS.c.ju_Random.prototype = new ScalaJS.h.O();
ScalaJS.c.ju_Random.prototype.constructor = ScalaJS.c.ju_Random;
/** @constructor */
ScalaJS.h.ju_Random = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_Random.prototype = ScalaJS.c.ju_Random.prototype;
ScalaJS.c.ju_Random.prototype.init___ = (function() {
  ScalaJS.c.ju_Random.prototype.init___J.call(this, ScalaJS.m.ju_Random$().java$util$Random$$randomSeed__J());
  return this
});
ScalaJS.c.ju_Random.prototype.init___J = (function(seed_in) {
  this.haveNextNextGaussian$1 = false;
  this.setSeed__J__V(seed_in);
  return this
});
ScalaJS.c.ju_Random.prototype.nextInt__I__I = (function(n) {
  if ((n <= 0)) {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___T("n must be positive")
  };
  return (((n & ((-n) | 0)) === n) ? new ScalaJS.c.sjsr_RuntimeLong().init___I(n).$$times__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I(this.next__I__I(31))).$$greater$greater__I__sjsr_RuntimeLong(31).toInt__I() : this.loop$1__p1__I__I(n))
});
ScalaJS.c.ju_Random.prototype.next__I__I = (function(bits) {
  var oldSeedHi = this.seedHi$1;
  var oldSeedLo = this.seedLo$1;
  var loProd = (11 + (15525485 * oldSeedLo));
  var hiProd = ((1502 * oldSeedLo) + (15525485 * oldSeedHi));
  var newSeedHi = (16777215 & ((((loProd / 16777216) | 0) + (16777215 & (hiProd | 0))) | 0));
  var newSeedLo = (16777215 & (loProd | 0));
  this.seedHi$1 = newSeedHi;
  this.seedLo$1 = newSeedLo;
  var result32 = ((newSeedHi << 8) | (newSeedLo >> 16));
  return ((result32 >>> ((32 - bits) | 0)) | 0)
});
ScalaJS.c.ju_Random.prototype.loop$1__p1__I__I = (function(n$1) {
  _loop: while (true) {
    var bits = this.next__I__I(31);
    var value = (bits % n$1);
    if ((((((bits - value) | 0) + (((-1) + n$1) | 0)) | 0) < 0)) {
      continue _loop
    } else {
      return value
    }
  }
});
ScalaJS.c.ju_Random.prototype.setSeed__J__V = (function(seed_in) {
  var seed = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 4194303, 15).$$amp__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(2942573, 6011, 0).$$up__sjsr_RuntimeLong__sjsr_RuntimeLong(seed_in));
  this.seedHi$1 = seed.$$greater$greater$greater__I__sjsr_RuntimeLong(24).toInt__I();
  this.seedLo$1 = (16777215 & seed.toInt__I());
  this.haveNextNextGaussian$1 = false
});
ScalaJS.d.ju_Random = new ScalaJS.ClassTypeData({
  ju_Random: 0
}, false, "java.util.Random", {
  ju_Random: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.ju_Random.prototype.$classData = ScalaJS.d.ju_Random;
/** @constructor */
ScalaJS.c.s_Predef$$anon$3 = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_Predef$$anon$3.prototype = new ScalaJS.h.O();
ScalaJS.c.s_Predef$$anon$3.prototype.constructor = ScalaJS.c.s_Predef$$anon$3;
/** @constructor */
ScalaJS.h.s_Predef$$anon$3 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Predef$$anon$3.prototype = ScalaJS.c.s_Predef$$anon$3.prototype;
ScalaJS.c.s_Predef$$anon$3.prototype.apply__scm_Builder = (function() {
  return new ScalaJS.c.scm_StringBuilder().init___()
});
ScalaJS.c.s_Predef$$anon$3.prototype.apply__O__scm_Builder = (function(from) {
  ScalaJS.as.T(from);
  return new ScalaJS.c.scm_StringBuilder().init___()
});
ScalaJS.d.s_Predef$$anon$3 = new ScalaJS.ClassTypeData({
  s_Predef$$anon$3: 0
}, false, "scala.Predef$$anon$3", {
  s_Predef$$anon$3: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
ScalaJS.c.s_Predef$$anon$3.prototype.$classData = ScalaJS.d.s_Predef$$anon$3;
ScalaJS.is.s_math_ScalaNumber = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_math_ScalaNumber)))
});
ScalaJS.as.s_math_ScalaNumber = (function(obj) {
  return ((ScalaJS.is.s_math_ScalaNumber(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.math.ScalaNumber"))
});
ScalaJS.isArrayOf.s_math_ScalaNumber = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_math_ScalaNumber)))
});
ScalaJS.asArrayOf.s_math_ScalaNumber = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.s_math_ScalaNumber(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.math.ScalaNumber;", depth))
});
/** @constructor */
ScalaJS.c.s_package$$anon$1 = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_package$$anon$1.prototype = new ScalaJS.h.O();
ScalaJS.c.s_package$$anon$1.prototype.constructor = ScalaJS.c.s_package$$anon$1;
/** @constructor */
ScalaJS.h.s_package$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_package$$anon$1.prototype = ScalaJS.c.s_package$$anon$1.prototype;
ScalaJS.c.s_package$$anon$1.prototype.toString__T = (function() {
  return "object AnyRef"
});
ScalaJS.d.s_package$$anon$1 = new ScalaJS.ClassTypeData({
  s_package$$anon$1: 0
}, false, "scala.package$$anon$1", {
  s_package$$anon$1: 1,
  O: 1,
  s_Specializable: 1
});
ScalaJS.c.s_package$$anon$1.prototype.$classData = ScalaJS.d.s_package$$anon$1;
/** @constructor */
ScalaJS.c.s_util_hashing_MurmurHash3$ = (function() {
  ScalaJS.c.s_util_hashing_MurmurHash3.call(this);
  this.arraySeed$2 = 0;
  this.stringSeed$2 = 0;
  this.productSeed$2 = 0;
  this.symmetricSeed$2 = 0;
  this.traversableSeed$2 = 0;
  this.seqSeed$2 = 0;
  this.mapSeed$2 = 0;
  this.setSeed$2 = 0
});
ScalaJS.c.s_util_hashing_MurmurHash3$.prototype = new ScalaJS.h.s_util_hashing_MurmurHash3();
ScalaJS.c.s_util_hashing_MurmurHash3$.prototype.constructor = ScalaJS.c.s_util_hashing_MurmurHash3$;
/** @constructor */
ScalaJS.h.s_util_hashing_MurmurHash3$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_hashing_MurmurHash3$.prototype = ScalaJS.c.s_util_hashing_MurmurHash3$.prototype;
ScalaJS.c.s_util_hashing_MurmurHash3$.prototype.init___ = (function() {
  ScalaJS.n.s_util_hashing_MurmurHash3$ = this;
  this.seqSeed$2 = ScalaJS.m.sjsr_RuntimeString$().hashCode__T__I("Seq");
  this.mapSeed$2 = ScalaJS.m.sjsr_RuntimeString$().hashCode__T__I("Map");
  this.setSeed$2 = ScalaJS.m.sjsr_RuntimeString$().hashCode__T__I("Set");
  return this
});
ScalaJS.c.s_util_hashing_MurmurHash3$.prototype.seqHash__sc_Seq__I = (function(xs) {
  if (ScalaJS.is.sci_List(xs)) {
    var x2 = ScalaJS.as.sci_List(xs);
    return this.listHash__sci_List__I__I(x2, this.seqSeed$2)
  } else {
    return this.orderedHash__sc_TraversableOnce__I__I(xs, this.seqSeed$2)
  }
});
ScalaJS.d.s_util_hashing_MurmurHash3$ = new ScalaJS.ClassTypeData({
  s_util_hashing_MurmurHash3$: 0
}, false, "scala.util.hashing.MurmurHash3$", {
  s_util_hashing_MurmurHash3$: 1,
  s_util_hashing_MurmurHash3: 1,
  O: 1
});
ScalaJS.c.s_util_hashing_MurmurHash3$.prototype.$classData = ScalaJS.d.s_util_hashing_MurmurHash3$;
ScalaJS.n.s_util_hashing_MurmurHash3$ = (void 0);
ScalaJS.m.s_util_hashing_MurmurHash3$ = (function() {
  if ((!ScalaJS.n.s_util_hashing_MurmurHash3$)) {
    ScalaJS.n.s_util_hashing_MurmurHash3$ = new ScalaJS.c.s_util_hashing_MurmurHash3$().init___()
  };
  return ScalaJS.n.s_util_hashing_MurmurHash3$
});
/** @constructor */
ScalaJS.c.sc_TraversableLike$WithFilter = (function() {
  ScalaJS.c.O.call(this);
  this.p$1 = null;
  this.$$outer$f = null
});
ScalaJS.c.sc_TraversableLike$WithFilter.prototype = new ScalaJS.h.O();
ScalaJS.c.sc_TraversableLike$WithFilter.prototype.constructor = ScalaJS.c.sc_TraversableLike$WithFilter;
/** @constructor */
ScalaJS.h.sc_TraversableLike$WithFilter = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_TraversableLike$WithFilter.prototype = ScalaJS.c.sc_TraversableLike$WithFilter.prototype;
ScalaJS.c.sc_TraversableLike$WithFilter.prototype.foreach__F1__V = (function(f) {
  this.$$outer$f.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2, f$1) {
    return (function(x$2) {
      return (ScalaJS.uZ(this$2.p$1.apply__O__O(x$2)) ? f$1.apply__O__O(x$2) : (void 0))
    })
  })(this, f)))
});
ScalaJS.c.sc_TraversableLike$WithFilter.prototype.init___sc_TraversableLike__F1 = (function($$outer, p) {
  this.p$1 = p;
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$f = $$outer
  };
  return this
});
ScalaJS.d.sc_TraversableLike$WithFilter = new ScalaJS.ClassTypeData({
  sc_TraversableLike$WithFilter: 0
}, false, "scala.collection.TraversableLike$WithFilter", {
  sc_TraversableLike$WithFilter: 1,
  O: 1,
  scg_FilterMonadic: 1
});
ScalaJS.c.sc_TraversableLike$WithFilter.prototype.$classData = ScalaJS.d.sc_TraversableLike$WithFilter;
/** @constructor */
ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom = (function() {
  ScalaJS.c.O.call(this);
  this.$$outer$f = null
});
ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom.prototype = new ScalaJS.h.O();
ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom.prototype.constructor = ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom;
/** @constructor */
ScalaJS.h.scg_GenMapFactory$MapCanBuildFrom = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenMapFactory$MapCanBuildFrom.prototype = ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom.prototype;
ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom.prototype.apply__scm_Builder = (function() {
  return this.$$outer$f.newBuilder__scm_Builder()
});
ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom.prototype.apply__O__scm_Builder = (function(from) {
  ScalaJS.as.sc_GenMap(from);
  return this.$$outer$f.newBuilder__scm_Builder()
});
ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom.prototype.init___scg_GenMapFactory = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$f = $$outer
  };
  return this
});
ScalaJS.d.scg_GenMapFactory$MapCanBuildFrom = new ScalaJS.ClassTypeData({
  scg_GenMapFactory$MapCanBuildFrom: 0
}, false, "scala.collection.generic.GenMapFactory$MapCanBuildFrom", {
  scg_GenMapFactory$MapCanBuildFrom: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom.prototype.$classData = ScalaJS.d.scg_GenMapFactory$MapCanBuildFrom;
/** @constructor */
ScalaJS.c.scg_GenSetFactory = (function() {
  ScalaJS.c.scg_GenericCompanion.call(this)
});
ScalaJS.c.scg_GenSetFactory.prototype = new ScalaJS.h.scg_GenericCompanion();
ScalaJS.c.scg_GenSetFactory.prototype.constructor = ScalaJS.c.scg_GenSetFactory;
/** @constructor */
ScalaJS.h.scg_GenSetFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenSetFactory.prototype = ScalaJS.c.scg_GenSetFactory.prototype;
/** @constructor */
ScalaJS.c.scg_GenSetFactory$$anon$1 = (function() {
  ScalaJS.c.O.call(this);
  this.$$outer$1 = null
});
ScalaJS.c.scg_GenSetFactory$$anon$1.prototype = new ScalaJS.h.O();
ScalaJS.c.scg_GenSetFactory$$anon$1.prototype.constructor = ScalaJS.c.scg_GenSetFactory$$anon$1;
/** @constructor */
ScalaJS.h.scg_GenSetFactory$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenSetFactory$$anon$1.prototype = ScalaJS.c.scg_GenSetFactory$$anon$1.prototype;
ScalaJS.c.scg_GenSetFactory$$anon$1.prototype.apply__scm_Builder = (function() {
  return this.$$outer$1.newBuilder__scm_Builder()
});
ScalaJS.c.scg_GenSetFactory$$anon$1.prototype.apply__O__scm_Builder = (function(from) {
  return this.apply__sc_GenSet__scm_Builder(ScalaJS.as.sc_GenSet(from))
});
ScalaJS.c.scg_GenSetFactory$$anon$1.prototype.init___scg_GenSetFactory = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$1 = $$outer
  };
  return this
});
ScalaJS.c.scg_GenSetFactory$$anon$1.prototype.apply__sc_GenSet__scm_Builder = (function(from) {
  return this.$$outer$1.newBuilder__scm_Builder()
});
ScalaJS.d.scg_GenSetFactory$$anon$1 = new ScalaJS.ClassTypeData({
  scg_GenSetFactory$$anon$1: 0
}, false, "scala.collection.generic.GenSetFactory$$anon$1", {
  scg_GenSetFactory$$anon$1: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
ScalaJS.c.scg_GenSetFactory$$anon$1.prototype.$classData = ScalaJS.d.scg_GenSetFactory$$anon$1;
/** @constructor */
ScalaJS.c.scg_GenTraversableFactory = (function() {
  ScalaJS.c.scg_GenericCompanion.call(this);
  this.ReusableCBFInstance$2 = null
});
ScalaJS.c.scg_GenTraversableFactory.prototype = new ScalaJS.h.scg_GenericCompanion();
ScalaJS.c.scg_GenTraversableFactory.prototype.constructor = ScalaJS.c.scg_GenTraversableFactory;
/** @constructor */
ScalaJS.h.scg_GenTraversableFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenTraversableFactory.prototype = ScalaJS.c.scg_GenTraversableFactory.prototype;
ScalaJS.c.scg_GenTraversableFactory.prototype.init___ = (function() {
  this.ReusableCBFInstance$2 = new ScalaJS.c.scg_GenTraversableFactory$$anon$1().init___scg_GenTraversableFactory(this);
  return this
});
/** @constructor */
ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom = (function() {
  ScalaJS.c.O.call(this);
  this.$$outer$f = null
});
ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype = new ScalaJS.h.O();
ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype.constructor = ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom;
/** @constructor */
ScalaJS.h.scg_GenTraversableFactory$GenericCanBuildFrom = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenTraversableFactory$GenericCanBuildFrom.prototype = ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype;
ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype.apply__scm_Builder = (function() {
  return this.$$outer$f.newBuilder__scm_Builder()
});
ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype.apply__O__scm_Builder = (function(from) {
  var from$1 = ScalaJS.as.sc_GenTraversable(from);
  return from$1.companion__scg_GenericCompanion().newBuilder__scm_Builder()
});
ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype.init___scg_GenTraversableFactory = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$f = $$outer
  };
  return this
});
/** @constructor */
ScalaJS.c.scg_MapFactory = (function() {
  ScalaJS.c.scg_GenMapFactory.call(this)
});
ScalaJS.c.scg_MapFactory.prototype = new ScalaJS.h.scg_GenMapFactory();
ScalaJS.c.scg_MapFactory.prototype.constructor = ScalaJS.c.scg_MapFactory;
/** @constructor */
ScalaJS.h.scg_MapFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_MapFactory.prototype = ScalaJS.c.scg_MapFactory.prototype;
/** @constructor */
ScalaJS.c.sci_HashMap$$anon$2 = (function() {
  ScalaJS.c.sci_HashMap$Merger.call(this);
  this.invert$2 = null;
  this.mergef$1$f = null
});
ScalaJS.c.sci_HashMap$$anon$2.prototype = new ScalaJS.h.sci_HashMap$Merger();
ScalaJS.c.sci_HashMap$$anon$2.prototype.constructor = ScalaJS.c.sci_HashMap$$anon$2;
/** @constructor */
ScalaJS.h.sci_HashMap$$anon$2 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$$anon$2.prototype = ScalaJS.c.sci_HashMap$$anon$2.prototype;
ScalaJS.c.sci_HashMap$$anon$2.prototype.init___F2 = (function(mergef$1) {
  this.mergef$1$f = mergef$1;
  this.invert$2 = new ScalaJS.c.sci_HashMap$$anon$2$$anon$3().init___sci_HashMap$$anon$2(this);
  return this
});
ScalaJS.c.sci_HashMap$$anon$2.prototype.apply__T2__T2__T2 = (function(kv1, kv2) {
  return ScalaJS.as.T2(this.mergef$1$f.apply__O__O__O(kv1, kv2))
});
ScalaJS.d.sci_HashMap$$anon$2 = new ScalaJS.ClassTypeData({
  sci_HashMap$$anon$2: 0
}, false, "scala.collection.immutable.HashMap$$anon$2", {
  sci_HashMap$$anon$2: 1,
  sci_HashMap$Merger: 1,
  O: 1
});
ScalaJS.c.sci_HashMap$$anon$2.prototype.$classData = ScalaJS.d.sci_HashMap$$anon$2;
/** @constructor */
ScalaJS.c.sci_HashMap$$anon$2$$anon$3 = (function() {
  ScalaJS.c.sci_HashMap$Merger.call(this);
  this.$$outer$2 = null
});
ScalaJS.c.sci_HashMap$$anon$2$$anon$3.prototype = new ScalaJS.h.sci_HashMap$Merger();
ScalaJS.c.sci_HashMap$$anon$2$$anon$3.prototype.constructor = ScalaJS.c.sci_HashMap$$anon$2$$anon$3;
/** @constructor */
ScalaJS.h.sci_HashMap$$anon$2$$anon$3 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$$anon$2$$anon$3.prototype = ScalaJS.c.sci_HashMap$$anon$2$$anon$3.prototype;
ScalaJS.c.sci_HashMap$$anon$2$$anon$3.prototype.apply__T2__T2__T2 = (function(kv1, kv2) {
  return ScalaJS.as.T2(this.$$outer$2.mergef$1$f.apply__O__O__O(kv2, kv1))
});
ScalaJS.c.sci_HashMap$$anon$2$$anon$3.prototype.init___sci_HashMap$$anon$2 = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  return this
});
ScalaJS.d.sci_HashMap$$anon$2$$anon$3 = new ScalaJS.ClassTypeData({
  sci_HashMap$$anon$2$$anon$3: 0
}, false, "scala.collection.immutable.HashMap$$anon$2$$anon$3", {
  sci_HashMap$$anon$2$$anon$3: 1,
  sci_HashMap$Merger: 1,
  O: 1
});
ScalaJS.c.sci_HashMap$$anon$2$$anon$3.prototype.$classData = ScalaJS.d.sci_HashMap$$anon$2$$anon$3;
/** @constructor */
ScalaJS.c.sci_List$$anon$1 = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sci_List$$anon$1.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_List$$anon$1.prototype.constructor = ScalaJS.c.sci_List$$anon$1;
/** @constructor */
ScalaJS.h.sci_List$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_List$$anon$1.prototype = ScalaJS.c.sci_List$$anon$1.prototype;
ScalaJS.c.sci_List$$anon$1.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sci_List$$anon$1.prototype.apply__O__O = (function(x) {
  return this
});
ScalaJS.c.sci_List$$anon$1.prototype.toString__T = (function() {
  return "<function1>"
});
ScalaJS.d.sci_List$$anon$1 = new ScalaJS.ClassTypeData({
  sci_List$$anon$1: 0
}, false, "scala.collection.immutable.List$$anon$1", {
  sci_List$$anon$1: 1,
  O: 1,
  F1: 1
});
ScalaJS.c.sci_List$$anon$1.prototype.$classData = ScalaJS.d.sci_List$$anon$1;
ScalaJS.is.scm_Builder = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_Builder)))
});
ScalaJS.as.scm_Builder = (function(obj) {
  return ((ScalaJS.is.scm_Builder(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.Builder"))
});
ScalaJS.isArrayOf.scm_Builder = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_Builder)))
});
ScalaJS.asArrayOf.scm_Builder = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_Builder(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.Builder;", depth))
});
/** @constructor */
ScalaJS.c.sr_AbstractFunction0 = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sr_AbstractFunction0.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_AbstractFunction0.prototype.constructor = ScalaJS.c.sr_AbstractFunction0;
/** @constructor */
ScalaJS.h.sr_AbstractFunction0 = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_AbstractFunction0.prototype = ScalaJS.c.sr_AbstractFunction0.prototype;
ScalaJS.c.sr_AbstractFunction0.prototype.toString__T = (function() {
  return "<function0>"
});
/** @constructor */
ScalaJS.c.sr_AbstractFunction1 = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sr_AbstractFunction1.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_AbstractFunction1.prototype.constructor = ScalaJS.c.sr_AbstractFunction1;
/** @constructor */
ScalaJS.h.sr_AbstractFunction1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_AbstractFunction1.prototype = ScalaJS.c.sr_AbstractFunction1.prototype;
ScalaJS.c.sr_AbstractFunction1.prototype.toString__T = (function() {
  return "<function1>"
});
/** @constructor */
ScalaJS.c.sr_AbstractFunction2 = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sr_AbstractFunction2.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_AbstractFunction2.prototype.constructor = ScalaJS.c.sr_AbstractFunction2;
/** @constructor */
ScalaJS.h.sr_AbstractFunction2 = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_AbstractFunction2.prototype = ScalaJS.c.sr_AbstractFunction2.prototype;
ScalaJS.c.sr_AbstractFunction2.prototype.toString__T = (function() {
  return "<function2>"
});
/** @constructor */
ScalaJS.c.sr_BooleanRef = (function() {
  ScalaJS.c.O.call(this);
  this.elem$1 = false
});
ScalaJS.c.sr_BooleanRef.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_BooleanRef.prototype.constructor = ScalaJS.c.sr_BooleanRef;
/** @constructor */
ScalaJS.h.sr_BooleanRef = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_BooleanRef.prototype = ScalaJS.c.sr_BooleanRef.prototype;
ScalaJS.c.sr_BooleanRef.prototype.toString__T = (function() {
  var value = this.elem$1;
  return ("" + value)
});
ScalaJS.c.sr_BooleanRef.prototype.init___Z = (function(elem) {
  this.elem$1 = elem;
  return this
});
ScalaJS.d.sr_BooleanRef = new ScalaJS.ClassTypeData({
  sr_BooleanRef: 0
}, false, "scala.runtime.BooleanRef", {
  sr_BooleanRef: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sr_BooleanRef.prototype.$classData = ScalaJS.d.sr_BooleanRef;
/** @constructor */
ScalaJS.c.sr_IntRef = (function() {
  ScalaJS.c.O.call(this);
  this.elem$1 = 0
});
ScalaJS.c.sr_IntRef.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_IntRef.prototype.constructor = ScalaJS.c.sr_IntRef;
/** @constructor */
ScalaJS.h.sr_IntRef = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_IntRef.prototype = ScalaJS.c.sr_IntRef.prototype;
ScalaJS.c.sr_IntRef.prototype.toString__T = (function() {
  var value = this.elem$1;
  return ("" + value)
});
ScalaJS.c.sr_IntRef.prototype.init___I = (function(elem) {
  this.elem$1 = elem;
  return this
});
ScalaJS.d.sr_IntRef = new ScalaJS.ClassTypeData({
  sr_IntRef: 0
}, false, "scala.runtime.IntRef", {
  sr_IntRef: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sr_IntRef.prototype.$classData = ScalaJS.d.sr_IntRef;
/** @constructor */
ScalaJS.c.sr_ObjectRef = (function() {
  ScalaJS.c.O.call(this);
  this.elem$1 = null
});
ScalaJS.c.sr_ObjectRef.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_ObjectRef.prototype.constructor = ScalaJS.c.sr_ObjectRef;
/** @constructor */
ScalaJS.h.sr_ObjectRef = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_ObjectRef.prototype = ScalaJS.c.sr_ObjectRef.prototype;
ScalaJS.c.sr_ObjectRef.prototype.toString__T = (function() {
  return ScalaJS.m.sjsr_RuntimeString$().valueOf__O__T(this.elem$1)
});
ScalaJS.c.sr_ObjectRef.prototype.init___O = (function(elem) {
  this.elem$1 = elem;
  return this
});
ScalaJS.d.sr_ObjectRef = new ScalaJS.ClassTypeData({
  sr_ObjectRef: 0
}, false, "scala.runtime.ObjectRef", {
  sr_ObjectRef: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sr_ObjectRef.prototype.$classData = ScalaJS.d.sr_ObjectRef;
/** @constructor */
ScalaJS.c.Lenemies_BaseHuman = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.timeToNextShot$3 = 0;
  this.shotCooldown$3 = 0;
  this.gun$3 = null
});
ScalaJS.c.Lenemies_BaseHuman.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lenemies_BaseHuman.prototype.constructor = ScalaJS.c.Lenemies_BaseHuman;
/** @constructor */
ScalaJS.h.Lenemies_BaseHuman = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_BaseHuman.prototype = ScalaJS.c.Lenemies_BaseHuman.prototype;
ScalaJS.c.Lenemies_BaseHuman.prototype.aiMove__Lgame_Game__V = (function(g) {
  this.gun$3.tick__V();
  if (this.gun$3.canShoot__Z()) {
    var closestAct = this.getClosestEnemyInLOS__Lgame_Game__Lobjects_Actor(g);
    if ((closestAct !== null)) {
      var distance = this.distanceTo__Lobjects_Obj__I(closestAct);
      if ((distance <= this.gun$3.range$1)) {
        this.gun$3.shoot__Lobjects_Actor__Lgame_Game__V(closestAct, g)
      }
    }
  }
});
ScalaJS.c.Lenemies_BaseHuman.prototype.init___Lglobalvars_Pt__I = (function(loc_, hp_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), hp_, 2, "human", 0, "human");
  this.timeToNextShot$3 = 0;
  this.shotCooldown$3 = 20;
  this.gun$3 = new ScalaJS.c.Lenemies_Gun().init___I__I__I__I__Lobjects_Actor(ScalaJS.m.Lglobalvars_GV$().PISTOL$undFIRETIME$1, ScalaJS.m.Lglobalvars_GV$().PISTOL$undDAMAGE$1, ScalaJS.m.Lglobalvars_GV$().PISTOL$undRANGE$1, ScalaJS.m.Lglobalvars_GV$().PISTOL$undAPS$1, this);
  return this
});
ScalaJS.is.Lenemies_BaseHuman = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lenemies_BaseHuman)))
});
ScalaJS.as.Lenemies_BaseHuman = (function(obj) {
  return ((ScalaJS.is.Lenemies_BaseHuman(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "enemies.BaseHuman"))
});
ScalaJS.isArrayOf.Lenemies_BaseHuman = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lenemies_BaseHuman)))
});
ScalaJS.asArrayOf.Lenemies_BaseHuman = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lenemies_BaseHuman(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lenemies.BaseHuman;", depth))
});
/** @constructor */
ScalaJS.c.Lenemies_Charger = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.direction$3 = null;
  this.chanceToChangeDirection$3 = 0;
  this.state$3 = null;
  this.chargeLine$3 = null;
  this.chargeCooldown$3 = 0;
  this.chargeTimer$3 = 0;
  this.chargeRange$3 = 0;
  this.chargeSpeed$3 = 0
});
ScalaJS.c.Lenemies_Charger.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lenemies_Charger.prototype.constructor = ScalaJS.c.Lenemies_Charger;
/** @constructor */
ScalaJS.h.Lenemies_Charger = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Charger.prototype = ScalaJS.c.Lenemies_Charger.prototype;
ScalaJS.c.Lenemies_Charger.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().BIGUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().BIGUNITSIZE$1), 200.0, 1, "zombie", 5, "charger");
  this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0);
  this.chanceToChangeDirection$3 = 400;
  this.state$3 = "moving";
  this.chargeLine$3 = null;
  this.chargeCooldown$3 = ScalaJS.m.Lglobalvars_GV$().CHARGER$undCHARGECOOLDOWN$1;
  this.chargeTimer$3 = 0;
  this.chargeRange$3 = ScalaJS.m.Lglobalvars_GV$().CHARGER$undCHARGERANGE$1;
  this.chargeSpeed$3 = ScalaJS.m.Lglobalvars_GV$().CHARGER$undCHARGESPEED$1;
  this.momentumFactor$2 = ScalaJS.m.Lglobalvars_GV$().BIGMOMENTUMFACTOR$1;
  return this
});
ScalaJS.c.Lenemies_Charger.prototype.stateMoving__V = (function() {
  this.state$3 = "moving";
  this.chargeLine$3 = null;
  this.momentumFactor$2 = ScalaJS.m.Lglobalvars_GV$().BIGMOMENTUMFACTOR$1
});
ScalaJS.c.Lenemies_Charger.prototype.aiMove__Lgame_Game__V = (function(g) {
  var y = (((-1) + this.chargeTimer$3) | 0);
  this.chargeTimer$3 = ((y < 0) ? 0 : y);
  if ((this.state$3 === "moving")) {
    var closestAct = this.getClosestEnemyInLOS__Lgame_Game__Lobjects_Actor(g);
    if ((closestAct !== null)) {
      this.chargeLine$3 = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(this.loc$1.cloone__Lglobalvars_Pt(), closestAct.loc$1.cloone__Lglobalvars_Pt(), "black")
    };
    if ((((this.chargeLine$3 !== null) && (this.chargeLine$3.length__D() <= this.chargeRange$3)) && (this.chargeTimer$3 <= 0))) {
      this.stateCharging__V()
    } else {
      var x = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0);
      var x$2 = this.direction$3;
      if (x.equals__O__Z(x$2)) {
        var this$3 = g.r$1;
        var jsx$1 = this$3.self$1.nextInt__I__I(3);
        var this$4 = g.r$1;
        this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I((((-1) + jsx$1) | 0), (((-1) + this$4.self$1.nextInt__I__I(3)) | 0))
      };
      if ((!this.moveLoc__D__D__Lgame_Game__Z(this.direction$3.$$und1$mcI$sp__I(), this.direction$3.$$und2$mcI$sp__I(), g))) {
        this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0)
      } else {
        var this$5 = g.r$1;
        var n = this.chanceToChangeDirection$3;
        if ((this$5.self$1.nextInt__I__I(n) === 0)) {
          this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0)
        }
      }
    }
  } else if ((this.state$3 === "charging")) {
    if ((this.distanceTo__Lglobalvars_Pt__I(this.chargeLine$3.start$1) > this.chargeRange$3)) {
      this.stateMoving__V()
    } else {
      var end = this.chargeSpeed$3;
      var isEmpty$4 = (end < 1);
      var numRangeElements$4 = (isEmpty$4 ? 0 : end);
      var lastElement$4 = (isEmpty$4 ? 0 : end);
      var terminalElement$4 = ((1 + lastElement$4) | 0);
      var f = new ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1().init___Lenemies_Charger__Lgame_Game(this, g);
      if ((numRangeElements$4 < 0)) {
        ScalaJS.m.sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(1, end, 1, true)
      };
      var i = 1;
      var count = 0;
      while ((i !== terminalElement$4)) {
        f.apply$mcVI$sp__I__V(i);
        count = ((1 + count) | 0);
        i = ((1 + i) | 0)
      }
    }
  }
});
ScalaJS.c.Lenemies_Charger.prototype.stateCharging__V = (function() {
  this.state$3 = "charging";
  ScalaJS.g["console"]["log"]("Charging");
  this.chargeTimer$3 = this.chargeCooldown$3;
  this.momentumFactor$2 = ScalaJS.m.Lglobalvars_GV$().CHARGER$undCHARGINGMOMENTUMFACTOR$1
});
ScalaJS.c.Lenemies_Charger.prototype.moveToNewMap__Lgame_Game__V = (function(g) {
  var y = ((this.chargeTimer$3 - ((this.loc$1.y$1 / this.speed$2) | 0)) | 0);
  this.chargeTimer$3 = ((y < 0) ? 0 : y)
});
ScalaJS.c.Lenemies_Charger.prototype.draw__Lgame_Game__V = (function(g) {
  var jsx$1 = g.ctx$1;
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(100, 150, 100)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  jsx$1["fillStyle"] = s;
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.d.Lenemies_Charger = new ScalaJS.ClassTypeData({
  Lenemies_Charger: 0
}, false, "enemies.Charger", {
  Lenemies_Charger: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lenemies_Charger.prototype.$classData = ScalaJS.d.Lenemies_Charger;
/** @constructor */
ScalaJS.c.Lenemies_Spitter = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.direction$3 = null;
  this.chanceToChangeDirection$3 = 0;
  this.spitTimer$3 = 0;
  this.spitRate$3 = 0;
  this.spitRadius$3 = 0;
  this.spitReduceRate$3 = 0
});
ScalaJS.c.Lenemies_Spitter.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lenemies_Spitter.prototype.constructor = ScalaJS.c.Lenemies_Spitter;
/** @constructor */
ScalaJS.h.Lenemies_Spitter = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Spitter.prototype = ScalaJS.c.Lenemies_Spitter.prototype;
ScalaJS.c.Lenemies_Spitter.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), 70.0, 1, "zombie", 5, "spitter");
  this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0);
  this.chanceToChangeDirection$3 = 400;
  this.spitTimer$3 = 0;
  this.spitRate$3 = ScalaJS.m.Lglobalvars_GV$().SPITTER$undSPITRATE$1;
  this.spitRadius$3 = ScalaJS.m.Lglobalvars_GV$().SPITTER$undSPITRADIUS$1;
  this.spitReduceRate$3 = ScalaJS.m.Lglobalvars_GV$().SPITTER$undSPITREDUCERATE$1;
  return this
});
ScalaJS.c.Lenemies_Spitter.prototype.onDeath__Lgame_Game__V = (function(g) {
  var spit = new ScalaJS.c.Lobjects_CausticAcid().init___Lglobalvars_Pt__I__I(this.loc$1, ((ScalaJS.imul(2, this.spitRadius$3) / 3) | 0), this.spitReduceRate$3);
  g.addActor__Lobjects_Actor__scm_Set(spit)
});
ScalaJS.c.Lenemies_Spitter.prototype.aiMove__Lgame_Game__V = (function(g) {
  var y = (((-1) + this.spitTimer$3) | 0);
  this.spitTimer$3 = ((y < 0) ? 0 : y);
  if ((this.spitTimer$3 <= 0)) {
    var target = this.getClosestEnemyInLOS__Lgame_Game__Lobjects_Actor(g);
    if (((target !== null) && (this.distanceTo__Lobjects_Obj__I(target) < ScalaJS.m.Lglobalvars_GV$().SPITTER$undSPITRANGE$1))) {
      var spitLine = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(this.loc$1.cloone__Lglobalvars_Pt(), target.loc$1.cloone__Lglobalvars_Pt(), "black");
      var spit = new ScalaJS.c.Lobjects_CausticAcid().init___Lglobalvars_Pt__I__I(target.loc$1.cloone__Lglobalvars_Pt(), this.spitRadius$3, this.spitReduceRate$3);
      var proj = new ScalaJS.c.Lobjects_ProjectileActor().init___Lobjects_Actor__Lglobalvars_SimpleLine__I(spit, spitLine, 6);
      g.addActor__Lobjects_Actor__scm_Set(proj);
      this.spitTimer$3 = this.spitRate$3
    }
  };
  var x = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0);
  var x$2 = this.direction$3;
  if (x.equals__O__Z(x$2)) {
    var this$3 = g.r$1;
    var jsx$1 = this$3.self$1.nextInt__I__I(3);
    var this$4 = g.r$1;
    this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I((((-1) + jsx$1) | 0), (((-1) + this$4.self$1.nextInt__I__I(3)) | 0))
  };
  if ((!this.moveLoc__D__D__Lgame_Game__Z(this.direction$3.$$und1$mcI$sp__I(), this.direction$3.$$und2$mcI$sp__I(), g))) {
    this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0)
  } else {
    var this$5 = g.r$1;
    var n = this.chanceToChangeDirection$3;
    if ((this$5.self$1.nextInt__I__I(n) === 0)) {
      this.direction$3 = new ScalaJS.c.s_Tuple2$mcII$sp().init___I__I(0, 0)
    }
  }
});
ScalaJS.c.Lenemies_Spitter.prototype.moveToNewMap__Lgame_Game__V = (function(g) {
  var y = ((this.spitTimer$3 - ((this.loc$1.y$1 / this.speed$2) | 0)) | 0);
  this.spitTimer$3 = ((y < 0) ? 0 : y)
});
ScalaJS.c.Lenemies_Spitter.prototype.draw__Lgame_Game__V = (function(g) {
  var jsx$1 = g.ctx$1;
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(0, 255, 0)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  jsx$1["fillStyle"] = s;
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.d.Lenemies_Spitter = new ScalaJS.ClassTypeData({
  Lenemies_Spitter: 0
}, false, "enemies.Spitter", {
  Lenemies_Spitter: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lenemies_Spitter.prototype.$classData = ScalaJS.d.Lenemies_Spitter;
/** @constructor */
ScalaJS.c.Lenemies_Tank = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.target$3 = null;
  this.direction$3 = null;
  this.chanceToChangeDirection$3 = 0
});
ScalaJS.c.Lenemies_Tank.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lenemies_Tank.prototype.constructor = ScalaJS.c.Lenemies_Tank;
/** @constructor */
ScalaJS.h.Lenemies_Tank = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Tank.prototype = ScalaJS.c.Lenemies_Tank.prototype;
ScalaJS.c.Lenemies_Tank.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().HUGEUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().HUGEUNITSIZE$1), ScalaJS.m.Lglobalvars_GV$().TANK$undHEALTH$1, 1, "zombie", 50, "tank");
  this.target$3 = null;
  this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0);
  this.chanceToChangeDirection$3 = 400;
  this.momentumFactor$2 = ScalaJS.m.Lglobalvars_GV$().HUGEMOMENTUMFACTOR$1;
  return this
});
ScalaJS.c.Lenemies_Tank.prototype.aiMove__Lgame_Game__V = (function(g) {
  var this$1 = g.r$1;
  if ((this$1.self$1.nextInt__I__I(400) === 0)) {
    g.addHeadText__Lobjects_Obj__T__I__V(this, "ROOOAAAR", 10)
  };
  this.target$3 = this.getClosestEnemyInLOS__Lgame_Game__Lobjects_Actor(g);
  if ((this.target$3 !== null)) {
    var dest = this.target$3.loc$1;
    var x = (dest.x$1 - this.loc$1.x$1);
    var dx = ((x > 0) ? 1.0 : ((x < 0) ? (-1.0) : x));
    var x$1 = (dest.y$1 - this.loc$1.y$1);
    var dy = ((x$1 > 0) ? 1.0 : ((x$1 < 0) ? (-1.0) : x$1));
    this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(dx, dy)
  } else if (this.direction$3.is$undzero__Z()) {
    var this$6 = g.r$1;
    var jsx$1 = this$6.self$1.nextInt__I__I(3);
    var this$7 = g.r$1;
    this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D((((-1) + jsx$1) | 0), (((-1) + this$7.self$1.nextInt__I__I(3)) | 0))
  };
  if ((!this.moveLoc__D__D__Lgame_Game__Z(this.direction$3.x$1, this.direction$3.y$1, g))) {
    this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0)
  } else {
    var this$8 = g.r$1;
    var n = this.chanceToChangeDirection$3;
    if ((this$8.self$1.nextInt__I__I(n) === 0)) {
      this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0)
    }
  };
  if (((this.target$3 !== null) && this.target$3.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.loc$1.x$1 + this.direction$3.x$1), (this.loc$1.y$1 + this.direction$3.y$1)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.size$1.x$1, this.size$1.y$1)))) {
    this.target$3.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(this, ScalaJS.m.Lglobalvars_GV$().TANK$undDAMAGE$1, 1.5, g)
  }
});
ScalaJS.c.Lenemies_Tank.prototype.draw__Lgame_Game__V = (function(g) {
  var jsx$1 = g.ctx$1;
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(30, 150, 30)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  jsx$1["fillStyle"] = s;
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.d.Lenemies_Tank = new ScalaJS.ClassTypeData({
  Lenemies_Tank: 0
}, false, "enemies.Tank", {
  Lenemies_Tank: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lenemies_Tank.prototype.$classData = ScalaJS.d.Lenemies_Tank;
/** @constructor */
ScalaJS.c.Lenemies_Zombie = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.target$3 = null;
  this.direction$3 = null;
  this.chanceToChangeDirection$3 = 0
});
ScalaJS.c.Lenemies_Zombie.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lenemies_Zombie.prototype.constructor = ScalaJS.c.Lenemies_Zombie;
/** @constructor */
ScalaJS.h.Lenemies_Zombie = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Zombie.prototype = ScalaJS.c.Lenemies_Zombie.prototype;
ScalaJS.c.Lenemies_Zombie.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), 20.0, 1, "zombie", 1, "zombie");
  this.target$3 = null;
  this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0);
  this.chanceToChangeDirection$3 = 400;
  return this
});
ScalaJS.c.Lenemies_Zombie.prototype.aiMove__Lgame_Game__V = (function(g) {
  var this$1 = g.r$1;
  if ((this$1.self$1.nextInt__I__I(400) === 0)) {
    g.addHeadText__Lobjects_Obj__T__I__V(this, "byeah", 10)
  };
  this.target$3 = this.getClosestEnemyInLOS__Lgame_Game__Lobjects_Actor(g);
  if ((this.target$3 !== null)) {
    var dest = this.target$3.loc$1;
    var x = (dest.x$1 - this.loc$1.x$1);
    var dx = ((x > 0) ? 1.0 : ((x < 0) ? (-1.0) : x));
    var x$1 = (dest.y$1 - this.loc$1.y$1);
    var dy = ((x$1 > 0) ? 1.0 : ((x$1 < 0) ? (-1.0) : x$1));
    this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(dx, dy)
  } else if (this.direction$3.is$undzero__Z()) {
    var this$6 = g.r$1;
    var jsx$1 = this$6.self$1.nextInt__I__I(3);
    var this$7 = g.r$1;
    this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D((((-1) + jsx$1) | 0), (((-1) + this$7.self$1.nextInt__I__I(3)) | 0))
  };
  if (((this.target$3 !== null) && this.target$3.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(new ScalaJS.c.Lglobalvars_Pt().init___D__D((this.loc$1.x$1 + this.direction$3.x$1), (this.loc$1.y$1 + this.direction$3.y$1)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.size$1.x$1, this.size$1.y$1)))) {
    this.target$3.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(this, 10.0, 1.0, g)
  };
  if ((!this.moveLoc__D__D__Lgame_Game__Z(this.direction$3.x$1, this.direction$3.y$1, g))) {
    this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0)
  } else {
    var this$8 = g.r$1;
    var n = this.chanceToChangeDirection$3;
    if ((this$8.self$1.nextInt__I__I(n) === 0)) {
      this.direction$3 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0)
    }
  }
});
ScalaJS.c.Lenemies_Zombie.prototype.draw__Lgame_Game__V = (function(g) {
  var jsx$1 = g.ctx$1;
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(30, 150, 30)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  jsx$1["fillStyle"] = s;
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.d.Lenemies_Zombie = new ScalaJS.ClassTypeData({
  Lenemies_Zombie: 0
}, false, "enemies.Zombie", {
  Lenemies_Zombie: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lenemies_Zombie.prototype.$classData = ScalaJS.d.Lenemies_Zombie;
/** @constructor */
ScalaJS.c.Ljava_io_OutputStream = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.Ljava_io_OutputStream.prototype = new ScalaJS.h.O();
ScalaJS.c.Ljava_io_OutputStream.prototype.constructor = ScalaJS.c.Ljava_io_OutputStream;
/** @constructor */
ScalaJS.h.Ljava_io_OutputStream = (function() {
  /*<skip>*/
});
ScalaJS.h.Ljava_io_OutputStream.prototype = ScalaJS.c.Ljava_io_OutputStream.prototype;
ScalaJS.c.Ljava_io_OutputStream.prototype.close__V = (function() {
  /*<skip>*/
});
/** @constructor */
ScalaJS.c.Lobjects_CausticAcid = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.radius$3 = 0;
  this.reduceRate$3 = 0;
  this.reduceTimer$3 = 0;
  this.minRadius$3 = 0
});
ScalaJS.c.Lobjects_CausticAcid.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_CausticAcid.prototype.constructor = ScalaJS.c.Lobjects_CausticAcid;
/** @constructor */
ScalaJS.h.Lobjects_CausticAcid = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_CausticAcid.prototype = ScalaJS.c.Lobjects_CausticAcid.prototype;
ScalaJS.c.Lobjects_CausticAcid.prototype.aiMove__Lgame_Game__V = (function(g) {
  g.acts$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, g$6) {
    return (function(a$2) {
      var a = ScalaJS.as.Lobjects_Actor(a$2);
      if ((((a.faction$2 !== "NA") && arg$outer.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(a.loc$1, a.size$1)) && arg$outer.hasLosTo__Lobjects_Obj__Lgame_Game__Z(a, g$6))) {
        a.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(arg$outer, 1.0, 0.0, g$6)
      }
    })
  })(this, g)));
  if ((this.reduceTimer$3 === 0)) {
    if ((this.radius$3 <= this.minRadius$3)) {
      g.removeActor__Lobjects_Actor__s_Option(this)
    } else {
      this.reduceTimer$3 = this.reduceRate$3;
      this.radius$3 = (((-1) + this.radius$3) | 0);
      var newLoc = new ScalaJS.c.Lglobalvars_Pt().init___D__D((1 + this.loc$1.x$1), (1 + this.loc$1.y$1));
      this.loc$1 = newLoc.cloone__Lglobalvars_Pt();
      this.size$1.x$1 = ((-2.0) + this.size$1.x$1);
      this.size$1.y$1 = ((-2.0) + this.size$1.y$1)
    }
  } else if ((this.reduceTimer$3 > 0)) {
    this.reduceTimer$3 = (((-1) + this.reduceTimer$3) | 0)
  }
});
ScalaJS.c.Lobjects_CausticAcid.prototype.init___Lglobalvars_Pt__I__I = (function(loc_, radius_, reduceRate_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, new ScalaJS.c.Lglobalvars_Pt().init___D__D((loc_.x$1 - radius_), (loc_.y$1 - radius_)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.imul(2, radius_), ScalaJS.imul(2, radius_)), (-1.0), 0, "NA", 0, "caustic acid");
  this.radius$3 = radius_;
  this.reduceRate$3 = reduceRate_;
  this.reduceTimer$3 = this.reduceRate$3;
  this.minRadius$3 = ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1;
  this.blocksMovement$1 = false;
  this.lowPriority$1 = true;
  return this
});
ScalaJS.c.Lobjects_CausticAcid.prototype.draw__Lgame_Game__V = (function(g) {
  var jsx$1 = g.ctx$1;
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(0, 255, 0)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  jsx$1["fillStyle"] = s;
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.d.Lobjects_CausticAcid = new ScalaJS.ClassTypeData({
  Lobjects_CausticAcid: 0
}, false, "objects.CausticAcid", {
  Lobjects_CausticAcid: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_CausticAcid.prototype.$classData = ScalaJS.d.Lobjects_CausticAcid;
/** @constructor */
ScalaJS.c.Lobjects_DelayedActor = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.act$3 = null;
  this.time$3 = 0
});
ScalaJS.c.Lobjects_DelayedActor.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_DelayedActor.prototype.constructor = ScalaJS.c.Lobjects_DelayedActor;
/** @constructor */
ScalaJS.h.Lobjects_DelayedActor = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_DelayedActor.prototype = ScalaJS.c.Lobjects_DelayedActor.prototype;
ScalaJS.c.Lobjects_DelayedActor.prototype.aiMove__Lgame_Game__V = (function(g) {
  if ((this.time$3 <= 0)) {
    if ((!g.collision__Lobjects_Obj__Z(this.act$3))) {
      g.addActor__Lobjects_Actor__scm_Set(this.act$3);
      var this$1 = g.delayedActs$1;
      ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(this$1, this)
    } else {
      var jsx$1 = this.time$3;
      var this$2 = g.r$1;
      this.time$3 = ((jsx$1 + this$2.self$1.nextInt__I__I(1000)) | 0)
    }
  } else {
    this.time$3 = (((-1) + this.time$3) | 0)
  }
});
ScalaJS.c.Lobjects_DelayedActor.prototype.init___Lobjects_Actor__I = (function(act_, delayTime_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, new ScalaJS.c.Lglobalvars_Pt().init___D__D((-1.0), (-1.0)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0), (-1.0), act_.speed$2, "NA", 0, ("delayed " + act_.name$2));
  this.act$3 = act_;
  this.time$3 = delayTime_;
  this.blocksMovement$1 = false;
  return this
});
ScalaJS.c.Lobjects_DelayedActor.prototype.moveToNewMap__Lgame_Game__V = (function(g) {
  /*<skip>*/
});
ScalaJS.c.Lobjects_DelayedActor.prototype.draw__Lgame_Game__V = (function(g) {
  /*<skip>*/
});
ScalaJS.is.Lobjects_DelayedActor = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_DelayedActor)))
});
ScalaJS.as.Lobjects_DelayedActor = (function(obj) {
  return ((ScalaJS.is.Lobjects_DelayedActor(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.DelayedActor"))
});
ScalaJS.isArrayOf.Lobjects_DelayedActor = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_DelayedActor)))
});
ScalaJS.asArrayOf.Lobjects_DelayedActor = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_DelayedActor(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.DelayedActor;", depth))
});
ScalaJS.d.Lobjects_DelayedActor = new ScalaJS.ClassTypeData({
  Lobjects_DelayedActor: 0
}, false, "objects.DelayedActor", {
  Lobjects_DelayedActor: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_DelayedActor.prototype.$classData = ScalaJS.d.Lobjects_DelayedActor;
/** @constructor */
ScalaJS.c.Lobjects_DummyItem = (function() {
  ScalaJS.c.Lobjects_Actor.call(this)
});
ScalaJS.c.Lobjects_DummyItem.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_DummyItem.prototype.constructor = ScalaJS.c.Lobjects_DummyItem;
/** @constructor */
ScalaJS.h.Lobjects_DummyItem = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_DummyItem.prototype = ScalaJS.c.Lobjects_DummyItem.prototype;
ScalaJS.c.Lobjects_DummyItem.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0), (-1.0), 0, "NA", 0, "dummy");
  return this
});
ScalaJS.is.Lobjects_DummyItem = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_DummyItem)))
});
ScalaJS.as.Lobjects_DummyItem = (function(obj) {
  return ((ScalaJS.is.Lobjects_DummyItem(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.DummyItem"))
});
ScalaJS.isArrayOf.Lobjects_DummyItem = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_DummyItem)))
});
ScalaJS.asArrayOf.Lobjects_DummyItem = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_DummyItem(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.DummyItem;", depth))
});
ScalaJS.d.Lobjects_DummyItem = new ScalaJS.ClassTypeData({
  Lobjects_DummyItem: 0
}, false, "objects.DummyItem", {
  Lobjects_DummyItem: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_DummyItem.prototype.$classData = ScalaJS.d.Lobjects_DummyItem;
/** @constructor */
ScalaJS.c.Lobjects_FirePatch = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.radius$3 = 0;
  this.timer$3 = 0;
  this.actsOnFire$3 = null
});
ScalaJS.c.Lobjects_FirePatch.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_FirePatch.prototype.constructor = ScalaJS.c.Lobjects_FirePatch;
/** @constructor */
ScalaJS.h.Lobjects_FirePatch = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_FirePatch.prototype = ScalaJS.c.Lobjects_FirePatch.prototype;
ScalaJS.c.Lobjects_FirePatch.prototype.aiMove__Lgame_Game__V = (function(g) {
  g.acts$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, g$7) {
    return (function(a$2) {
      var a = ScalaJS.as.Lobjects_Actor(a$2);
      if ((((a.faction$2 !== "NA") && arg$outer.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(a.loc$1, a.size$1)) && arg$outer.hasLosTo__Lobjects_Obj__Lgame_Game__Z(a, g$7))) {
        var this$1 = arg$outer.actsOnFire$3;
        return this$1.$$plus$eq__O__scm_HashSet(a)
      } else {
        return (void 0)
      }
    })
  })(this, g)));
  this.actsOnFire$3.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer$1, g$7$1) {
    return (function(a$2$1) {
      var a$1 = ScalaJS.as.Lobjects_Actor(a$2$1);
      a$1.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(arg$outer$1, ScalaJS.m.Lglobalvars_GV$().FIRE$undDAMAGE$1, 0.0, g$7$1);
      if (a$1.isDead__Z()) {
        var this$2 = arg$outer$1.actsOnFire$3;
        return ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(this$2, a$1)
      } else {
        return (void 0)
      }
    })
  })(this, g)));
  if ((this.timer$3 <= 0)) {
    var this$3 = this.actsOnFire$3;
    ScalaJS.s.scm_FlatHashTable$class__clearTable__scm_FlatHashTable__V(this$3);
    g.removeActor__Lobjects_Actor__s_Option(this)
  } else {
    this.timer$3 = (((-1) + this.timer$3) | 0)
  }
});
ScalaJS.c.Lobjects_FirePatch.prototype.init___Lglobalvars_Pt__I__I = (function(loc_, radius_, timer_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, new ScalaJS.c.Lglobalvars_Pt().init___D__D((loc_.x$1 - radius_), (loc_.y$1 - radius_)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.imul(2, radius_), ScalaJS.imul(2, radius_)), (-1.0), 0, "NA", 0, "fire");
  this.radius$3 = radius_;
  this.timer$3 = timer_;
  this.actsOnFire$3 = ScalaJS.as.scm_Set(ScalaJS.m.scm_Set$().apply__sc_Seq__sc_GenTraversable(ScalaJS.m.sci_Nil$()));
  this.blocksMovement$1 = false;
  this.lowPriority$1 = true;
  return this
});
ScalaJS.c.Lobjects_FirePatch.prototype.draw__Lgame_Game__V = (function(g) {
  var jsx$1 = g.ctx$1;
  var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(200, 50, 50)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
  jsx$1["fillStyle"] = s;
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.d.Lobjects_FirePatch = new ScalaJS.ClassTypeData({
  Lobjects_FirePatch: 0
}, false, "objects.FirePatch", {
  Lobjects_FirePatch: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_FirePatch.prototype.$classData = ScalaJS.d.Lobjects_FirePatch;
/** @constructor */
ScalaJS.c.Lobjects_Item = (function() {
  ScalaJS.c.Lobjects_Actor.call(this)
});
ScalaJS.c.Lobjects_Item.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_Item.prototype.constructor = ScalaJS.c.Lobjects_Item;
/** @constructor */
ScalaJS.h.Lobjects_Item = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_Item.prototype = ScalaJS.c.Lobjects_Item.prototype;
ScalaJS.c.Lobjects_Item.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), (-1.0), 0, "NA", 0, "item");
  this.blocksMovement$1 = false;
  return this
});
ScalaJS.c.Lobjects_Item.prototype.aiMove__Lgame_Game__V = (function(g) {
  g.acts$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, g$1) {
    return (function(a$2) {
      var a = ScalaJS.as.Lobjects_Actor(a$2);
      return ((arg$outer.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(a.loc$1, a.size$1) && a.canTakeItem__Lobjects_Actor__Z(arg$outer)) ? (arg$outer.pickup__Lobjects_Actor__V(a), g$1.removeActor__Lobjects_Actor__s_Option(arg$outer)) : (void 0))
    })
  })(this, g)))
});
ScalaJS.is.Lobjects_Item = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_Item)))
});
ScalaJS.as.Lobjects_Item = (function(obj) {
  return ((ScalaJS.is.Lobjects_Item(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.Item"))
});
ScalaJS.isArrayOf.Lobjects_Item = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_Item)))
});
ScalaJS.asArrayOf.Lobjects_Item = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_Item(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.Item;", depth))
});
/** @constructor */
ScalaJS.c.Lobjects_LandMine = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.radius$3 = 0;
  this.damage$3 = 0;
  this.enabledTimer$3 = 0
});
ScalaJS.c.Lobjects_LandMine.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_LandMine.prototype.constructor = ScalaJS.c.Lobjects_LandMine;
/** @constructor */
ScalaJS.h.Lobjects_LandMine = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_LandMine.prototype = ScalaJS.c.Lobjects_LandMine.prototype;
ScalaJS.c.Lobjects_LandMine.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), (-1.0), 0, "NA", 0, "land mine");
  this.radius$3 = ScalaJS.m.Lglobalvars_GV$().LANDMINE$undRADIUS$1;
  this.damage$3 = ScalaJS.m.Lglobalvars_GV$().LANDMINE$undDAMAGE$1;
  this.enabledTimer$3 = ScalaJS.m.Lglobalvars_GV$().LANDMINE$undDELAY$1;
  this.blocksMovement$1 = false;
  this.lowPriority$1 = true;
  return this
});
ScalaJS.c.Lobjects_LandMine.prototype.aiMove__Lgame_Game__V = (function(g) {
  if ((this.enabledTimer$3 <= 0)) {
    g.acts$1.foreach__F1__V(new ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4().init___Lobjects_LandMine__Lgame_Game(this, g))
  } else {
    this.enabledTimer$3 = (((-1) + this.enabledTimer$3) | 0)
  }
});
ScalaJS.c.Lobjects_LandMine.prototype.draw__Lgame_Game__V = (function(g) {
  if ((this.enabledTimer$3 <= 0)) {
    var jsx$1 = g.ctx$1;
    var s = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(50, 50, 50)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
    jsx$1["fillStyle"] = s;
    g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1);
    var jsx$2 = g.ctx$1;
    var s$1 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(200, 50, 50)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
    jsx$2["fillStyle"] = s$1;
    g.ctx$1["fillRect"]((2 + this.loc$1.x$1), (2 + this.loc$1.y$1), ((-4.0) + this.size$1.x$1), ((-4.0) + this.size$1.y$1))
  } else {
    var jsx$3 = g.ctx$1;
    var s$2 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(50, 50, 50)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
    jsx$3["fillStyle"] = s$2;
    g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1);
    var jsx$4 = g.ctx$1;
    var s$3 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["rgb(50, 50, 50)"])).s__sc_Seq__T(ScalaJS.m.sci_Nil$());
    jsx$4["fillStyle"] = s$3;
    g.ctx$1["fillRect"]((2 + this.loc$1.x$1), (2 + this.loc$1.y$1), ((-4.0) + this.size$1.x$1), ((-4.0) + this.size$1.y$1))
  }
});
ScalaJS.d.Lobjects_LandMine = new ScalaJS.ClassTypeData({
  Lobjects_LandMine: 0
}, false, "objects.LandMine", {
  Lobjects_LandMine: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_LandMine.prototype.$classData = ScalaJS.d.Lobjects_LandMine;
/** @constructor */
ScalaJS.c.Lobjects_PipeBomb = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.timer$3 = 0
});
ScalaJS.c.Lobjects_PipeBomb.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_PipeBomb.prototype.constructor = ScalaJS.c.Lobjects_PipeBomb;
/** @constructor */
ScalaJS.h.Lobjects_PipeBomb = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_PipeBomb.prototype = ScalaJS.c.Lobjects_PipeBomb.prototype;
ScalaJS.c.Lobjects_PipeBomb.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), 100000.0, 0, "human", 0, "pipe bomb");
  this.timer$3 = ScalaJS.m.Lglobalvars_GV$().PIPEBOMB$undTIME$1;
  this.momentumFactor$2 = 0.0;
  return this
});
ScalaJS.c.Lobjects_PipeBomb.prototype.aiMove__Lgame_Game__V = (function(g) {
  if ((this.timer$3 <= 0)) {
    g.removeActor__Lobjects_Actor__s_Option(this);
    var bomb = new ScalaJS.c.Lobjects_SimpleBomb().init___Lglobalvars_Pt__I__I(this.loc$1, ScalaJS.m.Lglobalvars_GV$().PIPEBOMB$undRADIUS$1, ScalaJS.m.Lglobalvars_GV$().PIPEBOMB$undDAMAGE$1);
    g.addActor__Lobjects_Actor__scm_Set(bomb)
  } else {
    this.timer$3 = (((-1) + this.timer$3) | 0)
  }
});
ScalaJS.c.Lobjects_PipeBomb.prototype.draw__Lgame_Game__V = (function(g) {
  var img = g.images$1.apply__O__O("item_pipebomb");
  g.ctx$1["drawImage"](img, this.loc$1.x$1, this.loc$1.y$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1)
});
ScalaJS.d.Lobjects_PipeBomb = new ScalaJS.ClassTypeData({
  Lobjects_PipeBomb: 0
}, false, "objects.PipeBomb", {
  Lobjects_PipeBomb: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_PipeBomb.prototype.$classData = ScalaJS.d.Lobjects_PipeBomb;
/** @constructor */
ScalaJS.c.Lobjects_ProjectileActor = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.line$3 = null;
  this.act$3 = null;
  this.rate$3 = 0
});
ScalaJS.c.Lobjects_ProjectileActor.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_ProjectileActor.prototype.constructor = ScalaJS.c.Lobjects_ProjectileActor;
/** @constructor */
ScalaJS.h.Lobjects_ProjectileActor = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_ProjectileActor.prototype = ScalaJS.c.Lobjects_ProjectileActor.prototype;
ScalaJS.c.Lobjects_ProjectileActor.prototype.init___Lobjects_Actor__Lglobalvars_SimpleLine__I = (function(act_, line_, rate_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, line_.start$1.cloone__Lglobalvars_Pt(), new ScalaJS.c.Lglobalvars_Pt().init___D__D(0.0, 0.0), (-1.0), 0, "NA", 0, ("projectile " + act_.name$2));
  this.line$3 = line_;
  this.act$3 = act_;
  this.rate$3 = rate_;
  this.blocksMovement$1 = false;
  return this
});
ScalaJS.c.Lobjects_ProjectileActor.prototype.aiMove__Lgame_Game__V = (function(g) {
  var step = this.line$3.unitStep__Lglobalvars_Pt();
  var elem$1 = false;
  elem$1 = false;
  var start = this.loc$1.cloone__Lglobalvars_Pt();
  var end = this.rate$3;
  var isEmpty$4 = (end < 1);
  var numRangeElements$4 = (isEmpty$4 ? 0 : end);
  var lastElement$4 = (isEmpty$4 ? 0 : end);
  var terminalElement$4 = ((1 + lastElement$4) | 0);
  if ((numRangeElements$4 < 0)) {
    ScalaJS.m.sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(1, end, 1, true)
  };
  var i = 1;
  var count = 0;
  while ((i !== terminalElement$4)) {
    var v1 = i;
    if ((!elem$1)) {
      var x = (this.loc$1.x$1 - this.line$3.end$1.x$1);
      if ((((x < 0) ? (-x) : x) < 1)) {
        var x$1 = (this.loc$1.y$1 - this.line$3.end$1.y$1);
        var jsx$1 = (((x$1 < 0) ? (-x$1) : x$1) < 1)
      } else {
        var jsx$1 = false
      };
      if (jsx$1) {
        g.addActor__Lobjects_Actor__scm_Set(this.act$3);
        g.removeActor__Lobjects_Actor__s_Option(this);
        elem$1 = true
      } else {
        this.changeLocRel__D__D__V(step.x$1, step.y$1)
      }
    };
    count = ((1 + count) | 0);
    i = ((1 + i) | 0)
  };
  var lineToDraw = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(start, this.loc$1, this.line$3.color$1);
  g.linesToDraw$1.$$plus$eq__O__scm_Buffer(lineToDraw)
});
ScalaJS.c.Lobjects_ProjectileActor.prototype.draw__Lgame_Game__V = (function(g) {
  /*<skip>*/
});
ScalaJS.is.Lobjects_ProjectileActor = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_ProjectileActor)))
});
ScalaJS.as.Lobjects_ProjectileActor = (function(obj) {
  return ((ScalaJS.is.Lobjects_ProjectileActor(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.ProjectileActor"))
});
ScalaJS.isArrayOf.Lobjects_ProjectileActor = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_ProjectileActor)))
});
ScalaJS.asArrayOf.Lobjects_ProjectileActor = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_ProjectileActor(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.ProjectileActor;", depth))
});
ScalaJS.d.Lobjects_ProjectileActor = new ScalaJS.ClassTypeData({
  Lobjects_ProjectileActor: 0
}, false, "objects.ProjectileActor", {
  Lobjects_ProjectileActor: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_ProjectileActor.prototype.$classData = ScalaJS.d.Lobjects_ProjectileActor;
/** @constructor */
ScalaJS.c.Lobjects_SimpleBomb = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.radius$3 = 0;
  this.damage$3 = 0
});
ScalaJS.c.Lobjects_SimpleBomb.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_SimpleBomb.prototype.constructor = ScalaJS.c.Lobjects_SimpleBomb;
/** @constructor */
ScalaJS.h.Lobjects_SimpleBomb = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_SimpleBomb.prototype = ScalaJS.c.Lobjects_SimpleBomb.prototype;
ScalaJS.c.Lobjects_SimpleBomb.prototype.aiMove__Lgame_Game__V = (function(g) {
  g.acts$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, g$4) {
    return (function(a$2) {
      var a = ScalaJS.as.Lobjects_Actor(a$2);
      if (((a.faction$2 !== "NA") && (arg$outer.distanceTo__Lobjects_Obj__I(a) < arg$outer.radius$3))) {
        a.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(arg$outer, arg$outer.damage$3, 1.0, g$4)
      }
    })
  })(this, g)));
  g.removeActor__Lobjects_Actor__s_Option(this)
});
ScalaJS.c.Lobjects_SimpleBomb.prototype.init___Lglobalvars_Pt__I__I = (function(loc_, radius_, damage_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), (-1.0), 0, "NA", 0, "bomb");
  this.radius$3 = radius_;
  this.damage$3 = damage_;
  return this
});
ScalaJS.d.Lobjects_SimpleBomb = new ScalaJS.ClassTypeData({
  Lobjects_SimpleBomb: 0
}, false, "objects.SimpleBomb", {
  Lobjects_SimpleBomb: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_SimpleBomb.prototype.$classData = ScalaJS.d.Lobjects_SimpleBomb;
/** @constructor */
ScalaJS.c.Lobjects_UsableGrenade = (function() {
  ScalaJS.c.Lobjects_ThrowableItem.call(this)
});
ScalaJS.c.Lobjects_UsableGrenade.prototype = new ScalaJS.h.Lobjects_ThrowableItem();
ScalaJS.c.Lobjects_UsableGrenade.prototype.constructor = ScalaJS.c.Lobjects_UsableGrenade;
/** @constructor */
ScalaJS.h.Lobjects_UsableGrenade = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_UsableGrenade.prototype = ScalaJS.c.Lobjects_UsableGrenade.prototype;
ScalaJS.c.Lobjects_UsableGrenade.prototype.use__Lgame_Game__V = (function(g) {
  var dest = this.getThrowPosition__Lglobalvars_Pt();
  var spit = new ScalaJS.c.Lobjects_SimpleBomb().init___Lglobalvars_Pt__I__I(dest, ScalaJS.m.Lglobalvars_GV$().GRENADE$undRADIUS$1, ScalaJS.m.Lglobalvars_GV$().GRENADE$undDAMAGE$1);
  var spitLine = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(this.owner$1.loc$1.cloone__Lglobalvars_Pt(), dest, "black");
  var proj = new ScalaJS.c.Lobjects_ProjectileActor().init___Lobjects_Actor__Lglobalvars_SimpleLine__I(spit, spitLine, this.speed$2);
  g.addActor__Lobjects_Actor__scm_Set(proj)
});
ScalaJS.c.Lobjects_UsableGrenade.prototype.init___Lobjects_Actor = (function(owner_) {
  ScalaJS.c.Lobjects_ThrowableItem.prototype.init___Lobjects_Actor__T__T.call(this, owner_, "grenade", "item_pipebomb");
  return this
});
ScalaJS.d.Lobjects_UsableGrenade = new ScalaJS.ClassTypeData({
  Lobjects_UsableGrenade: 0
}, false, "objects.UsableGrenade", {
  Lobjects_UsableGrenade: 1,
  Lobjects_ThrowableItem: 1,
  Lobjects_UsableItem: 1,
  O: 1
});
ScalaJS.c.Lobjects_UsableGrenade.prototype.$classData = ScalaJS.d.Lobjects_UsableGrenade;
/** @constructor */
ScalaJS.c.Lobjects_UsablePipeBomb = (function() {
  ScalaJS.c.Lobjects_ThrowableItem.call(this)
});
ScalaJS.c.Lobjects_UsablePipeBomb.prototype = new ScalaJS.h.Lobjects_ThrowableItem();
ScalaJS.c.Lobjects_UsablePipeBomb.prototype.constructor = ScalaJS.c.Lobjects_UsablePipeBomb;
/** @constructor */
ScalaJS.h.Lobjects_UsablePipeBomb = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_UsablePipeBomb.prototype = ScalaJS.c.Lobjects_UsablePipeBomb.prototype;
ScalaJS.c.Lobjects_UsablePipeBomb.prototype.use__Lgame_Game__V = (function(g) {
  var dest = this.getThrowPosition__Lglobalvars_Pt();
  var spit = new ScalaJS.c.Lobjects_PipeBomb().init___Lglobalvars_Pt(dest);
  var spitLine = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(this.owner$1.loc$1.cloone__Lglobalvars_Pt(), dest, "black");
  var proj = new ScalaJS.c.Lobjects_ProjectileActor().init___Lobjects_Actor__Lglobalvars_SimpleLine__I(spit, spitLine, this.speed$2);
  g.addActor__Lobjects_Actor__scm_Set(proj)
});
ScalaJS.c.Lobjects_UsablePipeBomb.prototype.init___Lobjects_Actor = (function(owner_) {
  ScalaJS.c.Lobjects_ThrowableItem.prototype.init___Lobjects_Actor__T__T.call(this, owner_, "pipe bomb", "item_pipebomb");
  return this
});
ScalaJS.d.Lobjects_UsablePipeBomb = new ScalaJS.ClassTypeData({
  Lobjects_UsablePipeBomb: 0
}, false, "objects.UsablePipeBomb", {
  Lobjects_UsablePipeBomb: 1,
  Lobjects_ThrowableItem: 1,
  Lobjects_UsableItem: 1,
  O: 1
});
ScalaJS.c.Lobjects_UsablePipeBomb.prototype.$classData = ScalaJS.d.Lobjects_UsablePipeBomb;
/** @constructor */
ScalaJS.c.Lobjects_UsableSpitterAcid = (function() {
  ScalaJS.c.Lobjects_ThrowableItem.call(this)
});
ScalaJS.c.Lobjects_UsableSpitterAcid.prototype = new ScalaJS.h.Lobjects_ThrowableItem();
ScalaJS.c.Lobjects_UsableSpitterAcid.prototype.constructor = ScalaJS.c.Lobjects_UsableSpitterAcid;
/** @constructor */
ScalaJS.h.Lobjects_UsableSpitterAcid = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_UsableSpitterAcid.prototype = ScalaJS.c.Lobjects_UsableSpitterAcid.prototype;
ScalaJS.c.Lobjects_UsableSpitterAcid.prototype.use__Lgame_Game__V = (function(g) {
  var dest = this.getThrowPosition__Lglobalvars_Pt();
  var spit = new ScalaJS.c.Lobjects_CausticAcid().init___Lglobalvars_Pt__I__I(dest, ScalaJS.m.Lglobalvars_GV$().SPITTER$undSPITRADIUS$1, ScalaJS.m.Lglobalvars_GV$().SPITTER$undSPITREDUCERATE$1);
  var spitLine = new ScalaJS.c.Lglobalvars_SimpleLine().init___Lglobalvars_Pt__Lglobalvars_Pt__T(this.owner$1.loc$1.cloone__Lglobalvars_Pt(), dest, "black");
  var proj = new ScalaJS.c.Lobjects_ProjectileActor().init___Lobjects_Actor__Lglobalvars_SimpleLine__I(spit, spitLine, this.speed$2);
  g.addActor__Lobjects_Actor__scm_Set(proj)
});
ScalaJS.c.Lobjects_UsableSpitterAcid.prototype.init___Lobjects_Actor = (function(owner_) {
  ScalaJS.c.Lobjects_ThrowableItem.prototype.init___Lobjects_Actor__T__T.call(this, owner_, "spitter acid", "item_acid");
  return this
});
ScalaJS.d.Lobjects_UsableSpitterAcid = new ScalaJS.ClassTypeData({
  Lobjects_UsableSpitterAcid: 0
}, false, "objects.UsableSpitterAcid", {
  Lobjects_UsableSpitterAcid: 1,
  Lobjects_ThrowableItem: 1,
  Lobjects_UsableItem: 1,
  O: 1
});
ScalaJS.c.Lobjects_UsableSpitterAcid.prototype.$classData = ScalaJS.d.Lobjects_UsableSpitterAcid;
/** @constructor */
ScalaJS.c.Lobjects_ZombieSpawner = (function() {
  ScalaJS.c.Lobjects_Actor.call(this);
  this.spawnRate$3 = 0;
  this.timeToNextSpawn$3 = 0
});
ScalaJS.c.Lobjects_ZombieSpawner.prototype = new ScalaJS.h.Lobjects_Actor();
ScalaJS.c.Lobjects_ZombieSpawner.prototype.constructor = ScalaJS.c.Lobjects_ZombieSpawner;
/** @constructor */
ScalaJS.h.Lobjects_ZombieSpawner = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_ZombieSpawner.prototype = ScalaJS.c.Lobjects_ZombieSpawner.prototype;
ScalaJS.c.Lobjects_ZombieSpawner.prototype.aiMove__Lgame_Game__V = (function(g) {
  if ((this.timeToNextSpawn$3 === 0)) {
    var newZed = new ScalaJS.c.Lenemies_Zombie().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D((-100.0), (-100.0)));
    if (newZed.canMoveTo__Lglobalvars_Pt__Lgame_Game__Z(this.loc$1, g)) {
      g.addActor__Lobjects_Actor__scm_Set(newZed);
      var newLoc = this.loc$1;
      newZed.loc$1 = newLoc.cloone__Lglobalvars_Pt()
    };
    this.timeToNextSpawn$3 = this.spawnRate$3;
    var jsx$1 = ScalaJS.g["console"];
    var this$1 = g.acts$1;
    var s = (("Spawning: " + this$1.tableSize$5) + " total actors");
    jsx$1["log"](s)
  } else {
    this.timeToNextSpawn$3 = (((-1) + this.timeToNextSpawn$3) | 0)
  }
});
ScalaJS.c.Lobjects_ZombieSpawner.prototype.init___Lglobalvars_Pt__I = (function(loc_, spawnRate_) {
  ScalaJS.c.Lobjects_Actor.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__D__I__T__I__T.call(this, loc_, new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), (-1.0), 1, "NA", 0, "zombie spawner");
  this.spawnRate$3 = spawnRate_;
  this.timeToNextSpawn$3 = this.spawnRate$3;
  this.blocksMovement$1 = false;
  this.important$2 = true;
  return this
});
ScalaJS.c.Lobjects_ZombieSpawner.prototype.draw__Lgame_Game__V = (function(g) {
  /*<skip>*/
});
ScalaJS.d.Lobjects_ZombieSpawner = new ScalaJS.ClassTypeData({
  Lobjects_ZombieSpawner: 0
}, false, "objects.ZombieSpawner", {
  Lobjects_ZombieSpawner: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_ZombieSpawner.prototype.$classData = ScalaJS.d.Lobjects_ZombieSpawner;
ScalaJS.d.jl_Byte = new ScalaJS.ClassTypeData({
  jl_Byte: 0
}, false, "java.lang.Byte", {
  jl_Byte: 1,
  jl_Number: 1,
  O: 1,
  jl_Comparable: 1
}, (void 0), (function(x) {
  return ScalaJS.isByte(x)
}));
ScalaJS.isArrayOf.jl_Double = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Double)))
});
ScalaJS.asArrayOf.jl_Double = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_Double(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Double;", depth))
});
ScalaJS.d.jl_Double = new ScalaJS.ClassTypeData({
  jl_Double: 0
}, false, "java.lang.Double", {
  jl_Double: 1,
  jl_Number: 1,
  O: 1,
  jl_Comparable: 1
}, (void 0), (function(x) {
  return ((typeof x) === "number")
}));
/** @constructor */
ScalaJS.c.jl_Error = (function() {
  ScalaJS.c.jl_Throwable.call(this)
});
ScalaJS.c.jl_Error.prototype = new ScalaJS.h.jl_Throwable();
ScalaJS.c.jl_Error.prototype.constructor = ScalaJS.c.jl_Error;
/** @constructor */
ScalaJS.h.jl_Error = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Error.prototype = ScalaJS.c.jl_Error.prototype;
ScalaJS.c.jl_Error.prototype.init___T = (function(s) {
  ScalaJS.c.jl_Error.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
/** @constructor */
ScalaJS.c.jl_Exception = (function() {
  ScalaJS.c.jl_Throwable.call(this)
});
ScalaJS.c.jl_Exception.prototype = new ScalaJS.h.jl_Throwable();
ScalaJS.c.jl_Exception.prototype.constructor = ScalaJS.c.jl_Exception;
/** @constructor */
ScalaJS.h.jl_Exception = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_Exception.prototype = ScalaJS.c.jl_Exception.prototype;
ScalaJS.d.jl_Float = new ScalaJS.ClassTypeData({
  jl_Float: 0
}, false, "java.lang.Float", {
  jl_Float: 1,
  jl_Number: 1,
  O: 1,
  jl_Comparable: 1
}, (void 0), (function(x) {
  return ScalaJS.isFloat(x)
}));
ScalaJS.isArrayOf.jl_Integer = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Integer)))
});
ScalaJS.asArrayOf.jl_Integer = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_Integer(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Integer;", depth))
});
ScalaJS.d.jl_Integer = new ScalaJS.ClassTypeData({
  jl_Integer: 0
}, false, "java.lang.Integer", {
  jl_Integer: 1,
  jl_Number: 1,
  O: 1,
  jl_Comparable: 1
}, (void 0), (function(x) {
  return ScalaJS.isInt(x)
}));
ScalaJS.isArrayOf.jl_Long = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Long)))
});
ScalaJS.asArrayOf.jl_Long = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_Long(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.Long;", depth))
});
ScalaJS.d.jl_Long = new ScalaJS.ClassTypeData({
  jl_Long: 0
}, false, "java.lang.Long", {
  jl_Long: 1,
  jl_Number: 1,
  O: 1,
  jl_Comparable: 1
}, (void 0), (function(x) {
  return ScalaJS.is.sjsr_RuntimeLong(x)
}));
ScalaJS.d.jl_Short = new ScalaJS.ClassTypeData({
  jl_Short: 0
}, false, "java.lang.Short", {
  jl_Short: 1,
  jl_Number: 1,
  O: 1,
  jl_Comparable: 1
}, (void 0), (function(x) {
  return ScalaJS.isShort(x)
}));
/** @constructor */
ScalaJS.c.ju_Formatter = (function() {
  ScalaJS.c.O.call(this);
  this.dest$1 = null;
  this.closed$1 = false
});
ScalaJS.c.ju_Formatter.prototype = new ScalaJS.h.O();
ScalaJS.c.ju_Formatter.prototype.constructor = ScalaJS.c.ju_Formatter;
/** @constructor */
ScalaJS.h.ju_Formatter = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_Formatter.prototype = ScalaJS.c.ju_Formatter.prototype;
ScalaJS.c.ju_Formatter.prototype.init___ = (function() {
  ScalaJS.c.ju_Formatter.prototype.init___jl_Appendable.call(this, new ScalaJS.c.jl_StringBuilder().init___());
  return this
});
ScalaJS.c.ju_Formatter.prototype.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable = (function(argStr, prefix, preventZero, flags$1, width$1, conversion$1) {
  var prePadLen = ((ScalaJS.uI(argStr["length"]) + ScalaJS.uI(prefix["length"])) | 0);
  if ((width$1 <= prePadLen)) {
    var padStr = (("" + prefix) + argStr)
  } else {
    var padRight = this.hasFlag$1__p1__T__T__Z("-", flags$1);
    var padZero = (this.hasFlag$1__p1__T__T__Z("0", flags$1) && (!ScalaJS.uZ(preventZero)));
    var padLength = ((width$1 - prePadLen) | 0);
    var padChar = (padZero ? "0" : " ");
    var padding = this.strRepeat$1__p1__T__I__T(padChar, padLength);
    if ((padZero && padRight)) {
      var padStr;
      throw new ScalaJS.c.ju_IllegalFormatFlagsException().init___T(flags$1)
    } else {
      var padStr = (padRight ? ((("" + prefix) + argStr) + padding) : (padZero ? ((("" + prefix) + padding) + argStr) : ((("" + padding) + prefix) + argStr)))
    }
  };
  var casedStr = (ScalaJS.m.jl_Character$().isUpperCase__C__Z(conversion$1) ? ScalaJS.as.T(padStr["toUpperCase"]()) : padStr);
  return this.dest$1.append__jl_CharSequence__jl_Appendable(casedStr)
});
ScalaJS.c.ju_Formatter.prototype.toString__T = (function() {
  return this.out__jl_Appendable().toString__T()
});
ScalaJS.c.ju_Formatter.prototype.init___jl_Appendable = (function(dest) {
  this.dest$1 = dest;
  this.closed$1 = false;
  return this
});
ScalaJS.c.ju_Formatter.prototype.padCaptureSign$1__p1__T__T__T__I__C__jl_Appendable = (function(argStr, prefix, flags$1, width$1, conversion$1) {
  var firstChar = (65535 & ScalaJS.uI(argStr["charCodeAt"](0)));
  return (((firstChar === 43) || (firstChar === 45)) ? this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(ScalaJS.as.T(argStr["substring"](1)), (("" + new ScalaJS.c.jl_Character().init___C(firstChar)) + prefix), false, flags$1, width$1, conversion$1) : this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(argStr, prefix, false, flags$1, width$1, conversion$1))
});
ScalaJS.c.ju_Formatter.prototype.hasFlag$1__p1__T__T__Z = (function(flag, flags$1) {
  return (ScalaJS.uI(flags$1["indexOf"](flag)) >= 0)
});
ScalaJS.c.ju_Formatter.prototype.out__jl_Appendable = (function() {
  return (this.closed$1 ? this.java$util$Formatter$$throwClosedException__sr_Nothing$() : this.dest$1)
});
ScalaJS.c.ju_Formatter.prototype.format__T__AO__ju_Formatter = (function(format_in, args) {
  if (this.closed$1) {
    this.java$util$Formatter$$throwClosedException__sr_Nothing$()
  } else {
    var fmt = format_in;
    var lastImplicitIndex = 0;
    var lastIndex = 0;
    while (true) {
      var thiz = fmt;
      if ((thiz === null)) {
        var jsx$1;
        throw new ScalaJS.c.jl_NullPointerException().init___()
      } else {
        var jsx$1 = thiz
      };
      if ((!(jsx$1 === ""))) {
        var x1 = fmt;
        matchEnd9: {
          var o12 = ScalaJS.m.ju_Formatter$().java$util$Formatter$$RegularChunk$1.unapply__T__s_Option(x1);
          if ((!o12.isEmpty__Z())) {
            var matchResult = o12.get__O();
            var thiz$2 = fmt;
            var $$this = matchResult[0];
            if (($$this === (void 0))) {
              var jsx$2;
              throw new ScalaJS.c.ju_NoSuchElementException().init___T("undefined.get")
            } else {
              var jsx$2 = $$this
            };
            var thiz$1 = ScalaJS.as.T(jsx$2);
            var beginIndex = ScalaJS.uI(thiz$1["length"]);
            fmt = ScalaJS.as.T(thiz$2["substring"](beginIndex));
            var jsx$4 = this.dest$1;
            var $$this$1 = matchResult[0];
            if (($$this$1 === (void 0))) {
              var jsx$3;
              throw new ScalaJS.c.ju_NoSuchElementException().init___T("undefined.get")
            } else {
              var jsx$3 = $$this$1
            };
            jsx$4.append__jl_CharSequence__jl_Appendable(ScalaJS.as.jl_CharSequence(jsx$3));
            break matchEnd9
          };
          var o14 = ScalaJS.m.ju_Formatter$().java$util$Formatter$$DoublePercent$1.unapply__T__s_Option(x1);
          if ((!o14.isEmpty__Z())) {
            var thiz$3 = fmt;
            fmt = ScalaJS.as.T(thiz$3["substring"](2));
            this.dest$1.append__C__jl_Appendable(37);
            break matchEnd9
          };
          var o16 = ScalaJS.m.ju_Formatter$().java$util$Formatter$$EOLChunk$1.unapply__T__s_Option(x1);
          if ((!o16.isEmpty__Z())) {
            var thiz$4 = fmt;
            fmt = ScalaJS.as.T(thiz$4["substring"](2));
            this.dest$1.append__C__jl_Appendable(10);
            break matchEnd9
          };
          var o18 = ScalaJS.m.ju_Formatter$().java$util$Formatter$$FormattedChunk$1.unapply__T__s_Option(x1);
          if ((!o18.isEmpty__Z())) {
            var matchResult$2 = o18.get__O();
            var thiz$6 = fmt;
            var $$this$2 = matchResult$2[0];
            if (($$this$2 === (void 0))) {
              var jsx$5;
              throw new ScalaJS.c.ju_NoSuchElementException().init___T("undefined.get")
            } else {
              var jsx$5 = $$this$2
            };
            var thiz$5 = ScalaJS.as.T(jsx$5);
            var beginIndex$1 = ScalaJS.uI(thiz$5["length"]);
            fmt = ScalaJS.as.T(thiz$6["substring"](beginIndex$1));
            var $$this$3 = matchResult$2[2];
            if (($$this$3 === (void 0))) {
              var jsx$6;
              throw new ScalaJS.c.ju_NoSuchElementException().init___T("undefined.get")
            } else {
              var jsx$6 = $$this$3
            };
            var flags = ScalaJS.as.T(jsx$6);
            var $$this$4 = matchResult$2[1];
            var indexStr = ScalaJS.as.T((($$this$4 === (void 0)) ? "" : $$this$4));
            if ((indexStr === null)) {
              var jsx$7;
              throw new ScalaJS.c.jl_NullPointerException().init___()
            } else {
              var jsx$7 = indexStr
            };
            if ((jsx$7 !== "")) {
              var this$28 = ScalaJS.m.jl_Integer$();
              var index = this$28.parseInt__T__I__I(indexStr, 10)
            } else if (this.hasFlag$1__p1__T__T__Z("<", flags)) {
              var index = lastIndex
            } else {
              lastImplicitIndex = ((1 + lastImplicitIndex) | 0);
              var index = lastImplicitIndex
            };
            lastIndex = index;
            if (((index <= 0) || (index > args.u["length"]))) {
              var $$this$5 = matchResult$2[5];
              if (($$this$5 === (void 0))) {
                var jsx$8;
                throw new ScalaJS.c.ju_NoSuchElementException().init___T("undefined.get")
              } else {
                var jsx$8 = $$this$5
              };
              throw new ScalaJS.c.ju_MissingFormatArgumentException().init___T(ScalaJS.as.T(jsx$8))
            };
            var arg = args.u[(((-1) + index) | 0)];
            var $$this$6 = matchResult$2[3];
            var widthStr = ScalaJS.as.T((($$this$6 === (void 0)) ? "" : $$this$6));
            if ((widthStr === null)) {
              var jsx$9;
              throw new ScalaJS.c.jl_NullPointerException().init___()
            } else {
              var jsx$9 = widthStr
            };
            var hasWidth = (jsx$9 !== "");
            if (hasWidth) {
              var this$36 = ScalaJS.m.jl_Integer$();
              var width = this$36.parseInt__T__I__I(widthStr, 10)
            } else {
              var width = 0
            };
            var $$this$7 = matchResult$2[4];
            var precisionStr = ScalaJS.as.T((($$this$7 === (void 0)) ? "" : $$this$7));
            if ((precisionStr === null)) {
              var jsx$10;
              throw new ScalaJS.c.jl_NullPointerException().init___()
            } else {
              var jsx$10 = precisionStr
            };
            var hasPrecision = (jsx$10 !== "");
            if (hasPrecision) {
              var this$41 = ScalaJS.m.jl_Integer$();
              var precision = this$41.parseInt__T__I__I(precisionStr, 10)
            } else {
              var precision = 0
            };
            var $$this$8 = matchResult$2[5];
            if (($$this$8 === (void 0))) {
              var jsx$11;
              throw new ScalaJS.c.ju_NoSuchElementException().init___T("undefined.get")
            } else {
              var jsx$11 = $$this$8
            };
            var thiz$7 = ScalaJS.as.T(jsx$11);
            var conversion = (65535 & ScalaJS.uI(thiz$7["charCodeAt"](0)));
            switch (conversion) {
              case 98:
                /*<skip>*/;
              case 66:
                {
                  if ((arg === null)) {
                    var jsx$12 = "false"
                  } else if (((typeof arg) === "boolean")) {
                    var x3 = ScalaJS.asBoolean(arg);
                    var jsx$12 = ScalaJS.m.sjsr_RuntimeString$().valueOf__O__T(x3)
                  } else {
                    var jsx$12 = "true"
                  };
                  this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(jsx$12, "", false, flags, width, conversion);
                  break
                };
              case 104:
                /*<skip>*/;
              case 72:
                {
                  if ((arg === null)) {
                    var jsx$13 = "null"
                  } else {
                    var i = ScalaJS.objectHashCode(arg);
                    var x = ScalaJS.uD((i >>> 0));
                    var jsx$14 = x["toString"](16);
                    var jsx$13 = ScalaJS.as.T(jsx$14)
                  };
                  this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(jsx$13, "", false, flags, width, conversion);
                  break
                };
              case 115:
                /*<skip>*/;
              case 83:
                {
                  matchEnd6: {
                    if ((arg === null)) {
                      if ((!this.hasFlag$1__p1__T__T__Z("#", flags))) {
                        this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable("null", "", false, flags, width, conversion);
                        break matchEnd6
                      }
                    };
                    if (ScalaJS.is.ju_Formattable(arg)) {
                      var x3$2 = ScalaJS.as.ju_Formattable(arg);
                      var flags$2 = (((this.hasFlag$1__p1__T__T__Z("-", flags) ? 1 : 0) | (this.hasFlag$1__p1__T__T__Z("#", flags) ? 4 : 0)) | (ScalaJS.m.jl_Character$().isUpperCase__C__Z(conversion) ? 2 : 0));
                      x3$2.formatTo__ju_Formatter__I__I__I__V(this, flags$2, (hasWidth ? width : (-1)), (hasPrecision ? precision : (-1)));
                      ScalaJS.m.s_None$();
                      break matchEnd6
                    };
                    if ((arg !== null)) {
                      if ((!this.hasFlag$1__p1__T__T__Z("#", flags))) {
                        this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(ScalaJS.objectToString(arg), "", false, flags, width, conversion);
                        break matchEnd6
                      }
                    };
                    throw new ScalaJS.c.ju_FormatFlagsConversionMismatchException().init___T__C("#", 115)
                  };
                  break
                };
              case 99:
                /*<skip>*/;
              case 67:
                {
                  var c = (65535 & this.intArg$1__p1__O__I(arg));
                  this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c)), "", false, flags, width, conversion);
                  break
                };
              case 100:
                {
                  var this$67 = this.numberArg$1__p1__O__D(arg);
                  this.with$und$plus$1__p1__T__Z__T__I__C__jl_Appendable(("" + this$67), false, flags, width, conversion);
                  break
                };
              case 111:
                {
                  if (ScalaJS.isInt(arg)) {
                    var x2 = ScalaJS.uI(arg);
                    var x$1 = ScalaJS.uD((x2 >>> 0));
                    var jsx$15 = x$1["toString"](8);
                    var str = ScalaJS.as.T(jsx$15)
                  } else if (ScalaJS.is.sjsr_RuntimeLong(arg)) {
                    var x3$3 = ScalaJS.uJ(arg);
                    var str = ScalaJS.as.sjsr_RuntimeLong(x3$3).toOctalString__T()
                  } else {
                    var str;
                    throw new ScalaJS.c.s_MatchError().init___O(arg)
                  };
                  this.padCaptureSign$1__p1__T__T__T__I__C__jl_Appendable(str, (this.hasFlag$1__p1__T__T__Z("#", flags) ? "0" : ""), flags, width, conversion);
                  break
                };
              case 120:
                /*<skip>*/;
              case 88:
                {
                  if (ScalaJS.isInt(arg)) {
                    var x2$2 = ScalaJS.uI(arg);
                    var x$2 = ScalaJS.uD((x2$2 >>> 0));
                    var jsx$16 = x$2["toString"](16);
                    var str$2 = ScalaJS.as.T(jsx$16)
                  } else if (ScalaJS.is.sjsr_RuntimeLong(arg)) {
                    var x3$4 = ScalaJS.uJ(arg);
                    var str$2 = ScalaJS.as.sjsr_RuntimeLong(x3$4).toHexString__T()
                  } else {
                    var str$2;
                    throw new ScalaJS.c.s_MatchError().init___O(arg)
                  };
                  this.padCaptureSign$1__p1__T__T__T__I__C__jl_Appendable(str$2, (this.hasFlag$1__p1__T__T__Z("#", flags) ? "0x" : ""), flags, width, conversion);
                  break
                };
              case 101:
                /*<skip>*/;
              case 69:
                {
                  this.sciNotation$1__p1__I__T__O__I__C__jl_Appendable((hasPrecision ? precision : 6), flags, arg, width, conversion);
                  break
                };
              case 103:
                /*<skip>*/;
              case 71:
                {
                  var a = this.numberArg$1__p1__O__D(arg);
                  var m = ((a < 0) ? (-a) : a);
                  var p = ((!hasPrecision) ? 6 : ((precision === 0) ? 1 : precision));
                  if (((m >= 1.0E-4) && (m < ScalaJS.uD(ScalaJS.g["Math"]["pow"](10.0, p))))) {
                    var a$1 = (ScalaJS.uD(ScalaJS.g["Math"]["log"](m)) / 2.302585092994046);
                    var sig = (ScalaJS.uD(ScalaJS.g["Math"]["ceil"](a$1)) | 0);
                    var x$3 = this.numberArg$1__p1__O__D(arg);
                    var a$2 = ((p - sig) | 0);
                    var jsx$17 = x$3["toFixed"](((a$2 > 0) ? a$2 : 0));
                    this.with$und$plus$1__p1__T__Z__T__I__C__jl_Appendable(ScalaJS.as.T(jsx$17), false, flags, width, conversion)
                  } else {
                    this.sciNotation$1__p1__I__T__O__I__C__jl_Appendable((((-1) + p) | 0), flags, arg, width, conversion)
                  };
                  break
                };
              case 102:
                {
                  var x$4 = this.numberArg$1__p1__O__D(arg);
                  var jsx$20 = x$4["toFixed"]((hasPrecision ? precision : 6));
                  var jsx$19 = ScalaJS.as.T(jsx$20);
                  var this$86 = this.numberArg$1__p1__O__D(arg);
                  if ((this$86 !== this$86)) {
                    var jsx$18 = true
                  } else {
                    var this$90 = this.numberArg$1__p1__O__D(arg);
                    var jsx$18 = ((this$90 === Infinity) || (this$90 === (-Infinity)))
                  };
                  this.with$und$plus$1__p1__T__Z__T__I__C__jl_Appendable(jsx$19, jsx$18, flags, width, conversion);
                  break
                };
              default:
                throw new ScalaJS.c.s_MatchError().init___O(new ScalaJS.c.jl_Character().init___C(conversion));
            };
            break matchEnd9
          };
          throw new ScalaJS.c.s_MatchError().init___O(x1)
        }
      } else {
        break
      }
    };
    return this
  }
});
ScalaJS.c.ju_Formatter.prototype.strRepeat$1__p1__T__I__T = (function(s, times) {
  var result = "";
  var i = times;
  while ((i > 0)) {
    result = (("" + result) + s);
    i = (((-1) + i) | 0)
  };
  return result
});
ScalaJS.c.ju_Formatter.prototype.sciNotation$1__p1__I__T__O__I__C__jl_Appendable = (function(precision, flags$1, arg$1, width$1, conversion$1) {
  var x = this.numberArg$1__p1__O__D(arg$1);
  var jsx$1 = x["toExponential"](precision);
  var exp = ScalaJS.as.T(jsx$1);
  var index = (((-3) + ScalaJS.uI(exp["length"])) | 0);
  if (((65535 & ScalaJS.uI(exp["charCodeAt"](index))) === 101)) {
    var endIndex = (((-1) + ScalaJS.uI(exp["length"])) | 0);
    var jsx$4 = ScalaJS.as.T(exp["substring"](0, endIndex));
    var index$1 = (((-1) + ScalaJS.uI(exp["length"])) | 0);
    var c = (65535 & ScalaJS.uI(exp["charCodeAt"](index$1)));
    var jsx$3 = ((jsx$4 + "0") + new ScalaJS.c.jl_Character().init___C(c))
  } else {
    var jsx$3 = exp
  };
  var this$13 = this.numberArg$1__p1__O__D(arg$1);
  if ((this$13 !== this$13)) {
    var jsx$2 = true
  } else {
    var this$17 = this.numberArg$1__p1__O__D(arg$1);
    var jsx$2 = ((this$17 === Infinity) || (this$17 === (-Infinity)))
  };
  return this.with$und$plus$1__p1__T__Z__T__I__C__jl_Appendable(jsx$3, jsx$2, flags$1, width$1, conversion$1)
});
ScalaJS.c.ju_Formatter.prototype.intArg$1__p1__O__I = (function(arg$1) {
  if (ScalaJS.isInt(arg$1)) {
    var x2 = ScalaJS.uI(arg$1);
    return x2
  } else if (ScalaJS.is.jl_Character(arg$1)) {
    if ((arg$1 === null)) {
      var x3 = 0
    } else {
      var this$2 = ScalaJS.as.jl_Character(arg$1);
      var x3 = this$2.value$1
    };
    return x3
  } else {
    throw new ScalaJS.c.s_MatchError().init___O(arg$1)
  }
});
ScalaJS.c.ju_Formatter.prototype.java$util$Formatter$$throwClosedException__sr_Nothing$ = (function() {
  throw new ScalaJS.c.ju_FormatterClosedException().init___()
});
ScalaJS.c.ju_Formatter.prototype.close__V = (function() {
  if ((!this.closed$1)) {
    var x1 = this.dest$1;
    if (ScalaJS.is.Ljava_io_Closeable(x1)) {
      ScalaJS.as.Ljava_io_Closeable(x1).close__V()
    }
  };
  this.closed$1 = true
});
ScalaJS.c.ju_Formatter.prototype.with$und$plus$1__p1__T__Z__T__I__C__jl_Appendable = (function(s, preventZero, flags$1, width$1, conversion$1) {
  return (((65535 & ScalaJS.uI(s["charCodeAt"](0))) !== 45) ? (this.hasFlag$1__p1__T__T__Z("+", flags$1) ? this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(s, "+", preventZero, flags$1, width$1, conversion$1) : (this.hasFlag$1__p1__T__T__Z(" ", flags$1) ? this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(s, " ", preventZero, flags$1, width$1, conversion$1) : this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(s, "", preventZero, flags$1, width$1, conversion$1))) : (this.hasFlag$1__p1__T__T__Z("(", flags$1) ? this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable((ScalaJS.as.T(s["substring"](1)) + ")"), "(", preventZero, flags$1, width$1, conversion$1) : this.pad$1__p1__T__T__jl_Boolean__T__I__C__jl_Appendable(ScalaJS.as.T(s["substring"](1)), "-", preventZero, flags$1, width$1, conversion$1)))
});
ScalaJS.c.ju_Formatter.prototype.numberArg$1__p1__O__D = (function(arg$1) {
  if (ScalaJS.is.jl_Number(arg$1)) {
    var x2 = ScalaJS.as.jl_Number(arg$1);
    return ScalaJS.numberDoubleValue(x2)
  } else if (ScalaJS.is.jl_Character(arg$1)) {
    if ((arg$1 === null)) {
      var x3 = 0
    } else {
      var this$2 = ScalaJS.as.jl_Character(arg$1);
      var x3 = this$2.value$1
    };
    return x3
  } else {
    throw new ScalaJS.c.s_MatchError().init___O(arg$1)
  }
});
ScalaJS.d.ju_Formatter = new ScalaJS.ClassTypeData({
  ju_Formatter: 0
}, false, "java.util.Formatter", {
  ju_Formatter: 1,
  O: 1,
  Ljava_io_Closeable: 1,
  Ljava_io_Flushable: 1
});
ScalaJS.c.ju_Formatter.prototype.$classData = ScalaJS.d.ju_Formatter;
/** @constructor */
ScalaJS.c.ju_Random$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.ju_Random$.prototype = new ScalaJS.h.O();
ScalaJS.c.ju_Random$.prototype.constructor = ScalaJS.c.ju_Random$;
/** @constructor */
ScalaJS.h.ju_Random$ = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_Random$.prototype = ScalaJS.c.ju_Random$.prototype;
ScalaJS.c.ju_Random$.prototype.java$util$Random$$randomSeed__J = (function() {
  return new ScalaJS.c.sjsr_RuntimeLong().init___I(this.randomInt__p1__I()).$$less$less__I__sjsr_RuntimeLong(32).$$bar__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 1023, 0).$$amp__sjsr_RuntimeLong__sjsr_RuntimeLong(new ScalaJS.c.sjsr_RuntimeLong().init___I(this.randomInt__p1__I())))
});
ScalaJS.c.ju_Random$.prototype.randomInt__p1__I = (function() {
  var a = (4.294967296E9 * ScalaJS.uD(ScalaJS.g["Math"]["random"]()));
  return (((-2.147483648E9) + ScalaJS.uD(ScalaJS.g["Math"]["floor"](a))) | 0)
});
ScalaJS.d.ju_Random$ = new ScalaJS.ClassTypeData({
  ju_Random$: 0
}, false, "java.util.Random$", {
  ju_Random$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.ju_Random$.prototype.$classData = ScalaJS.d.ju_Random$;
ScalaJS.n.ju_Random$ = (void 0);
ScalaJS.m.ju_Random$ = (function() {
  if ((!ScalaJS.n.ju_Random$)) {
    ScalaJS.n.ju_Random$ = new ScalaJS.c.ju_Random$().init___()
  };
  return ScalaJS.n.ju_Random$
});
/** @constructor */
ScalaJS.c.s_Console$ = (function() {
  ScalaJS.c.s_DeprecatedConsole.call(this);
  this.outVar$2 = null;
  this.errVar$2 = null;
  this.inVar$2 = null
});
ScalaJS.c.s_Console$.prototype = new ScalaJS.h.s_DeprecatedConsole();
ScalaJS.c.s_Console$.prototype.constructor = ScalaJS.c.s_Console$;
/** @constructor */
ScalaJS.h.s_Console$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Console$.prototype = ScalaJS.c.s_Console$.prototype;
ScalaJS.c.s_Console$.prototype.init___ = (function() {
  ScalaJS.n.s_Console$ = this;
  this.outVar$2 = new ScalaJS.c.s_util_DynamicVariable().init___O(ScalaJS.m.jl_System$().out$1);
  this.errVar$2 = new ScalaJS.c.s_util_DynamicVariable().init___O(ScalaJS.m.jl_System$().err$1);
  this.inVar$2 = new ScalaJS.c.s_util_DynamicVariable().init___O(null);
  return this
});
ScalaJS.d.s_Console$ = new ScalaJS.ClassTypeData({
  s_Console$: 0
}, false, "scala.Console$", {
  s_Console$: 1,
  s_DeprecatedConsole: 1,
  O: 1,
  s_io_AnsiColor: 1
});
ScalaJS.c.s_Console$.prototype.$classData = ScalaJS.d.s_Console$;
ScalaJS.n.s_Console$ = (void 0);
ScalaJS.m.s_Console$ = (function() {
  if ((!ScalaJS.n.s_Console$)) {
    ScalaJS.n.s_Console$ = new ScalaJS.c.s_Console$().init___()
  };
  return ScalaJS.n.s_Console$
});
/** @constructor */
ScalaJS.c.s_Option$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_Option$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_Option$.prototype.constructor = ScalaJS.c.s_Option$;
/** @constructor */
ScalaJS.h.s_Option$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Option$.prototype = ScalaJS.c.s_Option$.prototype;
ScalaJS.c.s_Option$.prototype.apply__O__s_Option = (function(x) {
  return ((x === null) ? ScalaJS.m.s_None$() : new ScalaJS.c.s_Some().init___O(x))
});
ScalaJS.d.s_Option$ = new ScalaJS.ClassTypeData({
  s_Option$: 0
}, false, "scala.Option$", {
  s_Option$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_Option$.prototype.$classData = ScalaJS.d.s_Option$;
ScalaJS.n.s_Option$ = (void 0);
ScalaJS.m.s_Option$ = (function() {
  if ((!ScalaJS.n.s_Option$)) {
    ScalaJS.n.s_Option$ = new ScalaJS.c.s_Option$().init___()
  };
  return ScalaJS.n.s_Option$
});
/** @constructor */
ScalaJS.c.s_PartialFunction$$anon$1 = (function() {
  ScalaJS.c.O.call(this);
  this.lift$1 = null
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype = new ScalaJS.h.O();
ScalaJS.c.s_PartialFunction$$anon$1.prototype.constructor = ScalaJS.c.s_PartialFunction$$anon$1;
/** @constructor */
ScalaJS.h.s_PartialFunction$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_PartialFunction$$anon$1.prototype = ScalaJS.c.s_PartialFunction$$anon$1.prototype;
ScalaJS.c.s_PartialFunction$$anon$1.prototype.init___ = (function() {
  this.lift$1 = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2) {
    return (function(x$2) {
      return ScalaJS.m.s_None$()
    })
  })(this));
  return this
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype.apply__O__O = (function(v1) {
  this.apply__O__sr_Nothing$(v1)
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype.runWith__F1__F1 = (function(action) {
  return ScalaJS.m.s_PartialFunction$().scala$PartialFunction$$constFalse$f
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype.toString__T = (function() {
  return "<function1>"
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype.isDefinedAt__O__Z = (function(x) {
  return false
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype.applyOrElse__O__F1__O = (function(x, default$2) {
  return ScalaJS.s.s_PartialFunction$class__applyOrElse__s_PartialFunction__O__F1__O(this, x, default$2)
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype.apply__O__sr_Nothing$ = (function(x) {
  throw new ScalaJS.c.s_MatchError().init___O(x)
});
ScalaJS.d.s_PartialFunction$$anon$1 = new ScalaJS.ClassTypeData({
  s_PartialFunction$$anon$1: 0
}, false, "scala.PartialFunction$$anon$1", {
  s_PartialFunction$$anon$1: 1,
  O: 1,
  s_PartialFunction: 1,
  F1: 1
});
ScalaJS.c.s_PartialFunction$$anon$1.prototype.$classData = ScalaJS.d.s_PartialFunction$$anon$1;
/** @constructor */
ScalaJS.c.s_Predef$ = (function() {
  ScalaJS.c.s_LowPriorityImplicits.call(this);
  this.Map$2 = null;
  this.Set$2 = null;
  this.ClassManifest$2 = null;
  this.Manifest$2 = null;
  this.NoManifest$2 = null;
  this.StringCanBuildFrom$2 = null;
  this.singleton$und$less$colon$less$2 = null;
  this.scala$Predef$$singleton$und$eq$colon$eq$f = null
});
ScalaJS.c.s_Predef$.prototype = new ScalaJS.h.s_LowPriorityImplicits();
ScalaJS.c.s_Predef$.prototype.constructor = ScalaJS.c.s_Predef$;
/** @constructor */
ScalaJS.h.s_Predef$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Predef$.prototype = ScalaJS.c.s_Predef$.prototype;
ScalaJS.c.s_Predef$.prototype.assert__Z__V = (function(assertion) {
  if ((!assertion)) {
    throw new ScalaJS.c.jl_AssertionError().init___O("assertion failed")
  }
});
ScalaJS.c.s_Predef$.prototype.init___ = (function() {
  ScalaJS.n.s_Predef$ = this;
  ScalaJS.m.s_package$();
  ScalaJS.m.sci_List$();
  this.Map$2 = ScalaJS.m.sci_Map$();
  this.Set$2 = ScalaJS.m.sci_Set$();
  this.ClassManifest$2 = ScalaJS.m.s_reflect_package$().ClassManifest$1;
  this.Manifest$2 = ScalaJS.m.s_reflect_package$().Manifest$1;
  this.NoManifest$2 = ScalaJS.m.s_reflect_NoManifest$();
  this.StringCanBuildFrom$2 = new ScalaJS.c.s_Predef$$anon$3().init___();
  this.singleton$und$less$colon$less$2 = new ScalaJS.c.s_Predef$$anon$1().init___();
  this.scala$Predef$$singleton$und$eq$colon$eq$f = new ScalaJS.c.s_Predef$$anon$2().init___();
  return this
});
ScalaJS.c.s_Predef$.prototype.require__Z__V = (function(requirement) {
  if ((!requirement)) {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___T("requirement failed")
  }
});
ScalaJS.d.s_Predef$ = new ScalaJS.ClassTypeData({
  s_Predef$: 0
}, false, "scala.Predef$", {
  s_Predef$: 1,
  s_LowPriorityImplicits: 1,
  O: 1,
  s_DeprecatedPredef: 1
});
ScalaJS.c.s_Predef$.prototype.$classData = ScalaJS.d.s_Predef$;
ScalaJS.n.s_Predef$ = (void 0);
ScalaJS.m.s_Predef$ = (function() {
  if ((!ScalaJS.n.s_Predef$)) {
    ScalaJS.n.s_Predef$ = new ScalaJS.c.s_Predef$().init___()
  };
  return ScalaJS.n.s_Predef$
});
/** @constructor */
ScalaJS.c.s_StringContext$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_StringContext$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_StringContext$.prototype.constructor = ScalaJS.c.s_StringContext$;
/** @constructor */
ScalaJS.h.s_StringContext$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_StringContext$.prototype = ScalaJS.c.s_StringContext$.prototype;
ScalaJS.c.s_StringContext$.prototype.treatEscapes0__p1__T__Z__T = (function(str, strict) {
  var len = ScalaJS.uI(str["length"]);
  var x1 = ScalaJS.m.sjsr_RuntimeString$().indexOf__T__I__I(str, 92);
  switch (x1) {
    case (-1):
      {
        return str;
        break
      };
    default:
      return this.replace$1__p1__I__T__Z__I__T(x1, str, strict, len);
  }
});
ScalaJS.c.s_StringContext$.prototype.loop$1__p1__I__I__T__Z__I__jl_StringBuilder__T = (function(i, next, str$1, strict$1, len$1, b$1) {
  _loop: while (true) {
    if ((next >= 0)) {
      if ((next > i)) {
        b$1.append__jl_CharSequence__I__I__jl_StringBuilder(str$1, i, next)
      };
      var idx = ((1 + next) | 0);
      if ((idx >= len$1)) {
        throw new ScalaJS.c.s_StringContext$InvalidEscapeException().init___T__I(str$1, next)
      };
      var index = idx;
      var x1 = (65535 & ScalaJS.uI(str$1["charCodeAt"](index)));
      switch (x1) {
        case 98:
          {
            var c = 8;
            break
          };
        case 116:
          {
            var c = 9;
            break
          };
        case 110:
          {
            var c = 10;
            break
          };
        case 102:
          {
            var c = 12;
            break
          };
        case 114:
          {
            var c = 13;
            break
          };
        case 34:
          {
            var c = 34;
            break
          };
        case 39:
          {
            var c = 39;
            break
          };
        case 92:
          {
            var c = 92;
            break
          };
        default:
          if (((x1 >= 48) && (x1 <= 55))) {
            if (strict$1) {
              throw new ScalaJS.c.s_StringContext$InvalidEscapeException().init___T__I(str$1, next)
            };
            var index$1 = idx;
            var leadch = (65535 & ScalaJS.uI(str$1["charCodeAt"](index$1)));
            var oct = (((-48) + leadch) | 0);
            idx = ((1 + idx) | 0);
            if ((idx < len$1)) {
              var index$2 = idx;
              var jsx$2 = ((65535 & ScalaJS.uI(str$1["charCodeAt"](index$2))) >= 48)
            } else {
              var jsx$2 = false
            };
            if (jsx$2) {
              var index$3 = idx;
              var jsx$1 = ((65535 & ScalaJS.uI(str$1["charCodeAt"](index$3))) <= 55)
            } else {
              var jsx$1 = false
            };
            if (jsx$1) {
              var jsx$3 = oct;
              var index$4 = idx;
              oct = (((-48) + ((ScalaJS.imul(8, jsx$3) + (65535 & ScalaJS.uI(str$1["charCodeAt"](index$4)))) | 0)) | 0);
              idx = ((1 + idx) | 0);
              if (((idx < len$1) && (leadch <= 51))) {
                var index$5 = idx;
                var jsx$5 = ((65535 & ScalaJS.uI(str$1["charCodeAt"](index$5))) >= 48)
              } else {
                var jsx$5 = false
              };
              if (jsx$5) {
                var index$6 = idx;
                var jsx$4 = ((65535 & ScalaJS.uI(str$1["charCodeAt"](index$6))) <= 55)
              } else {
                var jsx$4 = false
              };
              if (jsx$4) {
                var jsx$6 = oct;
                var index$7 = idx;
                oct = (((-48) + ((ScalaJS.imul(8, jsx$6) + (65535 & ScalaJS.uI(str$1["charCodeAt"](index$7)))) | 0)) | 0);
                idx = ((1 + idx) | 0)
              }
            };
            idx = (((-1) + idx) | 0);
            var c = (65535 & oct)
          } else {
            var c;
            throw new ScalaJS.c.s_StringContext$InvalidEscapeException().init___T__I(str$1, next)
          };
      };
      idx = ((1 + idx) | 0);
      b$1.append__C__jl_StringBuilder(c);
      var temp$i = idx;
      var temp$next = ScalaJS.m.sjsr_RuntimeString$().indexOf__T__I__I__I(str$1, 92, idx);
      i = temp$i;
      next = temp$next;
      continue _loop
    } else {
      if ((i < len$1)) {
        b$1.append__jl_CharSequence__I__I__jl_StringBuilder(str$1, i, len$1)
      };
      return b$1.content$1
    }
  }
});
ScalaJS.c.s_StringContext$.prototype.replace$1__p1__I__T__Z__I__T = (function(first, str$1, strict$1, len$1) {
  var b = new ScalaJS.c.jl_StringBuilder().init___();
  return this.loop$1__p1__I__I__T__Z__I__jl_StringBuilder__T(0, first, str$1, strict$1, len$1, b)
});
ScalaJS.d.s_StringContext$ = new ScalaJS.ClassTypeData({
  s_StringContext$: 0
}, false, "scala.StringContext$", {
  s_StringContext$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_StringContext$.prototype.$classData = ScalaJS.d.s_StringContext$;
ScalaJS.n.s_StringContext$ = (void 0);
ScalaJS.m.s_StringContext$ = (function() {
  if ((!ScalaJS.n.s_StringContext$)) {
    ScalaJS.n.s_StringContext$ = new ScalaJS.c.s_StringContext$().init___()
  };
  return ScalaJS.n.s_StringContext$
});
/** @constructor */
ScalaJS.c.s_math_Fractional$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_math_Fractional$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_math_Fractional$.prototype.constructor = ScalaJS.c.s_math_Fractional$;
/** @constructor */
ScalaJS.h.s_math_Fractional$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_math_Fractional$.prototype = ScalaJS.c.s_math_Fractional$.prototype;
ScalaJS.d.s_math_Fractional$ = new ScalaJS.ClassTypeData({
  s_math_Fractional$: 0
}, false, "scala.math.Fractional$", {
  s_math_Fractional$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_math_Fractional$.prototype.$classData = ScalaJS.d.s_math_Fractional$;
ScalaJS.n.s_math_Fractional$ = (void 0);
ScalaJS.m.s_math_Fractional$ = (function() {
  if ((!ScalaJS.n.s_math_Fractional$)) {
    ScalaJS.n.s_math_Fractional$ = new ScalaJS.c.s_math_Fractional$().init___()
  };
  return ScalaJS.n.s_math_Fractional$
});
/** @constructor */
ScalaJS.c.s_math_Integral$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_math_Integral$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_math_Integral$.prototype.constructor = ScalaJS.c.s_math_Integral$;
/** @constructor */
ScalaJS.h.s_math_Integral$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_math_Integral$.prototype = ScalaJS.c.s_math_Integral$.prototype;
ScalaJS.d.s_math_Integral$ = new ScalaJS.ClassTypeData({
  s_math_Integral$: 0
}, false, "scala.math.Integral$", {
  s_math_Integral$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_math_Integral$.prototype.$classData = ScalaJS.d.s_math_Integral$;
ScalaJS.n.s_math_Integral$ = (void 0);
ScalaJS.m.s_math_Integral$ = (function() {
  if ((!ScalaJS.n.s_math_Integral$)) {
    ScalaJS.n.s_math_Integral$ = new ScalaJS.c.s_math_Integral$().init___()
  };
  return ScalaJS.n.s_math_Integral$
});
/** @constructor */
ScalaJS.c.s_math_Numeric$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_math_Numeric$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_math_Numeric$.prototype.constructor = ScalaJS.c.s_math_Numeric$;
/** @constructor */
ScalaJS.h.s_math_Numeric$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_math_Numeric$.prototype = ScalaJS.c.s_math_Numeric$.prototype;
ScalaJS.d.s_math_Numeric$ = new ScalaJS.ClassTypeData({
  s_math_Numeric$: 0
}, false, "scala.math.Numeric$", {
  s_math_Numeric$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_math_Numeric$.prototype.$classData = ScalaJS.d.s_math_Numeric$;
ScalaJS.n.s_math_Numeric$ = (void 0);
ScalaJS.m.s_math_Numeric$ = (function() {
  if ((!ScalaJS.n.s_math_Numeric$)) {
    ScalaJS.n.s_math_Numeric$ = new ScalaJS.c.s_math_Numeric$().init___()
  };
  return ScalaJS.n.s_math_Numeric$
});
/** @constructor */
ScalaJS.c.s_reflect_ClassTag$ = (function() {
  ScalaJS.c.O.call(this);
  this.ObjectTYPE$1 = null;
  this.NothingTYPE$1 = null;
  this.NullTYPE$1 = null;
  this.Byte$1 = null;
  this.Short$1 = null;
  this.Char$1 = null;
  this.Int$1 = null;
  this.Long$1 = null;
  this.Float$1 = null;
  this.Double$1 = null;
  this.Boolean$1 = null;
  this.Unit$1 = null;
  this.Any$1 = null;
  this.Object$1 = null;
  this.AnyVal$1 = null;
  this.AnyRef$1 = null;
  this.Nothing$1 = null;
  this.Null$1 = null
});
ScalaJS.c.s_reflect_ClassTag$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_reflect_ClassTag$.prototype.constructor = ScalaJS.c.s_reflect_ClassTag$;
/** @constructor */
ScalaJS.h.s_reflect_ClassTag$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ClassTag$.prototype = ScalaJS.c.s_reflect_ClassTag$.prototype;
ScalaJS.c.s_reflect_ClassTag$.prototype.init___ = (function() {
  ScalaJS.n.s_reflect_ClassTag$ = this;
  this.ObjectTYPE$1 = ScalaJS.d.O.getClassOf();
  this.NothingTYPE$1 = ScalaJS.d.sr_Nothing$.getClassOf();
  this.NullTYPE$1 = ScalaJS.d.sr_Null$.getClassOf();
  this.Byte$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Byte$1;
  this.Short$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Short$1;
  this.Char$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Char$1;
  this.Int$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Int$1;
  this.Long$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Long$1;
  this.Float$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Float$1;
  this.Double$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Double$1;
  this.Boolean$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Boolean$1;
  this.Unit$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Unit$1;
  this.Any$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Any$1;
  this.Object$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Object$1;
  this.AnyVal$1 = ScalaJS.m.s_reflect_package$().Manifest$1.AnyVal$1;
  this.AnyRef$1 = ScalaJS.m.s_reflect_package$().Manifest$1.AnyRef$1;
  this.Nothing$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Nothing$1;
  this.Null$1 = ScalaJS.m.s_reflect_package$().Manifest$1.Null$1;
  return this
});
ScalaJS.d.s_reflect_ClassTag$ = new ScalaJS.ClassTypeData({
  s_reflect_ClassTag$: 0
}, false, "scala.reflect.ClassTag$", {
  s_reflect_ClassTag$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_reflect_ClassTag$.prototype.$classData = ScalaJS.d.s_reflect_ClassTag$;
ScalaJS.n.s_reflect_ClassTag$ = (void 0);
ScalaJS.m.s_reflect_ClassTag$ = (function() {
  if ((!ScalaJS.n.s_reflect_ClassTag$)) {
    ScalaJS.n.s_reflect_ClassTag$ = new ScalaJS.c.s_reflect_ClassTag$().init___()
  };
  return ScalaJS.n.s_reflect_ClassTag$
});
/** @constructor */
ScalaJS.c.s_util_DynamicVariable$$anon$1 = (function() {
  ScalaJS.c.jl_InheritableThreadLocal.call(this);
  this.$$outer$3 = null
});
ScalaJS.c.s_util_DynamicVariable$$anon$1.prototype = new ScalaJS.h.jl_InheritableThreadLocal();
ScalaJS.c.s_util_DynamicVariable$$anon$1.prototype.constructor = ScalaJS.c.s_util_DynamicVariable$$anon$1;
/** @constructor */
ScalaJS.h.s_util_DynamicVariable$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_DynamicVariable$$anon$1.prototype = ScalaJS.c.s_util_DynamicVariable$$anon$1.prototype;
ScalaJS.c.s_util_DynamicVariable$$anon$1.prototype.init___s_util_DynamicVariable = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$3 = $$outer
  };
  ScalaJS.c.jl_InheritableThreadLocal.prototype.init___.call(this);
  return this
});
ScalaJS.c.s_util_DynamicVariable$$anon$1.prototype.initialValue__O = (function() {
  return this.$$outer$3.scala$util$DynamicVariable$$init$f
});
ScalaJS.d.s_util_DynamicVariable$$anon$1 = new ScalaJS.ClassTypeData({
  s_util_DynamicVariable$$anon$1: 0
}, false, "scala.util.DynamicVariable$$anon$1", {
  s_util_DynamicVariable$$anon$1: 1,
  jl_InheritableThreadLocal: 1,
  jl_ThreadLocal: 1,
  O: 1
});
ScalaJS.c.s_util_DynamicVariable$$anon$1.prototype.$classData = ScalaJS.d.s_util_DynamicVariable$$anon$1;
/** @constructor */
ScalaJS.c.s_util_Left$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_util_Left$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_Left$.prototype.constructor = ScalaJS.c.s_util_Left$;
/** @constructor */
ScalaJS.h.s_util_Left$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_Left$.prototype = ScalaJS.c.s_util_Left$.prototype;
ScalaJS.c.s_util_Left$.prototype.toString__T = (function() {
  return "Left"
});
ScalaJS.d.s_util_Left$ = new ScalaJS.ClassTypeData({
  s_util_Left$: 0
}, false, "scala.util.Left$", {
  s_util_Left$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_util_Left$.prototype.$classData = ScalaJS.d.s_util_Left$;
ScalaJS.n.s_util_Left$ = (void 0);
ScalaJS.m.s_util_Left$ = (function() {
  if ((!ScalaJS.n.s_util_Left$)) {
    ScalaJS.n.s_util_Left$ = new ScalaJS.c.s_util_Left$().init___()
  };
  return ScalaJS.n.s_util_Left$
});
/** @constructor */
ScalaJS.c.s_util_Random = (function() {
  ScalaJS.c.O.call(this);
  this.self$1 = null
});
ScalaJS.c.s_util_Random.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_Random.prototype.constructor = ScalaJS.c.s_util_Random;
/** @constructor */
ScalaJS.h.s_util_Random = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_Random.prototype = ScalaJS.c.s_util_Random.prototype;
ScalaJS.c.s_util_Random.prototype.init___ = (function() {
  ScalaJS.c.s_util_Random.prototype.init___ju_Random.call(this, new ScalaJS.c.ju_Random().init___());
  return this
});
ScalaJS.c.s_util_Random.prototype.init___ju_Random = (function(self) {
  this.self$1 = self;
  return this
});
ScalaJS.d.s_util_Random = new ScalaJS.ClassTypeData({
  s_util_Random: 0
}, false, "scala.util.Random", {
  s_util_Random: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_util_Random.prototype.$classData = ScalaJS.d.s_util_Random;
/** @constructor */
ScalaJS.c.s_util_Right$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_util_Right$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_Right$.prototype.constructor = ScalaJS.c.s_util_Right$;
/** @constructor */
ScalaJS.h.s_util_Right$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_Right$.prototype = ScalaJS.c.s_util_Right$.prototype;
ScalaJS.c.s_util_Right$.prototype.toString__T = (function() {
  return "Right"
});
ScalaJS.d.s_util_Right$ = new ScalaJS.ClassTypeData({
  s_util_Right$: 0
}, false, "scala.util.Right$", {
  s_util_Right$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_util_Right$.prototype.$classData = ScalaJS.d.s_util_Right$;
ScalaJS.n.s_util_Right$ = (void 0);
ScalaJS.m.s_util_Right$ = (function() {
  if ((!ScalaJS.n.s_util_Right$)) {
    ScalaJS.n.s_util_Right$ = new ScalaJS.c.s_util_Right$().init___()
  };
  return ScalaJS.n.s_util_Right$
});
/** @constructor */
ScalaJS.c.s_util_control_NoStackTrace$ = (function() {
  ScalaJS.c.O.call(this);
  this.$$undnoSuppression$1 = false
});
ScalaJS.c.s_util_control_NoStackTrace$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_util_control_NoStackTrace$.prototype.constructor = ScalaJS.c.s_util_control_NoStackTrace$;
/** @constructor */
ScalaJS.h.s_util_control_NoStackTrace$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_control_NoStackTrace$.prototype = ScalaJS.c.s_util_control_NoStackTrace$.prototype;
ScalaJS.c.s_util_control_NoStackTrace$.prototype.init___ = (function() {
  ScalaJS.n.s_util_control_NoStackTrace$ = this;
  this.$$undnoSuppression$1 = false;
  return this
});
ScalaJS.d.s_util_control_NoStackTrace$ = new ScalaJS.ClassTypeData({
  s_util_control_NoStackTrace$: 0
}, false, "scala.util.control.NoStackTrace$", {
  s_util_control_NoStackTrace$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_util_control_NoStackTrace$.prototype.$classData = ScalaJS.d.s_util_control_NoStackTrace$;
ScalaJS.n.s_util_control_NoStackTrace$ = (void 0);
ScalaJS.m.s_util_control_NoStackTrace$ = (function() {
  if ((!ScalaJS.n.s_util_control_NoStackTrace$)) {
    ScalaJS.n.s_util_control_NoStackTrace$ = new ScalaJS.c.s_util_control_NoStackTrace$().init___()
  };
  return ScalaJS.n.s_util_control_NoStackTrace$
});
/** @constructor */
ScalaJS.c.sc_IndexedSeq$$anon$1 = (function() {
  ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.call(this)
});
ScalaJS.c.sc_IndexedSeq$$anon$1.prototype = new ScalaJS.h.scg_GenTraversableFactory$GenericCanBuildFrom();
ScalaJS.c.sc_IndexedSeq$$anon$1.prototype.constructor = ScalaJS.c.sc_IndexedSeq$$anon$1;
/** @constructor */
ScalaJS.h.sc_IndexedSeq$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_IndexedSeq$$anon$1.prototype = ScalaJS.c.sc_IndexedSeq$$anon$1.prototype;
ScalaJS.c.sc_IndexedSeq$$anon$1.prototype.apply__scm_Builder = (function() {
  ScalaJS.m.sc_IndexedSeq$();
  ScalaJS.m.sci_IndexedSeq$();
  ScalaJS.m.sci_Vector$();
  return new ScalaJS.c.sci_VectorBuilder().init___()
});
ScalaJS.c.sc_IndexedSeq$$anon$1.prototype.init___ = (function() {
  ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype.init___scg_GenTraversableFactory.call(this, ScalaJS.m.sc_IndexedSeq$());
  return this
});
ScalaJS.d.sc_IndexedSeq$$anon$1 = new ScalaJS.ClassTypeData({
  sc_IndexedSeq$$anon$1: 0
}, false, "scala.collection.IndexedSeq$$anon$1", {
  sc_IndexedSeq$$anon$1: 1,
  scg_GenTraversableFactory$GenericCanBuildFrom: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
ScalaJS.c.sc_IndexedSeq$$anon$1.prototype.$classData = ScalaJS.d.sc_IndexedSeq$$anon$1;
/** @constructor */
ScalaJS.c.scg_GenSeqFactory = (function() {
  ScalaJS.c.scg_GenTraversableFactory.call(this)
});
ScalaJS.c.scg_GenSeqFactory.prototype = new ScalaJS.h.scg_GenTraversableFactory();
ScalaJS.c.scg_GenSeqFactory.prototype.constructor = ScalaJS.c.scg_GenSeqFactory;
/** @constructor */
ScalaJS.h.scg_GenSeqFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenSeqFactory.prototype = ScalaJS.c.scg_GenSeqFactory.prototype;
/** @constructor */
ScalaJS.c.scg_GenTraversableFactory$$anon$1 = (function() {
  ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.call(this);
  this.$$outer$2 = null
});
ScalaJS.c.scg_GenTraversableFactory$$anon$1.prototype = new ScalaJS.h.scg_GenTraversableFactory$GenericCanBuildFrom();
ScalaJS.c.scg_GenTraversableFactory$$anon$1.prototype.constructor = ScalaJS.c.scg_GenTraversableFactory$$anon$1;
/** @constructor */
ScalaJS.h.scg_GenTraversableFactory$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_GenTraversableFactory$$anon$1.prototype = ScalaJS.c.scg_GenTraversableFactory$$anon$1.prototype;
ScalaJS.c.scg_GenTraversableFactory$$anon$1.prototype.apply__scm_Builder = (function() {
  return this.$$outer$2.newBuilder__scm_Builder()
});
ScalaJS.c.scg_GenTraversableFactory$$anon$1.prototype.init___scg_GenTraversableFactory = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype.init___scg_GenTraversableFactory.call(this, $$outer);
  return this
});
ScalaJS.d.scg_GenTraversableFactory$$anon$1 = new ScalaJS.ClassTypeData({
  scg_GenTraversableFactory$$anon$1: 0
}, false, "scala.collection.generic.GenTraversableFactory$$anon$1", {
  scg_GenTraversableFactory$$anon$1: 1,
  scg_GenTraversableFactory$GenericCanBuildFrom: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
ScalaJS.c.scg_GenTraversableFactory$$anon$1.prototype.$classData = ScalaJS.d.scg_GenTraversableFactory$$anon$1;
/** @constructor */
ScalaJS.c.scg_ImmutableMapFactory = (function() {
  ScalaJS.c.scg_MapFactory.call(this)
});
ScalaJS.c.scg_ImmutableMapFactory.prototype = new ScalaJS.h.scg_MapFactory();
ScalaJS.c.scg_ImmutableMapFactory.prototype.constructor = ScalaJS.c.scg_ImmutableMapFactory;
/** @constructor */
ScalaJS.h.scg_ImmutableMapFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_ImmutableMapFactory.prototype = ScalaJS.c.scg_ImmutableMapFactory.prototype;
/** @constructor */
ScalaJS.c.scg_MutableMapFactory = (function() {
  ScalaJS.c.scg_MapFactory.call(this)
});
ScalaJS.c.scg_MutableMapFactory.prototype = new ScalaJS.h.scg_MapFactory();
ScalaJS.c.scg_MutableMapFactory.prototype.constructor = ScalaJS.c.scg_MutableMapFactory;
/** @constructor */
ScalaJS.h.scg_MutableMapFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_MutableMapFactory.prototype = ScalaJS.c.scg_MutableMapFactory.prototype;
ScalaJS.c.scg_MutableMapFactory.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_HashMap().init___()
});
/** @constructor */
ScalaJS.c.sci_$colon$colon$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sci_$colon$colon$.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_$colon$colon$.prototype.constructor = ScalaJS.c.sci_$colon$colon$;
/** @constructor */
ScalaJS.h.sci_$colon$colon$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_$colon$colon$.prototype = ScalaJS.c.sci_$colon$colon$.prototype;
ScalaJS.c.sci_$colon$colon$.prototype.toString__T = (function() {
  return "::"
});
ScalaJS.d.sci_$colon$colon$ = new ScalaJS.ClassTypeData({
  sci_$colon$colon$: 0
}, false, "scala.collection.immutable.$colon$colon$", {
  sci_$colon$colon$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_$colon$colon$.prototype.$classData = ScalaJS.d.sci_$colon$colon$;
ScalaJS.n.sci_$colon$colon$ = (void 0);
ScalaJS.m.sci_$colon$colon$ = (function() {
  if ((!ScalaJS.n.sci_$colon$colon$)) {
    ScalaJS.n.sci_$colon$colon$ = new ScalaJS.c.sci_$colon$colon$().init___()
  };
  return ScalaJS.n.sci_$colon$colon$
});
/** @constructor */
ScalaJS.c.sci_Range$ = (function() {
  ScalaJS.c.O.call(this);
  this.MAX$undPRINT$1 = 0
});
ScalaJS.c.sci_Range$.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_Range$.prototype.constructor = ScalaJS.c.sci_Range$;
/** @constructor */
ScalaJS.h.sci_Range$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Range$.prototype = ScalaJS.c.sci_Range$.prototype;
ScalaJS.c.sci_Range$.prototype.init___ = (function() {
  ScalaJS.n.sci_Range$ = this;
  this.MAX$undPRINT$1 = 512;
  return this
});
ScalaJS.c.sci_Range$.prototype.description__p1__I__I__I__Z__T = (function(start, end, step, isInclusive) {
  var this$2 = new ScalaJS.c.sci_StringOps().init___T("%d %s %d by %s");
  var array = [start, (isInclusive ? "to" : "until"), end, step];
  var jsx$4 = ScalaJS.m.sjsr_RuntimeString$();
  var jsx$3 = this$2.repr$1;
  var this$4 = ScalaJS.m.sc_Seq$();
  this$4.ReusableCBFInstance$2;
  ScalaJS.m.sjs_js_WrappedArray$();
  var array$1 = [];
  ScalaJS.uI(array["length"]);
  var i = 0;
  var len = ScalaJS.uI(array["length"]);
  while ((i < len)) {
    var index = i;
    var arg1 = array[index];
    var elem = ScalaJS.s.sci_StringLike$class__unwrapArg__p0__sci_StringLike__O__O(this$2, arg1);
    array$1["push"](elem);
    i = ((1 + i) | 0)
  };
  var evidence$1 = ScalaJS.m.s_reflect_ClassTag$().AnyRef$1;
  var result = evidence$1.newArray__I__O(ScalaJS.uI(array$1["length"]));
  var len$1 = ScalaJS.m.sr_ScalaRunTime$().array$undlength__O__I(result);
  var i$1 = 0;
  var j = 0;
  var $$this$1 = ScalaJS.uI(array$1["length"]);
  var $$this$2 = (($$this$1 < len$1) ? $$this$1 : len$1);
  var that = ScalaJS.m.sr_ScalaRunTime$().array$undlength__O__I(result);
  var end$1 = (($$this$2 < that) ? $$this$2 : that);
  while ((i$1 < end$1)) {
    var jsx$2 = ScalaJS.m.sr_ScalaRunTime$();
    var jsx$1 = j;
    var index$1 = i$1;
    jsx$2.array$undupdate__O__I__O__V(result, jsx$1, array$1[index$1]);
    i$1 = ((1 + i$1) | 0);
    j = ((1 + j) | 0)
  };
  return jsx$4.format__T__AO__T(jsx$3, ScalaJS.asArrayOf.O(result, 1))
});
ScalaJS.c.sci_Range$.prototype.scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$ = (function(start, end, step, isInclusive) {
  throw new ScalaJS.c.jl_IllegalArgumentException().init___T((this.description__p1__I__I__I__Z__T(start, end, step, isInclusive) + ": seqs cannot contain more than Int.MaxValue elements."))
});
ScalaJS.d.sci_Range$ = new ScalaJS.ClassTypeData({
  sci_Range$: 0
}, false, "scala.collection.immutable.Range$", {
  sci_Range$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Range$.prototype.$classData = ScalaJS.d.sci_Range$;
ScalaJS.n.sci_Range$ = (void 0);
ScalaJS.m.sci_Range$ = (function() {
  if ((!ScalaJS.n.sci_Range$)) {
    ScalaJS.n.sci_Range$ = new ScalaJS.c.sci_Range$().init___()
  };
  return ScalaJS.n.sci_Range$
});
/** @constructor */
ScalaJS.c.sci_Stream$StreamCanBuildFrom = (function() {
  ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.call(this)
});
ScalaJS.c.sci_Stream$StreamCanBuildFrom.prototype = new ScalaJS.h.scg_GenTraversableFactory$GenericCanBuildFrom();
ScalaJS.c.sci_Stream$StreamCanBuildFrom.prototype.constructor = ScalaJS.c.sci_Stream$StreamCanBuildFrom;
/** @constructor */
ScalaJS.h.sci_Stream$StreamCanBuildFrom = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream$StreamCanBuildFrom.prototype = ScalaJS.c.sci_Stream$StreamCanBuildFrom.prototype;
ScalaJS.c.sci_Stream$StreamCanBuildFrom.prototype.init___ = (function() {
  ScalaJS.c.scg_GenTraversableFactory$GenericCanBuildFrom.prototype.init___scg_GenTraversableFactory.call(this, ScalaJS.m.sci_Stream$());
  return this
});
ScalaJS.d.sci_Stream$StreamCanBuildFrom = new ScalaJS.ClassTypeData({
  sci_Stream$StreamCanBuildFrom: 0
}, false, "scala.collection.immutable.Stream$StreamCanBuildFrom", {
  sci_Stream$StreamCanBuildFrom: 1,
  scg_GenTraversableFactory$GenericCanBuildFrom: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
ScalaJS.c.sci_Stream$StreamCanBuildFrom.prototype.$classData = ScalaJS.d.sci_Stream$StreamCanBuildFrom;
/** @constructor */
ScalaJS.c.scm_StringBuilder$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.scm_StringBuilder$.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_StringBuilder$.prototype.constructor = ScalaJS.c.scm_StringBuilder$;
/** @constructor */
ScalaJS.h.scm_StringBuilder$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_StringBuilder$.prototype = ScalaJS.c.scm_StringBuilder$.prototype;
ScalaJS.d.scm_StringBuilder$ = new ScalaJS.ClassTypeData({
  scm_StringBuilder$: 0
}, false, "scala.collection.mutable.StringBuilder$", {
  scm_StringBuilder$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_StringBuilder$.prototype.$classData = ScalaJS.d.scm_StringBuilder$;
ScalaJS.n.scm_StringBuilder$ = (void 0);
ScalaJS.m.scm_StringBuilder$ = (function() {
  if ((!ScalaJS.n.scm_StringBuilder$)) {
    ScalaJS.n.scm_StringBuilder$ = new ScalaJS.c.scm_StringBuilder$().init___()
  };
  return ScalaJS.n.scm_StringBuilder$
});
/** @constructor */
ScalaJS.c.sjsr_AnonFunction0 = (function() {
  ScalaJS.c.sr_AbstractFunction0.call(this);
  this.f$2 = null
});
ScalaJS.c.sjsr_AnonFunction0.prototype = new ScalaJS.h.sr_AbstractFunction0();
ScalaJS.c.sjsr_AnonFunction0.prototype.constructor = ScalaJS.c.sjsr_AnonFunction0;
/** @constructor */
ScalaJS.h.sjsr_AnonFunction0 = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_AnonFunction0.prototype = ScalaJS.c.sjsr_AnonFunction0.prototype;
ScalaJS.c.sjsr_AnonFunction0.prototype.apply__O = (function() {
  return (0, this.f$2)()
});
ScalaJS.c.sjsr_AnonFunction0.prototype.init___sjs_js_Function0 = (function(f) {
  this.f$2 = f;
  return this
});
ScalaJS.d.sjsr_AnonFunction0 = new ScalaJS.ClassTypeData({
  sjsr_AnonFunction0: 0
}, false, "scala.scalajs.runtime.AnonFunction0", {
  sjsr_AnonFunction0: 1,
  sr_AbstractFunction0: 1,
  O: 1,
  F0: 1
});
ScalaJS.c.sjsr_AnonFunction0.prototype.$classData = ScalaJS.d.sjsr_AnonFunction0;
/** @constructor */
ScalaJS.c.sjsr_AnonFunction1 = (function() {
  ScalaJS.c.sr_AbstractFunction1.call(this);
  this.f$2 = null
});
ScalaJS.c.sjsr_AnonFunction1.prototype = new ScalaJS.h.sr_AbstractFunction1();
ScalaJS.c.sjsr_AnonFunction1.prototype.constructor = ScalaJS.c.sjsr_AnonFunction1;
/** @constructor */
ScalaJS.h.sjsr_AnonFunction1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_AnonFunction1.prototype = ScalaJS.c.sjsr_AnonFunction1.prototype;
ScalaJS.c.sjsr_AnonFunction1.prototype.apply__O__O = (function(arg1) {
  return (0, this.f$2)(arg1)
});
ScalaJS.c.sjsr_AnonFunction1.prototype.init___sjs_js_Function1 = (function(f) {
  this.f$2 = f;
  return this
});
ScalaJS.d.sjsr_AnonFunction1 = new ScalaJS.ClassTypeData({
  sjsr_AnonFunction1: 0
}, false, "scala.scalajs.runtime.AnonFunction1", {
  sjsr_AnonFunction1: 1,
  sr_AbstractFunction1: 1,
  O: 1,
  F1: 1
});
ScalaJS.c.sjsr_AnonFunction1.prototype.$classData = ScalaJS.d.sjsr_AnonFunction1;
/** @constructor */
ScalaJS.c.sjsr_AnonFunction2 = (function() {
  ScalaJS.c.sr_AbstractFunction2.call(this);
  this.f$2 = null
});
ScalaJS.c.sjsr_AnonFunction2.prototype = new ScalaJS.h.sr_AbstractFunction2();
ScalaJS.c.sjsr_AnonFunction2.prototype.constructor = ScalaJS.c.sjsr_AnonFunction2;
/** @constructor */
ScalaJS.h.sjsr_AnonFunction2 = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_AnonFunction2.prototype = ScalaJS.c.sjsr_AnonFunction2.prototype;
ScalaJS.c.sjsr_AnonFunction2.prototype.init___sjs_js_Function2 = (function(f) {
  this.f$2 = f;
  return this
});
ScalaJS.c.sjsr_AnonFunction2.prototype.apply__O__O__O = (function(arg1, arg2) {
  return (0, this.f$2)(arg1, arg2)
});
ScalaJS.d.sjsr_AnonFunction2 = new ScalaJS.ClassTypeData({
  sjsr_AnonFunction2: 0
}, false, "scala.scalajs.runtime.AnonFunction2", {
  sjsr_AnonFunction2: 1,
  sr_AbstractFunction2: 1,
  O: 1,
  F2: 1
});
ScalaJS.c.sjsr_AnonFunction2.prototype.$classData = ScalaJS.d.sjsr_AnonFunction2;
/** @constructor */
ScalaJS.c.sjsr_RuntimeLong = (function() {
  ScalaJS.c.jl_Number.call(this);
  this.l$2 = 0;
  this.m$2 = 0;
  this.h$2 = 0
});
ScalaJS.c.sjsr_RuntimeLong.prototype = new ScalaJS.h.jl_Number();
ScalaJS.c.sjsr_RuntimeLong.prototype.constructor = ScalaJS.c.sjsr_RuntimeLong;
/** @constructor */
ScalaJS.h.sjsr_RuntimeLong = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_RuntimeLong.prototype = ScalaJS.c.sjsr_RuntimeLong.prototype;
ScalaJS.c.sjsr_RuntimeLong.prototype.longValue__J = (function() {
  return ScalaJS.uJ(this)
});
ScalaJS.c.sjsr_RuntimeLong.prototype.powerOfTwo__p2__I = (function() {
  return (((((this.h$2 === 0) && (this.m$2 === 0)) && (this.l$2 !== 0)) && ((this.l$2 & (((-1) + this.l$2) | 0)) === 0)) ? ScalaJS.m.jl_Integer$().numberOfTrailingZeros__I__I(this.l$2) : (((((this.h$2 === 0) && (this.m$2 !== 0)) && (this.l$2 === 0)) && ((this.m$2 & (((-1) + this.m$2) | 0)) === 0)) ? ((22 + ScalaJS.m.jl_Integer$().numberOfTrailingZeros__I__I(this.m$2)) | 0) : (((((this.h$2 !== 0) && (this.m$2 === 0)) && (this.l$2 === 0)) && ((this.h$2 & (((-1) + this.h$2) | 0)) === 0)) ? ((44 + ScalaJS.m.jl_Integer$().numberOfTrailingZeros__I__I(this.h$2)) | 0) : (-1))))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$bar__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((this.l$2 | y.l$2), (this.m$2 | y.m$2), (this.h$2 | y.h$2))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$greater$eq__sjsr_RuntimeLong__Z = (function(y) {
  return (((524288 & this.h$2) === 0) ? (((((524288 & y.h$2) !== 0) || (this.h$2 > y.h$2)) || ((this.h$2 === y.h$2) && (this.m$2 > y.m$2))) || (((this.h$2 === y.h$2) && (this.m$2 === y.m$2)) && (this.l$2 >= y.l$2))) : (!(((((524288 & y.h$2) === 0) || (this.h$2 < y.h$2)) || ((this.h$2 === y.h$2) && (this.m$2 < y.m$2))) || (((this.h$2 === y.h$2) && (this.m$2 === y.m$2)) && (this.l$2 < y.l$2)))))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.byteValue__B = (function() {
  return this.toByte__B()
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toShort__S = (function() {
  return ((this.toInt__I() << 16) >> 16)
});
ScalaJS.c.sjsr_RuntimeLong.prototype.equals__O__Z = (function(that) {
  if (ScalaJS.is.sjsr_RuntimeLong(that)) {
    var x2 = ScalaJS.as.sjsr_RuntimeLong(that);
    return this.equals__sjsr_RuntimeLong__Z(x2)
  } else {
    return false
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$less__sjsr_RuntimeLong__Z = (function(y) {
  return y.$$greater__sjsr_RuntimeLong__Z(this)
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toHexString__T = (function() {
  var mp = (this.m$2 >> 2);
  var lp = (this.l$2 | ((3 & this.m$2) << 22));
  if ((this.h$2 !== 0)) {
    var i = this.h$2;
    var x = ScalaJS.uD((i >>> 0));
    var jsx$5 = x["toString"](16);
    var jsx$4 = ScalaJS.as.T(jsx$5);
    var x$1 = ScalaJS.uD((mp >>> 0));
    var jsx$2 = x$1["toString"](16);
    var s = ScalaJS.as.T(jsx$2);
    var beginIndex = ((1 + ScalaJS.uI(s["length"])) | 0);
    var jsx$3 = ScalaJS.as.T("000000"["substring"](beginIndex));
    var x$2 = ScalaJS.uD((lp >>> 0));
    var jsx$1 = x$2["toString"](16);
    var s$1 = ScalaJS.as.T(jsx$1);
    var beginIndex$1 = ScalaJS.uI(s$1["length"]);
    return ((jsx$4 + (("" + jsx$3) + s)) + (("" + ScalaJS.as.T("000000"["substring"](beginIndex$1))) + s$1))
  } else if ((mp !== 0)) {
    var x$3 = ScalaJS.uD((mp >>> 0));
    var jsx$8 = x$3["toString"](16);
    var jsx$7 = ScalaJS.as.T(jsx$8);
    var x$4 = ScalaJS.uD((lp >>> 0));
    var jsx$6 = x$4["toString"](16);
    var s$2 = ScalaJS.as.T(jsx$6);
    var beginIndex$2 = ScalaJS.uI(s$2["length"]);
    return (jsx$7 + (("" + ScalaJS.as.T("000000"["substring"](beginIndex$2))) + s$2))
  } else {
    var x$5 = ScalaJS.uD((lp >>> 0));
    var jsx$9 = x$5["toString"](16);
    return ScalaJS.as.T(jsx$9)
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$times__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  var _1 = (8191 & this.l$2);
  var _2 = ((this.l$2 >> 13) | ((15 & this.m$2) << 9));
  var _3 = (8191 & (this.m$2 >> 4));
  var _4 = ((this.m$2 >> 17) | ((255 & this.h$2) << 5));
  var _5 = ((1048320 & this.h$2) >> 8);
  matchEnd3: {
    var x$1;
    var x$1_$_$$und1$1 = _1;
    var x$1_$_$$und2$1 = _2;
    var x$1_$_$$und3$1 = _3;
    var x$1_$_$$und4$1 = _4;
    var x$1_$_$$und5$1 = _5;
    break matchEnd3
  };
  var a0$2 = ScalaJS.uI(x$1_$_$$und1$1);
  var a1$2 = ScalaJS.uI(x$1_$_$$und2$1);
  var a2$2 = ScalaJS.uI(x$1_$_$$und3$1);
  var a3$2 = ScalaJS.uI(x$1_$_$$und4$1);
  var a4$2 = ScalaJS.uI(x$1_$_$$und5$1);
  var _1$1 = (8191 & y.l$2);
  var _2$1 = ((y.l$2 >> 13) | ((15 & y.m$2) << 9));
  var _3$1 = (8191 & (y.m$2 >> 4));
  var _4$1 = ((y.m$2 >> 17) | ((255 & y.h$2) << 5));
  var _5$1 = ((1048320 & y.h$2) >> 8);
  matchEnd3$2: {
    var x$2;
    var x$2_$_$$und1$1 = _1$1;
    var x$2_$_$$und2$1 = _2$1;
    var x$2_$_$$und3$1 = _3$1;
    var x$2_$_$$und4$1 = _4$1;
    var x$2_$_$$und5$1 = _5$1;
    break matchEnd3$2
  };
  var b0$2 = ScalaJS.uI(x$2_$_$$und1$1);
  var b1$2 = ScalaJS.uI(x$2_$_$$und2$1);
  var b2$2 = ScalaJS.uI(x$2_$_$$und3$1);
  var b3$2 = ScalaJS.uI(x$2_$_$$und4$1);
  var b4$2 = ScalaJS.uI(x$2_$_$$und5$1);
  var p0 = ScalaJS.imul(a0$2, b0$2);
  var p1 = ScalaJS.imul(a1$2, b0$2);
  var p2 = ScalaJS.imul(a2$2, b0$2);
  var p3 = ScalaJS.imul(a3$2, b0$2);
  var p4 = ScalaJS.imul(a4$2, b0$2);
  if ((b1$2 !== 0)) {
    p1 = ((p1 + ScalaJS.imul(a0$2, b1$2)) | 0);
    p2 = ((p2 + ScalaJS.imul(a1$2, b1$2)) | 0);
    p3 = ((p3 + ScalaJS.imul(a2$2, b1$2)) | 0);
    p4 = ((p4 + ScalaJS.imul(a3$2, b1$2)) | 0)
  };
  if ((b2$2 !== 0)) {
    p2 = ((p2 + ScalaJS.imul(a0$2, b2$2)) | 0);
    p3 = ((p3 + ScalaJS.imul(a1$2, b2$2)) | 0);
    p4 = ((p4 + ScalaJS.imul(a2$2, b2$2)) | 0)
  };
  if ((b3$2 !== 0)) {
    p3 = ((p3 + ScalaJS.imul(a0$2, b3$2)) | 0);
    p4 = ((p4 + ScalaJS.imul(a1$2, b3$2)) | 0)
  };
  if ((b4$2 !== 0)) {
    p4 = ((p4 + ScalaJS.imul(a0$2, b4$2)) | 0)
  };
  var c00 = (4194303 & p0);
  var c01 = ((511 & p1) << 13);
  var c0 = ((c00 + c01) | 0);
  var c10 = (p0 >> 22);
  var c11 = (p1 >> 9);
  var c12 = ((262143 & p2) << 4);
  var c13 = ((31 & p3) << 17);
  var c1 = ((((((c10 + c11) | 0) + c12) | 0) + c13) | 0);
  var c22 = (p2 >> 18);
  var c23 = (p3 >> 5);
  var c24 = ((4095 & p4) << 8);
  var c2 = ((((c22 + c23) | 0) + c24) | 0);
  var c1n = ((c1 + (c0 >> 22)) | 0);
  var h = ((c2 + (c1n >> 22)) | 0);
  return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & c0), (4194303 & c1n), (1048575 & h))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.init___I__I__I = (function(l, m, h) {
  this.l$2 = l;
  this.m$2 = m;
  this.h$2 = h;
  return this
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$percent__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  return ScalaJS.as.sjsr_RuntimeLong(this.scala$scalajs$runtime$RuntimeLong$$divMod__sjsr_RuntimeLong__sjs_js_Array(y)[1])
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toString__T = (function() {
  if ((((this.l$2 === 0) && (this.m$2 === 0)) && (this.h$2 === 0))) {
    return "0"
  } else if (this.equals__sjsr_RuntimeLong__Z(ScalaJS.m.sjsr_RuntimeLong$().MinValue$1)) {
    return "-9223372036854775808"
  } else if (((524288 & this.h$2) !== 0)) {
    return ("-" + this.unary$und$minus__sjsr_RuntimeLong().toString__T())
  } else {
    var tenPow9 = ScalaJS.m.sjsr_RuntimeLong$().TenPow9$1;
    var v = this;
    var acc = "";
    _toString0: while (true) {
      var this$1 = v;
      if ((((this$1.l$2 === 0) && (this$1.m$2 === 0)) && (this$1.h$2 === 0))) {
        return acc
      } else {
        var quotRem = v.scala$scalajs$runtime$RuntimeLong$$divMod__sjsr_RuntimeLong__sjs_js_Array(tenPow9);
        var quot = ScalaJS.as.sjsr_RuntimeLong(quotRem[0]);
        var rem = ScalaJS.as.sjsr_RuntimeLong(quotRem[1]);
        var this$2 = rem.toInt__I();
        var digits = ("" + this$2);
        if ((((quot.l$2 === 0) && (quot.m$2 === 0)) && (quot.h$2 === 0))) {
          var zeroPrefix = ""
        } else {
          var beginIndex = ScalaJS.uI(digits["length"]);
          var zeroPrefix = ScalaJS.as.T("000000000"["substring"](beginIndex))
        };
        var temp$acc = ((zeroPrefix + digits) + acc);
        v = quot;
        acc = temp$acc;
        continue _toString0
      }
    }
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$less$eq__sjsr_RuntimeLong__Z = (function(y) {
  return y.$$greater$eq__sjsr_RuntimeLong__Z(this)
});
ScalaJS.c.sjsr_RuntimeLong.prototype.compareTo__O__I = (function(x$1) {
  var that = ScalaJS.as.sjsr_RuntimeLong(x$1);
  return this.compareTo__sjsr_RuntimeLong__I(ScalaJS.as.sjsr_RuntimeLong(that))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.scala$scalajs$runtime$RuntimeLong$$setBit__I__sjsr_RuntimeLong = (function(bit) {
  return ((bit < 22) ? new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((this.l$2 | (1 << bit)), this.m$2, this.h$2) : ((bit < 44) ? new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(this.l$2, (this.m$2 | (1 << (((-22) + bit) | 0))), this.h$2) : new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(this.l$2, this.m$2, (this.h$2 | (1 << (((-44) + bit) | 0))))))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.scala$scalajs$runtime$RuntimeLong$$divMod__sjsr_RuntimeLong__sjs_js_Array = (function(y) {
  if ((((y.l$2 === 0) && (y.m$2 === 0)) && (y.h$2 === 0))) {
    throw new ScalaJS.c.jl_ArithmeticException().init___T("/ by zero")
  } else if ((((this.l$2 === 0) && (this.m$2 === 0)) && (this.h$2 === 0))) {
    return [ScalaJS.m.sjsr_RuntimeLong$().Zero$1, ScalaJS.m.sjsr_RuntimeLong$().Zero$1]
  } else if (y.equals__sjsr_RuntimeLong__Z(ScalaJS.m.sjsr_RuntimeLong$().MinValue$1)) {
    return (this.equals__sjsr_RuntimeLong__Z(ScalaJS.m.sjsr_RuntimeLong$().MinValue$1) ? [ScalaJS.m.sjsr_RuntimeLong$().One$1, ScalaJS.m.sjsr_RuntimeLong$().Zero$1] : [ScalaJS.m.sjsr_RuntimeLong$().Zero$1, this])
  } else {
    var xNegative = ((524288 & this.h$2) !== 0);
    var yNegative = ((524288 & y.h$2) !== 0);
    var xMinValue = this.equals__sjsr_RuntimeLong__Z(ScalaJS.m.sjsr_RuntimeLong$().MinValue$1);
    var pow = y.powerOfTwo__p2__I();
    if ((pow >= 0)) {
      if (xMinValue) {
        var z = this.$$greater$greater__I__sjsr_RuntimeLong(pow);
        return [(yNegative ? z.unary$und$minus__sjsr_RuntimeLong() : z), ScalaJS.m.sjsr_RuntimeLong$().Zero$1]
      } else {
        var absX = (((524288 & this.h$2) !== 0) ? this.unary$und$minus__sjsr_RuntimeLong() : this);
        var absZ = absX.$$greater$greater__I__sjsr_RuntimeLong(pow);
        var z$2 = ((xNegative !== yNegative) ? absZ.unary$und$minus__sjsr_RuntimeLong() : absZ);
        var remAbs = ((pow <= 22) ? new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((absX.l$2 & (((-1) + (1 << pow)) | 0)), 0, 0) : ((pow <= 44) ? new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(absX.l$2, (absX.m$2 & (((-1) + (1 << (((-22) + pow) | 0))) | 0)), 0) : new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(absX.l$2, absX.m$2, (absX.h$2 & (((-1) + (1 << (((-44) + pow) | 0))) | 0)))));
        var rem = (xNegative ? remAbs.unary$und$minus__sjsr_RuntimeLong() : remAbs);
        return [z$2, rem]
      }
    } else {
      var absY = (((524288 & y.h$2) !== 0) ? y.unary$und$minus__sjsr_RuntimeLong() : y);
      if (xMinValue) {
        var newX = ScalaJS.m.sjsr_RuntimeLong$().MaxValue$1
      } else {
        var absX$2 = (((524288 & this.h$2) !== 0) ? this.unary$und$minus__sjsr_RuntimeLong() : this);
        if (absY.$$greater__sjsr_RuntimeLong__Z(absX$2)) {
          var newX;
          return [ScalaJS.m.sjsr_RuntimeLong$().Zero$1, this]
        } else {
          var newX = absX$2
        }
      };
      var shift = ((absY.numberOfLeadingZeros__I() - newX.numberOfLeadingZeros__I()) | 0);
      var yShift = absY.$$less$less__I__sjsr_RuntimeLong(shift);
      var shift$1 = shift;
      var yShift$1 = yShift;
      var curX = newX;
      var quot = ScalaJS.m.sjsr_RuntimeLong$().Zero$1;
      x: {
        var x1;
        _divide0: while (true) {
          if ((shift$1 < 0)) {
            var jsx$1 = true
          } else {
            var this$1 = curX;
            var jsx$1 = (((this$1.l$2 === 0) && (this$1.m$2 === 0)) && (this$1.h$2 === 0))
          };
          if (jsx$1) {
            var _1 = quot;
            var _2 = curX;
            var x1_$_$$und1$f = _1;
            var x1_$_$$und2$f = _2;
            break x
          } else {
            var this$2 = curX;
            var y$1 = yShift$1;
            var newX$1 = this$2.$$plus__sjsr_RuntimeLong__sjsr_RuntimeLong(y$1.unary$und$minus__sjsr_RuntimeLong());
            if (((524288 & newX$1.h$2) === 0)) {
              var temp$shift = (((-1) + shift$1) | 0);
              var temp$yShift = yShift$1.$$greater$greater__I__sjsr_RuntimeLong(1);
              var temp$quot = quot.scala$scalajs$runtime$RuntimeLong$$setBit__I__sjsr_RuntimeLong(shift$1);
              shift$1 = temp$shift;
              yShift$1 = temp$yShift;
              curX = newX$1;
              quot = temp$quot;
              continue _divide0
            } else {
              var temp$shift$2 = (((-1) + shift$1) | 0);
              var temp$yShift$2 = yShift$1.$$greater$greater__I__sjsr_RuntimeLong(1);
              shift$1 = temp$shift$2;
              yShift$1 = temp$yShift$2;
              continue _divide0
            }
          }
        }
      };
      var absQuot = ScalaJS.as.sjsr_RuntimeLong(x1_$_$$und1$f);
      var absRem = ScalaJS.as.sjsr_RuntimeLong(x1_$_$$und2$f);
      var x$3_$_$$und1$f = absQuot;
      var x$3_$_$$und2$f = absRem;
      var absQuot$2 = ScalaJS.as.sjsr_RuntimeLong(x$3_$_$$und1$f);
      var absRem$2 = ScalaJS.as.sjsr_RuntimeLong(x$3_$_$$und2$f);
      var quot$1 = ((xNegative !== yNegative) ? absQuot$2.unary$und$minus__sjsr_RuntimeLong() : absQuot$2);
      if ((xNegative && xMinValue)) {
        var this$3 = absRem$2.unary$und$minus__sjsr_RuntimeLong();
        var y$2 = ScalaJS.m.sjsr_RuntimeLong$().One$1;
        var rem$1 = this$3.$$plus__sjsr_RuntimeLong__sjsr_RuntimeLong(y$2.unary$und$minus__sjsr_RuntimeLong())
      } else {
        var rem$1 = (xNegative ? absRem$2.unary$und$minus__sjsr_RuntimeLong() : absRem$2)
      };
      return [quot$1, rem$1]
    }
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$amp__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((this.l$2 & y.l$2), (this.m$2 & y.m$2), (this.h$2 & y.h$2))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$greater$greater$greater__I__sjsr_RuntimeLong = (function(n_in) {
  var n = (63 & n_in);
  if ((n < 22)) {
    var remBits = ((22 - n) | 0);
    var l = ((this.l$2 >> n) | (this.m$2 << remBits));
    var m = ((this.m$2 >> n) | (this.h$2 << remBits));
    var h = ((this.h$2 >>> n) | 0);
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l), (4194303 & m), (1048575 & h))
  } else if ((n < 44)) {
    var shfBits = (((-22) + n) | 0);
    var remBits$2 = ((44 - n) | 0);
    var l$1 = ((this.m$2 >> shfBits) | (this.h$2 << remBits$2));
    var m$1 = ((this.h$2 >>> shfBits) | 0);
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l$1), (4194303 & m$1), 0)
  } else {
    var l$2 = ((this.h$2 >>> (((-44) + n) | 0)) | 0);
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l$2), 0, 0)
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.compareTo__sjsr_RuntimeLong__I = (function(that) {
  return (this.equals__sjsr_RuntimeLong__Z(that) ? 0 : (this.$$greater__sjsr_RuntimeLong__Z(that) ? 1 : (-1)))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$greater__sjsr_RuntimeLong__Z = (function(y) {
  return (((524288 & this.h$2) === 0) ? (((((524288 & y.h$2) !== 0) || (this.h$2 > y.h$2)) || ((this.h$2 === y.h$2) && (this.m$2 > y.m$2))) || (((this.h$2 === y.h$2) && (this.m$2 === y.m$2)) && (this.l$2 > y.l$2))) : (!(((((524288 & y.h$2) === 0) || (this.h$2 < y.h$2)) || ((this.h$2 === y.h$2) && (this.m$2 < y.m$2))) || (((this.h$2 === y.h$2) && (this.m$2 === y.m$2)) && (this.l$2 <= y.l$2)))))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$less$less__I__sjsr_RuntimeLong = (function(n_in) {
  var n = (63 & n_in);
  if ((n < 22)) {
    var remBits = ((22 - n) | 0);
    var l = (this.l$2 << n);
    var m = ((this.m$2 << n) | (this.l$2 >> remBits));
    var h = ((this.h$2 << n) | (this.m$2 >> remBits));
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l), (4194303 & m), (1048575 & h))
  } else if ((n < 44)) {
    var shfBits = (((-22) + n) | 0);
    var remBits$2 = ((44 - n) | 0);
    var m$1 = (this.l$2 << shfBits);
    var h$1 = ((this.m$2 << shfBits) | (this.l$2 >> remBits$2));
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(0, (4194303 & m$1), (1048575 & h$1))
  } else {
    var h$2 = (this.l$2 << (((-44) + n) | 0));
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(0, 0, (1048575 & h$2))
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toInt__I = (function() {
  return (this.l$2 | (this.m$2 << 22))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.init___I = (function(value) {
  ScalaJS.c.sjsr_RuntimeLong.prototype.init___I__I__I.call(this, (4194303 & value), (4194303 & (value >> 22)), ((value < 0) ? 1048575 : 0));
  return this
});
ScalaJS.c.sjsr_RuntimeLong.prototype.notEquals__sjsr_RuntimeLong__Z = (function(that) {
  return (!this.equals__sjsr_RuntimeLong__Z(that))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.unary$und$minus__sjsr_RuntimeLong = (function() {
  var neg0 = (4194303 & ((1 + (~this.l$2)) | 0));
  var neg1 = (4194303 & (((~this.m$2) + ((neg0 === 0) ? 1 : 0)) | 0));
  var neg2 = (1048575 & (((~this.h$2) + (((neg0 === 0) && (neg1 === 0)) ? 1 : 0)) | 0));
  return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(neg0, neg1, neg2)
});
ScalaJS.c.sjsr_RuntimeLong.prototype.shortValue__S = (function() {
  return this.toShort__S()
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$plus__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  var sum0 = ((this.l$2 + y.l$2) | 0);
  var sum1 = ((((this.m$2 + y.m$2) | 0) + (sum0 >> 22)) | 0);
  var sum2 = ((((this.h$2 + y.h$2) | 0) + (sum1 >> 22)) | 0);
  return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & sum0), (4194303 & sum1), (1048575 & sum2))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$greater$greater__I__sjsr_RuntimeLong = (function(n_in) {
  var n = (63 & n_in);
  var negative = ((524288 & this.h$2) !== 0);
  var xh = (negative ? ((-1048576) | this.h$2) : this.h$2);
  if ((n < 22)) {
    var remBits = ((22 - n) | 0);
    var l = ((this.l$2 >> n) | (this.m$2 << remBits));
    var m = ((this.m$2 >> n) | (xh << remBits));
    var h = (xh >> n);
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l), (4194303 & m), (1048575 & h))
  } else if ((n < 44)) {
    var shfBits = (((-22) + n) | 0);
    var remBits$2 = ((44 - n) | 0);
    var l$1 = ((this.m$2 >> shfBits) | (xh << remBits$2));
    var m$1 = (xh >> shfBits);
    var h$1 = (negative ? 1048575 : 0);
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l$1), (4194303 & m$1), (1048575 & h$1))
  } else {
    var l$2 = (xh >> (((-44) + n) | 0));
    var m$2 = (negative ? 4194303 : 0);
    var h$2 = (negative ? 1048575 : 0);
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l$2), (4194303 & m$2), (1048575 & h$2))
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toDouble__D = (function() {
  return (this.equals__sjsr_RuntimeLong__Z(ScalaJS.m.sjsr_RuntimeLong$().MinValue$1) ? (-9.223372036854776E18) : (((524288 & this.h$2) !== 0) ? (-this.unary$und$minus__sjsr_RuntimeLong().toDouble__D()) : ((this.l$2 + (4194304.0 * this.m$2)) + (1.7592186044416E13 * this.h$2))))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$div__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  return ScalaJS.as.sjsr_RuntimeLong(this.scala$scalajs$runtime$RuntimeLong$$divMod__sjsr_RuntimeLong__sjs_js_Array(y)[0])
});
ScalaJS.c.sjsr_RuntimeLong.prototype.numberOfLeadingZeros__I = (function() {
  return ((this.h$2 !== 0) ? (((-12) + ScalaJS.m.jl_Integer$().numberOfLeadingZeros__I__I(this.h$2)) | 0) : ((this.m$2 !== 0) ? ((10 + ScalaJS.m.jl_Integer$().numberOfLeadingZeros__I__I(this.m$2)) | 0) : ((32 + ScalaJS.m.jl_Integer$().numberOfLeadingZeros__I__I(this.l$2)) | 0)))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toByte__B = (function() {
  return ((this.toInt__I() << 24) >> 24)
});
ScalaJS.c.sjsr_RuntimeLong.prototype.doubleValue__D = (function() {
  return this.toDouble__D()
});
ScalaJS.c.sjsr_RuntimeLong.prototype.hashCode__I = (function() {
  return this.$$up__sjsr_RuntimeLong__sjsr_RuntimeLong(this.$$greater$greater$greater__I__sjsr_RuntimeLong(32)).toInt__I()
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toOctalString__T = (function() {
  var lp = (2097151 & this.l$2);
  var mp = (((1048575 & this.m$2) << 1) | (this.l$2 >> 21));
  var hp = ((this.h$2 << 2) | (this.m$2 >> 20));
  if ((hp !== 0)) {
    var x = ScalaJS.uD((hp >>> 0));
    var jsx$5 = x["toString"](8);
    var jsx$4 = ScalaJS.as.T(jsx$5);
    var x$1 = ScalaJS.uD((mp >>> 0));
    var jsx$2 = x$1["toString"](8);
    var s = ScalaJS.as.T(jsx$2);
    var beginIndex = ScalaJS.uI(s["length"]);
    var jsx$3 = ScalaJS.as.T("0000000"["substring"](beginIndex));
    var x$2 = ScalaJS.uD((lp >>> 0));
    var jsx$1 = x$2["toString"](8);
    var s$1 = ScalaJS.as.T(jsx$1);
    var beginIndex$1 = ScalaJS.uI(s$1["length"]);
    return ((jsx$4 + (("" + jsx$3) + s)) + (("" + ScalaJS.as.T("0000000"["substring"](beginIndex$1))) + s$1))
  } else if ((mp !== 0)) {
    var x$3 = ScalaJS.uD((mp >>> 0));
    var jsx$8 = x$3["toString"](8);
    var jsx$7 = ScalaJS.as.T(jsx$8);
    var x$4 = ScalaJS.uD((lp >>> 0));
    var jsx$6 = x$4["toString"](8);
    var s$2 = ScalaJS.as.T(jsx$6);
    var beginIndex$2 = ScalaJS.uI(s$2["length"]);
    return (jsx$7 + (("" + ScalaJS.as.T("0000000"["substring"](beginIndex$2))) + s$2))
  } else {
    var x$5 = ScalaJS.uD((lp >>> 0));
    var jsx$9 = x$5["toString"](8);
    return ScalaJS.as.T(jsx$9)
  }
});
ScalaJS.c.sjsr_RuntimeLong.prototype.intValue__I = (function() {
  return this.toInt__I()
});
ScalaJS.c.sjsr_RuntimeLong.prototype.unary$und$tilde__sjsr_RuntimeLong = (function() {
  var l = (~this.l$2);
  var m = (~this.m$2);
  var h = (~this.h$2);
  return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((4194303 & l), (4194303 & m), (1048575 & h))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.compareTo__jl_Long__I = (function(that) {
  return this.compareTo__sjsr_RuntimeLong__I(ScalaJS.as.sjsr_RuntimeLong(that))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.floatValue__F = (function() {
  return this.toFloat__F()
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$minus__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  return this.$$plus__sjsr_RuntimeLong__sjsr_RuntimeLong(y.unary$und$minus__sjsr_RuntimeLong())
});
ScalaJS.c.sjsr_RuntimeLong.prototype.toFloat__F = (function() {
  return ScalaJS.fround(this.toDouble__D())
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$$up__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(y) {
  return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I((this.l$2 ^ y.l$2), (this.m$2 ^ y.m$2), (this.h$2 ^ y.h$2))
});
ScalaJS.c.sjsr_RuntimeLong.prototype.equals__sjsr_RuntimeLong__Z = (function(y) {
  return (((this.l$2 === y.l$2) && (this.m$2 === y.m$2)) && (this.h$2 === y.h$2))
});
ScalaJS.is.sjsr_RuntimeLong = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sjsr_RuntimeLong)))
});
ScalaJS.as.sjsr_RuntimeLong = (function(obj) {
  return ((ScalaJS.is.sjsr_RuntimeLong(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.scalajs.runtime.RuntimeLong"))
});
ScalaJS.isArrayOf.sjsr_RuntimeLong = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sjsr_RuntimeLong)))
});
ScalaJS.asArrayOf.sjsr_RuntimeLong = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sjsr_RuntimeLong(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.scalajs.runtime.RuntimeLong;", depth))
});
ScalaJS.d.sjsr_RuntimeLong = new ScalaJS.ClassTypeData({
  sjsr_RuntimeLong: 0
}, false, "scala.scalajs.runtime.RuntimeLong", {
  sjsr_RuntimeLong: 1,
  jl_Number: 1,
  O: 1,
  jl_Comparable: 1
});
ScalaJS.c.sjsr_RuntimeLong.prototype.$classData = ScalaJS.d.sjsr_RuntimeLong;
/** @constructor */
ScalaJS.c.sjsr_RuntimeLong$ = (function() {
  ScalaJS.c.O.call(this);
  this.BITS$1 = 0;
  this.BITS01$1 = 0;
  this.BITS2$1 = 0;
  this.MASK$1 = 0;
  this.MASK$und2$1 = 0;
  this.SIGN$undBIT$1 = 0;
  this.SIGN$undBIT$undVALUE$1 = 0;
  this.TWO$undPWR$und15$undDBL$1 = 0.0;
  this.TWO$undPWR$und16$undDBL$1 = 0.0;
  this.TWO$undPWR$und22$undDBL$1 = 0.0;
  this.TWO$undPWR$und31$undDBL$1 = 0.0;
  this.TWO$undPWR$und32$undDBL$1 = 0.0;
  this.TWO$undPWR$und44$undDBL$1 = 0.0;
  this.TWO$undPWR$und63$undDBL$1 = 0.0;
  this.Zero$1 = null;
  this.One$1 = null;
  this.MinusOne$1 = null;
  this.MinValue$1 = null;
  this.MaxValue$1 = null;
  this.TenPow9$1 = null
});
ScalaJS.c.sjsr_RuntimeLong$.prototype = new ScalaJS.h.O();
ScalaJS.c.sjsr_RuntimeLong$.prototype.constructor = ScalaJS.c.sjsr_RuntimeLong$;
/** @constructor */
ScalaJS.h.sjsr_RuntimeLong$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_RuntimeLong$.prototype = ScalaJS.c.sjsr_RuntimeLong$.prototype;
ScalaJS.c.sjsr_RuntimeLong$.prototype.init___ = (function() {
  ScalaJS.n.sjsr_RuntimeLong$ = this;
  this.Zero$1 = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(0, 0, 0);
  this.One$1 = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(1, 0, 0);
  this.MinusOne$1 = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 4194303, 1048575);
  this.MinValue$1 = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(0, 0, 524288);
  this.MaxValue$1 = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(4194303, 4194303, 524287);
  this.TenPow9$1 = new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(1755648, 238, 0);
  return this
});
ScalaJS.c.sjsr_RuntimeLong$.prototype.Zero__sjsr_RuntimeLong = (function() {
  return this.Zero$1
});
ScalaJS.c.sjsr_RuntimeLong$.prototype.fromDouble__D__sjsr_RuntimeLong = (function(value) {
  if ((value !== value)) {
    return this.Zero$1
  } else if ((value < (-9.223372036854776E18))) {
    return this.MinValue$1
  } else if ((value >= 9.223372036854776E18)) {
    return this.MaxValue$1
  } else if ((value < 0)) {
    return this.fromDouble__D__sjsr_RuntimeLong((-value)).unary$und$minus__sjsr_RuntimeLong()
  } else {
    var acc = value;
    var a2 = ((acc >= 1.7592186044416E13) ? ((acc / 1.7592186044416E13) | 0) : 0);
    acc = (acc - (1.7592186044416E13 * a2));
    var a1 = ((acc >= 4194304.0) ? ((acc / 4194304.0) | 0) : 0);
    acc = (acc - (4194304.0 * a1));
    var a0 = (acc | 0);
    return new ScalaJS.c.sjsr_RuntimeLong().init___I__I__I(a0, a1, a2)
  }
});
ScalaJS.d.sjsr_RuntimeLong$ = new ScalaJS.ClassTypeData({
  sjsr_RuntimeLong$: 0
}, false, "scala.scalajs.runtime.RuntimeLong$", {
  sjsr_RuntimeLong$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sjsr_RuntimeLong$.prototype.$classData = ScalaJS.d.sjsr_RuntimeLong$;
ScalaJS.n.sjsr_RuntimeLong$ = (void 0);
ScalaJS.m.sjsr_RuntimeLong$ = (function() {
  if ((!ScalaJS.n.sjsr_RuntimeLong$)) {
    ScalaJS.n.sjsr_RuntimeLong$ = new ScalaJS.c.sjsr_RuntimeLong$().init___()
  };
  return ScalaJS.n.sjsr_RuntimeLong$
});
/** @constructor */
ScalaJS.c.sr_AbstractPartialFunction = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sr_AbstractPartialFunction.prototype = new ScalaJS.h.O();
ScalaJS.c.sr_AbstractPartialFunction.prototype.constructor = ScalaJS.c.sr_AbstractPartialFunction;
/** @constructor */
ScalaJS.h.sr_AbstractPartialFunction = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_AbstractPartialFunction.prototype = ScalaJS.c.sr_AbstractPartialFunction.prototype;
ScalaJS.c.sr_AbstractPartialFunction.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sr_AbstractPartialFunction.prototype.apply__O__O = (function(x) {
  return this.applyOrElse__O__F1__O(x, ScalaJS.m.s_PartialFunction$().empty$undpf$1)
});
ScalaJS.c.sr_AbstractPartialFunction.prototype.runWith__F1__F1 = (function(action) {
  return ScalaJS.s.s_PartialFunction$class__runWith__s_PartialFunction__F1__F1(this, action)
});
ScalaJS.c.sr_AbstractPartialFunction.prototype.toString__T = (function() {
  return "<function1>"
});
ScalaJS.d.sr_Nothing$ = new ScalaJS.ClassTypeData({
  sr_Nothing$: 0
}, false, "scala.runtime.Nothing$", {
  sr_Nothing$: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
/** @constructor */
ScalaJS.c.Lenemies_Human = (function() {
  ScalaJS.c.Lenemies_BaseHuman.call(this);
  this.dest$4 = null;
  this.hasSeenPlayer$4 = false;
  this.isLowAmmo$4 = false;
  this.isLowHealth$4 = false
});
ScalaJS.c.Lenemies_Human.prototype = new ScalaJS.h.Lenemies_BaseHuman();
ScalaJS.c.Lenemies_Human.prototype.constructor = ScalaJS.c.Lenemies_Human;
/** @constructor */
ScalaJS.h.Lenemies_Human = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Human.prototype = ScalaJS.c.Lenemies_Human.prototype;
ScalaJS.c.Lenemies_Human.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lenemies_BaseHuman.prototype.init___Lglobalvars_Pt__I.call(this, loc_, ScalaJS.m.Lglobalvars_GV$().HUMAN$undHEALTH$1);
  this.dest$4 = new ScalaJS.c.Lglobalvars_Pt().init___D__D((-1.0), (-1.0));
  this.hasSeenPlayer$4 = false;
  this.isLowAmmo$4 = false;
  this.isLowHealth$4 = false;
  this.gun$3.ammo$1 = ScalaJS.m.Lglobalvars_GV$().HUMAN$undAMMO$1;
  return this
});
ScalaJS.c.Lenemies_Human.prototype.onLowAmmo__Lgame_Game__V = (function(g) {
  ScalaJS.m.sci_List$();
  var xs = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["I'm almost out of ammo!", "I'm running low on bullets!", "We'd better find more ammo soon!"]);
  var this$2 = ScalaJS.m.sci_List$();
  var cbf = this$2.ReusableCBFInstance$2;
  var phrases = ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(xs, cbf));
  var this$3 = g.r$1;
  var n = ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I(phrases);
  var n$1 = this$3.self$1.nextInt__I__I(n);
  var phrase = ScalaJS.as.T(ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(phrases, n$1));
  g.addHeadText__Lobjects_Obj__T__I__V(this, phrase, 100);
  this.isLowAmmo$4 = true
});
ScalaJS.c.Lenemies_Human.prototype.canTakeItem__Lobjects_Actor__Z = (function(item) {
  if (ScalaJS.is.Lobjects_AmmoPack(item)) {
    if (this.isLowAmmo$4) {
      this.isLowAmmo$4 = false;
      return true
    }
  } else if (ScalaJS.is.Lobjects_HealthPack(item)) {
    if (this.isLowHealth$4) {
      this.isLowHealth$4 = false;
      return true
    }
  } else {
    return false
  };
  return false
});
ScalaJS.c.Lenemies_Human.prototype.onLowHealth__Lgame_Game__V = (function(g) {
  ScalaJS.m.sci_List$();
  var xs = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["I'm gonna die!", "I'm dying bro!", "I'm not gonna make it!", "Go on without me!"]);
  var this$2 = ScalaJS.m.sci_List$();
  var cbf = this$2.ReusableCBFInstance$2;
  var phrases = ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(xs, cbf));
  var this$3 = g.r$1;
  var n = ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I(phrases);
  var n$1 = this$3.self$1.nextInt__I__I(n);
  var phrase = ScalaJS.as.T(ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(phrases, n$1));
  g.addHeadText__Lobjects_Obj__T__I__V(this, phrase, 100);
  this.isLowHealth$4 = true
});
ScalaJS.c.Lenemies_Human.prototype.onSeePlayer__Lgame_Game__V = (function(g) {
  ScalaJS.m.sci_List$();
  var xs = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["Good to see you!", "We'll bang ok", "Help me!", "Save me!", "Take the lead!", "I'll follow you!", "Freakin' ZOMBIES man!", "Let's get out of here!", "Run for your life!"]);
  var this$2 = ScalaJS.m.sci_List$();
  var cbf = this$2.ReusableCBFInstance$2;
  var phrases = ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(xs, cbf));
  var this$3 = g.r$1;
  var n = ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I(phrases);
  var n$1 = this$3.self$1.nextInt__I__I(n);
  var phrase = ScalaJS.as.T(ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(phrases, n$1));
  g.addHeadText__Lobjects_Obj__T__I__V(this, phrase, 100);
  this.hasSeenPlayer$4 = true
});
ScalaJS.c.Lenemies_Human.prototype.onDeath__Lgame_Game__V = (function(g) {
  if ((this.gun$3.ammo$1 > 0)) {
    var pack = new ScalaJS.c.Lobjects_AmmoPack().init___Lglobalvars_Pt__I(this.loc$1, this.gun$3.ammo$1);
    g.addActor__Lobjects_Actor__scm_Set(pack)
  }
});
ScalaJS.c.Lenemies_Human.prototype.aiMove__Lgame_Game__V = (function(g) {
  ScalaJS.c.Lenemies_BaseHuman.prototype.aiMove__Lgame_Game__V.call(this, g);
  if (((!this.hasSeenPlayer$4) && this.hasLosTo__Lobjects_Obj__Lgame_Game__Z(g.player$1, g))) {
    this.onSeePlayer__Lgame_Game__V(g)
  };
  if (((!this.isLowAmmo$4) && (this.gun$3.ammo$1 <= 10))) {
    this.onLowAmmo__Lgame_Game__V(g)
  };
  if (((!this.isLowHealth$4) && (this.hp$2 <= 20))) {
    this.onLowHealth__Lgame_Game__V(g)
  };
  if (this.dest$4.is$undneg$undone__Z()) {
    var jsx$1 = g.player$1.loc$1.x$1;
    var this$1 = g.r$1;
    var n = ScalaJS.imul(10, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1);
    var destX = ((jsx$1 + this$1.self$1.nextInt__I__I(n)) - ScalaJS.imul(5, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1));
    var jsx$2 = g.player$1.loc$1.y$1;
    var this$2 = g.r$1;
    var n$1 = ScalaJS.imul(10, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1);
    var destY = ((jsx$2 + this$2.self$1.nextInt__I__I(n$1)) - ScalaJS.imul(5, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1));
    this.dest$4 = new ScalaJS.c.Lglobalvars_Pt().init___D__D(destX, destY)
  };
  var x = (this.dest$4.x$1 - this.loc$1.x$1);
  var dx = ((x < 2.0) ? x : 2.0);
  var x$1 = dx;
  dx = ((x$1 > (-2.0)) ? x$1 : (-2.0));
  var x$2 = (this.dest$4.y$1 - this.loc$1.y$1);
  var dy = ((x$2 < 2.0) ? x$2 : 2.0);
  var x$3 = dy;
  dy = ((x$3 > (-2.0)) ? x$3 : (-2.0));
  if ((!this.moveLoc__D__D__Lgame_Game__Z(dx, dy, g))) {
    this.dest$4 = new ScalaJS.c.Lglobalvars_Pt().init___D__D((-1.0), (-1.0))
  }
});
ScalaJS.c.Lenemies_Human.prototype.moveToNewMap__Lgame_Game__V = (function(g) {
  this.dest$4 = new ScalaJS.c.Lglobalvars_Pt().init___D__D((-1.0), (-1.0))
});
ScalaJS.c.Lenemies_Human.prototype.draw__Lgame_Game__V = (function(g) {
  g.ctx$1["fillStyle"] = "pink";
  g.ctx$1["fillRect"](this.loc$1.x$1, this.loc$1.y$1, this.size$1.x$1, this.size$1.y$1)
});
ScalaJS.d.Lenemies_Human = new ScalaJS.ClassTypeData({
  Lenemies_Human: 0
}, false, "enemies.Human", {
  Lenemies_Human: 1,
  Lenemies_BaseHuman: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lenemies_Human.prototype.$classData = ScalaJS.d.Lenemies_Human;
/** @constructor */
ScalaJS.c.Lenemies_Player = (function() {
  ScalaJS.c.Lenemies_BaseHuman.call(this);
  this.usableItems$4 = null
});
ScalaJS.c.Lenemies_Player.prototype = new ScalaJS.h.Lenemies_BaseHuman();
ScalaJS.c.Lenemies_Player.prototype.constructor = ScalaJS.c.Lenemies_Player;
/** @constructor */
ScalaJS.h.Lenemies_Player = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Player.prototype = ScalaJS.c.Lenemies_Player.prototype;
ScalaJS.c.Lenemies_Player.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lenemies_BaseHuman.prototype.init___Lglobalvars_Pt__I.call(this, loc_, ScalaJS.m.Lglobalvars_GV$().PLAYER$undHEALTH$1);
  this.usableItems$4 = new ScalaJS.c.scm_Stack().init___();
  return this
});
ScalaJS.c.Lenemies_Player.prototype.canTakeItem__Lobjects_Actor__Z = (function(item) {
  return true
});
ScalaJS.c.Lenemies_Player.prototype.useItem__Lgame_Game__V = (function(g) {
  var this$1 = this.usableItems$4;
  var this$2 = this$1.elems$5;
  if ((ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I(this$2) > 0)) {
    var usable = ScalaJS.as.Lobjects_UsableItem(this.usableItems$4.pop__O());
    usable.use__Lgame_Game__V(g)
  }
});
ScalaJS.c.Lenemies_Player.prototype.aiMove__Lgame_Game__V = (function(g) {
  ScalaJS.c.Lenemies_BaseHuman.prototype.aiMove__Lgame_Game__V.call(this, g)
});
ScalaJS.c.Lenemies_Player.prototype.addUsableItem__Lobjects_UsableItem__V = (function(item) {
  this.usableItems$4.push__O__scm_Stack(item);
  var jsx$1 = ScalaJS.g["console"];
  var this$1 = this.usableItems$4;
  var this$2 = this$1.elems$5;
  var value = ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I(this$2);
  jsx$1["log"](value)
});
ScalaJS.c.Lenemies_Player.prototype.draw__Lgame_Game__V = (function(g) {
  var img = g.images$1.apply__O__O("char_human_1");
  g.ctx$1["drawImage"](img, this.loc$1.x$1, this.loc$1.y$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1)
});
ScalaJS.is.Lenemies_Player = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lenemies_Player)))
});
ScalaJS.as.Lenemies_Player = (function(obj) {
  return ((ScalaJS.is.Lenemies_Player(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "enemies.Player"))
});
ScalaJS.isArrayOf.Lenemies_Player = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lenemies_Player)))
});
ScalaJS.asArrayOf.Lenemies_Player = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lenemies_Player(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lenemies.Player;", depth))
});
ScalaJS.d.Lenemies_Player = new ScalaJS.ClassTypeData({
  Lenemies_Player: 0
}, false, "enemies.Player", {
  Lenemies_Player: 1,
  Lenemies_BaseHuman: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lenemies_Player.prototype.$classData = ScalaJS.d.Lenemies_Player;
/** @constructor */
ScalaJS.c.Ljava_io_FilterOutputStream = (function() {
  ScalaJS.c.Ljava_io_OutputStream.call(this);
  this.out$2 = null
});
ScalaJS.c.Ljava_io_FilterOutputStream.prototype = new ScalaJS.h.Ljava_io_OutputStream();
ScalaJS.c.Ljava_io_FilterOutputStream.prototype.constructor = ScalaJS.c.Ljava_io_FilterOutputStream;
/** @constructor */
ScalaJS.h.Ljava_io_FilterOutputStream = (function() {
  /*<skip>*/
});
ScalaJS.h.Ljava_io_FilterOutputStream.prototype = ScalaJS.c.Ljava_io_FilterOutputStream.prototype;
ScalaJS.c.Ljava_io_FilterOutputStream.prototype.init___Ljava_io_OutputStream = (function(out) {
  this.out$2 = out;
  return this
});
/** @constructor */
ScalaJS.c.Lobjects_AmmoPack = (function() {
  ScalaJS.c.Lobjects_Item.call(this);
  this.amount$4 = 0
});
ScalaJS.c.Lobjects_AmmoPack.prototype = new ScalaJS.h.Lobjects_Item();
ScalaJS.c.Lobjects_AmmoPack.prototype.constructor = ScalaJS.c.Lobjects_AmmoPack;
/** @constructor */
ScalaJS.h.Lobjects_AmmoPack = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_AmmoPack.prototype = ScalaJS.c.Lobjects_AmmoPack.prototype;
ScalaJS.c.Lobjects_AmmoPack.prototype.pickup__Lobjects_Actor__V = (function(owner) {
  if (ScalaJS.is.Lenemies_BaseHuman(owner)) {
    var x2 = ScalaJS.as.Lenemies_BaseHuman(owner);
    var ev$5 = x2.gun$3;
    ev$5.ammo$1 = ((ev$5.ammo$1 + this.amount$4) | 0)
  }
});
ScalaJS.c.Lobjects_AmmoPack.prototype.init___Lglobalvars_Pt__I = (function(loc_, amt_) {
  ScalaJS.c.Lobjects_Item.prototype.init___Lglobalvars_Pt.call(this, loc_);
  this.amount$4 = amt_;
  return this
});
ScalaJS.c.Lobjects_AmmoPack.prototype.draw__Lgame_Game__V = (function(g) {
  var img = g.images$1.apply__O__O("item_ammobox");
  g.ctx$1["drawImage"](img, this.loc$1.x$1, this.loc$1.y$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1)
});
ScalaJS.is.Lobjects_AmmoPack = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_AmmoPack)))
});
ScalaJS.as.Lobjects_AmmoPack = (function(obj) {
  return ((ScalaJS.is.Lobjects_AmmoPack(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.AmmoPack"))
});
ScalaJS.isArrayOf.Lobjects_AmmoPack = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_AmmoPack)))
});
ScalaJS.asArrayOf.Lobjects_AmmoPack = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_AmmoPack(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.AmmoPack;", depth))
});
ScalaJS.d.Lobjects_AmmoPack = new ScalaJS.ClassTypeData({
  Lobjects_AmmoPack: 0
}, false, "objects.AmmoPack", {
  Lobjects_AmmoPack: 1,
  Lobjects_Item: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_AmmoPack.prototype.$classData = ScalaJS.d.Lobjects_AmmoPack;
/** @constructor */
ScalaJS.c.Lobjects_GroundGun = (function() {
  ScalaJS.c.Lobjects_Item.call(this);
  this.gun$4 = null;
  this.displayName$4 = null
});
ScalaJS.c.Lobjects_GroundGun.prototype = new ScalaJS.h.Lobjects_Item();
ScalaJS.c.Lobjects_GroundGun.prototype.constructor = ScalaJS.c.Lobjects_GroundGun;
/** @constructor */
ScalaJS.h.Lobjects_GroundGun = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_GroundGun.prototype = ScalaJS.c.Lobjects_GroundGun.prototype;
ScalaJS.c.Lobjects_GroundGun.prototype.pickup__Lobjects_Actor__V = (function(owner) {
  this.gun$4.owner$1 = owner;
  if (ScalaJS.is.Lenemies_BaseHuman(owner)) {
    var x2 = ScalaJS.as.Lenemies_BaseHuman(owner);
    x2.gun$3 = this.gun$4
  }
});
ScalaJS.c.Lobjects_GroundGun.prototype.init___Lglobalvars_Pt__Lenemies_Gun__T = (function(loc_, gun_, displayName_) {
  ScalaJS.c.Lobjects_Item.prototype.init___Lglobalvars_Pt.call(this, loc_);
  this.gun$4 = gun_;
  this.displayName$4 = displayName_;
  return this
});
ScalaJS.c.Lobjects_GroundGun.prototype.draw__Lgame_Game__V = (function(g) {
  var img = g.images$1.apply__O__O(this.displayName$4);
  g.ctx$1["drawImage"](img, this.loc$1.x$1, this.loc$1.y$1, ScalaJS.imul(2, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1), ScalaJS.imul(2, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1))
});
ScalaJS.d.Lobjects_GroundGun = new ScalaJS.ClassTypeData({
  Lobjects_GroundGun: 0
}, false, "objects.GroundGun", {
  Lobjects_GroundGun: 1,
  Lobjects_Item: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_GroundGun.prototype.$classData = ScalaJS.d.Lobjects_GroundGun;
/** @constructor */
ScalaJS.c.Lobjects_GroundUsableItem = (function() {
  ScalaJS.c.Lobjects_Item.call(this);
  this.item$4 = null
});
ScalaJS.c.Lobjects_GroundUsableItem.prototype = new ScalaJS.h.Lobjects_Item();
ScalaJS.c.Lobjects_GroundUsableItem.prototype.constructor = ScalaJS.c.Lobjects_GroundUsableItem;
/** @constructor */
ScalaJS.h.Lobjects_GroundUsableItem = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_GroundUsableItem.prototype = ScalaJS.c.Lobjects_GroundUsableItem.prototype;
ScalaJS.c.Lobjects_GroundUsableItem.prototype.pickup__Lobjects_Actor__V = (function(owner) {
  if (ScalaJS.is.Lenemies_Player(owner)) {
    var x2 = ScalaJS.as.Lenemies_Player(owner);
    this.item$4.owner$1 = x2;
    x2.addUsableItem__Lobjects_UsableItem__V(this.item$4)
  }
});
ScalaJS.c.Lobjects_GroundUsableItem.prototype.init___Lglobalvars_Pt__Lobjects_UsableItem__T = (function(loc_, item_, displayName_) {
  ScalaJS.c.Lobjects_Item.prototype.init___Lglobalvars_Pt.call(this, loc_);
  this.item$4 = item_;
  return this
});
ScalaJS.c.Lobjects_GroundUsableItem.prototype.draw__Lgame_Game__V = (function(g) {
  var img = g.images$1.apply__O__O(this.item$4.displayName$1);
  g.ctx$1["drawImage"](img, this.loc$1.x$1, this.loc$1.y$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1)
});
ScalaJS.d.Lobjects_GroundUsableItem = new ScalaJS.ClassTypeData({
  Lobjects_GroundUsableItem: 0
}, false, "objects.GroundUsableItem", {
  Lobjects_GroundUsableItem: 1,
  Lobjects_Item: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_GroundUsableItem.prototype.$classData = ScalaJS.d.Lobjects_GroundUsableItem;
/** @constructor */
ScalaJS.c.Lobjects_HealthPack = (function() {
  ScalaJS.c.Lobjects_Item.call(this);
  this.amount$4 = 0
});
ScalaJS.c.Lobjects_HealthPack.prototype = new ScalaJS.h.Lobjects_Item();
ScalaJS.c.Lobjects_HealthPack.prototype.constructor = ScalaJS.c.Lobjects_HealthPack;
/** @constructor */
ScalaJS.h.Lobjects_HealthPack = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_HealthPack.prototype = ScalaJS.c.Lobjects_HealthPack.prototype;
ScalaJS.c.Lobjects_HealthPack.prototype.pickup__Lobjects_Actor__V = (function(owner) {
  var x = (owner.hp$2 + this.amount$4);
  var y = owner.maxHp$2;
  owner.hp$2 = ((x < y) ? x : y)
});
ScalaJS.c.Lobjects_HealthPack.prototype.init___Lglobalvars_Pt__I = (function(loc_, amt_) {
  ScalaJS.c.Lobjects_Item.prototype.init___Lglobalvars_Pt.call(this, loc_);
  this.amount$4 = amt_;
  return this
});
ScalaJS.c.Lobjects_HealthPack.prototype.draw__Lgame_Game__V = (function(g) {
  var img = g.images$1.apply__O__O("item_healthkit");
  g.ctx$1["drawImage"](img, this.loc$1.x$1, this.loc$1.y$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1, ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1)
});
ScalaJS.is.Lobjects_HealthPack = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lobjects_HealthPack)))
});
ScalaJS.as.Lobjects_HealthPack = (function(obj) {
  return ((ScalaJS.is.Lobjects_HealthPack(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "objects.HealthPack"))
});
ScalaJS.isArrayOf.Lobjects_HealthPack = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lobjects_HealthPack)))
});
ScalaJS.asArrayOf.Lobjects_HealthPack = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lobjects_HealthPack(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lobjects.HealthPack;", depth))
});
ScalaJS.is.T = (function(obj) {
  return ((typeof obj) === "string")
});
ScalaJS.as.T = (function(obj) {
  return ((ScalaJS.is.T(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.lang.String"))
});
ScalaJS.isArrayOf.T = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.T)))
});
ScalaJS.asArrayOf.T = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.T(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.String;", depth))
});
ScalaJS.d.T = new ScalaJS.ClassTypeData({
  T: 0
}, false, "java.lang.String", {
  T: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_CharSequence: 1,
  jl_Comparable: 1
}, (void 0), ScalaJS.is.T);
/** @constructor */
ScalaJS.c.jl_AssertionError = (function() {
  ScalaJS.c.jl_Error.call(this)
});
ScalaJS.c.jl_AssertionError.prototype = new ScalaJS.h.jl_Error();
ScalaJS.c.jl_AssertionError.prototype.constructor = ScalaJS.c.jl_AssertionError;
/** @constructor */
ScalaJS.h.jl_AssertionError = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_AssertionError.prototype = ScalaJS.c.jl_AssertionError.prototype;
ScalaJS.c.jl_AssertionError.prototype.init___O = (function(o) {
  ScalaJS.c.jl_AssertionError.prototype.init___T.call(this, ScalaJS.objectToString(o));
  return this
});
ScalaJS.d.jl_AssertionError = new ScalaJS.ClassTypeData({
  jl_AssertionError: 0
}, false, "java.lang.AssertionError", {
  jl_AssertionError: 1,
  jl_Error: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_AssertionError.prototype.$classData = ScalaJS.d.jl_AssertionError;
/** @constructor */
ScalaJS.c.jl_JSConsoleBasedPrintStream$DummyOutputStream = (function() {
  ScalaJS.c.Ljava_io_OutputStream.call(this)
});
ScalaJS.c.jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype = new ScalaJS.h.Ljava_io_OutputStream();
ScalaJS.c.jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype.constructor = ScalaJS.c.jl_JSConsoleBasedPrintStream$DummyOutputStream;
/** @constructor */
ScalaJS.h.jl_JSConsoleBasedPrintStream$DummyOutputStream = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype = ScalaJS.c.jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype;
ScalaJS.d.jl_JSConsoleBasedPrintStream$DummyOutputStream = new ScalaJS.ClassTypeData({
  jl_JSConsoleBasedPrintStream$DummyOutputStream: 0
}, false, "java.lang.JSConsoleBasedPrintStream$DummyOutputStream", {
  jl_JSConsoleBasedPrintStream$DummyOutputStream: 1,
  Ljava_io_OutputStream: 1,
  O: 1,
  Ljava_io_Closeable: 1,
  Ljava_io_Flushable: 1
});
ScalaJS.c.jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype.$classData = ScalaJS.d.jl_JSConsoleBasedPrintStream$DummyOutputStream;
/** @constructor */
ScalaJS.c.jl_RuntimeException = (function() {
  ScalaJS.c.jl_Exception.call(this)
});
ScalaJS.c.jl_RuntimeException.prototype = new ScalaJS.h.jl_Exception();
ScalaJS.c.jl_RuntimeException.prototype.constructor = ScalaJS.c.jl_RuntimeException;
/** @constructor */
ScalaJS.h.jl_RuntimeException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_RuntimeException.prototype = ScalaJS.c.jl_RuntimeException.prototype;
ScalaJS.c.jl_RuntimeException.prototype.init___ = (function() {
  ScalaJS.c.jl_RuntimeException.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
ScalaJS.c.jl_RuntimeException.prototype.init___T = (function(s) {
  ScalaJS.c.jl_RuntimeException.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
/** @constructor */
ScalaJS.c.jl_StringBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.content$1 = null
});
ScalaJS.c.jl_StringBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.jl_StringBuilder.prototype.constructor = ScalaJS.c.jl_StringBuilder;
/** @constructor */
ScalaJS.h.jl_StringBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_StringBuilder.prototype = ScalaJS.c.jl_StringBuilder.prototype;
ScalaJS.c.jl_StringBuilder.prototype.init___ = (function() {
  ScalaJS.c.jl_StringBuilder.prototype.init___T.call(this, "");
  return this
});
ScalaJS.c.jl_StringBuilder.prototype.subSequence__I__I__jl_CharSequence = (function(start, end) {
  var thiz = this.content$1;
  return ScalaJS.as.T(thiz["substring"](start, end))
});
ScalaJS.c.jl_StringBuilder.prototype.append__T__jl_StringBuilder = (function(s) {
  this.content$1 = (("" + this.content$1) + ((s === null) ? "null" : s));
  return this
});
ScalaJS.c.jl_StringBuilder.prototype.toString__T = (function() {
  return this.content$1
});
ScalaJS.c.jl_StringBuilder.prototype.init___jl_CharSequence = (function(csq) {
  ScalaJS.c.jl_StringBuilder.prototype.init___T.call(this, ScalaJS.objectToString(csq));
  return this
});
ScalaJS.c.jl_StringBuilder.prototype.append__jl_CharSequence__jl_Appendable = (function(csq) {
  return this.append__O__jl_StringBuilder(csq)
});
ScalaJS.c.jl_StringBuilder.prototype.append__O__jl_StringBuilder = (function(obj) {
  return ((obj === null) ? this.append__T__jl_StringBuilder(null) : this.append__T__jl_StringBuilder(ScalaJS.objectToString(obj)))
});
ScalaJS.c.jl_StringBuilder.prototype.init___I = (function(initialCapacity) {
  ScalaJS.c.jl_StringBuilder.prototype.init___T.call(this, "");
  return this
});
ScalaJS.c.jl_StringBuilder.prototype.append__jl_CharSequence__I__I__jl_StringBuilder = (function(csq, start, end) {
  return ((csq === null) ? this.append__jl_CharSequence__I__I__jl_StringBuilder("null", start, end) : this.append__T__jl_StringBuilder(ScalaJS.objectToString(ScalaJS.charSequenceSubSequence(csq, start, end))))
});
ScalaJS.c.jl_StringBuilder.prototype.append__C__jl_StringBuilder = (function(c) {
  return this.append__T__jl_StringBuilder(ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c)))
});
ScalaJS.c.jl_StringBuilder.prototype.append__C__jl_Appendable = (function(c) {
  return this.append__C__jl_StringBuilder(c)
});
ScalaJS.c.jl_StringBuilder.prototype.init___T = (function(content) {
  this.content$1 = content;
  return this
});
ScalaJS.c.jl_StringBuilder.prototype.reverse__jl_StringBuilder = (function() {
  var original = this.content$1;
  var result = "";
  var i = 0;
  while ((i < ScalaJS.uI(original["length"]))) {
    var index = i;
    var c = (65535 & ScalaJS.uI(original["charCodeAt"](index)));
    if ((((64512 & c) === 55296) && (((1 + i) | 0) < ScalaJS.uI(original["length"])))) {
      var index$1 = ((1 + i) | 0);
      var c2 = (65535 & ScalaJS.uI(original["charCodeAt"](index$1)));
      if (((64512 & c2) === 56320)) {
        result = ((("" + ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c))) + ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c2))) + result);
        i = ((2 + i) | 0)
      } else {
        result = (("" + ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c))) + result);
        i = ((1 + i) | 0)
      }
    } else {
      result = (("" + ScalaJS.as.T(ScalaJS.g["String"]["fromCharCode"](c))) + result);
      i = ((1 + i) | 0)
    }
  };
  this.content$1 = result;
  return this
});
ScalaJS.d.jl_StringBuilder = new ScalaJS.ClassTypeData({
  jl_StringBuilder: 0
}, false, "java.lang.StringBuilder", {
  jl_StringBuilder: 1,
  O: 1,
  jl_CharSequence: 1,
  jl_Appendable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_StringBuilder.prototype.$classData = ScalaJS.d.jl_StringBuilder;
/** @constructor */
ScalaJS.c.s_Array$ = (function() {
  ScalaJS.c.s_FallbackArrayBuilding.call(this);
  this.emptyBooleanArray$2 = null;
  this.emptyByteArray$2 = null;
  this.emptyCharArray$2 = null;
  this.emptyDoubleArray$2 = null;
  this.emptyFloatArray$2 = null;
  this.emptyIntArray$2 = null;
  this.emptyLongArray$2 = null;
  this.emptyShortArray$2 = null;
  this.emptyObjectArray$2 = null
});
ScalaJS.c.s_Array$.prototype = new ScalaJS.h.s_FallbackArrayBuilding();
ScalaJS.c.s_Array$.prototype.constructor = ScalaJS.c.s_Array$;
/** @constructor */
ScalaJS.h.s_Array$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Array$.prototype = ScalaJS.c.s_Array$.prototype;
ScalaJS.c.s_Array$.prototype.init___ = (function() {
  ScalaJS.n.s_Array$ = this;
  this.emptyBooleanArray$2 = ScalaJS.newArrayObject(ScalaJS.d.Z.getArrayOf(), [0]);
  this.emptyByteArray$2 = ScalaJS.newArrayObject(ScalaJS.d.B.getArrayOf(), [0]);
  this.emptyCharArray$2 = ScalaJS.newArrayObject(ScalaJS.d.C.getArrayOf(), [0]);
  this.emptyDoubleArray$2 = ScalaJS.newArrayObject(ScalaJS.d.D.getArrayOf(), [0]);
  this.emptyFloatArray$2 = ScalaJS.newArrayObject(ScalaJS.d.F.getArrayOf(), [0]);
  this.emptyIntArray$2 = ScalaJS.newArrayObject(ScalaJS.d.I.getArrayOf(), [0]);
  this.emptyLongArray$2 = ScalaJS.newArrayObject(ScalaJS.d.J.getArrayOf(), [0]);
  this.emptyShortArray$2 = ScalaJS.newArrayObject(ScalaJS.d.S.getArrayOf(), [0]);
  this.emptyObjectArray$2 = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [0]);
  return this
});
ScalaJS.c.s_Array$.prototype.slowcopy__p2__O__I__O__I__I__V = (function(src, srcPos, dest, destPos, length) {
  var i = srcPos;
  var j = destPos;
  var srcUntil = ((srcPos + length) | 0);
  while ((i < srcUntil)) {
    ScalaJS.m.sr_ScalaRunTime$().array$undupdate__O__I__O__V(dest, j, ScalaJS.m.sr_ScalaRunTime$().array$undapply__O__I__O(src, i));
    i = ((1 + i) | 0);
    j = ((1 + j) | 0)
  }
});
ScalaJS.c.s_Array$.prototype.copy__O__I__O__I__I__V = (function(src, srcPos, dest, destPos, length) {
  var srcClass = ScalaJS.objectGetClass(src);
  if ((srcClass.isArray__Z() && ScalaJS.objectGetClass(dest).isAssignableFrom__jl_Class__Z(srcClass))) {
    ScalaJS.systemArraycopy(src, srcPos, dest, destPos, length)
  } else {
    this.slowcopy__p2__O__I__O__I__I__V(src, srcPos, dest, destPos, length)
  }
});
ScalaJS.d.s_Array$ = new ScalaJS.ClassTypeData({
  s_Array$: 0
}, false, "scala.Array$", {
  s_Array$: 1,
  s_FallbackArrayBuilding: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_Array$.prototype.$classData = ScalaJS.d.s_Array$;
ScalaJS.n.s_Array$ = (void 0);
ScalaJS.m.s_Array$ = (function() {
  if ((!ScalaJS.n.s_Array$)) {
    ScalaJS.n.s_Array$ = new ScalaJS.c.s_Array$().init___()
  };
  return ScalaJS.n.s_Array$
});
/** @constructor */
ScalaJS.c.s_Predef$$eq$colon$eq = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_Predef$$eq$colon$eq.prototype = new ScalaJS.h.O();
ScalaJS.c.s_Predef$$eq$colon$eq.prototype.constructor = ScalaJS.c.s_Predef$$eq$colon$eq;
/** @constructor */
ScalaJS.h.s_Predef$$eq$colon$eq = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Predef$$eq$colon$eq.prototype = ScalaJS.c.s_Predef$$eq$colon$eq.prototype;
ScalaJS.c.s_Predef$$eq$colon$eq.prototype.init___ = (function() {
  return this
});
ScalaJS.c.s_Predef$$eq$colon$eq.prototype.toString__T = (function() {
  return "<function1>"
});
/** @constructor */
ScalaJS.c.s_Predef$$less$colon$less = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_Predef$$less$colon$less.prototype = new ScalaJS.h.O();
ScalaJS.c.s_Predef$$less$colon$less.prototype.constructor = ScalaJS.c.s_Predef$$less$colon$less;
/** @constructor */
ScalaJS.h.s_Predef$$less$colon$less = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Predef$$less$colon$less.prototype = ScalaJS.c.s_Predef$$less$colon$less.prototype;
ScalaJS.c.s_Predef$$less$colon$less.prototype.init___ = (function() {
  return this
});
ScalaJS.c.s_Predef$$less$colon$less.prototype.toString__T = (function() {
  return "<function1>"
});
/** @constructor */
ScalaJS.c.s_math_Equiv$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_math_Equiv$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_math_Equiv$.prototype.constructor = ScalaJS.c.s_math_Equiv$;
/** @constructor */
ScalaJS.h.s_math_Equiv$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_math_Equiv$.prototype = ScalaJS.c.s_math_Equiv$.prototype;
ScalaJS.c.s_math_Equiv$.prototype.init___ = (function() {
  ScalaJS.n.s_math_Equiv$ = this;
  return this
});
ScalaJS.d.s_math_Equiv$ = new ScalaJS.ClassTypeData({
  s_math_Equiv$: 0
}, false, "scala.math.Equiv$", {
  s_math_Equiv$: 1,
  O: 1,
  s_math_LowPriorityEquiv: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_math_Equiv$.prototype.$classData = ScalaJS.d.s_math_Equiv$;
ScalaJS.n.s_math_Equiv$ = (void 0);
ScalaJS.m.s_math_Equiv$ = (function() {
  if ((!ScalaJS.n.s_math_Equiv$)) {
    ScalaJS.n.s_math_Equiv$ = new ScalaJS.c.s_math_Equiv$().init___()
  };
  return ScalaJS.n.s_math_Equiv$
});
/** @constructor */
ScalaJS.c.s_math_Ordering$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_math_Ordering$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_math_Ordering$.prototype.constructor = ScalaJS.c.s_math_Ordering$;
/** @constructor */
ScalaJS.h.s_math_Ordering$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_math_Ordering$.prototype = ScalaJS.c.s_math_Ordering$.prototype;
ScalaJS.c.s_math_Ordering$.prototype.init___ = (function() {
  ScalaJS.n.s_math_Ordering$ = this;
  return this
});
ScalaJS.d.s_math_Ordering$ = new ScalaJS.ClassTypeData({
  s_math_Ordering$: 0
}, false, "scala.math.Ordering$", {
  s_math_Ordering$: 1,
  O: 1,
  s_math_LowPriorityOrderingImplicits: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_math_Ordering$.prototype.$classData = ScalaJS.d.s_math_Ordering$;
ScalaJS.n.s_math_Ordering$ = (void 0);
ScalaJS.m.s_math_Ordering$ = (function() {
  if ((!ScalaJS.n.s_math_Ordering$)) {
    ScalaJS.n.s_math_Ordering$ = new ScalaJS.c.s_math_Ordering$().init___()
  };
  return ScalaJS.n.s_math_Ordering$
});
/** @constructor */
ScalaJS.c.s_reflect_NoManifest$ = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_reflect_NoManifest$.prototype = new ScalaJS.h.O();
ScalaJS.c.s_reflect_NoManifest$.prototype.constructor = ScalaJS.c.s_reflect_NoManifest$;
/** @constructor */
ScalaJS.h.s_reflect_NoManifest$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_NoManifest$.prototype = ScalaJS.c.s_reflect_NoManifest$.prototype;
ScalaJS.c.s_reflect_NoManifest$.prototype.toString__T = (function() {
  return "<?>"
});
ScalaJS.d.s_reflect_NoManifest$ = new ScalaJS.ClassTypeData({
  s_reflect_NoManifest$: 0
}, false, "scala.reflect.NoManifest$", {
  s_reflect_NoManifest$: 1,
  O: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_reflect_NoManifest$.prototype.$classData = ScalaJS.d.s_reflect_NoManifest$;
ScalaJS.n.s_reflect_NoManifest$ = (void 0);
ScalaJS.m.s_reflect_NoManifest$ = (function() {
  if ((!ScalaJS.n.s_reflect_NoManifest$)) {
    ScalaJS.n.s_reflect_NoManifest$ = new ScalaJS.c.s_reflect_NoManifest$().init___()
  };
  return ScalaJS.n.s_reflect_NoManifest$
});
/** @constructor */
ScalaJS.c.sc_AbstractIterator = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sc_AbstractIterator.prototype = new ScalaJS.h.O();
ScalaJS.c.sc_AbstractIterator.prototype.constructor = ScalaJS.c.sc_AbstractIterator;
/** @constructor */
ScalaJS.h.sc_AbstractIterator = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_AbstractIterator.prototype = ScalaJS.c.sc_AbstractIterator.prototype;
ScalaJS.c.sc_AbstractIterator.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sc_AbstractIterator.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sc_AbstractIterator.prototype.toList__sci_List = (function() {
  var this$1 = ScalaJS.m.sci_List$();
  var cbf = this$1.ReusableCBFInstance$2;
  return ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableOnce$class__to__sc_TraversableOnce__scg_CanBuildFrom__O(this, cbf))
});
ScalaJS.c.sc_AbstractIterator.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_Iterator$class__isEmpty__sc_Iterator__Z(this)
});
ScalaJS.c.sc_AbstractIterator.prototype.toString__T = (function() {
  return ScalaJS.s.sc_Iterator$class__toString__sc_Iterator__T(this)
});
ScalaJS.c.sc_AbstractIterator.prototype.foreach__F1__V = (function(f) {
  ScalaJS.s.sc_Iterator$class__foreach__sc_Iterator__F1__V(this, f)
});
ScalaJS.c.sc_AbstractIterator.prototype.toVector__sci_Vector = (function() {
  ScalaJS.m.sci_Vector$();
  var cbf = ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6;
  return ScalaJS.as.sci_Vector(ScalaJS.s.sc_TraversableOnce$class__to__sc_TraversableOnce__scg_CanBuildFrom__O(this, cbf))
});
ScalaJS.c.sc_AbstractIterator.prototype.size__I = (function() {
  return ScalaJS.s.sc_TraversableOnce$class__size__sc_TraversableOnce__I(this)
});
ScalaJS.c.sc_AbstractIterator.prototype.toStream__sci_Stream = (function() {
  return ScalaJS.s.sc_Iterator$class__toStream__sc_Iterator__sci_Stream(this)
});
ScalaJS.c.sc_AbstractIterator.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return ScalaJS.s.sc_TraversableOnce$class__addString__sc_TraversableOnce__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end)
});
ScalaJS.c.sc_AbstractIterator.prototype.isTraversableAgain__Z = (function() {
  return false
});
/** @constructor */
ScalaJS.c.scg_SetFactory = (function() {
  ScalaJS.c.scg_GenSetFactory.call(this)
});
ScalaJS.c.scg_SetFactory.prototype = new ScalaJS.h.scg_GenSetFactory();
ScalaJS.c.scg_SetFactory.prototype.constructor = ScalaJS.c.scg_SetFactory;
/** @constructor */
ScalaJS.h.scg_SetFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_SetFactory.prototype = ScalaJS.c.scg_SetFactory.prototype;
/** @constructor */
ScalaJS.c.sci_ListSet$ListSetBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.elems$1 = null;
  this.seen$1 = null
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.constructor = ScalaJS.c.sci_ListSet$ListSetBuilder;
/** @constructor */
ScalaJS.h.sci_ListSet$ListSetBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListSet$ListSetBuilder.prototype = ScalaJS.c.sci_ListSet$ListSetBuilder.prototype;
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.result__sci_ListSet = (function() {
  var this$2 = this.elems$1;
  var z = ScalaJS.m.sci_ListSet$EmptyListSet$();
  var this$3 = this$2.scala$collection$mutable$ListBuffer$$start$6;
  var acc = z;
  var these = this$3;
  while ((!these.isEmpty__Z())) {
    var arg1 = acc;
    var arg2 = these.head__O();
    var x$1 = ScalaJS.as.sci_ListSet(arg1);
    acc = new ScalaJS.c.sci_ListSet$Node().init___sci_ListSet__O(x$1, arg2);
    these = ScalaJS.as.sc_LinearSeqOptimized(these.tail__O())
  };
  return ScalaJS.as.sci_ListSet(acc)
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.init___ = (function() {
  ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.init___sci_ListSet.call(this, ScalaJS.m.sci_ListSet$EmptyListSet$());
  return this
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__sci_ListSet$ListSetBuilder(elem)
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.init___sci_ListSet = (function(initial) {
  var this$1 = new ScalaJS.c.scm_ListBuffer().init___().$$plus$plus$eq__sc_TraversableOnce__scm_ListBuffer(initial);
  this.elems$1 = ScalaJS.as.scm_ListBuffer(ScalaJS.s.sc_SeqLike$class__reverse__sc_SeqLike__O(this$1));
  var this$2 = new ScalaJS.c.scm_HashSet().init___();
  this.seen$1 = ScalaJS.as.scm_HashSet(ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this$2, initial));
  return this
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.result__O = (function() {
  return this.result__sci_ListSet()
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__sci_ListSet$ListSetBuilder(elem)
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.$$plus$eq__O__sci_ListSet$ListSetBuilder = (function(x) {
  var this$1 = this.seen$1;
  if ((!ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z(this$1, x))) {
    this.elems$1.$$plus$eq__O__scm_ListBuffer(x);
    this.seen$1.$$plus$eq__O__scm_HashSet(x)
  };
  return this
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
ScalaJS.d.sci_ListSet$ListSetBuilder = new ScalaJS.ClassTypeData({
  sci_ListSet$ListSetBuilder: 0
}, false, "scala.collection.immutable.ListSet$ListSetBuilder", {
  sci_ListSet$ListSetBuilder: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1
});
ScalaJS.c.sci_ListSet$ListSetBuilder.prototype.$classData = ScalaJS.d.sci_ListSet$ListSetBuilder;
/** @constructor */
ScalaJS.c.sci_Map$ = (function() {
  ScalaJS.c.scg_ImmutableMapFactory.call(this)
});
ScalaJS.c.sci_Map$.prototype = new ScalaJS.h.scg_ImmutableMapFactory();
ScalaJS.c.sci_Map$.prototype.constructor = ScalaJS.c.sci_Map$;
/** @constructor */
ScalaJS.h.sci_Map$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Map$.prototype = ScalaJS.c.sci_Map$.prototype;
ScalaJS.c.sci_Map$.prototype.empty__sc_GenMap = (function() {
  return ScalaJS.m.sci_Map$EmptyMap$()
});
ScalaJS.d.sci_Map$ = new ScalaJS.ClassTypeData({
  sci_Map$: 0
}, false, "scala.collection.immutable.Map$", {
  sci_Map$: 1,
  scg_ImmutableMapFactory: 1,
  scg_MapFactory: 1,
  scg_GenMapFactory: 1,
  O: 1
});
ScalaJS.c.sci_Map$.prototype.$classData = ScalaJS.d.sci_Map$;
ScalaJS.n.sci_Map$ = (void 0);
ScalaJS.m.sci_Map$ = (function() {
  if ((!ScalaJS.n.sci_Map$)) {
    ScalaJS.n.sci_Map$ = new ScalaJS.c.sci_Map$().init___()
  };
  return ScalaJS.n.sci_Map$
});
/** @constructor */
ScalaJS.c.scm_DefaultEntry = (function() {
  ScalaJS.c.O.call(this);
  this.key$1 = null;
  this.value$1 = null;
  this.next$1 = null
});
ScalaJS.c.scm_DefaultEntry.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_DefaultEntry.prototype.constructor = ScalaJS.c.scm_DefaultEntry;
/** @constructor */
ScalaJS.h.scm_DefaultEntry = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_DefaultEntry.prototype = ScalaJS.c.scm_DefaultEntry.prototype;
ScalaJS.c.scm_DefaultEntry.prototype.chainString__T = (function() {
  var jsx$3 = this.key$1;
  var jsx$2 = this.value$1;
  if ((this.next$1 !== null)) {
    var this$1 = ScalaJS.as.scm_DefaultEntry(this.next$1);
    var jsx$1 = (" -> " + this$1.chainString__T())
  } else {
    var jsx$1 = ""
  };
  return ((((("(kv: " + jsx$3) + ", ") + jsx$2) + ")") + jsx$1)
});
ScalaJS.c.scm_DefaultEntry.prototype.init___O__O = (function(key, value) {
  this.key$1 = key;
  this.value$1 = value;
  return this
});
ScalaJS.c.scm_DefaultEntry.prototype.toString__T = (function() {
  return this.chainString__T()
});
ScalaJS.is.scm_DefaultEntry = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_DefaultEntry)))
});
ScalaJS.as.scm_DefaultEntry = (function(obj) {
  return ((ScalaJS.is.scm_DefaultEntry(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.DefaultEntry"))
});
ScalaJS.isArrayOf.scm_DefaultEntry = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_DefaultEntry)))
});
ScalaJS.asArrayOf.scm_DefaultEntry = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_DefaultEntry(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.DefaultEntry;", depth))
});
ScalaJS.d.scm_DefaultEntry = new ScalaJS.ClassTypeData({
  scm_DefaultEntry: 0
}, false, "scala.collection.mutable.DefaultEntry", {
  scm_DefaultEntry: 1,
  O: 1,
  scm_HashEntry: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_DefaultEntry.prototype.$classData = ScalaJS.d.scm_DefaultEntry;
/** @constructor */
ScalaJS.c.scm_GrowingBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.empty$1 = null;
  this.elems$1 = null
});
ScalaJS.c.scm_GrowingBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_GrowingBuilder.prototype.constructor = ScalaJS.c.scm_GrowingBuilder;
/** @constructor */
ScalaJS.h.scm_GrowingBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_GrowingBuilder.prototype = ScalaJS.c.scm_GrowingBuilder.prototype;
ScalaJS.c.scm_GrowingBuilder.prototype.init___scg_Growable = (function(empty) {
  this.empty$1 = empty;
  this.elems$1 = empty;
  return this
});
ScalaJS.c.scm_GrowingBuilder.prototype.$$plus$eq__O__scm_GrowingBuilder = (function(x) {
  this.elems$1.$$plus$eq__O__scg_Growable(x);
  return this
});
ScalaJS.c.scm_GrowingBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_GrowingBuilder(elem)
});
ScalaJS.c.scm_GrowingBuilder.prototype.result__O = (function() {
  return this.elems$1
});
ScalaJS.c.scm_GrowingBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_GrowingBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_GrowingBuilder(elem)
});
ScalaJS.c.scm_GrowingBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_GrowingBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
ScalaJS.d.scm_GrowingBuilder = new ScalaJS.ClassTypeData({
  scm_GrowingBuilder: 0
}, false, "scala.collection.mutable.GrowingBuilder", {
  scm_GrowingBuilder: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1
});
ScalaJS.c.scm_GrowingBuilder.prototype.$classData = ScalaJS.d.scm_GrowingBuilder;
/** @constructor */
ScalaJS.c.scm_LazyBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.parts$1 = null
});
ScalaJS.c.scm_LazyBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_LazyBuilder.prototype.constructor = ScalaJS.c.scm_LazyBuilder;
/** @constructor */
ScalaJS.h.scm_LazyBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_LazyBuilder.prototype = ScalaJS.c.scm_LazyBuilder.prototype;
ScalaJS.c.scm_LazyBuilder.prototype.init___ = (function() {
  this.parts$1 = new ScalaJS.c.scm_ListBuffer().init___();
  return this
});
ScalaJS.c.scm_LazyBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scm_LazyBuilder = (function(xs) {
  this.parts$1.$$plus$eq__O__scm_ListBuffer(xs);
  return this
});
ScalaJS.c.scm_LazyBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_LazyBuilder(elem)
});
ScalaJS.c.scm_LazyBuilder.prototype.$$plus$eq__O__scm_LazyBuilder = (function(x) {
  var jsx$1 = this.parts$1;
  ScalaJS.m.sci_List$();
  var xs = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([x]);
  var this$2 = ScalaJS.m.sci_List$();
  var cbf = this$2.ReusableCBFInstance$2;
  jsx$1.$$plus$eq__O__scm_ListBuffer(ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(xs, cbf)));
  return this
});
ScalaJS.c.scm_LazyBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_LazyBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_LazyBuilder(elem)
});
ScalaJS.c.scm_LazyBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_LazyBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return this.$$plus$plus$eq__sc_TraversableOnce__scm_LazyBuilder(xs)
});
/** @constructor */
ScalaJS.c.scm_MapBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.empty$1 = null;
  this.elems$1 = null
});
ScalaJS.c.scm_MapBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_MapBuilder.prototype.constructor = ScalaJS.c.scm_MapBuilder;
/** @constructor */
ScalaJS.h.scm_MapBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_MapBuilder.prototype = ScalaJS.c.scm_MapBuilder.prototype;
ScalaJS.c.scm_MapBuilder.prototype.$$plus$eq__T2__scm_MapBuilder = (function(x) {
  this.elems$1 = this.elems$1.$$plus__T2__sc_GenMap(x);
  return this
});
ScalaJS.c.scm_MapBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__T2__scm_MapBuilder(ScalaJS.as.T2(elem))
});
ScalaJS.c.scm_MapBuilder.prototype.result__O = (function() {
  return this.elems$1
});
ScalaJS.c.scm_MapBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_MapBuilder.prototype.init___sc_GenMap = (function(empty) {
  this.empty$1 = empty;
  this.elems$1 = empty;
  return this
});
ScalaJS.c.scm_MapBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__T2__scm_MapBuilder(ScalaJS.as.T2(elem))
});
ScalaJS.c.scm_MapBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_MapBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
ScalaJS.d.scm_MapBuilder = new ScalaJS.ClassTypeData({
  scm_MapBuilder: 0
}, false, "scala.collection.mutable.MapBuilder", {
  scm_MapBuilder: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1
});
ScalaJS.c.scm_MapBuilder.prototype.$classData = ScalaJS.d.scm_MapBuilder;
/** @constructor */
ScalaJS.c.scm_SetBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.empty$1 = null;
  this.elems$1 = null
});
ScalaJS.c.scm_SetBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_SetBuilder.prototype.constructor = ScalaJS.c.scm_SetBuilder;
/** @constructor */
ScalaJS.h.scm_SetBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_SetBuilder.prototype = ScalaJS.c.scm_SetBuilder.prototype;
ScalaJS.c.scm_SetBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_SetBuilder(elem)
});
ScalaJS.c.scm_SetBuilder.prototype.result__O = (function() {
  return this.elems$1
});
ScalaJS.c.scm_SetBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_SetBuilder.prototype.$$plus$eq__O__scm_SetBuilder = (function(x) {
  this.elems$1 = this.elems$1.$$plus__O__sc_Set(x);
  return this
});
ScalaJS.c.scm_SetBuilder.prototype.init___sc_Set = (function(empty) {
  this.empty$1 = empty;
  this.elems$1 = empty;
  return this
});
ScalaJS.c.scm_SetBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_SetBuilder(elem)
});
ScalaJS.c.scm_SetBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_SetBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
ScalaJS.d.scm_SetBuilder = new ScalaJS.ClassTypeData({
  scm_SetBuilder: 0
}, false, "scala.collection.mutable.SetBuilder", {
  scm_SetBuilder: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1
});
ScalaJS.c.scm_SetBuilder.prototype.$classData = ScalaJS.d.scm_SetBuilder;
/** @constructor */
ScalaJS.c.scm_Stack$StackBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.lbuff$1 = null
});
ScalaJS.c.scm_Stack$StackBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_Stack$StackBuilder.prototype.constructor = ScalaJS.c.scm_Stack$StackBuilder;
/** @constructor */
ScalaJS.h.scm_Stack$StackBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_Stack$StackBuilder.prototype = ScalaJS.c.scm_Stack$StackBuilder.prototype;
ScalaJS.c.scm_Stack$StackBuilder.prototype.init___ = (function() {
  this.lbuff$1 = new ScalaJS.c.scm_ListBuffer().init___();
  return this
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_Stack$StackBuilder(elem)
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.result__O = (function() {
  return this.result__scm_Stack()
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_Stack$StackBuilder(elem)
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.result__scm_Stack = (function() {
  var this$1 = this.lbuff$1;
  return new ScalaJS.c.scm_Stack().init___sci_List(this$1.toList__sci_List())
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.$$plus$eq__O__scm_Stack$StackBuilder = (function(elem) {
  this.lbuff$1.$$plus$eq__O__scm_ListBuffer(elem);
  return this
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
ScalaJS.d.scm_Stack$StackBuilder = new ScalaJS.ClassTypeData({
  scm_Stack$StackBuilder: 0
}, false, "scala.collection.mutable.Stack$StackBuilder", {
  scm_Stack$StackBuilder: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1
});
ScalaJS.c.scm_Stack$StackBuilder.prototype.$classData = ScalaJS.d.scm_Stack$StackBuilder;
/** @constructor */
ScalaJS.c.sr_AbstractFunction1$mcVI$sp = (function() {
  ScalaJS.c.sr_AbstractFunction1.call(this)
});
ScalaJS.c.sr_AbstractFunction1$mcVI$sp.prototype = new ScalaJS.h.sr_AbstractFunction1();
ScalaJS.c.sr_AbstractFunction1$mcVI$sp.prototype.constructor = ScalaJS.c.sr_AbstractFunction1$mcVI$sp;
/** @constructor */
ScalaJS.h.sr_AbstractFunction1$mcVI$sp = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_AbstractFunction1$mcVI$sp.prototype = ScalaJS.c.sr_AbstractFunction1$mcVI$sp.prototype;
/** @constructor */
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2 = (function() {
  ScalaJS.c.sr_AbstractFunction1.call(this);
  this.$$outer$2 = null;
  this.overlap$1$2 = 0;
  this.rooms$1$2 = null
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype = new ScalaJS.h.sr_AbstractFunction1();
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype.constructor = ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2;
/** @constructor */
ScalaJS.h.Lgame_Game$$anonfun$genFarmhouse$2 = (function() {
  /*<skip>*/
});
ScalaJS.h.Lgame_Game$$anonfun$genFarmhouse$2.prototype = ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype;
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype.apply__I__scm_Set = (function(i) {
  var x1 = ScalaJS.as.T2(this.rooms$1$2.apply__I__O(i));
  if ((x1 !== null)) {
    var room = ScalaJS.as.Lobjects_Wall(x1.$$und1__O());
    var relation = ScalaJS.as.T(x1.$$und2__O());
    var x$8_$_$$und1$f = room;
    var x$8_$_$$und2$f = relation
  } else {
    var x$8;
    throw new ScalaJS.c.s_MatchError().init___O(x1)
  };
  var room$2 = ScalaJS.as.Lobjects_Wall(x$8_$_$$und1$f);
  var relation$2 = ScalaJS.as.T(x$8_$_$$und2$f);
  var this$1 = this.$$outer$2.r$1;
  var n = (((((room$2.size$1.x$1 | 0) - ScalaJS.imul(2, this.overlap$1$2)) | 0) - ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1) | 0);
  var x = (((this$1.self$1.nextInt__I__I(n) + this.overlap$1$2) | 0) + room$2.loc$1.x$1);
  var this$2 = this.$$outer$2.r$1;
  var n$1 = (((((room$2.size$1.y$1 | 0) - ScalaJS.imul(2, this.overlap$1$2)) | 0) - ScalaJS.m.Lglobalvars_GV$().NORMUNITSIZE$1) | 0);
  var y = (((this$2.self$1.nextInt__I__I(n$1) + this.overlap$1$2) | 0) + room$2.loc$1.y$1);
  var this$3 = this.$$outer$2.r$1;
  var n$2 = ScalaJS.m.Lglobalvars_GV$().ITEM$undCHANCE$1;
  if ((this$3.self$1.nextInt__I__I(n$2) === 0)) {
    var newItem = new ScalaJS.c.Lobjects_DummyItem().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(x, y));
    this.$$outer$2.addActor__Lobjects_Actor__scm_Set(newItem)
  } else {
    var newZomb = new ScalaJS.c.Lenemies_Zombie().init___Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(x, y));
    this.$$outer$2.addActor__Lobjects_Actor__scm_Set(newZomb)
  };
  var nrelation = "";
  if ((i !== (((-1) + this.rooms$1$2.length__I()) | 0))) {
    nrelation = ScalaJS.as.T(ScalaJS.as.T2(this.rooms$1$2.apply__I__O(((1 + i) | 0))).$$und2__O())
  };
  var north = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(room$2.loc$1.cloone__Lglobalvars_Pt(), new ScalaJS.c.Lglobalvars_Pt().init___D__D(room$2.size$1.x$1, this.overlap$1$2));
  var west = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(room$2.loc$1.cloone__Lglobalvars_Pt(), new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.overlap$1$2, room$2.size$1.y$1));
  var south = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(room$2.loc$1.x$1, ((room$2.loc$1.y$1 + room$2.size$1.y$1) - this.overlap$1$2)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(room$2.size$1.x$1, this.overlap$1$2));
  var east = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(((room$2.loc$1.x$1 + room$2.size$1.x$1) - this.overlap$1$2), room$2.loc$1.y$1), new ScalaJS.c.Lglobalvars_Pt().init___D__D(this.overlap$1$2, room$2.size$1.y$1));
  var array = [north, east, south, west];
  if ((ScalaJS.uI(array["length"]) === 0)) {
    var jsx$1 = new ScalaJS.c.scm_HashSet().init___()
  } else {
    var b = new ScalaJS.c.scm_GrowingBuilder().init___scg_Growable(new ScalaJS.c.scm_HashSet().init___());
    matchEnd4: {
      var i$1 = 0;
      var len = ScalaJS.uI(array["length"]);
      while ((i$1 < len)) {
        var index = i$1;
        var arg1 = array[index];
        b.$$plus$eq__O__scm_GrowingBuilder(arg1);
        i$1 = ((1 + i$1) | 0)
      };
      break matchEnd4
    };
    var jsx$1 = ScalaJS.as.sc_GenTraversable(b.elems$1)
  };
  var walls = ScalaJS.as.scm_Set(jsx$1);
  var dir = "";
  if (((i === 0) || (i === (((-1) + this.rooms$1$2.length__I()) | 0)))) {
    var array$1 = ["north", "east", "south", "west"];
    if ((ScalaJS.uI(array$1["length"]) === 0)) {
      var jsx$2 = new ScalaJS.c.scm_HashSet().init___()
    } else {
      var b$1 = new ScalaJS.c.scm_GrowingBuilder().init___scg_Growable(new ScalaJS.c.scm_HashSet().init___());
      matchEnd4$1: {
        var i$2 = 0;
        var len$1 = ScalaJS.uI(array$1["length"]);
        while ((i$2 < len$1)) {
          var index$1 = i$2;
          var arg1$1 = array$1[index$1];
          b$1.$$plus$eq__O__scm_GrowingBuilder(arg1$1);
          i$2 = ((1 + i$2) | 0)
        };
        break matchEnd4$1
      };
      var jsx$2 = ScalaJS.as.sc_GenTraversable(b$1.elems$1)
    };
    var relations = ScalaJS.as.scm_Set(jsx$2);
    ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(relations, relation$2);
    if ((nrelation === "west")) {
      ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(relations, "east")
    };
    if ((nrelation === "north")) {
      ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(relations, "south")
    };
    if ((nrelation === "south")) {
      ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(relations, "north")
    };
    if ((nrelation === "east")) {
      ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(relations, "west")
    };
    var this$10 = ScalaJS.m.sci_List$();
    var cbf = this$10.ReusableCBFInstance$2;
    var this$12 = ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(relations, cbf));
    var this$11 = this.$$outer$2.r$1;
    var n$3 = this$11.self$1.nextInt__I__I(2);
    dir = ScalaJS.as.T(ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this$12, n$3))
  };
  if (((relation$2 === "west") || (dir === "west"))) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(west, walls)
  };
  if (((relation$2 === "north") || (dir === "north"))) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(north, walls)
  };
  if (((relation$2 === "south") || (dir === "south"))) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(south, walls)
  };
  if (((relation$2 === "east") || (dir === "east"))) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(east, walls)
  };
  if ((nrelation === "west")) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(east, walls)
  };
  if ((nrelation === "north")) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(south, walls)
  };
  if ((nrelation === "south")) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(north, walls)
  };
  if ((nrelation === "east")) {
    this.carveWall$1__p2__Lobjects_Wall__scm_Set__Z(west, walls)
  };
  var f = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer) {
    return (function(x$10$2) {
      var x$10 = ScalaJS.as.Lobjects_Wall(x$10$2);
      var this$13 = arg$outer.$$outer$2;
      var this$14 = this$13.objs$1;
      return this$14.$$plus$eq__O__scm_HashSet(x$10)
    })
  })(this));
  var this$15 = ScalaJS.m.scm_Set$();
  var bf = new ScalaJS.c.scg_GenSetFactory$$anon$1().init___scg_GenSetFactory(this$15);
  return ScalaJS.as.scm_Set(ScalaJS.s.sc_TraversableLike$class__map__sc_TraversableLike__F1__scg_CanBuildFrom__O(walls, f, bf))
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype.carveWall$1__p2__Lobjects_Wall__scm_Set__Z = (function(w, walls$1) {
  var x1 = w.splitWithDoorAt__I__I__T2(0, 20);
  if ((x1 !== null)) {
    var w1 = ScalaJS.as.Lobjects_Wall(x1.$$und1__O());
    var w2 = ScalaJS.as.Lobjects_Wall(x1.$$und2__O());
    var x$9_$_$$und1$f = w1;
    var x$9_$_$$und2$f = w2
  } else {
    var x$9;
    throw new ScalaJS.c.s_MatchError().init___O(x1)
  };
  var w1$2 = ScalaJS.as.Lobjects_Wall(x$9_$_$$und1$f);
  var w2$2 = ScalaJS.as.Lobjects_Wall(x$9_$_$$und2$f);
  ScalaJS.s.scm_FlatHashTable$class__removeElem__scm_FlatHashTable__O__Z(walls$1, w);
  ScalaJS.s.scm_FlatHashTable$class__addElem__scm_FlatHashTable__O__Z(walls$1, w1$2);
  return ScalaJS.s.scm_FlatHashTable$class__addElem__scm_FlatHashTable__O__Z(walls$1, w2$2)
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype.apply__O__O = (function(v1) {
  return this.apply__I__scm_Set(ScalaJS.uI(v1))
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype.init___Lgame_Game__I__scm_Buffer = (function($$outer, overlap$1, rooms$1) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  this.overlap$1$2 = overlap$1;
  this.rooms$1$2 = rooms$1;
  return this
});
ScalaJS.d.Lgame_Game$$anonfun$genFarmhouse$2 = new ScalaJS.ClassTypeData({
  Lgame_Game$$anonfun$genFarmhouse$2: 0
}, false, "game.Game$$anonfun$genFarmhouse$2", {
  Lgame_Game$$anonfun$genFarmhouse$2: 1,
  sr_AbstractFunction1: 1,
  O: 1,
  F1: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$2.prototype.$classData = ScalaJS.d.Lgame_Game$$anonfun$genFarmhouse$2;
/** @constructor */
ScalaJS.c.Lglobalvars_SimpleLine = (function() {
  ScalaJS.c.O.call(this);
  this.start$1 = null;
  this.end$1 = null;
  this.color$1 = null
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype = new ScalaJS.h.O();
ScalaJS.c.Lglobalvars_SimpleLine.prototype.constructor = ScalaJS.c.Lglobalvars_SimpleLine;
/** @constructor */
ScalaJS.h.Lglobalvars_SimpleLine = (function() {
  /*<skip>*/
});
ScalaJS.h.Lglobalvars_SimpleLine.prototype = ScalaJS.c.Lglobalvars_SimpleLine.prototype;
ScalaJS.c.Lglobalvars_SimpleLine.prototype.productPrefix__T = (function() {
  return "SimpleLine"
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.productArity__I = (function() {
  return 3
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if (ScalaJS.is.Lglobalvars_SimpleLine(x$1)) {
    var SimpleLine$1 = ScalaJS.as.Lglobalvars_SimpleLine(x$1);
    var x = this.start$1;
    var x$2 = SimpleLine$1.start$1;
    if ((x === x$2)) {
      var x$3 = this.end$1;
      var x$4 = SimpleLine$1.end$1;
      var jsx$1 = (x$3 === x$4)
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      return (this.color$1 === SimpleLine$1.color$1)
    } else {
      return false
    }
  } else {
    return false
  }
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0:
      {
        return this.start$1;
        break
      };
    case 1:
      {
        return this.end$1;
        break
      };
    case 2:
      {
        return this.color$1;
        break
      };
    default:
      throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + x$1));
  }
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.length__D = (function() {
  var x = (this.start$1.x$1 - this.end$1.x$1);
  var jsx$1 = ScalaJS.uD(ScalaJS.g["Math"]["pow"](x, 2.0));
  var x$1 = (this.start$1.y$1 - this.end$1.y$1);
  var x$2 = (jsx$1 + ScalaJS.uD(ScalaJS.g["Math"]["pow"](x$1, 2.0)));
  var dist = ScalaJS.uD(ScalaJS.g["Math"]["sqrt"](x$2));
  return dist
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.toString__T = (function() {
  return ScalaJS.m.sr_ScalaRunTime$().$$undtoString__s_Product__T(this)
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.unitStep__Lglobalvars_Pt = (function() {
  var ux = ((this.end$1.x$1 - this.start$1.x$1) / this.length__D());
  var uy = ((this.end$1.y$1 - this.start$1.y$1) / this.length__D());
  return new ScalaJS.c.Lglobalvars_Pt().init___D__D(ux, uy)
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.hashCode__I = (function() {
  var this$2 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.init___Lglobalvars_Pt__Lglobalvars_Pt__T = (function(start, end, color) {
  this.start$1 = start;
  this.end$1 = end;
  this.color$1 = color;
  return this
});
ScalaJS.is.Lglobalvars_SimpleLine = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lglobalvars_SimpleLine)))
});
ScalaJS.as.Lglobalvars_SimpleLine = (function(obj) {
  return ((ScalaJS.is.Lglobalvars_SimpleLine(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "globalvars.SimpleLine"))
});
ScalaJS.isArrayOf.Lglobalvars_SimpleLine = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lglobalvars_SimpleLine)))
});
ScalaJS.asArrayOf.Lglobalvars_SimpleLine = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Lglobalvars_SimpleLine(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lglobalvars.SimpleLine;", depth))
});
ScalaJS.d.Lglobalvars_SimpleLine = new ScalaJS.ClassTypeData({
  Lglobalvars_SimpleLine: 0
}, false, "globalvars.SimpleLine", {
  Lglobalvars_SimpleLine: 1,
  O: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.Lglobalvars_SimpleLine.prototype.$classData = ScalaJS.d.Lglobalvars_SimpleLine;
/** @constructor */
ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4 = (function() {
  ScalaJS.c.sr_AbstractFunction1.call(this);
  this.$$outer$2 = null;
  this.g$5$f = null
});
ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4.prototype = new ScalaJS.h.sr_AbstractFunction1();
ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4.prototype.constructor = ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4;
/** @constructor */
ScalaJS.h.Lobjects_LandMine$$anonfun$aiMove$4 = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_LandMine$$anonfun$aiMove$4.prototype = ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4.prototype;
ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4.prototype.apply__O__O = (function(v1) {
  return this.apply__Lobjects_Actor__O(ScalaJS.as.Lobjects_Actor(v1))
});
ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4.prototype.init___Lobjects_LandMine__Lgame_Game = (function($$outer, g$5) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  this.g$5$f = g$5;
  return this
});
ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4.prototype.apply__Lobjects_Actor__O = (function(a) {
  if ((a.faction$2 !== "NA")) {
    var this$1 = this.$$outer$2;
    var jsx$1 = this$1.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(a.loc$1, a.size$1)
  } else {
    var jsx$1 = false
  };
  if (jsx$1) {
    this.g$5$f.acts$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer) {
      return (function(a$2) {
        var a$1 = ScalaJS.as.Lobjects_Actor(a$2);
        if ((arg$outer.$$outer$2.distanceTo__Lobjects_Obj__I(a$1) < arg$outer.$$outer$2.radius$3)) {
          a$1.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(arg$outer.$$outer$2, arg$outer.$$outer$2.damage$3, 1.0, arg$outer.g$5$f)
        }
      })
    })(this)));
    return this.g$5$f.removeActor__Lobjects_Actor__s_Option(this.$$outer$2)
  } else {
    return (void 0)
  }
});
ScalaJS.d.Lobjects_LandMine$$anonfun$aiMove$4 = new ScalaJS.ClassTypeData({
  Lobjects_LandMine$$anonfun$aiMove$4: 0
}, false, "objects.LandMine$$anonfun$aiMove$4", {
  Lobjects_LandMine$$anonfun$aiMove$4: 1,
  sr_AbstractFunction1: 1,
  O: 1,
  F1: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.Lobjects_LandMine$$anonfun$aiMove$4.prototype.$classData = ScalaJS.d.Lobjects_LandMine$$anonfun$aiMove$4;
/** @constructor */
ScalaJS.c.Lobjects_MediumAmmoPack = (function() {
  ScalaJS.c.Lobjects_AmmoPack.call(this)
});
ScalaJS.c.Lobjects_MediumAmmoPack.prototype = new ScalaJS.h.Lobjects_AmmoPack();
ScalaJS.c.Lobjects_MediumAmmoPack.prototype.constructor = ScalaJS.c.Lobjects_MediumAmmoPack;
/** @constructor */
ScalaJS.h.Lobjects_MediumAmmoPack = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_MediumAmmoPack.prototype = ScalaJS.c.Lobjects_MediumAmmoPack.prototype;
ScalaJS.c.Lobjects_MediumAmmoPack.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_AmmoPack.prototype.init___Lglobalvars_Pt__I.call(this, loc_, ScalaJS.m.Lglobalvars_GV$().AMMOPACK$undAMOUNT$1);
  return this
});
ScalaJS.d.Lobjects_MediumAmmoPack = new ScalaJS.ClassTypeData({
  Lobjects_MediumAmmoPack: 0
}, false, "objects.MediumAmmoPack", {
  Lobjects_MediumAmmoPack: 1,
  Lobjects_AmmoPack: 1,
  Lobjects_Item: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_MediumAmmoPack.prototype.$classData = ScalaJS.d.Lobjects_MediumAmmoPack;
/** @constructor */
ScalaJS.c.Lobjects_MediumHealthPack = (function() {
  ScalaJS.c.Lobjects_HealthPack.call(this)
});
ScalaJS.c.Lobjects_MediumHealthPack.prototype = new ScalaJS.h.Lobjects_HealthPack();
ScalaJS.c.Lobjects_MediumHealthPack.prototype.constructor = ScalaJS.c.Lobjects_MediumHealthPack;
/** @constructor */
ScalaJS.h.Lobjects_MediumHealthPack = (function() {
  /*<skip>*/
});
ScalaJS.h.Lobjects_MediumHealthPack.prototype = ScalaJS.c.Lobjects_MediumHealthPack.prototype;
ScalaJS.c.Lobjects_MediumHealthPack.prototype.init___Lglobalvars_Pt = (function(loc_) {
  ScalaJS.c.Lobjects_HealthPack.prototype.init___Lglobalvars_Pt__I.call(this, loc_, ScalaJS.m.Lglobalvars_GV$().HEALTHPACK$undAMOUNT$1);
  return this
});
ScalaJS.d.Lobjects_MediumHealthPack = new ScalaJS.ClassTypeData({
  Lobjects_MediumHealthPack: 0
}, false, "objects.MediumHealthPack", {
  Lobjects_MediumHealthPack: 1,
  Lobjects_HealthPack: 1,
  Lobjects_Item: 1,
  Lobjects_Actor: 1,
  Lobjects_Obj: 1,
  O: 1
});
ScalaJS.c.Lobjects_MediumHealthPack.prototype.$classData = ScalaJS.d.Lobjects_MediumHealthPack;
/** @constructor */
ScalaJS.c.jl_ArithmeticException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.jl_ArithmeticException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.jl_ArithmeticException.prototype.constructor = ScalaJS.c.jl_ArithmeticException;
/** @constructor */
ScalaJS.h.jl_ArithmeticException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_ArithmeticException.prototype = ScalaJS.c.jl_ArithmeticException.prototype;
ScalaJS.d.jl_ArithmeticException = new ScalaJS.ClassTypeData({
  jl_ArithmeticException: 0
}, false, "java.lang.ArithmeticException", {
  jl_ArithmeticException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_ArithmeticException.prototype.$classData = ScalaJS.d.jl_ArithmeticException;
/** @constructor */
ScalaJS.c.jl_ClassCastException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.jl_ClassCastException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.jl_ClassCastException.prototype.constructor = ScalaJS.c.jl_ClassCastException;
/** @constructor */
ScalaJS.h.jl_ClassCastException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_ClassCastException.prototype = ScalaJS.c.jl_ClassCastException.prototype;
ScalaJS.is.jl_ClassCastException = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.jl_ClassCastException)))
});
ScalaJS.as.jl_ClassCastException = (function(obj) {
  return ((ScalaJS.is.jl_ClassCastException(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.lang.ClassCastException"))
});
ScalaJS.isArrayOf.jl_ClassCastException = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_ClassCastException)))
});
ScalaJS.asArrayOf.jl_ClassCastException = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.jl_ClassCastException(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.lang.ClassCastException;", depth))
});
ScalaJS.d.jl_ClassCastException = new ScalaJS.ClassTypeData({
  jl_ClassCastException: 0
}, false, "java.lang.ClassCastException", {
  jl_ClassCastException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_ClassCastException.prototype.$classData = ScalaJS.d.jl_ClassCastException;
/** @constructor */
ScalaJS.c.jl_IllegalArgumentException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.jl_IllegalArgumentException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.jl_IllegalArgumentException.prototype.constructor = ScalaJS.c.jl_IllegalArgumentException;
/** @constructor */
ScalaJS.h.jl_IllegalArgumentException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_IllegalArgumentException.prototype = ScalaJS.c.jl_IllegalArgumentException.prototype;
ScalaJS.c.jl_IllegalArgumentException.prototype.init___ = (function() {
  ScalaJS.c.jl_IllegalArgumentException.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
ScalaJS.c.jl_IllegalArgumentException.prototype.init___T = (function(s) {
  ScalaJS.c.jl_IllegalArgumentException.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
ScalaJS.d.jl_IllegalArgumentException = new ScalaJS.ClassTypeData({
  jl_IllegalArgumentException: 0
}, false, "java.lang.IllegalArgumentException", {
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_IllegalArgumentException.prototype.$classData = ScalaJS.d.jl_IllegalArgumentException;
/** @constructor */
ScalaJS.c.jl_IllegalStateException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.jl_IllegalStateException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.jl_IllegalStateException.prototype.constructor = ScalaJS.c.jl_IllegalStateException;
/** @constructor */
ScalaJS.h.jl_IllegalStateException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_IllegalStateException.prototype = ScalaJS.c.jl_IllegalStateException.prototype;
ScalaJS.c.jl_IllegalStateException.prototype.init___ = (function() {
  ScalaJS.c.jl_IllegalStateException.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
/** @constructor */
ScalaJS.c.jl_IndexOutOfBoundsException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.jl_IndexOutOfBoundsException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.jl_IndexOutOfBoundsException.prototype.constructor = ScalaJS.c.jl_IndexOutOfBoundsException;
/** @constructor */
ScalaJS.h.jl_IndexOutOfBoundsException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_IndexOutOfBoundsException.prototype = ScalaJS.c.jl_IndexOutOfBoundsException.prototype;
ScalaJS.d.jl_IndexOutOfBoundsException = new ScalaJS.ClassTypeData({
  jl_IndexOutOfBoundsException: 0
}, false, "java.lang.IndexOutOfBoundsException", {
  jl_IndexOutOfBoundsException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_IndexOutOfBoundsException.prototype.$classData = ScalaJS.d.jl_IndexOutOfBoundsException;
/** @constructor */
ScalaJS.c.jl_NullPointerException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.jl_NullPointerException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.jl_NullPointerException.prototype.constructor = ScalaJS.c.jl_NullPointerException;
/** @constructor */
ScalaJS.h.jl_NullPointerException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_NullPointerException.prototype = ScalaJS.c.jl_NullPointerException.prototype;
ScalaJS.c.jl_NullPointerException.prototype.init___ = (function() {
  ScalaJS.c.jl_NullPointerException.prototype.init___T.call(this, null);
  return this
});
ScalaJS.d.jl_NullPointerException = new ScalaJS.ClassTypeData({
  jl_NullPointerException: 0
}, false, "java.lang.NullPointerException", {
  jl_NullPointerException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_NullPointerException.prototype.$classData = ScalaJS.d.jl_NullPointerException;
/** @constructor */
ScalaJS.c.jl_UnsupportedOperationException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.jl_UnsupportedOperationException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.jl_UnsupportedOperationException.prototype.constructor = ScalaJS.c.jl_UnsupportedOperationException;
/** @constructor */
ScalaJS.h.jl_UnsupportedOperationException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_UnsupportedOperationException.prototype = ScalaJS.c.jl_UnsupportedOperationException.prototype;
ScalaJS.c.jl_UnsupportedOperationException.prototype.init___T = (function(s) {
  ScalaJS.c.jl_UnsupportedOperationException.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
ScalaJS.d.jl_UnsupportedOperationException = new ScalaJS.ClassTypeData({
  jl_UnsupportedOperationException: 0
}, false, "java.lang.UnsupportedOperationException", {
  jl_UnsupportedOperationException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_UnsupportedOperationException.prototype.$classData = ScalaJS.d.jl_UnsupportedOperationException;
/** @constructor */
ScalaJS.c.ju_NoSuchElementException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this)
});
ScalaJS.c.ju_NoSuchElementException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.ju_NoSuchElementException.prototype.constructor = ScalaJS.c.ju_NoSuchElementException;
/** @constructor */
ScalaJS.h.ju_NoSuchElementException = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_NoSuchElementException.prototype = ScalaJS.c.ju_NoSuchElementException.prototype;
ScalaJS.c.ju_NoSuchElementException.prototype.init___ = (function() {
  ScalaJS.c.ju_NoSuchElementException.prototype.init___T.call(this, null);
  return this
});
ScalaJS.d.ju_NoSuchElementException = new ScalaJS.ClassTypeData({
  ju_NoSuchElementException: 0
}, false, "java.util.NoSuchElementException", {
  ju_NoSuchElementException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.ju_NoSuchElementException.prototype.$classData = ScalaJS.d.ju_NoSuchElementException;
/** @constructor */
ScalaJS.c.s_MatchError = (function() {
  ScalaJS.c.jl_RuntimeException.call(this);
  this.obj$4 = null;
  this.objString$4 = null;
  this.bitmap$0$4 = false
});
ScalaJS.c.s_MatchError.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.s_MatchError.prototype.constructor = ScalaJS.c.s_MatchError;
/** @constructor */
ScalaJS.h.s_MatchError = (function() {
  /*<skip>*/
});
ScalaJS.h.s_MatchError.prototype = ScalaJS.c.s_MatchError.prototype;
ScalaJS.c.s_MatchError.prototype.objString$lzycompute__p4__T = (function() {
  if ((!this.bitmap$0$4)) {
    this.objString$4 = ((this.obj$4 === null) ? "null" : this.liftedTree1$1__p4__T());
    this.bitmap$0$4 = true
  };
  return this.objString$4
});
ScalaJS.c.s_MatchError.prototype.ofClass$1__p4__T = (function() {
  return ("of class " + ScalaJS.objectGetClass(this.obj$4).getName__T())
});
ScalaJS.c.s_MatchError.prototype.liftedTree1$1__p4__T = (function() {
  try {
    return (((ScalaJS.objectToString(this.obj$4) + " (") + this.ofClass$1__p4__T()) + ")")
  } catch (e) {
    var e$2 = ScalaJS.m.sjsr_package$().wrapJavaScriptException__O__jl_Throwable(e);
    if ((e$2 !== null)) {
      return ("an instance " + this.ofClass$1__p4__T())
    } else {
      throw e
    }
  }
});
ScalaJS.c.s_MatchError.prototype.getMessage__T = (function() {
  return this.objString__p4__T()
});
ScalaJS.c.s_MatchError.prototype.objString__p4__T = (function() {
  return ((!this.bitmap$0$4) ? this.objString$lzycompute__p4__T() : this.objString$4)
});
ScalaJS.c.s_MatchError.prototype.init___O = (function(obj) {
  this.obj$4 = obj;
  ScalaJS.c.jl_RuntimeException.prototype.init___.call(this);
  return this
});
ScalaJS.d.s_MatchError = new ScalaJS.ClassTypeData({
  s_MatchError: 0
}, false, "scala.MatchError", {
  s_MatchError: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_MatchError.prototype.$classData = ScalaJS.d.s_MatchError;
/** @constructor */
ScalaJS.c.s_Option = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.s_Option.prototype = new ScalaJS.h.O();
ScalaJS.c.s_Option.prototype.constructor = ScalaJS.c.s_Option;
/** @constructor */
ScalaJS.h.s_Option = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Option.prototype = ScalaJS.c.s_Option.prototype;
ScalaJS.c.s_Option.prototype.init___ = (function() {
  return this
});
ScalaJS.c.s_Option.prototype.isDefined__Z = (function() {
  return (!this.isEmpty__Z())
});
/** @constructor */
ScalaJS.c.s_Predef$$anon$1 = (function() {
  ScalaJS.c.s_Predef$$less$colon$less.call(this)
});
ScalaJS.c.s_Predef$$anon$1.prototype = new ScalaJS.h.s_Predef$$less$colon$less();
ScalaJS.c.s_Predef$$anon$1.prototype.constructor = ScalaJS.c.s_Predef$$anon$1;
/** @constructor */
ScalaJS.h.s_Predef$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Predef$$anon$1.prototype = ScalaJS.c.s_Predef$$anon$1.prototype;
ScalaJS.c.s_Predef$$anon$1.prototype.apply__O__O = (function(x) {
  return x
});
ScalaJS.d.s_Predef$$anon$1 = new ScalaJS.ClassTypeData({
  s_Predef$$anon$1: 0
}, false, "scala.Predef$$anon$1", {
  s_Predef$$anon$1: 1,
  s_Predef$$less$colon$less: 1,
  O: 1,
  F1: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_Predef$$anon$1.prototype.$classData = ScalaJS.d.s_Predef$$anon$1;
/** @constructor */
ScalaJS.c.s_Predef$$anon$2 = (function() {
  ScalaJS.c.s_Predef$$eq$colon$eq.call(this)
});
ScalaJS.c.s_Predef$$anon$2.prototype = new ScalaJS.h.s_Predef$$eq$colon$eq();
ScalaJS.c.s_Predef$$anon$2.prototype.constructor = ScalaJS.c.s_Predef$$anon$2;
/** @constructor */
ScalaJS.h.s_Predef$$anon$2 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Predef$$anon$2.prototype = ScalaJS.c.s_Predef$$anon$2.prototype;
ScalaJS.c.s_Predef$$anon$2.prototype.apply__O__O = (function(x) {
  return x
});
ScalaJS.d.s_Predef$$anon$2 = new ScalaJS.ClassTypeData({
  s_Predef$$anon$2: 0
}, false, "scala.Predef$$anon$2", {
  s_Predef$$anon$2: 1,
  s_Predef$$eq$colon$eq: 1,
  O: 1,
  F1: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_Predef$$anon$2.prototype.$classData = ScalaJS.d.s_Predef$$anon$2;
/** @constructor */
ScalaJS.c.s_StringContext = (function() {
  ScalaJS.c.O.call(this);
  this.parts$1 = null
});
ScalaJS.c.s_StringContext.prototype = new ScalaJS.h.O();
ScalaJS.c.s_StringContext.prototype.constructor = ScalaJS.c.s_StringContext;
/** @constructor */
ScalaJS.h.s_StringContext = (function() {
  /*<skip>*/
});
ScalaJS.h.s_StringContext.prototype = ScalaJS.c.s_StringContext.prototype;
ScalaJS.c.s_StringContext.prototype.productPrefix__T = (function() {
  return "StringContext"
});
ScalaJS.c.s_StringContext.prototype.productArity__I = (function() {
  return 1
});
ScalaJS.c.s_StringContext.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if (ScalaJS.is.s_StringContext(x$1)) {
    var StringContext$1 = ScalaJS.as.s_StringContext(x$1);
    var x = this.parts$1;
    var x$2 = StringContext$1.parts$1;
    return ((x === null) ? (x$2 === null) : x.equals__O__Z(x$2))
  } else {
    return false
  }
});
ScalaJS.c.s_StringContext.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0:
      {
        return this.parts$1;
        break
      };
    default:
      throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + x$1));
  }
});
ScalaJS.c.s_StringContext.prototype.toString__T = (function() {
  return ScalaJS.m.sr_ScalaRunTime$().$$undtoString__s_Product__T(this)
});
ScalaJS.c.s_StringContext.prototype.checkLengths__sc_Seq__V = (function(args) {
  if ((this.parts$1.length__I() !== ((1 + args.length__I()) | 0))) {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___T((((("wrong number of arguments (" + args.length__I()) + ") for interpolated string with ") + this.parts$1.length__I()) + " parts"))
  }
});
ScalaJS.c.s_StringContext.prototype.s__sc_Seq__T = (function(args) {
  var f = (function(this$2) {
    return (function(str$2) {
      var str = ScalaJS.as.T(str$2);
      var this$1 = ScalaJS.m.s_StringContext$();
      return this$1.treatEscapes0__p1__T__Z__T(str, false)
    })
  })(this);
  this.checkLengths__sc_Seq__V(args);
  var pi = this.parts$1.iterator__sc_Iterator();
  var ai = args.iterator__sc_Iterator();
  var arg1 = pi.next__O();
  var bldr = new ScalaJS.c.jl_StringBuilder().init___T(ScalaJS.as.T(f(arg1)));
  while (ai.hasNext__Z()) {
    bldr.append__O__jl_StringBuilder(ai.next__O());
    var arg1$1 = pi.next__O();
    bldr.append__T__jl_StringBuilder(ScalaJS.as.T(f(arg1$1)))
  };
  return bldr.content$1
});
ScalaJS.c.s_StringContext.prototype.init___sc_Seq = (function(parts) {
  this.parts$1 = parts;
  return this
});
ScalaJS.c.s_StringContext.prototype.hashCode__I = (function() {
  var this$2 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
ScalaJS.c.s_StringContext.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.is.s_StringContext = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_StringContext)))
});
ScalaJS.as.s_StringContext = (function(obj) {
  return ((ScalaJS.is.s_StringContext(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.StringContext"))
});
ScalaJS.isArrayOf.s_StringContext = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_StringContext)))
});
ScalaJS.asArrayOf.s_StringContext = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.s_StringContext(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.StringContext;", depth))
});
ScalaJS.d.s_StringContext = new ScalaJS.ClassTypeData({
  s_StringContext: 0
}, false, "scala.StringContext", {
  s_StringContext: 1,
  O: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_StringContext.prototype.$classData = ScalaJS.d.s_StringContext;
/** @constructor */
ScalaJS.c.s_util_control_BreakControl = (function() {
  ScalaJS.c.jl_Throwable.call(this)
});
ScalaJS.c.s_util_control_BreakControl.prototype = new ScalaJS.h.jl_Throwable();
ScalaJS.c.s_util_control_BreakControl.prototype.constructor = ScalaJS.c.s_util_control_BreakControl;
/** @constructor */
ScalaJS.h.s_util_control_BreakControl = (function() {
  /*<skip>*/
});
ScalaJS.h.s_util_control_BreakControl.prototype = ScalaJS.c.s_util_control_BreakControl.prototype;
ScalaJS.c.s_util_control_BreakControl.prototype.init___ = (function() {
  ScalaJS.c.jl_Throwable.prototype.init___.call(this);
  return this
});
ScalaJS.c.s_util_control_BreakControl.prototype.fillInStackTrace__jl_Throwable = (function() {
  return ScalaJS.s.s_util_control_NoStackTrace$class__fillInStackTrace__s_util_control_NoStackTrace__jl_Throwable(this)
});
ScalaJS.c.s_util_control_BreakControl.prototype.scala$util$control$NoStackTrace$$super$fillInStackTrace__jl_Throwable = (function() {
  return ScalaJS.c.jl_Throwable.prototype.fillInStackTrace__jl_Throwable.call(this)
});
ScalaJS.d.s_util_control_BreakControl = new ScalaJS.ClassTypeData({
  s_util_control_BreakControl: 0
}, false, "scala.util.control.BreakControl", {
  s_util_control_BreakControl: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_util_control_ControlThrowable: 1,
  s_util_control_NoStackTrace: 1
});
ScalaJS.c.s_util_control_BreakControl.prototype.$classData = ScalaJS.d.s_util_control_BreakControl;
ScalaJS.is.sc_GenTraversable = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_GenTraversable)))
});
ScalaJS.as.sc_GenTraversable = (function(obj) {
  return ((ScalaJS.is.sc_GenTraversable(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.GenTraversable"))
});
ScalaJS.isArrayOf.sc_GenTraversable = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_GenTraversable)))
});
ScalaJS.asArrayOf.sc_GenTraversable = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_GenTraversable(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.GenTraversable;", depth))
});
/** @constructor */
ScalaJS.c.sc_Iterable$ = (function() {
  ScalaJS.c.scg_GenTraversableFactory.call(this)
});
ScalaJS.c.sc_Iterable$.prototype = new ScalaJS.h.scg_GenTraversableFactory();
ScalaJS.c.sc_Iterable$.prototype.constructor = ScalaJS.c.sc_Iterable$;
/** @constructor */
ScalaJS.h.sc_Iterable$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_Iterable$.prototype = ScalaJS.c.sc_Iterable$.prototype;
ScalaJS.c.sc_Iterable$.prototype.newBuilder__scm_Builder = (function() {
  ScalaJS.m.sci_Iterable$();
  return new ScalaJS.c.scm_ListBuffer().init___()
});
ScalaJS.d.sc_Iterable$ = new ScalaJS.ClassTypeData({
  sc_Iterable$: 0
}, false, "scala.collection.Iterable$", {
  sc_Iterable$: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sc_Iterable$.prototype.$classData = ScalaJS.d.sc_Iterable$;
ScalaJS.n.sc_Iterable$ = (void 0);
ScalaJS.m.sc_Iterable$ = (function() {
  if ((!ScalaJS.n.sc_Iterable$)) {
    ScalaJS.n.sc_Iterable$ = new ScalaJS.c.sc_Iterable$().init___()
  };
  return ScalaJS.n.sc_Iterable$
});
/** @constructor */
ScalaJS.c.sc_Iterator$$anon$11 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.$$outer$2 = null;
  this.f$3$2 = null
});
ScalaJS.c.sc_Iterator$$anon$11.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sc_Iterator$$anon$11.prototype.constructor = ScalaJS.c.sc_Iterator$$anon$11;
/** @constructor */
ScalaJS.h.sc_Iterator$$anon$11 = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_Iterator$$anon$11.prototype = ScalaJS.c.sc_Iterator$$anon$11.prototype;
ScalaJS.c.sc_Iterator$$anon$11.prototype.next__O = (function() {
  return this.f$3$2.apply__O__O(this.$$outer$2.next__O())
});
ScalaJS.c.sc_Iterator$$anon$11.prototype.init___sc_Iterator__F1 = (function($$outer, f$3) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  this.f$3$2 = f$3;
  return this
});
ScalaJS.c.sc_Iterator$$anon$11.prototype.hasNext__Z = (function() {
  return this.$$outer$2.hasNext__Z()
});
ScalaJS.d.sc_Iterator$$anon$11 = new ScalaJS.ClassTypeData({
  sc_Iterator$$anon$11: 0
}, false, "scala.collection.Iterator$$anon$11", {
  sc_Iterator$$anon$11: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sc_Iterator$$anon$11.prototype.$classData = ScalaJS.d.sc_Iterator$$anon$11;
/** @constructor */
ScalaJS.c.sc_Iterator$$anon$2 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this)
});
ScalaJS.c.sc_Iterator$$anon$2.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sc_Iterator$$anon$2.prototype.constructor = ScalaJS.c.sc_Iterator$$anon$2;
/** @constructor */
ScalaJS.h.sc_Iterator$$anon$2 = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_Iterator$$anon$2.prototype = ScalaJS.c.sc_Iterator$$anon$2.prototype;
ScalaJS.c.sc_Iterator$$anon$2.prototype.next__O = (function() {
  this.next__sr_Nothing$()
});
ScalaJS.c.sc_Iterator$$anon$2.prototype.next__sr_Nothing$ = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("next on empty iterator")
});
ScalaJS.c.sc_Iterator$$anon$2.prototype.hasNext__Z = (function() {
  return false
});
ScalaJS.d.sc_Iterator$$anon$2 = new ScalaJS.ClassTypeData({
  sc_Iterator$$anon$2: 0
}, false, "scala.collection.Iterator$$anon$2", {
  sc_Iterator$$anon$2: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sc_Iterator$$anon$2.prototype.$classData = ScalaJS.d.sc_Iterator$$anon$2;
/** @constructor */
ScalaJS.c.sc_LinearSeqLike$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.these$2 = null
});
ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype.constructor = ScalaJS.c.sc_LinearSeqLike$$anon$1;
/** @constructor */
ScalaJS.h.sc_LinearSeqLike$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_LinearSeqLike$$anon$1.prototype = ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype;
ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype.init___sc_LinearSeqLike = (function($$outer) {
  this.these$2 = $$outer;
  return this
});
ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype.next__O = (function() {
  if (this.hasNext__Z()) {
    var result = this.these$2.head__O();
    this.these$2 = ScalaJS.as.sc_LinearSeqLike(this.these$2.tail__O());
    return result
  } else {
    return ScalaJS.m.sc_Iterator$().empty$1.next__O()
  }
});
ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype.toList__sci_List = (function() {
  var xs = this.these$2.toList__sci_List();
  this.these$2 = ScalaJS.as.sc_LinearSeqLike(this.these$2.take__I__O(0));
  return xs
});
ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype.hasNext__Z = (function() {
  return (!this.these$2.isEmpty__Z())
});
ScalaJS.d.sc_LinearSeqLike$$anon$1 = new ScalaJS.ClassTypeData({
  sc_LinearSeqLike$$anon$1: 0
}, false, "scala.collection.LinearSeqLike$$anon$1", {
  sc_LinearSeqLike$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sc_LinearSeqLike$$anon$1.prototype.$classData = ScalaJS.d.sc_LinearSeqLike$$anon$1;
/** @constructor */
ScalaJS.c.sc_Traversable$ = (function() {
  ScalaJS.c.scg_GenTraversableFactory.call(this);
  this.breaks$3 = null
});
ScalaJS.c.sc_Traversable$.prototype = new ScalaJS.h.scg_GenTraversableFactory();
ScalaJS.c.sc_Traversable$.prototype.constructor = ScalaJS.c.sc_Traversable$;
/** @constructor */
ScalaJS.h.sc_Traversable$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_Traversable$.prototype = ScalaJS.c.sc_Traversable$.prototype;
ScalaJS.c.sc_Traversable$.prototype.init___ = (function() {
  ScalaJS.c.scg_GenTraversableFactory.prototype.init___.call(this);
  ScalaJS.n.sc_Traversable$ = this;
  this.breaks$3 = new ScalaJS.c.s_util_control_Breaks().init___();
  return this
});
ScalaJS.c.sc_Traversable$.prototype.newBuilder__scm_Builder = (function() {
  ScalaJS.m.sci_Traversable$();
  return new ScalaJS.c.scm_ListBuffer().init___()
});
ScalaJS.d.sc_Traversable$ = new ScalaJS.ClassTypeData({
  sc_Traversable$: 0
}, false, "scala.collection.Traversable$", {
  sc_Traversable$: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sc_Traversable$.prototype.$classData = ScalaJS.d.sc_Traversable$;
ScalaJS.n.sc_Traversable$ = (void 0);
ScalaJS.m.sc_Traversable$ = (function() {
  if ((!ScalaJS.n.sc_Traversable$)) {
    ScalaJS.n.sc_Traversable$ = new ScalaJS.c.sc_Traversable$().init___()
  };
  return ScalaJS.n.sc_Traversable$
});
/** @constructor */
ScalaJS.c.scg_ImmutableSetFactory = (function() {
  ScalaJS.c.scg_SetFactory.call(this)
});
ScalaJS.c.scg_ImmutableSetFactory.prototype = new ScalaJS.h.scg_SetFactory();
ScalaJS.c.scg_ImmutableSetFactory.prototype.constructor = ScalaJS.c.scg_ImmutableSetFactory;
/** @constructor */
ScalaJS.h.scg_ImmutableSetFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_ImmutableSetFactory.prototype = ScalaJS.c.scg_ImmutableSetFactory.prototype;
ScalaJS.c.scg_ImmutableSetFactory.prototype.empty__sc_GenTraversable = (function() {
  return this.emptyInstance__sci_Set()
});
ScalaJS.c.scg_ImmutableSetFactory.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_SetBuilder().init___sc_Set(this.emptyInstance__sci_Set())
});
/** @constructor */
ScalaJS.c.scg_MutableSetFactory = (function() {
  ScalaJS.c.scg_SetFactory.call(this)
});
ScalaJS.c.scg_MutableSetFactory.prototype = new ScalaJS.h.scg_SetFactory();
ScalaJS.c.scg_MutableSetFactory.prototype.constructor = ScalaJS.c.scg_MutableSetFactory;
/** @constructor */
ScalaJS.h.scg_MutableSetFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_MutableSetFactory.prototype = ScalaJS.c.scg_MutableSetFactory.prototype;
ScalaJS.c.scg_MutableSetFactory.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_GrowingBuilder().init___scg_Growable(ScalaJS.as.scg_Growable(this.empty__sc_GenTraversable()))
});
/** @constructor */
ScalaJS.c.sci_Iterable$ = (function() {
  ScalaJS.c.scg_GenTraversableFactory.call(this)
});
ScalaJS.c.sci_Iterable$.prototype = new ScalaJS.h.scg_GenTraversableFactory();
ScalaJS.c.sci_Iterable$.prototype.constructor = ScalaJS.c.sci_Iterable$;
/** @constructor */
ScalaJS.h.sci_Iterable$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Iterable$.prototype = ScalaJS.c.sci_Iterable$.prototype;
ScalaJS.c.sci_Iterable$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_ListBuffer().init___()
});
ScalaJS.d.sci_Iterable$ = new ScalaJS.ClassTypeData({
  sci_Iterable$: 0
}, false, "scala.collection.immutable.Iterable$", {
  sci_Iterable$: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sci_Iterable$.prototype.$classData = ScalaJS.d.sci_Iterable$;
ScalaJS.n.sci_Iterable$ = (void 0);
ScalaJS.m.sci_Iterable$ = (function() {
  if ((!ScalaJS.n.sci_Iterable$)) {
    ScalaJS.n.sci_Iterable$ = new ScalaJS.c.sci_Iterable$().init___()
  };
  return ScalaJS.n.sci_Iterable$
});
/** @constructor */
ScalaJS.c.sci_ListMap$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.self$2 = null
});
ScalaJS.c.sci_ListMap$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sci_ListMap$$anon$1.prototype.constructor = ScalaJS.c.sci_ListMap$$anon$1;
/** @constructor */
ScalaJS.h.sci_ListMap$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListMap$$anon$1.prototype = ScalaJS.c.sci_ListMap$$anon$1.prototype;
ScalaJS.c.sci_ListMap$$anon$1.prototype.next__O = (function() {
  return this.next__T2()
});
ScalaJS.c.sci_ListMap$$anon$1.prototype.init___sci_ListMap = (function($$outer) {
  this.self$2 = $$outer;
  return this
});
ScalaJS.c.sci_ListMap$$anon$1.prototype.next__T2 = (function() {
  if ((!this.hasNext__Z())) {
    throw new ScalaJS.c.ju_NoSuchElementException().init___T("next on empty iterator")
  } else {
    var res = new ScalaJS.c.T2().init___O__O(this.self$2.key__O(), this.self$2.value__O());
    this.self$2 = this.self$2.next__sci_ListMap();
    return res
  }
});
ScalaJS.c.sci_ListMap$$anon$1.prototype.hasNext__Z = (function() {
  return (!this.self$2.isEmpty__Z())
});
ScalaJS.d.sci_ListMap$$anon$1 = new ScalaJS.ClassTypeData({
  sci_ListMap$$anon$1: 0
}, false, "scala.collection.immutable.ListMap$$anon$1", {
  sci_ListMap$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sci_ListMap$$anon$1.prototype.$classData = ScalaJS.d.sci_ListMap$$anon$1;
/** @constructor */
ScalaJS.c.sci_ListSet$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.that$2 = null
});
ScalaJS.c.sci_ListSet$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sci_ListSet$$anon$1.prototype.constructor = ScalaJS.c.sci_ListSet$$anon$1;
/** @constructor */
ScalaJS.h.sci_ListSet$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListSet$$anon$1.prototype = ScalaJS.c.sci_ListSet$$anon$1.prototype;
ScalaJS.c.sci_ListSet$$anon$1.prototype.next__O = (function() {
  var this$1 = this.that$2;
  if (ScalaJS.s.sc_TraversableOnce$class__nonEmpty__sc_TraversableOnce__Z(this$1)) {
    var res = this.that$2.head__O();
    this.that$2 = this.that$2.tail__sci_ListSet();
    return res
  } else {
    return ScalaJS.m.sc_Iterator$().empty$1.next__O()
  }
});
ScalaJS.c.sci_ListSet$$anon$1.prototype.init___sci_ListSet = (function($$outer) {
  this.that$2 = $$outer;
  return this
});
ScalaJS.c.sci_ListSet$$anon$1.prototype.hasNext__Z = (function() {
  var this$1 = this.that$2;
  return ScalaJS.s.sc_TraversableOnce$class__nonEmpty__sc_TraversableOnce__Z(this$1)
});
ScalaJS.d.sci_ListSet$$anon$1 = new ScalaJS.ClassTypeData({
  sci_ListSet$$anon$1: 0
}, false, "scala.collection.immutable.ListSet$$anon$1", {
  sci_ListSet$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sci_ListSet$$anon$1.prototype.$classData = ScalaJS.d.sci_ListSet$$anon$1;
/** @constructor */
ScalaJS.c.sci_Stream$StreamBuilder = (function() {
  ScalaJS.c.scm_LazyBuilder.call(this)
});
ScalaJS.c.sci_Stream$StreamBuilder.prototype = new ScalaJS.h.scm_LazyBuilder();
ScalaJS.c.sci_Stream$StreamBuilder.prototype.constructor = ScalaJS.c.sci_Stream$StreamBuilder;
/** @constructor */
ScalaJS.h.sci_Stream$StreamBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream$StreamBuilder.prototype = ScalaJS.c.sci_Stream$StreamBuilder.prototype;
ScalaJS.c.sci_Stream$StreamBuilder.prototype.result__O = (function() {
  return this.result__sci_Stream()
});
ScalaJS.c.sci_Stream$StreamBuilder.prototype.result__sci_Stream = (function() {
  var this$1 = this.parts$1;
  return ScalaJS.as.sci_Stream(this$1.scala$collection$mutable$ListBuffer$$start$6.toStream__sci_Stream().flatMap__F1__scg_CanBuildFrom__O(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2) {
    return (function(x$5$2) {
      var x$5 = ScalaJS.as.sc_TraversableOnce(x$5$2);
      return x$5.toStream__sci_Stream()
    })
  })(this)), (ScalaJS.m.sci_Stream$(), new ScalaJS.c.sci_Stream$StreamCanBuildFrom().init___())))
});
ScalaJS.is.sci_Stream$StreamBuilder = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_Stream$StreamBuilder)))
});
ScalaJS.as.sci_Stream$StreamBuilder = (function(obj) {
  return ((ScalaJS.is.sci_Stream$StreamBuilder(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.Stream$StreamBuilder"))
});
ScalaJS.isArrayOf.sci_Stream$StreamBuilder = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_Stream$StreamBuilder)))
});
ScalaJS.asArrayOf.sci_Stream$StreamBuilder = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_Stream$StreamBuilder(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.Stream$StreamBuilder;", depth))
});
ScalaJS.d.sci_Stream$StreamBuilder = new ScalaJS.ClassTypeData({
  sci_Stream$StreamBuilder: 0
}, false, "scala.collection.immutable.Stream$StreamBuilder", {
  sci_Stream$StreamBuilder: 1,
  scm_LazyBuilder: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1
});
ScalaJS.c.sci_Stream$StreamBuilder.prototype.$classData = ScalaJS.d.sci_Stream$StreamBuilder;
/** @constructor */
ScalaJS.c.sci_StreamIterator = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.these$2 = null
});
ScalaJS.c.sci_StreamIterator.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sci_StreamIterator.prototype.constructor = ScalaJS.c.sci_StreamIterator;
/** @constructor */
ScalaJS.h.sci_StreamIterator = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_StreamIterator.prototype = ScalaJS.c.sci_StreamIterator.prototype;
ScalaJS.c.sci_StreamIterator.prototype.next__O = (function() {
  if (ScalaJS.s.sc_Iterator$class__isEmpty__sc_Iterator__Z(this)) {
    return ScalaJS.m.sc_Iterator$().empty$1.next__O()
  } else {
    var cur = this.these$2.v__sci_Stream();
    var result = cur.head__O();
    this.these$2 = new ScalaJS.c.sci_StreamIterator$LazyCell().init___sci_StreamIterator__F0(this, new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2, cur$1) {
      return (function() {
        return ScalaJS.as.sci_Stream(cur$1.tail__O())
      })
    })(this, cur)));
    return result
  }
});
ScalaJS.c.sci_StreamIterator.prototype.toList__sci_List = (function() {
  var this$1 = this.toStream__sci_Stream();
  var this$2 = ScalaJS.m.sci_List$();
  var cbf = this$2.ReusableCBFInstance$2;
  return ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(this$1, cbf))
});
ScalaJS.c.sci_StreamIterator.prototype.init___sci_Stream = (function(self) {
  this.these$2 = new ScalaJS.c.sci_StreamIterator$LazyCell().init___sci_StreamIterator__F0(this, new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2, self$1) {
    return (function() {
      return self$1
    })
  })(this, self)));
  return this
});
ScalaJS.c.sci_StreamIterator.prototype.hasNext__Z = (function() {
  var this$1 = this.these$2.v__sci_Stream();
  return ScalaJS.s.sc_TraversableOnce$class__nonEmpty__sc_TraversableOnce__Z(this$1)
});
ScalaJS.c.sci_StreamIterator.prototype.toStream__sci_Stream = (function() {
  var result = this.these$2.v__sci_Stream();
  this.these$2 = new ScalaJS.c.sci_StreamIterator$LazyCell().init___sci_StreamIterator__F0(this, new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2) {
    return (function() {
      ScalaJS.m.sci_Stream$();
      return ScalaJS.m.sci_Stream$Empty$()
    })
  })(this)));
  return result
});
ScalaJS.d.sci_StreamIterator = new ScalaJS.ClassTypeData({
  sci_StreamIterator: 0
}, false, "scala.collection.immutable.StreamIterator", {
  sci_StreamIterator: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sci_StreamIterator.prototype.$classData = ScalaJS.d.sci_StreamIterator;
/** @constructor */
ScalaJS.c.sci_Traversable$ = (function() {
  ScalaJS.c.scg_GenTraversableFactory.call(this)
});
ScalaJS.c.sci_Traversable$.prototype = new ScalaJS.h.scg_GenTraversableFactory();
ScalaJS.c.sci_Traversable$.prototype.constructor = ScalaJS.c.sci_Traversable$;
/** @constructor */
ScalaJS.h.sci_Traversable$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Traversable$.prototype = ScalaJS.c.sci_Traversable$.prototype;
ScalaJS.c.sci_Traversable$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_ListBuffer().init___()
});
ScalaJS.d.sci_Traversable$ = new ScalaJS.ClassTypeData({
  sci_Traversable$: 0
}, false, "scala.collection.immutable.Traversable$", {
  sci_Traversable$: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sci_Traversable$.prototype.$classData = ScalaJS.d.sci_Traversable$;
ScalaJS.n.sci_Traversable$ = (void 0);
ScalaJS.m.sci_Traversable$ = (function() {
  if ((!ScalaJS.n.sci_Traversable$)) {
    ScalaJS.n.sci_Traversable$ = new ScalaJS.c.sci_Traversable$().init___()
  };
  return ScalaJS.n.sci_Traversable$
});
/** @constructor */
ScalaJS.c.sci_TrieIterator = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.elems$2 = null;
  this.scala$collection$immutable$TrieIterator$$depth$f = 0;
  this.scala$collection$immutable$TrieIterator$$arrayStack$f = null;
  this.scala$collection$immutable$TrieIterator$$posStack$f = null;
  this.scala$collection$immutable$TrieIterator$$arrayD$f = null;
  this.scala$collection$immutable$TrieIterator$$posD$f = 0;
  this.scala$collection$immutable$TrieIterator$$subIter$f = null
});
ScalaJS.c.sci_TrieIterator.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sci_TrieIterator.prototype.constructor = ScalaJS.c.sci_TrieIterator;
/** @constructor */
ScalaJS.h.sci_TrieIterator = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_TrieIterator.prototype = ScalaJS.c.sci_TrieIterator.prototype;
ScalaJS.c.sci_TrieIterator.prototype.isContainer__p2__O__Z = (function(x) {
  return (ScalaJS.is.sci_HashMap$HashMap1(x) || ScalaJS.is.sci_HashSet$HashSet1(x))
});
ScalaJS.c.sci_TrieIterator.prototype.next__O = (function() {
  if ((this.scala$collection$immutable$TrieIterator$$subIter$f !== null)) {
    var el = this.scala$collection$immutable$TrieIterator$$subIter$f.next__O();
    if ((!this.scala$collection$immutable$TrieIterator$$subIter$f.hasNext__Z())) {
      this.scala$collection$immutable$TrieIterator$$subIter$f = null
    };
    return el
  } else {
    return this.next0__p2__Asci_Iterable__I__O(this.scala$collection$immutable$TrieIterator$$arrayD$f, this.scala$collection$immutable$TrieIterator$$posD$f)
  }
});
ScalaJS.c.sci_TrieIterator.prototype.initPosStack__AI = (function() {
  return ScalaJS.newArrayObject(ScalaJS.d.I.getArrayOf(), [6])
});
ScalaJS.c.sci_TrieIterator.prototype.hasNext__Z = (function() {
  return ((this.scala$collection$immutable$TrieIterator$$subIter$f !== null) || (this.scala$collection$immutable$TrieIterator$$depth$f >= 0))
});
ScalaJS.c.sci_TrieIterator.prototype.next0__p2__Asci_Iterable__I__O = (function(elems, i) {
  _next0: while (true) {
    if ((i === (((-1) + elems.u["length"]) | 0))) {
      this.scala$collection$immutable$TrieIterator$$depth$f = (((-1) + this.scala$collection$immutable$TrieIterator$$depth$f) | 0);
      if ((this.scala$collection$immutable$TrieIterator$$depth$f >= 0)) {
        this.scala$collection$immutable$TrieIterator$$arrayD$f = this.scala$collection$immutable$TrieIterator$$arrayStack$f.u[this.scala$collection$immutable$TrieIterator$$depth$f];
        this.scala$collection$immutable$TrieIterator$$posD$f = this.scala$collection$immutable$TrieIterator$$posStack$f.u[this.scala$collection$immutable$TrieIterator$$depth$f];
        this.scala$collection$immutable$TrieIterator$$arrayStack$f.u[this.scala$collection$immutable$TrieIterator$$depth$f] = null
      } else {
        this.scala$collection$immutable$TrieIterator$$arrayD$f = null;
        this.scala$collection$immutable$TrieIterator$$posD$f = 0
      }
    } else {
      this.scala$collection$immutable$TrieIterator$$posD$f = ((1 + this.scala$collection$immutable$TrieIterator$$posD$f) | 0)
    };
    var m = elems.u[i];
    if (this.isContainer__p2__O__Z(m)) {
      return this.getElem__O__O(m)
    } else if (this.isTrie__p2__O__Z(m)) {
      if ((this.scala$collection$immutable$TrieIterator$$depth$f >= 0)) {
        this.scala$collection$immutable$TrieIterator$$arrayStack$f.u[this.scala$collection$immutable$TrieIterator$$depth$f] = this.scala$collection$immutable$TrieIterator$$arrayD$f;
        this.scala$collection$immutable$TrieIterator$$posStack$f.u[this.scala$collection$immutable$TrieIterator$$depth$f] = this.scala$collection$immutable$TrieIterator$$posD$f
      };
      this.scala$collection$immutable$TrieIterator$$depth$f = ((1 + this.scala$collection$immutable$TrieIterator$$depth$f) | 0);
      this.scala$collection$immutable$TrieIterator$$arrayD$f = this.getElems__p2__sci_Iterable__Asci_Iterable(m);
      this.scala$collection$immutable$TrieIterator$$posD$f = 0;
      var temp$elems = this.getElems__p2__sci_Iterable__Asci_Iterable(m);
      elems = temp$elems;
      i = 0;
      continue _next0
    } else {
      this.scala$collection$immutable$TrieIterator$$subIter$f = m.iterator__sc_Iterator();
      return this.next__O()
    }
  }
});
ScalaJS.c.sci_TrieIterator.prototype.getElems__p2__sci_Iterable__Asci_Iterable = (function(x) {
  if (ScalaJS.is.sci_HashMap$HashTrieMap(x)) {
    var x2 = ScalaJS.as.sci_HashMap$HashTrieMap(x);
    var jsx$1 = x2.elems$6
  } else if (ScalaJS.is.sci_HashSet$HashTrieSet(x)) {
    var x3 = ScalaJS.as.sci_HashSet$HashTrieSet(x);
    var jsx$1 = x3.elems$5
  } else {
    var jsx$1;
    throw new ScalaJS.c.s_MatchError().init___O(x)
  };
  return ScalaJS.asArrayOf.sci_Iterable(jsx$1, 1)
});
ScalaJS.c.sci_TrieIterator.prototype.init___Asci_Iterable = (function(elems) {
  this.elems$2 = elems;
  this.scala$collection$immutable$TrieIterator$$depth$f = 0;
  this.scala$collection$immutable$TrieIterator$$arrayStack$f = this.initArrayStack__AAsci_Iterable();
  this.scala$collection$immutable$TrieIterator$$posStack$f = this.initPosStack__AI();
  this.scala$collection$immutable$TrieIterator$$arrayD$f = this.elems$2;
  this.scala$collection$immutable$TrieIterator$$posD$f = 0;
  this.scala$collection$immutable$TrieIterator$$subIter$f = null;
  return this
});
ScalaJS.c.sci_TrieIterator.prototype.isTrie__p2__O__Z = (function(x) {
  return (ScalaJS.is.sci_HashMap$HashTrieMap(x) || ScalaJS.is.sci_HashSet$HashTrieSet(x))
});
ScalaJS.c.sci_TrieIterator.prototype.initArrayStack__AAsci_Iterable = (function() {
  return ScalaJS.newArrayObject(ScalaJS.d.sci_Iterable.getArrayOf().getArrayOf(), [6])
});
/** @constructor */
ScalaJS.c.sci_Vector$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.i$2 = 0;
  this.$$outer$2 = null
});
ScalaJS.c.sci_Vector$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sci_Vector$$anon$1.prototype.constructor = ScalaJS.c.sci_Vector$$anon$1;
/** @constructor */
ScalaJS.h.sci_Vector$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Vector$$anon$1.prototype = ScalaJS.c.sci_Vector$$anon$1.prototype;
ScalaJS.c.sci_Vector$$anon$1.prototype.next__O = (function() {
  if ((this.i$2 > 0)) {
    this.i$2 = (((-1) + this.i$2) | 0);
    return this.$$outer$2.apply__I__O(this.i$2)
  } else {
    return ScalaJS.m.sc_Iterator$().empty$1.next__O()
  }
});
ScalaJS.c.sci_Vector$$anon$1.prototype.hasNext__Z = (function() {
  return (this.i$2 > 0)
});
ScalaJS.c.sci_Vector$$anon$1.prototype.init___sci_Vector = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  this.i$2 = $$outer.length__I();
  return this
});
ScalaJS.d.sci_Vector$$anon$1 = new ScalaJS.ClassTypeData({
  sci_Vector$$anon$1: 0
}, false, "scala.collection.immutable.Vector$$anon$1", {
  sci_Vector$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sci_Vector$$anon$1.prototype.$classData = ScalaJS.d.sci_Vector$$anon$1;
/** @constructor */
ScalaJS.c.sci_VectorBuilder = (function() {
  ScalaJS.c.O.call(this);
  this.blockIndex$1 = 0;
  this.lo$1 = 0;
  this.depth$1 = 0;
  this.display0$1 = null;
  this.display1$1 = null;
  this.display2$1 = null;
  this.display3$1 = null;
  this.display4$1 = null;
  this.display5$1 = null
});
ScalaJS.c.sci_VectorBuilder.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_VectorBuilder.prototype.constructor = ScalaJS.c.sci_VectorBuilder;
/** @constructor */
ScalaJS.h.sci_VectorBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_VectorBuilder.prototype = ScalaJS.c.sci_VectorBuilder.prototype;
ScalaJS.c.sci_VectorBuilder.prototype.display3__AO = (function() {
  return this.display3$1
});
ScalaJS.c.sci_VectorBuilder.prototype.init___ = (function() {
  this.display0$1 = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]);
  this.depth$1 = 1;
  this.blockIndex$1 = 0;
  this.lo$1 = 0;
  return this
});
ScalaJS.c.sci_VectorBuilder.prototype.depth__I = (function() {
  return this.depth$1
});
ScalaJS.c.sci_VectorBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__sci_VectorBuilder(elem)
});
ScalaJS.c.sci_VectorBuilder.prototype.display5$und$eq__AO__V = (function(x$1) {
  this.display5$1 = x$1
});
ScalaJS.c.sci_VectorBuilder.prototype.display0__AO = (function() {
  return this.display0$1
});
ScalaJS.c.sci_VectorBuilder.prototype.display4__AO = (function() {
  return this.display4$1
});
ScalaJS.c.sci_VectorBuilder.prototype.display2$und$eq__AO__V = (function(x$1) {
  this.display2$1 = x$1
});
ScalaJS.c.sci_VectorBuilder.prototype.$$plus$eq__O__sci_VectorBuilder = (function(elem) {
  if ((this.lo$1 >= this.display0$1.u["length"])) {
    var newBlockIndex = ((32 + this.blockIndex$1) | 0);
    var xor = (this.blockIndex$1 ^ newBlockIndex);
    ScalaJS.s.sci_VectorPointer$class__gotoNextBlockStartWritable__sci_VectorPointer__I__I__V(this, newBlockIndex, xor);
    this.blockIndex$1 = newBlockIndex;
    this.lo$1 = 0
  };
  this.display0$1.u[this.lo$1] = elem;
  this.lo$1 = ((1 + this.lo$1) | 0);
  return this
});
ScalaJS.c.sci_VectorBuilder.prototype.result__O = (function() {
  return this.result__sci_Vector()
});
ScalaJS.c.sci_VectorBuilder.prototype.display1$und$eq__AO__V = (function(x$1) {
  this.display1$1 = x$1
});
ScalaJS.c.sci_VectorBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.sci_VectorBuilder.prototype.display4$und$eq__AO__V = (function(x$1) {
  this.display4$1 = x$1
});
ScalaJS.c.sci_VectorBuilder.prototype.display1__AO = (function() {
  return this.display1$1
});
ScalaJS.c.sci_VectorBuilder.prototype.display5__AO = (function() {
  return this.display5$1
});
ScalaJS.c.sci_VectorBuilder.prototype.result__sci_Vector = (function() {
  var size = ((this.blockIndex$1 + this.lo$1) | 0);
  if ((size === 0)) {
    var this$1 = ScalaJS.m.sci_Vector$();
    return this$1.NIL$6
  };
  var s = new ScalaJS.c.sci_Vector().init___I__I__I(0, size, 0);
  var depth = this.depth$1;
  ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s, this, depth);
  if ((this.depth$1 > 1)) {
    var xor = (((-1) + size) | 0);
    ScalaJS.s.sci_VectorPointer$class__gotoPos__sci_VectorPointer__I__I__V(s, 0, xor)
  };
  return s
});
ScalaJS.c.sci_VectorBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__sci_VectorBuilder(elem)
});
ScalaJS.c.sci_VectorBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.sci_VectorBuilder.prototype.depth$und$eq__I__V = (function(x$1) {
  this.depth$1 = x$1
});
ScalaJS.c.sci_VectorBuilder.prototype.display2__AO = (function() {
  return this.display2$1
});
ScalaJS.c.sci_VectorBuilder.prototype.display0$und$eq__AO__V = (function(x$1) {
  this.display0$1 = x$1
});
ScalaJS.c.sci_VectorBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.as.sci_VectorBuilder(ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs))
});
ScalaJS.c.sci_VectorBuilder.prototype.display3$und$eq__AO__V = (function(x$1) {
  this.display3$1 = x$1
});
ScalaJS.is.sci_VectorBuilder = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_VectorBuilder)))
});
ScalaJS.as.sci_VectorBuilder = (function(obj) {
  return ((ScalaJS.is.sci_VectorBuilder(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.VectorBuilder"))
});
ScalaJS.isArrayOf.sci_VectorBuilder = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_VectorBuilder)))
});
ScalaJS.asArrayOf.sci_VectorBuilder = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_VectorBuilder(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.VectorBuilder;", depth))
});
ScalaJS.d.sci_VectorBuilder = new ScalaJS.ClassTypeData({
  sci_VectorBuilder: 0
}, false, "scala.collection.immutable.VectorBuilder", {
  sci_VectorBuilder: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  sci_VectorPointer: 1
});
ScalaJS.c.sci_VectorBuilder.prototype.$classData = ScalaJS.d.sci_VectorBuilder;
/** @constructor */
ScalaJS.c.scm_Builder$$anon$1 = (function() {
  ScalaJS.c.O.call(this);
  this.self$1 = null;
  this.f$1$1 = null
});
ScalaJS.c.scm_Builder$$anon$1.prototype = new ScalaJS.h.O();
ScalaJS.c.scm_Builder$$anon$1.prototype.constructor = ScalaJS.c.scm_Builder$$anon$1;
/** @constructor */
ScalaJS.h.scm_Builder$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_Builder$$anon$1.prototype = ScalaJS.c.scm_Builder$$anon$1.prototype;
ScalaJS.c.scm_Builder$$anon$1.prototype.init___scm_Builder__F1 = (function($$outer, f$1) {
  this.f$1$1 = f$1;
  this.self$1 = $$outer;
  return this
});
ScalaJS.c.scm_Builder$$anon$1.prototype.equals__O__Z = (function(that) {
  return ScalaJS.s.s_Proxy$class__equals__s_Proxy__O__Z(this, that)
});
ScalaJS.c.scm_Builder$$anon$1.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_Builder$$anon$1(elem)
});
ScalaJS.c.scm_Builder$$anon$1.prototype.toString__T = (function() {
  return ScalaJS.s.s_Proxy$class__toString__s_Proxy__T(this)
});
ScalaJS.c.scm_Builder$$anon$1.prototype.$$plus$plus$eq__sc_TraversableOnce__scm_Builder$$anon$1 = (function(xs) {
  this.self$1.$$plus$plus$eq__sc_TraversableOnce__scg_Growable(xs);
  return this
});
ScalaJS.c.scm_Builder$$anon$1.prototype.result__O = (function() {
  return this.f$1$1.apply__O__O(this.self$1.result__O())
});
ScalaJS.c.scm_Builder$$anon$1.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundColl) {
  this.self$1.sizeHintBounded__I__sc_TraversableLike__V(size, boundColl)
});
ScalaJS.c.scm_Builder$$anon$1.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_Builder$$anon$1(elem)
});
ScalaJS.c.scm_Builder$$anon$1.prototype.$$plus$eq__O__scm_Builder$$anon$1 = (function(x) {
  this.self$1.$$plus$eq__O__scm_Builder(x);
  return this
});
ScalaJS.c.scm_Builder$$anon$1.prototype.hashCode__I = (function() {
  return this.self$1.hashCode__I()
});
ScalaJS.c.scm_Builder$$anon$1.prototype.sizeHint__I__V = (function(size) {
  this.self$1.sizeHint__I__V(size)
});
ScalaJS.c.scm_Builder$$anon$1.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return this.$$plus$plus$eq__sc_TraversableOnce__scm_Builder$$anon$1(xs)
});
ScalaJS.d.scm_Builder$$anon$1 = new ScalaJS.ClassTypeData({
  scm_Builder$$anon$1: 0
}, false, "scala.collection.mutable.Builder$$anon$1", {
  scm_Builder$$anon$1: 1,
  O: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  s_Proxy: 1
});
ScalaJS.c.scm_Builder$$anon$1.prototype.$classData = ScalaJS.d.scm_Builder$$anon$1;
/** @constructor */
ScalaJS.c.scm_FlatHashTable$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.i$2 = 0;
  this.$$outer$2 = null
});
ScalaJS.c.scm_FlatHashTable$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.scm_FlatHashTable$$anon$1.prototype.constructor = ScalaJS.c.scm_FlatHashTable$$anon$1;
/** @constructor */
ScalaJS.h.scm_FlatHashTable$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_FlatHashTable$$anon$1.prototype = ScalaJS.c.scm_FlatHashTable$$anon$1.prototype;
ScalaJS.c.scm_FlatHashTable$$anon$1.prototype.next__O = (function() {
  if (this.hasNext__Z()) {
    this.i$2 = ((1 + this.i$2) | 0);
    var this$1 = this.$$outer$2;
    var entry = this.$$outer$2.table$5.u[(((-1) + this.i$2) | 0)];
    return ScalaJS.s.scm_FlatHashTable$HashUtils$class__entryToElem__scm_FlatHashTable$HashUtils__O__O(this$1, entry)
  } else {
    return ScalaJS.m.sc_Iterator$().empty$1.next__O()
  }
});
ScalaJS.c.scm_FlatHashTable$$anon$1.prototype.init___scm_FlatHashTable = (function($$outer) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  this.i$2 = 0;
  return this
});
ScalaJS.c.scm_FlatHashTable$$anon$1.prototype.hasNext__Z = (function() {
  while (((this.i$2 < this.$$outer$2.table$5.u["length"]) && (this.$$outer$2.table$5.u[this.i$2] === null))) {
    this.i$2 = ((1 + this.i$2) | 0)
  };
  return (this.i$2 < this.$$outer$2.table$5.u["length"])
});
ScalaJS.d.scm_FlatHashTable$$anon$1 = new ScalaJS.ClassTypeData({
  scm_FlatHashTable$$anon$1: 0
}, false, "scala.collection.mutable.FlatHashTable$$anon$1", {
  scm_FlatHashTable$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.scm_FlatHashTable$$anon$1.prototype.$classData = ScalaJS.d.scm_FlatHashTable$$anon$1;
/** @constructor */
ScalaJS.c.scm_HashTable$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.iterTable$2 = null;
  this.idx$2 = 0;
  this.es$2 = null
});
ScalaJS.c.scm_HashTable$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.scm_HashTable$$anon$1.prototype.constructor = ScalaJS.c.scm_HashTable$$anon$1;
/** @constructor */
ScalaJS.h.scm_HashTable$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_HashTable$$anon$1.prototype = ScalaJS.c.scm_HashTable$$anon$1.prototype;
ScalaJS.c.scm_HashTable$$anon$1.prototype.init___scm_HashTable = (function($$outer) {
  this.iterTable$2 = $$outer.table$5;
  this.idx$2 = ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$lastPopulatedIndex__scm_HashTable__I($$outer);
  this.es$2 = this.iterTable$2.u[this.idx$2];
  return this
});
ScalaJS.c.scm_HashTable$$anon$1.prototype.next__O = (function() {
  return this.next__scm_HashEntry()
});
ScalaJS.c.scm_HashTable$$anon$1.prototype.next__scm_HashEntry = (function() {
  var res = this.es$2;
  this.es$2 = ScalaJS.as.scm_HashEntry(this.es$2.next$1);
  while (((this.es$2 === null) && (this.idx$2 > 0))) {
    this.idx$2 = (((-1) + this.idx$2) | 0);
    this.es$2 = this.iterTable$2.u[this.idx$2]
  };
  return res
});
ScalaJS.c.scm_HashTable$$anon$1.prototype.hasNext__Z = (function() {
  return (this.es$2 !== null)
});
ScalaJS.d.scm_HashTable$$anon$1 = new ScalaJS.ClassTypeData({
  scm_HashTable$$anon$1: 0
}, false, "scala.collection.mutable.HashTable$$anon$1", {
  scm_HashTable$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.scm_HashTable$$anon$1.prototype.$classData = ScalaJS.d.scm_HashTable$$anon$1;
/** @constructor */
ScalaJS.c.scm_Iterable$ = (function() {
  ScalaJS.c.scg_GenTraversableFactory.call(this)
});
ScalaJS.c.scm_Iterable$.prototype = new ScalaJS.h.scg_GenTraversableFactory();
ScalaJS.c.scm_Iterable$.prototype.constructor = ScalaJS.c.scm_Iterable$;
/** @constructor */
ScalaJS.h.scm_Iterable$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_Iterable$.prototype = ScalaJS.c.scm_Iterable$.prototype;
ScalaJS.c.scm_Iterable$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_ArrayBuffer().init___()
});
ScalaJS.d.scm_Iterable$ = new ScalaJS.ClassTypeData({
  scm_Iterable$: 0
}, false, "scala.collection.mutable.Iterable$", {
  scm_Iterable$: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.scm_Iterable$.prototype.$classData = ScalaJS.d.scm_Iterable$;
ScalaJS.n.scm_Iterable$ = (void 0);
ScalaJS.m.scm_Iterable$ = (function() {
  if ((!ScalaJS.n.scm_Iterable$)) {
    ScalaJS.n.scm_Iterable$ = new ScalaJS.c.scm_Iterable$().init___()
  };
  return ScalaJS.n.scm_Iterable$
});
/** @constructor */
ScalaJS.c.scm_ListBuffer$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.cursor$2 = null
});
ScalaJS.c.scm_ListBuffer$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.scm_ListBuffer$$anon$1.prototype.constructor = ScalaJS.c.scm_ListBuffer$$anon$1;
/** @constructor */
ScalaJS.h.scm_ListBuffer$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_ListBuffer$$anon$1.prototype = ScalaJS.c.scm_ListBuffer$$anon$1.prototype;
ScalaJS.c.scm_ListBuffer$$anon$1.prototype.init___scm_ListBuffer = (function($$outer) {
  this.cursor$2 = ($$outer.scala$collection$mutable$ListBuffer$$start$6.isEmpty__Z() ? ScalaJS.m.sci_Nil$() : $$outer.scala$collection$mutable$ListBuffer$$start$6);
  return this
});
ScalaJS.c.scm_ListBuffer$$anon$1.prototype.next__O = (function() {
  if ((!this.hasNext__Z())) {
    throw new ScalaJS.c.ju_NoSuchElementException().init___T("next on empty Iterator")
  } else {
    var ans = this.cursor$2.head__O();
    this.cursor$2 = ScalaJS.as.sci_List(this.cursor$2.tail__O());
    return ans
  }
});
ScalaJS.c.scm_ListBuffer$$anon$1.prototype.hasNext__Z = (function() {
  return (this.cursor$2 !== ScalaJS.m.sci_Nil$())
});
ScalaJS.d.scm_ListBuffer$$anon$1 = new ScalaJS.ClassTypeData({
  scm_ListBuffer$$anon$1: 0
}, false, "scala.collection.mutable.ListBuffer$$anon$1", {
  scm_ListBuffer$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.scm_ListBuffer$$anon$1.prototype.$classData = ScalaJS.d.scm_ListBuffer$$anon$1;
/** @constructor */
ScalaJS.c.sr_NonLocalReturnControl = (function() {
  ScalaJS.c.jl_Throwable.call(this);
  this.key$2 = null;
  this.value$f = null
});
ScalaJS.c.sr_NonLocalReturnControl.prototype = new ScalaJS.h.jl_Throwable();
ScalaJS.c.sr_NonLocalReturnControl.prototype.constructor = ScalaJS.c.sr_NonLocalReturnControl;
/** @constructor */
ScalaJS.h.sr_NonLocalReturnControl = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_NonLocalReturnControl.prototype = ScalaJS.c.sr_NonLocalReturnControl.prototype;
ScalaJS.c.sr_NonLocalReturnControl.prototype.fillInStackTrace__jl_Throwable = (function() {
  return this
});
ScalaJS.c.sr_NonLocalReturnControl.prototype.init___O__O = (function(key, value) {
  this.key$2 = key;
  this.value$f = value;
  ScalaJS.c.jl_Throwable.prototype.init___.call(this);
  return this
});
ScalaJS.c.sr_NonLocalReturnControl.prototype.scala$util$control$NoStackTrace$$super$fillInStackTrace__jl_Throwable = (function() {
  return ScalaJS.c.jl_Throwable.prototype.fillInStackTrace__jl_Throwable.call(this)
});
ScalaJS.is.sr_NonLocalReturnControl = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sr_NonLocalReturnControl)))
});
ScalaJS.as.sr_NonLocalReturnControl = (function(obj) {
  return ((ScalaJS.is.sr_NonLocalReturnControl(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.runtime.NonLocalReturnControl"))
});
ScalaJS.isArrayOf.sr_NonLocalReturnControl = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sr_NonLocalReturnControl)))
});
ScalaJS.asArrayOf.sr_NonLocalReturnControl = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sr_NonLocalReturnControl(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.runtime.NonLocalReturnControl;", depth))
});
/** @constructor */
ScalaJS.c.sr_ScalaRunTime$$anon$1 = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.c$2 = 0;
  this.cmax$2 = 0;
  this.x$2$2 = null
});
ScalaJS.c.sr_ScalaRunTime$$anon$1.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sr_ScalaRunTime$$anon$1.prototype.constructor = ScalaJS.c.sr_ScalaRunTime$$anon$1;
/** @constructor */
ScalaJS.h.sr_ScalaRunTime$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_ScalaRunTime$$anon$1.prototype = ScalaJS.c.sr_ScalaRunTime$$anon$1.prototype;
ScalaJS.c.sr_ScalaRunTime$$anon$1.prototype.next__O = (function() {
  var result = this.x$2$2.productElement__I__O(this.c$2);
  this.c$2 = ((1 + this.c$2) | 0);
  return result
});
ScalaJS.c.sr_ScalaRunTime$$anon$1.prototype.init___s_Product = (function(x$2) {
  this.x$2$2 = x$2;
  this.c$2 = 0;
  this.cmax$2 = x$2.productArity__I();
  return this
});
ScalaJS.c.sr_ScalaRunTime$$anon$1.prototype.hasNext__Z = (function() {
  return (this.c$2 < this.cmax$2)
});
ScalaJS.d.sr_ScalaRunTime$$anon$1 = new ScalaJS.ClassTypeData({
  sr_ScalaRunTime$$anon$1: 0
}, false, "scala.runtime.ScalaRunTime$$anon$1", {
  sr_ScalaRunTime$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sr_ScalaRunTime$$anon$1.prototype.$classData = ScalaJS.d.sr_ScalaRunTime$$anon$1;
/** @constructor */
ScalaJS.c.Lgame_Game$$anonfun$1 = (function() {
  ScalaJS.c.sr_AbstractPartialFunction.call(this)
});
ScalaJS.c.Lgame_Game$$anonfun$1.prototype = new ScalaJS.h.sr_AbstractPartialFunction();
ScalaJS.c.Lgame_Game$$anonfun$1.prototype.constructor = ScalaJS.c.Lgame_Game$$anonfun$1;
/** @constructor */
ScalaJS.h.Lgame_Game$$anonfun$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.Lgame_Game$$anonfun$1.prototype = ScalaJS.c.Lgame_Game$$anonfun$1.prototype;
ScalaJS.c.Lgame_Game$$anonfun$1.prototype.applyOrElse__Lobjects_Actor__F1__O = (function(x1, default$2) {
  if (ScalaJS.is.Lobjects_DummyItem(x1)) {
    var x2 = ScalaJS.as.Lobjects_DummyItem(x1);
    return x2
  } else {
    return default$2.apply__O__O(x1)
  }
});
ScalaJS.c.Lgame_Game$$anonfun$1.prototype.isDefinedAt__O__Z = (function(x) {
  return this.isDefinedAt__Lobjects_Actor__Z(ScalaJS.as.Lobjects_Actor(x))
});
ScalaJS.c.Lgame_Game$$anonfun$1.prototype.applyOrElse__O__F1__O = (function(x, default$2) {
  return this.applyOrElse__Lobjects_Actor__F1__O(ScalaJS.as.Lobjects_Actor(x), default$2)
});
ScalaJS.c.Lgame_Game$$anonfun$1.prototype.isDefinedAt__Lobjects_Actor__Z = (function(x1) {
  return ScalaJS.is.Lobjects_DummyItem(x1)
});
ScalaJS.c.Lgame_Game$$anonfun$1.prototype.init___Lgame_Game = (function($$outer) {
  return this
});
ScalaJS.d.Lgame_Game$$anonfun$1 = new ScalaJS.ClassTypeData({
  Lgame_Game$$anonfun$1: 0
}, false, "game.Game$$anonfun$1", {
  Lgame_Game$$anonfun$1: 1,
  sr_AbstractPartialFunction: 1,
  O: 1,
  F1: 1,
  s_PartialFunction: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.Lgame_Game$$anonfun$1.prototype.$classData = ScalaJS.d.Lgame_Game$$anonfun$1;
/** @constructor */
ScalaJS.c.Ljava_io_PrintStream = (function() {
  ScalaJS.c.Ljava_io_FilterOutputStream.call(this);
  this.autoFlush$3 = false;
  this.charset$3 = null;
  this.encoder$3 = null;
  this.closing$3 = false;
  this.java$io$PrintStream$$closed$3 = false;
  this.errorFlag$3 = false;
  this.bitmap$0$3 = false
});
ScalaJS.c.Ljava_io_PrintStream.prototype = new ScalaJS.h.Ljava_io_FilterOutputStream();
ScalaJS.c.Ljava_io_PrintStream.prototype.constructor = ScalaJS.c.Ljava_io_PrintStream;
/** @constructor */
ScalaJS.h.Ljava_io_PrintStream = (function() {
  /*<skip>*/
});
ScalaJS.h.Ljava_io_PrintStream.prototype = ScalaJS.c.Ljava_io_PrintStream.prototype;
ScalaJS.c.Ljava_io_PrintStream.prototype.append__jl_CharSequence__jl_Appendable = (function(x$1) {
  return this.append__jl_CharSequence__Ljava_io_PrintStream(x$1)
});
ScalaJS.c.Ljava_io_PrintStream.prototype.println__O__V = (function(obj) {
  this.print__O__V(obj);
  this.printString__p4__T__V("\n")
});
ScalaJS.c.Ljava_io_PrintStream.prototype.init___Ljava_io_OutputStream__Z__Ljava_nio_charset_Charset = (function(_out, autoFlush, charset) {
  this.autoFlush$3 = autoFlush;
  this.charset$3 = charset;
  ScalaJS.c.Ljava_io_FilterOutputStream.prototype.init___Ljava_io_OutputStream.call(this, _out);
  this.closing$3 = false;
  this.java$io$PrintStream$$closed$3 = false;
  this.errorFlag$3 = false;
  return this
});
ScalaJS.c.Ljava_io_PrintStream.prototype.append__jl_CharSequence__Ljava_io_PrintStream = (function(csq) {
  this.print__T__V(((csq === null) ? "null" : ScalaJS.objectToString(csq)));
  return this
});
ScalaJS.c.Ljava_io_PrintStream.prototype.append__C__jl_Appendable = (function(x$1) {
  return this.append__C__Ljava_io_PrintStream(x$1)
});
ScalaJS.c.Ljava_io_PrintStream.prototype.append__C__Ljava_io_PrintStream = (function(c) {
  this.print__C__V(c);
  return this
});
ScalaJS.c.Ljava_io_PrintStream.prototype.init___Ljava_io_OutputStream = (function(out) {
  ScalaJS.c.Ljava_io_PrintStream.prototype.init___Ljava_io_OutputStream__Z__Ljava_nio_charset_Charset.call(this, out, false, null);
  return this
});
ScalaJS.is.Ljava_io_PrintStream = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Ljava_io_PrintStream)))
});
ScalaJS.as.Ljava_io_PrintStream = (function(obj) {
  return ((ScalaJS.is.Ljava_io_PrintStream(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "java.io.PrintStream"))
});
ScalaJS.isArrayOf.Ljava_io_PrintStream = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Ljava_io_PrintStream)))
});
ScalaJS.asArrayOf.Ljava_io_PrintStream = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.Ljava_io_PrintStream(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Ljava.io.PrintStream;", depth))
});
/** @constructor */
ScalaJS.c.T2 = (function() {
  ScalaJS.c.O.call(this);
  this.$$und1$f = null;
  this.$$und2$f = null
});
ScalaJS.c.T2.prototype = new ScalaJS.h.O();
ScalaJS.c.T2.prototype.constructor = ScalaJS.c.T2;
/** @constructor */
ScalaJS.h.T2 = (function() {
  /*<skip>*/
});
ScalaJS.h.T2.prototype = ScalaJS.c.T2.prototype;
ScalaJS.c.T2.prototype.$$und1$mcI$sp__I = (function() {
  return ScalaJS.uI(this.$$und1__O())
});
ScalaJS.c.T2.prototype.productPrefix__T = (function() {
  return "Tuple2"
});
ScalaJS.c.T2.prototype.productArity__I = (function() {
  return 2
});
ScalaJS.c.T2.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if (ScalaJS.is.T2(x$1)) {
    var Tuple2$1 = ScalaJS.as.T2(x$1);
    return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(this.$$und1__O(), Tuple2$1.$$und1__O()) && ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(this.$$und2__O(), Tuple2$1.$$und2__O()))
  } else {
    return false
  }
});
ScalaJS.c.T2.prototype.productElement__I__O = (function(n) {
  return ScalaJS.s.s_Product2$class__productElement__s_Product2__I__O(this, n)
});
ScalaJS.c.T2.prototype.init___O__O = (function(_1, _2) {
  this.$$und1$f = _1;
  this.$$und2$f = _2;
  return this
});
ScalaJS.c.T2.prototype.toString__T = (function() {
  return (((("(" + this.$$und1__O()) + ",") + this.$$und2__O()) + ")")
});
ScalaJS.c.T2.prototype.$$und2__O = (function() {
  return this.$$und2$f
});
ScalaJS.c.T2.prototype.$$und2$mcI$sp__I = (function() {
  return ScalaJS.uI(this.$$und2__O())
});
ScalaJS.c.T2.prototype.hashCode__I = (function() {
  var this$2 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
ScalaJS.c.T2.prototype.$$und1__O = (function() {
  return this.$$und1$f
});
ScalaJS.c.T2.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.is.T2 = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.T2)))
});
ScalaJS.as.T2 = (function(obj) {
  return ((ScalaJS.is.T2(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.Tuple2"))
});
ScalaJS.isArrayOf.T2 = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.T2)))
});
ScalaJS.asArrayOf.T2 = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.T2(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.Tuple2;", depth))
});
ScalaJS.d.T2 = new ScalaJS.ClassTypeData({
  T2: 0
}, false, "scala.Tuple2", {
  T2: 1,
  O: 1,
  s_Product2: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.T2.prototype.$classData = ScalaJS.d.T2;
/** @constructor */
ScalaJS.c.jl_NumberFormatException = (function() {
  ScalaJS.c.jl_IllegalArgumentException.call(this)
});
ScalaJS.c.jl_NumberFormatException.prototype = new ScalaJS.h.jl_IllegalArgumentException();
ScalaJS.c.jl_NumberFormatException.prototype.constructor = ScalaJS.c.jl_NumberFormatException;
/** @constructor */
ScalaJS.h.jl_NumberFormatException = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_NumberFormatException.prototype = ScalaJS.c.jl_NumberFormatException.prototype;
ScalaJS.d.jl_NumberFormatException = new ScalaJS.ClassTypeData({
  jl_NumberFormatException: 0
}, false, "java.lang.NumberFormatException", {
  jl_NumberFormatException: 1,
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.jl_NumberFormatException.prototype.$classData = ScalaJS.d.jl_NumberFormatException;
/** @constructor */
ScalaJS.c.ju_FormatterClosedException = (function() {
  ScalaJS.c.jl_IllegalStateException.call(this)
});
ScalaJS.c.ju_FormatterClosedException.prototype = new ScalaJS.h.jl_IllegalStateException();
ScalaJS.c.ju_FormatterClosedException.prototype.constructor = ScalaJS.c.ju_FormatterClosedException;
/** @constructor */
ScalaJS.h.ju_FormatterClosedException = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_FormatterClosedException.prototype = ScalaJS.c.ju_FormatterClosedException.prototype;
ScalaJS.d.ju_FormatterClosedException = new ScalaJS.ClassTypeData({
  ju_FormatterClosedException: 0
}, false, "java.util.FormatterClosedException", {
  ju_FormatterClosedException: 1,
  jl_IllegalStateException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.ju_FormatterClosedException.prototype.$classData = ScalaJS.d.ju_FormatterClosedException;
/** @constructor */
ScalaJS.c.ju_IllegalFormatException = (function() {
  ScalaJS.c.jl_IllegalArgumentException.call(this)
});
ScalaJS.c.ju_IllegalFormatException.prototype = new ScalaJS.h.jl_IllegalArgumentException();
ScalaJS.c.ju_IllegalFormatException.prototype.constructor = ScalaJS.c.ju_IllegalFormatException;
/** @constructor */
ScalaJS.h.ju_IllegalFormatException = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_IllegalFormatException.prototype = ScalaJS.c.ju_IllegalFormatException.prototype;
/** @constructor */
ScalaJS.c.s_None$ = (function() {
  ScalaJS.c.s_Option.call(this)
});
ScalaJS.c.s_None$.prototype = new ScalaJS.h.s_Option();
ScalaJS.c.s_None$.prototype.constructor = ScalaJS.c.s_None$;
/** @constructor */
ScalaJS.h.s_None$ = (function() {
  /*<skip>*/
});
ScalaJS.h.s_None$.prototype = ScalaJS.c.s_None$.prototype;
ScalaJS.c.s_None$.prototype.productPrefix__T = (function() {
  return "None"
});
ScalaJS.c.s_None$.prototype.productArity__I = (function() {
  return 0
});
ScalaJS.c.s_None$.prototype.isEmpty__Z = (function() {
  return true
});
ScalaJS.c.s_None$.prototype.get__O = (function() {
  this.get__sr_Nothing$()
});
ScalaJS.c.s_None$.prototype.productElement__I__O = (function(x$1) {
  matchEnd3: {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + x$1))
  }
});
ScalaJS.c.s_None$.prototype.toString__T = (function() {
  return "None"
});
ScalaJS.c.s_None$.prototype.get__sr_Nothing$ = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("None.get")
});
ScalaJS.c.s_None$.prototype.hashCode__I = (function() {
  return 2433880
});
ScalaJS.c.s_None$.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.d.s_None$ = new ScalaJS.ClassTypeData({
  s_None$: 0
}, false, "scala.None$", {
  s_None$: 1,
  s_Option: 1,
  O: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_None$.prototype.$classData = ScalaJS.d.s_None$;
ScalaJS.n.s_None$ = (void 0);
ScalaJS.m.s_None$ = (function() {
  if ((!ScalaJS.n.s_None$)) {
    ScalaJS.n.s_None$ = new ScalaJS.c.s_None$().init___()
  };
  return ScalaJS.n.s_None$
});
/** @constructor */
ScalaJS.c.s_PartialFunction$$anonfun$4 = (function() {
  ScalaJS.c.sr_AbstractPartialFunction.call(this)
});
ScalaJS.c.s_PartialFunction$$anonfun$4.prototype = new ScalaJS.h.sr_AbstractPartialFunction();
ScalaJS.c.s_PartialFunction$$anonfun$4.prototype.constructor = ScalaJS.c.s_PartialFunction$$anonfun$4;
/** @constructor */
ScalaJS.h.s_PartialFunction$$anonfun$4 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_PartialFunction$$anonfun$4.prototype = ScalaJS.c.s_PartialFunction$$anonfun$4.prototype;
ScalaJS.c.s_PartialFunction$$anonfun$4.prototype.isDefinedAt__O__Z = (function(x1) {
  return true
});
ScalaJS.c.s_PartialFunction$$anonfun$4.prototype.applyOrElse__O__F1__O = (function(x1, default$2) {
  return ScalaJS.m.s_PartialFunction$().scala$PartialFunction$$fallback$undpf$f
});
ScalaJS.d.s_PartialFunction$$anonfun$4 = new ScalaJS.ClassTypeData({
  s_PartialFunction$$anonfun$4: 0
}, false, "scala.PartialFunction$$anonfun$4", {
  s_PartialFunction$$anonfun$4: 1,
  sr_AbstractPartialFunction: 1,
  O: 1,
  F1: 1,
  s_PartialFunction: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_PartialFunction$$anonfun$4.prototype.$classData = ScalaJS.d.s_PartialFunction$$anonfun$4;
/** @constructor */
ScalaJS.c.s_Some = (function() {
  ScalaJS.c.s_Option.call(this);
  this.x$2 = null
});
ScalaJS.c.s_Some.prototype = new ScalaJS.h.s_Option();
ScalaJS.c.s_Some.prototype.constructor = ScalaJS.c.s_Some;
/** @constructor */
ScalaJS.h.s_Some = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Some.prototype = ScalaJS.c.s_Some.prototype;
ScalaJS.c.s_Some.prototype.productPrefix__T = (function() {
  return "Some"
});
ScalaJS.c.s_Some.prototype.productArity__I = (function() {
  return 1
});
ScalaJS.c.s_Some.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if (ScalaJS.is.s_Some(x$1)) {
    var Some$1 = ScalaJS.as.s_Some(x$1);
    return ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(this.x$2, Some$1.x$2)
  } else {
    return false
  }
});
ScalaJS.c.s_Some.prototype.isEmpty__Z = (function() {
  return false
});
ScalaJS.c.s_Some.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0:
      {
        return this.x$2;
        break
      };
    default:
      throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + x$1));
  }
});
ScalaJS.c.s_Some.prototype.get__O = (function() {
  return this.x$2
});
ScalaJS.c.s_Some.prototype.toString__T = (function() {
  return ScalaJS.m.sr_ScalaRunTime$().$$undtoString__s_Product__T(this)
});
ScalaJS.c.s_Some.prototype.init___O = (function(x) {
  this.x$2 = x;
  return this
});
ScalaJS.c.s_Some.prototype.hashCode__I = (function() {
  var this$2 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
ScalaJS.c.s_Some.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.is.s_Some = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_Some)))
});
ScalaJS.as.s_Some = (function(obj) {
  return ((ScalaJS.is.s_Some(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.Some"))
});
ScalaJS.isArrayOf.s_Some = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_Some)))
});
ScalaJS.asArrayOf.s_Some = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.s_Some(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.Some;", depth))
});
ScalaJS.d.s_Some = new ScalaJS.ClassTypeData({
  s_Some: 0
}, false, "scala.Some", {
  s_Some: 1,
  s_Option: 1,
  O: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_Some.prototype.$classData = ScalaJS.d.s_Some;
/** @constructor */
ScalaJS.c.s_StringContext$InvalidEscapeException = (function() {
  ScalaJS.c.jl_IllegalArgumentException.call(this);
  this.index$5 = 0
});
ScalaJS.c.s_StringContext$InvalidEscapeException.prototype = new ScalaJS.h.jl_IllegalArgumentException();
ScalaJS.c.s_StringContext$InvalidEscapeException.prototype.constructor = ScalaJS.c.s_StringContext$InvalidEscapeException;
/** @constructor */
ScalaJS.h.s_StringContext$InvalidEscapeException = (function() {
  /*<skip>*/
});
ScalaJS.h.s_StringContext$InvalidEscapeException.prototype = ScalaJS.c.s_StringContext$InvalidEscapeException.prototype;
ScalaJS.c.s_StringContext$InvalidEscapeException.prototype.init___T__I = (function(str, index) {
  this.index$5 = index;
  var jsx$3 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["invalid escape ", " index ", " in \"", "\". Use \\\\\\\\ for literal \\\\."]));
  ScalaJS.m.s_Predef$().require__Z__V(((index >= 0) && (index < ScalaJS.uI(str["length"]))));
  if ((index === (((-1) + ScalaJS.uI(str["length"])) | 0))) {
    var jsx$1 = "at terminal"
  } else {
    var jsx$2 = new ScalaJS.c.s_StringContext().init___sc_Seq(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array(["'\\\\", "' not one of ", " at"]));
    var index$1 = ((1 + index) | 0);
    var c = (65535 & ScalaJS.uI(str["charCodeAt"](index$1)));
    var jsx$1 = jsx$2.s__sc_Seq__T(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([new ScalaJS.c.jl_Character().init___C(c), "[\\b, \\t, \\n, \\f, \\r, \\\\, \\\", \\']"]))
  };
  ScalaJS.c.jl_IllegalArgumentException.prototype.init___T.call(this, jsx$3.s__sc_Seq__T(new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([jsx$1, index, str])));
  return this
});
ScalaJS.d.s_StringContext$InvalidEscapeException = new ScalaJS.ClassTypeData({
  s_StringContext$InvalidEscapeException: 0
}, false, "scala.StringContext$InvalidEscapeException", {
  s_StringContext$InvalidEscapeException: 1,
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.s_StringContext$InvalidEscapeException.prototype.$classData = ScalaJS.d.s_StringContext$InvalidEscapeException;
ScalaJS.is.sc_TraversableLike = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_TraversableLike)))
});
ScalaJS.as.sc_TraversableLike = (function(obj) {
  return ((ScalaJS.is.sc_TraversableLike(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.TraversableLike"))
});
ScalaJS.isArrayOf.sc_TraversableLike = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_TraversableLike)))
});
ScalaJS.asArrayOf.sc_TraversableLike = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_TraversableLike(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.TraversableLike;", depth))
});
/** @constructor */
ScalaJS.c.scg_SeqFactory = (function() {
  ScalaJS.c.scg_GenSeqFactory.call(this)
});
ScalaJS.c.scg_SeqFactory.prototype = new ScalaJS.h.scg_GenSeqFactory();
ScalaJS.c.scg_SeqFactory.prototype.constructor = ScalaJS.c.scg_SeqFactory;
/** @constructor */
ScalaJS.h.scg_SeqFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_SeqFactory.prototype = ScalaJS.c.scg_SeqFactory.prototype;
/** @constructor */
ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1 = (function() {
  ScalaJS.c.sci_TrieIterator.call(this)
});
ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1.prototype = new ScalaJS.h.sci_TrieIterator();
ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1.prototype.constructor = ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1;
/** @constructor */
ScalaJS.h.sci_HashMap$HashTrieMap$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$HashTrieMap$$anon$1.prototype = ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1.prototype;
ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1.prototype.init___sci_HashMap$HashTrieMap = (function($$outer) {
  ScalaJS.c.sci_TrieIterator.prototype.init___Asci_Iterable.call(this, $$outer.elems$6);
  return this
});
ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1.prototype.getElem__O__O = (function(x) {
  return ScalaJS.as.sci_HashMap$HashMap1(x).ensurePair__T2()
});
ScalaJS.d.sci_HashMap$HashTrieMap$$anon$1 = new ScalaJS.ClassTypeData({
  sci_HashMap$HashTrieMap$$anon$1: 0
}, false, "scala.collection.immutable.HashMap$HashTrieMap$$anon$1", {
  sci_HashMap$HashTrieMap$$anon$1: 1,
  sci_TrieIterator: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1.prototype.$classData = ScalaJS.d.sci_HashMap$HashTrieMap$$anon$1;
/** @constructor */
ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1 = (function() {
  ScalaJS.c.sci_TrieIterator.call(this)
});
ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1.prototype = new ScalaJS.h.sci_TrieIterator();
ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1.prototype.constructor = ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1;
/** @constructor */
ScalaJS.h.sci_HashSet$HashTrieSet$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet$HashTrieSet$$anon$1.prototype = ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1.prototype;
ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1.prototype.init___sci_HashSet$HashTrieSet = (function($$outer) {
  ScalaJS.c.sci_TrieIterator.prototype.init___Asci_Iterable.call(this, $$outer.elems$5);
  return this
});
ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1.prototype.getElem__O__O = (function(cc) {
  return ScalaJS.as.sci_HashSet$HashSet1(cc).key$6
});
ScalaJS.d.sci_HashSet$HashTrieSet$$anon$1 = new ScalaJS.ClassTypeData({
  sci_HashSet$HashTrieSet$$anon$1: 0
}, false, "scala.collection.immutable.HashSet$HashTrieSet$$anon$1", {
  sci_HashSet$HashTrieSet$$anon$1: 1,
  sci_TrieIterator: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1.prototype.$classData = ScalaJS.d.sci_HashSet$HashTrieSet$$anon$1;
/** @constructor */
ScalaJS.c.sci_Set$ = (function() {
  ScalaJS.c.scg_ImmutableSetFactory.call(this)
});
ScalaJS.c.sci_Set$.prototype = new ScalaJS.h.scg_ImmutableSetFactory();
ScalaJS.c.sci_Set$.prototype.constructor = ScalaJS.c.sci_Set$;
/** @constructor */
ScalaJS.h.sci_Set$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Set$.prototype = ScalaJS.c.sci_Set$.prototype;
ScalaJS.c.sci_Set$.prototype.emptyInstance__sci_Set = (function() {
  return ScalaJS.m.sci_Set$EmptySet$()
});
ScalaJS.d.sci_Set$ = new ScalaJS.ClassTypeData({
  sci_Set$: 0
}, false, "scala.collection.immutable.Set$", {
  sci_Set$: 1,
  scg_ImmutableSetFactory: 1,
  scg_SetFactory: 1,
  scg_GenSetFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sci_Set$.prototype.$classData = ScalaJS.d.sci_Set$;
ScalaJS.n.sci_Set$ = (void 0);
ScalaJS.m.sci_Set$ = (function() {
  if ((!ScalaJS.n.sci_Set$)) {
    ScalaJS.n.sci_Set$ = new ScalaJS.c.sci_Set$().init___()
  };
  return ScalaJS.n.sci_Set$
});
/** @constructor */
ScalaJS.c.sci_VectorIterator = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.endIndex$2 = 0;
  this.blockIndex$2 = 0;
  this.lo$2 = 0;
  this.endLo$2 = 0;
  this.$$undhasNext$2 = false;
  this.depth$2 = 0;
  this.display0$2 = null;
  this.display1$2 = null;
  this.display2$2 = null;
  this.display3$2 = null;
  this.display4$2 = null;
  this.display5$2 = null
});
ScalaJS.c.sci_VectorIterator.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sci_VectorIterator.prototype.constructor = ScalaJS.c.sci_VectorIterator;
/** @constructor */
ScalaJS.h.sci_VectorIterator = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_VectorIterator.prototype = ScalaJS.c.sci_VectorIterator.prototype;
ScalaJS.c.sci_VectorIterator.prototype.next__O = (function() {
  if ((!this.$$undhasNext$2)) {
    throw new ScalaJS.c.ju_NoSuchElementException().init___T("reached iterator end")
  };
  var res = this.display0$2.u[this.lo$2];
  this.lo$2 = ((1 + this.lo$2) | 0);
  if ((this.lo$2 === this.endLo$2)) {
    if ((((this.blockIndex$2 + this.lo$2) | 0) < this.endIndex$2)) {
      var newBlockIndex = ((32 + this.blockIndex$2) | 0);
      var xor = (this.blockIndex$2 ^ newBlockIndex);
      ScalaJS.s.sci_VectorPointer$class__gotoNextBlockStart__sci_VectorPointer__I__I__V(this, newBlockIndex, xor);
      this.blockIndex$2 = newBlockIndex;
      var x = ((this.endIndex$2 - this.blockIndex$2) | 0);
      this.endLo$2 = ((x < 32) ? x : 32);
      this.lo$2 = 0
    } else {
      this.$$undhasNext$2 = false
    }
  };
  return res
});
ScalaJS.c.sci_VectorIterator.prototype.display3__AO = (function() {
  return this.display3$2
});
ScalaJS.c.sci_VectorIterator.prototype.depth__I = (function() {
  return this.depth$2
});
ScalaJS.c.sci_VectorIterator.prototype.display5$und$eq__AO__V = (function(x$1) {
  this.display5$2 = x$1
});
ScalaJS.c.sci_VectorIterator.prototype.init___I__I = (function(_startIndex, endIndex) {
  this.endIndex$2 = endIndex;
  this.blockIndex$2 = ((-32) & _startIndex);
  this.lo$2 = (31 & _startIndex);
  var x = ((endIndex - this.blockIndex$2) | 0);
  this.endLo$2 = ((x < 32) ? x : 32);
  this.$$undhasNext$2 = (((this.blockIndex$2 + this.lo$2) | 0) < endIndex);
  return this
});
ScalaJS.c.sci_VectorIterator.prototype.display0__AO = (function() {
  return this.display0$2
});
ScalaJS.c.sci_VectorIterator.prototype.display4__AO = (function() {
  return this.display4$2
});
ScalaJS.c.sci_VectorIterator.prototype.display2$und$eq__AO__V = (function(x$1) {
  this.display2$2 = x$1
});
ScalaJS.c.sci_VectorIterator.prototype.display1$und$eq__AO__V = (function(x$1) {
  this.display1$2 = x$1
});
ScalaJS.c.sci_VectorIterator.prototype.hasNext__Z = (function() {
  return this.$$undhasNext$2
});
ScalaJS.c.sci_VectorIterator.prototype.display4$und$eq__AO__V = (function(x$1) {
  this.display4$2 = x$1
});
ScalaJS.c.sci_VectorIterator.prototype.display1__AO = (function() {
  return this.display1$2
});
ScalaJS.c.sci_VectorIterator.prototype.display5__AO = (function() {
  return this.display5$2
});
ScalaJS.c.sci_VectorIterator.prototype.depth$und$eq__I__V = (function(x$1) {
  this.depth$2 = x$1
});
ScalaJS.c.sci_VectorIterator.prototype.display2__AO = (function() {
  return this.display2$2
});
ScalaJS.c.sci_VectorIterator.prototype.display0$und$eq__AO__V = (function(x$1) {
  this.display0$2 = x$1
});
ScalaJS.c.sci_VectorIterator.prototype.display3$und$eq__AO__V = (function(x$1) {
  this.display3$2 = x$1
});
ScalaJS.d.sci_VectorIterator = new ScalaJS.ClassTypeData({
  sci_VectorIterator: 0
}, false, "scala.collection.immutable.VectorIterator", {
  sci_VectorIterator: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sci_VectorPointer: 1
});
ScalaJS.c.sci_VectorIterator.prototype.$classData = ScalaJS.d.sci_VectorIterator;
/** @constructor */
ScalaJS.c.scm_HashMap$ = (function() {
  ScalaJS.c.scg_MutableMapFactory.call(this)
});
ScalaJS.c.scm_HashMap$.prototype = new ScalaJS.h.scg_MutableMapFactory();
ScalaJS.c.scm_HashMap$.prototype.constructor = ScalaJS.c.scm_HashMap$;
/** @constructor */
ScalaJS.h.scm_HashMap$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_HashMap$.prototype = ScalaJS.c.scm_HashMap$.prototype;
ScalaJS.c.scm_HashMap$.prototype.empty__sc_GenMap = (function() {
  return new ScalaJS.c.scm_HashMap().init___()
});
ScalaJS.d.scm_HashMap$ = new ScalaJS.ClassTypeData({
  scm_HashMap$: 0
}, false, "scala.collection.mutable.HashMap$", {
  scm_HashMap$: 1,
  scg_MutableMapFactory: 1,
  scg_MapFactory: 1,
  scg_GenMapFactory: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_HashMap$.prototype.$classData = ScalaJS.d.scm_HashMap$;
ScalaJS.n.scm_HashMap$ = (void 0);
ScalaJS.m.scm_HashMap$ = (function() {
  if ((!ScalaJS.n.scm_HashMap$)) {
    ScalaJS.n.scm_HashMap$ = new ScalaJS.c.scm_HashMap$().init___()
  };
  return ScalaJS.n.scm_HashMap$
});
/** @constructor */
ScalaJS.c.scm_Set$ = (function() {
  ScalaJS.c.scg_MutableSetFactory.call(this)
});
ScalaJS.c.scm_Set$.prototype = new ScalaJS.h.scg_MutableSetFactory();
ScalaJS.c.scm_Set$.prototype.constructor = ScalaJS.c.scm_Set$;
/** @constructor */
ScalaJS.h.scm_Set$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_Set$.prototype = ScalaJS.c.scm_Set$.prototype;
ScalaJS.c.scm_Set$.prototype.empty__sc_GenTraversable = (function() {
  return new ScalaJS.c.scm_HashSet().init___()
});
ScalaJS.d.scm_Set$ = new ScalaJS.ClassTypeData({
  scm_Set$: 0
}, false, "scala.collection.mutable.Set$", {
  scm_Set$: 1,
  scg_MutableSetFactory: 1,
  scg_SetFactory: 1,
  scg_GenSetFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.scm_Set$.prototype.$classData = ScalaJS.d.scm_Set$;
ScalaJS.n.scm_Set$ = (void 0);
ScalaJS.m.scm_Set$ = (function() {
  if ((!ScalaJS.n.scm_Set$)) {
    ScalaJS.n.scm_Set$ = new ScalaJS.c.scm_Set$().init___()
  };
  return ScalaJS.n.scm_Set$
});
/** @constructor */
ScalaJS.c.sjsr_UndefinedBehaviorError = (function() {
  ScalaJS.c.jl_Error.call(this)
});
ScalaJS.c.sjsr_UndefinedBehaviorError.prototype = new ScalaJS.h.jl_Error();
ScalaJS.c.sjsr_UndefinedBehaviorError.prototype.constructor = ScalaJS.c.sjsr_UndefinedBehaviorError;
/** @constructor */
ScalaJS.h.sjsr_UndefinedBehaviorError = (function() {
  /*<skip>*/
});
ScalaJS.h.sjsr_UndefinedBehaviorError.prototype = ScalaJS.c.sjsr_UndefinedBehaviorError.prototype;
ScalaJS.c.sjsr_UndefinedBehaviorError.prototype.fillInStackTrace__jl_Throwable = (function() {
  return ScalaJS.c.jl_Throwable.prototype.fillInStackTrace__jl_Throwable.call(this)
});
ScalaJS.c.sjsr_UndefinedBehaviorError.prototype.scala$util$control$NoStackTrace$$super$fillInStackTrace__jl_Throwable = (function() {
  return ScalaJS.c.jl_Throwable.prototype.fillInStackTrace__jl_Throwable.call(this)
});
ScalaJS.c.sjsr_UndefinedBehaviorError.prototype.init___jl_Throwable = (function(cause) {
  ScalaJS.c.sjsr_UndefinedBehaviorError.prototype.init___T__jl_Throwable.call(this, ("An undefined behavior was detected" + ((cause === null) ? "" : (": " + cause.getMessage__T()))), cause);
  return this
});
ScalaJS.c.sjsr_UndefinedBehaviorError.prototype.init___T__jl_Throwable = (function(message, cause) {
  ScalaJS.c.jl_Error.prototype.init___T__jl_Throwable.call(this, message, cause);
  return this
});
ScalaJS.d.sjsr_UndefinedBehaviorError = new ScalaJS.ClassTypeData({
  sjsr_UndefinedBehaviorError: 0
}, false, "scala.scalajs.runtime.UndefinedBehaviorError", {
  sjsr_UndefinedBehaviorError: 1,
  jl_Error: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_util_control_ControlThrowable: 1,
  s_util_control_NoStackTrace: 1
});
ScalaJS.c.sjsr_UndefinedBehaviorError.prototype.$classData = ScalaJS.d.sjsr_UndefinedBehaviorError;
/** @constructor */
ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp = (function() {
  ScalaJS.c.sr_NonLocalReturnControl.call(this);
  this.value$mcZ$sp$f = false
});
ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp.prototype = new ScalaJS.h.sr_NonLocalReturnControl();
ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp.prototype.constructor = ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp;
/** @constructor */
ScalaJS.h.sr_NonLocalReturnControl$mcZ$sp = (function() {
  /*<skip>*/
});
ScalaJS.h.sr_NonLocalReturnControl$mcZ$sp.prototype = ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp.prototype;
ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp.prototype.init___O__Z = (function(key, value$mcZ$sp) {
  this.value$mcZ$sp$f = value$mcZ$sp;
  ScalaJS.c.sr_NonLocalReturnControl.prototype.init___O__O.call(this, key, null);
  return this
});
ScalaJS.d.sr_NonLocalReturnControl$mcZ$sp = new ScalaJS.ClassTypeData({
  sr_NonLocalReturnControl$mcZ$sp: 0
}, false, "scala.runtime.NonLocalReturnControl$mcZ$sp", {
  sr_NonLocalReturnControl$mcZ$sp: 1,
  sr_NonLocalReturnControl: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_util_control_ControlThrowable: 1,
  s_util_control_NoStackTrace: 1
});
ScalaJS.c.sr_NonLocalReturnControl$mcZ$sp.prototype.$classData = ScalaJS.d.sr_NonLocalReturnControl$mcZ$sp;
/** @constructor */
ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1 = (function() {
  ScalaJS.c.sr_AbstractFunction1$mcVI$sp.call(this);
  this.$$outer$3 = null;
  this.g$1$f = null
});
ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1.prototype = new ScalaJS.h.sr_AbstractFunction1$mcVI$sp();
ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1.prototype.constructor = ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1;
/** @constructor */
ScalaJS.h.Lenemies_Charger$$anonfun$aiMove$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.Lenemies_Charger$$anonfun$aiMove$1.prototype = ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1.prototype;
ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1.prototype.apply__O__O = (function(v1) {
  var i = ScalaJS.uI(v1);
  this.apply$mcVI$sp__I__V(i)
});
ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1.prototype.apply$mcVI$sp__I__V = (function(i) {
  if ((this.$$outer$3.state$3 === "charging")) {
    var d = this.$$outer$3.chargeLine$3.unitStep__Lglobalvars_Pt();
    var moveSuccess = this.$$outer$3.moveLoc__D__D__Lgame_Game__Z(d.x$1, d.y$1, this.g$1$f);
    if ((!moveSuccess)) {
      this.$$outer$3.stateMoving__V()
    };
    this.g$1$f.acts$1.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, d$1) {
      return (function(a$2) {
        var a = ScalaJS.as.Lobjects_Actor(a$2);
        var x$2 = arg$outer.$$outer$3;
        if ((((!(a === x$2)) && (a.faction$2 !== "NA")) && a.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(arg$outer.$$outer$3.loc$1.$$plus__Lglobalvars_Pt__Lglobalvars_Pt(d$1), arg$outer.$$outer$3.size$1))) {
          a.takeDamage__Lobjects_Actor__D__D__Lgame_Game__V(arg$outer.$$outer$3, 10.0, 4.0, arg$outer.g$1$f);
          arg$outer.$$outer$3.stateMoving__V()
        }
      })
    })(this, d)))
  }
});
ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1.prototype.init___Lenemies_Charger__Lgame_Game = (function($$outer, g$1) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$3 = $$outer
  };
  this.g$1$f = g$1;
  return this
});
ScalaJS.d.Lenemies_Charger$$anonfun$aiMove$1 = new ScalaJS.ClassTypeData({
  Lenemies_Charger$$anonfun$aiMove$1: 0
}, false, "enemies.Charger$$anonfun$aiMove$1", {
  Lenemies_Charger$$anonfun$aiMove$1: 1,
  sr_AbstractFunction1$mcVI$sp: 1,
  sr_AbstractFunction1: 1,
  O: 1,
  F1: 1,
  s_Function1$mcVI$sp: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.Lenemies_Charger$$anonfun$aiMove$1.prototype.$classData = ScalaJS.d.Lenemies_Charger$$anonfun$aiMove$1;
/** @constructor */
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1 = (function() {
  ScalaJS.c.sr_AbstractFunction1$mcVI$sp.call(this);
  this.$$outer$3 = null;
  this.overlap$1$3 = 0;
  this.minSize$1$3 = 0;
  this.maxSize$1$3 = 0;
  this.rooms$1$3 = null;
  this.last$1$f = null;
  this.lastRelation$1$3 = null
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1.prototype = new ScalaJS.h.sr_AbstractFunction1$mcVI$sp();
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1.prototype.constructor = ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1;
/** @constructor */
ScalaJS.h.Lgame_Game$$anonfun$genFarmhouse$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.Lgame_Game$$anonfun$genFarmhouse$1.prototype = ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1.prototype;
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1.prototype.apply__O__O = (function(v1) {
  var i = ScalaJS.uI(v1);
  this.apply$mcVI$sp__I__V(i)
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1.prototype.apply$mcVI$sp__I__V = (function(i) {
  var this$1 = this.$$outer$3.r$1;
  var n = ((this.maxSize$1$3 - this.minSize$1$3) | 0);
  var height = ((this$1.self$1.nextInt__I__I(n) + this.minSize$1$3) | 0);
  var this$2 = this.$$outer$3.r$1;
  var n$1 = ((this.maxSize$1$3 - this.minSize$1$3) | 0);
  var width = ((this$2.self$1.nextInt__I__I(n$1) + this.minSize$1$3) | 0);
  var newRoom = new ScalaJS.c.sr_ObjectRef().init___O(null);
  var relation = "none";
  var this$4 = this.$$outer$3.r$1;
  var whichWall = this$4.self$1.nextInt__I__I(4);
  if (((whichWall === 0) && (ScalaJS.as.T(this.lastRelation$1$3.elem$1) !== "east"))) {
    newRoom.elem$1 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(((ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.x$1 + ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).size$1.x$1) - this.overlap$1$3), ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.y$1), new ScalaJS.c.Lglobalvars_Pt().init___D__D(width, height));
    relation = "west"
  } else if (((whichWall === 1) && (ScalaJS.as.T(this.lastRelation$1$3.elem$1) !== "south"))) {
    newRoom.elem$1 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.x$1, ((ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.y$1 + ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).size$1.y$1) - this.overlap$1$3)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(width, height));
    relation = "north"
  } else if (((whichWall === 2) && (ScalaJS.as.T(this.lastRelation$1$3.elem$1) !== "west"))) {
    newRoom.elem$1 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(((ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.x$1 - width) + this.overlap$1$3), ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.y$1), new ScalaJS.c.Lglobalvars_Pt().init___D__D(width, height));
    relation = "east"
  } else if (((whichWall === 3) && (ScalaJS.as.T(this.lastRelation$1$3.elem$1) !== "north"))) {
    newRoom.elem$1 = new ScalaJS.c.Lobjects_Wall().init___Lglobalvars_Pt__Lglobalvars_Pt(new ScalaJS.c.Lglobalvars_Pt().init___D__D(ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.x$1, ((ScalaJS.as.Lobjects_Wall(this.last$1$f.elem$1).loc$1.y$1 - height) + this.overlap$1$3)), new ScalaJS.c.Lglobalvars_Pt().init___D__D(width, height));
    relation = "south"
  };
  if (((ScalaJS.as.Lobjects_Wall(newRoom.elem$1) !== null) && ((((ScalaJS.as.Lobjects_Wall(newRoom.elem$1).loc$1.x$1 < 0) || (ScalaJS.as.Lobjects_Wall(newRoom.elem$1).loc$1.y$1 < 0)) || ((ScalaJS.as.Lobjects_Wall(newRoom.elem$1).loc$1.x$1 + ScalaJS.as.Lobjects_Wall(newRoom.elem$1).size$1.x$1) > ScalaJS.m.Lglobalvars_GV$().GAMEX$1)) || ((ScalaJS.as.Lobjects_Wall(newRoom.elem$1).loc$1.y$1 + ScalaJS.as.Lobjects_Wall(newRoom.elem$1).size$1.y$1) > ScalaJS.m.Lglobalvars_GV$().GAMEY$1)))) {
    newRoom.elem$1 = null
  } else if (((ScalaJS.as.Lobjects_Wall(newRoom.elem$1) !== null) && this.$$outer$3.collision__Lobjects_Obj__Z(ScalaJS.as.Lobjects_Wall(newRoom.elem$1)))) {
    newRoom.elem$1 = null
  };
  var this$5 = this.rooms$1$3;
  var p = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(check$ifrefutable$3$2) {
    var check$ifrefutable$3 = ScalaJS.as.T2(check$ifrefutable$3$2);
    return (check$ifrefutable$3 !== null)
  }));
  new ScalaJS.c.sc_TraversableLike$WithFilter().init___sc_TraversableLike__F1(this$5, p).foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(arg$outer, newRoom$1) {
    return (function(x$7$2) {
      var x$7 = ScalaJS.as.T2(x$7$2);
      if ((x$7 !== null)) {
        var r = ScalaJS.as.Lobjects_Wall(x$7.$$und1__O());
        if ((ScalaJS.as.Lobjects_Wall(newRoom$1.elem$1) !== null)) {
          var x$2 = ScalaJS.as.Lobjects_Wall(arg$outer.last$1$f.elem$1);
          var jsx$2 = (!(r === x$2))
        } else {
          var jsx$2 = false
        };
        if (jsx$2) {
          var other = ScalaJS.as.Lobjects_Wall(newRoom$1.elem$1);
          var jsx$1 = r.collides__Lglobalvars_Pt__Lglobalvars_Pt__Z(other.loc$1, other.size$1)
        } else {
          var jsx$1 = false
        };
        if (jsx$1) {
          newRoom$1.elem$1 = null
        }
      } else {
        throw new ScalaJS.c.s_MatchError().init___O(x$7)
      }
    })
  })(this, newRoom)));
  if ((ScalaJS.as.Lobjects_Wall(newRoom.elem$1) !== null)) {
    this.rooms$1$3.$$plus$eq__O__scm_Buffer(new ScalaJS.c.T2().init___O__O(ScalaJS.as.Lobjects_Wall(newRoom.elem$1), relation));
    this.last$1$f.elem$1 = ScalaJS.as.Lobjects_Wall(newRoom.elem$1);
    this.lastRelation$1$3.elem$1 = relation
  }
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1.prototype.init___Lgame_Game__I__I__I__scm_Buffer__sr_ObjectRef__sr_ObjectRef = (function($$outer, overlap$1, minSize$1, maxSize$1, rooms$1, last$1, lastRelation$1) {
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$3 = $$outer
  };
  this.overlap$1$3 = overlap$1;
  this.minSize$1$3 = minSize$1;
  this.maxSize$1$3 = maxSize$1;
  this.rooms$1$3 = rooms$1;
  this.last$1$f = last$1;
  this.lastRelation$1$3 = lastRelation$1;
  return this
});
ScalaJS.d.Lgame_Game$$anonfun$genFarmhouse$1 = new ScalaJS.ClassTypeData({
  Lgame_Game$$anonfun$genFarmhouse$1: 0
}, false, "game.Game$$anonfun$genFarmhouse$1", {
  Lgame_Game$$anonfun$genFarmhouse$1: 1,
  sr_AbstractFunction1$mcVI$sp: 1,
  sr_AbstractFunction1: 1,
  O: 1,
  F1: 1,
  s_Function1$mcVI$sp: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.Lgame_Game$$anonfun$genFarmhouse$1.prototype.$classData = ScalaJS.d.Lgame_Game$$anonfun$genFarmhouse$1;
/** @constructor */
ScalaJS.c.jl_JSConsoleBasedPrintStream = (function() {
  ScalaJS.c.Ljava_io_PrintStream.call(this);
  this.isErr$4 = null;
  this.flushed$4 = false;
  this.buffer$4 = null
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype = new ScalaJS.h.Ljava_io_PrintStream();
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.constructor = ScalaJS.c.jl_JSConsoleBasedPrintStream;
/** @constructor */
ScalaJS.h.jl_JSConsoleBasedPrintStream = (function() {
  /*<skip>*/
});
ScalaJS.h.jl_JSConsoleBasedPrintStream.prototype = ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype;
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.print__T__V = (function(s) {
  this.printString__p4__T__V(((s === null) ? "null" : s))
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.init___jl_Boolean = (function(isErr) {
  this.isErr$4 = isErr;
  ScalaJS.c.Ljava_io_PrintStream.prototype.init___Ljava_io_OutputStream.call(this, new ScalaJS.c.jl_JSConsoleBasedPrintStream$DummyOutputStream().init___());
  this.flushed$4 = true;
  this.buffer$4 = "";
  return this
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.doWriteLine__p4__T__V = (function(line) {
  var x = ScalaJS.g["console"];
  if (ScalaJS.uZ((!(!x)))) {
    var x$1 = this.isErr$4;
    if (ScalaJS.uZ(x$1)) {
      var x$2 = ScalaJS.g["console"]["error"];
      var jsx$1 = ScalaJS.uZ((!(!x$2)))
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      ScalaJS.g["console"]["error"](line)
    } else {
      ScalaJS.g["console"]["log"](line)
    }
  }
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.print__C__V = (function(c) {
  this.printString__p4__T__V(ScalaJS.m.sjsr_RuntimeString$().valueOf__C__T(c))
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.print__O__V = (function(obj) {
  this.printString__p4__T__V(ScalaJS.m.sjsr_RuntimeString$().valueOf__O__T(obj))
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.printString__p4__T__V = (function(s) {
  var rest = s;
  while ((rest !== "")) {
    var thiz = rest;
    var nlPos = ScalaJS.uI(thiz["indexOf"]("\n"));
    if ((nlPos < 0)) {
      this.buffer$4 = (("" + this.buffer$4) + rest);
      this.flushed$4 = false;
      rest = ""
    } else {
      var jsx$1 = this.buffer$4;
      var thiz$1 = rest;
      this.doWriteLine__p4__T__V((("" + jsx$1) + ScalaJS.as.T(thiz$1["substring"](0, nlPos))));
      this.buffer$4 = "";
      this.flushed$4 = true;
      var thiz$2 = rest;
      var beginIndex = ((1 + nlPos) | 0);
      rest = ScalaJS.as.T(thiz$2["substring"](beginIndex))
    }
  }
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.close__V = (function() {
  /*<skip>*/
});
ScalaJS.d.jl_JSConsoleBasedPrintStream = new ScalaJS.ClassTypeData({
  jl_JSConsoleBasedPrintStream: 0
}, false, "java.lang.JSConsoleBasedPrintStream", {
  jl_JSConsoleBasedPrintStream: 1,
  Ljava_io_PrintStream: 1,
  Ljava_io_FilterOutputStream: 1,
  Ljava_io_OutputStream: 1,
  O: 1,
  Ljava_io_Closeable: 1,
  Ljava_io_Flushable: 1,
  jl_Appendable: 1
});
ScalaJS.c.jl_JSConsoleBasedPrintStream.prototype.$classData = ScalaJS.d.jl_JSConsoleBasedPrintStream;
/** @constructor */
ScalaJS.c.ju_FormatFlagsConversionMismatchException = (function() {
  ScalaJS.c.ju_IllegalFormatException.call(this);
  this.c$6 = 0;
  this.f$6 = null
});
ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype = new ScalaJS.h.ju_IllegalFormatException();
ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype.constructor = ScalaJS.c.ju_FormatFlagsConversionMismatchException;
/** @constructor */
ScalaJS.h.ju_FormatFlagsConversionMismatchException = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_FormatFlagsConversionMismatchException.prototype = ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype;
ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype.getMessage__T = (function() {
  var c = this.c$6;
  return ((("Conversion = " + new ScalaJS.c.jl_Character().init___C(c)) + ", Flags = ") + this.f$6)
});
ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype.init___C = (function(c) {
  this.c$6 = c;
  ScalaJS.c.ju_IllegalFormatException.prototype.init___.call(this);
  this.f$6 = null;
  return this
});
ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype.init___T__C = (function(f, c) {
  ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype.init___C.call(this, c);
  if ((f === null)) {
    throw new ScalaJS.c.jl_NullPointerException().init___()
  };
  this.f$6 = f;
  return this
});
ScalaJS.d.ju_FormatFlagsConversionMismatchException = new ScalaJS.ClassTypeData({
  ju_FormatFlagsConversionMismatchException: 0
}, false, "java.util.FormatFlagsConversionMismatchException", {
  ju_FormatFlagsConversionMismatchException: 1,
  ju_IllegalFormatException: 1,
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.ju_FormatFlagsConversionMismatchException.prototype.$classData = ScalaJS.d.ju_FormatFlagsConversionMismatchException;
/** @constructor */
ScalaJS.c.ju_IllegalFormatFlagsException = (function() {
  ScalaJS.c.ju_IllegalFormatException.call(this);
  this.flags$6 = null
});
ScalaJS.c.ju_IllegalFormatFlagsException.prototype = new ScalaJS.h.ju_IllegalFormatException();
ScalaJS.c.ju_IllegalFormatFlagsException.prototype.constructor = ScalaJS.c.ju_IllegalFormatFlagsException;
/** @constructor */
ScalaJS.h.ju_IllegalFormatFlagsException = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_IllegalFormatFlagsException.prototype = ScalaJS.c.ju_IllegalFormatFlagsException.prototype;
ScalaJS.c.ju_IllegalFormatFlagsException.prototype.init___ = (function() {
  ScalaJS.c.ju_IllegalFormatException.prototype.init___.call(this);
  this.flags$6 = null;
  return this
});
ScalaJS.c.ju_IllegalFormatFlagsException.prototype.getMessage__T = (function() {
  return (("Flags = '" + this.flags$6) + "'")
});
ScalaJS.c.ju_IllegalFormatFlagsException.prototype.init___T = (function(f) {
  ScalaJS.c.ju_IllegalFormatFlagsException.prototype.init___.call(this);
  if ((f === null)) {
    throw new ScalaJS.c.jl_NullPointerException().init___()
  };
  this.flags$6 = f;
  return this
});
ScalaJS.d.ju_IllegalFormatFlagsException = new ScalaJS.ClassTypeData({
  ju_IllegalFormatFlagsException: 0
}, false, "java.util.IllegalFormatFlagsException", {
  ju_IllegalFormatFlagsException: 1,
  ju_IllegalFormatException: 1,
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.ju_IllegalFormatFlagsException.prototype.$classData = ScalaJS.d.ju_IllegalFormatFlagsException;
/** @constructor */
ScalaJS.c.ju_MissingFormatArgumentException = (function() {
  ScalaJS.c.ju_IllegalFormatException.call(this);
  this.s$6 = null
});
ScalaJS.c.ju_MissingFormatArgumentException.prototype = new ScalaJS.h.ju_IllegalFormatException();
ScalaJS.c.ju_MissingFormatArgumentException.prototype.constructor = ScalaJS.c.ju_MissingFormatArgumentException;
/** @constructor */
ScalaJS.h.ju_MissingFormatArgumentException = (function() {
  /*<skip>*/
});
ScalaJS.h.ju_MissingFormatArgumentException.prototype = ScalaJS.c.ju_MissingFormatArgumentException.prototype;
ScalaJS.c.ju_MissingFormatArgumentException.prototype.init___ = (function() {
  ScalaJS.c.ju_IllegalFormatException.prototype.init___.call(this);
  this.s$6 = null;
  return this
});
ScalaJS.c.ju_MissingFormatArgumentException.prototype.getMessage__T = (function() {
  return (("Format specifier '" + this.s$6) + "'")
});
ScalaJS.c.ju_MissingFormatArgumentException.prototype.init___T = (function(s) {
  ScalaJS.c.ju_MissingFormatArgumentException.prototype.init___.call(this);
  if ((s === null)) {
    throw new ScalaJS.c.jl_NullPointerException().init___()
  };
  this.s$6 = s;
  return this
});
ScalaJS.d.ju_MissingFormatArgumentException = new ScalaJS.ClassTypeData({
  ju_MissingFormatArgumentException: 0
}, false, "java.util.MissingFormatArgumentException", {
  ju_MissingFormatArgumentException: 1,
  ju_IllegalFormatException: 1,
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.ju_MissingFormatArgumentException.prototype.$classData = ScalaJS.d.ju_MissingFormatArgumentException;
/** @constructor */
ScalaJS.c.sc_Seq$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.sc_Seq$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.sc_Seq$.prototype.constructor = ScalaJS.c.sc_Seq$;
/** @constructor */
ScalaJS.h.sc_Seq$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_Seq$.prototype = ScalaJS.c.sc_Seq$.prototype;
ScalaJS.c.sc_Seq$.prototype.newBuilder__scm_Builder = (function() {
  ScalaJS.m.sci_Seq$();
  return new ScalaJS.c.scm_ListBuffer().init___()
});
ScalaJS.d.sc_Seq$ = new ScalaJS.ClassTypeData({
  sc_Seq$: 0
}, false, "scala.collection.Seq$", {
  sc_Seq$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sc_Seq$.prototype.$classData = ScalaJS.d.sc_Seq$;
ScalaJS.n.sc_Seq$ = (void 0);
ScalaJS.m.sc_Seq$ = (function() {
  if ((!ScalaJS.n.sc_Seq$)) {
    ScalaJS.n.sc_Seq$ = new ScalaJS.c.sc_Seq$().init___()
  };
  return ScalaJS.n.sc_Seq$
});
/** @constructor */
ScalaJS.c.scg_IndexedSeqFactory = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.scg_IndexedSeqFactory.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.scg_IndexedSeqFactory.prototype.constructor = ScalaJS.c.scg_IndexedSeqFactory;
/** @constructor */
ScalaJS.h.scg_IndexedSeqFactory = (function() {
  /*<skip>*/
});
ScalaJS.h.scg_IndexedSeqFactory.prototype = ScalaJS.c.scg_IndexedSeqFactory.prototype;
/** @constructor */
ScalaJS.c.sci_HashMap$ = (function() {
  ScalaJS.c.scg_ImmutableMapFactory.call(this);
  this.defaultMerger$4 = null
});
ScalaJS.c.sci_HashMap$.prototype = new ScalaJS.h.scg_ImmutableMapFactory();
ScalaJS.c.sci_HashMap$.prototype.constructor = ScalaJS.c.sci_HashMap$;
/** @constructor */
ScalaJS.h.sci_HashMap$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$.prototype = ScalaJS.c.sci_HashMap$.prototype;
ScalaJS.c.sci_HashMap$.prototype.init___ = (function() {
  ScalaJS.n.sci_HashMap$ = this;
  var mergef = new ScalaJS.c.sjsr_AnonFunction2().init___sjs_js_Function2((function(this$2) {
    return (function(a$2, b$2) {
      var a = ScalaJS.as.T2(a$2);
      ScalaJS.as.T2(b$2);
      return a
    })
  })(this));
  this.defaultMerger$4 = new ScalaJS.c.sci_HashMap$$anon$2().init___F2(mergef);
  return this
});
ScalaJS.c.sci_HashMap$.prototype.scala$collection$immutable$HashMap$$makeHashTrieMap__I__sci_HashMap__I__sci_HashMap__I__I__sci_HashMap$HashTrieMap = (function(hash0, elem0, hash1, elem1, level, size) {
  var index0 = (31 & ((hash0 >>> level) | 0));
  var index1 = (31 & ((hash1 >>> level) | 0));
  if ((index0 !== index1)) {
    var bitmap = ((1 << index0) | (1 << index1));
    var elems = ScalaJS.newArrayObject(ScalaJS.d.sci_HashMap.getArrayOf(), [2]);
    if ((index0 < index1)) {
      elems.u[0] = elem0;
      elems.u[1] = elem1
    } else {
      elems.u[0] = elem1;
      elems.u[1] = elem0
    };
    return new ScalaJS.c.sci_HashMap$HashTrieMap().init___I__Asci_HashMap__I(bitmap, elems, size)
  } else {
    var elems$2 = ScalaJS.newArrayObject(ScalaJS.d.sci_HashMap.getArrayOf(), [1]);
    var bitmap$2 = (1 << index0);
    elems$2.u[0] = this.scala$collection$immutable$HashMap$$makeHashTrieMap__I__sci_HashMap__I__sci_HashMap__I__I__sci_HashMap$HashTrieMap(hash0, elem0, hash1, elem1, ((5 + level) | 0), size);
    return new ScalaJS.c.sci_HashMap$HashTrieMap().init___I__Asci_HashMap__I(bitmap$2, elems$2, size)
  }
});
ScalaJS.c.sci_HashMap$.prototype.empty__sc_GenMap = (function() {
  return ScalaJS.m.sci_HashMap$EmptyHashMap$()
});
ScalaJS.d.sci_HashMap$ = new ScalaJS.ClassTypeData({
  sci_HashMap$: 0
}, false, "scala.collection.immutable.HashMap$", {
  sci_HashMap$: 1,
  scg_ImmutableMapFactory: 1,
  scg_MapFactory: 1,
  scg_GenMapFactory: 1,
  O: 1,
  scg_BitOperations$Int: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_HashMap$.prototype.$classData = ScalaJS.d.sci_HashMap$;
ScalaJS.n.sci_HashMap$ = (void 0);
ScalaJS.m.sci_HashMap$ = (function() {
  if ((!ScalaJS.n.sci_HashMap$)) {
    ScalaJS.n.sci_HashMap$ = new ScalaJS.c.sci_HashMap$().init___()
  };
  return ScalaJS.n.sci_HashMap$
});
/** @constructor */
ScalaJS.c.sci_Seq$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.sci_Seq$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.sci_Seq$.prototype.constructor = ScalaJS.c.sci_Seq$;
/** @constructor */
ScalaJS.h.sci_Seq$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Seq$.prototype = ScalaJS.c.sci_Seq$.prototype;
ScalaJS.c.sci_Seq$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_ListBuffer().init___()
});
ScalaJS.d.sci_Seq$ = new ScalaJS.ClassTypeData({
  sci_Seq$: 0
}, false, "scala.collection.immutable.Seq$", {
  sci_Seq$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sci_Seq$.prototype.$classData = ScalaJS.d.sci_Seq$;
ScalaJS.n.sci_Seq$ = (void 0);
ScalaJS.m.sci_Seq$ = (function() {
  if ((!ScalaJS.n.sci_Seq$)) {
    ScalaJS.n.sci_Seq$ = new ScalaJS.c.sci_Seq$().init___()
  };
  return ScalaJS.n.sci_Seq$
});
/** @constructor */
ScalaJS.c.scm_Buffer$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.scm_Buffer$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.scm_Buffer$.prototype.constructor = ScalaJS.c.scm_Buffer$;
/** @constructor */
ScalaJS.h.scm_Buffer$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_Buffer$.prototype = ScalaJS.c.scm_Buffer$.prototype;
ScalaJS.c.scm_Buffer$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.sjs_js_WrappedArray().init___()
});
ScalaJS.d.scm_Buffer$ = new ScalaJS.ClassTypeData({
  scm_Buffer$: 0
}, false, "scala.collection.mutable.Buffer$", {
  scm_Buffer$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.scm_Buffer$.prototype.$classData = ScalaJS.d.scm_Buffer$;
ScalaJS.n.scm_Buffer$ = (void 0);
ScalaJS.m.scm_Buffer$ = (function() {
  if ((!ScalaJS.n.scm_Buffer$)) {
    ScalaJS.n.scm_Buffer$ = new ScalaJS.c.scm_Buffer$().init___()
  };
  return ScalaJS.n.scm_Buffer$
});
/** @constructor */
ScalaJS.c.scm_IndexedSeq$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.scm_IndexedSeq$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.scm_IndexedSeq$.prototype.constructor = ScalaJS.c.scm_IndexedSeq$;
/** @constructor */
ScalaJS.h.scm_IndexedSeq$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_IndexedSeq$.prototype = ScalaJS.c.scm_IndexedSeq$.prototype;
ScalaJS.c.scm_IndexedSeq$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_ArrayBuffer().init___()
});
ScalaJS.d.scm_IndexedSeq$ = new ScalaJS.ClassTypeData({
  scm_IndexedSeq$: 0
}, false, "scala.collection.mutable.IndexedSeq$", {
  scm_IndexedSeq$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.scm_IndexedSeq$.prototype.$classData = ScalaJS.d.scm_IndexedSeq$;
ScalaJS.n.scm_IndexedSeq$ = (void 0);
ScalaJS.m.scm_IndexedSeq$ = (function() {
  if ((!ScalaJS.n.scm_IndexedSeq$)) {
    ScalaJS.n.scm_IndexedSeq$ = new ScalaJS.c.scm_IndexedSeq$().init___()
  };
  return ScalaJS.n.scm_IndexedSeq$
});
/** @constructor */
ScalaJS.c.sjs_js_WrappedArray$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.sjs_js_WrappedArray$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.sjs_js_WrappedArray$.prototype.constructor = ScalaJS.c.sjs_js_WrappedArray$;
/** @constructor */
ScalaJS.h.sjs_js_WrappedArray$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sjs_js_WrappedArray$.prototype = ScalaJS.c.sjs_js_WrappedArray$.prototype;
ScalaJS.c.sjs_js_WrappedArray$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.sjs_js_WrappedArray().init___()
});
ScalaJS.d.sjs_js_WrappedArray$ = new ScalaJS.ClassTypeData({
  sjs_js_WrappedArray$: 0
}, false, "scala.scalajs.js.WrappedArray$", {
  sjs_js_WrappedArray$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sjs_js_WrappedArray$.prototype.$classData = ScalaJS.d.sjs_js_WrappedArray$;
ScalaJS.n.sjs_js_WrappedArray$ = (void 0);
ScalaJS.m.sjs_js_WrappedArray$ = (function() {
  if ((!ScalaJS.n.sjs_js_WrappedArray$)) {
    ScalaJS.n.sjs_js_WrappedArray$ = new ScalaJS.c.sjs_js_WrappedArray$().init___()
  };
  return ScalaJS.n.sjs_js_WrappedArray$
});
/** @constructor */
ScalaJS.c.s_Tuple2$mcII$sp = (function() {
  ScalaJS.c.T2.call(this);
  this.$$und1$mcI$sp$f = 0;
  this.$$und2$mcI$sp$f = 0
});
ScalaJS.c.s_Tuple2$mcII$sp.prototype = new ScalaJS.h.T2();
ScalaJS.c.s_Tuple2$mcII$sp.prototype.constructor = ScalaJS.c.s_Tuple2$mcII$sp;
/** @constructor */
ScalaJS.h.s_Tuple2$mcII$sp = (function() {
  /*<skip>*/
});
ScalaJS.h.s_Tuple2$mcII$sp.prototype = ScalaJS.c.s_Tuple2$mcII$sp.prototype;
ScalaJS.c.s_Tuple2$mcII$sp.prototype.$$und1$mcI$sp__I = (function() {
  return this.$$und1$mcI$sp$f
});
ScalaJS.c.s_Tuple2$mcII$sp.prototype.init___I__I = (function(_1$mcI$sp, _2$mcI$sp) {
  this.$$und1$mcI$sp$f = _1$mcI$sp;
  this.$$und2$mcI$sp$f = _2$mcI$sp;
  ScalaJS.c.T2.prototype.init___O__O.call(this, null, null);
  return this
});
ScalaJS.c.s_Tuple2$mcII$sp.prototype.$$und2__O = (function() {
  return this.$$und2$mcI$sp$f
});
ScalaJS.c.s_Tuple2$mcII$sp.prototype.$$und2$mcI$sp__I = (function() {
  return this.$$und2$mcI$sp$f
});
ScalaJS.c.s_Tuple2$mcII$sp.prototype.$$und1__O = (function() {
  return this.$$und1$mcI$sp$f
});
ScalaJS.d.s_Tuple2$mcII$sp = new ScalaJS.ClassTypeData({
  s_Tuple2$mcII$sp: 0
}, false, "scala.Tuple2$mcII$sp", {
  s_Tuple2$mcII$sp: 1,
  T2: 1,
  O: 1,
  s_Product2: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Product2$mcII$sp: 1
});
ScalaJS.c.s_Tuple2$mcII$sp.prototype.$classData = ScalaJS.d.s_Tuple2$mcII$sp;
/** @constructor */
ScalaJS.c.s_reflect_AnyValManifest = (function() {
  ScalaJS.c.O.call(this);
  this.toString$1 = null;
  this.hashCode$1 = 0
});
ScalaJS.c.s_reflect_AnyValManifest.prototype = new ScalaJS.h.O();
ScalaJS.c.s_reflect_AnyValManifest.prototype.constructor = ScalaJS.c.s_reflect_AnyValManifest;
/** @constructor */
ScalaJS.h.s_reflect_AnyValManifest = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_AnyValManifest.prototype = ScalaJS.c.s_reflect_AnyValManifest.prototype;
ScalaJS.c.s_reflect_AnyValManifest.prototype.equals__O__Z = (function(that) {
  return (this === that)
});
ScalaJS.c.s_reflect_AnyValManifest.prototype.toString__T = (function() {
  return this.toString$1
});
ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T = (function(toString) {
  this.toString$1 = toString;
  this.hashCode$1 = ScalaJS.systemIdentityHashCode(this);
  return this
});
ScalaJS.c.s_reflect_AnyValManifest.prototype.hashCode__I = (function() {
  return this.hashCode$1
});
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest = (function() {
  ScalaJS.c.O.call(this);
  this.prefix$1 = null;
  this.runtimeClass$1 = null;
  this.typeArguments$1 = null
});
ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest.prototype = new ScalaJS.h.O();
ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$ClassTypeManifest = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$ClassTypeManifest.prototype = ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest.prototype;
ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest.prototype.init___s_Option__jl_Class__sci_List = (function(prefix, runtimeClass, typeArguments) {
  this.prefix$1 = prefix;
  this.runtimeClass$1 = runtimeClass;
  this.typeArguments$1 = typeArguments;
  return this
});
/** @constructor */
ScalaJS.c.sc_IndexedSeq$ = (function() {
  ScalaJS.c.scg_IndexedSeqFactory.call(this);
  this.ReusableCBF$6 = null
});
ScalaJS.c.sc_IndexedSeq$.prototype = new ScalaJS.h.scg_IndexedSeqFactory();
ScalaJS.c.sc_IndexedSeq$.prototype.constructor = ScalaJS.c.sc_IndexedSeq$;
/** @constructor */
ScalaJS.h.sc_IndexedSeq$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_IndexedSeq$.prototype = ScalaJS.c.sc_IndexedSeq$.prototype;
ScalaJS.c.sc_IndexedSeq$.prototype.init___ = (function() {
  ScalaJS.c.scg_IndexedSeqFactory.prototype.init___.call(this);
  ScalaJS.n.sc_IndexedSeq$ = this;
  this.ReusableCBF$6 = new ScalaJS.c.sc_IndexedSeq$$anon$1().init___();
  return this
});
ScalaJS.c.sc_IndexedSeq$.prototype.newBuilder__scm_Builder = (function() {
  ScalaJS.m.sci_IndexedSeq$();
  ScalaJS.m.sci_Vector$();
  return new ScalaJS.c.sci_VectorBuilder().init___()
});
ScalaJS.d.sc_IndexedSeq$ = new ScalaJS.ClassTypeData({
  sc_IndexedSeq$: 0
}, false, "scala.collection.IndexedSeq$", {
  sc_IndexedSeq$: 1,
  scg_IndexedSeqFactory: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sc_IndexedSeq$.prototype.$classData = ScalaJS.d.sc_IndexedSeq$;
ScalaJS.n.sc_IndexedSeq$ = (void 0);
ScalaJS.m.sc_IndexedSeq$ = (function() {
  if ((!ScalaJS.n.sc_IndexedSeq$)) {
    ScalaJS.n.sc_IndexedSeq$ = new ScalaJS.c.sc_IndexedSeq$().init___()
  };
  return ScalaJS.n.sc_IndexedSeq$
});
/** @constructor */
ScalaJS.c.sc_IndexedSeqLike$Elements = (function() {
  ScalaJS.c.sc_AbstractIterator.call(this);
  this.end$2 = 0;
  this.index$2 = 0;
  this.$$outer$f = null
});
ScalaJS.c.sc_IndexedSeqLike$Elements.prototype = new ScalaJS.h.sc_AbstractIterator();
ScalaJS.c.sc_IndexedSeqLike$Elements.prototype.constructor = ScalaJS.c.sc_IndexedSeqLike$Elements;
/** @constructor */
ScalaJS.h.sc_IndexedSeqLike$Elements = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_IndexedSeqLike$Elements.prototype = ScalaJS.c.sc_IndexedSeqLike$Elements.prototype;
ScalaJS.c.sc_IndexedSeqLike$Elements.prototype.next__O = (function() {
  if ((this.index$2 >= this.end$2)) {
    ScalaJS.m.sc_Iterator$().empty$1.next__O()
  };
  var x = this.$$outer$f.apply__I__O(this.index$2);
  this.index$2 = ((1 + this.index$2) | 0);
  return x
});
ScalaJS.c.sc_IndexedSeqLike$Elements.prototype.init___sc_IndexedSeqLike__I__I = (function($$outer, start, end) {
  this.end$2 = end;
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$f = $$outer
  };
  this.index$2 = start;
  return this
});
ScalaJS.c.sc_IndexedSeqLike$Elements.prototype.hasNext__Z = (function() {
  return (this.index$2 < this.end$2)
});
ScalaJS.d.sc_IndexedSeqLike$Elements = new ScalaJS.ClassTypeData({
  sc_IndexedSeqLike$Elements: 0
}, false, "scala.collection.IndexedSeqLike$Elements", {
  sc_IndexedSeqLike$Elements: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_BufferedIterator: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sc_IndexedSeqLike$Elements.prototype.$classData = ScalaJS.d.sc_IndexedSeqLike$Elements;
/** @constructor */
ScalaJS.c.sci_HashSet$ = (function() {
  ScalaJS.c.scg_ImmutableSetFactory.call(this)
});
ScalaJS.c.sci_HashSet$.prototype = new ScalaJS.h.scg_ImmutableSetFactory();
ScalaJS.c.sci_HashSet$.prototype.constructor = ScalaJS.c.sci_HashSet$;
/** @constructor */
ScalaJS.h.sci_HashSet$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet$.prototype = ScalaJS.c.sci_HashSet$.prototype;
ScalaJS.c.sci_HashSet$.prototype.scala$collection$immutable$HashSet$$makeHashTrieSet__I__sci_HashSet__I__sci_HashSet__I__sci_HashSet$HashTrieSet = (function(hash0, elem0, hash1, elem1, level) {
  var index0 = (31 & ((hash0 >>> level) | 0));
  var index1 = (31 & ((hash1 >>> level) | 0));
  if ((index0 !== index1)) {
    var bitmap = ((1 << index0) | (1 << index1));
    var elems = ScalaJS.newArrayObject(ScalaJS.d.sci_HashSet.getArrayOf(), [2]);
    if ((index0 < index1)) {
      elems.u[0] = elem0;
      elems.u[1] = elem1
    } else {
      elems.u[0] = elem1;
      elems.u[1] = elem0
    };
    return new ScalaJS.c.sci_HashSet$HashTrieSet().init___I__Asci_HashSet__I(bitmap, elems, ((elem0.size__I() + elem1.size__I()) | 0))
  } else {
    var elems$2 = ScalaJS.newArrayObject(ScalaJS.d.sci_HashSet.getArrayOf(), [1]);
    var bitmap$2 = (1 << index0);
    var child = this.scala$collection$immutable$HashSet$$makeHashTrieSet__I__sci_HashSet__I__sci_HashSet__I__sci_HashSet$HashTrieSet(hash0, elem0, hash1, elem1, ((5 + level) | 0));
    elems$2.u[0] = child;
    return new ScalaJS.c.sci_HashSet$HashTrieSet().init___I__Asci_HashSet__I(bitmap$2, elems$2, child.size0$5)
  }
});
ScalaJS.c.sci_HashSet$.prototype.emptyInstance__sci_Set = (function() {
  return ScalaJS.m.sci_HashSet$EmptyHashSet$()
});
ScalaJS.d.sci_HashSet$ = new ScalaJS.ClassTypeData({
  sci_HashSet$: 0
}, false, "scala.collection.immutable.HashSet$", {
  sci_HashSet$: 1,
  scg_ImmutableSetFactory: 1,
  scg_SetFactory: 1,
  scg_GenSetFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_HashSet$.prototype.$classData = ScalaJS.d.sci_HashSet$;
ScalaJS.n.sci_HashSet$ = (void 0);
ScalaJS.m.sci_HashSet$ = (function() {
  if ((!ScalaJS.n.sci_HashSet$)) {
    ScalaJS.n.sci_HashSet$ = new ScalaJS.c.sci_HashSet$().init___()
  };
  return ScalaJS.n.sci_HashSet$
});
/** @constructor */
ScalaJS.c.sci_IndexedSeq$ = (function() {
  ScalaJS.c.scg_IndexedSeqFactory.call(this)
});
ScalaJS.c.sci_IndexedSeq$.prototype = new ScalaJS.h.scg_IndexedSeqFactory();
ScalaJS.c.sci_IndexedSeq$.prototype.constructor = ScalaJS.c.sci_IndexedSeq$;
/** @constructor */
ScalaJS.h.sci_IndexedSeq$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_IndexedSeq$.prototype = ScalaJS.c.sci_IndexedSeq$.prototype;
ScalaJS.c.sci_IndexedSeq$.prototype.newBuilder__scm_Builder = (function() {
  ScalaJS.m.sci_Vector$();
  return new ScalaJS.c.sci_VectorBuilder().init___()
});
ScalaJS.d.sci_IndexedSeq$ = new ScalaJS.ClassTypeData({
  sci_IndexedSeq$: 0
}, false, "scala.collection.immutable.IndexedSeq$", {
  sci_IndexedSeq$: 1,
  scg_IndexedSeqFactory: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
ScalaJS.c.sci_IndexedSeq$.prototype.$classData = ScalaJS.d.sci_IndexedSeq$;
ScalaJS.n.sci_IndexedSeq$ = (void 0);
ScalaJS.m.sci_IndexedSeq$ = (function() {
  if ((!ScalaJS.n.sci_IndexedSeq$)) {
    ScalaJS.n.sci_IndexedSeq$ = new ScalaJS.c.sci_IndexedSeq$().init___()
  };
  return ScalaJS.n.sci_IndexedSeq$
});
/** @constructor */
ScalaJS.c.sci_ListSet$ = (function() {
  ScalaJS.c.scg_ImmutableSetFactory.call(this)
});
ScalaJS.c.sci_ListSet$.prototype = new ScalaJS.h.scg_ImmutableSetFactory();
ScalaJS.c.sci_ListSet$.prototype.constructor = ScalaJS.c.sci_ListSet$;
/** @constructor */
ScalaJS.h.sci_ListSet$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListSet$.prototype = ScalaJS.c.sci_ListSet$.prototype;
ScalaJS.c.sci_ListSet$.prototype.emptyInstance__sci_Set = (function() {
  return ScalaJS.m.sci_ListSet$EmptyListSet$()
});
ScalaJS.c.sci_ListSet$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.sci_ListSet$ListSetBuilder().init___()
});
ScalaJS.d.sci_ListSet$ = new ScalaJS.ClassTypeData({
  sci_ListSet$: 0
}, false, "scala.collection.immutable.ListSet$", {
  sci_ListSet$: 1,
  scg_ImmutableSetFactory: 1,
  scg_SetFactory: 1,
  scg_GenSetFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_ListSet$.prototype.$classData = ScalaJS.d.sci_ListSet$;
ScalaJS.n.sci_ListSet$ = (void 0);
ScalaJS.m.sci_ListSet$ = (function() {
  if ((!ScalaJS.n.sci_ListSet$)) {
    ScalaJS.n.sci_ListSet$ = new ScalaJS.c.sci_ListSet$().init___()
  };
  return ScalaJS.n.sci_ListSet$
});
/** @constructor */
ScalaJS.c.scm_HashSet$ = (function() {
  ScalaJS.c.scg_MutableSetFactory.call(this)
});
ScalaJS.c.scm_HashSet$.prototype = new ScalaJS.h.scg_MutableSetFactory();
ScalaJS.c.scm_HashSet$.prototype.constructor = ScalaJS.c.scm_HashSet$;
/** @constructor */
ScalaJS.h.scm_HashSet$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_HashSet$.prototype = ScalaJS.c.scm_HashSet$.prototype;
ScalaJS.c.scm_HashSet$.prototype.empty__sc_GenTraversable = (function() {
  return new ScalaJS.c.scm_HashSet().init___()
});
ScalaJS.d.scm_HashSet$ = new ScalaJS.ClassTypeData({
  scm_HashSet$: 0
}, false, "scala.collection.mutable.HashSet$", {
  scm_HashSet$: 1,
  scg_MutableSetFactory: 1,
  scg_SetFactory: 1,
  scg_GenSetFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_HashSet$.prototype.$classData = ScalaJS.d.scm_HashSet$;
ScalaJS.n.scm_HashSet$ = (void 0);
ScalaJS.m.scm_HashSet$ = (function() {
  if ((!ScalaJS.n.scm_HashSet$)) {
    ScalaJS.n.scm_HashSet$ = new ScalaJS.c.scm_HashSet$().init___()
  };
  return ScalaJS.n.scm_HashSet$
});
/** @constructor */
ScalaJS.c.sjs_js_JavaScriptException = (function() {
  ScalaJS.c.jl_RuntimeException.call(this);
  this.exception$4 = null
});
ScalaJS.c.sjs_js_JavaScriptException.prototype = new ScalaJS.h.jl_RuntimeException();
ScalaJS.c.sjs_js_JavaScriptException.prototype.constructor = ScalaJS.c.sjs_js_JavaScriptException;
/** @constructor */
ScalaJS.h.sjs_js_JavaScriptException = (function() {
  /*<skip>*/
});
ScalaJS.h.sjs_js_JavaScriptException.prototype = ScalaJS.c.sjs_js_JavaScriptException.prototype;
ScalaJS.c.sjs_js_JavaScriptException.prototype.productPrefix__T = (function() {
  return "JavaScriptException"
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.productArity__I = (function() {
  return 1
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.fillInStackTrace__jl_Throwable = (function() {
  ScalaJS.m.sjsr_StackTrace$().captureState__jl_Throwable__O__V(this, this.exception$4);
  return this
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if (ScalaJS.is.sjs_js_JavaScriptException(x$1)) {
    var JavaScriptException$1 = ScalaJS.as.sjs_js_JavaScriptException(x$1);
    return ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(this.exception$4, JavaScriptException$1.exception$4)
  } else {
    return false
  }
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0:
      {
        return this.exception$4;
        break
      };
    default:
      throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + x$1));
  }
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.toString__T = (function() {
  return ScalaJS.objectToString(this.exception$4)
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.init___O = (function(exception) {
  this.exception$4 = exception;
  ScalaJS.c.jl_RuntimeException.prototype.init___.call(this);
  return this
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.hashCode__I = (function() {
  var this$2 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.is.sjs_js_JavaScriptException = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sjs_js_JavaScriptException)))
});
ScalaJS.as.sjs_js_JavaScriptException = (function(obj) {
  return ((ScalaJS.is.sjs_js_JavaScriptException(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.scalajs.js.JavaScriptException"))
});
ScalaJS.isArrayOf.sjs_js_JavaScriptException = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sjs_js_JavaScriptException)))
});
ScalaJS.asArrayOf.sjs_js_JavaScriptException = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sjs_js_JavaScriptException(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.scalajs.js.JavaScriptException;", depth))
});
ScalaJS.d.sjs_js_JavaScriptException = new ScalaJS.ClassTypeData({
  sjs_js_JavaScriptException: 0
}, false, "scala.scalajs.js.JavaScriptException", {
  sjs_js_JavaScriptException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1
});
ScalaJS.c.sjs_js_JavaScriptException.prototype.$classData = ScalaJS.d.sjs_js_JavaScriptException;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$10 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$10.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$10.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$10;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$10 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$10.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$10.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$10.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Long");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$10.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AJ(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$10.prototype.newArray__I__AJ = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.J.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$10 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$10: 0
}, false, "scala.reflect.ManifestFactory$$anon$10", {
  s_reflect_ManifestFactory$$anon$10: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$10.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$10;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$11 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$11.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$11.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$11;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$11 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$11.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$11.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$11.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Float");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$11.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AF(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$11.prototype.newArray__I__AF = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.F.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$11 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$11: 0
}, false, "scala.reflect.ManifestFactory$$anon$11", {
  s_reflect_ManifestFactory$$anon$11: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$11.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$11;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$12 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$12.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$12.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$12;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$12 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$12.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$12.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$12.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Double");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$12.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AD(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$12.prototype.newArray__I__AD = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.D.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$12 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$12: 0
}, false, "scala.reflect.ManifestFactory$$anon$12", {
  s_reflect_ManifestFactory$$anon$12: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$12.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$12;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$13 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$13.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$13.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$13;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$13 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$13.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$13.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$13.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Boolean");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$13.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AZ(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$13.prototype.newArray__I__AZ = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.Z.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$13 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$13: 0
}, false, "scala.reflect.ManifestFactory$$anon$13", {
  s_reflect_ManifestFactory$$anon$13: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$13.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$13;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$14 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$14.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$14.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$14;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$14 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$14.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$14.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$14.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Unit");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$14.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__Asr_BoxedUnit(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$14.prototype.newArray__I__Asr_BoxedUnit = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.sr_BoxedUnit.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$14 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$14: 0
}, false, "scala.reflect.ManifestFactory$$anon$14", {
  s_reflect_ManifestFactory$$anon$14: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$14.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$14;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$6 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$6.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$6.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$6;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$6 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$6.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$6.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$6.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Byte");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$6.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AB(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$6.prototype.newArray__I__AB = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.B.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$6 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$6: 0
}, false, "scala.reflect.ManifestFactory$$anon$6", {
  s_reflect_ManifestFactory$$anon$6: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$6.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$6;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$7 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$7.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$7.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$7;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$7 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$7.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$7.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$7.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Short");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$7.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AS(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$7.prototype.newArray__I__AS = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.S.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$7 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$7: 0
}, false, "scala.reflect.ManifestFactory$$anon$7", {
  s_reflect_ManifestFactory$$anon$7: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$7.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$7;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$8 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$8.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$8.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$8;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$8 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$8.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$8.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$8.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Char");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$8.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AC(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$8.prototype.newArray__I__AC = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.C.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$8 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$8: 0
}, false, "scala.reflect.ManifestFactory$$anon$8", {
  s_reflect_ManifestFactory$$anon$8: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$8.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$8;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$9 = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$9.prototype = new ScalaJS.h.s_reflect_AnyValManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$9.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$9;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$9 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$9.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$9.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$9.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_AnyValManifest.prototype.init___T.call(this, "Int");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$9.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AI(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$9.prototype.newArray__I__AI = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.I.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$9 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$9: 0
}, false, "scala.reflect.ManifestFactory$$anon$9", {
  s_reflect_ManifestFactory$$anon$9: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$9.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$9;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest.call(this);
  this.toString$2 = null;
  this.hashCode$2 = 0
});
ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype = new ScalaJS.h.s_reflect_ManifestFactory$ClassTypeManifest();
ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$PhantomManifest = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$PhantomManifest.prototype = ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype;
ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.equals__O__Z = (function(that) {
  return (this === that)
});
ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.toString__T = (function() {
  return this.toString$2
});
ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.hashCode__I = (function() {
  return this.hashCode$2
});
ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.init___jl_Class__T = (function(_runtimeClass, toString) {
  this.toString$2 = toString;
  ScalaJS.c.s_reflect_ManifestFactory$ClassTypeManifest.prototype.init___s_Option__jl_Class__sci_List.call(this, ScalaJS.m.s_None$(), _runtimeClass, ScalaJS.m.sci_Nil$());
  this.hashCode$2 = ScalaJS.systemIdentityHashCode(this);
  return this
});
ScalaJS.is.sc_IterableLike = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_IterableLike)))
});
ScalaJS.as.sc_IterableLike = (function(obj) {
  return ((ScalaJS.is.sc_IterableLike(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.IterableLike"))
});
ScalaJS.isArrayOf.sc_IterableLike = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_IterableLike)))
});
ScalaJS.asArrayOf.sc_IterableLike = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_IterableLike(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.IterableLike;", depth))
});
/** @constructor */
ScalaJS.c.sci_List$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this);
  this.partialNotApplied$5 = null
});
ScalaJS.c.sci_List$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.sci_List$.prototype.constructor = ScalaJS.c.sci_List$;
/** @constructor */
ScalaJS.h.sci_List$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_List$.prototype = ScalaJS.c.sci_List$.prototype;
ScalaJS.c.sci_List$.prototype.init___ = (function() {
  ScalaJS.c.scg_SeqFactory.prototype.init___.call(this);
  ScalaJS.n.sci_List$ = this;
  this.partialNotApplied$5 = new ScalaJS.c.sci_List$$anon$1().init___();
  return this
});
ScalaJS.c.sci_List$.prototype.empty__sc_GenTraversable = (function() {
  return ScalaJS.m.sci_Nil$()
});
ScalaJS.c.sci_List$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_ListBuffer().init___()
});
ScalaJS.d.sci_List$ = new ScalaJS.ClassTypeData({
  sci_List$: 0
}, false, "scala.collection.immutable.List$", {
  sci_List$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_List$.prototype.$classData = ScalaJS.d.sci_List$;
ScalaJS.n.sci_List$ = (void 0);
ScalaJS.m.sci_List$ = (function() {
  if ((!ScalaJS.n.sci_List$)) {
    ScalaJS.n.sci_List$ = new ScalaJS.c.sci_List$().init___()
  };
  return ScalaJS.n.sci_List$
});
/** @constructor */
ScalaJS.c.sci_Stream$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.sci_Stream$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.sci_Stream$.prototype.constructor = ScalaJS.c.sci_Stream$;
/** @constructor */
ScalaJS.h.sci_Stream$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream$.prototype = ScalaJS.c.sci_Stream$.prototype;
ScalaJS.c.sci_Stream$.prototype.empty__sc_GenTraversable = (function() {
  return ScalaJS.m.sci_Stream$Empty$()
});
ScalaJS.c.sci_Stream$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.sci_Stream$StreamBuilder().init___()
});
ScalaJS.d.sci_Stream$ = new ScalaJS.ClassTypeData({
  sci_Stream$: 0
}, false, "scala.collection.immutable.Stream$", {
  sci_Stream$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Stream$.prototype.$classData = ScalaJS.d.sci_Stream$;
ScalaJS.n.sci_Stream$ = (void 0);
ScalaJS.m.sci_Stream$ = (function() {
  if ((!ScalaJS.n.sci_Stream$)) {
    ScalaJS.n.sci_Stream$ = new ScalaJS.c.sci_Stream$().init___()
  };
  return ScalaJS.n.sci_Stream$
});
/** @constructor */
ScalaJS.c.scm_ArrayBuffer$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.scm_ArrayBuffer$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.scm_ArrayBuffer$.prototype.constructor = ScalaJS.c.scm_ArrayBuffer$;
/** @constructor */
ScalaJS.h.scm_ArrayBuffer$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_ArrayBuffer$.prototype = ScalaJS.c.scm_ArrayBuffer$.prototype;
ScalaJS.c.scm_ArrayBuffer$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_ArrayBuffer().init___()
});
ScalaJS.d.scm_ArrayBuffer$ = new ScalaJS.ClassTypeData({
  scm_ArrayBuffer$: 0
}, false, "scala.collection.mutable.ArrayBuffer$", {
  scm_ArrayBuffer$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_ArrayBuffer$.prototype.$classData = ScalaJS.d.scm_ArrayBuffer$;
ScalaJS.n.scm_ArrayBuffer$ = (void 0);
ScalaJS.m.scm_ArrayBuffer$ = (function() {
  if ((!ScalaJS.n.scm_ArrayBuffer$)) {
    ScalaJS.n.scm_ArrayBuffer$ = new ScalaJS.c.scm_ArrayBuffer$().init___()
  };
  return ScalaJS.n.scm_ArrayBuffer$
});
/** @constructor */
ScalaJS.c.scm_ListBuffer$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this)
});
ScalaJS.c.scm_ListBuffer$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.scm_ListBuffer$.prototype.constructor = ScalaJS.c.scm_ListBuffer$;
/** @constructor */
ScalaJS.h.scm_ListBuffer$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_ListBuffer$.prototype = ScalaJS.c.scm_ListBuffer$.prototype;
ScalaJS.c.scm_ListBuffer$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_GrowingBuilder().init___scg_Growable(new ScalaJS.c.scm_ListBuffer().init___())
});
ScalaJS.d.scm_ListBuffer$ = new ScalaJS.ClassTypeData({
  scm_ListBuffer$: 0
}, false, "scala.collection.mutable.ListBuffer$", {
  scm_ListBuffer$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_ListBuffer$.prototype.$classData = ScalaJS.d.scm_ListBuffer$;
ScalaJS.n.scm_ListBuffer$ = (void 0);
ScalaJS.m.scm_ListBuffer$ = (function() {
  if ((!ScalaJS.n.scm_ListBuffer$)) {
    ScalaJS.n.scm_ListBuffer$ = new ScalaJS.c.scm_ListBuffer$().init___()
  };
  return ScalaJS.n.scm_ListBuffer$
});
/** @constructor */
ScalaJS.c.scm_Stack$ = (function() {
  ScalaJS.c.scg_SeqFactory.call(this);
  this.empty$5 = null
});
ScalaJS.c.scm_Stack$.prototype = new ScalaJS.h.scg_SeqFactory();
ScalaJS.c.scm_Stack$.prototype.constructor = ScalaJS.c.scm_Stack$;
/** @constructor */
ScalaJS.h.scm_Stack$ = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_Stack$.prototype = ScalaJS.c.scm_Stack$.prototype;
ScalaJS.c.scm_Stack$.prototype.init___ = (function() {
  ScalaJS.c.scg_SeqFactory.prototype.init___.call(this);
  ScalaJS.n.scm_Stack$ = this;
  this.empty$5 = new ScalaJS.c.scm_Stack().init___sci_List(ScalaJS.m.sci_Nil$());
  return this
});
ScalaJS.c.scm_Stack$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_Stack$StackBuilder().init___()
});
ScalaJS.d.scm_Stack$ = new ScalaJS.ClassTypeData({
  scm_Stack$: 0
}, false, "scala.collection.mutable.Stack$", {
  scm_Stack$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_Stack$.prototype.$classData = ScalaJS.d.scm_Stack$;
ScalaJS.n.scm_Stack$ = (void 0);
ScalaJS.m.scm_Stack$ = (function() {
  if ((!ScalaJS.n.scm_Stack$)) {
    ScalaJS.n.scm_Stack$ = new ScalaJS.c.scm_Stack$().init___()
  };
  return ScalaJS.n.scm_Stack$
});
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$1 = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$1.prototype = new ScalaJS.h.s_reflect_ManifestFactory$PhantomManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$1.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$1;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$1 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$1.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$1.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$1.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.init___jl_Class__T.call(this, ScalaJS.m.s_reflect_ManifestFactory$().scala$reflect$ManifestFactory$$ObjectTYPE$1, "Any");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$1.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AO(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$1.prototype.newArray__I__AO = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$1 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$1: 0
}, false, "scala.reflect.ManifestFactory$$anon$1", {
  s_reflect_ManifestFactory$$anon$1: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$1.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$1;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$2 = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$2.prototype = new ScalaJS.h.s_reflect_ManifestFactory$PhantomManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$2.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$2;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$2 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$2.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$2.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$2.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.init___jl_Class__T.call(this, ScalaJS.m.s_reflect_ManifestFactory$().scala$reflect$ManifestFactory$$ObjectTYPE$1, "Object");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$2.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AO(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$2.prototype.newArray__I__AO = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$2 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$2: 0
}, false, "scala.reflect.ManifestFactory$$anon$2", {
  s_reflect_ManifestFactory$$anon$2: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$2.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$2;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$3 = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$3.prototype = new ScalaJS.h.s_reflect_ManifestFactory$PhantomManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$3.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$3;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$3 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$3.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$3.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$3.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.init___jl_Class__T.call(this, ScalaJS.m.s_reflect_ManifestFactory$().scala$reflect$ManifestFactory$$ObjectTYPE$1, "AnyVal");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$3.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AO(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$3.prototype.newArray__I__AO = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$3 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$3: 0
}, false, "scala.reflect.ManifestFactory$$anon$3", {
  s_reflect_ManifestFactory$$anon$3: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$3.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$3;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$4 = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$4.prototype = new ScalaJS.h.s_reflect_ManifestFactory$PhantomManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$4.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$4;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$4 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$4.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$4.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$4.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.init___jl_Class__T.call(this, ScalaJS.m.s_reflect_ManifestFactory$().scala$reflect$ManifestFactory$$NullTYPE$1, "Null");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$4.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AO(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$4.prototype.newArray__I__AO = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$4 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$4: 0
}, false, "scala.reflect.ManifestFactory$$anon$4", {
  s_reflect_ManifestFactory$$anon$4: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$4.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$4;
/** @constructor */
ScalaJS.c.s_reflect_ManifestFactory$$anon$5 = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.call(this)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$5.prototype = new ScalaJS.h.s_reflect_ManifestFactory$PhantomManifest();
ScalaJS.c.s_reflect_ManifestFactory$$anon$5.prototype.constructor = ScalaJS.c.s_reflect_ManifestFactory$$anon$5;
/** @constructor */
ScalaJS.h.s_reflect_ManifestFactory$$anon$5 = (function() {
  /*<skip>*/
});
ScalaJS.h.s_reflect_ManifestFactory$$anon$5.prototype = ScalaJS.c.s_reflect_ManifestFactory$$anon$5.prototype;
ScalaJS.c.s_reflect_ManifestFactory$$anon$5.prototype.init___ = (function() {
  ScalaJS.c.s_reflect_ManifestFactory$PhantomManifest.prototype.init___jl_Class__T.call(this, ScalaJS.m.s_reflect_ManifestFactory$().scala$reflect$ManifestFactory$$NothingTYPE$1, "Nothing");
  return this
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$5.prototype.newArray__I__O = (function(len) {
  return this.newArray__I__AO(len)
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$5.prototype.newArray__I__AO = (function(len) {
  return ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [len])
});
ScalaJS.d.s_reflect_ManifestFactory$$anon$5 = new ScalaJS.ClassTypeData({
  s_reflect_ManifestFactory$$anon$5: 0
}, false, "scala.reflect.ManifestFactory$$anon$5", {
  s_reflect_ManifestFactory$$anon$5: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
ScalaJS.c.s_reflect_ManifestFactory$$anon$5.prototype.$classData = ScalaJS.d.s_reflect_ManifestFactory$$anon$5;
ScalaJS.is.sc_GenMap = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_GenMap)))
});
ScalaJS.as.sc_GenMap = (function(obj) {
  return ((ScalaJS.is.sc_GenMap(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.GenMap"))
});
ScalaJS.isArrayOf.sc_GenMap = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_GenMap)))
});
ScalaJS.asArrayOf.sc_GenMap = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_GenMap(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.GenMap;", depth))
});
ScalaJS.is.sc_GenSeq = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_GenSeq)))
});
ScalaJS.as.sc_GenSeq = (function(obj) {
  return ((ScalaJS.is.sc_GenSeq(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.GenSeq"))
});
ScalaJS.isArrayOf.sc_GenSeq = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_GenSeq)))
});
ScalaJS.asArrayOf.sc_GenSeq = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_GenSeq(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.GenSeq;", depth))
});
/** @constructor */
ScalaJS.c.sci_Vector$ = (function() {
  ScalaJS.c.scg_IndexedSeqFactory.call(this);
  this.NIL$6 = null;
  this.Log2ConcatFaster$6 = 0;
  this.TinyAppendFaster$6 = 0
});
ScalaJS.c.sci_Vector$.prototype = new ScalaJS.h.scg_IndexedSeqFactory();
ScalaJS.c.sci_Vector$.prototype.constructor = ScalaJS.c.sci_Vector$;
/** @constructor */
ScalaJS.h.sci_Vector$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Vector$.prototype = ScalaJS.c.sci_Vector$.prototype;
ScalaJS.c.sci_Vector$.prototype.init___ = (function() {
  ScalaJS.c.scg_IndexedSeqFactory.prototype.init___.call(this);
  ScalaJS.n.sci_Vector$ = this;
  this.NIL$6 = new ScalaJS.c.sci_Vector().init___I__I__I(0, 0, 0);
  return this
});
ScalaJS.c.sci_Vector$.prototype.empty__sc_GenTraversable = (function() {
  return this.NIL$6
});
ScalaJS.c.sci_Vector$.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.sci_VectorBuilder().init___()
});
ScalaJS.d.sci_Vector$ = new ScalaJS.ClassTypeData({
  sci_Vector$: 0
}, false, "scala.collection.immutable.Vector$", {
  sci_Vector$: 1,
  scg_IndexedSeqFactory: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Vector$.prototype.$classData = ScalaJS.d.sci_Vector$;
ScalaJS.n.sci_Vector$ = (void 0);
ScalaJS.m.sci_Vector$ = (function() {
  if ((!ScalaJS.n.sci_Vector$)) {
    ScalaJS.n.sci_Vector$ = new ScalaJS.c.sci_Vector$().init___()
  };
  return ScalaJS.n.sci_Vector$
});
/** @constructor */
ScalaJS.c.sc_AbstractTraversable = (function() {
  ScalaJS.c.O.call(this)
});
ScalaJS.c.sc_AbstractTraversable.prototype = new ScalaJS.h.O();
ScalaJS.c.sc_AbstractTraversable.prototype.constructor = ScalaJS.c.sc_AbstractTraversable;
/** @constructor */
ScalaJS.h.sc_AbstractTraversable = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_AbstractTraversable.prototype = ScalaJS.c.sc_AbstractTraversable.prototype;
ScalaJS.c.sc_AbstractTraversable.prototype.toList__sci_List = (function() {
  var this$1 = ScalaJS.m.sci_List$();
  var cbf = this$1.ReusableCBFInstance$2;
  return ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(this, cbf))
});
ScalaJS.c.sc_AbstractTraversable.prototype.mkString__T__T__T__T = (function(start, sep, end) {
  return ScalaJS.s.sc_TraversableOnce$class__mkString__sc_TraversableOnce__T__T__T__T(this, start, sep, end)
});
ScalaJS.c.sc_AbstractTraversable.prototype.toVector__sci_Vector = (function() {
  ScalaJS.m.sci_Vector$();
  var cbf = ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6;
  return ScalaJS.as.sci_Vector(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(this, cbf))
});
ScalaJS.c.sc_AbstractTraversable.prototype.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O = (function(that, bf) {
  return ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O(this, that, bf)
});
ScalaJS.c.sc_AbstractTraversable.prototype.tail__O = (function() {
  return ScalaJS.s.sc_TraversableLike$class__tail__sc_TraversableLike__O(this)
});
ScalaJS.c.sc_AbstractTraversable.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return ScalaJS.s.sc_TraversableOnce$class__addString__sc_TraversableOnce__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end)
});
ScalaJS.c.sc_AbstractTraversable.prototype.repr__O = (function() {
  return this
});
ScalaJS.c.sc_AbstractTraversable.prototype.isTraversableAgain__Z = (function() {
  return true
});
ScalaJS.c.sc_AbstractTraversable.prototype.newBuilder__scm_Builder = (function() {
  return this.companion__scg_GenericCompanion().newBuilder__scm_Builder()
});
ScalaJS.c.sc_AbstractTraversable.prototype.stringPrefix__T = (function() {
  return ScalaJS.s.sc_TraversableLike$class__stringPrefix__sc_TraversableLike__T(this)
});
ScalaJS.is.sc_GenSet = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_GenSet)))
});
ScalaJS.as.sc_GenSet = (function(obj) {
  return ((ScalaJS.is.sc_GenSet(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.GenSet"))
});
ScalaJS.isArrayOf.sc_GenSet = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_GenSet)))
});
ScalaJS.asArrayOf.sc_GenSet = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_GenSet(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.GenSet;", depth))
});
ScalaJS.is.sc_IndexedSeqLike = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_IndexedSeqLike)))
});
ScalaJS.as.sc_IndexedSeqLike = (function(obj) {
  return ((ScalaJS.is.sc_IndexedSeqLike(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.IndexedSeqLike"))
});
ScalaJS.isArrayOf.sc_IndexedSeqLike = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_IndexedSeqLike)))
});
ScalaJS.asArrayOf.sc_IndexedSeqLike = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_IndexedSeqLike(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.IndexedSeqLike;", depth))
});
ScalaJS.is.sc_LinearSeqLike = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_LinearSeqLike)))
});
ScalaJS.as.sc_LinearSeqLike = (function(obj) {
  return ((ScalaJS.is.sc_LinearSeqLike(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.LinearSeqLike"))
});
ScalaJS.isArrayOf.sc_LinearSeqLike = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_LinearSeqLike)))
});
ScalaJS.asArrayOf.sc_LinearSeqLike = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_LinearSeqLike(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.LinearSeqLike;", depth))
});
ScalaJS.is.sc_LinearSeqOptimized = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_LinearSeqOptimized)))
});
ScalaJS.as.sc_LinearSeqOptimized = (function(obj) {
  return ((ScalaJS.is.sc_LinearSeqOptimized(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.LinearSeqOptimized"))
});
ScalaJS.isArrayOf.sc_LinearSeqOptimized = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_LinearSeqOptimized)))
});
ScalaJS.asArrayOf.sc_LinearSeqOptimized = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_LinearSeqOptimized(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.LinearSeqOptimized;", depth))
});
/** @constructor */
ScalaJS.c.sc_AbstractIterable = (function() {
  ScalaJS.c.sc_AbstractTraversable.call(this)
});
ScalaJS.c.sc_AbstractIterable.prototype = new ScalaJS.h.sc_AbstractTraversable();
ScalaJS.c.sc_AbstractIterable.prototype.constructor = ScalaJS.c.sc_AbstractIterable;
/** @constructor */
ScalaJS.h.sc_AbstractIterable = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_AbstractIterable.prototype = ScalaJS.c.sc_AbstractIterable.prototype;
ScalaJS.c.sc_AbstractIterable.prototype.head__O = (function() {
  return this.iterator__sc_Iterator().next__O()
});
ScalaJS.c.sc_AbstractIterable.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_IterableLike$class__sameElements__sc_IterableLike__sc_GenIterable__Z(this, that)
});
ScalaJS.c.sc_AbstractIterable.prototype.forall__F1__Z = (function(p) {
  var this$1 = this.iterator__sc_Iterator();
  return ScalaJS.s.sc_Iterator$class__forall__sc_Iterator__F1__Z(this$1, p)
});
ScalaJS.c.sc_AbstractIterable.prototype.foreach__F1__V = (function(f) {
  var this$1 = this.iterator__sc_Iterator();
  ScalaJS.s.sc_Iterator$class__foreach__sc_Iterator__F1__V(this$1, f)
});
ScalaJS.c.sc_AbstractIterable.prototype.toStream__sci_Stream = (function() {
  return this.iterator__sc_Iterator().toStream__sci_Stream()
});
ScalaJS.c.sc_AbstractIterable.prototype.drop__I__O = (function(n) {
  return ScalaJS.s.sc_IterableLike$class__drop__sc_IterableLike__I__O(this, n)
});
ScalaJS.c.sc_AbstractIterable.prototype.copyToArray__O__I__I__V = (function(xs, start, len) {
  ScalaJS.s.sc_IterableLike$class__copyToArray__sc_IterableLike__O__I__I__V(this, xs, start, len)
});
ScalaJS.is.sci_Iterable = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_Iterable)))
});
ScalaJS.as.sci_Iterable = (function(obj) {
  return ((ScalaJS.is.sci_Iterable(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.Iterable"))
});
ScalaJS.isArrayOf.sci_Iterable = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_Iterable)))
});
ScalaJS.asArrayOf.sci_Iterable = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_Iterable(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.Iterable;", depth))
});
ScalaJS.d.sci_Iterable = new ScalaJS.ClassTypeData({
  sci_Iterable: 0
}, true, "scala.collection.immutable.Iterable", {
  sci_Iterable: 1,
  sci_Traversable: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  s_Immutable: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1
});
/** @constructor */
ScalaJS.c.sci_StringOps = (function() {
  ScalaJS.c.O.call(this);
  this.repr$1 = null
});
ScalaJS.c.sci_StringOps.prototype = new ScalaJS.h.O();
ScalaJS.c.sci_StringOps.prototype.constructor = ScalaJS.c.sci_StringOps;
/** @constructor */
ScalaJS.h.sci_StringOps = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_StringOps.prototype = ScalaJS.c.sci_StringOps.prototype;
ScalaJS.c.sci_StringOps.prototype.seq__sc_TraversableOnce = (function() {
  var $$this = this.repr$1;
  return new ScalaJS.c.sci_WrappedString().init___T($$this)
});
ScalaJS.c.sci_StringOps.prototype.head__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__head__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sci_StringOps.prototype.apply__I__O = (function(idx) {
  var $$this = this.repr$1;
  var c = (65535 & ScalaJS.uI($$this["charCodeAt"](idx)));
  return new ScalaJS.c.jl_Character().init___C(c)
});
ScalaJS.c.sci_StringOps.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__lengthCompare__sc_IndexedSeqOptimized__I__I(this, len)
});
ScalaJS.c.sci_StringOps.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__sameElements__sc_IndexedSeqOptimized__sc_GenIterable__Z(this, that)
});
ScalaJS.c.sci_StringOps.prototype.toList__sci_List = (function() {
  var this$1 = ScalaJS.m.sci_List$();
  var cbf = this$1.ReusableCBFInstance$2;
  return ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(this, cbf))
});
ScalaJS.c.sci_StringOps.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z(this)
});
ScalaJS.c.sci_StringOps.prototype.thisCollection__sc_Traversable = (function() {
  var $$this = this.repr$1;
  return new ScalaJS.c.sci_WrappedString().init___T($$this)
});
ScalaJS.c.sci_StringOps.prototype.equals__O__Z = (function(x$1) {
  return ScalaJS.m.sci_StringOps$().equals$extension__T__O__Z(this.repr$1, x$1)
});
ScalaJS.c.sci_StringOps.prototype.mkString__T__T__T__T = (function(start, sep, end) {
  return ScalaJS.s.sc_TraversableOnce$class__mkString__sc_TraversableOnce__T__T__T__T(this, start, sep, end)
});
ScalaJS.c.sci_StringOps.prototype.toString__T = (function() {
  var $$this = this.repr$1;
  return $$this
});
ScalaJS.c.sci_StringOps.prototype.foreach__F1__V = (function(f) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__foreach__sc_IndexedSeqOptimized__F1__V(this, f)
});
ScalaJS.c.sci_StringOps.prototype.slice__I__I__O = (function(from, until) {
  return ScalaJS.m.sci_StringOps$().slice$extension__T__I__I__T(this.repr$1, from, until)
});
ScalaJS.c.sci_StringOps.prototype.toVector__sci_Vector = (function() {
  ScalaJS.m.sci_Vector$();
  var cbf = ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6;
  return ScalaJS.as.sci_Vector(ScalaJS.s.sc_TraversableLike$class__to__sc_TraversableLike__scg_CanBuildFrom__O(this, cbf))
});
ScalaJS.c.sci_StringOps.prototype.reverse__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__reverse__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sci_StringOps.prototype.size__I = (function() {
  var $$this = this.repr$1;
  return ScalaJS.uI($$this["length"])
});
ScalaJS.c.sci_StringOps.prototype.iterator__sc_Iterator = (function() {
  var $$this = this.repr$1;
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, ScalaJS.uI($$this["length"]))
});
ScalaJS.c.sci_StringOps.prototype.length__I = (function() {
  var $$this = this.repr$1;
  return ScalaJS.uI($$this["length"])
});
ScalaJS.c.sci_StringOps.prototype.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O = (function(that, bf) {
  return ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O(this, that, bf)
});
ScalaJS.c.sci_StringOps.prototype.toStream__sci_Stream = (function() {
  var $$this = this.repr$1;
  var this$3 = new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, ScalaJS.uI($$this["length"]));
  return ScalaJS.s.sc_Iterator$class__toStream__sc_Iterator__sci_Stream(this$3)
});
ScalaJS.c.sci_StringOps.prototype.drop__I__O = (function(n) {
  var $$this = this.repr$1;
  var until = ScalaJS.uI($$this["length"]);
  return ScalaJS.m.sci_StringOps$().slice$extension__T__I__I__T(this.repr$1, n, until)
});
ScalaJS.c.sci_StringOps.prototype.thisCollection__sc_Seq = (function() {
  var $$this = this.repr$1;
  return new ScalaJS.c.sci_WrappedString().init___T($$this)
});
ScalaJS.c.sci_StringOps.prototype.tail__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__tail__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sci_StringOps.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return ScalaJS.s.sc_TraversableOnce$class__addString__sc_TraversableOnce__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end)
});
ScalaJS.c.sci_StringOps.prototype.repr__O = (function() {
  return this.repr$1
});
ScalaJS.c.sci_StringOps.prototype.hashCode__I = (function() {
  var $$this = this.repr$1;
  return ScalaJS.m.sjsr_RuntimeString$().hashCode__T__I($$this)
});
ScalaJS.c.sci_StringOps.prototype.copyToArray__O__I__I__V = (function(xs, start, len) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__copyToArray__sc_IndexedSeqOptimized__O__I__I__V(this, xs, start, len)
});
ScalaJS.c.sci_StringOps.prototype.isTraversableAgain__Z = (function() {
  return true
});
ScalaJS.c.sci_StringOps.prototype.init___T = (function(repr) {
  this.repr$1 = repr;
  return this
});
ScalaJS.c.sci_StringOps.prototype.toCollection__O__sc_Seq = (function(repr) {
  this.repr$1;
  var repr$1 = ScalaJS.as.T(repr);
  return new ScalaJS.c.sci_WrappedString().init___T(repr$1)
});
ScalaJS.c.sci_StringOps.prototype.newBuilder__scm_Builder = (function() {
  this.repr$1;
  return new ScalaJS.c.scm_StringBuilder().init___()
});
ScalaJS.c.sci_StringOps.prototype.stringPrefix__T = (function() {
  return ScalaJS.s.sc_TraversableLike$class__stringPrefix__sc_TraversableLike__T(this)
});
ScalaJS.is.sci_StringOps = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_StringOps)))
});
ScalaJS.as.sci_StringOps = (function(obj) {
  return ((ScalaJS.is.sci_StringOps(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.StringOps"))
});
ScalaJS.isArrayOf.sci_StringOps = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_StringOps)))
});
ScalaJS.asArrayOf.sci_StringOps = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_StringOps(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.StringOps;", depth))
});
ScalaJS.d.sci_StringOps = new ScalaJS.ClassTypeData({
  sci_StringOps: 0
}, false, "scala.collection.immutable.StringOps", {
  sci_StringOps: 1,
  O: 1,
  sci_StringLike: 1,
  sc_IndexedSeqOptimized: 1,
  sc_IndexedSeqLike: 1,
  sc_SeqLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenIterableLike: 1,
  sc_GenSeqLike: 1,
  s_math_Ordered: 1,
  jl_Comparable: 1
});
ScalaJS.c.sci_StringOps.prototype.$classData = ScalaJS.d.sci_StringOps;
ScalaJS.is.sc_Seq = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_Seq)))
});
ScalaJS.as.sc_Seq = (function(obj) {
  return ((ScalaJS.is.sc_Seq(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.Seq"))
});
ScalaJS.isArrayOf.sc_Seq = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_Seq)))
});
ScalaJS.asArrayOf.sc_Seq = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_Seq(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.Seq;", depth))
});
ScalaJS.is.sc_Set = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_Set)))
});
ScalaJS.as.sc_Set = (function(obj) {
  return ((ScalaJS.is.sc_Set(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.Set"))
});
ScalaJS.isArrayOf.sc_Set = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_Set)))
});
ScalaJS.asArrayOf.sc_Set = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_Set(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.Set;", depth))
});
/** @constructor */
ScalaJS.c.scm_AbstractIterable = (function() {
  ScalaJS.c.sc_AbstractIterable.call(this)
});
ScalaJS.c.scm_AbstractIterable.prototype = new ScalaJS.h.sc_AbstractIterable();
ScalaJS.c.scm_AbstractIterable.prototype.constructor = ScalaJS.c.scm_AbstractIterable;
/** @constructor */
ScalaJS.h.scm_AbstractIterable = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_AbstractIterable.prototype = ScalaJS.c.scm_AbstractIterable.prototype;
ScalaJS.is.sc_IndexedSeq = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_IndexedSeq)))
});
ScalaJS.as.sc_IndexedSeq = (function(obj) {
  return ((ScalaJS.is.sc_IndexedSeq(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.IndexedSeq"))
});
ScalaJS.isArrayOf.sc_IndexedSeq = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_IndexedSeq)))
});
ScalaJS.asArrayOf.sc_IndexedSeq = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_IndexedSeq(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.IndexedSeq;", depth))
});
ScalaJS.is.sc_LinearSeq = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_LinearSeq)))
});
ScalaJS.as.sc_LinearSeq = (function(obj) {
  return ((ScalaJS.is.sc_LinearSeq(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.LinearSeq"))
});
ScalaJS.isArrayOf.sc_LinearSeq = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_LinearSeq)))
});
ScalaJS.asArrayOf.sc_LinearSeq = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sc_LinearSeq(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.LinearSeq;", depth))
});
/** @constructor */
ScalaJS.c.sc_AbstractSeq = (function() {
  ScalaJS.c.sc_AbstractIterable.call(this)
});
ScalaJS.c.sc_AbstractSeq.prototype = new ScalaJS.h.sc_AbstractIterable();
ScalaJS.c.sc_AbstractSeq.prototype.constructor = ScalaJS.c.sc_AbstractSeq;
/** @constructor */
ScalaJS.h.sc_AbstractSeq = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_AbstractSeq.prototype = ScalaJS.c.sc_AbstractSeq.prototype;
ScalaJS.c.sc_AbstractSeq.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_SeqLike$class__lengthCompare__sc_SeqLike__I__I(this, len)
});
ScalaJS.c.sc_AbstractSeq.prototype.runWith__F1__F1 = (function(action) {
  return ScalaJS.s.s_PartialFunction$class__runWith__s_PartialFunction__F1__F1(this, action)
});
ScalaJS.c.sc_AbstractSeq.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_SeqLike$class__isEmpty__sc_SeqLike__Z(this)
});
ScalaJS.c.sc_AbstractSeq.prototype.equals__O__Z = (function(that) {
  return ScalaJS.s.sc_GenSeqLike$class__equals__sc_GenSeqLike__O__Z(this, that)
});
ScalaJS.c.sc_AbstractSeq.prototype.toString__T = (function() {
  return ScalaJS.s.sc_TraversableLike$class__toString__sc_TraversableLike__T(this)
});
ScalaJS.c.sc_AbstractSeq.prototype.reverse__O = (function() {
  return ScalaJS.s.sc_SeqLike$class__reverse__sc_SeqLike__O(this)
});
ScalaJS.c.sc_AbstractSeq.prototype.size__I = (function() {
  return this.length__I()
});
ScalaJS.c.sc_AbstractSeq.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.sc_AbstractSeq.prototype.applyOrElse__O__F1__O = (function(x, default$2) {
  return ScalaJS.s.s_PartialFunction$class__applyOrElse__s_PartialFunction__O__F1__O(this, x, default$2)
});
ScalaJS.c.sc_AbstractSeq.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this.seq__sc_Seq())
});
ScalaJS.c.sc_AbstractSeq.prototype.toCollection__O__sc_Seq = (function(repr) {
  return ScalaJS.as.sc_Seq(repr)
});
/** @constructor */
ScalaJS.c.sc_AbstractMap = (function() {
  ScalaJS.c.sc_AbstractIterable.call(this)
});
ScalaJS.c.sc_AbstractMap.prototype = new ScalaJS.h.sc_AbstractIterable();
ScalaJS.c.sc_AbstractMap.prototype.constructor = ScalaJS.c.sc_AbstractMap;
/** @constructor */
ScalaJS.h.sc_AbstractMap = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_AbstractMap.prototype = ScalaJS.c.sc_AbstractMap.prototype;
ScalaJS.c.sc_AbstractMap.prototype.apply__O__O = (function(key) {
  return ScalaJS.s.sc_MapLike$class__apply__sc_MapLike__O__O(this, key)
});
ScalaJS.c.sc_AbstractMap.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_MapLike$class__isEmpty__sc_MapLike__Z(this)
});
ScalaJS.c.sc_AbstractMap.prototype.runWith__F1__F1 = (function(action) {
  return ScalaJS.s.s_PartialFunction$class__runWith__s_PartialFunction__F1__F1(this, action)
});
ScalaJS.c.sc_AbstractMap.prototype.equals__O__Z = (function(that) {
  return ScalaJS.s.sc_GenMapLike$class__equals__sc_GenMapLike__O__Z(this, that)
});
ScalaJS.c.sc_AbstractMap.prototype.toString__T = (function() {
  return ScalaJS.s.sc_TraversableLike$class__toString__sc_TraversableLike__T(this)
});
ScalaJS.c.sc_AbstractMap.prototype.contains__O__Z = (function(key) {
  return ScalaJS.s.sc_MapLike$class__contains__sc_MapLike__O__Z(this, key)
});
ScalaJS.c.sc_AbstractMap.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return ScalaJS.s.sc_MapLike$class__addString__sc_MapLike__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end)
});
ScalaJS.c.sc_AbstractMap.prototype.isDefinedAt__O__Z = (function(key) {
  return this.contains__O__Z(key)
});
ScalaJS.c.sc_AbstractMap.prototype.hashCode__I = (function() {
  var this$1 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  var xs = this.seq__sc_Map();
  return this$1.unorderedHash__sc_TraversableOnce__I__I(xs, this$1.mapSeed$2)
});
ScalaJS.c.sc_AbstractMap.prototype.applyOrElse__O__F1__O = (function(x, default$2) {
  return ScalaJS.s.s_PartialFunction$class__applyOrElse__s_PartialFunction__O__F1__O(this, x, default$2)
});
ScalaJS.c.sc_AbstractMap.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_MapBuilder().init___sc_GenMap(this.empty__sc_Map())
});
ScalaJS.c.sc_AbstractMap.prototype.stringPrefix__T = (function() {
  return "Map"
});
/** @constructor */
ScalaJS.c.sc_AbstractSet = (function() {
  ScalaJS.c.sc_AbstractIterable.call(this)
});
ScalaJS.c.sc_AbstractSet.prototype = new ScalaJS.h.sc_AbstractIterable();
ScalaJS.c.sc_AbstractSet.prototype.constructor = ScalaJS.c.sc_AbstractSet;
/** @constructor */
ScalaJS.h.sc_AbstractSet = (function() {
  /*<skip>*/
});
ScalaJS.h.sc_AbstractSet.prototype = ScalaJS.c.sc_AbstractSet.prototype;
ScalaJS.c.sc_AbstractSet.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_SetLike$class__isEmpty__sc_SetLike__Z(this)
});
ScalaJS.c.sc_AbstractSet.prototype.equals__O__Z = (function(that) {
  return ScalaJS.s.sc_GenSetLike$class__equals__sc_GenSetLike__O__Z(this, that)
});
ScalaJS.c.sc_AbstractSet.prototype.toString__T = (function() {
  return ScalaJS.s.sc_TraversableLike$class__toString__sc_TraversableLike__T(this)
});
ScalaJS.c.sc_AbstractSet.prototype.subsetOf__sc_GenSet__Z = (function(that) {
  return this.forall__F1__Z(that)
});
ScalaJS.c.sc_AbstractSet.prototype.hashCode__I = (function() {
  var this$1 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  return this$1.unorderedHash__sc_TraversableOnce__I__I(this, this$1.setSeed$2)
});
ScalaJS.c.sc_AbstractSet.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_SetBuilder().init___sc_Set(this.empty__sc_Set())
});
ScalaJS.c.sc_AbstractSet.prototype.stringPrefix__T = (function() {
  return "Set"
});
/** @constructor */
ScalaJS.c.sci_AbstractMap = (function() {
  ScalaJS.c.sc_AbstractMap.call(this)
});
ScalaJS.c.sci_AbstractMap.prototype = new ScalaJS.h.sc_AbstractMap();
ScalaJS.c.sci_AbstractMap.prototype.constructor = ScalaJS.c.sci_AbstractMap;
/** @constructor */
ScalaJS.h.sci_AbstractMap = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_AbstractMap.prototype = ScalaJS.c.sci_AbstractMap.prototype;
ScalaJS.c.sci_AbstractMap.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sci_AbstractMap.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_AbstractMap.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_AbstractMap.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Iterable$()
});
ScalaJS.c.sci_AbstractMap.prototype.empty__sc_Map = (function() {
  return this.empty__sci_Map()
});
ScalaJS.c.sci_AbstractMap.prototype.empty__sci_Map = (function() {
  return ScalaJS.m.sci_Map$EmptyMap$()
});
ScalaJS.c.sci_AbstractMap.prototype.seq__sc_Map = (function() {
  return this
});
/** @constructor */
ScalaJS.c.sci_ListSet = (function() {
  ScalaJS.c.sc_AbstractSet.call(this)
});
ScalaJS.c.sci_ListSet.prototype = new ScalaJS.h.sc_AbstractSet();
ScalaJS.c.sci_ListSet.prototype.constructor = ScalaJS.c.sci_ListSet;
/** @constructor */
ScalaJS.h.sci_ListSet = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListSet.prototype = ScalaJS.c.sci_ListSet.prototype;
ScalaJS.c.sci_ListSet.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_ListSet.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sci_ListSet.prototype.head__O = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("Set has no elements")
});
ScalaJS.c.sci_ListSet.prototype.apply__O__O = (function(v1) {
  return this.contains__O__Z(v1)
});
ScalaJS.c.sci_ListSet.prototype.isEmpty__Z = (function() {
  return true
});
ScalaJS.c.sci_ListSet.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_ListSet.prototype.scala$collection$immutable$ListSet$$unchecked$undouter__sci_ListSet = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("Empty ListSet has no outer pointer")
});
ScalaJS.c.sci_ListSet.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_ListSet$()
});
ScalaJS.c.sci_ListSet.prototype.$$plus__O__sci_ListSet = (function(elem) {
  return new ScalaJS.c.sci_ListSet$Node().init___sci_ListSet__O(this, elem)
});
ScalaJS.c.sci_ListSet.prototype.size__I = (function() {
  return 0
});
ScalaJS.c.sci_ListSet.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.sci_ListSet$$anon$1().init___sci_ListSet(this)
});
ScalaJS.c.sci_ListSet.prototype.empty__sc_Set = (function() {
  return ScalaJS.m.sci_ListSet$EmptyListSet$()
});
ScalaJS.c.sci_ListSet.prototype.tail__O = (function() {
  return this.tail__sci_ListSet()
});
ScalaJS.c.sci_ListSet.prototype.contains__O__Z = (function(elem) {
  return false
});
ScalaJS.c.sci_ListSet.prototype.$$plus__O__sc_Set = (function(elem) {
  return this.$$plus__O__sci_ListSet(elem)
});
ScalaJS.c.sci_ListSet.prototype.tail__sci_ListSet = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("Next of an empty set")
});
ScalaJS.c.sci_ListSet.prototype.stringPrefix__T = (function() {
  return "ListSet"
});
ScalaJS.is.sci_ListSet = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_ListSet)))
});
ScalaJS.as.sci_ListSet = (function(obj) {
  return ((ScalaJS.is.sci_ListSet(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.ListSet"))
});
ScalaJS.isArrayOf.sci_ListSet = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ListSet)))
});
ScalaJS.asArrayOf.sci_ListSet = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_ListSet(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.ListSet;", depth))
});
/** @constructor */
ScalaJS.c.sci_Set$EmptySet$ = (function() {
  ScalaJS.c.sc_AbstractSet.call(this)
});
ScalaJS.c.sci_Set$EmptySet$.prototype = new ScalaJS.h.sc_AbstractSet();
ScalaJS.c.sci_Set$EmptySet$.prototype.constructor = ScalaJS.c.sci_Set$EmptySet$;
/** @constructor */
ScalaJS.h.sci_Set$EmptySet$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Set$EmptySet$.prototype = ScalaJS.c.sci_Set$EmptySet$.prototype;
ScalaJS.c.sci_Set$EmptySet$.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_Set$EmptySet$.prototype.init___ = (function() {
  ScalaJS.n.sci_Set$EmptySet$ = this;
  return this
});
ScalaJS.c.sci_Set$EmptySet$.prototype.apply__O__O = (function(v1) {
  return false
});
ScalaJS.c.sci_Set$EmptySet$.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_Set$EmptySet$.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Set$()
});
ScalaJS.c.sci_Set$EmptySet$.prototype.foreach__F1__V = (function(f) {
  /*<skip>*/
});
ScalaJS.c.sci_Set$EmptySet$.prototype.size__I = (function() {
  return 0
});
ScalaJS.c.sci_Set$EmptySet$.prototype.iterator__sc_Iterator = (function() {
  return ScalaJS.m.sc_Iterator$().empty$1
});
ScalaJS.c.sci_Set$EmptySet$.prototype.empty__sc_Set = (function() {
  return ScalaJS.m.sci_Set$EmptySet$()
});
ScalaJS.c.sci_Set$EmptySet$.prototype.$$plus__O__sc_Set = (function(elem) {
  return new ScalaJS.c.sci_Set$Set1().init___O(elem)
});
ScalaJS.d.sci_Set$EmptySet$ = new ScalaJS.ClassTypeData({
  sci_Set$EmptySet$: 0
}, false, "scala.collection.immutable.Set$EmptySet$", {
  sci_Set$EmptySet$: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Set$EmptySet$.prototype.$classData = ScalaJS.d.sci_Set$EmptySet$;
ScalaJS.n.sci_Set$EmptySet$ = (void 0);
ScalaJS.m.sci_Set$EmptySet$ = (function() {
  if ((!ScalaJS.n.sci_Set$EmptySet$)) {
    ScalaJS.n.sci_Set$EmptySet$ = new ScalaJS.c.sci_Set$EmptySet$().init___()
  };
  return ScalaJS.n.sci_Set$EmptySet$
});
/** @constructor */
ScalaJS.c.sci_Set$Set1 = (function() {
  ScalaJS.c.sc_AbstractSet.call(this);
  this.elem1$4 = null
});
ScalaJS.c.sci_Set$Set1.prototype = new ScalaJS.h.sc_AbstractSet();
ScalaJS.c.sci_Set$Set1.prototype.constructor = ScalaJS.c.sci_Set$Set1;
/** @constructor */
ScalaJS.h.sci_Set$Set1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Set$Set1.prototype = ScalaJS.c.sci_Set$Set1.prototype;
ScalaJS.c.sci_Set$Set1.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_Set$Set1.prototype.apply__O__O = (function(v1) {
  return this.contains__O__Z(v1)
});
ScalaJS.c.sci_Set$Set1.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_Set$Set1.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Set$()
});
ScalaJS.c.sci_Set$Set1.prototype.forall__F1__Z = (function(f) {
  return ScalaJS.uZ(f.apply__O__O(this.elem1$4))
});
ScalaJS.c.sci_Set$Set1.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(this.elem1$4)
});
ScalaJS.c.sci_Set$Set1.prototype.size__I = (function() {
  return 1
});
ScalaJS.c.sci_Set$Set1.prototype.init___O = (function(elem1) {
  this.elem1$4 = elem1;
  return this
});
ScalaJS.c.sci_Set$Set1.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([this.elem1$4]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Set$Set1.prototype.empty__sc_Set = (function() {
  return ScalaJS.m.sci_Set$EmptySet$()
});
ScalaJS.c.sci_Set$Set1.prototype.$$plus__O__sci_Set = (function(elem) {
  return (this.contains__O__Z(elem) ? this : new ScalaJS.c.sci_Set$Set2().init___O__O(this.elem1$4, elem))
});
ScalaJS.c.sci_Set$Set1.prototype.contains__O__Z = (function(elem) {
  return ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem1$4)
});
ScalaJS.c.sci_Set$Set1.prototype.$$plus__O__sc_Set = (function(elem) {
  return this.$$plus__O__sci_Set(elem)
});
ScalaJS.d.sci_Set$Set1 = new ScalaJS.ClassTypeData({
  sci_Set$Set1: 0
}, false, "scala.collection.immutable.Set$Set1", {
  sci_Set$Set1: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Set$Set1.prototype.$classData = ScalaJS.d.sci_Set$Set1;
/** @constructor */
ScalaJS.c.sci_Set$Set2 = (function() {
  ScalaJS.c.sc_AbstractSet.call(this);
  this.elem1$4 = null;
  this.elem2$4 = null
});
ScalaJS.c.sci_Set$Set2.prototype = new ScalaJS.h.sc_AbstractSet();
ScalaJS.c.sci_Set$Set2.prototype.constructor = ScalaJS.c.sci_Set$Set2;
/** @constructor */
ScalaJS.h.sci_Set$Set2 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Set$Set2.prototype = ScalaJS.c.sci_Set$Set2.prototype;
ScalaJS.c.sci_Set$Set2.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_Set$Set2.prototype.apply__O__O = (function(v1) {
  return this.contains__O__Z(v1)
});
ScalaJS.c.sci_Set$Set2.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_Set$Set2.prototype.init___O__O = (function(elem1, elem2) {
  this.elem1$4 = elem1;
  this.elem2$4 = elem2;
  return this
});
ScalaJS.c.sci_Set$Set2.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Set$()
});
ScalaJS.c.sci_Set$Set2.prototype.forall__F1__Z = (function(f) {
  return (ScalaJS.uZ(f.apply__O__O(this.elem1$4)) && ScalaJS.uZ(f.apply__O__O(this.elem2$4)))
});
ScalaJS.c.sci_Set$Set2.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(this.elem1$4);
  f.apply__O__O(this.elem2$4)
});
ScalaJS.c.sci_Set$Set2.prototype.size__I = (function() {
  return 2
});
ScalaJS.c.sci_Set$Set2.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([this.elem1$4, this.elem2$4]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Set$Set2.prototype.empty__sc_Set = (function() {
  return ScalaJS.m.sci_Set$EmptySet$()
});
ScalaJS.c.sci_Set$Set2.prototype.$$plus__O__sci_Set = (function(elem) {
  return (this.contains__O__Z(elem) ? this : new ScalaJS.c.sci_Set$Set3().init___O__O__O(this.elem1$4, this.elem2$4, elem))
});
ScalaJS.c.sci_Set$Set2.prototype.contains__O__Z = (function(elem) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem1$4) || ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem2$4))
});
ScalaJS.c.sci_Set$Set2.prototype.$$plus__O__sc_Set = (function(elem) {
  return this.$$plus__O__sci_Set(elem)
});
ScalaJS.d.sci_Set$Set2 = new ScalaJS.ClassTypeData({
  sci_Set$Set2: 0
}, false, "scala.collection.immutable.Set$Set2", {
  sci_Set$Set2: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Set$Set2.prototype.$classData = ScalaJS.d.sci_Set$Set2;
/** @constructor */
ScalaJS.c.sci_Set$Set3 = (function() {
  ScalaJS.c.sc_AbstractSet.call(this);
  this.elem1$4 = null;
  this.elem2$4 = null;
  this.elem3$4 = null
});
ScalaJS.c.sci_Set$Set3.prototype = new ScalaJS.h.sc_AbstractSet();
ScalaJS.c.sci_Set$Set3.prototype.constructor = ScalaJS.c.sci_Set$Set3;
/** @constructor */
ScalaJS.h.sci_Set$Set3 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Set$Set3.prototype = ScalaJS.c.sci_Set$Set3.prototype;
ScalaJS.c.sci_Set$Set3.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_Set$Set3.prototype.apply__O__O = (function(v1) {
  return this.contains__O__Z(v1)
});
ScalaJS.c.sci_Set$Set3.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_Set$Set3.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Set$()
});
ScalaJS.c.sci_Set$Set3.prototype.forall__F1__Z = (function(f) {
  return ((ScalaJS.uZ(f.apply__O__O(this.elem1$4)) && ScalaJS.uZ(f.apply__O__O(this.elem2$4))) && ScalaJS.uZ(f.apply__O__O(this.elem3$4)))
});
ScalaJS.c.sci_Set$Set3.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(this.elem1$4);
  f.apply__O__O(this.elem2$4);
  f.apply__O__O(this.elem3$4)
});
ScalaJS.c.sci_Set$Set3.prototype.init___O__O__O = (function(elem1, elem2, elem3) {
  this.elem1$4 = elem1;
  this.elem2$4 = elem2;
  this.elem3$4 = elem3;
  return this
});
ScalaJS.c.sci_Set$Set3.prototype.size__I = (function() {
  return 3
});
ScalaJS.c.sci_Set$Set3.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([this.elem1$4, this.elem2$4, this.elem3$4]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Set$Set3.prototype.empty__sc_Set = (function() {
  return ScalaJS.m.sci_Set$EmptySet$()
});
ScalaJS.c.sci_Set$Set3.prototype.$$plus__O__sci_Set = (function(elem) {
  return (this.contains__O__Z(elem) ? this : new ScalaJS.c.sci_Set$Set4().init___O__O__O__O(this.elem1$4, this.elem2$4, this.elem3$4, elem))
});
ScalaJS.c.sci_Set$Set3.prototype.contains__O__Z = (function(elem) {
  return ((ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem1$4) || ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem2$4)) || ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem3$4))
});
ScalaJS.c.sci_Set$Set3.prototype.$$plus__O__sc_Set = (function(elem) {
  return this.$$plus__O__sci_Set(elem)
});
ScalaJS.d.sci_Set$Set3 = new ScalaJS.ClassTypeData({
  sci_Set$Set3: 0
}, false, "scala.collection.immutable.Set$Set3", {
  sci_Set$Set3: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Set$Set3.prototype.$classData = ScalaJS.d.sci_Set$Set3;
/** @constructor */
ScalaJS.c.sci_Set$Set4 = (function() {
  ScalaJS.c.sc_AbstractSet.call(this);
  this.elem1$4 = null;
  this.elem2$4 = null;
  this.elem3$4 = null;
  this.elem4$4 = null
});
ScalaJS.c.sci_Set$Set4.prototype = new ScalaJS.h.sc_AbstractSet();
ScalaJS.c.sci_Set$Set4.prototype.constructor = ScalaJS.c.sci_Set$Set4;
/** @constructor */
ScalaJS.h.sci_Set$Set4 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Set$Set4.prototype = ScalaJS.c.sci_Set$Set4.prototype;
ScalaJS.c.sci_Set$Set4.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_Set$Set4.prototype.apply__O__O = (function(v1) {
  return this.contains__O__Z(v1)
});
ScalaJS.c.sci_Set$Set4.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_Set$Set4.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Set$()
});
ScalaJS.c.sci_Set$Set4.prototype.forall__F1__Z = (function(f) {
  return (((ScalaJS.uZ(f.apply__O__O(this.elem1$4)) && ScalaJS.uZ(f.apply__O__O(this.elem2$4))) && ScalaJS.uZ(f.apply__O__O(this.elem3$4))) && ScalaJS.uZ(f.apply__O__O(this.elem4$4)))
});
ScalaJS.c.sci_Set$Set4.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(this.elem1$4);
  f.apply__O__O(this.elem2$4);
  f.apply__O__O(this.elem3$4);
  f.apply__O__O(this.elem4$4)
});
ScalaJS.c.sci_Set$Set4.prototype.size__I = (function() {
  return 4
});
ScalaJS.c.sci_Set$Set4.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([this.elem1$4, this.elem2$4, this.elem3$4, this.elem4$4]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Set$Set4.prototype.empty__sc_Set = (function() {
  return ScalaJS.m.sci_Set$EmptySet$()
});
ScalaJS.c.sci_Set$Set4.prototype.$$plus__O__sci_Set = (function(elem) {
  if (this.contains__O__Z(elem)) {
    return this
  } else {
    var this$1 = new ScalaJS.c.sci_HashSet().init___();
    var elem1 = this.elem1$4;
    var elem2 = this.elem2$4;
    var array = [this.elem3$4, this.elem4$4, elem];
    var this$2 = this$1.$$plus__O__sci_HashSet(elem1).$$plus__O__sci_HashSet(elem2);
    var start = 0;
    var end = ScalaJS.uI(array["length"]);
    var z = this$2;
    x: {
      var jsx$1;
      _foldl: while (true) {
        if ((start === end)) {
          var jsx$1 = z;
          break x
        } else {
          var temp$start = ((1 + start) | 0);
          var arg1 = z;
          var index = start;
          var arg2 = array[index];
          var x$2 = ScalaJS.as.sc_Set(arg1);
          var temp$z = x$2.$$plus__O__sc_Set(arg2);
          start = temp$start;
          z = temp$z;
          continue _foldl
        }
      }
    };
    return ScalaJS.as.sci_HashSet(ScalaJS.as.sc_Set(jsx$1))
  }
});
ScalaJS.c.sci_Set$Set4.prototype.contains__O__Z = (function(elem) {
  return (((ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem1$4) || ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem2$4)) || ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem3$4)) || ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(elem, this.elem4$4))
});
ScalaJS.c.sci_Set$Set4.prototype.init___O__O__O__O = (function(elem1, elem2, elem3, elem4) {
  this.elem1$4 = elem1;
  this.elem2$4 = elem2;
  this.elem3$4 = elem3;
  this.elem4$4 = elem4;
  return this
});
ScalaJS.c.sci_Set$Set4.prototype.$$plus__O__sc_Set = (function(elem) {
  return this.$$plus__O__sci_Set(elem)
});
ScalaJS.d.sci_Set$Set4 = new ScalaJS.ClassTypeData({
  sci_Set$Set4: 0
}, false, "scala.collection.immutable.Set$Set4", {
  sci_Set$Set4: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Set$Set4.prototype.$classData = ScalaJS.d.sci_Set$Set4;
ScalaJS.is.scm_IndexedSeq = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_IndexedSeq)))
});
ScalaJS.as.scm_IndexedSeq = (function(obj) {
  return ((ScalaJS.is.scm_IndexedSeq(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.IndexedSeq"))
});
ScalaJS.isArrayOf.scm_IndexedSeq = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_IndexedSeq)))
});
ScalaJS.asArrayOf.scm_IndexedSeq = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_IndexedSeq(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.IndexedSeq;", depth))
});
/** @constructor */
ScalaJS.c.sci_HashSet = (function() {
  ScalaJS.c.sc_AbstractSet.call(this)
});
ScalaJS.c.sci_HashSet.prototype = new ScalaJS.h.sc_AbstractSet();
ScalaJS.c.sci_HashSet.prototype.constructor = ScalaJS.c.sci_HashSet;
/** @constructor */
ScalaJS.h.sci_HashSet = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet.prototype = ScalaJS.c.sci_HashSet.prototype;
ScalaJS.c.sci_HashSet.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_HashSet.prototype.updated0__O__I__I__sci_HashSet = (function(key, hash, level) {
  return new ScalaJS.c.sci_HashSet$HashSet1().init___O__I(key, hash)
});
ScalaJS.c.sci_HashSet.prototype.computeHash__O__I = (function(key) {
  return this.improve__I__I(ScalaJS.m.sr_ScalaRunTime$().hash__O__I(key))
});
ScalaJS.c.sci_HashSet.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sci_HashSet.prototype.apply__O__O = (function(v1) {
  return this.contains__O__Z(v1)
});
ScalaJS.c.sci_HashSet.prototype.$$plus__O__sci_HashSet = (function(e) {
  return this.updated0__O__I__I__sci_HashSet(e, this.computeHash__O__I(e), 0)
});
ScalaJS.c.sci_HashSet.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_HashSet.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_HashSet$()
});
ScalaJS.c.sci_HashSet.prototype.foreach__F1__V = (function(f) {
  /*<skip>*/
});
ScalaJS.c.sci_HashSet.prototype.subsetOf__sc_GenSet__Z = (function(that) {
  if (ScalaJS.is.sci_HashSet(that)) {
    var x2 = ScalaJS.as.sci_HashSet(that);
    return this.subsetOf0__sci_HashSet__I__Z(x2, 0)
  } else {
    var this$1 = this.iterator__sc_Iterator();
    return ScalaJS.s.sc_Iterator$class__forall__sc_Iterator__F1__Z(this$1, that)
  }
});
ScalaJS.c.sci_HashSet.prototype.size__I = (function() {
  return 0
});
ScalaJS.c.sci_HashSet.prototype.iterator__sc_Iterator = (function() {
  return ScalaJS.m.sc_Iterator$().empty$1
});
ScalaJS.c.sci_HashSet.prototype.empty__sc_Set = (function() {
  return ScalaJS.m.sci_HashSet$EmptyHashSet$()
});
ScalaJS.c.sci_HashSet.prototype.improve__I__I = (function(hcode) {
  var h = ((hcode + (~(hcode << 9))) | 0);
  h = (h ^ ((h >>> 14) | 0));
  h = ((h + (h << 4)) | 0);
  return (h ^ ((h >>> 10) | 0))
});
ScalaJS.c.sci_HashSet.prototype.contains__O__Z = (function(e) {
  return this.get0__O__I__I__Z(e, this.computeHash__O__I(e), 0)
});
ScalaJS.c.sci_HashSet.prototype.$$plus__O__sc_Set = (function(elem) {
  return this.$$plus__O__sci_HashSet(elem)
});
ScalaJS.c.sci_HashSet.prototype.get0__O__I__I__Z = (function(key, hash, level) {
  return false
});
ScalaJS.c.sci_HashSet.prototype.subsetOf0__sci_HashSet__I__Z = (function(that, level) {
  return true
});
ScalaJS.is.sci_HashSet = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_HashSet)))
});
ScalaJS.as.sci_HashSet = (function(obj) {
  return ((ScalaJS.is.sci_HashSet(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.HashSet"))
});
ScalaJS.isArrayOf.sci_HashSet = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_HashSet)))
});
ScalaJS.asArrayOf.sci_HashSet = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_HashSet(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.HashSet;", depth))
});
ScalaJS.d.sci_HashSet = new ScalaJS.ClassTypeData({
  sci_HashSet: 0
}, false, "scala.collection.immutable.HashSet", {
  sci_HashSet: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_HashSet.prototype.$classData = ScalaJS.d.sci_HashSet;
/** @constructor */
ScalaJS.c.sci_ListSet$EmptyListSet$ = (function() {
  ScalaJS.c.sci_ListSet.call(this)
});
ScalaJS.c.sci_ListSet$EmptyListSet$.prototype = new ScalaJS.h.sci_ListSet();
ScalaJS.c.sci_ListSet$EmptyListSet$.prototype.constructor = ScalaJS.c.sci_ListSet$EmptyListSet$;
/** @constructor */
ScalaJS.h.sci_ListSet$EmptyListSet$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListSet$EmptyListSet$.prototype = ScalaJS.c.sci_ListSet$EmptyListSet$.prototype;
ScalaJS.d.sci_ListSet$EmptyListSet$ = new ScalaJS.ClassTypeData({
  sci_ListSet$EmptyListSet$: 0
}, false, "scala.collection.immutable.ListSet$EmptyListSet$", {
  sci_ListSet$EmptyListSet$: 1,
  sci_ListSet: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_ListSet$EmptyListSet$.prototype.$classData = ScalaJS.d.sci_ListSet$EmptyListSet$;
ScalaJS.n.sci_ListSet$EmptyListSet$ = (void 0);
ScalaJS.m.sci_ListSet$EmptyListSet$ = (function() {
  if ((!ScalaJS.n.sci_ListSet$EmptyListSet$)) {
    ScalaJS.n.sci_ListSet$EmptyListSet$ = new ScalaJS.c.sci_ListSet$EmptyListSet$().init___()
  };
  return ScalaJS.n.sci_ListSet$EmptyListSet$
});
/** @constructor */
ScalaJS.c.sci_ListSet$Node = (function() {
  ScalaJS.c.sci_ListSet.call(this);
  this.head$5 = null;
  this.$$outer$f = null
});
ScalaJS.c.sci_ListSet$Node.prototype = new ScalaJS.h.sci_ListSet();
ScalaJS.c.sci_ListSet$Node.prototype.constructor = ScalaJS.c.sci_ListSet$Node;
/** @constructor */
ScalaJS.h.sci_ListSet$Node = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListSet$Node.prototype = ScalaJS.c.sci_ListSet$Node.prototype;
ScalaJS.c.sci_ListSet$Node.prototype.head__O = (function() {
  return this.head$5
});
ScalaJS.c.sci_ListSet$Node.prototype.isEmpty__Z = (function() {
  return false
});
ScalaJS.c.sci_ListSet$Node.prototype.scala$collection$immutable$ListSet$$unchecked$undouter__sci_ListSet = (function() {
  return this.$$outer$f
});
ScalaJS.c.sci_ListSet$Node.prototype.$$plus__O__sci_ListSet = (function(e) {
  return (this.containsInternal__p5__sci_ListSet__O__Z(this, e) ? this : new ScalaJS.c.sci_ListSet$Node().init___sci_ListSet__O(this, e))
});
ScalaJS.c.sci_ListSet$Node.prototype.sizeInternal__p5__sci_ListSet__I__I = (function(n, acc) {
  _sizeInternal: while (true) {
    if (n.isEmpty__Z()) {
      return acc
    } else {
      var temp$n = n.scala$collection$immutable$ListSet$$unchecked$undouter__sci_ListSet();
      var temp$acc = ((1 + acc) | 0);
      n = temp$n;
      acc = temp$acc;
      continue _sizeInternal
    }
  }
});
ScalaJS.c.sci_ListSet$Node.prototype.size__I = (function() {
  return this.sizeInternal__p5__sci_ListSet__I__I(this, 0)
});
ScalaJS.c.sci_ListSet$Node.prototype.init___sci_ListSet__O = (function($$outer, head) {
  this.head$5 = head;
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$f = $$outer
  };
  return this
});
ScalaJS.c.sci_ListSet$Node.prototype.contains__O__Z = (function(e) {
  return this.containsInternal__p5__sci_ListSet__O__Z(this, e)
});
ScalaJS.c.sci_ListSet$Node.prototype.tail__O = (function() {
  return this.$$outer$f
});
ScalaJS.c.sci_ListSet$Node.prototype.containsInternal__p5__sci_ListSet__O__Z = (function(n, e) {
  _containsInternal: while (true) {
    if ((!n.isEmpty__Z())) {
      if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(n.head__O(), e)) {
        return true
      } else {
        n = n.scala$collection$immutable$ListSet$$unchecked$undouter__sci_ListSet();
        continue _containsInternal
      }
    } else {
      return false
    }
  }
});
ScalaJS.c.sci_ListSet$Node.prototype.tail__sci_ListSet = (function() {
  return this.$$outer$f
});
ScalaJS.c.sci_ListSet$Node.prototype.$$plus__O__sc_Set = (function(elem) {
  return this.$$plus__O__sci_ListSet(elem)
});
ScalaJS.d.sci_ListSet$Node = new ScalaJS.ClassTypeData({
  sci_ListSet$Node: 0
}, false, "scala.collection.immutable.ListSet$Node", {
  sci_ListSet$Node: 1,
  sci_ListSet: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_ListSet$Node.prototype.$classData = ScalaJS.d.sci_ListSet$Node;
/** @constructor */
ScalaJS.c.scm_AbstractSeq = (function() {
  ScalaJS.c.sc_AbstractSeq.call(this)
});
ScalaJS.c.scm_AbstractSeq.prototype = new ScalaJS.h.sc_AbstractSeq();
ScalaJS.c.scm_AbstractSeq.prototype.constructor = ScalaJS.c.scm_AbstractSeq;
/** @constructor */
ScalaJS.h.scm_AbstractSeq = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_AbstractSeq.prototype = ScalaJS.c.scm_AbstractSeq.prototype;
ScalaJS.c.scm_AbstractSeq.prototype.seq__sc_TraversableOnce = (function() {
  return this.seq__scm_Seq()
});
ScalaJS.c.scm_AbstractSeq.prototype.seq__scm_Seq = (function() {
  return this
});
ScalaJS.is.scm_Map = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_Map)))
});
ScalaJS.as.scm_Map = (function(obj) {
  return ((ScalaJS.is.scm_Map(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.Map"))
});
ScalaJS.isArrayOf.scm_Map = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_Map)))
});
ScalaJS.asArrayOf.scm_Map = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_Map(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.Map;", depth))
});
/** @constructor */
ScalaJS.c.sci_HashSet$EmptyHashSet$ = (function() {
  ScalaJS.c.sci_HashSet.call(this)
});
ScalaJS.c.sci_HashSet$EmptyHashSet$.prototype = new ScalaJS.h.sci_HashSet();
ScalaJS.c.sci_HashSet$EmptyHashSet$.prototype.constructor = ScalaJS.c.sci_HashSet$EmptyHashSet$;
/** @constructor */
ScalaJS.h.sci_HashSet$EmptyHashSet$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet$EmptyHashSet$.prototype = ScalaJS.c.sci_HashSet$EmptyHashSet$.prototype;
ScalaJS.d.sci_HashSet$EmptyHashSet$ = new ScalaJS.ClassTypeData({
  sci_HashSet$EmptyHashSet$: 0
}, false, "scala.collection.immutable.HashSet$EmptyHashSet$", {
  sci_HashSet$EmptyHashSet$: 1,
  sci_HashSet: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_HashSet$EmptyHashSet$.prototype.$classData = ScalaJS.d.sci_HashSet$EmptyHashSet$;
ScalaJS.n.sci_HashSet$EmptyHashSet$ = (void 0);
ScalaJS.m.sci_HashSet$EmptyHashSet$ = (function() {
  if ((!ScalaJS.n.sci_HashSet$EmptyHashSet$)) {
    ScalaJS.n.sci_HashSet$EmptyHashSet$ = new ScalaJS.c.sci_HashSet$EmptyHashSet$().init___()
  };
  return ScalaJS.n.sci_HashSet$EmptyHashSet$
});
/** @constructor */
ScalaJS.c.sci_HashSet$HashTrieSet = (function() {
  ScalaJS.c.sci_HashSet.call(this);
  this.bitmap$5 = 0;
  this.elems$5 = null;
  this.size0$5 = 0
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype = new ScalaJS.h.sci_HashSet();
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.constructor = ScalaJS.c.sci_HashSet$HashTrieSet;
/** @constructor */
ScalaJS.h.sci_HashSet$HashTrieSet = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet$HashTrieSet.prototype = ScalaJS.c.sci_HashSet$HashTrieSet.prototype;
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.updated0__O__I__I__sci_HashSet = (function(key, hash, level) {
  var index = (31 & ((hash >>> level) | 0));
  var mask = (1 << index);
  var offset = ScalaJS.m.jl_Integer$().bitCount__I__I((this.bitmap$5 & (((-1) + mask) | 0)));
  if (((this.bitmap$5 & mask) !== 0)) {
    var sub = this.elems$5.u[offset];
    var subNew = sub.updated0__O__I__I__sci_HashSet(key, hash, ((5 + level) | 0));
    if ((sub === subNew)) {
      return this
    } else {
      var elemsNew = ScalaJS.newArrayObject(ScalaJS.d.sci_HashSet.getArrayOf(), [this.elems$5.u["length"]]);
      ScalaJS.m.s_Array$().copy__O__I__O__I__I__V(this.elems$5, 0, elemsNew, 0, this.elems$5.u["length"]);
      elemsNew.u[offset] = subNew;
      return new ScalaJS.c.sci_HashSet$HashTrieSet().init___I__Asci_HashSet__I(this.bitmap$5, elemsNew, ((this.size0$5 + ((subNew.size__I() - sub.size__I()) | 0)) | 0))
    }
  } else {
    var elemsNew$2 = ScalaJS.newArrayObject(ScalaJS.d.sci_HashSet.getArrayOf(), [((1 + this.elems$5.u["length"]) | 0)]);
    ScalaJS.m.s_Array$().copy__O__I__O__I__I__V(this.elems$5, 0, elemsNew$2, 0, offset);
    elemsNew$2.u[offset] = new ScalaJS.c.sci_HashSet$HashSet1().init___O__I(key, hash);
    ScalaJS.m.s_Array$().copy__O__I__O__I__I__V(this.elems$5, offset, elemsNew$2, ((1 + offset) | 0), ((this.elems$5.u["length"] - offset) | 0));
    var bitmapNew = (this.bitmap$5 | mask);
    return new ScalaJS.c.sci_HashSet$HashTrieSet().init___I__Asci_HashSet__I(bitmapNew, elemsNew$2, ((1 + this.size0$5) | 0))
  }
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.foreach__F1__V = (function(f) {
  var i = 0;
  while ((i < this.elems$5.u["length"])) {
    this.elems$5.u[i].foreach__F1__V(f);
    i = ((1 + i) | 0)
  }
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.sci_HashSet$HashTrieSet$$anon$1().init___sci_HashSet$HashTrieSet(this)
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.size__I = (function() {
  return this.size0$5
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.init___I__Asci_HashSet__I = (function(bitmap, elems, size0) {
  this.bitmap$5 = bitmap;
  this.elems$5 = elems;
  this.size0$5 = size0;
  ScalaJS.m.s_Predef$().assert__Z__V((ScalaJS.m.jl_Integer$().bitCount__I__I(bitmap) === elems.u["length"]));
  return this
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.get0__O__I__I__Z = (function(key, hash, level) {
  var index = (31 & ((hash >>> level) | 0));
  var mask = (1 << index);
  if ((this.bitmap$5 === (-1))) {
    return this.elems$5.u[(31 & index)].get0__O__I__I__Z(key, hash, ((5 + level) | 0))
  } else if (((this.bitmap$5 & mask) !== 0)) {
    var offset = ScalaJS.m.jl_Integer$().bitCount__I__I((this.bitmap$5 & (((-1) + mask) | 0)));
    return this.elems$5.u[offset].get0__O__I__I__Z(key, hash, ((5 + level) | 0))
  } else {
    return false
  }
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.subsetOf0__sci_HashSet__I__Z = (function(that, level) {
  if ((that === this)) {
    return true
  } else {
    if (ScalaJS.is.sci_HashSet$HashTrieSet(that)) {
      var x2 = ScalaJS.as.sci_HashSet$HashTrieSet(that);
      if ((this.size0$5 <= x2.size0$5)) {
        var abm = this.bitmap$5;
        var a = this.elems$5;
        var ai = 0;
        var b = x2.elems$5;
        var bbm = x2.bitmap$5;
        var bi = 0;
        if (((abm & bbm) === abm)) {
          while ((abm !== 0)) {
            var alsb = (abm ^ (abm & (((-1) + abm) | 0)));
            var blsb = (bbm ^ (bbm & (((-1) + bbm) | 0)));
            if ((alsb === blsb)) {
              if ((!a.u[ai].subsetOf0__sci_HashSet__I__Z(b.u[bi], ((5 + level) | 0)))) {
                return false
              };
              abm = (abm & (~alsb));
              ai = ((1 + ai) | 0)
            };
            bbm = (bbm & (~blsb));
            bi = ((1 + bi) | 0)
          };
          return true
        } else {
          return false
        }
      }
    };
    return false
  }
});
ScalaJS.is.sci_HashSet$HashTrieSet = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_HashSet$HashTrieSet)))
});
ScalaJS.as.sci_HashSet$HashTrieSet = (function(obj) {
  return ((ScalaJS.is.sci_HashSet$HashTrieSet(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.HashSet$HashTrieSet"))
});
ScalaJS.isArrayOf.sci_HashSet$HashTrieSet = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_HashSet$HashTrieSet)))
});
ScalaJS.asArrayOf.sci_HashSet$HashTrieSet = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_HashSet$HashTrieSet(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.HashSet$HashTrieSet;", depth))
});
ScalaJS.d.sci_HashSet$HashTrieSet = new ScalaJS.ClassTypeData({
  sci_HashSet$HashTrieSet: 0
}, false, "scala.collection.immutable.HashSet$HashTrieSet", {
  sci_HashSet$HashTrieSet: 1,
  sci_HashSet: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_HashSet$HashTrieSet.prototype.$classData = ScalaJS.d.sci_HashSet$HashTrieSet;
/** @constructor */
ScalaJS.c.sci_HashSet$LeafHashSet = (function() {
  ScalaJS.c.sci_HashSet.call(this)
});
ScalaJS.c.sci_HashSet$LeafHashSet.prototype = new ScalaJS.h.sci_HashSet();
ScalaJS.c.sci_HashSet$LeafHashSet.prototype.constructor = ScalaJS.c.sci_HashSet$LeafHashSet;
/** @constructor */
ScalaJS.h.sci_HashSet$LeafHashSet = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet$LeafHashSet.prototype = ScalaJS.c.sci_HashSet$LeafHashSet.prototype;
/** @constructor */
ScalaJS.c.sci_ListMap = (function() {
  ScalaJS.c.sci_AbstractMap.call(this)
});
ScalaJS.c.sci_ListMap.prototype = new ScalaJS.h.sci_AbstractMap();
ScalaJS.c.sci_ListMap.prototype.constructor = ScalaJS.c.sci_ListMap;
/** @constructor */
ScalaJS.h.sci_ListMap = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListMap.prototype = ScalaJS.c.sci_ListMap.prototype;
ScalaJS.c.sci_ListMap.prototype.value__O = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("empty map")
});
ScalaJS.c.sci_ListMap.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_ListMap.prototype.empty__sc_Map = (function() {
  return ScalaJS.m.sci_ListMap$EmptyListMap$()
});
ScalaJS.c.sci_ListMap.prototype.empty__sci_Map = (function() {
  return ScalaJS.m.sci_ListMap$EmptyListMap$()
});
ScalaJS.c.sci_ListMap.prototype.size__I = (function() {
  return 0
});
ScalaJS.c.sci_ListMap.prototype.seq__sc_Map = (function() {
  return this
});
ScalaJS.c.sci_ListMap.prototype.iterator__sc_Iterator = (function() {
  var this$1 = new ScalaJS.c.sci_ListMap$$anon$1().init___sci_ListMap(this);
  var this$2 = ScalaJS.m.sci_List$();
  var cbf = this$2.ReusableCBFInstance$2;
  var this$3 = ScalaJS.as.sci_List(ScalaJS.s.sc_TraversableOnce$class__to__sc_TraversableOnce__scg_CanBuildFrom__O(this$1, cbf));
  return ScalaJS.s.sc_SeqLike$class__reverseIterator__sc_SeqLike__sc_Iterator(this$3)
});
ScalaJS.c.sci_ListMap.prototype.key__O = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("empty map")
});
ScalaJS.c.sci_ListMap.prototype.updated__O__O__sci_ListMap = (function(key, value) {
  return new ScalaJS.c.sci_ListMap$Node().init___sci_ListMap__O__O(this, key, value)
});
ScalaJS.c.sci_ListMap.prototype.get__O__s_Option = (function(key) {
  return ScalaJS.m.s_None$()
});
ScalaJS.c.sci_ListMap.prototype.next__sci_ListMap = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("empty map")
});
ScalaJS.c.sci_ListMap.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  return this.updated__O__O__sci_ListMap(kv.$$und1__O(), kv.$$und2__O())
});
ScalaJS.is.sci_ListMap = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_ListMap)))
});
ScalaJS.as.sci_ListMap = (function(obj) {
  return ((ScalaJS.is.sci_ListMap(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.ListMap"))
});
ScalaJS.isArrayOf.sci_ListMap = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ListMap)))
});
ScalaJS.asArrayOf.sci_ListMap = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_ListMap(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.ListMap;", depth))
});
/** @constructor */
ScalaJS.c.sci_Map$EmptyMap$ = (function() {
  ScalaJS.c.sci_AbstractMap.call(this)
});
ScalaJS.c.sci_Map$EmptyMap$.prototype = new ScalaJS.h.sci_AbstractMap();
ScalaJS.c.sci_Map$EmptyMap$.prototype.constructor = ScalaJS.c.sci_Map$EmptyMap$;
/** @constructor */
ScalaJS.h.sci_Map$EmptyMap$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Map$EmptyMap$.prototype = ScalaJS.c.sci_Map$EmptyMap$.prototype;
ScalaJS.c.sci_Map$EmptyMap$.prototype.iterator__sc_Iterator = (function() {
  return ScalaJS.m.sc_Iterator$().empty$1
});
ScalaJS.c.sci_Map$EmptyMap$.prototype.size__I = (function() {
  return 0
});
ScalaJS.c.sci_Map$EmptyMap$.prototype.get__O__s_Option = (function(key) {
  return ScalaJS.m.s_None$()
});
ScalaJS.c.sci_Map$EmptyMap$.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  var key = kv.$$und1__O();
  var value = kv.$$und2__O();
  return new ScalaJS.c.sci_Map$Map1().init___O__O(key, value)
});
ScalaJS.d.sci_Map$EmptyMap$ = new ScalaJS.ClassTypeData({
  sci_Map$EmptyMap$: 0
}, false, "scala.collection.immutable.Map$EmptyMap$", {
  sci_Map$EmptyMap$: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Map$EmptyMap$.prototype.$classData = ScalaJS.d.sci_Map$EmptyMap$;
ScalaJS.n.sci_Map$EmptyMap$ = (void 0);
ScalaJS.m.sci_Map$EmptyMap$ = (function() {
  if ((!ScalaJS.n.sci_Map$EmptyMap$)) {
    ScalaJS.n.sci_Map$EmptyMap$ = new ScalaJS.c.sci_Map$EmptyMap$().init___()
  };
  return ScalaJS.n.sci_Map$EmptyMap$
});
/** @constructor */
ScalaJS.c.sci_Map$Map1 = (function() {
  ScalaJS.c.sci_AbstractMap.call(this);
  this.key1$5 = null;
  this.value1$5 = null
});
ScalaJS.c.sci_Map$Map1.prototype = new ScalaJS.h.sci_AbstractMap();
ScalaJS.c.sci_Map$Map1.prototype.constructor = ScalaJS.c.sci_Map$Map1;
/** @constructor */
ScalaJS.h.sci_Map$Map1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Map$Map1.prototype = ScalaJS.c.sci_Map$Map1.prototype;
ScalaJS.c.sci_Map$Map1.prototype.init___O__O = (function(key1, value1) {
  this.key1$5 = key1;
  this.value1$5 = value1;
  return this
});
ScalaJS.c.sci_Map$Map1.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5))
});
ScalaJS.c.sci_Map$Map1.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5)]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Map$Map1.prototype.size__I = (function() {
  return 1
});
ScalaJS.c.sci_Map$Map1.prototype.updated__O__O__sci_Map = (function(key, value) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.sci_Map$Map1().init___O__O(this.key1$5, value) : new ScalaJS.c.sci_Map$Map2().init___O__O__O__O(this.key1$5, this.value1$5, key, value))
});
ScalaJS.c.sci_Map$Map1.prototype.get__O__s_Option = (function(key) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.s_Some().init___O(this.value1$5) : ScalaJS.m.s_None$())
});
ScalaJS.c.sci_Map$Map1.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  return this.updated__O__O__sci_Map(kv.$$und1__O(), kv.$$und2__O())
});
ScalaJS.d.sci_Map$Map1 = new ScalaJS.ClassTypeData({
  sci_Map$Map1: 0
}, false, "scala.collection.immutable.Map$Map1", {
  sci_Map$Map1: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Map$Map1.prototype.$classData = ScalaJS.d.sci_Map$Map1;
/** @constructor */
ScalaJS.c.sci_Map$Map2 = (function() {
  ScalaJS.c.sci_AbstractMap.call(this);
  this.key1$5 = null;
  this.value1$5 = null;
  this.key2$5 = null;
  this.value2$5 = null
});
ScalaJS.c.sci_Map$Map2.prototype = new ScalaJS.h.sci_AbstractMap();
ScalaJS.c.sci_Map$Map2.prototype.constructor = ScalaJS.c.sci_Map$Map2;
/** @constructor */
ScalaJS.h.sci_Map$Map2 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Map$Map2.prototype = ScalaJS.c.sci_Map$Map2.prototype;
ScalaJS.c.sci_Map$Map2.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5));
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key2$5, this.value2$5))
});
ScalaJS.c.sci_Map$Map2.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5), new ScalaJS.c.T2().init___O__O(this.key2$5, this.value2$5)]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Map$Map2.prototype.size__I = (function() {
  return 2
});
ScalaJS.c.sci_Map$Map2.prototype.updated__O__O__sci_Map = (function(key, value) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.sci_Map$Map2().init___O__O__O__O(this.key1$5, value, this.key2$5, this.value2$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key2$5) ? new ScalaJS.c.sci_Map$Map2().init___O__O__O__O(this.key1$5, this.value1$5, this.key2$5, value) : new ScalaJS.c.sci_Map$Map3().init___O__O__O__O__O__O(this.key1$5, this.value1$5, this.key2$5, this.value2$5, key, value)))
});
ScalaJS.c.sci_Map$Map2.prototype.get__O__s_Option = (function(key) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.s_Some().init___O(this.value1$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key2$5) ? new ScalaJS.c.s_Some().init___O(this.value2$5) : ScalaJS.m.s_None$()))
});
ScalaJS.c.sci_Map$Map2.prototype.init___O__O__O__O = (function(key1, value1, key2, value2) {
  this.key1$5 = key1;
  this.value1$5 = value1;
  this.key2$5 = key2;
  this.value2$5 = value2;
  return this
});
ScalaJS.c.sci_Map$Map2.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  return this.updated__O__O__sci_Map(kv.$$und1__O(), kv.$$und2__O())
});
ScalaJS.d.sci_Map$Map2 = new ScalaJS.ClassTypeData({
  sci_Map$Map2: 0
}, false, "scala.collection.immutable.Map$Map2", {
  sci_Map$Map2: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Map$Map2.prototype.$classData = ScalaJS.d.sci_Map$Map2;
/** @constructor */
ScalaJS.c.sci_Map$Map3 = (function() {
  ScalaJS.c.sci_AbstractMap.call(this);
  this.key1$5 = null;
  this.value1$5 = null;
  this.key2$5 = null;
  this.value2$5 = null;
  this.key3$5 = null;
  this.value3$5 = null
});
ScalaJS.c.sci_Map$Map3.prototype = new ScalaJS.h.sci_AbstractMap();
ScalaJS.c.sci_Map$Map3.prototype.constructor = ScalaJS.c.sci_Map$Map3;
/** @constructor */
ScalaJS.h.sci_Map$Map3 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Map$Map3.prototype = ScalaJS.c.sci_Map$Map3.prototype;
ScalaJS.c.sci_Map$Map3.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5));
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key2$5, this.value2$5));
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key3$5, this.value3$5))
});
ScalaJS.c.sci_Map$Map3.prototype.init___O__O__O__O__O__O = (function(key1, value1, key2, value2, key3, value3) {
  this.key1$5 = key1;
  this.value1$5 = value1;
  this.key2$5 = key2;
  this.value2$5 = value2;
  this.key3$5 = key3;
  this.value3$5 = value3;
  return this
});
ScalaJS.c.sci_Map$Map3.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5), new ScalaJS.c.T2().init___O__O(this.key2$5, this.value2$5), new ScalaJS.c.T2().init___O__O(this.key3$5, this.value3$5)]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Map$Map3.prototype.size__I = (function() {
  return 3
});
ScalaJS.c.sci_Map$Map3.prototype.updated__O__O__sci_Map = (function(key, value) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.sci_Map$Map3().init___O__O__O__O__O__O(this.key1$5, value, this.key2$5, this.value2$5, this.key3$5, this.value3$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key2$5) ? new ScalaJS.c.sci_Map$Map3().init___O__O__O__O__O__O(this.key1$5, this.value1$5, this.key2$5, value, this.key3$5, this.value3$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key3$5) ? new ScalaJS.c.sci_Map$Map3().init___O__O__O__O__O__O(this.key1$5, this.value1$5, this.key2$5, this.value2$5, this.key3$5, value) : new ScalaJS.c.sci_Map$Map4().init___O__O__O__O__O__O__O__O(this.key1$5, this.value1$5, this.key2$5, this.value2$5, this.key3$5, this.value3$5, key, value))))
});
ScalaJS.c.sci_Map$Map3.prototype.get__O__s_Option = (function(key) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.s_Some().init___O(this.value1$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key2$5) ? new ScalaJS.c.s_Some().init___O(this.value2$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key3$5) ? new ScalaJS.c.s_Some().init___O(this.value3$5) : ScalaJS.m.s_None$())))
});
ScalaJS.c.sci_Map$Map3.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  return this.updated__O__O__sci_Map(kv.$$und1__O(), kv.$$und2__O())
});
ScalaJS.d.sci_Map$Map3 = new ScalaJS.ClassTypeData({
  sci_Map$Map3: 0
}, false, "scala.collection.immutable.Map$Map3", {
  sci_Map$Map3: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Map$Map3.prototype.$classData = ScalaJS.d.sci_Map$Map3;
/** @constructor */
ScalaJS.c.sci_Map$Map4 = (function() {
  ScalaJS.c.sci_AbstractMap.call(this);
  this.key1$5 = null;
  this.value1$5 = null;
  this.key2$5 = null;
  this.value2$5 = null;
  this.key3$5 = null;
  this.value3$5 = null;
  this.key4$5 = null;
  this.value4$5 = null
});
ScalaJS.c.sci_Map$Map4.prototype = new ScalaJS.h.sci_AbstractMap();
ScalaJS.c.sci_Map$Map4.prototype.constructor = ScalaJS.c.sci_Map$Map4;
/** @constructor */
ScalaJS.h.sci_Map$Map4 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Map$Map4.prototype = ScalaJS.c.sci_Map$Map4.prototype;
ScalaJS.c.sci_Map$Map4.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5));
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key2$5, this.value2$5));
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key3$5, this.value3$5));
  f.apply__O__O(new ScalaJS.c.T2().init___O__O(this.key4$5, this.value4$5))
});
ScalaJS.c.sci_Map$Map4.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5), new ScalaJS.c.T2().init___O__O(this.key2$5, this.value2$5), new ScalaJS.c.T2().init___O__O(this.key3$5, this.value3$5), new ScalaJS.c.T2().init___O__O(this.key4$5, this.value4$5)]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_Map$Map4.prototype.size__I = (function() {
  return 4
});
ScalaJS.c.sci_Map$Map4.prototype.init___O__O__O__O__O__O__O__O = (function(key1, value1, key2, value2, key3, value3, key4, value4) {
  this.key1$5 = key1;
  this.value1$5 = value1;
  this.key2$5 = key2;
  this.value2$5 = value2;
  this.key3$5 = key3;
  this.value3$5 = value3;
  this.key4$5 = key4;
  this.value4$5 = value4;
  return this
});
ScalaJS.c.sci_Map$Map4.prototype.updated__O__O__sci_Map = (function(key, value) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.sci_Map$Map4().init___O__O__O__O__O__O__O__O(this.key1$5, value, this.key2$5, this.value2$5, this.key3$5, this.value3$5, this.key4$5, this.value4$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key2$5) ? new ScalaJS.c.sci_Map$Map4().init___O__O__O__O__O__O__O__O(this.key1$5, this.value1$5, this.key2$5, value, this.key3$5, this.value3$5, this.key4$5, this.value4$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key3$5) ? new ScalaJS.c.sci_Map$Map4().init___O__O__O__O__O__O__O__O(this.key1$5, this.value1$5, this.key2$5, this.value2$5, this.key3$5, value, this.key4$5, this.value4$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key4$5) ? new ScalaJS.c.sci_Map$Map4().init___O__O__O__O__O__O__O__O(this.key1$5, this.value1$5, this.key2$5, this.value2$5, this.key3$5, this.value3$5, this.key4$5, value) : new ScalaJS.c.sci_HashMap().init___().$$plus__T2__T2__sc_Seq__sci_HashMap(new ScalaJS.c.T2().init___O__O(this.key1$5, this.value1$5), new ScalaJS.c.T2().init___O__O(this.key2$5, this.value2$5), new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([new ScalaJS.c.T2().init___O__O(this.key3$5, this.value3$5), new ScalaJS.c.T2().init___O__O(this.key4$5, this.value4$5), new ScalaJS.c.T2().init___O__O(key, value)]))))))
});
ScalaJS.c.sci_Map$Map4.prototype.get__O__s_Option = (function(key) {
  return (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key1$5) ? new ScalaJS.c.s_Some().init___O(this.value1$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key2$5) ? new ScalaJS.c.s_Some().init___O(this.value2$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key3$5) ? new ScalaJS.c.s_Some().init___O(this.value3$5) : (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key4$5) ? new ScalaJS.c.s_Some().init___O(this.value4$5) : ScalaJS.m.s_None$()))))
});
ScalaJS.c.sci_Map$Map4.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  return this.updated__O__O__sci_Map(kv.$$und1__O(), kv.$$und2__O())
});
ScalaJS.d.sci_Map$Map4 = new ScalaJS.ClassTypeData({
  sci_Map$Map4: 0
}, false, "scala.collection.immutable.Map$Map4", {
  sci_Map$Map4: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Map$Map4.prototype.$classData = ScalaJS.d.sci_Map$Map4;
ScalaJS.is.scm_Set = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_Set)))
});
ScalaJS.as.scm_Set = (function(obj) {
  return ((ScalaJS.is.scm_Set(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.Set"))
});
ScalaJS.isArrayOf.scm_Set = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_Set)))
});
ScalaJS.asArrayOf.scm_Set = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_Set(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.Set;", depth))
});
/** @constructor */
ScalaJS.c.sci_HashMap = (function() {
  ScalaJS.c.sci_AbstractMap.call(this)
});
ScalaJS.c.sci_HashMap.prototype = new ScalaJS.h.sci_AbstractMap();
ScalaJS.c.sci_HashMap.prototype.constructor = ScalaJS.c.sci_HashMap;
/** @constructor */
ScalaJS.h.sci_HashMap = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap.prototype = ScalaJS.c.sci_HashMap.prototype;
ScalaJS.c.sci_HashMap.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_HashMap.prototype.computeHash__O__I = (function(key) {
  return this.improve__I__I(ScalaJS.m.sr_ScalaRunTime$().hash__O__I(key))
});
ScalaJS.c.sci_HashMap.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sci_HashMap.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_HashMap.prototype.updated0__O__I__I__O__T2__sci_HashMap$Merger__sci_HashMap = (function(key, hash, level, value, kv, merger) {
  return new ScalaJS.c.sci_HashMap$HashMap1().init___O__I__O__T2(key, hash, value, kv)
});
ScalaJS.c.sci_HashMap.prototype.get0__O__I__I__s_Option = (function(key, hash, level) {
  return ScalaJS.m.s_None$()
});
ScalaJS.c.sci_HashMap.prototype.foreach__F1__V = (function(f) {
  /*<skip>*/
});
ScalaJS.c.sci_HashMap.prototype.$$plus__T2__sci_HashMap = (function(kv) {
  return this.updated0__O__I__I__O__T2__sci_HashMap$Merger__sci_HashMap(kv.$$und1__O(), this.computeHash__O__I(kv.$$und1__O()), 0, kv.$$und2__O(), kv, null)
});
ScalaJS.c.sci_HashMap.prototype.empty__sc_Map = (function() {
  ScalaJS.m.sci_HashMap$();
  return ScalaJS.m.sci_HashMap$EmptyHashMap$()
});
ScalaJS.c.sci_HashMap.prototype.empty__sci_Map = (function() {
  ScalaJS.m.sci_HashMap$();
  return ScalaJS.m.sci_HashMap$EmptyHashMap$()
});
ScalaJS.c.sci_HashMap.prototype.seq__sc_Map = (function() {
  return this
});
ScalaJS.c.sci_HashMap.prototype.size__I = (function() {
  return 0
});
ScalaJS.c.sci_HashMap.prototype.iterator__sc_Iterator = (function() {
  return ScalaJS.m.sc_Iterator$().empty$1
});
ScalaJS.c.sci_HashMap.prototype.get__O__s_Option = (function(key) {
  return this.get0__O__I__I__s_Option(key, this.computeHash__O__I(key), 0)
});
ScalaJS.c.sci_HashMap.prototype.improve__I__I = (function(hcode) {
  var h = ((hcode + (~(hcode << 9))) | 0);
  h = (h ^ ((h >>> 14) | 0));
  h = ((h + (h << 4)) | 0);
  return (h ^ ((h >>> 10) | 0))
});
ScalaJS.c.sci_HashMap.prototype.$$plus__T2__T2__sc_Seq__sci_HashMap = (function(elem1, elem2, elems) {
  var this$2 = this.$$plus__T2__sci_HashMap(elem1).$$plus__T2__sci_HashMap(elem2);
  var this$1 = ScalaJS.m.sci_HashMap$();
  var bf = new ScalaJS.c.scg_GenMapFactory$MapCanBuildFrom().init___scg_GenMapFactory(this$1);
  return ScalaJS.as.sci_HashMap(ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O(this$2, elems, bf))
});
ScalaJS.c.sci_HashMap.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  return this.$$plus__T2__sci_HashMap(kv)
});
ScalaJS.is.sci_HashMap = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_HashMap)))
});
ScalaJS.as.sci_HashMap = (function(obj) {
  return ((ScalaJS.is.sci_HashMap(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.HashMap"))
});
ScalaJS.isArrayOf.sci_HashMap = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_HashMap)))
});
ScalaJS.asArrayOf.sci_HashMap = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_HashMap(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.HashMap;", depth))
});
ScalaJS.d.sci_HashMap = new ScalaJS.ClassTypeData({
  sci_HashMap: 0
}, false, "scala.collection.immutable.HashMap", {
  sci_HashMap: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  sc_CustomParallelizable: 1
});
ScalaJS.c.sci_HashMap.prototype.$classData = ScalaJS.d.sci_HashMap;
/** @constructor */
ScalaJS.c.sci_HashSet$HashSet1 = (function() {
  ScalaJS.c.sci_HashSet$LeafHashSet.call(this);
  this.key$6 = null;
  this.hash$6 = 0
});
ScalaJS.c.sci_HashSet$HashSet1.prototype = new ScalaJS.h.sci_HashSet$LeafHashSet();
ScalaJS.c.sci_HashSet$HashSet1.prototype.constructor = ScalaJS.c.sci_HashSet$HashSet1;
/** @constructor */
ScalaJS.h.sci_HashSet$HashSet1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet$HashSet1.prototype = ScalaJS.c.sci_HashSet$HashSet1.prototype;
ScalaJS.c.sci_HashSet$HashSet1.prototype.updated0__O__I__I__sci_HashSet = (function(key, hash, level) {
  if (((hash === this.hash$6) && ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key$6))) {
    return this
  } else if ((hash !== this.hash$6)) {
    return ScalaJS.m.sci_HashSet$().scala$collection$immutable$HashSet$$makeHashTrieSet__I__sci_HashSet__I__sci_HashSet__I__sci_HashSet$HashTrieSet(this.hash$6, this, hash, new ScalaJS.c.sci_HashSet$HashSet1().init___O__I(key, hash), level)
  } else {
    var this$2 = ScalaJS.m.sci_ListSet$EmptyListSet$();
    var elem = this.key$6;
    return new ScalaJS.c.sci_HashSet$HashSetCollision1().init___I__sci_ListSet(hash, new ScalaJS.c.sci_ListSet$Node().init___sci_ListSet__O(this$2, elem).$$plus__O__sci_ListSet(key))
  }
});
ScalaJS.c.sci_HashSet$HashSet1.prototype.init___O__I = (function(key, hash) {
  this.key$6 = key;
  this.hash$6 = hash;
  return this
});
ScalaJS.c.sci_HashSet$HashSet1.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(this.key$6)
});
ScalaJS.c.sci_HashSet$HashSet1.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([this.key$6]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_HashSet$HashSet1.prototype.size__I = (function() {
  return 1
});
ScalaJS.c.sci_HashSet$HashSet1.prototype.get0__O__I__I__Z = (function(key, hash, level) {
  return ((hash === this.hash$6) && ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key$6))
});
ScalaJS.c.sci_HashSet$HashSet1.prototype.subsetOf0__sci_HashSet__I__Z = (function(that, level) {
  return that.get0__O__I__I__Z(this.key$6, this.hash$6, level)
});
ScalaJS.is.sci_HashSet$HashSet1 = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_HashSet$HashSet1)))
});
ScalaJS.as.sci_HashSet$HashSet1 = (function(obj) {
  return ((ScalaJS.is.sci_HashSet$HashSet1(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.HashSet$HashSet1"))
});
ScalaJS.isArrayOf.sci_HashSet$HashSet1 = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_HashSet$HashSet1)))
});
ScalaJS.asArrayOf.sci_HashSet$HashSet1 = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_HashSet$HashSet1(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.HashSet$HashSet1;", depth))
});
ScalaJS.d.sci_HashSet$HashSet1 = new ScalaJS.ClassTypeData({
  sci_HashSet$HashSet1: 0
}, false, "scala.collection.immutable.HashSet$HashSet1", {
  sci_HashSet$HashSet1: 1,
  sci_HashSet$LeafHashSet: 1,
  sci_HashSet: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_HashSet$HashSet1.prototype.$classData = ScalaJS.d.sci_HashSet$HashSet1;
/** @constructor */
ScalaJS.c.sci_HashSet$HashSetCollision1 = (function() {
  ScalaJS.c.sci_HashSet$LeafHashSet.call(this);
  this.hash$6 = 0;
  this.ks$6 = null
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype = new ScalaJS.h.sci_HashSet$LeafHashSet();
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.constructor = ScalaJS.c.sci_HashSet$HashSetCollision1;
/** @constructor */
ScalaJS.h.sci_HashSet$HashSetCollision1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashSet$HashSetCollision1.prototype = ScalaJS.c.sci_HashSet$HashSetCollision1.prototype;
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.updated0__O__I__I__sci_HashSet = (function(key, hash, level) {
  return ((hash === this.hash$6) ? new ScalaJS.c.sci_HashSet$HashSetCollision1().init___I__sci_ListSet(hash, this.ks$6.$$plus__O__sci_ListSet(key)) : ScalaJS.m.sci_HashSet$().scala$collection$immutable$HashSet$$makeHashTrieSet__I__sci_HashSet__I__sci_HashSet__I__sci_HashSet$HashTrieSet(this.hash$6, this, hash, new ScalaJS.c.sci_HashSet$HashSet1().init___O__I(key, hash), level))
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.foreach__F1__V = (function(f) {
  var this$1 = this.ks$6;
  var this$2 = new ScalaJS.c.sci_ListSet$$anon$1().init___sci_ListSet(this$1);
  ScalaJS.s.sc_Iterator$class__foreach__sc_Iterator__F1__V(this$2, f)
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.iterator__sc_Iterator = (function() {
  var this$1 = this.ks$6;
  return new ScalaJS.c.sci_ListSet$$anon$1().init___sci_ListSet(this$1)
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.size__I = (function() {
  return this.ks$6.size__I()
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.init___I__sci_ListSet = (function(hash, ks) {
  this.hash$6 = hash;
  this.ks$6 = ks;
  return this
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.get0__O__I__I__Z = (function(key, hash, level) {
  return ((hash === this.hash$6) && this.ks$6.contains__O__Z(key))
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.subsetOf0__sci_HashSet__I__Z = (function(that, level) {
  var this$1 = this.ks$6;
  var this$2 = new ScalaJS.c.sci_ListSet$$anon$1().init___sci_ListSet(this$1);
  var res = true;
  while (true) {
    if (res) {
      var this$3 = this$2.that$2;
      var jsx$1 = ScalaJS.s.sc_TraversableOnce$class__nonEmpty__sc_TraversableOnce__Z(this$3)
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      var arg1 = this$2.next__O();
      res = that.get0__O__I__I__Z(arg1, this.hash$6, level)
    } else {
      break
    }
  };
  return res
});
ScalaJS.d.sci_HashSet$HashSetCollision1 = new ScalaJS.ClassTypeData({
  sci_HashSet$HashSetCollision1: 0
}, false, "scala.collection.immutable.HashSet$HashSetCollision1", {
  sci_HashSet$HashSetCollision1: 1,
  sci_HashSet$LeafHashSet: 1,
  sci_HashSet: 1,
  sc_AbstractSet: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  sci_Set: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_HashSet$HashSetCollision1.prototype.$classData = ScalaJS.d.sci_HashSet$HashSetCollision1;
/** @constructor */
ScalaJS.c.sci_List = (function() {
  ScalaJS.c.sc_AbstractSeq.call(this)
});
ScalaJS.c.sci_List.prototype = new ScalaJS.h.sc_AbstractSeq();
ScalaJS.c.sci_List.prototype.constructor = ScalaJS.c.sci_List;
/** @constructor */
ScalaJS.h.sci_List = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_List.prototype = ScalaJS.c.sci_List.prototype;
ScalaJS.c.sci_List.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_List.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sci_List.prototype.apply__I__O = (function(n) {
  return ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this, n)
});
ScalaJS.c.sci_List.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_LinearSeqOptimized$class__lengthCompare__sc_LinearSeqOptimized__I__I(this, len)
});
ScalaJS.c.sci_List.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_LinearSeqOptimized$class__sameElements__sc_LinearSeqOptimized__sc_GenIterable__Z(this, that)
});
ScalaJS.c.sci_List.prototype.apply__O__O = (function(v1) {
  var n = ScalaJS.uI(v1);
  return ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this, n)
});
ScalaJS.c.sci_List.prototype.toList__sci_List = (function() {
  return this
});
ScalaJS.c.sci_List.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_List.prototype.drop__I__sc_LinearSeqOptimized = (function(n) {
  return this.drop__I__sci_List(n)
});
ScalaJS.c.sci_List.prototype.take__I__sci_List = (function(n) {
  if ((this.isEmpty__Z() || (n <= 0))) {
    return ScalaJS.m.sci_Nil$()
  } else {
    var h = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(this.head__O(), ScalaJS.m.sci_Nil$());
    var t = h;
    var rest = ScalaJS.as.sci_List(this.tail__O());
    var i = 1;
    while (true) {
      if (rest.isEmpty__Z()) {
        return this
      };
      if ((i < n)) {
        i = ((1 + i) | 0);
        var nx = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(rest.head__O(), ScalaJS.m.sci_Nil$());
        t.tl$5 = nx;
        t = nx;
        rest = ScalaJS.as.sci_List(rest.tail__O())
      } else {
        break
      }
    };
    return h
  }
});
ScalaJS.c.sci_List.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_List$()
});
ScalaJS.c.sci_List.prototype.foreach__F1__V = (function(f) {
  var these = this;
  while ((!these.isEmpty__Z())) {
    f.apply__O__O(these.head__O());
    these = ScalaJS.as.sci_List(these.tail__O())
  }
});
ScalaJS.c.sci_List.prototype.$$colon$colon$colon__sci_List__sci_List = (function(prefix) {
  return (this.isEmpty__Z() ? prefix : (prefix.isEmpty__Z() ? this : new ScalaJS.c.scm_ListBuffer().init___().$$plus$plus$eq__sc_TraversableOnce__scm_ListBuffer(prefix).prependToList__sci_List__sci_List(this)))
});
ScalaJS.c.sci_List.prototype.reverse__O = (function() {
  return this.reverse__sci_List()
});
ScalaJS.c.sci_List.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.sc_LinearSeqLike$$anon$1().init___sc_LinearSeqLike(this)
});
ScalaJS.c.sci_List.prototype.drop__I__sci_List = (function(n) {
  var these = this;
  var count = n;
  while (((!these.isEmpty__Z()) && (count > 0))) {
    these = ScalaJS.as.sci_List(these.tail__O());
    count = (((-1) + count) | 0)
  };
  return these
});
ScalaJS.c.sci_List.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_List.prototype.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O = (function(that, bf) {
  return ((bf === ScalaJS.m.sci_List$().ReusableCBFInstance$2) ? that.seq__sc_TraversableOnce().toList__sci_List().$$colon$colon$colon__sci_List__sci_List(this) : ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O(this, that, bf))
});
ScalaJS.c.sci_List.prototype.length__I = (function() {
  return ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I(this)
});
ScalaJS.c.sci_List.prototype.take__I__O = (function(n) {
  return this.take__I__sci_List(n)
});
ScalaJS.c.sci_List.prototype.toStream__sci_Stream = (function() {
  return (this.isEmpty__Z() ? ScalaJS.m.sci_Stream$Empty$() : new ScalaJS.c.sci_Stream$Cons().init___O__F0(this.head__O(), new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2) {
    return (function() {
      return ScalaJS.as.sci_List(this$2.tail__O()).toStream__sci_Stream()
    })
  })(this))))
});
ScalaJS.c.sci_List.prototype.drop__I__O = (function(n) {
  return this.drop__I__sci_List(n)
});
ScalaJS.c.sci_List.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_List.prototype.isDefinedAt__O__Z = (function(x) {
  var x$1 = ScalaJS.uI(x);
  return ScalaJS.s.sc_LinearSeqOptimized$class__isDefinedAt__sc_LinearSeqOptimized__I__Z(this, x$1)
});
ScalaJS.c.sci_List.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
ScalaJS.c.sci_List.prototype.toCollection__O__sc_Seq = (function(repr) {
  var repr$1 = ScalaJS.as.sc_LinearSeqLike(repr);
  return ScalaJS.as.sc_LinearSeq(repr$1)
});
ScalaJS.c.sci_List.prototype.reverse__sci_List = (function() {
  var result = ScalaJS.m.sci_Nil$();
  var these = this;
  while ((!these.isEmpty__Z())) {
    var x$4 = these.head__O();
    var this$1 = result;
    result = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(x$4, this$1);
    these = ScalaJS.as.sci_List(these.tail__O())
  };
  return result
});
ScalaJS.c.sci_List.prototype.stringPrefix__T = (function() {
  return "List"
});
ScalaJS.is.sci_List = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_List)))
});
ScalaJS.as.sci_List = (function(obj) {
  return ((ScalaJS.is.sci_List(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.List"))
});
ScalaJS.isArrayOf.sci_List = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_List)))
});
ScalaJS.asArrayOf.sci_List = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_List(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.List;", depth))
});
/** @constructor */
ScalaJS.c.sci_ListMap$EmptyListMap$ = (function() {
  ScalaJS.c.sci_ListMap.call(this)
});
ScalaJS.c.sci_ListMap$EmptyListMap$.prototype = new ScalaJS.h.sci_ListMap();
ScalaJS.c.sci_ListMap$EmptyListMap$.prototype.constructor = ScalaJS.c.sci_ListMap$EmptyListMap$;
/** @constructor */
ScalaJS.h.sci_ListMap$EmptyListMap$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListMap$EmptyListMap$.prototype = ScalaJS.c.sci_ListMap$EmptyListMap$.prototype;
ScalaJS.d.sci_ListMap$EmptyListMap$ = new ScalaJS.ClassTypeData({
  sci_ListMap$EmptyListMap$: 0
}, false, "scala.collection.immutable.ListMap$EmptyListMap$", {
  sci_ListMap$EmptyListMap$: 1,
  sci_ListMap: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_ListMap$EmptyListMap$.prototype.$classData = ScalaJS.d.sci_ListMap$EmptyListMap$;
ScalaJS.n.sci_ListMap$EmptyListMap$ = (void 0);
ScalaJS.m.sci_ListMap$EmptyListMap$ = (function() {
  if ((!ScalaJS.n.sci_ListMap$EmptyListMap$)) {
    ScalaJS.n.sci_ListMap$EmptyListMap$ = new ScalaJS.c.sci_ListMap$EmptyListMap$().init___()
  };
  return ScalaJS.n.sci_ListMap$EmptyListMap$
});
/** @constructor */
ScalaJS.c.sci_ListMap$Node = (function() {
  ScalaJS.c.sci_ListMap.call(this);
  this.key$6 = null;
  this.value$6 = null;
  this.$$outer$f = null
});
ScalaJS.c.sci_ListMap$Node.prototype = new ScalaJS.h.sci_ListMap();
ScalaJS.c.sci_ListMap$Node.prototype.constructor = ScalaJS.c.sci_ListMap$Node;
/** @constructor */
ScalaJS.h.sci_ListMap$Node = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_ListMap$Node.prototype = ScalaJS.c.sci_ListMap$Node.prototype;
ScalaJS.c.sci_ListMap$Node.prototype.value__O = (function() {
  return this.value$6
});
ScalaJS.c.sci_ListMap$Node.prototype.apply__O__O = (function(k) {
  return this.apply0__p6__sci_ListMap__O__O(this, k)
});
ScalaJS.c.sci_ListMap$Node.prototype.isEmpty__Z = (function() {
  return false
});
ScalaJS.c.sci_ListMap$Node.prototype.apply0__p6__sci_ListMap__O__O = (function(cur, k) {
  _apply0: while (true) {
    if (cur.isEmpty__Z()) {
      throw new ScalaJS.c.ju_NoSuchElementException().init___T(("key not found: " + k))
    } else if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(k, cur.key__O())) {
      return cur.value__O()
    } else {
      cur = cur.next__sci_ListMap();
      continue _apply0
    }
  }
});
ScalaJS.c.sci_ListMap$Node.prototype.size0__p6__sci_ListMap__I__I = (function(cur, acc) {
  _size0: while (true) {
    if (cur.isEmpty__Z()) {
      return acc
    } else {
      var temp$cur = cur.next__sci_ListMap();
      var temp$acc = ((1 + acc) | 0);
      cur = temp$cur;
      acc = temp$acc;
      continue _size0
    }
  }
});
ScalaJS.c.sci_ListMap$Node.prototype.size__I = (function() {
  return this.size0__p6__sci_ListMap__I__I(this, 0)
});
ScalaJS.c.sci_ListMap$Node.prototype.key__O = (function() {
  return this.key$6
});
ScalaJS.c.sci_ListMap$Node.prototype.updated__O__O__sci_ListMap = (function(k, v) {
  var m = this.remove0__p6__O__sci_ListMap__sci_List__sci_ListMap(k, this, ScalaJS.m.sci_Nil$());
  return new ScalaJS.c.sci_ListMap$Node().init___sci_ListMap__O__O(m, k, v)
});
ScalaJS.c.sci_ListMap$Node.prototype.get__O__s_Option = (function(k) {
  return this.get0__p6__sci_ListMap__O__s_Option(this, k)
});
ScalaJS.c.sci_ListMap$Node.prototype.get0__p6__sci_ListMap__O__s_Option = (function(cur, k) {
  _get0: while (true) {
    if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(k, cur.key__O())) {
      return new ScalaJS.c.s_Some().init___O(cur.value__O())
    } else {
      var this$1 = cur.next__sci_ListMap();
      if (ScalaJS.s.sc_TraversableOnce$class__nonEmpty__sc_TraversableOnce__Z(this$1)) {
        cur = cur.next__sci_ListMap();
        continue _get0
      } else {
        return ScalaJS.m.s_None$()
      }
    }
  }
});
ScalaJS.c.sci_ListMap$Node.prototype.init___sci_ListMap__O__O = (function($$outer, key, value) {
  this.key$6 = key;
  this.value$6 = value;
  if (($$outer === null)) {
    throw ScalaJS.m.sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$f = $$outer
  };
  return this
});
ScalaJS.c.sci_ListMap$Node.prototype.remove0__p6__O__sci_ListMap__sci_List__sci_ListMap = (function(k, cur, acc) {
  _remove0: while (true) {
    if (cur.isEmpty__Z()) {
      var this$1 = acc;
      return ScalaJS.as.sci_ListMap(ScalaJS.s.sc_LinearSeqOptimized$class__last__sc_LinearSeqOptimized__O(this$1))
    } else if (ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(k, cur.key__O())) {
      var x$4 = cur.next__sci_ListMap();
      var this$2 = acc;
      var acc$1 = x$4;
      var these = this$2;
      while ((!these.isEmpty__Z())) {
        var arg1 = acc$1;
        var arg2 = these.head__O();
        var x0$1 = ScalaJS.as.sci_ListMap(arg1);
        var x1$1 = ScalaJS.as.sci_ListMap(arg2);
        matchEnd3: {
          acc$1 = new ScalaJS.c.sci_ListMap$Node().init___sci_ListMap__O__O(x0$1, x1$1.key__O(), x1$1.value__O());
          break matchEnd3
        };
        these = ScalaJS.as.sc_LinearSeqOptimized(these.tail__O())
      };
      return ScalaJS.as.sci_ListMap(acc$1)
    } else {
      var temp$cur = cur.next__sci_ListMap();
      var x$5 = cur;
      var this$3 = acc;
      var temp$acc = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(x$5, this$3);
      cur = temp$cur;
      acc = temp$acc;
      continue _remove0
    }
  }
});
ScalaJS.c.sci_ListMap$Node.prototype.next__sci_ListMap = (function() {
  return this.$$outer$f
});
ScalaJS.d.sci_ListMap$Node = new ScalaJS.ClassTypeData({
  sci_ListMap$Node: 0
}, false, "scala.collection.immutable.ListMap$Node", {
  sci_ListMap$Node: 1,
  sci_ListMap: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_ListMap$Node.prototype.$classData = ScalaJS.d.sci_ListMap$Node;
/** @constructor */
ScalaJS.c.sci_Stream = (function() {
  ScalaJS.c.sc_AbstractSeq.call(this)
});
ScalaJS.c.sci_Stream.prototype = new ScalaJS.h.sc_AbstractSeq();
ScalaJS.c.sci_Stream.prototype.constructor = ScalaJS.c.sci_Stream;
/** @constructor */
ScalaJS.h.sci_Stream = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream.prototype = ScalaJS.c.sci_Stream.prototype;
ScalaJS.c.sci_Stream.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_Stream.prototype.reverse__sci_Stream = (function() {
  var elem = ScalaJS.m.sci_Stream$Empty$();
  var result = new ScalaJS.c.sr_ObjectRef().init___O(elem);
  var these = this;
  while ((!these.isEmpty__Z())) {
    ScalaJS.m.sci_Stream$();
    var stream = new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2, result$1) {
      return (function() {
        return ScalaJS.as.sci_Stream(result$1.elem$1)
      })
    })(this, result));
    var r = new ScalaJS.c.sci_Stream$ConsWrapper().init___F0(stream).$$hash$colon$colon__O__sci_Stream(these.head__O());
    r.tail__O();
    result.elem$1 = r;
    these = ScalaJS.as.sci_Stream(these.tail__O())
  };
  return ScalaJS.as.sci_Stream(result.elem$1)
});
ScalaJS.c.sci_Stream.prototype.init___ = (function() {
  return this
});
ScalaJS.c.sci_Stream.prototype.apply__I__O = (function(n) {
  return ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this, n)
});
ScalaJS.c.sci_Stream.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_LinearSeqOptimized$class__lengthCompare__sc_LinearSeqOptimized__I__I(this, len)
});
ScalaJS.c.sci_Stream.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_LinearSeqOptimized$class__sameElements__sc_LinearSeqOptimized__sc_GenIterable__Z(this, that)
});
ScalaJS.c.sci_Stream.prototype.apply__O__O = (function(v1) {
  var n = ScalaJS.uI(v1);
  return ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this, n)
});
ScalaJS.c.sci_Stream.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_Stream.prototype.flatMap__F1__scg_CanBuildFrom__O = (function(f, bf) {
  if (ScalaJS.is.sci_Stream$StreamBuilder(bf.apply__O__scm_Builder(this))) {
    if (this.isEmpty__Z()) {
      var x$1 = ScalaJS.m.sci_Stream$Empty$()
    } else {
      var nonEmptyPrefix = new ScalaJS.c.sr_ObjectRef().init___O(this);
      var prefix = ScalaJS.as.sc_GenTraversableOnce(f.apply__O__O(ScalaJS.as.sci_Stream(nonEmptyPrefix.elem$1).head__O())).toStream__sci_Stream();
      while (((!ScalaJS.as.sci_Stream(nonEmptyPrefix.elem$1).isEmpty__Z()) && prefix.isEmpty__Z())) {
        nonEmptyPrefix.elem$1 = ScalaJS.as.sci_Stream(ScalaJS.as.sci_Stream(nonEmptyPrefix.elem$1).tail__O());
        if ((!ScalaJS.as.sci_Stream(nonEmptyPrefix.elem$1).isEmpty__Z())) {
          prefix = ScalaJS.as.sc_GenTraversableOnce(f.apply__O__O(ScalaJS.as.sci_Stream(nonEmptyPrefix.elem$1).head__O())).toStream__sci_Stream()
        }
      };
      var x$1 = (ScalaJS.as.sci_Stream(nonEmptyPrefix.elem$1).isEmpty__Z() ? (ScalaJS.m.sci_Stream$(), ScalaJS.m.sci_Stream$Empty$()) : prefix.append__F0__sci_Stream(new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2$1, f$1, nonEmptyPrefix$1) {
        return (function() {
          var x = ScalaJS.as.sci_Stream(ScalaJS.as.sci_Stream(nonEmptyPrefix$1.elem$1).tail__O()).flatMap__F1__scg_CanBuildFrom__O(f$1, (ScalaJS.m.sci_Stream$(), new ScalaJS.c.sci_Stream$StreamCanBuildFrom().init___()));
          return ScalaJS.as.sci_Stream(x)
        })
      })(this, f, nonEmptyPrefix))))
    };
    return x$1
  } else {
    return ScalaJS.s.sc_TraversableLike$class__flatMap__sc_TraversableLike__F1__scg_CanBuildFrom__O(this, f, bf)
  }
});
ScalaJS.c.sci_Stream.prototype.drop__I__sc_LinearSeqOptimized = (function(n) {
  return this.drop__I__sci_Stream(n)
});
ScalaJS.c.sci_Stream.prototype.mkString__T__T__T__T = (function(start, sep, end) {
  this.force__sci_Stream();
  return ScalaJS.s.sc_TraversableOnce$class__mkString__sc_TraversableOnce__T__T__T__T(this, start, sep, end)
});
ScalaJS.c.sci_Stream.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Stream$()
});
ScalaJS.c.sci_Stream.prototype.toString__T = (function() {
  return ScalaJS.s.sc_TraversableOnce$class__mkString__sc_TraversableOnce__T__T__T__T(this, ("Stream" + "("), ", ", ")")
});
ScalaJS.c.sci_Stream.prototype.foreach__F1__V = (function(f) {
  var _$this = this;
  x: {
    _foreach: while (true) {
      if ((!_$this.isEmpty__Z())) {
        f.apply__O__O(_$this.head__O());
        _$this = ScalaJS.as.sci_Stream(_$this.tail__O());
        continue _foreach
      };
      break x
    }
  }
});
ScalaJS.c.sci_Stream.prototype.reverse__O = (function() {
  return this.reverse__sci_Stream()
});
ScalaJS.c.sci_Stream.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.sci_StreamIterator().init___sci_Stream(this)
});
ScalaJS.c.sci_Stream.prototype.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O = (function(that, bf) {
  if (ScalaJS.is.sci_Stream$StreamBuilder(bf.apply__O__scm_Builder(this))) {
    if (this.isEmpty__Z()) {
      var x$1 = that.toStream__sci_Stream()
    } else {
      var hd = this.head__O();
      var tl = new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2, that$1) {
        return (function() {
          var x = ScalaJS.as.sci_Stream(this$2.tail__O()).$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O(that$1, (ScalaJS.m.sci_Stream$(), new ScalaJS.c.sci_Stream$StreamCanBuildFrom().init___()));
          return ScalaJS.as.sci_Stream(x)
        })
      })(this, that));
      var x$1 = new ScalaJS.c.sci_Stream$Cons().init___O__F0(hd, tl)
    };
    return x$1
  } else {
    return ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O(this, that, bf)
  }
});
ScalaJS.c.sci_Stream.prototype.length__I = (function() {
  var len = 0;
  var left = this;
  while ((!left.isEmpty__Z())) {
    len = ((1 + len) | 0);
    left = ScalaJS.as.sci_Stream(left.tail__O())
  };
  return len
});
ScalaJS.c.sci_Stream.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_Stream.prototype.take__I__O = (function(n) {
  return this.take__I__sci_Stream(n)
});
ScalaJS.c.sci_Stream.prototype.toStream__sci_Stream = (function() {
  return this
});
ScalaJS.c.sci_Stream.prototype.drop__I__O = (function(n) {
  return this.drop__I__sci_Stream(n)
});
ScalaJS.c.sci_Stream.prototype.drop__I__sci_Stream = (function(n) {
  var _$this = this;
  _drop: while (true) {
    if (((n <= 0) || _$this.isEmpty__Z())) {
      return _$this
    } else {
      var temp$_$this = ScalaJS.as.sci_Stream(_$this.tail__O());
      var temp$n = (((-1) + n) | 0);
      _$this = temp$_$this;
      n = temp$n;
      continue _drop
    }
  }
});
ScalaJS.c.sci_Stream.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_Stream.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  b.append__T__scm_StringBuilder(start);
  if ((!this.isEmpty__Z())) {
    b.append__O__scm_StringBuilder(this.head__O());
    var cursor = this;
    var n = 1;
    if (cursor.tailDefined__Z()) {
      var scout = ScalaJS.as.sci_Stream(this.tail__O());
      if (scout.isEmpty__Z()) {
        b.append__T__scm_StringBuilder(end);
        return b
      };
      if (((cursor !== scout) && scout.tailDefined__Z())) {
        cursor = scout;
        scout = ScalaJS.as.sci_Stream(scout.tail__O());
        while (((cursor !== scout) && scout.tailDefined__Z())) {
          b.append__T__scm_StringBuilder(sep).append__O__scm_StringBuilder(cursor.head__O());
          n = ((1 + n) | 0);
          cursor = ScalaJS.as.sci_Stream(cursor.tail__O());
          scout = ScalaJS.as.sci_Stream(scout.tail__O());
          if (scout.tailDefined__Z()) {
            scout = ScalaJS.as.sci_Stream(scout.tail__O())
          }
        }
      };
      if ((!scout.tailDefined__Z())) {
        while ((cursor !== scout)) {
          b.append__T__scm_StringBuilder(sep).append__O__scm_StringBuilder(cursor.head__O());
          n = ((1 + n) | 0);
          cursor = ScalaJS.as.sci_Stream(cursor.tail__O())
        }
      } else {
        var runner = this;
        var k = 0;
        while ((runner !== scout)) {
          runner = ScalaJS.as.sci_Stream(runner.tail__O());
          scout = ScalaJS.as.sci_Stream(scout.tail__O());
          k = ((1 + k) | 0)
        };
        if (((cursor === scout) && (k > 0))) {
          b.append__T__scm_StringBuilder(sep).append__O__scm_StringBuilder(cursor.head__O());
          n = ((1 + n) | 0);
          cursor = ScalaJS.as.sci_Stream(cursor.tail__O())
        };
        while ((cursor !== scout)) {
          b.append__T__scm_StringBuilder(sep).append__O__scm_StringBuilder(cursor.head__O());
          n = ((1 + n) | 0);
          cursor = ScalaJS.as.sci_Stream(cursor.tail__O())
        };
        n = ((n - k) | 0)
      }
    };
    if ((!cursor.isEmpty__Z())) {
      if ((!cursor.tailDefined__Z())) {
        b.append__T__scm_StringBuilder(sep).append__T__scm_StringBuilder("?")
      } else {
        b.append__T__scm_StringBuilder(sep).append__T__scm_StringBuilder("...")
      }
    }
  };
  b.append__T__scm_StringBuilder(end);
  return b
});
ScalaJS.c.sci_Stream.prototype.force__sci_Stream = (function() {
  var these = this;
  var those = this;
  if ((!these.isEmpty__Z())) {
    these = ScalaJS.as.sci_Stream(these.tail__O())
  };
  while ((those !== these)) {
    if (these.isEmpty__Z()) {
      return this
    };
    these = ScalaJS.as.sci_Stream(these.tail__O());
    if (these.isEmpty__Z()) {
      return this
    };
    these = ScalaJS.as.sci_Stream(these.tail__O());
    if ((these === those)) {
      return this
    };
    those = ScalaJS.as.sci_Stream(those.tail__O())
  };
  return this
});
ScalaJS.c.sci_Stream.prototype.isDefinedAt__O__Z = (function(x) {
  var x$1 = ScalaJS.uI(x);
  return ScalaJS.s.sc_LinearSeqOptimized$class__isDefinedAt__sc_LinearSeqOptimized__I__Z(this, x$1)
});
ScalaJS.c.sci_Stream.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
ScalaJS.c.sci_Stream.prototype.take__I__sci_Stream = (function(n) {
  if (((n <= 0) || this.isEmpty__Z())) {
    ScalaJS.m.sci_Stream$();
    return ScalaJS.m.sci_Stream$Empty$()
  } else if ((n === 1)) {
    var hd = this.head__O();
    var tl = new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2) {
      return (function() {
        ScalaJS.m.sci_Stream$();
        return ScalaJS.m.sci_Stream$Empty$()
      })
    })(this));
    return new ScalaJS.c.sci_Stream$Cons().init___O__F0(hd, tl)
  } else {
    var hd$1 = this.head__O();
    var tl$1 = new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$3$1, n$1) {
      return (function() {
        return ScalaJS.as.sci_Stream(this$3$1.tail__O()).take__I__sci_Stream((((-1) + n$1) | 0))
      })
    })(this, n));
    return new ScalaJS.c.sci_Stream$Cons().init___O__F0(hd$1, tl$1)
  }
});
ScalaJS.c.sci_Stream.prototype.toCollection__O__sc_Seq = (function(repr) {
  var repr$1 = ScalaJS.as.sc_LinearSeqLike(repr);
  return ScalaJS.as.sc_LinearSeq(repr$1)
});
ScalaJS.c.sci_Stream.prototype.append__F0__sci_Stream = (function(rest) {
  if (this.isEmpty__Z()) {
    return ScalaJS.as.sc_GenTraversableOnce(rest.apply__O()).toStream__sci_Stream()
  } else {
    var hd = this.head__O();
    var tl = new ScalaJS.c.sjsr_AnonFunction0().init___sjs_js_Function0((function(this$2, rest$1) {
      return (function() {
        return ScalaJS.as.sci_Stream(this$2.tail__O()).append__F0__sci_Stream(rest$1)
      })
    })(this, rest));
    return new ScalaJS.c.sci_Stream$Cons().init___O__F0(hd, tl)
  }
});
ScalaJS.c.sci_Stream.prototype.stringPrefix__T = (function() {
  return "Stream"
});
ScalaJS.is.sci_Stream = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_Stream)))
});
ScalaJS.as.sci_Stream = (function(obj) {
  return ((ScalaJS.is.sci_Stream(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.Stream"))
});
ScalaJS.isArrayOf.sci_Stream = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_Stream)))
});
ScalaJS.asArrayOf.sci_Stream = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_Stream(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.Stream;", depth))
});
ScalaJS.is.scm_Buffer = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_Buffer)))
});
ScalaJS.as.scm_Buffer = (function(obj) {
  return ((ScalaJS.is.scm_Buffer(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.Buffer"))
});
ScalaJS.isArrayOf.scm_Buffer = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_Buffer)))
});
ScalaJS.asArrayOf.scm_Buffer = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_Buffer(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.Buffer;", depth))
});
/** @constructor */
ScalaJS.c.sci_HashMap$EmptyHashMap$ = (function() {
  ScalaJS.c.sci_HashMap.call(this)
});
ScalaJS.c.sci_HashMap$EmptyHashMap$.prototype = new ScalaJS.h.sci_HashMap();
ScalaJS.c.sci_HashMap$EmptyHashMap$.prototype.constructor = ScalaJS.c.sci_HashMap$EmptyHashMap$;
/** @constructor */
ScalaJS.h.sci_HashMap$EmptyHashMap$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$EmptyHashMap$.prototype = ScalaJS.c.sci_HashMap$EmptyHashMap$.prototype;
ScalaJS.d.sci_HashMap$EmptyHashMap$ = new ScalaJS.ClassTypeData({
  sci_HashMap$EmptyHashMap$: 0
}, false, "scala.collection.immutable.HashMap$EmptyHashMap$", {
  sci_HashMap$EmptyHashMap$: 1,
  sci_HashMap: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  sc_CustomParallelizable: 1
});
ScalaJS.c.sci_HashMap$EmptyHashMap$.prototype.$classData = ScalaJS.d.sci_HashMap$EmptyHashMap$;
ScalaJS.n.sci_HashMap$EmptyHashMap$ = (void 0);
ScalaJS.m.sci_HashMap$EmptyHashMap$ = (function() {
  if ((!ScalaJS.n.sci_HashMap$EmptyHashMap$)) {
    ScalaJS.n.sci_HashMap$EmptyHashMap$ = new ScalaJS.c.sci_HashMap$EmptyHashMap$().init___()
  };
  return ScalaJS.n.sci_HashMap$EmptyHashMap$
});
/** @constructor */
ScalaJS.c.sci_HashMap$HashMap1 = (function() {
  ScalaJS.c.sci_HashMap.call(this);
  this.key$6 = null;
  this.hash$6 = 0;
  this.value$6 = null;
  this.kv$6 = null
});
ScalaJS.c.sci_HashMap$HashMap1.prototype = new ScalaJS.h.sci_HashMap();
ScalaJS.c.sci_HashMap$HashMap1.prototype.constructor = ScalaJS.c.sci_HashMap$HashMap1;
/** @constructor */
ScalaJS.h.sci_HashMap$HashMap1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$HashMap1.prototype = ScalaJS.c.sci_HashMap$HashMap1.prototype;
ScalaJS.c.sci_HashMap$HashMap1.prototype.ensurePair__T2 = (function() {
  if ((this.kv$6 !== null)) {
    return this.kv$6
  } else {
    this.kv$6 = new ScalaJS.c.T2().init___O__O(this.key$6, this.value$6);
    return this.kv$6
  }
});
ScalaJS.c.sci_HashMap$HashMap1.prototype.init___O__I__O__T2 = (function(key, hash, value, kv) {
  this.key$6 = key;
  this.hash$6 = hash;
  this.value$6 = value;
  this.kv$6 = kv;
  return this
});
ScalaJS.c.sci_HashMap$HashMap1.prototype.updated0__O__I__I__O__T2__sci_HashMap$Merger__sci_HashMap = (function(key, hash, level, value, kv, merger) {
  if (((hash === this.hash$6) && ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key$6))) {
    if ((merger === null)) {
      return ((this.value$6 === value) ? this : new ScalaJS.c.sci_HashMap$HashMap1().init___O__I__O__T2(key, hash, value, kv))
    } else {
      var nkv = merger.apply__T2__T2__T2(this.kv$6, kv);
      return new ScalaJS.c.sci_HashMap$HashMap1().init___O__I__O__T2(nkv.$$und1__O(), hash, nkv.$$und2__O(), nkv)
    }
  } else if ((hash !== this.hash$6)) {
    var that = new ScalaJS.c.sci_HashMap$HashMap1().init___O__I__O__T2(key, hash, value, kv);
    return ScalaJS.m.sci_HashMap$().scala$collection$immutable$HashMap$$makeHashTrieMap__I__sci_HashMap__I__sci_HashMap__I__I__sci_HashMap$HashTrieMap(this.hash$6, this, hash, that, level, 2)
  } else {
    var this$2 = ScalaJS.m.sci_ListMap$EmptyListMap$();
    var key$1 = this.key$6;
    var value$1 = this.value$6;
    return new ScalaJS.c.sci_HashMap$HashMapCollision1().init___I__sci_ListMap(hash, new ScalaJS.c.sci_ListMap$Node().init___sci_ListMap__O__O(this$2, key$1, value$1).updated__O__O__sci_ListMap(key, value))
  }
});
ScalaJS.c.sci_HashMap$HashMap1.prototype.get0__O__I__I__s_Option = (function(key, hash, level) {
  return (((hash === this.hash$6) && ScalaJS.m.sr_BoxesRunTime$().equals__O__O__Z(key, this.key$6)) ? new ScalaJS.c.s_Some().init___O(this.value$6) : ScalaJS.m.s_None$())
});
ScalaJS.c.sci_HashMap$HashMap1.prototype.foreach__F1__V = (function(f) {
  f.apply__O__O(this.ensurePair__T2())
});
ScalaJS.c.sci_HashMap$HashMap1.prototype.iterator__sc_Iterator = (function() {
  ScalaJS.m.sc_Iterator$();
  var elems = new ScalaJS.c.sjs_js_WrappedArray().init___sjs_js_Array([this.ensurePair__T2()]);
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(elems, 0, ScalaJS.uI(elems.array$6["length"]))
});
ScalaJS.c.sci_HashMap$HashMap1.prototype.size__I = (function() {
  return 1
});
ScalaJS.is.sci_HashMap$HashMap1 = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_HashMap$HashMap1)))
});
ScalaJS.as.sci_HashMap$HashMap1 = (function(obj) {
  return ((ScalaJS.is.sci_HashMap$HashMap1(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.HashMap$HashMap1"))
});
ScalaJS.isArrayOf.sci_HashMap$HashMap1 = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_HashMap$HashMap1)))
});
ScalaJS.asArrayOf.sci_HashMap$HashMap1 = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_HashMap$HashMap1(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.HashMap$HashMap1;", depth))
});
ScalaJS.d.sci_HashMap$HashMap1 = new ScalaJS.ClassTypeData({
  sci_HashMap$HashMap1: 0
}, false, "scala.collection.immutable.HashMap$HashMap1", {
  sci_HashMap$HashMap1: 1,
  sci_HashMap: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  sc_CustomParallelizable: 1
});
ScalaJS.c.sci_HashMap$HashMap1.prototype.$classData = ScalaJS.d.sci_HashMap$HashMap1;
/** @constructor */
ScalaJS.c.sci_HashMap$HashMapCollision1 = (function() {
  ScalaJS.c.sci_HashMap.call(this);
  this.hash$6 = 0;
  this.kvs$6 = null
});
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype = new ScalaJS.h.sci_HashMap();
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.constructor = ScalaJS.c.sci_HashMap$HashMapCollision1;
/** @constructor */
ScalaJS.h.sci_HashMap$HashMapCollision1 = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$HashMapCollision1.prototype = ScalaJS.c.sci_HashMap$HashMapCollision1.prototype;
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.updated0__O__I__I__O__T2__sci_HashMap$Merger__sci_HashMap = (function(key, hash, level, value, kv, merger) {
  if ((hash === this.hash$6)) {
    if ((merger === null)) {
      var jsx$1 = true
    } else {
      var this$1 = this.kvs$6;
      var jsx$1 = (!ScalaJS.s.sc_MapLike$class__contains__sc_MapLike__O__Z(this$1, key))
    };
    if (jsx$1) {
      return new ScalaJS.c.sci_HashMap$HashMapCollision1().init___I__sci_ListMap(hash, this.kvs$6.updated__O__O__sci_ListMap(key, value))
    } else {
      var this$2 = this.kvs$6;
      var kv$1 = merger.apply__T2__T2__T2(new ScalaJS.c.T2().init___O__O(key, this.kvs$6.apply__O__O(key)), kv);
      return new ScalaJS.c.sci_HashMap$HashMapCollision1().init___I__sci_ListMap(hash, this$2.updated__O__O__sci_ListMap(kv$1.$$und1__O(), kv$1.$$und2__O()))
    }
  } else {
    var that = new ScalaJS.c.sci_HashMap$HashMap1().init___O__I__O__T2(key, hash, value, kv);
    return ScalaJS.m.sci_HashMap$().scala$collection$immutable$HashMap$$makeHashTrieMap__I__sci_HashMap__I__sci_HashMap__I__I__sci_HashMap$HashTrieMap(this.hash$6, this, hash, that, level, ((1 + this.kvs$6.size__I()) | 0))
  }
});
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.get0__O__I__I__s_Option = (function(key, hash, level) {
  return ((hash === this.hash$6) ? this.kvs$6.get__O__s_Option(key) : ScalaJS.m.s_None$())
});
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.foreach__F1__V = (function(f) {
  var this$1 = this.kvs$6;
  var this$2 = this$1.iterator__sc_Iterator();
  ScalaJS.s.sc_Iterator$class__foreach__sc_Iterator__F1__V(this$2, f)
});
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.iterator__sc_Iterator = (function() {
  return this.kvs$6.iterator__sc_Iterator()
});
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.size__I = (function() {
  return this.kvs$6.size__I()
});
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.init___I__sci_ListMap = (function(hash, kvs) {
  this.hash$6 = hash;
  this.kvs$6 = kvs;
  return this
});
ScalaJS.d.sci_HashMap$HashMapCollision1 = new ScalaJS.ClassTypeData({
  sci_HashMap$HashMapCollision1: 0
}, false, "scala.collection.immutable.HashMap$HashMapCollision1", {
  sci_HashMap$HashMapCollision1: 1,
  sci_HashMap: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  sc_CustomParallelizable: 1
});
ScalaJS.c.sci_HashMap$HashMapCollision1.prototype.$classData = ScalaJS.d.sci_HashMap$HashMapCollision1;
/** @constructor */
ScalaJS.c.sci_HashMap$HashTrieMap = (function() {
  ScalaJS.c.sci_HashMap.call(this);
  this.bitmap$6 = 0;
  this.elems$6 = null;
  this.size0$6 = 0
});
ScalaJS.c.sci_HashMap$HashTrieMap.prototype = new ScalaJS.h.sci_HashMap();
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.constructor = ScalaJS.c.sci_HashMap$HashTrieMap;
/** @constructor */
ScalaJS.h.sci_HashMap$HashTrieMap = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_HashMap$HashTrieMap.prototype = ScalaJS.c.sci_HashMap$HashTrieMap.prototype;
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.updated0__O__I__I__O__T2__sci_HashMap$Merger__sci_HashMap = (function(key, hash, level, value, kv, merger) {
  var index = (31 & ((hash >>> level) | 0));
  var mask = (1 << index);
  var offset = ScalaJS.m.jl_Integer$().bitCount__I__I((this.bitmap$6 & (((-1) + mask) | 0)));
  if (((this.bitmap$6 & mask) !== 0)) {
    var sub = this.elems$6.u[offset];
    var subNew = sub.updated0__O__I__I__O__T2__sci_HashMap$Merger__sci_HashMap(key, hash, ((5 + level) | 0), value, kv, merger);
    if ((subNew === sub)) {
      return this
    } else {
      var elemsNew = ScalaJS.newArrayObject(ScalaJS.d.sci_HashMap.getArrayOf(), [this.elems$6.u["length"]]);
      ScalaJS.m.s_Array$().copy__O__I__O__I__I__V(this.elems$6, 0, elemsNew, 0, this.elems$6.u["length"]);
      elemsNew.u[offset] = subNew;
      return new ScalaJS.c.sci_HashMap$HashTrieMap().init___I__Asci_HashMap__I(this.bitmap$6, elemsNew, ((this.size0$6 + ((subNew.size__I() - sub.size__I()) | 0)) | 0))
    }
  } else {
    var elemsNew$2 = ScalaJS.newArrayObject(ScalaJS.d.sci_HashMap.getArrayOf(), [((1 + this.elems$6.u["length"]) | 0)]);
    ScalaJS.m.s_Array$().copy__O__I__O__I__I__V(this.elems$6, 0, elemsNew$2, 0, offset);
    elemsNew$2.u[offset] = new ScalaJS.c.sci_HashMap$HashMap1().init___O__I__O__T2(key, hash, value, kv);
    ScalaJS.m.s_Array$().copy__O__I__O__I__I__V(this.elems$6, offset, elemsNew$2, ((1 + offset) | 0), ((this.elems$6.u["length"] - offset) | 0));
    return new ScalaJS.c.sci_HashMap$HashTrieMap().init___I__Asci_HashMap__I((this.bitmap$6 | mask), elemsNew$2, ((1 + this.size0$6) | 0))
  }
});
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.get0__O__I__I__s_Option = (function(key, hash, level) {
  var index = (31 & ((hash >>> level) | 0));
  var mask = (1 << index);
  if ((this.bitmap$6 === (-1))) {
    return this.elems$6.u[(31 & index)].get0__O__I__I__s_Option(key, hash, ((5 + level) | 0))
  } else if (((this.bitmap$6 & mask) !== 0)) {
    var offset = ScalaJS.m.jl_Integer$().bitCount__I__I((this.bitmap$6 & (((-1) + mask) | 0)));
    return this.elems$6.u[offset].get0__O__I__I__s_Option(key, hash, ((5 + level) | 0))
  } else {
    return ScalaJS.m.s_None$()
  }
});
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.foreach__F1__V = (function(f) {
  var i = 0;
  while ((i < this.elems$6.u["length"])) {
    this.elems$6.u[i].foreach__F1__V(f);
    i = ((1 + i) | 0)
  }
});
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.sci_HashMap$HashTrieMap$$anon$1().init___sci_HashMap$HashTrieMap(this)
});
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.size__I = (function() {
  return this.size0$6
});
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.init___I__Asci_HashMap__I = (function(bitmap, elems, size0) {
  this.bitmap$6 = bitmap;
  this.elems$6 = elems;
  this.size0$6 = size0;
  return this
});
ScalaJS.is.sci_HashMap$HashTrieMap = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_HashMap$HashTrieMap)))
});
ScalaJS.as.sci_HashMap$HashTrieMap = (function(obj) {
  return ((ScalaJS.is.sci_HashMap$HashTrieMap(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.HashMap$HashTrieMap"))
});
ScalaJS.isArrayOf.sci_HashMap$HashTrieMap = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_HashMap$HashTrieMap)))
});
ScalaJS.asArrayOf.sci_HashMap$HashTrieMap = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_HashMap$HashTrieMap(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.HashMap$HashTrieMap;", depth))
});
ScalaJS.d.sci_HashMap$HashTrieMap = new ScalaJS.ClassTypeData({
  sci_HashMap$HashTrieMap: 0
}, false, "scala.collection.immutable.HashMap$HashTrieMap", {
  sci_HashMap$HashTrieMap: 1,
  sci_HashMap: 1,
  sci_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  sci_Map: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sci_MapLike: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  sc_CustomParallelizable: 1
});
ScalaJS.c.sci_HashMap$HashTrieMap.prototype.$classData = ScalaJS.d.sci_HashMap$HashTrieMap;
/** @constructor */
ScalaJS.c.sci_Stream$Cons = (function() {
  ScalaJS.c.sci_Stream.call(this);
  this.hd$5 = null;
  this.tlVal$5 = null;
  this.tlGen$5 = null
});
ScalaJS.c.sci_Stream$Cons.prototype = new ScalaJS.h.sci_Stream();
ScalaJS.c.sci_Stream$Cons.prototype.constructor = ScalaJS.c.sci_Stream$Cons;
/** @constructor */
ScalaJS.h.sci_Stream$Cons = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream$Cons.prototype = ScalaJS.c.sci_Stream$Cons.prototype;
ScalaJS.c.sci_Stream$Cons.prototype.head__O = (function() {
  return this.hd$5
});
ScalaJS.c.sci_Stream$Cons.prototype.tail__sci_Stream = (function() {
  if ((!this.tailDefined__Z())) {
    if ((!this.tailDefined__Z())) {
      this.tlVal$5 = ScalaJS.as.sci_Stream(this.tlGen$5.apply__O());
      this.tlGen$5 = null
    }
  };
  return this.tlVal$5
});
ScalaJS.c.sci_Stream$Cons.prototype.tailDefined__Z = (function() {
  return (this.tlGen$5 === null)
});
ScalaJS.c.sci_Stream$Cons.prototype.isEmpty__Z = (function() {
  return false
});
ScalaJS.c.sci_Stream$Cons.prototype.tail__O = (function() {
  return this.tail__sci_Stream()
});
ScalaJS.c.sci_Stream$Cons.prototype.init___O__F0 = (function(hd, tl) {
  this.hd$5 = hd;
  this.tlGen$5 = tl;
  return this
});
ScalaJS.d.sci_Stream$Cons = new ScalaJS.ClassTypeData({
  sci_Stream$Cons: 0
}, false, "scala.collection.immutable.Stream$Cons", {
  sci_Stream$Cons: 1,
  sci_Stream: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_LinearSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_LinearSeq: 1,
  sc_LinearSeqLike: 1,
  sc_LinearSeqOptimized: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Stream$Cons.prototype.$classData = ScalaJS.d.sci_Stream$Cons;
/** @constructor */
ScalaJS.c.sci_Stream$Empty$ = (function() {
  ScalaJS.c.sci_Stream.call(this)
});
ScalaJS.c.sci_Stream$Empty$.prototype = new ScalaJS.h.sci_Stream();
ScalaJS.c.sci_Stream$Empty$.prototype.constructor = ScalaJS.c.sci_Stream$Empty$;
/** @constructor */
ScalaJS.h.sci_Stream$Empty$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Stream$Empty$.prototype = ScalaJS.c.sci_Stream$Empty$.prototype;
ScalaJS.c.sci_Stream$Empty$.prototype.head__O = (function() {
  this.head__sr_Nothing$()
});
ScalaJS.c.sci_Stream$Empty$.prototype.tailDefined__Z = (function() {
  return false
});
ScalaJS.c.sci_Stream$Empty$.prototype.isEmpty__Z = (function() {
  return true
});
ScalaJS.c.sci_Stream$Empty$.prototype.tail__sr_Nothing$ = (function() {
  throw new ScalaJS.c.jl_UnsupportedOperationException().init___T("tail of empty stream")
});
ScalaJS.c.sci_Stream$Empty$.prototype.head__sr_Nothing$ = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("head of empty stream")
});
ScalaJS.c.sci_Stream$Empty$.prototype.tail__O = (function() {
  this.tail__sr_Nothing$()
});
ScalaJS.d.sci_Stream$Empty$ = new ScalaJS.ClassTypeData({
  sci_Stream$Empty$: 0
}, false, "scala.collection.immutable.Stream$Empty$", {
  sci_Stream$Empty$: 1,
  sci_Stream: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_LinearSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_LinearSeq: 1,
  sc_LinearSeqLike: 1,
  sc_LinearSeqOptimized: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.sci_Stream$Empty$.prototype.$classData = ScalaJS.d.sci_Stream$Empty$;
ScalaJS.n.sci_Stream$Empty$ = (void 0);
ScalaJS.m.sci_Stream$Empty$ = (function() {
  if ((!ScalaJS.n.sci_Stream$Empty$)) {
    ScalaJS.n.sci_Stream$Empty$ = new ScalaJS.c.sci_Stream$Empty$().init___()
  };
  return ScalaJS.n.sci_Stream$Empty$
});
/** @constructor */
ScalaJS.c.sci_Vector = (function() {
  ScalaJS.c.sc_AbstractSeq.call(this);
  this.startIndex$4 = 0;
  this.endIndex$4 = 0;
  this.focus$4 = 0;
  this.dirty$4 = false;
  this.depth$4 = 0;
  this.display0$4 = null;
  this.display1$4 = null;
  this.display2$4 = null;
  this.display3$4 = null;
  this.display4$4 = null;
  this.display5$4 = null
});
ScalaJS.c.sci_Vector.prototype = new ScalaJS.h.sc_AbstractSeq();
ScalaJS.c.sci_Vector.prototype.constructor = ScalaJS.c.sci_Vector;
/** @constructor */
ScalaJS.h.sci_Vector = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Vector.prototype = ScalaJS.c.sci_Vector.prototype;
ScalaJS.c.sci_Vector.prototype.checkRangeConvert__p4__I__I = (function(index) {
  var idx = ((index + this.startIndex$4) | 0);
  if (((index >= 0) && (idx < this.endIndex$4))) {
    return idx
  } else {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + index))
  }
});
ScalaJS.c.sci_Vector.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_Vector.prototype.display3__AO = (function() {
  return this.display3$4
});
ScalaJS.c.sci_Vector.prototype.gotoPosWritable__p4__I__I__I__V = (function(oldIndex, newIndex, xor) {
  if (this.dirty$4) {
    ScalaJS.s.sci_VectorPointer$class__gotoPosWritable1__sci_VectorPointer__I__I__I__V(this, oldIndex, newIndex, xor)
  } else {
    ScalaJS.s.sci_VectorPointer$class__gotoPosWritable0__sci_VectorPointer__I__I__V(this, newIndex, xor);
    this.dirty$4 = true
  }
});
ScalaJS.c.sci_Vector.prototype.head__O = (function() {
  if (ScalaJS.s.sc_SeqLike$class__isEmpty__sc_SeqLike__Z(this)) {
    throw new ScalaJS.c.jl_UnsupportedOperationException().init___T("empty.head")
  };
  return this.apply__I__O(0)
});
ScalaJS.c.sci_Vector.prototype.apply__I__O = (function(index) {
  var idx = this.checkRangeConvert__p4__I__I(index);
  var xor = (idx ^ this.focus$4);
  return ScalaJS.s.sci_VectorPointer$class__getElem__sci_VectorPointer__I__I__O(this, idx, xor)
});
ScalaJS.c.sci_Vector.prototype.depth__I = (function() {
  return this.depth$4
});
ScalaJS.c.sci_Vector.prototype.lengthCompare__I__I = (function(len) {
  return ((this.length__I() - len) | 0)
});
ScalaJS.c.sci_Vector.prototype.apply__O__O = (function(v1) {
  return this.apply__I__O(ScalaJS.uI(v1))
});
ScalaJS.c.sci_Vector.prototype.initIterator__sci_VectorIterator__V = (function(s) {
  var depth = this.depth$4;
  ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s, this, depth);
  if (this.dirty$4) {
    var index = this.focus$4;
    ScalaJS.s.sci_VectorPointer$class__stabilize__sci_VectorPointer__I__V(s, index)
  };
  if ((s.depth$2 > 1)) {
    var index$1 = this.startIndex$4;
    var xor = (this.startIndex$4 ^ this.focus$4);
    ScalaJS.s.sci_VectorPointer$class__gotoPos__sci_VectorPointer__I__I__V(s, index$1, xor)
  }
});
ScalaJS.c.sci_Vector.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_Vector.prototype.init___I__I__I = (function(startIndex, endIndex, focus) {
  this.startIndex$4 = startIndex;
  this.endIndex$4 = endIndex;
  this.focus$4 = focus;
  this.dirty$4 = false;
  return this
});
ScalaJS.c.sci_Vector.prototype.display5$und$eq__AO__V = (function(x$1) {
  this.display5$4 = x$1
});
ScalaJS.c.sci_Vector.prototype.$$colon$plus__O__scg_CanBuildFrom__O = (function(elem, bf) {
  return ((bf === (ScalaJS.m.sci_IndexedSeq$(), ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6)) ? this.appendBack__O__sci_Vector(elem) : ScalaJS.s.sc_SeqLike$class__$$colon$plus__sc_SeqLike__O__scg_CanBuildFrom__O(this, elem, bf))
});
ScalaJS.c.sci_Vector.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_Vector$()
});
ScalaJS.c.sci_Vector.prototype.cleanLeftEdge__p4__I__V = (function(cutIndex) {
  if ((cutIndex < 32)) {
    this.zeroLeft__p4__AO__I__V(this.display0$4, cutIndex)
  } else if ((cutIndex < 1024)) {
    this.zeroLeft__p4__AO__I__V(this.display0$4, (31 & cutIndex));
    this.display1$4 = this.copyRight__p4__AO__I__AO(this.display1$4, ((cutIndex >>> 5) | 0))
  } else if ((cutIndex < 32768)) {
    this.zeroLeft__p4__AO__I__V(this.display0$4, (31 & cutIndex));
    this.display1$4 = this.copyRight__p4__AO__I__AO(this.display1$4, (31 & ((cutIndex >>> 5) | 0)));
    this.display2$4 = this.copyRight__p4__AO__I__AO(this.display2$4, ((cutIndex >>> 10) | 0))
  } else if ((cutIndex < 1048576)) {
    this.zeroLeft__p4__AO__I__V(this.display0$4, (31 & cutIndex));
    this.display1$4 = this.copyRight__p4__AO__I__AO(this.display1$4, (31 & ((cutIndex >>> 5) | 0)));
    this.display2$4 = this.copyRight__p4__AO__I__AO(this.display2$4, (31 & ((cutIndex >>> 10) | 0)));
    this.display3$4 = this.copyRight__p4__AO__I__AO(this.display3$4, ((cutIndex >>> 15) | 0))
  } else if ((cutIndex < 33554432)) {
    this.zeroLeft__p4__AO__I__V(this.display0$4, (31 & cutIndex));
    this.display1$4 = this.copyRight__p4__AO__I__AO(this.display1$4, (31 & ((cutIndex >>> 5) | 0)));
    this.display2$4 = this.copyRight__p4__AO__I__AO(this.display2$4, (31 & ((cutIndex >>> 10) | 0)));
    this.display3$4 = this.copyRight__p4__AO__I__AO(this.display3$4, (31 & ((cutIndex >>> 15) | 0)));
    this.display4$4 = this.copyRight__p4__AO__I__AO(this.display4$4, ((cutIndex >>> 20) | 0))
  } else if ((cutIndex < 1073741824)) {
    this.zeroLeft__p4__AO__I__V(this.display0$4, (31 & cutIndex));
    this.display1$4 = this.copyRight__p4__AO__I__AO(this.display1$4, (31 & ((cutIndex >>> 5) | 0)));
    this.display2$4 = this.copyRight__p4__AO__I__AO(this.display2$4, (31 & ((cutIndex >>> 10) | 0)));
    this.display3$4 = this.copyRight__p4__AO__I__AO(this.display3$4, (31 & ((cutIndex >>> 15) | 0)));
    this.display4$4 = this.copyRight__p4__AO__I__AO(this.display4$4, (31 & ((cutIndex >>> 20) | 0)));
    this.display5$4 = this.copyRight__p4__AO__I__AO(this.display5$4, ((cutIndex >>> 25) | 0))
  } else {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___()
  }
});
ScalaJS.c.sci_Vector.prototype.display0__AO = (function() {
  return this.display0$4
});
ScalaJS.c.sci_Vector.prototype.display2$und$eq__AO__V = (function(x$1) {
  this.display2$4 = x$1
});
ScalaJS.c.sci_Vector.prototype.display4__AO = (function() {
  return this.display4$4
});
ScalaJS.c.sci_Vector.prototype.shiftTopLevel__p4__I__I__V = (function(oldLeft, newLeft) {
  var x1 = (((-1) + this.depth$4) | 0);
  switch (x1) {
    case 0:
      {
        var array = this.display0$4;
        this.display0$4 = ScalaJS.s.sci_VectorPointer$class__copyRange__sci_VectorPointer__AO__I__I__AO(this, array, oldLeft, newLeft);
        break
      };
    case 1:
      {
        var array$1 = this.display1$4;
        this.display1$4 = ScalaJS.s.sci_VectorPointer$class__copyRange__sci_VectorPointer__AO__I__I__AO(this, array$1, oldLeft, newLeft);
        break
      };
    case 2:
      {
        var array$2 = this.display2$4;
        this.display2$4 = ScalaJS.s.sci_VectorPointer$class__copyRange__sci_VectorPointer__AO__I__I__AO(this, array$2, oldLeft, newLeft);
        break
      };
    case 3:
      {
        var array$3 = this.display3$4;
        this.display3$4 = ScalaJS.s.sci_VectorPointer$class__copyRange__sci_VectorPointer__AO__I__I__AO(this, array$3, oldLeft, newLeft);
        break
      };
    case 4:
      {
        var array$4 = this.display4$4;
        this.display4$4 = ScalaJS.s.sci_VectorPointer$class__copyRange__sci_VectorPointer__AO__I__I__AO(this, array$4, oldLeft, newLeft);
        break
      };
    case 5:
      {
        var array$5 = this.display5$4;
        this.display5$4 = ScalaJS.s.sci_VectorPointer$class__copyRange__sci_VectorPointer__AO__I__I__AO(this, array$5, oldLeft, newLeft);
        break
      };
    default:
      throw new ScalaJS.c.s_MatchError().init___O(x1);
  }
});
ScalaJS.c.sci_Vector.prototype.tail__sci_Vector = (function() {
  if (ScalaJS.s.sc_SeqLike$class__isEmpty__sc_SeqLike__Z(this)) {
    throw new ScalaJS.c.jl_UnsupportedOperationException().init___T("empty.tail")
  };
  return this.drop__I__sci_Vector(1)
});
ScalaJS.c.sci_Vector.prototype.toVector__sci_Vector = (function() {
  return this
});
ScalaJS.c.sci_Vector.prototype.appendBack__O__sci_Vector = (function(value) {
  if ((this.endIndex$4 !== this.startIndex$4)) {
    var blockIndex = ((-32) & this.endIndex$4);
    var lo = (31 & this.endIndex$4);
    if ((this.endIndex$4 !== blockIndex)) {
      var s = new ScalaJS.c.sci_Vector().init___I__I__I(this.startIndex$4, ((1 + this.endIndex$4) | 0), blockIndex);
      var depth = this.depth$4;
      ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s, this, depth);
      s.dirty$4 = this.dirty$4;
      s.gotoPosWritable__p4__I__I__I__V(this.focus$4, blockIndex, (this.focus$4 ^ blockIndex));
      s.display0$4.u[lo] = value;
      return s
    } else {
      var shift = (this.startIndex$4 & (~(((-1) + (1 << ScalaJS.imul(5, (((-1) + this.depth$4) | 0)))) | 0)));
      var shiftBlocks = ((this.startIndex$4 >>> ScalaJS.imul(5, (((-1) + this.depth$4) | 0))) | 0);
      if ((shift !== 0)) {
        ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(this);
        if ((this.depth$4 > 1)) {
          var newBlockIndex = ((blockIndex - shift) | 0);
          var newFocus = ((this.focus$4 - shift) | 0);
          var s$2 = new ScalaJS.c.sci_Vector().init___I__I__I(((this.startIndex$4 - shift) | 0), ((((1 + this.endIndex$4) | 0) - shift) | 0), newBlockIndex);
          var depth$1 = this.depth$4;
          ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s$2, this, depth$1);
          s$2.dirty$4 = this.dirty$4;
          s$2.shiftTopLevel__p4__I__I__V(shiftBlocks, 0);
          ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$2);
          s$2.gotoFreshPosWritable__p4__I__I__I__V(newFocus, newBlockIndex, (newFocus ^ newBlockIndex));
          s$2.display0$4.u[lo] = value;
          ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$2);
          return s$2
        } else {
          var newBlockIndex$2 = (((-32) + blockIndex) | 0);
          var newFocus$2 = this.focus$4;
          var s$3 = new ScalaJS.c.sci_Vector().init___I__I__I(((this.startIndex$4 - shift) | 0), ((((1 + this.endIndex$4) | 0) - shift) | 0), newBlockIndex$2);
          var depth$2 = this.depth$4;
          ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s$3, this, depth$2);
          s$3.dirty$4 = this.dirty$4;
          s$3.shiftTopLevel__p4__I__I__V(shiftBlocks, 0);
          s$3.gotoPosWritable__p4__I__I__I__V(newFocus$2, newBlockIndex$2, (newFocus$2 ^ newBlockIndex$2));
          s$3.display0$4.u[((32 - shift) | 0)] = value;
          ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$3);
          return s$3
        }
      } else {
        var newFocus$3 = this.focus$4;
        var s$4 = new ScalaJS.c.sci_Vector().init___I__I__I(this.startIndex$4, ((1 + this.endIndex$4) | 0), blockIndex);
        var depth$3 = this.depth$4;
        ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s$4, this, depth$3);
        s$4.dirty$4 = this.dirty$4;
        s$4.gotoFreshPosWritable__p4__I__I__I__V(newFocus$3, blockIndex, (newFocus$3 ^ blockIndex));
        s$4.display0$4.u[lo] = value;
        if ((s$4.depth$4 === ((1 + this.depth$4) | 0))) {
          ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$4)
        };
        return s$4
      }
    }
  } else {
    var elems = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]);
    elems.u[0] = value;
    var s$5 = new ScalaJS.c.sci_Vector().init___I__I__I(0, 1, 0);
    s$5.depth$4 = 1;
    s$5.display0$4 = elems;
    return s$5
  }
});
ScalaJS.c.sci_Vector.prototype.preClean__p4__I__V = (function(depth) {
  this.depth$4 = depth;
  var x1 = (((-1) + depth) | 0);
  switch (x1) {
    case 0:
      {
        this.display1$4 = null;
        this.display2$4 = null;
        this.display3$4 = null;
        this.display4$4 = null;
        this.display5$4 = null;
        break
      };
    case 1:
      {
        this.display2$4 = null;
        this.display3$4 = null;
        this.display4$4 = null;
        this.display5$4 = null;
        break
      };
    case 2:
      {
        this.display3$4 = null;
        this.display4$4 = null;
        this.display5$4 = null;
        break
      };
    case 3:
      {
        this.display4$4 = null;
        this.display5$4 = null;
        break
      };
    case 4:
      {
        this.display5$4 = null;
        break
      };
    case 5:
      break;
    default:
      throw new ScalaJS.c.s_MatchError().init___O(x1);
  }
});
ScalaJS.c.sci_Vector.prototype.$$plus$colon__O__scg_CanBuildFrom__O = (function(elem, bf) {
  return ((bf === (ScalaJS.m.sci_IndexedSeq$(), ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6)) ? this.appendFront__O__sci_Vector(elem) : ScalaJS.s.sc_SeqLike$class__$$plus$colon__sc_SeqLike__O__scg_CanBuildFrom__O(this, elem, bf))
});
ScalaJS.c.sci_Vector.prototype.iterator__sc_Iterator = (function() {
  return this.iterator__sci_VectorIterator()
});
ScalaJS.c.sci_Vector.prototype.display1$und$eq__AO__V = (function(x$1) {
  this.display1$4 = x$1
});
ScalaJS.c.sci_Vector.prototype.$$plus$plus__sc_GenTraversableOnce__scg_CanBuildFrom__O = (function(that, bf) {
  if ((bf === (ScalaJS.m.sci_IndexedSeq$(), ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6))) {
    if (that.isEmpty__Z()) {
      return this
    } else {
      var again = ((!that.isTraversableAgain__Z()) ? that.toVector__sci_Vector() : that);
      var x1 = again.size__I();
      switch (x1) {
        default:
          if (((x1 <= 2) || (x1 < (this.length__I() >> 5)))) {
            var v = new ScalaJS.c.sr_ObjectRef().init___O(this);
            again.foreach__F1__V(new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2$1, v$1) {
              return (function(x$2) {
                v$1.elem$1 = ScalaJS.as.sci_Vector(ScalaJS.as.sci_Vector(v$1.elem$1).$$colon$plus__O__scg_CanBuildFrom__O(x$2, (ScalaJS.m.sci_Vector$(), ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6)))
              })
            })(this, v)));
            return ScalaJS.as.sci_Vector(v.elem$1)
          } else if (((this.length__I() < (x1 >> 5)) && ScalaJS.is.sci_Vector(again))) {
            var v$2 = ScalaJS.as.sci_Vector(again);
            var ri = new ScalaJS.c.sci_Vector$$anon$1().init___sci_Vector(this);
            while (ri.hasNext__Z()) {
              var x$1 = ri.next__O();
              v$2 = ScalaJS.as.sci_Vector(v$2.$$plus$colon__O__scg_CanBuildFrom__O(x$1, (ScalaJS.m.sci_Vector$(), ScalaJS.m.sc_IndexedSeq$().ReusableCBF$6)))
            };
            return v$2
          } else {
            return ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O(this, again, bf)
          };
      }
    }
  } else {
    return ScalaJS.s.sc_TraversableLike$class__$$plus$plus__sc_TraversableLike__sc_GenTraversableOnce__scg_CanBuildFrom__O(this, that.seq__sc_TraversableOnce(), bf)
  }
});
ScalaJS.c.sci_Vector.prototype.length__I = (function() {
  return ((this.endIndex$4 - this.startIndex$4) | 0)
});
ScalaJS.c.sci_Vector.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_Vector.prototype.display4$und$eq__AO__V = (function(x$1) {
  this.display4$4 = x$1
});
ScalaJS.c.sci_Vector.prototype.gotoFreshPosWritable__p4__I__I__I__V = (function(oldIndex, newIndex, xor) {
  if (this.dirty$4) {
    ScalaJS.s.sci_VectorPointer$class__gotoFreshPosWritable1__sci_VectorPointer__I__I__I__V(this, oldIndex, newIndex, xor)
  } else {
    ScalaJS.s.sci_VectorPointer$class__gotoFreshPosWritable0__sci_VectorPointer__I__I__I__V(this, oldIndex, newIndex, xor);
    this.dirty$4 = true
  }
});
ScalaJS.c.sci_Vector.prototype.display1__AO = (function() {
  return this.display1$4
});
ScalaJS.c.sci_Vector.prototype.drop__I__O = (function(n) {
  return this.drop__I__sci_Vector(n)
});
ScalaJS.c.sci_Vector.prototype.display5__AO = (function() {
  return this.display5$4
});
ScalaJS.c.sci_Vector.prototype.tail__O = (function() {
  return this.tail__sci_Vector()
});
ScalaJS.c.sci_Vector.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_Vector.prototype.requiredDepth__p4__I__I = (function(xor) {
  if ((xor < 32)) {
    return 1
  } else if ((xor < 1024)) {
    return 2
  } else if ((xor < 32768)) {
    return 3
  } else if ((xor < 1048576)) {
    return 4
  } else if ((xor < 33554432)) {
    return 5
  } else if ((xor < 1073741824)) {
    return 6
  } else {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___()
  }
});
ScalaJS.c.sci_Vector.prototype.iterator__sci_VectorIterator = (function() {
  var s = new ScalaJS.c.sci_VectorIterator().init___I__I(this.startIndex$4, this.endIndex$4);
  this.initIterator__sci_VectorIterator__V(s);
  return s
});
ScalaJS.c.sci_Vector.prototype.isDefinedAt__O__Z = (function(x) {
  var idx = ScalaJS.uI(x);
  return ScalaJS.s.sc_GenSeqLike$class__isDefinedAt__sc_GenSeqLike__I__Z(this, idx)
});
ScalaJS.c.sci_Vector.prototype.zeroLeft__p4__AO__I__V = (function(array, index) {
  var i = 0;
  while ((i < index)) {
    array.u[i] = null;
    i = ((1 + i) | 0)
  }
});
ScalaJS.c.sci_Vector.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
ScalaJS.c.sci_Vector.prototype.depth$und$eq__I__V = (function(x$1) {
  this.depth$4 = x$1
});
ScalaJS.c.sci_Vector.prototype.display2__AO = (function() {
  return this.display2$4
});
ScalaJS.c.sci_Vector.prototype.dropFront0__p4__I__sci_Vector = (function(cutIndex) {
  var blockIndex = ((-32) & cutIndex);
  var xor = (cutIndex ^ (((-1) + this.endIndex$4) | 0));
  var d = this.requiredDepth__p4__I__I(xor);
  var shift = (cutIndex & (~(((-1) + (1 << ScalaJS.imul(5, d))) | 0)));
  var s = new ScalaJS.c.sci_Vector().init___I__I__I(((cutIndex - shift) | 0), ((this.endIndex$4 - shift) | 0), ((blockIndex - shift) | 0));
  var depth = this.depth$4;
  ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s, this, depth);
  s.dirty$4 = this.dirty$4;
  s.gotoPosWritable__p4__I__I__I__V(this.focus$4, blockIndex, (this.focus$4 ^ blockIndex));
  s.preClean__p4__I__V(d);
  s.cleanLeftEdge__p4__I__V(((cutIndex - shift) | 0));
  return s
});
ScalaJS.c.sci_Vector.prototype.display0$und$eq__AO__V = (function(x$1) {
  this.display0$4 = x$1
});
ScalaJS.c.sci_Vector.prototype.appendFront__O__sci_Vector = (function(value) {
  if ((this.endIndex$4 !== this.startIndex$4)) {
    var blockIndex = ((-32) & (((-1) + this.startIndex$4) | 0));
    var lo = (31 & (((-1) + this.startIndex$4) | 0));
    if ((this.startIndex$4 !== ((32 + blockIndex) | 0))) {
      var s = new ScalaJS.c.sci_Vector().init___I__I__I((((-1) + this.startIndex$4) | 0), this.endIndex$4, blockIndex);
      var depth = this.depth$4;
      ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s, this, depth);
      s.dirty$4 = this.dirty$4;
      s.gotoPosWritable__p4__I__I__I__V(this.focus$4, blockIndex, (this.focus$4 ^ blockIndex));
      s.display0$4.u[lo] = value;
      return s
    } else {
      var freeSpace = (((1 << ScalaJS.imul(5, this.depth$4)) - this.endIndex$4) | 0);
      var shift = (freeSpace & (~(((-1) + (1 << ScalaJS.imul(5, (((-1) + this.depth$4) | 0)))) | 0)));
      var shiftBlocks = ((freeSpace >>> ScalaJS.imul(5, (((-1) + this.depth$4) | 0))) | 0);
      if ((shift !== 0)) {
        ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(this);
        if ((this.depth$4 > 1)) {
          var newBlockIndex = ((blockIndex + shift) | 0);
          var newFocus = ((this.focus$4 + shift) | 0);
          var s$2 = new ScalaJS.c.sci_Vector().init___I__I__I((((((-1) + this.startIndex$4) | 0) + shift) | 0), ((this.endIndex$4 + shift) | 0), newBlockIndex);
          var depth$1 = this.depth$4;
          ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s$2, this, depth$1);
          s$2.dirty$4 = this.dirty$4;
          s$2.shiftTopLevel__p4__I__I__V(0, shiftBlocks);
          ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$2);
          s$2.gotoFreshPosWritable__p4__I__I__I__V(newFocus, newBlockIndex, (newFocus ^ newBlockIndex));
          s$2.display0$4.u[lo] = value;
          return s$2
        } else {
          var newBlockIndex$2 = ((32 + blockIndex) | 0);
          var newFocus$2 = this.focus$4;
          var s$3 = new ScalaJS.c.sci_Vector().init___I__I__I((((((-1) + this.startIndex$4) | 0) + shift) | 0), ((this.endIndex$4 + shift) | 0), newBlockIndex$2);
          var depth$2 = this.depth$4;
          ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s$3, this, depth$2);
          s$3.dirty$4 = this.dirty$4;
          s$3.shiftTopLevel__p4__I__I__V(0, shiftBlocks);
          s$3.gotoPosWritable__p4__I__I__I__V(newFocus$2, newBlockIndex$2, (newFocus$2 ^ newBlockIndex$2));
          s$3.display0$4.u[(((-1) + shift) | 0)] = value;
          ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$3);
          return s$3
        }
      } else if ((blockIndex < 0)) {
        var move = (((1 << ScalaJS.imul(5, ((1 + this.depth$4) | 0))) - (1 << ScalaJS.imul(5, this.depth$4))) | 0);
        var newBlockIndex$3 = ((blockIndex + move) | 0);
        var newFocus$3 = ((this.focus$4 + move) | 0);
        var s$4 = new ScalaJS.c.sci_Vector().init___I__I__I((((((-1) + this.startIndex$4) | 0) + move) | 0), ((this.endIndex$4 + move) | 0), newBlockIndex$3);
        var depth$3 = this.depth$4;
        ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s$4, this, depth$3);
        s$4.dirty$4 = this.dirty$4;
        ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$4);
        s$4.gotoFreshPosWritable__p4__I__I__I__V(newFocus$3, newBlockIndex$3, (newFocus$3 ^ newBlockIndex$3));
        s$4.display0$4.u[lo] = value;
        ScalaJS.s.sci_VectorPointer$class__debug__sci_VectorPointer__V(s$4);
        return s$4
      } else {
        var newFocus$4 = this.focus$4;
        var s$5 = new ScalaJS.c.sci_Vector().init___I__I__I((((-1) + this.startIndex$4) | 0), this.endIndex$4, blockIndex);
        var depth$4 = this.depth$4;
        ScalaJS.s.sci_VectorPointer$class__initFrom__sci_VectorPointer__sci_VectorPointer__I__V(s$5, this, depth$4);
        s$5.dirty$4 = this.dirty$4;
        s$5.gotoFreshPosWritable__p4__I__I__I__V(newFocus$4, blockIndex, (newFocus$4 ^ blockIndex));
        s$5.display0$4.u[lo] = value;
        return s$5
      }
    }
  } else {
    var elems = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [32]);
    elems.u[31] = value;
    var s$6 = new ScalaJS.c.sci_Vector().init___I__I__I(31, 32, 0);
    s$6.depth$4 = 1;
    s$6.display0$4 = elems;
    return s$6
  }
});
ScalaJS.c.sci_Vector.prototype.drop__I__sci_Vector = (function(n) {
  if ((n <= 0)) {
    return this
  } else if ((((this.startIndex$4 + n) | 0) < this.endIndex$4)) {
    return this.dropFront0__p4__I__sci_Vector(((this.startIndex$4 + n) | 0))
  } else {
    var this$1 = ScalaJS.m.sci_Vector$();
    return this$1.NIL$6
  }
});
ScalaJS.c.sci_Vector.prototype.toCollection__O__sc_Seq = (function(repr) {
  return ScalaJS.as.sc_IndexedSeq(repr)
});
ScalaJS.c.sci_Vector.prototype.copyRight__p4__AO__I__AO = (function(array, left) {
  var a2 = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [array.u["length"]]);
  var length = ((a2.u["length"] - left) | 0);
  ScalaJS.systemArraycopy(array, left, a2, left, length);
  return a2
});
ScalaJS.c.sci_Vector.prototype.display3$und$eq__AO__V = (function(x$1) {
  this.display3$4 = x$1
});
ScalaJS.is.sci_Vector = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_Vector)))
});
ScalaJS.as.sci_Vector = (function(obj) {
  return ((ScalaJS.is.sci_Vector(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.Vector"))
});
ScalaJS.isArrayOf.sci_Vector = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_Vector)))
});
ScalaJS.asArrayOf.sci_Vector = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_Vector(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.Vector;", depth))
});
ScalaJS.d.sci_Vector = new ScalaJS.ClassTypeData({
  sci_Vector: 0
}, false, "scala.collection.immutable.Vector", {
  sci_Vector: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_IndexedSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  sci_VectorPointer: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  sc_CustomParallelizable: 1
});
ScalaJS.c.sci_Vector.prototype.$classData = ScalaJS.d.sci_Vector;
/** @constructor */
ScalaJS.c.sci_WrappedString = (function() {
  ScalaJS.c.sc_AbstractSeq.call(this);
  this.self$4 = null
});
ScalaJS.c.sci_WrappedString.prototype = new ScalaJS.h.sc_AbstractSeq();
ScalaJS.c.sci_WrappedString.prototype.constructor = ScalaJS.c.sci_WrappedString;
/** @constructor */
ScalaJS.h.sci_WrappedString = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_WrappedString.prototype = ScalaJS.c.sci_WrappedString.prototype;
ScalaJS.c.sci_WrappedString.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sci_WrappedString.prototype.head__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__head__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sci_WrappedString.prototype.apply__I__O = (function(idx) {
  var thiz = this.self$4;
  var c = (65535 & ScalaJS.uI(thiz["charCodeAt"](idx)));
  return new ScalaJS.c.jl_Character().init___C(c)
});
ScalaJS.c.sci_WrappedString.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__lengthCompare__sc_IndexedSeqOptimized__I__I(this, len)
});
ScalaJS.c.sci_WrappedString.prototype.apply__O__O = (function(v1) {
  var n = ScalaJS.uI(v1);
  var thiz = this.self$4;
  var c = (65535 & ScalaJS.uI(thiz["charCodeAt"](n)));
  return new ScalaJS.c.jl_Character().init___C(c)
});
ScalaJS.c.sci_WrappedString.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__sameElements__sc_IndexedSeqOptimized__sc_GenIterable__Z(this, that)
});
ScalaJS.c.sci_WrappedString.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z(this)
});
ScalaJS.c.sci_WrappedString.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sci_WrappedString.prototype.toString__T = (function() {
  return this.self$4
});
ScalaJS.c.sci_WrappedString.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sci_IndexedSeq$()
});
ScalaJS.c.sci_WrappedString.prototype.foreach__F1__V = (function(f) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__foreach__sc_IndexedSeqOptimized__F1__V(this, f)
});
ScalaJS.c.sci_WrappedString.prototype.slice__I__I__O = (function(from, until) {
  return this.slice__I__I__sci_WrappedString(from, until)
});
ScalaJS.c.sci_WrappedString.prototype.reverse__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__reverse__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sci_WrappedString.prototype.iterator__sc_Iterator = (function() {
  var thiz = this.self$4;
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, ScalaJS.uI(thiz["length"]))
});
ScalaJS.c.sci_WrappedString.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_WrappedString.prototype.length__I = (function() {
  var thiz = this.self$4;
  return ScalaJS.uI(thiz["length"])
});
ScalaJS.c.sci_WrappedString.prototype.drop__I__O = (function(n) {
  var thiz = this.self$4;
  var until = ScalaJS.uI(thiz["length"]);
  return this.slice__I__I__sci_WrappedString(n, until)
});
ScalaJS.c.sci_WrappedString.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.sci_WrappedString.prototype.tail__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__tail__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sci_WrappedString.prototype.isDefinedAt__O__Z = (function(x) {
  var idx = ScalaJS.uI(x);
  return ScalaJS.s.sc_GenSeqLike$class__isDefinedAt__sc_GenSeqLike__I__Z(this, idx)
});
ScalaJS.c.sci_WrappedString.prototype.copyToArray__O__I__I__V = (function(xs, start, len) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__copyToArray__sc_IndexedSeqOptimized__O__I__I__V(this, xs, start, len)
});
ScalaJS.c.sci_WrappedString.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
ScalaJS.c.sci_WrappedString.prototype.init___T = (function(self) {
  this.self$4 = self;
  return this
});
ScalaJS.c.sci_WrappedString.prototype.slice__I__I__sci_WrappedString = (function(from, until) {
  var start = ((from < 0) ? 0 : from);
  if ((until <= start)) {
    var jsx$1 = true
  } else {
    var thiz = this.self$4;
    var jsx$1 = (start >= ScalaJS.uI(thiz["length"]))
  };
  if (jsx$1) {
    return new ScalaJS.c.sci_WrappedString().init___T("")
  };
  var thiz$1 = this.self$4;
  if ((until > ScalaJS.uI(thiz$1["length"]))) {
    var thiz$2 = this.self$4;
    var end = ScalaJS.uI(thiz$2["length"])
  } else {
    var end = until
  };
  var thiz$3 = ScalaJS.m.s_Predef$().unwrapString__sci_WrappedString__T(this);
  return new ScalaJS.c.sci_WrappedString().init___T(ScalaJS.as.T(thiz$3["substring"](start, end)))
});
ScalaJS.c.sci_WrappedString.prototype.toCollection__O__sc_Seq = (function(repr) {
  var repr$1 = ScalaJS.as.sci_WrappedString(repr);
  return repr$1
});
ScalaJS.c.sci_WrappedString.prototype.newBuilder__scm_Builder = (function() {
  return ScalaJS.m.sci_WrappedString$().newBuilder__scm_Builder()
});
ScalaJS.is.sci_WrappedString = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_WrappedString)))
});
ScalaJS.as.sci_WrappedString = (function(obj) {
  return ((ScalaJS.is.sci_WrappedString(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.WrappedString"))
});
ScalaJS.isArrayOf.sci_WrappedString = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_WrappedString)))
});
ScalaJS.asArrayOf.sci_WrappedString = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_WrappedString(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.WrappedString;", depth))
});
ScalaJS.d.sci_WrappedString = new ScalaJS.ClassTypeData({
  sci_WrappedString: 0
}, false, "scala.collection.immutable.WrappedString", {
  sci_WrappedString: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_IndexedSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  sci_StringLike: 1,
  sc_IndexedSeqOptimized: 1,
  s_math_Ordered: 1,
  jl_Comparable: 1
});
ScalaJS.c.sci_WrappedString.prototype.$classData = ScalaJS.d.sci_WrappedString;
/** @constructor */
ScalaJS.c.scm_Stack = (function() {
  ScalaJS.c.scm_AbstractSeq.call(this);
  this.elems$5 = null
});
ScalaJS.c.scm_Stack.prototype = new ScalaJS.h.scm_AbstractSeq();
ScalaJS.c.scm_Stack.prototype.constructor = ScalaJS.c.scm_Stack;
/** @constructor */
ScalaJS.h.scm_Stack = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_Stack.prototype = ScalaJS.c.scm_Stack.prototype;
ScalaJS.c.scm_Stack.prototype.init___ = (function() {
  ScalaJS.c.scm_Stack.prototype.init___sci_List.call(this, ScalaJS.m.sci_Nil$());
  return this
});
ScalaJS.c.scm_Stack.prototype.apply__I__O = (function(index) {
  var this$1 = this.elems$5;
  return ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this$1, index)
});
ScalaJS.c.scm_Stack.prototype.apply__O__O = (function(v1) {
  var index = ScalaJS.uI(v1);
  var this$1 = this.elems$5;
  return ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this$1, index)
});
ScalaJS.c.scm_Stack.prototype.isEmpty__Z = (function() {
  return this.elems$5.isEmpty__Z()
});
ScalaJS.c.scm_Stack.prototype.toList__sci_List = (function() {
  return this.elems$5
});
ScalaJS.c.scm_Stack.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.scm_Stack.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.scm_Stack$()
});
ScalaJS.c.scm_Stack.prototype.foreach__F1__V = (function(f) {
  var this$1 = this.elems$5;
  var this$2 = new ScalaJS.c.sc_LinearSeqLike$$anon$1().init___sc_LinearSeqLike(this$1);
  ScalaJS.s.sc_Iterator$class__foreach__sc_Iterator__F1__V(this$2, f)
});
ScalaJS.c.scm_Stack.prototype.pop__O = (function() {
  var res = this.elems$5.head__O();
  this.elems$5 = ScalaJS.as.sci_List(this.elems$5.tail__O());
  return res
});
ScalaJS.c.scm_Stack.prototype.iterator__sc_Iterator = (function() {
  var this$1 = this.elems$5;
  return new ScalaJS.c.sc_LinearSeqLike$$anon$1().init___sc_LinearSeqLike(this$1)
});
ScalaJS.c.scm_Stack.prototype.push__O__scm_Stack = (function(elem) {
  var this$1 = this.elems$5;
  this.elems$5 = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(elem, this$1);
  return this
});
ScalaJS.c.scm_Stack.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.scm_Stack.prototype.length__I = (function() {
  var this$1 = this.elems$5;
  return ScalaJS.s.sc_LinearSeqOptimized$class__length__sc_LinearSeqOptimized__I(this$1)
});
ScalaJS.c.scm_Stack.prototype.init___sci_List = (function(elems) {
  this.elems$5 = elems;
  return this
});
ScalaJS.c.scm_Stack.prototype.isDefinedAt__O__Z = (function(x) {
  var idx = ScalaJS.uI(x);
  return ScalaJS.s.sc_GenSeqLike$class__isDefinedAt__sc_GenSeqLike__I__Z(this, idx)
});
ScalaJS.d.scm_Stack = new ScalaJS.ClassTypeData({
  scm_Stack: 0
}, false, "scala.collection.mutable.Stack", {
  scm_Stack: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_SeqLike: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_Stack.prototype.$classData = ScalaJS.d.scm_Stack;
/** @constructor */
ScalaJS.c.sci_$colon$colon = (function() {
  ScalaJS.c.sci_List.call(this);
  this.head$5 = null;
  this.tl$5 = null
});
ScalaJS.c.sci_$colon$colon.prototype = new ScalaJS.h.sci_List();
ScalaJS.c.sci_$colon$colon.prototype.constructor = ScalaJS.c.sci_$colon$colon;
/** @constructor */
ScalaJS.h.sci_$colon$colon = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_$colon$colon.prototype = ScalaJS.c.sci_$colon$colon.prototype;
ScalaJS.c.sci_$colon$colon.prototype.productPrefix__T = (function() {
  return "::"
});
ScalaJS.c.sci_$colon$colon.prototype.head__O = (function() {
  return this.head$5
});
ScalaJS.c.sci_$colon$colon.prototype.productArity__I = (function() {
  return 2
});
ScalaJS.c.sci_$colon$colon.prototype.isEmpty__Z = (function() {
  return false
});
ScalaJS.c.sci_$colon$colon.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0:
      {
        return this.head$5;
        break
      };
    case 1:
      {
        return this.tl$5;
        break
      };
    default:
      throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + x$1));
  }
});
ScalaJS.c.sci_$colon$colon.prototype.tail__O = (function() {
  return this.tl$5
});
ScalaJS.c.sci_$colon$colon.prototype.init___O__sci_List = (function(head, tl) {
  this.head$5 = head;
  this.tl$5 = tl;
  return this
});
ScalaJS.c.sci_$colon$colon.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.is.sci_$colon$colon = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_$colon$colon)))
});
ScalaJS.as.sci_$colon$colon = (function(obj) {
  return ((ScalaJS.is.sci_$colon$colon(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.immutable.$colon$colon"))
});
ScalaJS.isArrayOf.sci_$colon$colon = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_$colon$colon)))
});
ScalaJS.asArrayOf.sci_$colon$colon = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.sci_$colon$colon(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.immutable.$colon$colon;", depth))
});
ScalaJS.d.sci_$colon$colon = new ScalaJS.ClassTypeData({
  sci_$colon$colon: 0
}, false, "scala.collection.immutable.$colon$colon", {
  sci_$colon$colon: 1,
  sci_List: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_LinearSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_LinearSeq: 1,
  sc_LinearSeqLike: 1,
  s_Product: 1,
  sc_LinearSeqOptimized: 1,
  Ljava_io_Serializable: 1,
  s_Serializable: 1
});
ScalaJS.c.sci_$colon$colon.prototype.$classData = ScalaJS.d.sci_$colon$colon;
/** @constructor */
ScalaJS.c.sci_Nil$ = (function() {
  ScalaJS.c.sci_List.call(this)
});
ScalaJS.c.sci_Nil$.prototype = new ScalaJS.h.sci_List();
ScalaJS.c.sci_Nil$.prototype.constructor = ScalaJS.c.sci_Nil$;
/** @constructor */
ScalaJS.h.sci_Nil$ = (function() {
  /*<skip>*/
});
ScalaJS.h.sci_Nil$.prototype = ScalaJS.c.sci_Nil$.prototype;
ScalaJS.c.sci_Nil$.prototype.head__O = (function() {
  this.head__sr_Nothing$()
});
ScalaJS.c.sci_Nil$.prototype.productPrefix__T = (function() {
  return "Nil"
});
ScalaJS.c.sci_Nil$.prototype.productArity__I = (function() {
  return 0
});
ScalaJS.c.sci_Nil$.prototype.equals__O__Z = (function(that) {
  if (ScalaJS.is.sc_GenSeq(that)) {
    var x2 = ScalaJS.as.sc_GenSeq(that);
    return x2.isEmpty__Z()
  } else {
    return false
  }
});
ScalaJS.c.sci_Nil$.prototype.tail__sci_List = (function() {
  throw new ScalaJS.c.jl_UnsupportedOperationException().init___T("tail of empty list")
});
ScalaJS.c.sci_Nil$.prototype.isEmpty__Z = (function() {
  return true
});
ScalaJS.c.sci_Nil$.prototype.productElement__I__O = (function(x$1) {
  matchEnd3: {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + x$1))
  }
});
ScalaJS.c.sci_Nil$.prototype.head__sr_Nothing$ = (function() {
  throw new ScalaJS.c.ju_NoSuchElementException().init___T("head of empty list")
});
ScalaJS.c.sci_Nil$.prototype.tail__O = (function() {
  return this.tail__sci_List()
});
ScalaJS.c.sci_Nil$.prototype.productIterator__sc_Iterator = (function() {
  return new ScalaJS.c.sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
ScalaJS.d.sci_Nil$ = new ScalaJS.ClassTypeData({
  sci_Nil$: 0
}, false, "scala.collection.immutable.Nil$", {
  sci_Nil$: 1,
  sci_List: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_LinearSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_LinearSeq: 1,
  sc_LinearSeqLike: 1,
  s_Product: 1,
  sc_LinearSeqOptimized: 1,
  Ljava_io_Serializable: 1,
  s_Serializable: 1
});
ScalaJS.c.sci_Nil$.prototype.$classData = ScalaJS.d.sci_Nil$;
ScalaJS.n.sci_Nil$ = (void 0);
ScalaJS.m.sci_Nil$ = (function() {
  if ((!ScalaJS.n.sci_Nil$)) {
    ScalaJS.n.sci_Nil$ = new ScalaJS.c.sci_Nil$().init___()
  };
  return ScalaJS.n.sci_Nil$
});
/** @constructor */
ScalaJS.c.scm_AbstractMap = (function() {
  ScalaJS.c.sc_AbstractMap.call(this)
});
ScalaJS.c.scm_AbstractMap.prototype = new ScalaJS.h.sc_AbstractMap();
ScalaJS.c.scm_AbstractMap.prototype.constructor = ScalaJS.c.scm_AbstractMap;
/** @constructor */
ScalaJS.h.scm_AbstractMap = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_AbstractMap.prototype = ScalaJS.c.scm_AbstractMap.prototype;
ScalaJS.c.scm_AbstractMap.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.scm_Iterable$()
});
ScalaJS.c.scm_AbstractMap.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_AbstractMap.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_AbstractMap.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
ScalaJS.c.scm_AbstractMap.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_HashMap().init___()
});
/** @constructor */
ScalaJS.c.scm_AbstractSet = (function() {
  ScalaJS.c.scm_AbstractIterable.call(this)
});
ScalaJS.c.scm_AbstractSet.prototype = new ScalaJS.h.scm_AbstractIterable();
ScalaJS.c.scm_AbstractSet.prototype.constructor = ScalaJS.c.scm_AbstractSet;
/** @constructor */
ScalaJS.h.scm_AbstractSet = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_AbstractSet.prototype = ScalaJS.c.scm_AbstractSet.prototype;
ScalaJS.c.scm_AbstractSet.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_SetLike$class__isEmpty__sc_SetLike__Z(this)
});
ScalaJS.c.scm_AbstractSet.prototype.equals__O__Z = (function(that) {
  return ScalaJS.s.sc_GenSetLike$class__equals__sc_GenSetLike__O__Z(this, that)
});
ScalaJS.c.scm_AbstractSet.prototype.toString__T = (function() {
  return ScalaJS.s.sc_TraversableLike$class__toString__sc_TraversableLike__T(this)
});
ScalaJS.c.scm_AbstractSet.prototype.subsetOf__sc_GenSet__Z = (function(that) {
  var this$1 = new ScalaJS.c.scm_FlatHashTable$$anon$1().init___scm_FlatHashTable(this);
  return ScalaJS.s.sc_Iterator$class__forall__sc_Iterator__F1__Z(this$1, that)
});
ScalaJS.c.scm_AbstractSet.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_AbstractSet.prototype.hashCode__I = (function() {
  var this$1 = ScalaJS.m.s_util_hashing_MurmurHash3$();
  return this$1.unorderedHash__sc_TraversableOnce__I__I(this, this$1.setSeed$2)
});
ScalaJS.c.scm_AbstractSet.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_AbstractSet.prototype.stringPrefix__T = (function() {
  return "Set"
});
ScalaJS.c.scm_AbstractSet.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_HashSet().init___()
});
ScalaJS.c.scm_AbstractSet.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
/** @constructor */
ScalaJS.c.scm_AbstractBuffer = (function() {
  ScalaJS.c.scm_AbstractSeq.call(this)
});
ScalaJS.c.scm_AbstractBuffer.prototype = new ScalaJS.h.scm_AbstractSeq();
ScalaJS.c.scm_AbstractBuffer.prototype.constructor = ScalaJS.c.scm_AbstractBuffer;
/** @constructor */
ScalaJS.h.scm_AbstractBuffer = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_AbstractBuffer.prototype = ScalaJS.c.scm_AbstractBuffer.prototype;
ScalaJS.c.scm_AbstractBuffer.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
/** @constructor */
ScalaJS.c.scm_HashMap = (function() {
  ScalaJS.c.scm_AbstractMap.call(this);
  this.$$undloadFactor$5 = 0;
  this.table$5 = null;
  this.tableSize$5 = 0;
  this.threshold$5 = 0;
  this.sizemap$5 = null;
  this.seedvalue$5 = 0
});
ScalaJS.c.scm_HashMap.prototype = new ScalaJS.h.scm_AbstractMap();
ScalaJS.c.scm_HashMap.prototype.constructor = ScalaJS.c.scm_HashMap;
/** @constructor */
ScalaJS.h.scm_HashMap = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_HashMap.prototype = ScalaJS.c.scm_HashMap.prototype;
ScalaJS.c.scm_HashMap.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.scm_HashMap.prototype.init___ = (function() {
  ScalaJS.c.scm_HashMap.prototype.init___scm_HashTable$Contents.call(this, null);
  return this
});
ScalaJS.c.scm_HashMap.prototype.apply__O__O = (function(key) {
  var result = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findEntry__scm_HashTable__O__scm_HashEntry(this, key));
  return ((result === null) ? ScalaJS.s.sc_MapLike$class__$default__sc_MapLike__O__O(this, key) : result.value$1)
});
ScalaJS.c.scm_HashMap.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.scm_HashMap.prototype.remove__O__s_Option = (function(key) {
  var e = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__removeEntry__scm_HashTable__O__scm_HashEntry(this, key));
  return ((e !== null) ? new ScalaJS.c.s_Some().init___O(e.value$1) : ScalaJS.m.s_None$())
});
ScalaJS.c.scm_HashMap.prototype.$$plus$eq__T2__scm_HashMap = (function(kv) {
  var key = kv.$$und1__O();
  var value = kv.$$und2__O();
  var e = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findOrAddEntry__scm_HashTable__O__O__scm_HashEntry(this, key, value));
  if ((e !== null)) {
    e.value$1 = kv.$$und2__O()
  };
  return this
});
ScalaJS.c.scm_HashMap.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__T2__scm_HashMap(ScalaJS.as.T2(elem))
});
ScalaJS.c.scm_HashMap.prototype.foreach__F1__V = (function(f) {
  var iterTable = this.table$5;
  var idx = ScalaJS.s.scm_HashTable$class__scala$collection$mutable$HashTable$$lastPopulatedIndex__scm_HashTable__I(this);
  var es = iterTable.u[idx];
  while ((es !== null)) {
    var arg1 = es;
    var e = ScalaJS.as.scm_DefaultEntry(arg1);
    f.apply__O__O(new ScalaJS.c.T2().init___O__O(e.key$1, e.value$1));
    es = ScalaJS.as.scm_HashEntry(es.next$1);
    while (((es === null) && (idx > 0))) {
      idx = (((-1) + idx) | 0);
      es = iterTable.u[idx]
    }
  }
});
ScalaJS.c.scm_HashMap.prototype.empty__sc_Map = (function() {
  return new ScalaJS.c.scm_HashMap().init___()
});
ScalaJS.c.scm_HashMap.prototype.size__I = (function() {
  return this.tableSize$5
});
ScalaJS.c.scm_HashMap.prototype.seq__sc_Map = (function() {
  return this
});
ScalaJS.c.scm_HashMap.prototype.result__O = (function() {
  return this
});
ScalaJS.c.scm_HashMap.prototype.iterator__sc_Iterator = (function() {
  var this$1 = new ScalaJS.c.scm_HashTable$$anon$1().init___scm_HashTable(this);
  var f = new ScalaJS.c.sjsr_AnonFunction1().init___sjs_js_Function1((function(this$2) {
    return (function(e$2) {
      var e = ScalaJS.as.scm_DefaultEntry(e$2);
      return new ScalaJS.c.T2().init___O__O(e.key$1, e.value$1)
    })
  })(this));
  return new ScalaJS.c.sc_Iterator$$anon$11().init___sc_Iterator__F1(this$1, f)
});
ScalaJS.c.scm_HashMap.prototype.init___scm_HashTable$Contents = (function(contents) {
  ScalaJS.s.scm_HashTable$class__$$init$__scm_HashTable__V(this);
  ScalaJS.s.scm_HashTable$class__initWithContents__scm_HashTable__scm_HashTable$Contents__V(this, contents);
  return this
});
ScalaJS.c.scm_HashMap.prototype.get__O__s_Option = (function(key) {
  var e = ScalaJS.as.scm_DefaultEntry(ScalaJS.s.scm_HashTable$class__findEntry__scm_HashTable__O__scm_HashEntry(this, key));
  return ((e === null) ? ScalaJS.m.s_None$() : new ScalaJS.c.s_Some().init___O(e.value$1))
});
ScalaJS.c.scm_HashMap.prototype.contains__O__Z = (function(key) {
  return (ScalaJS.s.scm_HashTable$class__findEntry__scm_HashTable__O__scm_HashEntry(this, key) !== null)
});
ScalaJS.c.scm_HashMap.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__T2__scm_HashMap(ScalaJS.as.T2(elem))
});
ScalaJS.c.scm_HashMap.prototype.$$plus__T2__sc_GenMap = (function(kv) {
  var this$2 = new ScalaJS.c.scm_HashMap().init___();
  var this$3 = ScalaJS.as.scm_Map(ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this$2, this));
  return this$3.$$plus$eq__T2__scm_HashMap(kv)
});
ScalaJS.is.scm_HashMap = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_HashMap)))
});
ScalaJS.as.scm_HashMap = (function(obj) {
  return ((ScalaJS.is.scm_HashMap(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.HashMap"))
});
ScalaJS.isArrayOf.scm_HashMap = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_HashMap)))
});
ScalaJS.asArrayOf.scm_HashMap = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_HashMap(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.HashMap;", depth))
});
ScalaJS.d.scm_HashMap = new ScalaJS.ClassTypeData({
  scm_HashMap: 0
}, false, "scala.collection.mutable.HashMap", {
  scm_HashMap: 1,
  scm_AbstractMap: 1,
  sc_AbstractMap: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Map: 1,
  sc_GenMap: 1,
  sc_GenMapLike: 1,
  sc_MapLike: 1,
  s_PartialFunction: 1,
  F1: 1,
  scg_Subtractable: 1,
  scm_Map: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_MapLike: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  scg_Shrinkable: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  scm_HashTable: 1,
  scm_HashTable$HashUtils: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_HashMap.prototype.$classData = ScalaJS.d.scm_HashMap;
/** @constructor */
ScalaJS.c.scm_HashSet = (function() {
  ScalaJS.c.scm_AbstractSet.call(this);
  this.$$undloadFactor$5 = 0;
  this.table$5 = null;
  this.tableSize$5 = 0;
  this.threshold$5 = 0;
  this.sizemap$5 = null;
  this.seedvalue$5 = 0
});
ScalaJS.c.scm_HashSet.prototype = new ScalaJS.h.scm_AbstractSet();
ScalaJS.c.scm_HashSet.prototype.constructor = ScalaJS.c.scm_HashSet;
/** @constructor */
ScalaJS.h.scm_HashSet = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_HashSet.prototype = ScalaJS.c.scm_HashSet.prototype;
ScalaJS.c.scm_HashSet.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.scm_HashSet.prototype.init___ = (function() {
  ScalaJS.c.scm_HashSet.prototype.init___scm_FlatHashTable$Contents.call(this, null);
  return this
});
ScalaJS.c.scm_HashSet.prototype.apply__O__O = (function(v1) {
  return ScalaJS.s.scm_FlatHashTable$class__containsElem__scm_FlatHashTable__O__Z(this, v1)
});
ScalaJS.c.scm_HashSet.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.scm_HashSet.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_HashSet(elem)
});
ScalaJS.c.scm_HashSet.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.scm_HashSet$()
});
ScalaJS.c.scm_HashSet.prototype.foreach__F1__V = (function(f) {
  var i = 0;
  var len = this.table$5.u["length"];
  while ((i < len)) {
    var curEntry = this.table$5.u[i];
    if ((curEntry !== null)) {
      f.apply__O__O(ScalaJS.s.scm_FlatHashTable$HashUtils$class__entryToElem__scm_FlatHashTable$HashUtils__O__O(this, curEntry))
    };
    i = ((1 + i) | 0)
  }
});
ScalaJS.c.scm_HashSet.prototype.size__I = (function() {
  return this.tableSize$5
});
ScalaJS.c.scm_HashSet.prototype.result__O = (function() {
  return this
});
ScalaJS.c.scm_HashSet.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.scm_FlatHashTable$$anon$1().init___scm_FlatHashTable(this)
});
ScalaJS.c.scm_HashSet.prototype.init___scm_FlatHashTable$Contents = (function(contents) {
  ScalaJS.s.scm_FlatHashTable$class__$$init$__scm_FlatHashTable__V(this);
  ScalaJS.s.scm_FlatHashTable$class__initWithContents__scm_FlatHashTable__scm_FlatHashTable$Contents__V(this, contents);
  return this
});
ScalaJS.c.scm_HashSet.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_HashSet(elem)
});
ScalaJS.c.scm_HashSet.prototype.$$plus__O__sc_Set = (function(elem) {
  var this$1 = new ScalaJS.c.scm_HashSet().init___();
  var this$2 = ScalaJS.as.scm_HashSet(ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this$1, this));
  return this$2.$$plus$eq__O__scm_HashSet(elem)
});
ScalaJS.c.scm_HashSet.prototype.$$plus$eq__O__scm_HashSet = (function(elem) {
  ScalaJS.s.scm_FlatHashTable$class__addElem__scm_FlatHashTable__O__Z(this, elem);
  return this
});
ScalaJS.is.scm_HashSet = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_HashSet)))
});
ScalaJS.as.scm_HashSet = (function(obj) {
  return ((ScalaJS.is.scm_HashSet(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.HashSet"))
});
ScalaJS.isArrayOf.scm_HashSet = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_HashSet)))
});
ScalaJS.asArrayOf.scm_HashSet = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_HashSet(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.HashSet;", depth))
});
ScalaJS.d.scm_HashSet = new ScalaJS.ClassTypeData({
  scm_HashSet: 0
}, false, "scala.collection.mutable.HashSet", {
  scm_HashSet: 1,
  scm_AbstractSet: 1,
  scm_AbstractIterable: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_Set: 1,
  sc_Set: 1,
  F1: 1,
  sc_GenSet: 1,
  sc_GenSetLike: 1,
  scg_GenericSetTemplate: 1,
  sc_SetLike: 1,
  scg_Subtractable: 1,
  scm_SetLike: 1,
  sc_script_Scriptable: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  scg_Shrinkable: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  scm_FlatHashTable: 1,
  scm_FlatHashTable$HashUtils: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_HashSet.prototype.$classData = ScalaJS.d.scm_HashSet;
/** @constructor */
ScalaJS.c.scm_ListBuffer = (function() {
  ScalaJS.c.scm_AbstractBuffer.call(this);
  this.scala$collection$mutable$ListBuffer$$start$6 = null;
  this.last0$6 = null;
  this.exported$6 = false;
  this.len$6 = 0
});
ScalaJS.c.scm_ListBuffer.prototype = new ScalaJS.h.scm_AbstractBuffer();
ScalaJS.c.scm_ListBuffer.prototype.constructor = ScalaJS.c.scm_ListBuffer;
/** @constructor */
ScalaJS.h.scm_ListBuffer = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_ListBuffer.prototype = ScalaJS.c.scm_ListBuffer.prototype;
ScalaJS.c.scm_ListBuffer.prototype.copy__p6__V = (function() {
  if (this.scala$collection$mutable$ListBuffer$$start$6.isEmpty__Z()) {
    return (void 0)
  };
  var cursor = this.scala$collection$mutable$ListBuffer$$start$6;
  var this$1 = this.last0$6;
  var limit = this$1.tl$5;
  this.clear__V();
  while ((cursor !== limit)) {
    this.$$plus$eq__O__scm_ListBuffer(cursor.head__O());
    cursor = ScalaJS.as.sci_List(cursor.tail__O())
  }
});
ScalaJS.c.scm_ListBuffer.prototype.init___ = (function() {
  this.scala$collection$mutable$ListBuffer$$start$6 = ScalaJS.m.sci_Nil$();
  this.exported$6 = false;
  this.len$6 = 0;
  return this
});
ScalaJS.c.scm_ListBuffer.prototype.head__O = (function() {
  return this.scala$collection$mutable$ListBuffer$$start$6.head__O()
});
ScalaJS.c.scm_ListBuffer.prototype.apply__I__O = (function(n) {
  if (((n < 0) || (n >= this.len$6))) {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + n))
  } else {
    var this$2 = this.scala$collection$mutable$ListBuffer$$start$6;
    return ScalaJS.s.sc_LinearSeqOptimized$class__apply__sc_LinearSeqOptimized__I__O(this$2, n)
  }
});
ScalaJS.c.scm_ListBuffer.prototype.lengthCompare__I__I = (function(len) {
  var this$1 = this.scala$collection$mutable$ListBuffer$$start$6;
  return ScalaJS.s.sc_LinearSeqOptimized$class__lengthCompare__sc_LinearSeqOptimized__I__I(this$1, len)
});
ScalaJS.c.scm_ListBuffer.prototype.apply__O__O = (function(v1) {
  return this.apply__I__O(ScalaJS.uI(v1))
});
ScalaJS.c.scm_ListBuffer.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  var this$1 = this.scala$collection$mutable$ListBuffer$$start$6;
  return ScalaJS.s.sc_LinearSeqOptimized$class__sameElements__sc_LinearSeqOptimized__sc_GenIterable__Z(this$1, that)
});
ScalaJS.c.scm_ListBuffer.prototype.isEmpty__Z = (function() {
  return this.scala$collection$mutable$ListBuffer$$start$6.isEmpty__Z()
});
ScalaJS.c.scm_ListBuffer.prototype.toList__sci_List = (function() {
  this.exported$6 = (!this.scala$collection$mutable$ListBuffer$$start$6.isEmpty__Z());
  return this.scala$collection$mutable$ListBuffer$$start$6
});
ScalaJS.c.scm_ListBuffer.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.scm_ListBuffer.prototype.equals__O__Z = (function(that) {
  if (ScalaJS.is.scm_ListBuffer(that)) {
    var x2 = ScalaJS.as.scm_ListBuffer(that);
    return this.scala$collection$mutable$ListBuffer$$start$6.equals__O__Z(x2.scala$collection$mutable$ListBuffer$$start$6)
  } else {
    return ScalaJS.s.sc_GenSeqLike$class__equals__sc_GenSeqLike__O__Z(this, that)
  }
});
ScalaJS.c.scm_ListBuffer.prototype.mkString__T__T__T__T = (function(start, sep, end) {
  var this$1 = this.scala$collection$mutable$ListBuffer$$start$6;
  return ScalaJS.s.sc_TraversableOnce$class__mkString__sc_TraversableOnce__T__T__T__T(this$1, start, sep, end)
});
ScalaJS.c.scm_ListBuffer.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_ListBuffer(elem)
});
ScalaJS.c.scm_ListBuffer.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.scm_ListBuffer$()
});
ScalaJS.c.scm_ListBuffer.prototype.foreach__F1__V = (function(f) {
  var this$1 = this.scala$collection$mutable$ListBuffer$$start$6;
  var these = this$1;
  while ((!these.isEmpty__Z())) {
    f.apply__O__O(these.head__O());
    these = ScalaJS.as.sci_List(these.tail__O())
  }
});
ScalaJS.c.scm_ListBuffer.prototype.$$plus$eq__O__scm_Buffer = (function(elem) {
  return this.$$plus$eq__O__scm_ListBuffer(elem)
});
ScalaJS.c.scm_ListBuffer.prototype.size__I = (function() {
  return this.len$6
});
ScalaJS.c.scm_ListBuffer.prototype.result__O = (function() {
  return this.toList__sci_List()
});
ScalaJS.c.scm_ListBuffer.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.scm_ListBuffer$$anon$1().init___scm_ListBuffer(this)
});
ScalaJS.c.scm_ListBuffer.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_ListBuffer.prototype.length__I = (function() {
  return this.len$6
});
ScalaJS.c.scm_ListBuffer.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.scm_ListBuffer.prototype.remove__I__O = (function(n) {
  if (((n < 0) || (n >= this.len$6))) {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + n))
  };
  if (this.exported$6) {
    this.copy__p6__V()
  };
  var old = this.scala$collection$mutable$ListBuffer$$start$6.head__O();
  if ((n === 0)) {
    this.scala$collection$mutable$ListBuffer$$start$6 = ScalaJS.as.sci_List(this.scala$collection$mutable$ListBuffer$$start$6.tail__O())
  } else {
    var cursor = this.scala$collection$mutable$ListBuffer$$start$6;
    var i = 1;
    while ((i < n)) {
      cursor = ScalaJS.as.sci_List(cursor.tail__O());
      i = ((1 + i) | 0)
    };
    old = ScalaJS.as.sc_IterableLike(cursor.tail__O()).head__O();
    if ((this.last0$6 === cursor.tail__O())) {
      this.last0$6 = ScalaJS.as.sci_$colon$colon(cursor)
    };
    ScalaJS.as.sci_$colon$colon(cursor).tl$5 = ScalaJS.as.sci_List(ScalaJS.as.sc_TraversableLike(cursor.tail__O()).tail__O())
  };
  this.reduceLengthBy__p6__I__V(1);
  return old
});
ScalaJS.c.scm_ListBuffer.prototype.toStream__sci_Stream = (function() {
  return this.scala$collection$mutable$ListBuffer$$start$6.toStream__sci_Stream()
});
ScalaJS.c.scm_ListBuffer.prototype.reduceLengthBy__p6__I__V = (function(num) {
  this.len$6 = ((this.len$6 - num) | 0);
  if ((this.len$6 <= 0)) {
    this.last0$6 = null
  }
});
ScalaJS.c.scm_ListBuffer.prototype.prependToList__sci_List__sci_List = (function(xs) {
  if (this.scala$collection$mutable$ListBuffer$$start$6.isEmpty__Z()) {
    return xs
  } else {
    if (this.exported$6) {
      this.copy__p6__V()
    };
    this.last0$6.tl$5 = xs;
    return this.toList__sci_List()
  }
});
ScalaJS.c.scm_ListBuffer.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  var this$1 = this.scala$collection$mutable$ListBuffer$$start$6;
  return ScalaJS.s.sc_TraversableOnce$class__addString__sc_TraversableOnce__scm_StringBuilder__T__T__T__scm_StringBuilder(this$1, b, start, sep, end)
});
ScalaJS.c.scm_ListBuffer.prototype.$$plus$eq__O__scm_ListBuffer = (function(x) {
  if (this.exported$6) {
    this.copy__p6__V()
  };
  if (this.scala$collection$mutable$ListBuffer$$start$6.isEmpty__Z()) {
    this.last0$6 = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(x, ScalaJS.m.sci_Nil$());
    this.scala$collection$mutable$ListBuffer$$start$6 = this.last0$6
  } else {
    var last1 = this.last0$6;
    this.last0$6 = new ScalaJS.c.sci_$colon$colon().init___O__sci_List(x, ScalaJS.m.sci_Nil$());
    last1.tl$5 = this.last0$6
  };
  this.len$6 = ((1 + this.len$6) | 0);
  return this
});
ScalaJS.c.scm_ListBuffer.prototype.isDefinedAt__O__Z = (function(x) {
  var x$1 = ScalaJS.uI(x);
  var this$1 = this.scala$collection$mutable$ListBuffer$$start$6;
  return ScalaJS.s.sc_LinearSeqOptimized$class__isDefinedAt__sc_LinearSeqOptimized__I__Z(this$1, x$1)
});
ScalaJS.c.scm_ListBuffer.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_ListBuffer(elem)
});
ScalaJS.c.scm_ListBuffer.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_ListBuffer.prototype.clear__V = (function() {
  this.scala$collection$mutable$ListBuffer$$start$6 = ScalaJS.m.sci_Nil$();
  this.last0$6 = null;
  this.exported$6 = false;
  this.len$6 = 0
});
ScalaJS.c.scm_ListBuffer.prototype.$$plus$plus$eq__sc_TraversableOnce__scm_ListBuffer = (function(xs) {
  _$plus$plus$eq: while (true) {
    var x1 = xs;
    if ((x1 !== null)) {
      if ((x1 === this)) {
        var n = this.len$6;
        xs = ScalaJS.as.sc_TraversableOnce(ScalaJS.s.sc_IterableLike$class__take__sc_IterableLike__I__O(this, n));
        continue _$plus$plus$eq
      }
    };
    return ScalaJS.as.scm_ListBuffer(ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs))
  }
});
ScalaJS.c.scm_ListBuffer.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return this.$$plus$plus$eq__sc_TraversableOnce__scm_ListBuffer(xs)
});
ScalaJS.c.scm_ListBuffer.prototype.stringPrefix__T = (function() {
  return "ListBuffer"
});
ScalaJS.is.scm_ListBuffer = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_ListBuffer)))
});
ScalaJS.as.scm_ListBuffer = (function(obj) {
  return ((ScalaJS.is.scm_ListBuffer(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.ListBuffer"))
});
ScalaJS.isArrayOf.scm_ListBuffer = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ListBuffer)))
});
ScalaJS.asArrayOf.scm_ListBuffer = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_ListBuffer(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.ListBuffer;", depth))
});
ScalaJS.d.scm_ListBuffer = new ScalaJS.ClassTypeData({
  scm_ListBuffer: 0
}, false, "scala.collection.mutable.ListBuffer", {
  scm_ListBuffer: 1,
  scm_AbstractBuffer: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_SeqLike: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  scm_Buffer: 1,
  scm_BufferLike: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  scg_Shrinkable: 1,
  sc_script_Scriptable: 1,
  scg_Subtractable: 1,
  scm_Builder: 1,
  scg_SeqForwarder: 1,
  scg_IterableForwarder: 1,
  scg_TraversableForwarder: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_ListBuffer.prototype.$classData = ScalaJS.d.scm_ListBuffer;
/** @constructor */
ScalaJS.c.scm_StringBuilder = (function() {
  ScalaJS.c.scm_AbstractSeq.call(this);
  this.underlying$5 = null
});
ScalaJS.c.scm_StringBuilder.prototype = new ScalaJS.h.scm_AbstractSeq();
ScalaJS.c.scm_StringBuilder.prototype.constructor = ScalaJS.c.scm_StringBuilder;
/** @constructor */
ScalaJS.h.scm_StringBuilder = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_StringBuilder.prototype = ScalaJS.c.scm_StringBuilder.prototype;
ScalaJS.c.scm_StringBuilder.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.$$plus$eq__C__scm_StringBuilder = (function(x) {
  this.append__C__scm_StringBuilder(x);
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.head__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__head__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.scm_StringBuilder.prototype.init___ = (function() {
  ScalaJS.c.scm_StringBuilder.prototype.init___I__T.call(this, 16, "");
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.apply__I__O = (function(idx) {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  var c = (65535 & ScalaJS.uI(thiz["charCodeAt"](idx)));
  return new ScalaJS.c.jl_Character().init___C(c)
});
ScalaJS.c.scm_StringBuilder.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__lengthCompare__sc_IndexedSeqOptimized__I__I(this, len)
});
ScalaJS.c.scm_StringBuilder.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__sameElements__sc_IndexedSeqOptimized__sc_GenIterable__Z(this, that)
});
ScalaJS.c.scm_StringBuilder.prototype.apply__O__O = (function(v1) {
  var index = ScalaJS.uI(v1);
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  var c = (65535 & ScalaJS.uI(thiz["charCodeAt"](index)));
  return new ScalaJS.c.jl_Character().init___C(c)
});
ScalaJS.c.scm_StringBuilder.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z(this)
});
ScalaJS.c.scm_StringBuilder.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.subSequence__I__I__jl_CharSequence = (function(start, end) {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  return ScalaJS.as.T(thiz["substring"](start, end))
});
ScalaJS.c.scm_StringBuilder.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  if ((elem === null)) {
    var jsx$1 = 0
  } else {
    var this$2 = ScalaJS.as.jl_Character(elem);
    var jsx$1 = this$2.value$1
  };
  return this.$$plus$eq__C__scm_StringBuilder(jsx$1)
});
ScalaJS.c.scm_StringBuilder.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.scm_IndexedSeq$()
});
ScalaJS.c.scm_StringBuilder.prototype.toString__T = (function() {
  var this$1 = this.underlying$5;
  return this$1.content$1
});
ScalaJS.c.scm_StringBuilder.prototype.foreach__F1__V = (function(f) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__foreach__sc_IndexedSeqOptimized__F1__V(this, f)
});
ScalaJS.c.scm_StringBuilder.prototype.slice__I__I__O = (function(from, until) {
  return ScalaJS.s.sci_StringLike$class__slice__sci_StringLike__I__I__O(this, from, until)
});
ScalaJS.c.scm_StringBuilder.prototype.reverse__O = (function() {
  return this.reverse__scm_StringBuilder()
});
ScalaJS.c.scm_StringBuilder.prototype.result__O = (function() {
  var this$1 = this.underlying$5;
  return this$1.content$1
});
ScalaJS.c.scm_StringBuilder.prototype.append__T__scm_StringBuilder = (function(s) {
  this.underlying$5.append__T__jl_StringBuilder(s);
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.seq__scm_Seq = (function() {
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.iterator__sc_Iterator = (function() {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, ScalaJS.uI(thiz["length"]))
});
ScalaJS.c.scm_StringBuilder.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_StringBuilder.prototype.init___I__T = (function(initCapacity, initValue) {
  ScalaJS.c.scm_StringBuilder.prototype.init___jl_StringBuilder.call(this, new ScalaJS.c.jl_StringBuilder().init___I(((ScalaJS.uI(initValue["length"]) + initCapacity) | 0)).append__T__jl_StringBuilder(initValue));
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.length__I = (function() {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  return ScalaJS.uI(thiz["length"])
});
ScalaJS.c.scm_StringBuilder.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.drop__I__O = (function(n) {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  var until = ScalaJS.uI(thiz["length"]);
  return ScalaJS.s.sci_StringLike$class__slice__sci_StringLike__I__I__O(this, n, until)
});
ScalaJS.c.scm_StringBuilder.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.tail__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__tail__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.scm_StringBuilder.prototype.init___jl_StringBuilder = (function(underlying) {
  this.underlying$5 = underlying;
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.append__O__scm_StringBuilder = (function(x) {
  this.underlying$5.append__T__jl_StringBuilder(ScalaJS.m.sjsr_RuntimeString$().valueOf__O__T(x));
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.isDefinedAt__O__Z = (function(x) {
  var idx = ScalaJS.uI(x);
  return ScalaJS.s.sc_GenSeqLike$class__isDefinedAt__sc_GenSeqLike__I__Z(this, idx)
});
ScalaJS.c.scm_StringBuilder.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  if ((elem === null)) {
    var jsx$1 = 0
  } else {
    var this$2 = ScalaJS.as.jl_Character(elem);
    var jsx$1 = this$2.value$1
  };
  return this.$$plus$eq__C__scm_StringBuilder(jsx$1)
});
ScalaJS.c.scm_StringBuilder.prototype.copyToArray__O__I__I__V = (function(xs, start, len) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__copyToArray__sc_IndexedSeqOptimized__O__I__I__V(this, xs, start, len)
});
ScalaJS.c.scm_StringBuilder.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.scm_StringBuilder.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
ScalaJS.c.scm_StringBuilder.prototype.reverse__scm_StringBuilder = (function() {
  return new ScalaJS.c.scm_StringBuilder().init___jl_StringBuilder(new ScalaJS.c.jl_StringBuilder().init___jl_CharSequence(this.underlying$5).reverse__jl_StringBuilder())
});
ScalaJS.c.scm_StringBuilder.prototype.append__C__scm_StringBuilder = (function(x) {
  this.underlying$5.append__C__jl_StringBuilder(x);
  return this
});
ScalaJS.c.scm_StringBuilder.prototype.toCollection__O__sc_Seq = (function(repr) {
  var repr$1 = ScalaJS.as.scm_StringBuilder(repr);
  return repr$1
});
ScalaJS.c.scm_StringBuilder.prototype.newBuilder__scm_Builder = (function() {
  return new ScalaJS.c.scm_GrowingBuilder().init___scg_Growable(new ScalaJS.c.scm_StringBuilder().init___())
});
ScalaJS.c.scm_StringBuilder.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs)
});
ScalaJS.is.scm_StringBuilder = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_StringBuilder)))
});
ScalaJS.as.scm_StringBuilder = (function(obj) {
  return ((ScalaJS.is.scm_StringBuilder(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.StringBuilder"))
});
ScalaJS.isArrayOf.scm_StringBuilder = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_StringBuilder)))
});
ScalaJS.asArrayOf.scm_StringBuilder = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_StringBuilder(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.StringBuilder;", depth))
});
ScalaJS.d.scm_StringBuilder = new ScalaJS.ClassTypeData({
  scm_StringBuilder: 0
}, false, "scala.collection.mutable.StringBuilder", {
  scm_StringBuilder: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_SeqLike: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  jl_CharSequence: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  scm_IndexedSeqLike: 1,
  sci_StringLike: 1,
  sc_IndexedSeqOptimized: 1,
  s_math_Ordered: 1,
  jl_Comparable: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_StringBuilder.prototype.$classData = ScalaJS.d.scm_StringBuilder;
/** @constructor */
ScalaJS.c.sjs_js_WrappedArray = (function() {
  ScalaJS.c.scm_AbstractBuffer.call(this);
  this.array$6 = null
});
ScalaJS.c.sjs_js_WrappedArray.prototype = new ScalaJS.h.scm_AbstractBuffer();
ScalaJS.c.sjs_js_WrappedArray.prototype.constructor = ScalaJS.c.sjs_js_WrappedArray;
/** @constructor */
ScalaJS.h.sjs_js_WrappedArray = (function() {
  /*<skip>*/
});
ScalaJS.h.sjs_js_WrappedArray.prototype = ScalaJS.c.sjs_js_WrappedArray.prototype;
ScalaJS.c.sjs_js_WrappedArray.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.init___ = (function() {
  ScalaJS.c.sjs_js_WrappedArray.prototype.init___sjs_js_Array.call(this, []);
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.head__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__head__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.apply__I__O = (function(index) {
  return this.array$6[index]
});
ScalaJS.c.sjs_js_WrappedArray.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__lengthCompare__sc_IndexedSeqOptimized__I__I(this, len)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.apply__O__O = (function(v1) {
  var index = ScalaJS.uI(v1);
  return this.array$6[index]
});
ScalaJS.c.sjs_js_WrappedArray.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__sameElements__sc_IndexedSeqOptimized__sc_GenIterable__Z(this, that)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z(this)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  this.array$6["push"](elem);
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.sjs_js_WrappedArray$()
});
ScalaJS.c.sjs_js_WrappedArray.prototype.foreach__F1__V = (function(f) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__foreach__sc_IndexedSeqOptimized__F1__V(this, f)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.$$plus$eq__O__scm_Buffer = (function(elem) {
  this.array$6["push"](elem);
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.slice__I__I__O = (function(from, until) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__slice__sc_IndexedSeqOptimized__I__I__O(this, from, until)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.reverse__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__reverse__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.result__O = (function() {
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.seq__scm_Seq = (function() {
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, ScalaJS.uI(this.array$6["length"]))
});
ScalaJS.c.sjs_js_WrappedArray.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.length__I = (function() {
  return ScalaJS.uI(this.array$6["length"])
});
ScalaJS.c.sjs_js_WrappedArray.prototype.remove__I__O = (function(n) {
  return this.array$6["splice"](n, 1)[0]
});
ScalaJS.c.sjs_js_WrappedArray.prototype.drop__I__O = (function(n) {
  var until = ScalaJS.uI(this.array$6["length"]);
  return ScalaJS.s.sc_IndexedSeqOptimized$class__slice__sc_IndexedSeqOptimized__I__I__O(this, n, until)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.tail__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__tail__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.isDefinedAt__O__Z = (function(x) {
  var idx = ScalaJS.uI(x);
  return ScalaJS.s.sc_GenSeqLike$class__isDefinedAt__sc_GenSeqLike__I__Z(this, idx)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  this.array$6["push"](elem);
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.copyToArray__O__I__I__V = (function(xs, start, len) {
  ScalaJS.s.sc_IndexedSeqOptimized$class__copyToArray__sc_IndexedSeqOptimized__O__I__I__V(this, xs, start, len)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.sizeHint__I__V = (function(size) {
  /*<skip>*/
});
ScalaJS.c.sjs_js_WrappedArray.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.toCollection__O__sc_Seq = (function(repr) {
  return ScalaJS.as.scm_IndexedSeq(repr)
});
ScalaJS.c.sjs_js_WrappedArray.prototype.init___sjs_js_Array = (function(array) {
  this.array$6 = array;
  return this
});
ScalaJS.c.sjs_js_WrappedArray.prototype.stringPrefix__T = (function() {
  return "WrappedArray"
});
ScalaJS.d.sjs_js_WrappedArray = new ScalaJS.ClassTypeData({
  sjs_js_WrappedArray: 0
}, false, "scala.scalajs.js.WrappedArray", {
  sjs_js_WrappedArray: 1,
  scm_AbstractBuffer: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_SeqLike: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  scm_Buffer: 1,
  scm_BufferLike: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  scg_Shrinkable: 1,
  sc_script_Scriptable: 1,
  scg_Subtractable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  scm_IndexedSeqLike: 1,
  scm_ArrayLike: 1,
  scm_IndexedSeqOptimized: 1,
  sc_IndexedSeqOptimized: 1,
  scm_Builder: 1
});
ScalaJS.c.sjs_js_WrappedArray.prototype.$classData = ScalaJS.d.sjs_js_WrappedArray;
/** @constructor */
ScalaJS.c.scm_ArrayBuffer = (function() {
  ScalaJS.c.scm_AbstractBuffer.call(this);
  this.initialSize$6 = 0;
  this.array$6 = null;
  this.size0$6 = 0
});
ScalaJS.c.scm_ArrayBuffer.prototype = new ScalaJS.h.scm_AbstractBuffer();
ScalaJS.c.scm_ArrayBuffer.prototype.constructor = ScalaJS.c.scm_ArrayBuffer;
/** @constructor */
ScalaJS.h.scm_ArrayBuffer = (function() {
  /*<skip>*/
});
ScalaJS.h.scm_ArrayBuffer.prototype = ScalaJS.c.scm_ArrayBuffer.prototype;
ScalaJS.c.scm_ArrayBuffer.prototype.seq__sc_TraversableOnce = (function() {
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.$$plus$eq__O__scm_ArrayBuffer = (function(elem) {
  var n = ((1 + this.size0$6) | 0);
  ScalaJS.s.scm_ResizableArray$class__ensureSize__scm_ResizableArray__I__V(this, n);
  this.array$6.u[this.size0$6] = elem;
  this.size0$6 = ((1 + this.size0$6) | 0);
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.init___ = (function() {
  ScalaJS.c.scm_ArrayBuffer.prototype.init___I.call(this, 16);
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.head__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__head__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.scm_ArrayBuffer.prototype.apply__I__O = (function(idx) {
  return ScalaJS.s.scm_ResizableArray$class__apply__scm_ResizableArray__I__O(this, idx)
});
ScalaJS.c.scm_ArrayBuffer.prototype.remove__I__I__V = (function(n, count) {
  var requirement = (count >= 0);
  if ((!requirement)) {
    throw new ScalaJS.c.jl_IllegalArgumentException().init___T(("requirement failed: " + "removing negative number of elements"))
  };
  if (((n < 0) || (n > ((this.size0$6 - count) | 0)))) {
    throw new ScalaJS.c.jl_IndexOutOfBoundsException().init___T(("" + n))
  };
  var m = ((n + count) | 0);
  var len = ((this.size0$6 - ((n + count) | 0)) | 0);
  var src = this.array$6;
  var dest = this.array$6;
  ScalaJS.systemArraycopy(src, m, dest, n, len);
  var sz = ((this.size0$6 - count) | 0);
  ScalaJS.s.scm_ResizableArray$class__reduceToSize__scm_ResizableArray__I__V(this, sz)
});
ScalaJS.c.scm_ArrayBuffer.prototype.lengthCompare__I__I = (function(len) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__lengthCompare__sc_IndexedSeqOptimized__I__I(this, len)
});
ScalaJS.c.scm_ArrayBuffer.prototype.apply__O__O = (function(v1) {
  var idx = ScalaJS.uI(v1);
  return ScalaJS.s.scm_ResizableArray$class__apply__scm_ResizableArray__I__O(this, idx)
});
ScalaJS.c.scm_ArrayBuffer.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__sameElements__sc_IndexedSeqOptimized__sc_GenIterable__Z(this, that)
});
ScalaJS.c.scm_ArrayBuffer.prototype.isEmpty__Z = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__isEmpty__sc_IndexedSeqOptimized__Z(this)
});
ScalaJS.c.scm_ArrayBuffer.prototype.thisCollection__sc_Traversable = (function() {
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.$$plus$eq__O__scg_Growable = (function(elem) {
  return this.$$plus$eq__O__scm_ArrayBuffer(elem)
});
ScalaJS.c.scm_ArrayBuffer.prototype.companion__scg_GenericCompanion = (function() {
  return ScalaJS.m.scm_ArrayBuffer$()
});
ScalaJS.c.scm_ArrayBuffer.prototype.foreach__F1__V = (function(f) {
  ScalaJS.s.scm_ResizableArray$class__foreach__scm_ResizableArray__F1__V(this, f)
});
ScalaJS.c.scm_ArrayBuffer.prototype.$$plus$eq__O__scm_Buffer = (function(elem) {
  return this.$$plus$eq__O__scm_ArrayBuffer(elem)
});
ScalaJS.c.scm_ArrayBuffer.prototype.slice__I__I__O = (function(from, until) {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__slice__sc_IndexedSeqOptimized__I__I__O(this, from, until)
});
ScalaJS.c.scm_ArrayBuffer.prototype.reverse__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__reverse__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.scm_ArrayBuffer.prototype.result__O = (function() {
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.iterator__sc_Iterator = (function() {
  return new ScalaJS.c.sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, this.size0$6)
});
ScalaJS.c.scm_ArrayBuffer.prototype.seq__scm_Seq = (function() {
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.sizeHintBounded__I__sc_TraversableLike__V = (function(size, boundingColl) {
  ScalaJS.s.scm_Builder$class__sizeHintBounded__scm_Builder__I__sc_TraversableLike__V(this, size, boundingColl)
});
ScalaJS.c.scm_ArrayBuffer.prototype.init___I = (function(initialSize) {
  this.initialSize$6 = initialSize;
  ScalaJS.s.scm_ResizableArray$class__$$init$__scm_ResizableArray__V(this);
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.length__I = (function() {
  return this.size0$6
});
ScalaJS.c.scm_ArrayBuffer.prototype.seq__sc_Seq = (function() {
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.remove__I__O = (function(n) {
  var result = ScalaJS.s.scm_ResizableArray$class__apply__scm_ResizableArray__I__O(this, n);
  this.remove__I__I__V(n, 1);
  return result
});
ScalaJS.c.scm_ArrayBuffer.prototype.drop__I__O = (function(n) {
  var until = this.size0$6;
  return ScalaJS.s.sc_IndexedSeqOptimized$class__slice__sc_IndexedSeqOptimized__I__I__O(this, n, until)
});
ScalaJS.c.scm_ArrayBuffer.prototype.tail__O = (function() {
  return ScalaJS.s.sc_IndexedSeqOptimized$class__tail__sc_IndexedSeqOptimized__O(this)
});
ScalaJS.c.scm_ArrayBuffer.prototype.thisCollection__sc_Seq = (function() {
  return this
});
ScalaJS.c.scm_ArrayBuffer.prototype.$$plus$plus$eq__sc_TraversableOnce__scm_ArrayBuffer = (function(xs) {
  if (ScalaJS.is.sc_IndexedSeqLike(xs)) {
    var x2 = ScalaJS.as.sc_IndexedSeqLike(xs);
    var n = x2.length__I();
    var n$1 = ((this.size0$6 + n) | 0);
    ScalaJS.s.scm_ResizableArray$class__ensureSize__scm_ResizableArray__I__V(this, n$1);
    x2.copyToArray__O__I__I__V(this.array$6, this.size0$6, n);
    this.size0$6 = ((this.size0$6 + n) | 0);
    return this
  } else {
    return ScalaJS.as.scm_ArrayBuffer(ScalaJS.s.scg_Growable$class__$$plus$plus$eq__scg_Growable__sc_TraversableOnce__scg_Growable(this, xs))
  }
});
ScalaJS.c.scm_ArrayBuffer.prototype.isDefinedAt__O__Z = (function(x) {
  var idx = ScalaJS.uI(x);
  return ScalaJS.s.sc_GenSeqLike$class__isDefinedAt__sc_GenSeqLike__I__Z(this, idx)
});
ScalaJS.c.scm_ArrayBuffer.prototype.$$plus$eq__O__scm_Builder = (function(elem) {
  return this.$$plus$eq__O__scm_ArrayBuffer(elem)
});
ScalaJS.c.scm_ArrayBuffer.prototype.copyToArray__O__I__I__V = (function(xs, start, len) {
  ScalaJS.s.scm_ResizableArray$class__copyToArray__scm_ResizableArray__O__I__I__V(this, xs, start, len)
});
ScalaJS.c.scm_ArrayBuffer.prototype.sizeHint__I__V = (function(len) {
  if (((len > this.size0$6) && (len >= 1))) {
    var newarray = ScalaJS.newArrayObject(ScalaJS.d.O.getArrayOf(), [len]);
    var src = this.array$6;
    var length = this.size0$6;
    ScalaJS.systemArraycopy(src, 0, newarray, 0, length);
    this.array$6 = newarray
  }
});
ScalaJS.c.scm_ArrayBuffer.prototype.hashCode__I = (function() {
  return ScalaJS.m.s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
ScalaJS.c.scm_ArrayBuffer.prototype.toCollection__O__sc_Seq = (function(repr) {
  return ScalaJS.as.scm_IndexedSeq(repr)
});
ScalaJS.c.scm_ArrayBuffer.prototype.$$plus$plus$eq__sc_TraversableOnce__scg_Growable = (function(xs) {
  return this.$$plus$plus$eq__sc_TraversableOnce__scm_ArrayBuffer(xs)
});
ScalaJS.c.scm_ArrayBuffer.prototype.stringPrefix__T = (function() {
  return "ArrayBuffer"
});
ScalaJS.is.scm_ArrayBuffer = (function(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_ArrayBuffer)))
});
ScalaJS.as.scm_ArrayBuffer = (function(obj) {
  return ((ScalaJS.is.scm_ArrayBuffer(obj) || (obj === null)) ? obj : ScalaJS.throwClassCastException(obj, "scala.collection.mutable.ArrayBuffer"))
});
ScalaJS.isArrayOf.scm_ArrayBuffer = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArrayBuffer)))
});
ScalaJS.asArrayOf.scm_ArrayBuffer = (function(obj, depth) {
  return ((ScalaJS.isArrayOf.scm_ArrayBuffer(obj, depth) || (obj === null)) ? obj : ScalaJS.throwArrayCastException(obj, "Lscala.collection.mutable.ArrayBuffer;", depth))
});
ScalaJS.d.scm_ArrayBuffer = new ScalaJS.ClassTypeData({
  scm_ArrayBuffer: 0
}, false, "scala.collection.mutable.ArrayBuffer", {
  scm_ArrayBuffer: 1,
  scm_AbstractBuffer: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_SeqLike: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  scm_Buffer: 1,
  scm_BufferLike: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  scg_Shrinkable: 1,
  sc_script_Scriptable: 1,
  scg_Subtractable: 1,
  scm_IndexedSeqOptimized: 1,
  scm_IndexedSeqLike: 1,
  sc_IndexedSeqLike: 1,
  sc_IndexedSeqOptimized: 1,
  scm_Builder: 1,
  scm_ResizableArray: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
ScalaJS.c.scm_ArrayBuffer.prototype.$classData = ScalaJS.d.scm_ArrayBuffer;
//# sourceMappingURL=example-fastopt.js.map
