// <copyright file="Number.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;

    /// <summary>
    /// Utilities that have to do with numbers.
    /// Scheme numbers are preresented by .NET double.
    /// </summary>
    public class Number : SchemeObject
    {
        #region Static Fields
        /// <summary>
        /// The constant 0.
        /// </summary>
        private static readonly Number Zero = new Number(0.0);

        /// <summary>
        /// The constant 1.
        /// </summary>
        private static readonly Number One = new Number(1.0);
        #endregion

        #region Fields
        /// <summary>
        /// The number value.
        /// </summary>
        private readonly double n;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the Number class.
        /// Create a number from a double.
        /// </summary>
        /// <param name="number">The number value.</param>
        private Number(double number)
        {
            this.n = number;
        }

        /// <summary>
        /// Initializes a new instance of the Number class.
        /// Create a number from an int.
        /// </summary>
        /// <param name="number">The number value.</param>
        private Number(int number)
        {
            this.n = number;
        }

        /// <summary>
        /// Initializes a new instance of the Number class.
        /// Create a number from a long.
        /// </summary>
        /// <param name="number">The number value.</param>
        private Number(long number)
        {
            this.n = number;
        }

        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public override string TypeName
        {
            get { return ValueTypeName(ValueType.Number); }
        }
        #endregion

        #region Properties
        /// <summary>
        /// Gets the number itself.
        /// </summary>
        public double N
        {
            get { return this.n; }
        }
        #endregion

        /// <summary>
        /// When we need a Number but give an int, this takes care of it.
        /// </summary>
        /// <param name="x">The int.</param>
        /// <returns>A number corresponding to the given int.</returns>
        public static implicit operator Number(int x)
        {
            return New(x);
        }

        /// <summary>
        /// When we need a Number but give an double, this takes care of it.
        /// </summary>
        /// <param name="x">The double.</param>
        /// <returns>A number corresponding to the given double.</returns>
        public static implicit operator Number(double x)
        {
            return New(x);
        }

        #region Public Static Methods
        /// <summary>
        /// Create a new number from a long.
        /// </summary>
        /// <param name="number">The number value.</param>
        /// <returns>A Number</returns>
        public static Number New(long number)
        {
            if (number == 0)
            {
                return Zero;
            }

            if (number == 1)
            {
                return One;
            }

            return new Number(number);
        }

        /// <summary>
        /// Create a new number from a double.
        /// This provides a selection of cached numbers.
        /// </summary>
        /// <param name="number">The number value.</param>
        /// <returns>A Number</returns>
        public static Number New(double number)
        {
            if (number == 0.0)
            {
                return Zero;
            }

            if (number == 1.0)
            {
                return One;
            }

            return new Number(number);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Test two numbers for equality.
        /// </summary>
        /// <param name="obj1">The first number.</param>
        /// <param name="obj2">The second number.</param>
        /// <returns>True if they are both numbers are the same.</returns>
        public static SchemeBoolean Equal(SchemeObject obj1, SchemeObject obj2)
        {
            if (!(obj1 is Number) || !(obj2 is Number))
            {
                return false;
            }

            return Num(obj1) == Num(obj2);
        }

        /// <summary>
        /// Convert to a number.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding Number.</returns>
        public static int AsInt(SchemeObject x)
        {
            if (x is Number)
            {
                return Convert.ToInt32(((Number)x).N);
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0;
        }

        /// <summary>
        /// Convert to a double
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding double.</returns>
        public static double AsDouble(SchemeObject x)
        {
            if (x is Number)
            {
                return ((Number)x).N;
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0.0;
        }

        /// <summary>
        /// Convert to a float
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding float.</returns>
        public static float AsFloat(SchemeObject x)
        {
            if (x is Number)
            {
                return (float)((Number)x).N;
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0.0f;
        }

        /// <summary>
        /// Convert to a short
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding short.</returns>
        public static short AsShort(SchemeObject x)
        {
            if (x is Number)
            {
                return (short)((Number)x).N;
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0;
        }

        /// <summary>
        /// Convert to a long
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding short.</returns>
        public static long AsLong(SchemeObject x)
        {
            if (x is Number)
            {
                return (long)((Number)x).N;
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0;
        }

        /// <summary>
        /// Convert to a byte
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding byte.</returns>
        public static byte AsByte(SchemeObject x)
        {
            if (x is Number)
            {
                return (byte)((Number)x).N;
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the numeric primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static new void DefinePrimitives(PrimitiveEnvironment env)
        {
            // not implemented
            //// <r4rs section="6.5.5">(numerator <q>)</r4rs>
            //// <r4rs section="6.5.5">(denominator <q>)</r4rs>
            //// <r4rs section="6.5.5">(rationalize <x> <y>)</r4rs>
            //// <r4rs section="6.5.5">(make-rectangular <x1> <x2>)</r4rs>
            //// <r4rs section="6.5.5">(make-polar <x3> <x4>)</r4rs>
            //// <r4rs section="6.5.5">(real-part <z>)</r4rs>
            //// <r4rs section="6.5.5">(imag-part <z>)</r4rs>
            //// <r4rs section="6.5.5">(magnitude <z>)</r4rs>
            //// <r4rs section="6.5.5">(angle <z>)</r4rs>

            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.5.5">(* <z1> ...)</r4rs>
                .DefinePrimitive(
                    "*",
                    (args, caller) => Compute(args, (x, y) => x * y, 1.0),
                    0, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(+ <z1> ...)</r4rs>
                .DefinePrimitive(
                    "+",
                    (args, caller) => Compute(args, (x, y) => x + y, 0.0),
                    0, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(- <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(- <z>)</r4rs>
                .DefinePrimitive(
                    "-",
                    (args, caller) => Compute(args, (x, y) => x - y, 0.0),
                    1, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(/ <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z1> <z2> ...)</r4rs>
                .DefinePrimitive(
                    "/",
                    (args, caller) => Compute(args, (x, y) => x / y, 1.0),
                    1, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    "=", 
                    (args, caller) => Compare(args, (x, y) => x == y), 
                    2, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(< <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    "<", 
                    (args, caller) => Compare(args, (x, y) => x < y), 
                    2, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(> <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    ">", 
                    (args, caller) => Compare(args, (x, y) => x > y), 
                    2, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(<= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    "<=", 
                    (args, caller) => Compare(args, (x, y) => x <= y), 
                    2, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(>= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    ">=", 
                    (args, caller) => Compare(args, (x, y) => x >= y), 
                    2, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(abs <x>)</r4rs>
                .DefinePrimitive(
                    "abs", 
                    (args, caller) => (Number)Math.Abs(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(ceiling <x>)</r4rs>
                .DefinePrimitive(
                    "ceiling", 
                    (args, caller) => (Number)Math.Ceiling(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(floor <x>)</r4rs>
                .DefinePrimitive(
                    "floor", 
                    (args, caller) => (Number)Math.Floor(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(acos <z>)</r4rs>
                .DefinePrimitive(
                    "acos", 
                    (args, caller) => (Number)Math.Acos(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(asin <z>)</r4rs>
                .DefinePrimitive(
                    "asin", 
                    (args, caller) => (Number)Math.Asin(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(atan <z>)</r4rs>
                //// <r4rs section="6.5.5">(atan <y> <x>)</r4rs>
                .DefinePrimitive(
                    "atan", 
                    (args, caller) => (Number)Math.Atan(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(complex? <obj>)</r4rs>
                .DefinePrimitive(
                    "complex?", 
                    (args, caller) => SchemeBoolean.Truth(First(args) is Number), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.5.5">(cos <z>)</r4rs>
                .DefinePrimitive(
                    "cos", 
                    (args, caller) => (Number)Math.Cos(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(even? <n>)</r4rs>
                .DefinePrimitive(
                    "even?", 
                    (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 == 0), 
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(exact? <obj>)</r4rs>
                .DefinePrimitive(
                    "exact?", 
                    (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.5.5">(exact->inexact <z>)</r4rs>
                .DefinePrimitive(
                    "exact->inexact", 
                    (args, caller) => (Number)Num(First(args)),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(exp <z>)</r4rs>
                .DefinePrimitive(
                    "exp", 
                    (args, caller) => (Number)Math.Exp(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(expt <z1> <z2>)</r4rs>
                .DefinePrimitive(
                    "expt", 
                    (args, caller) => (Number)Expt(Num(First(args)), Num(Second(args))),
                    2, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(gcd <n1> ...)</r4rs>
                .DefinePrimitive(
                    "gcd", 
                    (args, caller) => (Number)(args is EmptyList ? 0 : Gcd(args)),
                    0, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(inexact? <obj>)</r4rs>
                .DefinePrimitive(
                    "inexact?", 
                    (args, caller) => SchemeBoolean.Truth(!IsExact(First(args))), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.5.5">(inexact->exact <z>)</r4rs>
                .DefinePrimitive(
                    "inexact->exact", 
                    (args, caller) => (Number)Num(First(args)),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.6">(integer->char <n>)</r4rs>
                .DefinePrimitive(
                    "integer->char", 
                    (args, caller) => (Character)(char)(int)Num(First(args)), 
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(integer? <obj>)</r4rs>
                .DefinePrimitive(
                    "integer?", 
                    (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.5.5">(lcm <n1> ...)</r4rs>
                .DefinePrimitive(
                    "lcm", 
                    (args, caller) => (Number)(args is EmptyList ? 1 : Lcm(args)),
                    0, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(log <z>)</r4rs>
                .DefinePrimitive(
                    "log", 
                    (args, caller) => (Number)Math.Log(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(max? <x1> <x2> ...)</r4rs>
                .DefinePrimitive(
                    "max", 
                    (args, caller) => Compute(args, Math.Max, Num(First(args))),
                    1, 
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(min? <x1> <x2> ...)</r4rs>
                .DefinePrimitive(
                    "min", 
                    (args, caller) => Compute(args, Math.Min, Num(First(args))),
                    1,
                    MaxInt, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(module <n1> <n2>)</r4rs>
                .DefinePrimitive(
                    "modulo", 
                    (args, caller) => (Number)Modulo(Num(First(args)), Num(Second(args))),
                    2, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(negative? <x>)</r4rs>
                .DefinePrimitive(
                    "negative?", 
                    (args, caller) => SchemeBoolean.Truth(Num(First(args)) < 0), 
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.6">(number->string <number>)</r4rs>
                //// <r4rs section="6.5.6">(number->string <number> <radix>)</r4rs>
                .DefinePrimitive(
                    "number->string", 
                    (args, caller) => NumberToString(Num(First(args)), Second(args)), 
                    1, 
                    2, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(number? <obj>)</r4rs>
                .DefinePrimitive(
                    "number?", 
                    (args, caller) => SchemeBoolean.Truth(First(args) is Number), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.5.5">(odd? <n>)</r4rs>
                .DefinePrimitive(
                    "odd?", 
                    (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 != 0), 
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(positive? <x>)</r4rs>
                .DefinePrimitive(
                    "positive?", 
                    (args, caller) => SchemeBoolean.Truth(Num(First(args)) > 0), 
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(quotient <n1> <n2>)</r4rs>
                .DefinePrimitive(
                    "quotient", 
                    (args, caller) => (Number)Quotient(Num(First(args)), Num(Second(args))),
                    2, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(rational? <obj>)</r4rs>
                .DefinePrimitive(
                    "rational?", 
                    (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.5.5">(real? <obj>)</r4rs>
                .DefinePrimitive(
                    "real?", 
                    (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    1, 
                    ValueType.Obj)
                //// <r4rs section="6.5.5">(remainder <n1> <n2>)</r4rs>
                .DefinePrimitive(
                    "remainder", 
                    (args, caller) => (Number)((long)Num(First(args)) % (long)Num(Second(args))), 
                    2, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(round <x>)</r4rs>
                .DefinePrimitive(
                    "round", 
                    (args, caller) => (Number)Math.Round(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(sin <z>)</r4rs>
                .DefinePrimitive(
                    "sin", 
                    (args, caller) => (Number)Math.Sin(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(sqrt <z>)</r4rs>
                .DefinePrimitive(
                    "sqrt", 
                    (args, caller) => (Number)Math.Sqrt(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(tan <z>)</r4rs>
                .DefinePrimitive(
                    "tan", 
                    (args, caller) => (Number)Math.Tan(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(truncate <x>)</r4rs>
                .DefinePrimitive(
                    "truncate", 
                    (args, caller) => (Number)Truncate(Num(First(args))),
                    1, 
                    ValueType.Number)
                //// <r4rs section="6.5.5">(zero? <z>)</r4rs>
                .DefinePrimitive(
                    "zero?", 
                    (args, caller) => SchemeBoolean.Truth(Num(First(args)) == 0), 
                    1, 
                    ValueType.Number);
        }
        #endregion

        #region Equality
        /// <summary>
        /// Provide our own version of the Equals method.
        /// </summary>
        /// <param name="other">The other object.</param>
        /// <returns>True if they are equal numbers.</returns>
        public override bool Equals(object other)
        {
            if (!(other is Number))
            {
                return false;
            }

            return this.Equals((Number)other);
        }

        /// <summary>
        /// Compares two Number values by comparing their underlying numerical value.
        /// </summary>
        /// <param name="other">The other Number.</param>
        /// <returns>True if they have the same number value.</returns>
        public bool Equals(Number other)
        {
            return this.n == other.n;
        }

        /// <summary>
        /// The hash code is the number's hash code.
        /// </summary>
        /// <returns>The hash code.</returns>
        public override int GetHashCode()
        {
            return this.n.GetHashCode();
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Create a string representation of the number for printing.
        /// </summary>
        /// <returns>The number as a string.</returns>
        public override string ToString()
        {
            if (this.n == Math.Round(this.n))
            {
                return ((long)Num(this)).ToString();
            }

            return Num(this).ToString();
        }

        /// <summary>
        /// Write the number to the string builder.
        /// For numbers without a fractional part, write as an integer.
        /// Otherwise, write as a double.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public override void PrintString(bool quoted, StringBuilder buf)
        {
            if (this.n == Math.Round(this.n))
            {
                buf.Append((long)this.n);
            }
            else
            {
                buf.Append(this.n);
            }
        }
        #endregion

        #region New
        /// <summary>
        /// Create a new number from an int.
        /// This provides a selection of cached numbers.
        /// </summary>
        /// <param name="number">The number value.</param>
        /// <returns>A Number</returns>
        internal static Number New(int number)
        {
            if (number == 0)
            {
                return Zero;
            }

            if (number == 1)
            {
                return One;
            }

            return new Number(number);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Another name for As, works better here.
        /// </summary>
        /// <param name="obj">The object to cast.  Caller ensures that the object is acutllay a Number.</param>
        /// <returns>The object, cast to double.</returns>
        private static double Num(SchemeObject obj)
        {
            return ((Number)obj).N;
        }

        /// <summary>
        /// Compute the greatest common divisor of a list of numbers.
        /// </summary>
        /// <param name="args">The list of numbers.</param>
        /// <returns>The greatest common divisor.</returns>
        private static double Gcd(SchemeObject args)
        {
            long gcd = 0;
            while (args is Pair)
            {
                gcd = Gcd2(Math.Abs((long)Num(First(args))), gcd);
                args = Rest(args);
            }

            return gcd;
        }

        /// <summary>
        /// Compute the greatest common divisor of two numbers at a time.
        /// </summary>
        /// <param name="a">The first number.</param>
        /// <param name="b">The second number.</param>
        /// <returns>The GCD of the two numbers.</returns>
        private static long Gcd2(long a, long b)
        {
            return b == 0 ? a : Gcd2(b, a % b);
        }

        /// <summary>
        /// Tests if the number is exact.
        /// The constant here is 2**53.
        /// This is because double has 52 bits or precision.
        /// This isn't the biggest integer, but above this the set of
        ///   integers has gaps.
        /// </summary>
        /// <param name="x">The number to test.</param>
        /// <returns>True if the number is exact.</returns>
        private static bool IsExact(SchemeObject x)
        {
            if (!(x is Number))
            {
                return false;
            }

            double d = Num(x);
            return d == Math.Round(d) && Math.Abs(d) < 9007199254740992.0;
        }

        /// <summary>
        /// Compute the least common multiple of a set of numbers.
        /// </summary>
        /// <param name="args">The numbers to use.</param>
        /// <returns>The LCM of these numbers.</returns>
        private static double Lcm(SchemeObject args)
        {
            long lcm = 1;
            while (args is Pair)
            {
                long n = Math.Abs((long)Num(First(args)));
                long g = Gcd2(n, lcm);
                lcm = g == 0 ? g : (n / g) * lcm;
                args = Rest(args);
            }

            return lcm;
        }

        /// <summary>
        /// Convert a number to its string equivalent.
        /// Optionally, use a number base different from 10.
        /// </summary>
        /// <param name="num">The number to convert.</param>
        /// <param name="numberBase">The number base.</param>
        /// <returns>A string version of the number.</returns>
        private static SchemeString NumberToString(double num, SchemeObject numberBase)
        {
            int b = numberBase is Number ? (int)Num(numberBase) : 10;
            if (b != 10 || num == Math.Round(num))
            {
                return Convert.ToString((long)num, b).ToCharArray();
            }

            return num.ToString();
        }

        /// <summary>
        /// Compare a set of numbers using a given comparison operator.
        /// Comparison stops when one of the results yields false.
        /// </summary>
        /// <param name="args">A list of numbers to compare.  Caller checks that these are all Number.</param>
        /// <param name="comp">The compare operation to apply successively to pairs of 
        ///     adjacent numbers.</param>
        /// <returns>True only if all comparisons are true.</returns>
        private static SchemeBoolean Compare(SchemeObject args, Func<double, double, bool> comp)
        {
            while (Rest(args) is Pair)
            {
                if (!comp(Num(First(args)), Num(Second(args))))
                {
                    return false;
                }

                args = Rest(args);
            }

            return true;
        }

        /// <summary>
        /// Compute an operation on a list of numbers.
        /// In addition to usual arithmetic operators, can also do unary - and /
        ///   and also max and min.
        /// </summary>
        /// <param name="args">The list of numbers to operate on.
        /// The caller has already checked that these are all Numbers</param>
        /// <param name="oper">The operation to apply</param>
        /// <param name="initial">The initial value of the computation.</param>
        /// <returns>The final result of the operation as a Number.</returns>
        private static Number Compute(SchemeObject args, Func<double, double, double> oper, double initial)
        {
            // If there are no numbers, apply return initial value
            if (args is EmptyList)
            {
                return initial;
            }

            double resultSoFar = Num(First(args));
            if (Rest(args) is EmptyList)
            {
                // If there is one number, apply the operation on the initial value and value.
                return oper(initial, resultSoFar);
            }

            args = Rest(args);
            while (args is Pair)
            {
                // If there are several numbers, apply the operation on values and partial results.
                resultSoFar = oper(resultSoFar, Num(First(args)));
                args = Rest(args);
            }

            return resultSoFar;
        }

        /// <summary>
        /// Calculate the first raised to the second power.
        /// </summary>
        /// <param name="first">The base number.</param>
        /// <param name="second">The exponent.</param>
        /// <returns>The base raised to the exponent.</returns>
        private static double Expt(double first, double second)
        {
            if (first == 0.0 && second < 0.0)
            {
                // Math.Pow gives infinity for this case
                return 0;
            }

            return Math.Pow(first, second);
        }

        /// <summary>
        /// Calculate the first modulo the second.
        /// </summary>
        /// <param name="first">The first number.</param>
        /// <param name="second">The second number.</param>
        /// <returns>The first modulo the second.</returns>
        private static double Modulo(double first, double second)
        {
            var x = (long)first;
            var y = (long)second;
            long m = x % y;
            return x * y > 0 || m == 0 ? m : m + y;
        }

        /// <summary>
        /// Calculate the first divided by the second.
        /// </summary>
        /// <param name="first">The first number.</param>
        /// <param name="second">The second number.</param>
        /// <returns>The first divided by the second.</returns>
        private static double Quotient(double first, double second)
        {
            double d = first / second;
            return d > 0 ? Math.Floor(d) : Math.Ceiling(d);
        }

        /// <summary>
        /// Truncate the given number.
        /// </summary>
        /// <param name="d">The number to truncate.</param>
        /// <returns>The argument truncated.</returns>
        private static double Truncate(double d)
        {
            return d < 0.0D ? Math.Ceiling(d) : Math.Floor(d);
        }
        #endregion
    }
}
