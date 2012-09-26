// <copyright file="Number.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Globalization;

    /// <summary>
    /// Utilities that have to do with numbers.
    /// Scheme numbers are preresented by .NET double.
    /// </summary>
    public class Number : SchemeObject, IEquatable<Number>
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
        /// Create a number from a double.
        /// </summary>
        /// <param name="number">The number value.</param>
        /// <param name="lineNumber">The line where the number is read.</param>
        private Number(double number, int lineNumber) : base(lineNumber)
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

        #region Properties
        /// <summary>
        /// Gets the number itself.
        /// </summary>
        public double N
        {
            get { return this.n; }
        }
        #endregion

        #region New
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
        #endregion

        #region New
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

        /// <summary>
        /// Create a new number from a double.
        /// This provides a selection of cached numbers.
        /// </summary>
        /// <param name="number">The number value.</param>
        /// <param name="lineNumber">The line where the number is read.</param>
        /// <returns>A Number</returns>
        public static Number New(double number, int lineNumber)
        {
            if (number == 0.0)
            {
                return Zero;
            }

            if (number == 1.0)
            {
                return One;
            }

            return new Number(number, lineNumber);
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
        /// <param name="primEnv">The environment to define the primitives into.</param>
        internal static new void DefinePrimitives(PrimitiveEnvironment primEnv)
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
            primEnv
                .DefinePrimitive(
                    "*", 
                    new[] { "6.5.5", "(* <z1> ...)" },
                    (args, env, caller) => Compute(args, (x, y) => x * y, 1.0),
                    new ArgsInfo(0, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "+", 
                    new[] { "6.5.5", "(+ <z1> ...)" },
                    (args, env, caller) => Compute(args, (x, y) => x + y, 0.0),
                    new ArgsInfo(0, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "-", 
                    new[] { "6.5.5", "(- <z1> <z2>)", "(- <z>)" },
                    (args, env, caller) => Compute(args, (x, y) => x - y, 0.0),
                    new ArgsInfo(1, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "/", 
                    new[] { "6.5.5", "(/ <z1> <z2>)", "(/ <z>)", "(/ <z1> <z2> ...)" },
                    (args, env, caller) => Compute(args, (x, y) => x / y, 1.0),
                    new ArgsInfo(1, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "=", 
                    new[] { "6.5.5", "(= <z1> <z2> <z3> ...)" }, 
                    (args, env, caller) => Compare(args, (x, y) => x == y), 
                    new ArgsInfo(2, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "<", 
                    new[] { "6.5.5", "(< <z1> <z2> <z3> ...)" }, 
                    (args, env, caller) => Compare(args, (x, y) => x < y), 
                    new ArgsInfo(2, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    ">", 
                    new[] { "6.5.5", "(> <z1> <z2> <z3> ...)" }, 
                    (args, env, caller) => Compare(args, (x, y) => x > y), 
                    new ArgsInfo(2, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "<=", 
                    new[] { "6.5.5", "(<= <z1> <z2> <z3> ...)" }, 
                    (args, env, caller) => Compare(args, (x, y) => x <= y), 
                    new ArgsInfo(2, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    ">=", 
                    new[] { "6.5.5", "(>= <z1> <z2> <z3> ...)" }, 
                    (args, env, caller) => Compare(args, (x, y) => x >= y), 
                    new ArgsInfo(2, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "abs", 
                    new[] { "6.5.5", "(abs <x>)" }, 
                    (args, env, caller) => (Number)Math.Abs(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "ceiling", 
                    new[] { "6.5.5", "(ceiling <x>)" }, 
                    (args, env, caller) => (Number)Math.Ceiling(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "floor", 
                    new[] { "6.5.5", "(floor <x>)" }, 
                    (args, env, caller) => (Number)Math.Floor(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "acos", 
                    new[] { "6.5.5", "(acos <z>)" }, 
                    (args, env, caller) => (Number)Math.Acos(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "asin", 
                    new[] { "6.5.5", "(asin <z>)" }, 
                    (args, env, caller) => (Number)Math.Asin(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "atan", 
                    new[] { "6.5.5", "(atan <z>)", "(atan <y> <x>)" }, 
                    (args, env, caller) => (Number)Math.Atan(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "complex?", 
                    new[] { "6.5.5", "(complex? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(First(args) is Number), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "cos", 
                    new[] { "6.5.5", "(cos <z>)" }, 
                    (args, env, caller) => (Number)Math.Cos(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "even?", 
                    new[] { "6.5.5", "(even? <n>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 == 0), 
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "exact?", 
                    new[] { "6.5.5", "(exact? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "exact->inexact", 
                    new[] { "6.5.5", "(exact->inexact <z>)" }, 
                    (args, env, caller) => (Number)Num(First(args)),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "exp", 
                    new[] { "6.5.5", "(exp <z>)" }, 
                    (args, env, caller) => (Number)Math.Exp(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "expt", 
                    new[] { "6.5.5", "(expt <z1> <z2>)" }, 
                    (args, env, caller) => (Number)Expt(Num(First(args)), Num(Second(args))),
                    new ArgsInfo(2, ArgType.Number))
                .DefinePrimitive(
                    "gcd", 
                    new[] { "6.5.5", "(gcd <n1> ...)" }, 
                    (args, env, caller) => (Number)(args is EmptyList ? 0 : Gcd(args)),
                    new ArgsInfo(0, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "inexact?", 
                    new[] { "6.5.5", "(inexact? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(!IsExact(First(args))), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "inexact->exact", 
                    new[] { "6.5.5", "(inexact->exact <z>)" }, 
                    (args, env, caller) => (Number)Num(First(args)),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "integer->char", 
                    new[] { "6.6", "(integer->char <n>)" }, 
                    (args, env, caller) => (Character)(char)(int)Num(First(args)), 
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "integer?", 
                    new[] { "6.5.5", "(integer? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "lcm", 
                    new[] { "6.5.5", "(lcm <n1> ...)" }, 
                    (args, env, caller) => (Number)(args is EmptyList ? 1 : Lcm(args)),
                    new ArgsInfo(0, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "log", 
                    new[] { "6.5.5", "(log <z>)" }, 
                    (args, env, caller) => (Number)Math.Log(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "max", 
                    new[] { "6.5.5", "(max? <x1> <x2> ...)" }, 
                    (args, env, caller) => Compute(args, Math.Max, Num(First(args))),
                    new ArgsInfo(1, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "min", 
                    new[] { "6.5.5", "(min? <x1> <x2> ...)" }, 
                    (args, env, caller) => Compute(args, Math.Min, Num(First(args))),
                    new ArgsInfo(1, MaxInt, ArgType.Number))
                .DefinePrimitive(
                    "modulo", 
                    new[] { "6.5.5", "(module <n1> <n2>)" }, 
                    (args, env, caller) => (Number)Modulo(Num(First(args)), Num(Second(args))),
                    new ArgsInfo(2, ArgType.Number))
                .DefinePrimitive(
                    "negative?", 
                    new[] { "6.5.5", "(negative? <x>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(Num(First(args)) < 0), 
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "number->string", 
                    new[] { "6.5.6", "(number->string <number>)", "(number->string <number> <radix>)" }, 
                    (args, env, caller) => NumberToString(Num(First(args)), Second(args)), 
                    new ArgsInfo(1, 2, ArgType.Number))
                .DefinePrimitive(
                    "number?", 
                    new[] { "6.5.5", "(number? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(First(args) is Number), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "odd?", 
                    new[] { "6.5.5", "(odd? <n>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 != 0), 
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "positive?", 
                    new[] { "6.5.5", "(positive? <x>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(Num(First(args)) > 0), 
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "quotient", 
                    new[] { "6.5.5", "(quotient <n1> <n2>)" }, 
                    (args, env, caller) => (Number)Quotient(Num(First(args)), Num(Second(args))),
                    new ArgsInfo(2, ArgType.Number))
                .DefinePrimitive(
                    "rational?", 
                    new[] { "6.5.5", "(rational? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "real?", 
                    new[] { "6.5.5", "(real? <obj>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(IsExact(First(args))), 
                    new ArgsInfo(1, ArgType.Obj))
                .DefinePrimitive(
                    "remainder", 
                    new[] { "6.5.5", "(remainder <n1> <n2>)" }, 
                    (args, env, caller) => (Number)((long)Num(First(args)) % (long)Num(Second(args))), 
                    new ArgsInfo(2, ArgType.Number))
                .DefinePrimitive(
                    "round", 
                    new[] { "6.5.5", "(round <x>)" }, 
                    (args, env, caller) => (Number)Math.Round(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "sin", 
                    new[] { "6.5.5", "(sin <z>)" }, 
                    (args, env, caller) => (Number)Math.Sin(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "sqrt", 
                    new[] { "6.5.5", "(sqrt <z>)" }, 
                    (args, env, caller) => (Number)Math.Sqrt(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "tan", 
                    new[] { "6.5.5", "(tan <z>)" }, 
                    (args, env, caller) => (Number)Math.Tan(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "truncate", 
                    new[] { "6.5.5", "(truncate <x>)" }, 
                    (args, env, caller) => (Number)Truncate(Num(First(args))),
                    new ArgsInfo(1, ArgType.Number))
                .DefinePrimitive(
                    "zero?", 
                    new[] { "6.5.5", "(zero? <z>)" }, 
                    (args, env, caller) => SchemeBoolean.Truth(Num(First(args)) == 0), 
                    new ArgsInfo(1, ArgType.Number));
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
                return ((long)Num(this)).ToString(CultureInfo.InvariantCulture);
            }

            return Num(this).ToString(CultureInfo.InvariantCulture);
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

        #region Internal Methods
        /// <summary>
        /// Describe a number by returning its value.
        /// </summary>
        /// <returns>The number as a string.</returns>
        internal override string Describe()
        {
            return this.ToString();
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

            return num.ToString(CultureInfo.InvariantCulture);
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
