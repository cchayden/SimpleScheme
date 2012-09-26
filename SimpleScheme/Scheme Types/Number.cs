// <copyright file="Number.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Utilities that have to do with numbers.
    /// Scheme numbers are preresented by .NET double.
    /// </summary>
    public class Number : IPrintable, ISchemeType
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
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Number); }
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

        #region Public Static Methods
        /// <summary>
        /// Identifies objects of this scheme type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is this scheme type.</returns>
        public static bool Is(Obj obj)
        {
            return obj is Number;
        }

        /// <summary>
        /// Create a new number from a double.
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
        /// Create a new number from an int.
        /// </summary>
        /// <param name="number">The number value.</param>
        /// <returns>A Number</returns>
        public static Number New(int number)
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
        /// Write the number to the string builder.
        /// For numbers without a fractional part, write as an integer.
        /// Otherwise, write as a double.
        /// </summary>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
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

        /// <summary>
        /// Create a string representation of the number for printing.
        /// </summary>
        /// <returns>The number as a string.</returns>
        public override string ToString()
        {
            if (this.n == Math.Round(this.n))
            {
                return ((long)this.N).ToString();
            }
            else
            {
                return this.N.ToString();
            }
        }

        /// <summary>
        /// Test two numbers for equality.
        /// </summary>
        /// <param name="obj1">The first number.</param>
        /// <param name="obj2">The second number.</param>
        /// <returns>True if they are both numbers are the same.</returns>
        public static SchemeBoolean Equal(Obj obj1, Obj obj2)
        {
            if (!obj1.IsNumber() || !obj2.IsNumber())
            {
                return SchemeBoolean.False;
            }

            return obj1.AsNumber().N == obj2.AsNumber().N ? SchemeBoolean.True : SchemeBoolean.False;
        }
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the numeric primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(PrimitiveEnvironment env)
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
                    Symbol.New("*"), 
                    (args, caller) => New(Compute(args, (x, y) => x * y, 1.0)), 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(+ <z1> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("+"), 
                    (args, caller) => New(Compute(args, (x, y) => x + y, 0.0)), 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(- <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(- <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("-"), 
                    (args, caller) => New(Compute(args, (x, y) => x - y, 0.0)), 
                    1, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(/ <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z1> <z2> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("/"), 
                    (args, caller) => New(Compute(args, (x, y) => x / y, 1.0)), 
                    1, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("="), 
                    (args, caller) => Compare(args, (x, y) => x == y), 
                    2, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(< <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("<"), 
                    (args, caller) => Compare(args, (x, y) => x < y), 
                    2, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(> <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New(">"), 
                    (args, caller) => Compare(args, (x, y) => x > y), 
                    2, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(<= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("<="), 
                    (args, caller) => Compare(args, (x, y) => x <= y), 
                    2, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(>= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New(">="), 
                    (args, caller) => Compare(args, (x, y) => x >= y), 
                    2, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(abs <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("abs"), 
                    (args, caller) => New(Math.Abs(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(ceiling <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("ceiling"), 
                    (args, caller) => New(Math.Ceiling(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(floor <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("floor"), 
                    (args, caller) => New(Math.Floor(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(acos <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("acos"), 
                    (args, caller) => New(Math.Acos(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(asin <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("asin"), 
                    (args, caller) => New(Math.Asin(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(atan <z>)</r4rs>
                //// <r4rs section="6.5.5">(atan <y> <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("atan"), 
                    (args, caller) => New(Math.Atan(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(complex? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("complex?"), 
                    (args, caller) => SchemeBoolean.Truth(args.First().IsNumber()), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.5.5">(cos <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("cos"), 
                    (args, caller) => New(Math.Cos(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(even? <n>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("even?"), 
                    (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(args.First())) % 2 == 0), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(exact? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("exact?"), 
                    (args, caller) => SchemeBoolean.Truth(IsExact(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.5.5">(exact->inexact <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("exact->inexact"), 
                    (args, caller) => Num(args.First()), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(exp <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("exp"), 
                    (args, caller) => New(Math.Exp(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(expt <z1> <z2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("expt"), 
                    (args, caller) => New(Expt(args.First(), args.Second())), 
                    2, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(gcd <n1> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("gcd"), 
                    (args, caller) => args.IsEmptyList() ? Zero : New(Gcd(args)), 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(inexact? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("inexact?"), 
                    (args, caller) => SchemeBoolean.Truth(!IsExact(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.5.5">(inexact->exact <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("inexact->exact"), 
                    (args, caller) => Num(args.First()), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.6">(integer->char <n>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("integer->char"), 
                    (args, caller) => Character.New((char)(int)Num(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(integer? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("integer?"), 
                    (args, caller) => SchemeBoolean.Truth(IsExact(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.5.5">(lcm <n1> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("lcm"), 
                    (args, caller) => args.IsEmptyList() ? One : New(Lcm(args)), 
                    0, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(log <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("log"), 
                    (args, caller) => New(Math.Log(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(max? <x1> <x2> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("max"), 
                    (args, caller) => New(Compute(args, Math.Max, Num(args.First()))), 
                    1, 
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(min? <x1> <x2> ...)</r4rs>
                .DefinePrimitive(
                    Symbol.New("min"), 
                    (args, caller) => New(Compute(args, Math.Min, Num(args.First()))), 
                    1,
                    MaxInt, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(module <n1> <n2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("modulo"), 
                    (args, caller) => New(Modulo(args.First(), args.Second())), 
                    2, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(negative? <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("negative?"), 
                    (args, caller) => SchemeBoolean.Truth(Num(args.First()) < 0), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.6">(number->string <number>)</r4rs>
                //// <r4rs section="6.5.6">(number->string <number> <radix>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("number->string"), 
                    (args, caller) => NumberToString(args.First(), args.Second()), 
                    1, 
                    2, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(number? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("number?"), 
                    (args, caller) => SchemeBoolean.Truth(args.First().IsNumber()), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.5.5">(odd? <n>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("odd?"), 
                    (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(args.First())) % 2 != 0), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(positive? <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("positive?"), 
                    (args, caller) => SchemeBoolean.Truth(Num(args.First()) > 0), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(quotient <n1> <n2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("quotient"), 
                    (args, caller) => New(Quotient(args.First(), args.Second())), 
                    2, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(rational? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("rational?"), 
                    (args, caller) => SchemeBoolean.Truth(IsExact(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.5.5">(real? <obj>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("real?"), 
                    (args, caller) => SchemeBoolean.Truth(IsExact(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Obj)
                //// <r4rs section="6.5.5">(remainder <n1> <n2>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("remainder"), 
                    (args, caller) => New((long)Num(args.First()) % (long)Num(args.Second())), 
                    2, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(round <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("round"), 
                    (args, caller) => New(Math.Round(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(sin <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("sin"), 
                    (args, caller) => New(Math.Sin(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(sqrt <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("sqrt"), 
                    (args, caller) => New(Math.Sqrt(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(tan <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("tan"), 
                    (args, caller) => New(Math.Tan(Num(args.First()))), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(truncate <x>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("truncate"), 
                    (args, caller) => New(Truncate(args.First())), 
                    1, 
                    TypePrimitives.ValueType.Number)
                //// <r4rs section="6.5.5">(zero? <z>)</r4rs>
                .DefinePrimitive(
                    Symbol.New("zero?"), 
                    (args, caller) => SchemeBoolean.Truth(Num(args.First()) == 0), 
                    1, 
                    TypePrimitives.ValueType.Number);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Another name for As, works better here.
        /// </summary>
        /// <param name="obj">The object to cast.</param>
        /// <returns>The object, cast to double.</returns>
        private static double Num(Obj obj)
        {
            return obj.AsNumber().N;
        }

        /// <summary>
        /// Compute the greatest common divisor of a list of numbers.
        /// </summary>
        /// <param name="args">The list of numbers.</param>
        /// <returns>The greatest common divisor.</returns>
        private static double Gcd(Obj args)
        {
            long gcd = 0;
            while (args.IsPair())
            {
                gcd = Gcd2(Math.Abs((long)Num(args.First())), gcd);
                args = args.Rest();
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
        private static bool IsExact(Obj x)
        {
            if (!x.IsNumber())
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
        private static double Lcm(Obj args)
        {
            long lcm = 1;
            while (args.IsPair())
            {
                long n = Math.Abs((long)Num(args.First()));
                long g = Gcd2(n, lcm);
                lcm = g == 0 ? g : (n / g) * lcm;
                args = args.Rest();
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
        private static SchemeString NumberToString(Obj num, Obj numberBase)
        {
            double d = Num(num);
            int b = numberBase.IsNumber() ? (int)Num(numberBase) : 10;
            if (b != 10 || d == Math.Round(d))
            {
                return SchemeString.New(Convert.ToString((long)d, b).ToCharArray());
            }

            return SchemeString.New(num.ToString().ToCharArray());
        }

        /// <summary>
        /// Compare a set of numbers using a given comparison operator.
        /// Comparison stops when one of the results yields false.
        /// </summary>
        /// <param name="args">A list of numbers to compare.</param>
        /// <param name="comp">The compare operation to apply successively to pairs of 
        ///     adjacent numbers.</param>
        /// <returns>True only if all comparisons are true.</returns>
        private static SchemeBoolean Compare(Obj args, Func<double, double, bool> comp)
        {
            while (args.Rest().IsPair())
            {
                if (!comp(Num(args.First()), Num(args.Second())))
                {
                    return SchemeBoolean.False;
                }

                args = args.Rest();
            }

            return SchemeBoolean.True;
        }

        /// <summary>
        /// Compute an operation on a list of numbers.
        /// In addition to usual arithmetic operators, can also do unary - and /
        ///   and also max and min.
        /// </summary>
        /// <param name="args">The list of numbers to operate on.</param>
        /// <param name="oper">The operation to apply</param>
        /// <param name="initial">The initial value of the computation.</param>
        /// <returns>The final result of the operation.</returns>
        private static double Compute(Obj args, Func<double, double, double> oper, double initial)
        {
            // If there are no numbers, apply return initial value
            if (args.IsEmptyList())
            {
                return initial;
            }

            double resultSoFar = args.First().AsNumber().N;
            if (args.Rest().IsEmptyList())
            {
                // If there is one number, apply the operation on the initial value and value.
                return oper(initial, resultSoFar);
            }

            args = args.Rest();
            while (args.IsPair())
            {
                // If there are several numbers, apply the operation on values and partial results.
                resultSoFar = oper(resultSoFar, args.First().AsNumber().N);
                args = args.Rest();
            }

            return resultSoFar;
        }

        /// <summary>
        /// Calculate the first raised to the second power.
        /// </summary>
        /// <param name="first">The base number.</param>
        /// <param name="second">The exponent.</param>
        /// <returns>The base raised to the exponent.</returns>
        private static double Expt(Obj first, Obj second)
        {
            if (Num(first) == 0.0 && Num(second) < 0.0)
            {
                // Math.Pow gives infinity for this case
                return 0;
            }

            return Math.Pow(Num(first), Num(second));
        }

        /// <summary>
        /// Calculate the first modulo the second.
        /// </summary>
        /// <param name="first">The first number.</param>
        /// <param name="second">The second number.</param>
        /// <returns>The first modulo the second.</returns>
        private static double Modulo(Obj first, Obj second)
        {
            var x = (long)Num(first);
            var y = (long)Num(second);
            long m = x % y;
            return x * y > 0 || m == 0 ? m : m + y;
        }

        /// <summary>
        /// Calculate the first divided by the second.
        /// </summary>
        /// <param name="first">The first number.</param>
        /// <param name="second">The second number.</param>
        /// <returns>The first divided by the second.</returns>
        private static double Quotient(Obj first, Obj second)
        {
            double d = Num(first) / Num(second);
            return d > 0 ? Math.Floor(d) : Math.Ceiling(d);
        }

        /// <summary>
        /// Truncate the given number.
        /// </summary>
        /// <param name="x">The number to truncate.</param>
        /// <returns>The argument truncated.</returns>
        private static double Truncate(Obj x)
        {
            double d = Num(x);
            return d < 0.0D ? Math.Ceiling(d) : Math.Floor(d);
        }
        #endregion
    }

    #region Extension Class
    /// <summary>
    /// Extension class for Number
    /// </summary>
    public static class NumberExtension
    {
        /// <summary>
        /// Tests whether to given object is a scheme number.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme number.</returns>
        public static bool IsNumber(this Obj obj)
        {
            return Number.Is(obj);
        }

        /// <summary>
        /// Check that the object is a number.
        /// The actual value is contained in the Number.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding Number.</returns>
        public static Number AsNumber(this Obj x)
        {
            var asNumber = x as Number;
            if (asNumber != null)
            {
                return asNumber;
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return null;
        }

        /// <summary>
        /// Convert to a number.
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding Number.</returns>
        public static int AsInt(this Obj x)
        {
            if (Number.Is(x))
            {
                return Convert.ToInt32(x.AsNumber().N);
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0;
        }

        /// <summary>
        /// Convert to a double
        /// </summary>
        /// <param name="x">The object.</param>
        /// <returns>The corresponding double.</returns>
        public static double AsDouble(this Obj x)
        {
            if (Number.Is(x))
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
        public static float AsFloat(this Obj x)
        {
            if (Number.Is(x))
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
        public static short AsShort(this Obj x)
        {
            if (Number.Is(x))
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
        public static long AsLong(this Obj x)
        {
            if (Number.Is(x))
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
        public static byte AsByte(this Obj x)
        {
            if (Number.Is(x))
            {
                return (byte)((Number)x).N;
            }

            ErrorHandlers.TypeError(typeof(Number), x);
            return 0;
        }
    }
    #endregion
}
