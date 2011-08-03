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
    public static class Number
    {
        #region Constants
        /// <summary>
        /// Define the zero obj.
        /// </summary>
        public const double Zero = 0.0D;

        /// <summary>
        /// Define the one obj.
        /// </summary>
        public const double One = 1.0D;

        /// <summary>
        /// The printable name of the scheme number type.
        /// </summary>
        private const string Name = "number";
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is a scheme number.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme number.</returns>
        public static bool IsNumber(Obj obj)
        {
            return obj is double || obj is float || obj is long || obj is int || obj is byte;
        }

        /// <summary>
        /// Convert an obj (containing a number) into a double.
        /// </summary>
        /// <param name="obj">The obj to convert.</param>
        /// <returns>The double contained in the obj.</returns>
        public static double Num(Obj obj)
        {
            if (obj is double)
            {
                return (double)obj;
            }

            if (obj is byte || obj is int || obj is long || obj is float)
            {
                return Convert.ToDouble(obj);
            }

            ErrorHandlers.TypeError(Name, obj);
            return 0.0;
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
            //// <r4rs section="6.5.5">(exact->inexact <z>)</r4rs>
            //// <r4rs section="6.5.5">(inexact->exact <z>)</r4rs>

            const int MaxInt = int.MaxValue;
            env
                //// <r4rs section="6.5.5">(* <z1> ...)</r4rs>
                .DefinePrimitive("*", (args, caller) => NumCompute(args, (x, y) => x * y, 1.0), 0, MaxInt)
                //// <r4rs section="6.5.5">(+ <z1> ...)</r4rs>
                .DefinePrimitive("+", (args, caller) => NumCompute(args, (x, y) => x + y, 0.0), 0, MaxInt)
                //// <r4rs section="6.5.5">(- <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(- <z>)</r4rs>
                .DefinePrimitive("-", (args, caller) => NumCompute(args, (x, y) => x - y, 0.0), 1, MaxInt)
                //// <r4rs section="6.5.5">(/ <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z1> <z2> ...)</r4rs>
                .DefinePrimitive("/", (args, caller) => NumCompute(args, (x, y) => x / y, 1.0), 1, MaxInt)
                //// <r4rs section="6.5.5">(= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("=", (args, caller) => NumCompare(args, (x, y) => x == y), 2, MaxInt)
                //// <r4rs section="6.5.5">(< <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("<", (args, caller) => NumCompare(args, (x, y) => x < y), 2, MaxInt)
                //// <r4rs section="6.5.5">(> <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(">", (args, caller) => NumCompare(args, (x, y) => x > y), 2, MaxInt)
                //// <r4rs section="6.5.5">(<= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("<=", (args, caller) => NumCompare(args, (x, y) => x <= y), 2, MaxInt)
                //// <r4rs section="6.5.5">(>= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(">=", (args, caller) => NumCompare(args, (x, y) => x >= y), 2, MaxInt)
                //// <r4rs section="6.5.5">(abs <x>)</r4rs>
                .DefinePrimitive("abs", (args, caller) => Num(Math.Abs(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(ceiling <x>)</r4rs>
                .DefinePrimitive("ceiling", (args, caller) => Num(Math.Ceiling(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(floor <x>)</r4rs>
                .DefinePrimitive("floor", (args, caller) => Num(Math.Floor(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(acos <z>)</r4rs>
                .DefinePrimitive("acos", (args, caller) => Num(Math.Acos(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(asin <z>)</r4rs>
                .DefinePrimitive("asin", (args, caller) => Num(Math.Asin(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(atan <z>)</r4rs>
                //// <r4rs section="6.5.5">(atan <y> <x>)</r4rs>
                .DefinePrimitive("atan", (args, caller) => Num(Math.Atan(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(complex? <obj>)</r4rs>
                .DefinePrimitive("complex?", (args, caller) => IsSchemeNumber(List.First(args)), 1)
                //// <r4rs section="6.5.5">(cos <z>)</r4rs>
                .DefinePrimitive("cos", (args, caller) => Num(Math.Cos(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(even? <n>)</r4rs>
                .DefinePrimitive("even?", (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(List.First(args))) % 2 == 0), 1)
                //// <r4rs section="6.5.5">(exact? <obj>)</r4rs>
                .DefinePrimitive("exact?", (args, caller) => SchemeBoolean.Truth(IsExact(List.First(args))), 1)
                //// <r4rs section="6.5.5">(exp <z>)</r4rs>
                .DefinePrimitive("exp", (args, caller) => Num(Math.Exp(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(expt <z>)</r4rs>
                .DefinePrimitive("expt", (args, caller) => Expt(List.First(args), List.Second(args)), 2)
                //// <r4rs section="6.5.5">(gcd <n1> ...)</r4rs>
                .DefinePrimitive("gcd", (args, caller) => EmptyList.IsEmptyList(args) ? Zero : Gcd(args), 0, MaxInt)
                //// <r4rs section="6.5.5">(inexact? <obj>)</r4rs>
                .DefinePrimitive("inexact?", (args, caller) => SchemeBoolean.Truth(!IsExact(List.First(args))), 1)
                //// <r4rs section="6.6">(integer->char <n>)</r4rs>
                .DefinePrimitive("integer->char", (args, caller) => Character.AsCharacter((char)(int)Num(List.First(args))), 1)
                //// <r4rs section="6.5.5">(integer? <obj>)</r4rs>
                .DefinePrimitive("integer?", (args, caller) => SchemeBoolean.Truth(IsExact(List.First(args))), 1)
                //// <r4rs section="6.5.5">(lcm <n1> ...)</r4rs>
                .DefinePrimitive("lcm", (args, caller) => EmptyList.IsEmptyList(args) ? One : Lcm(args), 0, MaxInt)
                //// <r4rs section="6.5.5">(log <z>)</r4rs>
                .DefinePrimitive("log", (args, caller) => Num(Math.Log(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(max? <x1> <x2> ...)</r4rs>
                .DefinePrimitive("max", (args, caller) => NumCompute(args, (x, y) => Math.Max(x, y), Num(List.First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(min? <x1> <x2> ...)</r4rs>
                .DefinePrimitive("min", (args, caller) => NumCompute(args, (x, y) => Math.Min(x, y), Num(List.First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(module <n1> <n2>)</r4rs>
                .DefinePrimitive("modulo", (args, caller) => Modulo(List.First(args), List.Second(args)), 2)
                //// <r4rs section="6.5.5">(negative? <x>)</r4rs>
                .DefinePrimitive("negative?", (args, caller) => SchemeBoolean.Truth(Num(List.First(args)) < 0), 1)
                //// <r4rs section="6.5.6">(number->string <number>)</r4rs>
                //// <r4rs section="6.5.6">(number->string <number> <radix>)</r4rs>
                .DefinePrimitive("number->string", (args, caller) => NumberToString(List.First(args), List.Second(args)), 1, 2)
                //// <r4rs section="6.5.5">(number? <obj>)</r4rs>
                .DefinePrimitive("number?", (args, caller) => IsNumber(List.First(args)), 1)
                //// <r4rs section="6.5.5">(odd? <n>)</r4rs>
                .DefinePrimitive("odd?", (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(List.First(args))) % 2 != 0), 1)
                //// <r4rs section="6.5.5">(positive? <x>)</r4rs>
                .DefinePrimitive("positive?", (args, caller) => SchemeBoolean.Truth(Num(List.First(args)) > 0), 1)
                //// <r4rs section="6.5.5">(quotient <n1> <n2>)</r4rs>
                .DefinePrimitive("quotient", (args, caller) => Quotient(List.First(args), List.Second(args)), 2)
                //// <r4rs section="6.5.5">(rational? <obj>)</r4rs>
                .DefinePrimitive("rational?", (args, caller) => SchemeBoolean.Truth(IsExact(List.First(args))), 1)
                //// <r4rs section="6.5.5">(real? <obj>)</r4rs>
                .DefinePrimitive("real?", (args, caller) => SchemeBoolean.Truth(IsExact(List.First(args))), 1)
                //// <r4rs section="6.5.5">(remainder <n1> <n2>)</r4rs>
                .DefinePrimitive("remainder", (args, caller) => Num((long)Num(List.First(args)) % (long)Num(List.Second(args))), 2)
                //// <r4rs section="6.5.5">(round <x>)</r4rs>
                .DefinePrimitive("round", (args, caller) => Num(Math.Round(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(sin <z>)</r4rs>
                .DefinePrimitive("sin", (args, caller) => Num(Math.Sin(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(sqrt <z>)</r4rs>
                .DefinePrimitive("sqrt", (args, caller) => Num(Math.Sqrt(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(tan <z>)</r4rs>
                .DefinePrimitive("tan", (args, caller) => Num(Math.Tan(Num(List.First(args)))), 1)
                //// <r4rs section="6.5.5">(truncate <x>)</r4rs>
                .DefinePrimitive("truncate", (args, caller) => Truncate(List.First(args)), 1)
                //// <r4rs section="6.5.5">(zero? <z>)</r4rs>
                .DefinePrimitive("zero?", (args, caller) => SchemeBoolean.Truth(Num(List.First(args)) == 0), 1);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Compute the greatest common divisor of a list of numbers.
        /// </summary>
        /// <param name="args">The list of numbers.</param>
        /// <returns>The greatest common divisor.</returns>
        private static Obj Gcd(Obj args)
        {
            long gcd = 0;

            while (Pair.IsPair(args))
            {
                gcd = Gcd2(Math.Abs((long)Num(List.First(args))), gcd);
                args = List.Rest(args);
            }

            return Num(gcd);
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
            if (!IsNumber(x))
            {
                return false;
            }

            double d = Num(x);
            return d == Math.Round(d) && Math.Abs(d) < 9007199254740992.0;
        }

        /// <summary>
        /// Tests if a object is a number.
        /// </summary>
        /// <param name="x">The number to test.</param>
        /// <returns>True if it is a number.</returns>
        private static bool IsSchemeNumber(Obj x)
        {
            return SchemeBoolean.Truth(IsNumber(x));
        }

        /// <summary>
        /// Compute the least common multiple of a set of numbers.
        /// </summary>
        /// <param name="args">The numbers to use.</param>
        /// <returns>The LCM of these numbers.</returns>
        private static Obj Lcm(Obj args)
        {
            long lcm = 1;

            while (Pair.IsPair(args))
            {
                long n = Math.Abs((long)Num(List.First(args)));
                long g = Gcd2(n, lcm);
                lcm = g == 0 ? g : (n / g) * lcm;
                args = List.Rest(args);
            }

            return Num(lcm);
        }

        /// <summary>
        /// Convert a number to its string equivalent.
        /// Optionally, use a number base different from 10.
        /// </summary>
        /// <param name="x">The number to convert.</param>
        /// <param name="y">The number base.</param>
        /// <returns>A string version of the number.</returns>
        private static Obj NumberToString(Obj x, Obj y)
        {
            int numberBase = IsNumber(y) ? (int)Num(y) : 10;
            if (numberBase != 10 || Num(x) == Math.Round(Num(x)))
            {
                return Convert.ToString((long)Num(x), numberBase).ToCharArray();
            }

            return x.ToString().ToCharArray();
        }

        /// <summary>
        /// Compare a set of numbers using a given comparison operator.
        /// Comparison stops when one of the results yields false.
        /// </summary>
        /// <param name="args">A list of numbers to compare.</param>
        /// <param name="comp">The compare operation to apply successively to pairs of 
        ///     adjacent numbers.</param>
        /// <returns>True only if all comparisons are true.</returns>
        private static Obj NumCompare(Obj args, Func<double, double, bool> comp)
        {
            while (Pair.IsPair(List.Rest(args)))
            {
                double x = Num(List.First(args));
                args = List.Rest(args);

                double y = Num(List.First(args));

                if (! comp(x, y))
                {
                    return SchemeBoolean.False;
                }
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
        private static Obj NumCompute(Obj args, Func<double, double, double> oper, double initial)
        {
            double resultSoFar = initial;

            // If there are no numbers, apply return initial value
            if (EmptyList.IsEmptyList(args))
            {
                return resultSoFar;
            }

            double d = Num(List.First(args));
            if (EmptyList.IsEmptyList(List.Rest(args)))
            {
                // If there is one number, apply a unary operation on the value.
                return oper(resultSoFar, d);
            }

            resultSoFar = d;

            args = List.Rest(args);
            while (Pair.IsPair(args))
            {
                d = Num(List.First(args));
                resultSoFar = oper(resultSoFar, d);
                args = List.Rest(args);
            }

            return resultSoFar;
        }

        /// <summary>
        /// Calculate the first raised to the second power.
        /// </summary>
        /// <param name="first">The base number.</param>
        /// <param name="second">The exponent.</param>
        /// <returns>The base raised to the exponent.</returns>
        private static Obj Expt(Obj first, Obj second)
        {
            if (Num(first) == 0.0 && Num(second) < 0.0)
            {
                // Math.Pow gives infinity for this case
                return 0.0;
            }

            return Math.Pow(Num(first), Num(second));
        }

        /// <summary>
        /// Calculate the first modulo the second.
        /// </summary>
        /// <param name="first">The first number.</param>
        /// <param name="second">The second number.</param>
        /// <returns>The first modulo the second.</returns>
        private static Obj Modulo(Obj first, Obj second)
        {
            long x = (long)Num(first);
            long y = (long)Num(second);
            long m = x % y;
            return x * y > 0 || m == 0 ? m : m + y;
        }

        /// <summary>
        /// Calculate the first divided by the second.
        /// </summary>
        /// <param name="first">The first number.</param>
        /// <param name="second">The second number.</param>
        /// <returns>The first divided by the second.</returns>
        private static Obj Quotient(Obj first, Obj second)
        {
            double d = Num(first) / Num(second);
            return d > 0 ? Math.Floor(d) : Math.Ceiling(d);
        }

        /// <summary>
        /// Truncate the given number.
        /// </summary>
        /// <param name="x">The number to truncate.</param>
        /// <returns>The argument truncated.</returns>
        private static Obj Truncate(Obj x)
        {
            double d = Num(x);
            return d < 0.0D ? Math.Ceiling(d) : Math.Floor(d);
        }
        #endregion
    }

    #region Extensions
    /// <summary>
    /// Provide common operations as extensions.
    /// </summary>
    public static partial class Extensions
    {
        /// <summary>
        /// Write the number to the string builder.
        /// </summary>
        /// <param name="d">The number.</param>
        /// <param name="quoted">Whether to quote (not used).</param>
        /// <param name="buf">The string builder to write to.</param>
        public static void AsString(this double d, bool quoted, StringBuilder buf)
        {
            if (d == Math.Round(d))
            {
                buf.Append((long)d);
            }
            else
            {
                buf.Append(d);
            }
        }
    }
    #endregion
}
