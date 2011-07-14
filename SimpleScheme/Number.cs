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
    public sealed class Number : ListPrimitives
    {
        #region Constants
        /// <summary>
        /// Define the zero obj.
        /// </summary>
        internal const double Zero = 0.0D;

        /// <summary>
        /// Define the one obj.
        /// </summary>
        internal const double One = 1.0D;
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Convert an obj (containing a number) into a double.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <returns>The double contained in the obj.</returns>
        public static double Num(Obj x)
        {
            if (IsType(x))
            {
                return (double)x;
            }

            if (x is byte || x is int || x is long || x is float)
            {
                return Convert.ToDouble(x);
            }

            return Num(ErrorHandlers.TypeError(TypeName(), x));
        }

        /// <summary>
        /// Convert the number a string.
        /// </summary>
        /// <param name="obj">The number to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        internal static void AsString(Obj obj, bool quoted, StringBuilder buf)
        {
            double d = Num(obj);
            if (Math.Round(d) == d)
            {
                buf.Append((long)d);
            }
            else
            {
                buf.Append(d);
            }
        }

        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the numeric primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        internal static void DefinePrimitives(Environment env)
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
                //// <r4rs section="6.5.5">(= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("=", (args, caller) => NumCompare(args, '='), 2, MaxInt)
                //// <r4rs section="6.5.5">(* <z1> ...)</r4rs>
                .DefinePrimitive("*", (args, caller) => NumCompute(args, '*', 1.0), 0, MaxInt)
                //// <r4rs section="6.5.5">(+ <z1> ...)</r4rs>
                .DefinePrimitive("+", (args, caller) => NumCompute(args, '+', 0.0), 0, MaxInt)
                //// <r4rs section="6.5.5">(- <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(- <z>)</r4rs>
                .DefinePrimitive("-", (args, caller) => NumCompute(Rest(args), '-', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(/ <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z1> <z2> ...)</r4rs>
                .DefinePrimitive("/", (args, caller) => NumCompute(Rest(args), '/', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("=", (args, caller) => NumCompare(args, '='), 2, MaxInt)
                //// <r4rs section="6.5.5">(< <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("<", (args, caller) => NumCompare(args, '<'), 2, MaxInt)
                //// <r4rs section="6.5.5">(> <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(">", (args, caller) => NumCompare(args, '>'), 2, MaxInt)
                //// <r4rs section="6.5.5">(<= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("<=", (args, caller) => NumCompare(args, 'L'), 2, MaxInt)
                //// <r4rs section="6.5.5">(>= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(">=", (args, caller) => NumCompare(args, 'G'), 2, MaxInt)
                //// <r4rs section="6.5.5">(abs <x>)</r4rs>
                .DefinePrimitive("abs", (args, caller) => Num(Math.Abs(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(ceiling <x>)</r4rs>
                .DefinePrimitive("ceiling", (args, caller) => Num(Math.Ceiling(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(floor <x>)</r4rs>
                .DefinePrimitive("floor", (args, caller) => Num(Math.Floor(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(acos <z>)</r4rs>
                .DefinePrimitive("acos", (args, caller) => Num(Math.Acos(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(asin <z>)</r4rs>
                .DefinePrimitive("asin", (args, caller) => Num(Math.Asin(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(atan <z>)</r4rs>
                //// <r4rs section="6.5.5">(atan <y> <x>)</r4rs>
                .DefinePrimitive("atan", (args, caller) => Num(Math.Atan(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(complex? <obj>)</r4rs>
                .DefinePrimitive("complex?", (args, caller) => IsNumber(First(args)), 1)
                //// <r4rs section="6.5.5">(cos <z>)</r4rs>
                .DefinePrimitive("cos", (args, caller) => Num(Math.Cos(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(even? <n>)</r4rs>
                .DefinePrimitive("even?", (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 == 0), 1)
                //// <r4rs section="6.5.5">(exact? <obj>)</r4rs>
                .DefinePrimitive("exact?", (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(exp <z>)</r4rs>
                .DefinePrimitive("exp", (args, caller) => Num(Math.Exp(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(expt <z>)</r4rs>
                .DefinePrimitive("expt", (args, caller) => Expt(First(args), Second(args)), 2)
                //// <r4rs section="6.5.5">(gcd <n1> ...)</r4rs>
                .DefinePrimitive("gcd", (args, caller) => EmptyList.IsType(args) ? Zero : Gcd(args), 0, MaxInt)
                //// <r4rs section="6.5.5">(inexact? <obj>)</r4rs>
                .DefinePrimitive("inexact?", (args, caller) => SchemeBoolean.Truth(!IsExact(First(args))), 1)
                //// <r4rs section="6.6">(integer->char <n>)</r4rs>
                .DefinePrimitive("integer->char", (args, caller) => Character.Chr((char)(int)Num(First(args))), 1)
                //// <r4rs section="6.5.5">(integer? <obj>)</r4rs>
                .DefinePrimitive("integer?", (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(lcm <n1> ...)</r4rs>
                .DefinePrimitive("lcm", (args, caller) => EmptyList.IsType(args) ? One : Lcm(args), 0, MaxInt)
                //// <r4rs section="6.5.5">(log <z>)</r4rs>
                .DefinePrimitive("log", (args, caller) => Num(Math.Log(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(max? <x1> <x2> ...)</r4rs>
                .DefinePrimitive("max", (args, caller) => NumCompute(args, 'X', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(min? <x1> <x2> ...)</r4rs>
                .DefinePrimitive("min", (args, caller) => NumCompute(args, 'N', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(module <n1> <n2>)</r4rs>
                .DefinePrimitive("modulo", (args, caller) => Modulo(First(args), Second(args)), 2)
                //// <r4rs section="6.5.5">(negative? <x>)</r4rs>
                .DefinePrimitive("negative?", (args, caller) => SchemeBoolean.Truth(Num(First(args)) < 0), 1)
                //// <r4rs section="6.5.6">(number->string <number>)</r4rs>
                //// <r4rs section="6.5.6">(number->string <number> <radix>)</r4rs>
                .DefinePrimitive("number->string", (args, caller) => NumberToString(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.5.5">(number? <obj>)</r4rs>
                .DefinePrimitive("number?", (args, caller) => IsNumber(First(args)), 1)
                //// <r4rs section="6.5.5">(odd? <n>)</r4rs>
                .DefinePrimitive("odd?", (args, caller) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 != 0), 1)
                //// <r4rs section="6.5.5">(positive? <x>)</r4rs>
                .DefinePrimitive("positive?", (args, caller) => SchemeBoolean.Truth(Num(First(args)) > 0), 1)
                //// <r4rs section="6.5.5">(quotient <n1> <n2>)</r4rs>
                .DefinePrimitive("quotient", (args, caller) => Quotient(First(args), Second(args)), 2)
                //// <r4rs section="6.5.5">(rational? <obj>)</r4rs>
                .DefinePrimitive("rational?", (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(real? <obj>)</r4rs>
                .DefinePrimitive("real?", (args, caller) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(remainder <n1> <n2>)</r4rs>
                .DefinePrimitive("remainder", (args, caller) => Num((long)Num(First(args)) % (long)Num(Second(args))), 2)
                //// <r4rs section="6.5.5">(round <x>)</r4rs>
                .DefinePrimitive("round", (args, caller) => Num(Math.Round(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(sin <z>)</r4rs>
                .DefinePrimitive("sin", (args, caller) => Num(Math.Sin(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(sqrt <z>)</r4rs>
                .DefinePrimitive("sqrt", (args, caller) => Num(Math.Sqrt(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(tan <z>)</r4rs>
                .DefinePrimitive("tan", (args, caller) => Num(Math.Tan(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(truncate <x>)</r4rs>
                .DefinePrimitive("truncate", (args, caller) => Truncate(First(args)), 1)
                //// <r4rs section="6.5.5">(zero? <z>)</r4rs>
                .DefinePrimitive("zero?", (args, caller) => SchemeBoolean.Truth(Num(First(args)) == 0), 1);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Test an object's type.
        /// </summary>
        /// <param name="obj">The object to test.</param>
        /// <returns>True if the object is a scheme number.</returns>
        internal static bool IsType(Obj obj)
        {
            return obj is double;
        }

        /// <summary>
        /// Give the name of the type (for display).
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string TypeName()
        {
            return "number";
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

            while (Pair.IsType(args))
            {
                gcd = Gcd2(Math.Abs((long)Num(First(args))), gcd);
                args = Rest(args);
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
        /// </summary>
        /// <param name="x">The number to test.</param>
        /// <returns>True if the number is exact.</returns>
        private static bool IsExact(Obj x)
        {
            if (!IsType(x))
            {
                return false;
            }

            double d = Num(x);
            return d == Math.Round(d) && Math.Abs(d) < 102962884861573423.0;
        }

        /// <summary>
        /// Tests if a object is a number.
        /// </summary>
        /// <param name="x">The number to test.</param>
        /// <returns>True if it is a number.</returns>
        private static bool IsNumber(Obj x)
        {
            return SchemeBoolean.Truth(x is byte || x is int || x is long ||
                                       x is float || x is double);
        }

        /// <summary>
        /// Compute the least common multiple of a set of numbers.
        /// </summary>
        /// <param name="args">The numbers to use.</param>
        /// <returns>The LCM of these numbers.</returns>
        private static Obj Lcm(Obj args)
        {
            long lcm = 1;

            while (Pair.IsType(args))
            {
                long n = Math.Abs((long)Num(First(args)));
                long g = Gcd2(n, lcm);
                lcm = g == 0 ? g : (n / g) * lcm;
                args = Rest(args);
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
            int numberBase = IsType(y) ? (int)Num(y) : 10;
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
        /// <param name="op">The operation to apply successively to pairs of 
        ///     adjacent numbers.</param>
        /// <returns>True only if all comparisons are true.</returns>
        private static Obj NumCompare(Obj args, char op)
        {
            while (Pair.IsType(Rest(args)))
            {
                double x = Num(First(args));
                args = Rest(args);

                double y = Num(First(args));

                switch (op)
                {
                    case '>':
                        if (!(x > y))
                        {
                            return SchemeBoolean.False;
                        }

                        break;

                    case '<':
                        if (!(x < y))
                        {
                            return SchemeBoolean.False;
                        }

                        break;

                    case '=':
                        if (x != y)
                        {
                            return SchemeBoolean.False;
                        }

                        break;

                    case 'L':
                        if (!(x <= y))
                        {
                            return SchemeBoolean.False;
                        }

                        break;

                    case 'G':
                        if (!(x >= y))
                        {
                            return SchemeBoolean.False;
                        }

                        break;

                    default:
                        ErrorHandlers.InternalError("Internal error: bad option to NumCompare: " + op);
                        break;
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
        /// <param name="op">The operation to apply</param>
        /// <param name="result">The starting value.</param>
        /// <returns>The result of applying the operation to the list, starting 
        /// with the starting value.</returns>
        private static Obj NumCompute(Obj args, char op, double result)
        {
            if (EmptyList.IsType(args))
            {
                // If there are no numbers, apply a unary operation on the starting value.
                switch (op)
                {
                    case '-':
                        return Num(0 - result);

                    case '/':
                        return Num(1 / result);

                    default:
                        return Num(result);
                }
            }

            while (Pair.IsType(args))
            {
                double x = Num(First(args));
                switch (op)
                {
                    case 'X': // max
                        if (x > result)
                        {
                            result = x;
                        }

                        break;

                    case 'N': // min
                        if (x < result)
                        {
                            result = x;
                        }

                        break;

                    case '+':
                        result += x;
                        break;

                    case '-':
                        result -= x;
                        break;

                    case '*':
                        result *= x;
                        break;

                    case '/':
                        result /= x;
                        break;

                    default:
                        ErrorHandlers.InternalError("Internal error: bad option to NumCompute: " + op);
                        break;
                }

                args = Rest(args);
            }

            return Num(result);
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
                return Num(0.0);
            }

            return Num(Math.Pow(Num(first), Num(second)));
        }

        /// <summary>
        /// Calculate the first modulo the second.
        /// </summary>
        /// <param name="first">The first number.</param>
        /// <param name="second">The second number.</param>
        /// <returns>The first modulo the second.</returns>
        private static Obj Modulo(Obj first, Obj second)
        {
            long xi = (long)Num(first);
            long yi = (long)Num(second);
            long m = xi % yi;
            return Num(xi * yi > 0 || m == 0 ? m : m + yi);
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
            return Num(d > 0 ? Math.Floor(d) : Math.Ceiling(d));
        }

        /// <summary>
        /// Truncate the given number.
        /// </summary>
        /// <param name="x">The number to truncate.</param>
        /// <returns>The argument truncated.</returns>
        private static Obj Truncate(Obj x)
        {
            double d = Num(x);
            return Num(d < 0.0D ? Math.Ceiling(d) : Math.Floor(d));
        }
        #endregion
    }
}
