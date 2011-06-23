// <copyright file="Number.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// Utilities that have to do with numbers.
    /// </summary>
    public sealed class Number : ListPrimitives
    {
        /// <summary>
        /// Define the zero object.
        /// </summary>
        private const double Zero = 0.0D;

        /// <summary>
        /// Define the one object.
        /// </summary>
        private const double One = 1.0D;

        /// <summary>
        /// Define the numeric primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
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
                .DefinePrimitive("=", (caller, args) => NumCompare(args, '='), 2, MaxInt)
                //// <r4rs section="6.5.5">(* <z1> ...)</r4rs>
                .DefinePrimitive("*", (caller, args) => NumCompute(args, '*', 1.0), 0, MaxInt)
                //// <r4rs section="6.5.5">(+ <z1> ...)</r4rs>
                .DefinePrimitive("+", (caller, args) => NumCompute(args, '+', 0.0), 0, MaxInt)
                //// <r4rs section="6.5.5">(- <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(- <z>)</r4rs>
                .DefinePrimitive("-", (caller, args) => NumCompute(Rest(args), '-', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(/ <z1> <z2>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z>)</r4rs>
                //// <r4rs section="6.5.5">(/ <z1> <z2> ...)</r4rs>
                .DefinePrimitive("/", (caller, args) => NumCompute(Rest(args), '/', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(< <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("<", (caller, args) => NumCompare(args, '<'), 2, MaxInt)
                //// <r4rs section="6.5.5">(> <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(">", (caller, args) => NumCompare(args, '>'), 2, MaxInt)
                //// <r4rs section="6.5.5">(<= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive("<=", (caller, args) => NumCompare(args, 'L'), 2, MaxInt)
                //// <r4rs section="6.5.5">(>= <z1> <z2> <z3> ...)</r4rs>
                .DefinePrimitive(">=", (caller, args) => NumCompare(args, 'G'), 2, MaxInt)
                //// <r4rs section="6.5.5">(abs <x>)</r4rs>
                .DefinePrimitive("abs", (caller, args) => Num(Math.Abs(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(ceiling <x>)</r4rs>
                .DefinePrimitive("ceiling", (caller, args) => Num(Math.Ceiling(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(floor <x>)</r4rs>
                .DefinePrimitive("floor", (caller, args) => Num(Math.Floor(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(acos <z>)</r4rs>
                .DefinePrimitive("acos", (caller, args) => Num(Math.Acos(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(asin <z>)</r4rs>
                .DefinePrimitive("asin", (caller, args) => Num(Math.Asin(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(atan <z>)</r4rs>
                //// <r4rs section="6.5.5">(atan <y> <x>)</r4rs>
                .DefinePrimitive("atan", (caller, args) => Num(Math.Atan(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(complex? <obj>)</r4rs>
                .DefinePrimitive(
                   "complex?", 
                   (caller, args) =>
                            {
                                object first = First(args);
                                return SchemeBoolean.Truth(first is byte || first is int || first is long ||
                                                    first is float || first is double);
                            },
                   1)
                //// <r4rs section="6.5.5">(cos <z>)</r4rs>
                .DefinePrimitive("cos", (caller, args) => Num(Math.Cos(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(even? <n>)</r4rs>
                .DefinePrimitive("even?", (caller, args) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 == 0), 1)
                //// <r4rs section="6.5.5">(exact? <obj>)</r4rs>
                .DefinePrimitive("exact?", (caller, args) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(exp <z>)</r4rs>
                .DefinePrimitive("exp", (caller, args) => Num(Math.Exp(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(expt <z>)</r4rs>
                .DefinePrimitive(
                   "expt",
                   (caller, args) =>
                   {
                       object first = First(args);
                       object second = Second(args);
                       if (Num(first) == 0.0 && Num(second) < 0.0)
                       {
                           // Math.Pow gives infinity for this case
                           return Num(0.0);
                       }

                       return Num(Math.Pow(Num(first), Num(second)));
                   },
                    2)
                //// <r4rs section="6.5.5">(gcd <n1> ...)</r4rs>
                .DefinePrimitive("gcd", (caller, args) => args == null ? Zero : Gcd(args), 0, MaxInt)
                //// <r4rs section="6.5.5">(inexact? <obj>)</r4rs>
                .DefinePrimitive("inexact?", (caller, args) => SchemeBoolean.Truth(!IsExact(First(args))), 1)
                //// <r4rs section="6.6">(integer->char <n>)</r4rs>
                .DefinePrimitive("integer->char", (caller, args) => SchemeString.Chr((char)(int)Num(First(args))), 1)
                //// <r4rs section="6.5.5">(integer? <obj>)</r4rs>
                .DefinePrimitive("integer?", (caller, args) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(lcm <n1> ...)</r4rs>
                .DefinePrimitive("lcm", (caller, args) => args == null ? One : Lcm(args), 0, MaxInt)
                //// <r4rs section="6.5.5">(log <z>)</r4rs>
                .DefinePrimitive("log", (caller, args) => Num(Math.Log(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(max? <x1> <x2> ...)</r4rs>
                .DefinePrimitive("max", (caller, args) => NumCompute(args, 'X', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(min? <x1> <x2> ...)</r4rs>
                .DefinePrimitive("min", (caller, args) => NumCompute(args, 'N', Num(First(args))), 1, MaxInt)
                //// <r4rs section="6.5.5">(module <n1> <n2>)</r4rs>
                .DefinePrimitive(
                   "modulo",
                   (caller, args) =>
                   {
                       long xi = (long)Num(First(args));
                       long yi = (long)Num(Second(args));
                       long m = xi % yi;
                       return Num(xi * yi > 0 || m == 0 ? m : m + yi);
                   },
                    2)
                //// <r4rs section="6.5.5">(negative? <x>)</r4rs>
                .DefinePrimitive("negative?", (caller, args) => SchemeBoolean.Truth(Num(First(args)) < 0), 1)
                //// <r4rs section="6.5.6">(number->string <number>)</r4rs>
                //// <r4rs section="6.5.6">(number->string <number> <radix>)</r4rs>
                .DefinePrimitive("number->string", (caller, args) => NumberToString(First(args), Second(args)), 1, 2)
                //// <r4rs section="6.5.5">(number? <obj>)</r4rs>
                .DefinePrimitive(
                   "number?",
                   (caller, args) =>
                   {
                       object first = First(args);
                       return SchemeBoolean.Truth(first is byte || first is int || first is long ||
                                           first is float || first is double);
                   },
                    1)
                //// <r4rs section="6.5.5">(odd? <n>)</r4rs>
                .DefinePrimitive("odd?", (caller, args) => SchemeBoolean.Truth(Math.Abs(Num(First(args))) % 2 != 0), 1)
                //// <r4rs section="6.5.5">(positive? <x>)</r4rs>
                .DefinePrimitive("positive?", (caller, args) => SchemeBoolean.Truth(Num(First(args)) > 0), 1)
                //// <r4rs section="6.5.5">(quotient <n1> <n2>)</r4rs>
                .DefinePrimitive(
                   "quotient",
                   (caller, args) =>
                   {
                       double d = Num(First(args)) / Num(Second(args));
                       return Num(d > 0 ? Math.Floor(d) : Math.Ceiling(d));
                   },
                    2)
                //// <r4rs section="6.5.5">(rational? <obj>)</r4rs>
                .DefinePrimitive("rational?", (caller, args) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(real? <obj>)</r4rs>
                .DefinePrimitive("real?", (caller, args) => SchemeBoolean.Truth(IsExact(First(args))), 1)
                //// <r4rs section="6.5.5">(remainder <n1> <n2>)</r4rs>
                .DefinePrimitive("remainder", (caller, args) => Num((long)Num(First(args)) % (long)Num(Second(args))), 2)
                //// <r4rs section="6.5.5">(round <x>)</r4rs>
                .DefinePrimitive("round", (caller, args) => Num(Math.Round(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(sin <z>)</r4rs>
                .DefinePrimitive("sin", (caller, args) => Num(Math.Sin(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(sqrt <z>)</r4rs>
                .DefinePrimitive("sqrt", (caller, args) => Num(Math.Sqrt(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(tan <z>)</r4rs>
                .DefinePrimitive("tan", (caller, args) => Num(Math.Tan(Num(First(args)))), 1)
                //// <r4rs section="6.5.5">(truncate <x>)</r4rs>
                .DefinePrimitive(
                    "truncate",
                    (caller, args) =>
                    {
                        double d = Num(First(args));
                        return Num(d < 0.0D ? Math.Ceiling(d) : Math.Floor(d));
                    },
                   1)
                //// <r4rs section="6.5.5">(zero? <z>)</r4rs>
                .DefinePrimitive("zero?", (caller, args) => SchemeBoolean.Truth(Num(First(args)) == 0), 1);
        }

        /// <summary>
        /// Convert an object (containing a number) into a double.
        /// </summary>
        /// <param name="x">The object to convert.</param>
        /// <returns>The double contained in the object.</returns>
        public static double Num(object x)
        {
            try
            {
                return Convert.ToDouble(x);
            }
            catch (InvalidCastException)
            {
                return Num(ErrorHandlers.Error("Expected a number, got: " + x));
            }
            catch (FormatException)
            {
                return Num(ErrorHandlers.Error("Expected a number, got: " + x));
            }
            catch (OverflowException)
            {
                return Num(ErrorHandlers.Error("Number overflow, got: " + x));
            }
        }

        /// <summary>
        /// Compute the greatest common divisor of a list of numbers.
        /// </summary>
        /// <param name="args">The list of numbers.</param>
        /// <returns>The greatest common divisor.</returns>
        private static object Gcd(object args)
        {
            long gcd = 0;

            if (args is Pair)
            {
                foreach (var elem in (Pair)args)
                {
                    gcd = Gcd2(Math.Abs((long)Num(elem)), gcd);
                }
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
        private static bool IsExact(object x)
        {
            if (!(x is double))
            {
                return false;
            }

            double d = Num(x);
            return d == Math.Round(d) && Math.Abs(d) < 102962884861573423.0;
        }

        /// <summary>
        /// Compute the least common multiple of a set of numbers.
        /// </summary>
        /// <param name="args">The numbers to use.</param>
        /// <returns>The LCM of these numbers.</returns>
        private static object Lcm(object args)
        {
            long lcm = 1;

            if (args is Pair)
            {
                foreach (var elem in (Pair)args)
                {
                    long n = Math.Abs((long)Num(elem));
                    long g = Gcd2(n, lcm);
                    lcm = g == 0 ? g : (n / g) * lcm;
                }
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
        private static object NumberToString(object x, object y)
        {
            int numberBase = y is double ? (int)Num(y) : 10;
            if (numberBase != 10 || Num(x) == Math.Round(Num(x)))
            {
                return new SchemeString(Convert.ToString((long)Num(x), numberBase));
            }

            return new SchemeString(x.ToString());
        }

        /// <summary>
        /// Compare a set of numbers using a given comparison operator.
        /// Comparison stops when one of the results yields false.
        /// </summary>
        /// <param name="args">A list of numbers to compare.</param>
        /// <param name="op">The operation to apply successively to pairs of 
        ///     adjacent numbers.</param>
        /// <returns>True only if all comparisons are true.</returns>
        private static object NumCompare(object args, char op)
        {
            while (Rest(args) is Pair)
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
                        ErrorHandlers.Error("Internal error: unrecognized number compare flag: " + op);
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
        private static object NumCompute(object args, char op, double result)
        {
            if (args == null)
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

            while (args is Pair)
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
                        ErrorHandlers.Error("Internal error: unrecognized number compute flag: " + op);
                        break;
                }

                args = Rest(args);
            }

            return Num(result);
        }
    }
}
