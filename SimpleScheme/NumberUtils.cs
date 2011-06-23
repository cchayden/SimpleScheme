// <copyright file="NumberUtils.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
using System;

namespace SimpleScheme
{
    class NumberUtils : SchemeUtils
    {
        /// <summary>
        /// Compute the greatest common divisor of a list of numbers.
        /// </summary>
        /// <param name="args">The list of numbers.</param>
        /// <returns>The greatest common divisor.</returns>
        public static object Gcd(object args)
        {
            long gcd = 0;
            while (args is Pair)
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
        public static long Gcd2(long a, long b)
        {
            if (b == 0)
            {
                return a;
            }

            return Gcd2(b, a % b);
        }

        /// <summary>
        /// Tests if the number is exact.
        /// </summary>
        /// <param name="x">The number to test.</param>
        /// <returns>True if the number is exact.</returns>
        public static bool IsExact(object x)
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
        public static object Lcm(object args)
        {
            long lcm = 1;
            while (args is Pair)
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
        public static object NumberToString(object x, object y)
        {
            int numberBase = y is double ? (int)Num(y) : 10;
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
        public static object NumCompare(object args, char op)
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
                            return False;
                        }

                        break;

                    case '<':
                        if (!(x < y))
                        {
                            return False;
                        }

                        break;

                    case '=':
                        if (x != y)
                        {
                            return False;
                        }

                        break;

                    case 'L':
                        if (!(x <= y))
                        {
                            return False;
                        }

                        break;

                    case 'G':
                        if (!(x >= y))
                        {
                            return False;
                        }

                        break;

                    default:
                        Error("internal error: unrecognized op: " + op);
                        break;
                }
            }

            return True;
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
        public static object NumCompute(object args, char op, double result)
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
                args = Rest(args);

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
                        Error("internal error: unrecognized op: " + op);
                        break;
                }
            }

            return Num(result);
        }
    }
}
