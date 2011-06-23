// <copyright file="TestClass.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Repl
{
    /// <summary>
    /// Used to test synchronous CLR methods.
    /// </summary>
    public class TestClass
    {
        /// <summary>
        /// Static test method.
        /// </summary>
        /// <returns>String identifying the method.</returns>
        public static string TestMethod1()
        {
            return "Static method";
        }

        /// <summary>
        /// Normal test method.
        /// </summary>
        /// <returns>String identifying the method.</returns>
        public string TestMethod2()
        {
            return "Member method";
        }
    }
}