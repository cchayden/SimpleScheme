// <copyright file="TestClass.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>

namespace Clr
{
    /// <summary>
    /// Contains various kinds of properties and methods that will be used by the
    ///   calling application.
    /// </summary>
    public class TestClass
    {
        /// <summary>
        /// Indexer value.
        /// </summary>
        private int lastIndex; // = 0;

        /// <summary>
        /// Gets or sets a test attribute.
        /// </summary>
        public string Attr { get; set; }

        /// <summary>
        /// A test indexer.
        /// </summary>
        /// <param name="i">The indexer param.</param>
        /// <returns>The last index that was set.</returns>
        public int this[int i]
        {
            get { return this.lastIndex; }
            set { this.lastIndex = value; }
        }

        /// <summary>
        /// A test static method.
        /// </summary>
        /// <param name="str">An input string.</param>
        /// <returns>The augmented input string.</returns>
        public static string AddStar(string str)
        {
            return "Static method: " + str + "*";
        }

        /// <summary>
        /// A test method.
        /// </summary>
        /// <param name="n">An input integer.</param>
        /// <returns>The augmented input integer.</returns>
        public string AddOne(int n)
        {
            return "Member method: " + (n + 1);
        }
    }
}