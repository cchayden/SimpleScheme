// <copyright file="Program.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace AsyncWCF
{
    /// <summary>
    /// Main test program.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Entry point.
        /// </summary>
        public static void Main()
        {
            new AsyncTest().Run().Close();
        }
    }
}
