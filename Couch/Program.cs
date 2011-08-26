// <copyright file="Program.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace CouchExample
{
    using System;
    using CouchLib;
    using SimpleScheme;

    /// <summary>
    /// Test program for couch db
    /// </summary>
    public class Program
    {
        /// <summary>
        /// Main entry point.
        /// This produces the same output two ways: a C# program and a scheme program.
        /// </summary>
        /// <param name="args">Command line arguments.</param>
        public static void Main(string[] args)
        {
            // First the C# program.
            Console.WriteLine("----- C# Program -----");
            CouchDb db = CouchDb.New("http://localhost:5984", "test_db");
            Console.WriteLine("Databases");
            foreach (var dbname in db.GetDatabases())
            {
                Console.WriteLine(dbname);
            }

            Console.WriteLine();
            Console.WriteLine("Contents of database {0}", db.DbName);

            foreach (var doc in db.GetAllDocuments())
            {
                Console.WriteLine(doc.Id);
            }

            Console.WriteLine();

            // Now the scheme program
            Interpreter.New(new[] { "couch.ss" });
        }
    }
}
