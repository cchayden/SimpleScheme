﻿// <copyright file="ClrTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
    using System.IO;
    using System.Threading;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;
    using Obj = System.Object;

    /// <summary>
    /// This tests clr interface
    /// </summary>
    [TestClass]
    public class ClrTest
    {
        /// <summary>
        /// A scheme interpreter, created for each test.
        /// </summary>
        private IInterpreter interpreter;

        /// <summary>
        /// Initialize each test.
        /// </summary>
        [TestInitialize]
        public void MyTestInitialize()
        {
            this.interpreter = Interpreter.New();
        }

        /// <summary>
        /// A test for string-related CLR procedures
        /// </summary>
        [TestMethod]
        public void CtorClrTest()
        {
            // new
            this.Run(typeof(ClrTestClass), "new clr test class", @"
              (begin
                (define new-test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                new-test-class)
            ", true);
        }

        /// <summary>
        /// A test for string-related CLR procedures
        /// </summary>
        [TestMethod]
        public void StringClrTest()
        {
            // static no arg
            this.Run("ok", "static no arg", @"
              (begin
                (define no-arg(method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticNoArg""))
                (no-arg))
            ");

            // no arg
            this.Run("ok", "member no arg", @"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define no-arg(method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberNoArg""))
                (no-arg test-class))
            ");

            // static string to string
            this.Run("test*", "static string to string", @"
              (begin
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticStringToString"" ""string""))
                (str-to-str ""test""))
            ");

            // static symbol to string
            this.Run("test*", "static symbol to string", @"
              (begin
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticStringToString"" ""string""))
                (str-to-str (string->symbol ""test"")))
            ");

            // string to string
            this.Run("test*", "string to string", @"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberStringToString"" ""string""))
                (str-to-str test-class 'test))
            ");

            // symbol to string
            this.Run("test*", "symbol to string", @"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberStringToString"" ""string""))
                (str-to-str test-class 'test))
            ");

            // string property set/get
            this.Run("ok", "string set/get", @"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define attr-set (property-set ""Tests.ClrTest+ClrTestClass,Tests"" ""StringAttr"" ""string""))
                (define attr-get (property-get ""Tests.ClrTest+ClrTestClass,Tests"" ""StringAttr""))
                (attr-set test-class 'ok)
                (attr-get test-class))
            ");

            // index set/get
            this.Run("xxx", "item array-list", @"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define item (index-get ""Tests.ClrTest+ClrTestClass,Tests"" ""int""))
                (define item-set! (index-set ""Tests.ClrTest+ClrTestClass,Tests"" ""int"" ""string"" ))
                (item-set! test-class 0 'xxx)
                (item test-class 0))
            ");
        }

        /// <summary>
        /// A test for int-related CLR procedures
        /// </summary>
        [TestMethod]
        public void IntClrTest()
        {
            // int to int
            this.Run(2, "static int to int", @"
              (begin
                (define int-to-int (method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticIntToInt"" ""int""))
                (int-to-int 1))
            ");

            // int to int
            this.Run(2, "int to int", @"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define int-to-int (method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberIntToInt"" ""int""))
                (int-to-int test-class 1))
            ");

            // int property set/get
            this.Run(5, "int set/get", @"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define attr-set (property-set ""Tests.ClrTest+ClrTestClass,Tests"" ""IntAttr"" ""int""))
                (define attr-get (property-get ""Tests.ClrTest+ClrTestClass,Tests"" ""IntAttr""))
                (attr-set test-class 5)
                (attr-get test-class))
            ");
        }

        /// <summary>
        /// A test for double-related CLR procedures
        /// </summary>
        [TestMethod]
        public void DoubleClrTest()
        {
            // double to double
            this.Run(4.5, "double to double", @"
              (begin
                (define double-to-double (method ""Tests.ClrTest+ClrTestClass,Tests"" ""DoubleToDouble"" ""double""))
                (double-to-double 2.25))
            ");

            // float to float
            this.Run(4.5f, "float to float", @"
              (begin
                (define float-to-float (method ""Tests.ClrTest+ClrTestClass,Tests"" ""FloatToFloat"" ""float""))
                (float-to-float 2.25))
            ");

            // short to short
            this.Run((short)10001, "short to short", @"
              (begin
                (define short-to-short (method ""Tests.ClrTest+ClrTestClass,Tests"" ""ShortToShort"" ""short""))
                (short-to-short 10000))
            ");

            // byte to byte
            this.Run((byte)42, "byte to byte", @"
              (begin
                (define byte-to-byte (method ""Tests.ClrTest+ClrTestClass,Tests"" ""ByteToByte"" ""byte""))
                (byte-to-byte 41))
            ");
        }

        /// <summary>
        /// A test for bool-related CLR procedures
        /// </summary>
        [TestMethod]
        public void BoolClrTest()
        {
            // bool to bool
            this.Run(true, "bool to bool", @"              
              (begin
                (define bool-to-bool (method ""Tests.ClrTest+ClrTestClass,Tests"" ""BoolToBool"" ""bool""))
                (bool-to-bool #f))
            ");

            // bool to bool
            this.Run(false, "static bool to bool", @"
              (begin
                (define bool-to-bool (method ""Tests.ClrTest+ClrTestClass,Tests"" ""BoolToBool"" ""bool""))
                (bool-to-bool #t))
            ");
        }

        /// <summary>
        /// A test for char-related CLR procedures
        /// </summary>
        [TestMethod]
        public void CharClrTest()
        {
            // char to char
            this.Run('A', "char to char", @"              
              (begin
                (define char-to-char (method ""Tests.ClrTest+ClrTestClass,Tests"" ""CharToChar"" ""char""))
                (char-to-char #\a))
            ");
        }

        /// <summary>
        /// A test for text reader and text writer related CLR procedures
        /// </summary>
        [TestMethod]
        public void TextClrText()
        {
            // text reader to text reader
            this.Run(typeof(StreamReader), "text reader to text reader", @"              
              (begin
                (define reader-to-reader (method ""Tests.ClrTest+ClrTestClass,Tests"" ""TextReaderToTextReader"" ""System.IO.TextReader""))
                (reader-to-reader (current-input-port)))
            ", true);

            // text reader to text writer
            this.Run(typeof(StreamWriter), "text writer to text writer", @"              
              (begin
                (define writer-to-writer (method ""Tests.ClrTest+ClrTestClass,Tests"" ""TextWriterToTextWriter"" ""System.IO.TextWriter""))
                (writer-to-writer (current-output-port)))
            ", true);
        }

        /// <summary>
        /// Run a test and check the result.
        /// </summary>
        /// <param name="expected">The expected result.</param>
        /// <param name="label">The label to display.</param>
        /// <param name="expr">The expression to evaluate.</param>
        /// <param name="compareType">If true, compare the expected type against the result type.</param>
        private void Run(object expected, string label, string expr, bool compareType = false)
        {
            Obj res = this.interpreter.EvalStr(expr);
            string actual = res != EmptyList.New() ? res.ToString() : "'()";
            Console.WriteLine("({0} {1}) ==> {2}", label, expected, actual);
            //Assert.AreEqual(expected, actual, "Failed");
            if (compareType)
            {
                Assert.AreEqual(expected, res.GetType(), "Failed");
            }
            else
            {
                Assert.AreEqual(expected, res, "Failed");
            }
        }

        private class ClrTestClass
        {
            /// <summary>
            /// Indexer value.
            /// </summary>
            private string[] indexerValue = new string[10];

            /// <summary>
            /// Gets or sets a string attribute.
            /// </summary>
            public string StringAttr { get; set; }

            /// <summary>
            /// Gets or sets an int attribute.
            /// </summary>
            public int IntAttr { get; set; }

            /// <summary>
            /// A test indexer.
            /// </summary>
            /// <param name="i">The index.</param>
            /// <returns>The stored value at the index.</returns>
            public string this[int i]
            {
                get { return this.indexerValue[i]; }
                set { this.indexerValue[i] = value; }
            }

            /// <summary>
            /// A test static method.
            /// </summary>
            /// <returns>ok</returns>
            public static string StaticNoArg()
            {
                return "ok";
            }

            /// <summary>
            /// A test method.
            /// </summary>
            /// <returns>ok</returns>
            public string MemberNoArg()
            {
                return "ok";
            }

            /// <summary>
            /// A test static method.
            /// </summary>
            /// <param name="str">An input string.</param>
            /// <returns>The input string plus *.</returns>
            public static string StaticStringToString(string str)
            {
                return str + "*";
            }

            /// <summary>
            /// A test member method.
            /// </summary>
            /// <param name="str">An input string.</param>
            /// <returns>The input string plus *.</returns>
            public string MemberStringToString(string str)
            {
                return str + "*";
            }

            /// <summary>
            /// A test static method.
            /// </summary>
            /// <param name="n">An input integer.</param>
            /// <returns>The input integer plus one.</returns>
            public static int StaticIntToInt(int n)
            {
                return n + 1;
            }

            /// <summary>
            /// A test member method.
            /// </summary>
            /// <param name="n">An input integer.</param>
            /// <returns>The input integer plus one.</returns>
            public int MemberIntToInt(int n)
            {
                return n + 1;
            }

            /// <summary>
            /// A boolean method.
            /// </summary>
            /// <param name="b">Input boolean</param>
            /// <returns>Not input</returns>
            public static bool BoolToBool(bool b)
            {
                return !b;
            }

            /// <summary>
            /// A double method.
            /// </summary>
            /// <param name="x">Input double.</param>
            /// <returns>Input * 2</returns>
            public static double DoubleToDouble(double x)
            {
                return 2.0 * x;
            }

            /// <summary>
            /// A float method.
            /// </summary>
            /// <param name="x">Input float.</param>
            /// <returns>Input * 2</returns>
            public static float FloatToFloat(float x)
            {
                return 2.0f * x;
            }

            /// <summary>
            /// A short method.
            /// </summary>
            /// <param name="x">Input short.</param>
            /// <returns>Input + 1</returns>
            public static short ShortToShort(short x)
            {
                return (short)(x + 1);
            }

            /// <summary>
            /// A byte method.
            /// </summary>
            /// <param name="x">Input byte.</param>
            /// <returns>Input + 1</returns>
            public static byte ByteToByte(byte x)
            {
                return (byte)(x + 1);
            }

            /// <summary>
            /// A char method.
            /// </summary>
            /// <param name="x">Input char.</param>
            /// <returns>Uppercase input</returns>
            public static char CharToChar(char x)
            {
                return Char.ToUpper(x);
            }

            /// <summary>
            /// A TextReader method.
            /// </summary>
            /// <param name="x">Input text reader.</param>
            /// <returns>Input</returns>
            public static TextReader TextReaderToTextReader(TextReader x)
            {
                return new StreamReader(new MemoryStream());
            }

            /// <summary>
            /// A TextWriter method.
            /// </summary>
            /// <param name="x">Input TextWriter.</param>
            /// <returns>Input</returns>
            public static TextWriter TextWriterToTextWriter(TextWriter x)
            {
                return new StreamWriter("test");
            }
        }
    }
}
