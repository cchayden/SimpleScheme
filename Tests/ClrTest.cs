// <copyright file="ClrTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
    using System.IO;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;

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
            var res = this.Run(@"
              (begin
                (define new-test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                new-test-class)
            ");
            CheckClrType(typeof(ClrTestClass), "new clr test class", res);
        }

        /// <summary>
        /// A test for string-related CLR procedures
        /// </summary>
        [TestMethod]
        public void StringClrTest()
        {
            // static no arg
            var res = this.Run(@"
              (begin
                (define no-arg(method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticNoArg""))
                (no-arg))
            ");
            Check((SchemeString)"ok", "static no arg", res);

            // no arg
            res = this.Run(@"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define no-arg(method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberNoArg""))
                (no-arg test-class))
            ");
            Check((SchemeString)"ok", "member no arg", res);

            // static string to string
            res = this.Run(@"
              (begin
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticStringToString"" ""string""))
                (str-to-str ""test""))
            ");
            Check((SchemeString)"test*", "static string to string", res);

            // static symbol to string
            res = this.Run(@"
              (begin
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticStringToString"" ""string""))
                (str-to-str (string->symbol ""test"")))
            ");
            Check((SchemeString)"test*", "static symbol to string", res);

            // string to string
            res = this.Run(@"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberStringToString"" ""string""))
                (str-to-str test-class 'test))
            ");
            Check((SchemeString)"test*", "string to string", res);

            // symbol to string
            res = this.Run(@"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define str-to-str (method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberStringToString"" ""string""))
                (str-to-str test-class 'test))
            ");
            Check((SchemeString)"test*", "symbol to string", res);

            // string property set/get
            res = this.Run(@"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define attr-set (property-set ""Tests.ClrTest+ClrTestClass,Tests"" ""StringAttr"" ""string""))
                (define attr-get (property-get ""Tests.ClrTest+ClrTestClass,Tests"" ""StringAttr""))
                (attr-set test-class 'ok)
                (attr-get test-class))
            ");
            Check((SchemeString)"ok", "string set/get", res);

            // index set/get
            res = this.Run(@"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define item (index-get ""Tests.ClrTest+ClrTestClass,Tests"" ""int""))
                (define item-set! (index-set ""Tests.ClrTest+ClrTestClass,Tests"" ""int"" ""string"" ))
                (item-set! test-class 0 'xxx)
                (item test-class 0))
            ");
            Check((SchemeString)"xxx", "item array-list", res);
        }

        /// <summary>
        /// A test for int-related CLR procedures
        /// </summary>
        [TestMethod]
        public void IntClrTest()
        {
            // int to int
            var res = this.Run(@"
              (begin
                (define int-to-int (method ""Tests.ClrTest+ClrTestClass,Tests"" ""StaticIntToInt"" ""int""))
                (int-to-int 1))
            ");
            Check((Number)2, "static int to int", res);

            // int to int
            res = this.Run(@"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define int-to-int (method ""Tests.ClrTest+ClrTestClass,Tests"" ""MemberIntToInt"" ""int""))
                (int-to-int test-class 1))
            ");
            Check((Number)2, "int to int", res);

            // int property set/get
            res = this.Run(@"
              (begin
                (define test-class (new ""Tests.ClrTest+ClrTestClass,Tests""))
                (define attr-set (property-set ""Tests.ClrTest+ClrTestClass,Tests"" ""IntAttr"" ""int""))
                (define attr-get (property-get ""Tests.ClrTest+ClrTestClass,Tests"" ""IntAttr""))
                (attr-set test-class 5)
                (attr-get test-class))
            ");
            Check((Number)5, "int set/get", res);
        }

        /// <summary>
        /// A test for double-related CLR procedures
        /// </summary>
        [TestMethod]
        public void DoubleClrTest()
        {
            // double to double
            var res = this.Run(@"
              (begin
                (define double-to-double (method ""Tests.ClrTest+ClrTestClass,Tests"" ""DoubleToDouble"" ""double""))
                (double-to-double 2.25))
            ");
            Check((Number)4.5, "double to double",  res);

            // float to float
            res = this.Run(@"
              (begin
                (define float-to-float (method ""Tests.ClrTest+ClrTestClass,Tests"" ""FloatToFloat"" ""float""))
                (float-to-float 2.25))
            ");
            Check((Number)4.5, "float to float", res);

            // short to short
            res = this.Run(@"
              (begin
                (define short-to-short (method ""Tests.ClrTest+ClrTestClass,Tests"" ""ShortToShort"" ""short""))
                (short-to-short 10000))
            ");
            Check((Number)10001, "short to short", res);

            // byte to byte
            res = this.Run(@"
              (begin
                (define byte-to-byte (method ""Tests.ClrTest+ClrTestClass,Tests"" ""ByteToByte"" ""byte""))
                (byte-to-byte 41))
            ");
            Check((Number)42, "byte to byte", res);
        }

        /// <summary>
        /// A test for bool-related CLR procedures
        /// </summary>
        [TestMethod]
        public void BoolClrTest()
        {
            // bool to bool
            var res = this.Run(@"              
              (begin
                (define bool-to-bool (method ""Tests.ClrTest+ClrTestClass,Tests"" ""BoolToBool"" ""bool""))
                (bool-to-bool #f))
            ");
            Check(SchemeBoolean.True, "bool to bool", res);

            // bool to bool
            res = this.Run(@"
              (begin
                (define bool-to-bool (method ""Tests.ClrTest+ClrTestClass,Tests"" ""BoolToBool"" ""bool""))
                (bool-to-bool #t))
            ");
            Check(SchemeBoolean.False, "static bool to bool", res);
        }

        /// <summary>
        /// A test for char-related CLR procedures
        /// </summary>
        [TestMethod]
        public void CharClrTest()
        {
            // char to char
            var res = this.Run(@"              
              (begin
                (define char-to-char (method ""Tests.ClrTest+ClrTestClass,Tests"" ""CharToChar"" ""char""))
                (char-to-char #\a))
            ");
            Check((Character)'A', "char to char", res);
        }

        /// <summary>
        /// A test for text reader and text writer related CLR procedures
        /// </summary>
        [TestMethod]
        public void TextClrText()
        {
            // text reader to text reader
            var res = this.Run(@"              
              (begin
                (define reader-to-reader (method ""Tests.ClrTest+ClrTestClass,Tests"" ""TextReaderToTextReader"" ""System.IO.TextReader""))
                (reader-to-reader (current-input-port)))
            ");
            CheckClrType(typeof(StreamReader), "text reader to text reader", res);

            // text reader to text writer
            res = this.Run(@"              
              (begin
                (define writer-to-writer (method ""Tests.ClrTest+ClrTestClass,Tests"" ""TextWriterToTextWriter"" ""System.IO.TextWriter""))
                (writer-to-writer (current-output-port)))
            ");
            CheckClrType(typeof(StreamWriter), "text writer to text writer", res);
        }

        /// <summary>
        /// A test for object[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void ObjectArrayClrTest()
        {
            var exp = new SchemeObject[] { (Character)'a', (Number)1, (SchemeString)"abc", (SchemeString)"def", SchemeBoolean.True, EmptyList.Instance };
            var res = this.Run(@"              
              (begin
                (define objarray-to-objarray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""ObjectArrayToObjectArray"" ""object[]""))
                (define array (make-vector 6))
                (vector-set! array 0 #\a)
                (vector-set! array 1 1)
                (vector-set! array 2 ""abc"")
                (vector-set! array 3 'def)
                (vector-set! array 4 #t)
                (vector-set! array 5 '())
                (objarray-to-objarray array))
            ");
            CheckVector<SchemeObject>(exp, "objarray to objarray", res);
        }

        /// <summary>
        /// A test for int[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void IntArrayClrTest()
        {
            var exp = new[] { (Number)0 };
            var res = this.Run(@"              
              (begin
                (define intarray-to-intarray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""IntArrayToIntArray"" ""int[]""))
                (define array (make-vector 1))
                (vector-set! array 0 0)
                (intarray-to-intarray array))
            ");
            CheckVector<Number>(exp, "intarray to intarray", res);
        }

        /// <summary>
        /// A test for bool[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void BoolArrayClrTest()
        {
            var exp = new[] { SchemeBoolean.True };
            var res = this.Run(@"              
              (begin
                (define boolarray-to-boolarray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""BoolArrayToBoolArray"" ""bool[]""))
                (define array (make-vector 1))
                (vector-set! array 0 #t)
                (boolarray-to-boolarray array))
            ");
            CheckVector<SchemeBoolean>(exp, "boolarray to boolarray", res);
        }

        /// <summary>
        /// A test for byte[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void ByteArrayClrTest()
        {
            var exp = new[] { (Number)0 };
            var res = this.Run(@"              
              (begin
                (define bytearray-to-bytearray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""ByteArrayToByteArray"" ""byte[]""))
                (define array (make-vector 1))
                (vector-set! array 0 0)
                (bytearray-to-bytearray array))
            ");
            CheckVector<Number>(exp, "bytearray to bytearray", res);
        }

        /// <summary>
        /// A test for short[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void ShortArrayClrTest()
        {
            var exp = new[] { (Number)0 };
            var res = this.Run(@"              
              (begin
                (define shortarray-to-shortarray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""ShortArrayToShortArray"" ""short[]""))
                (define array (make-vector 1))
                (vector-set! array 0 0)
                (shortarray-to-shortarray array))
            ");
            CheckVector<Number>(exp, "shortarray to shortarray", res);
        }

        /// <summary>
        /// A test for long[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void LongArrayClrTest()
        {
            var exp = new[] { (Number)0L };
            var res = this.Run(@"              
              (begin
                (define longarray-to-longarray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""LongArrayToLongArray"" ""long[]""))
                (define array (make-vector 1))
                (vector-set! array 0 0)
                (longarray-to-longarray array))
            ");
            CheckVector<Number>(exp, "longarray to longarray", res);
        }

        /// <summary>
        /// A test for float[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void FloatArrayClrTest()
        {
            var exp = new[] { (Number)0.0f };
            var res = this.Run(@"              
              (begin
                (define floatarray-to-floatarray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""FloatArrayToFloatArray"" ""float[]""))
                (define array (make-vector 1))
                (vector-set! array 0 0.0)
                (floatarray-to-floatarray array))
            ");
            CheckVector<Number>(exp, "floatarray to floatarray", res);
        }

        /// <summary>
        /// A test for double[]-related CLR procedures
        /// </summary>
        [TestMethod]
        public void DoubleArrayClrTest()
        {
            var exp = new[] { (Number)0.0d };
            var res = this.Run(@"              
              (begin
                (define doublearray-to-doublearray (method ""Tests.ClrTest+ClrTestClass,Tests"" ""DoubleArrayToDoubleArray"" ""double[]""))
                (define array (make-vector 1))
                (vector-set! array 0 0.0)
                (doublearray-to-doublearray array))
            ");
            CheckVector<Number>(exp, "doublearray to doublearray", res);
        }

        /// <summary>
        /// Evaluate the expression and return the result.
        /// </summary>
        /// <param name="expr">The expression to evaluate.</param>
        /// <returns>The result of evaluating the expression.</returns>
        private object Run(string expr)
        {
            return this.interpreter.Eval(expr);
        }

        private static void Check(object expected, string label, object res)
        {
            string actual = res != EmptyList.Instance ? res.ToString() : "'()";
            Console.WriteLine("({0} {1}) ==> {2}", label, expected, actual);
            Assert.AreEqual(expected, res, "Failed");
        }

        private static void CheckVector<T>(object expected, string label, object res)
        {
            string actual = res != EmptyList.Instance ? res.ToString() : "'()";
            Console.WriteLine("({0} {1}) ==> {2}", label, expected, actual);
            var expectedArray = expected as T[];
            Assert.IsNotNull(expectedArray);
            Assert.IsInstanceOfType(res, typeof(Vector));
            var vec = (Vector)res;
            for (var i = 0; i < expectedArray.Length; i++)
            {
                Assert.AreEqual(expectedArray[i], vec[i], "Failed " + i);
            }
        }

        /// <summary>
        /// Check the result (type types).
        /// </summary>
        /// <param name="expected">The expected result</param>
        /// <param name="label">Message label</param>
        /// <param name="res">Actual result</param>
        private static void CheckClrType(object expected, string label, object res)
        {
            string actual = res != EmptyList.Instance ? res.ToString() : "'()";
            Console.WriteLine("({0} {1}) ==> {2}", label, expected, actual);
            Assert.IsInstanceOfType(res, typeof(ClrObject));
            Assert.AreEqual(expected, ((ClrObject)res).Value.GetType(), "Failed");
        }

        // ReSharper disable UnusedMember.Local

        /// <summary>
        /// Class to instantiate and call to test CLR interface.
        /// </summary>
        private class ClrTestClass
        {
            /// <summary>
            /// Indexer value.
            /// </summary>
            private readonly string[] indexerValue = new string[10];

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
            /// <returns>ok string</returns>
            public static string StaticNoArg()
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
            /// A test static method.
            /// </summary>
            /// <param name="n">An input integer.</param>
            /// <returns>The input integer plus one.</returns>
            public static int StaticIntToInt(int n)
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
                return char.ToUpper(x);
            }

            /// <summary>
            /// A TextReader method.
            /// </summary>
            /// <param name="x">Input text reader.</param>
            /// <returns>Input value</returns>
            public static TextReader TextReaderToTextReader(TextReader x)
            {
                return new StreamReader(new MemoryStream());
            }

            /// <summary>
            /// A TextWriter method.
            /// </summary>
            /// <param name="x">Input TextWriter.</param>
            /// <returns>Input value</returns>
            public static TextWriter TextWriterToTextWriter(TextWriter x)
            {
                return new StreamWriter("test");
            }

            /// <summary>
            /// An object[] method.
            /// </summary>
            /// <param name="x">Input object[].</param>
            /// <returns>Input value</returns>
            public static object[] ObjectArrayToObjectArray(object[] x)
            {
                return x;
            }

            /// <summary>
            /// An int[] method.
            /// </summary>
            /// <param name="x">Input int[].</param>
            /// <returns>Input value</returns>
            public static int[] IntArrayToIntArray(int[] x)
            {
                return x;
            }

            /// <summary>
            /// A bool[] method.
            /// </summary>
            /// <param name="x">Input bool[].</param>
            /// <returns>Input value</returns>
            public static bool[] BoolArrayToBoolArray(bool[] x)
            {
                return x;
            }

            /// <summary>
            /// A byte[] method.
            /// </summary>
            /// <param name="x">Input byte[].</param>
            /// <returns>Input value</returns>
            public static byte[] ByteArrayToByteArray(byte[] x)
            {
                return x;
            }

            /// <summary>
            /// A short[] method.
            /// </summary>
            /// <param name="x">Input short[].</param>
            /// <returns>Input value</returns>
            public static short[] ShortArrayToShortArray(short[] x)
            {
                return x;
            }

            /// <summary>
            /// A long[] method.
            /// </summary>
            /// <param name="x">Input long[].</param>
            /// <returns>Input value</returns>
            public static long[] LongArrayToLongArray(long[] x)
            {
                return x;
            }

            /// <summary>
            /// A float[] method.
            /// </summary>
            /// <param name="x">Input float[].</param>
            /// <returns>Input value</returns>
            public static float[] FloatArrayToFloatArray(float[] x)
            {
                return x;
            }

            /// <summary>
            /// A double[] method.
            /// </summary>
            /// <param name="x">Input double[].</param>
            /// <returns>Input value</returns>
            public static double[] DoubleArrayToDoubleArray(double[] x)
            {
                return x;
            }

            /// <summary>
            /// A test method.
            /// </summary>
            /// <returns>ok string</returns>
            public string MemberNoArg()
            {
                return "ok";
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
            /// A test member method.
            /// </summary>
            /// <param name="n">An input integer.</param>
            /// <returns>The input integer plus one.</returns>
            public int MemberIntToInt(int n)
            {
                return n + 1;
            }
        }
    }
}
