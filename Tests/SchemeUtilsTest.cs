﻿// <copyright file="SchemeUtilsTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
    using System.IO;
    using System.Text;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;

    /// <summary>
    /// This is a test class for SchemeUtilsTest and is intended
    /// to contain all SchemeUtilsTest Unit Tests
    /// </summary>
    [TestClass]
    public class SchemeUtilsTest
    {
        /// <summary>
        /// Gets or sets the test context which provides
        /// information about and functionality for the current test run.
        /// </summary>
        public TestContext TestContext { get; set; }

        /// <summary>
        /// A test for AsBoolean
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void AsBooleanTest()
        {
            Assert.AreEqual(true, SchemeUtils_Accessor.AsBoolean(true));
            Assert.AreEqual(false, SchemeUtils_Accessor.AsBoolean(false));
            Assert.AreEqual(null, SchemeUtils_Accessor.AsBoolean(1));
        }

        /// <summary>
        /// A test for Chr
        /// </summary>
        [TestMethod]
        public void ChrTest()
        {
            Assert.AreEqual('a', SchemeUtils.Chr('a'));
            AssertEx.Throws(() => SchemeUtils.Chr(0));
        }

        /// <summary>
        /// A test for Cons, First, Rest
        /// </summary>
        [TestMethod]
        public void ConsTest()
        {
            var actual = SchemeUtils.Cons(1, 2);
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(1, SchemeUtils.First(actual));
            Assert.AreEqual(2, SchemeUtils.Rest(actual));
        }

        /// <summary>
        /// A test for Second
        /// </summary>
        [TestMethod]
        public void SecondTest()
        {
            var actual = SchemeUtils.Cons(1, SchemeUtils.Cons(2, 3));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(2, SchemeUtils.Second(actual));
        }

        /// <summary>
        /// A test for Third
        /// </summary>
        [TestMethod]
        public void ThirdTest()
        {
            var actual = SchemeUtils.Cons(1, SchemeUtils.Cons(2, SchemeUtils.Cons(3, 4)));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(3, SchemeUtils.Third(actual));
        }

        /// <summary>
        /// A test for SetFirst
        /// </summary>
        [TestMethod]
        public void SetFirstTest()
        {
            var actual = SchemeUtils.Cons(1, 2);
            SchemeUtils.SetFirst(actual, 10);
            Assert.AreEqual(10, SchemeUtils.First(actual));
            AssertEx.Throws(() => SchemeUtils.SetFirst(1, 10));
        }

        /// <summary>
        /// A test for SetRest
        /// </summary>
        [TestMethod]
        public void SetRestTest()
        {
            var actual = SchemeUtils.Cons(1, 2);
            SchemeUtils.SetRest(actual, 10);
            Assert.AreEqual(10, SchemeUtils.Rest(actual));
            AssertEx.Throws(() => SchemeUtils.SetRest(1, 10));
        }

        /// <summary>
        /// A test for Equal
        /// </summary>
        [TestMethod]
        public void EqualTest()
        {
            Assert.IsTrue(SchemeUtils.Equal(null, null));
            Assert.IsFalse(SchemeUtils.Equal(null, 1));
            Assert.IsTrue(SchemeUtils.Equal("abc", "abc"));
            Assert.IsFalse(SchemeUtils.Equal("abc", "ab"));
            Assert.IsFalse(SchemeUtils.Equal("abc", 1));
            Vector vec1 = new Vector(new object[] { 1, 2, 3 });
            Vector vec2 = new Vector(new object[] { 1, 2, 3 });
            Vector vec3 = new Vector(new object[] { 1, 2 });
            Vector vec4 = new Vector(new object[] { 1, 2, 4 });
            Assert.IsTrue(SchemeUtils.Equal(vec1, vec1));
            Assert.IsTrue(SchemeUtils.Equal(vec1, vec2));
            Assert.IsFalse(SchemeUtils.Equal(vec1, vec3));
            Assert.IsFalse(SchemeUtils.Equal(vec1, vec4));
            Assert.IsTrue(SchemeUtils.Equal(1, 1));
            Assert.IsFalse(SchemeUtils.Equal(1, 2));
            Assert.IsTrue(SchemeUtils.Equal(1.0, 1.0));
            Assert.IsFalse(SchemeUtils.Equal(1.0, 2.0));
            Assert.IsTrue(SchemeUtils.Equal(true, true));
            Assert.IsFalse(SchemeUtils.Equal(true, false));
            Assert.IsTrue(SchemeUtils.Equal('a', 'a'));
            Assert.IsFalse(SchemeUtils.Equal('a', 'b'));
        }

        /// <summary>
        /// A test for Eqv
        /// </summary>
        [TestMethod]
        public void EqvTest()
        {
            Assert.IsTrue(SchemeUtils.Eqv(null, null));
            Assert.IsFalse(SchemeUtils.Eqv(null, 1));
            Assert.IsTrue(SchemeUtils.Eqv("abc", "abc"));
            Assert.IsFalse(SchemeUtils.Eqv("abc", "ab"));
            Assert.IsFalse(SchemeUtils.Eqv("abc", 1));
            object[] vec1 = { 1, 2, 3 };
            object[] vec2 = { 1, 2, 3 };
            object[] vec3 = { 1, 2 };
            object[] vec4 = { 1, 2, 4 };
            Assert.IsTrue(SchemeUtils.Eqv(vec1, vec1));
            Assert.IsFalse(SchemeUtils.Eqv(vec1, vec2));
            Assert.IsFalse(SchemeUtils.Eqv(vec1, vec3));
            Assert.IsFalse(SchemeUtils.Eqv(vec1, vec4));
            Assert.IsTrue(SchemeUtils.Eqv(1, 1));
            Assert.IsFalse(SchemeUtils.Eqv(1, 2));
            Assert.IsTrue(SchemeUtils.Eqv(1.0, 1.0));
            Assert.IsFalse(SchemeUtils.Eqv(1.0, 2.0));
            Assert.IsTrue(SchemeUtils.Eqv(true, true));
            Assert.IsFalse(SchemeUtils.Eqv(true, false));
            Assert.IsTrue(SchemeUtils.Eqv('a', 'a'));
            Assert.IsFalse(SchemeUtils.Eqv('a', 'b'));
        }

        /// <summary>
        /// A test for InPort
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void InPortTest()
        {
            var files = new string[0];
            var accessor = new Interpreter_Accessor(false, files);
            Interpreter interpreter = accessor.Target as Interpreter;
            Assert.AreEqual(accessor.Input, InputPort.InPort(null, interpreter));
            using (StringReader reader = new StringReader("abc"))
            {
                InputPort input = new InputPort(reader);
                Assert.AreEqual(input, InputPort.InPort(input, interpreter));
            }

            AssertEx.Throws(() => InputPort.InPort(1, interpreter));
        }

        /// <summary>
        /// A test for OutPort
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void OutPortTest()
        {
            var files = new string[0];
            var accessor = new Interpreter_Accessor(false, files);
            Interpreter interpreter = accessor.Target as Interpreter;
            Assert.AreEqual(accessor.Output, OutputPort.OutPort(null, interpreter));
            using (StringWriter writer = new StringWriter())
            {
                OutputPort output = new OutputPort(writer);
                Assert.AreEqual(output, OutputPort.OutPort(output, interpreter));
            }

            AssertEx.Throws(() => OutputPort.OutPort(1, interpreter));
        }

        /// <summary>
        /// A test for IsFalse
        /// </summary>
        [TestMethod]
        public void IsFalseTest()
        {
            Assert.IsTrue(SchemeUtils.IsFalse(false));
            Assert.IsFalse(SchemeUtils.IsFalse(true));
            Assert.IsFalse(SchemeUtils.IsFalse(0));
        }

        /// <summary>
        /// A test for IsTrue
        /// </summary>
        [TestMethod]
        public void IsTrueTest()
        {
            Assert.IsTrue(SchemeUtils.IsTrue(true));
            Assert.IsFalse(SchemeUtils.IsTrue(false));
            Assert.IsFalse(SchemeUtils.IsTrue(0));
        }

        /// <summary>
        /// A test for Truth
        /// </summary>
        [TestMethod]
        public void TruthTest()
        {
            Assert.IsTrue(SchemeUtils.Truth(true));
            Assert.IsFalse(SchemeUtils.Truth(false));
            Assert.IsTrue(SchemeUtils.Truth(0));
        }

        /// <summary>
        /// A test for Length
        /// </summary>
        [TestMethod]
        public void LengthTest()
        {
            Assert.AreEqual(0, SchemeUtils.Length(null));
            Assert.AreEqual(0, SchemeUtils.Length(0));
            var actual = SchemeUtils.Cons(1, 2);
            Assert.AreEqual(1, SchemeUtils.Length(actual));
            actual = SchemeUtils.Cons(1, SchemeUtils.Cons(2, 3));
            Assert.AreEqual(2, SchemeUtils.Length(actual));
        }

        /// <summary>
        /// A test for List (one arg)
        /// </summary>
        [TestMethod]
        public void ListTest1()
        {
            var actual = SchemeUtils.List(10);
            Assert.AreEqual(1, SchemeUtils.Length(actual));
            Assert.AreEqual(10, SchemeUtils.First(actual));
            Assert.IsNull(SchemeUtils.Rest(actual));
        }

        /// <summary>
        /// A test for List (two args)
        /// </summary>
        [TestMethod]
        public void ListTest2()
        {
            var actual = SchemeUtils.List(10, 11);
            Assert.AreEqual(2, SchemeUtils.Length(actual));
            Assert.AreEqual(10, SchemeUtils.First(actual));
            Assert.AreEqual(11, SchemeUtils.First(SchemeUtils.Rest(actual)));
            Assert.IsNull(SchemeUtils.Rest(SchemeUtils.Rest(actual)));
        }

        /// <summary>
        /// A test for ListStar
        /// </summary>
        [TestMethod]
        public void ListStarTest()
        {
            var actual = SchemeUtils.ListStar(SchemeUtils.List(10));
            Assert.AreEqual(10, actual);
            actual = SchemeUtils.ListStar(SchemeUtils.List(10, 11));
            Assert.AreEqual(10, SchemeUtils.First(actual));
            Assert.AreEqual(11, ((Pair)actual).Rest);
            actual = SchemeUtils.ListStar(SchemeUtils.Cons(10, SchemeUtils.Cons(11, SchemeUtils.List(12))));
            Assert.AreEqual(10, SchemeUtils.First(actual));
            Assert.AreEqual(11, SchemeUtils.Second(actual));
            Assert.IsNull(SchemeUtils.Third(actual));
        }

        /// <summary>
        /// A test for ListToString
        /// </summary>
        [TestMethod]
        public void ListToStringTest()
        {
            var actual = SchemeString.ListToString(SchemeUtils.List('a', 'b'));
            Assert.AreEqual("ab", actual.AsString());
            actual = SchemeString.ListToString(1);
            Assert.AreEqual(string.Empty, actual.AsString());
            AssertEx.Throws(() => SchemeString.ListToString(SchemeUtils.List(1, 2)));
        }

        /// <summary>
        /// A test for ListToVector
        /// </summary>
        [TestMethod]
        public void ListToVectorTest()
        {
            var actual = Vector.ListToVector(SchemeUtils.List(1, 2));
            var expected = new object[] { 1, 2 };
            Assert.AreEqual(2, actual.Length);
            Assert.AreEqual(2, expected.Length);
            Assert.AreEqual(expected[0], actual[0]);
            Assert.AreEqual(expected[1], actual[1]);
            actual = Vector.ListToVector(1);
            Assert.AreEqual(0, actual.Length);
        }

        /// <summary>
        /// A test for Num
        /// </summary>
        [TestMethod]
        public void NumTest()
        {
            Assert.AreEqual(0.0, NumberUtils.Num(0.0));
            Assert.AreEqual(0.0, NumberUtils.Num(0));
            Assert.AreEqual(0.0, NumberUtils.Num("0"));
            Assert.AreEqual(1.0, NumberUtils.Num(1.0));
            Assert.AreEqual(1.0, NumberUtils.Num(1));
            Assert.AreEqual(1.0, NumberUtils.Num("1"));
            Assert.AreEqual(0.0, NumberUtils.Num(false));
            AssertEx.Throws(() => NumberUtils.Num('a'));
            AssertEx.Throws(() => NumberUtils.Num("xxx"));
            AssertEx.Throws(() => NumberUtils.Num("false"));
        }

        /// <summary>
        /// A test for Reverse
        /// </summary>
        [TestMethod]
        public void ReverseTest()
        {
            var test = SchemeUtils.List(1, 2);
            var actual = SchemeUtils.Reverse(test);
            Assert.AreEqual(2, SchemeUtils.First(actual));
            Assert.AreEqual(1, SchemeUtils.First(SchemeUtils.Rest(actual)));
        }

        /// <summary>
        /// A test for Str
        /// </summary>
        [TestMethod]
        public void StrTest()
        {
            var actual = SchemeString.Str(new SchemeString("abc"));
            Assert.AreEqual(3, actual.Length);
            Assert.AreEqual('a', actual[0]);
            Assert.AreEqual('b', actual[1]);
            Assert.AreEqual('c', actual[2]);
        }

        /// <summary>
        /// A test for AsString
        /// </summary>
        [TestMethod]
        public void AsStringTest()
        {
            Assert.AreEqual("()", SchemeString.AsString(null));
            Assert.AreEqual("1", SchemeString.AsString(1.0));
            Assert.AreEqual("1.5", SchemeString.AsString(1.5));
            Assert.AreEqual("#\\a", SchemeString.AsString('a'));
            Assert.AreEqual("(1 . 2)", SchemeString.AsString(new Pair(1, 2)));
            Assert.AreEqual("(1 2)", SchemeString.AsString(SchemeUtils.List(1, 2)));
            Assert.AreEqual(@"""abc""", SchemeString.AsString(new SchemeString("abc")));
            Assert.AreEqual(@"""\""""", SchemeString.AsString(new SchemeString(@"""")));
            var test = new Vector(new object[] { 1, 2 });
            Assert.AreEqual("#(1 2)", SchemeString.AsString(test));
            Assert.AreEqual("#t", SchemeString.AsString(true));
            Assert.AreEqual("#f", SchemeString.AsString(false));
            Assert.AreEqual("1", SchemeString.AsString(1));
        }

        /// <summary>
        /// A test for AsString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithQuote()
        {
            Assert.AreEqual("()", SchemeString.AsString(null, false));
            Assert.AreEqual("1", SchemeString.AsString(1.0, false));
            Assert.AreEqual("1.5", SchemeString.AsString(1.5, false));
            Assert.AreEqual("a", SchemeString.AsString('a', false));
            Assert.AreEqual("(1 . 2)", SchemeString.AsString(new Pair(1, 2), false));
            Assert.AreEqual("(1 2)", SchemeString.AsString(SchemeUtils.List(1, 2), false));
            Assert.AreEqual("abc", SchemeString.AsString(new SchemeString("abc"), false));
            Assert.AreEqual(@"""", SchemeString.AsString(new SchemeString(@""""), false));
            var test = new Vector(new object[] { 1, 2 });
            Assert.AreEqual("#(1 2)", SchemeString.AsString(test, false));
            Assert.AreEqual("#t", SchemeString.AsString(true, false));
            Assert.AreEqual("#f", SchemeString.AsString(false, false));
            Assert.AreEqual("1", SchemeString.AsString(1));
        }

        /// <summary>
        /// A test for AsString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithBuf()
        {
            StringBuilder buf = new StringBuilder().Append("x");
            SchemeString.AsString(null, false, buf);
            Assert.AreEqual("x()", buf.ToString());
            buf = new StringBuilder().Append("x");
            SchemeString.AsString(1.0, false, buf);
            Assert.AreEqual("x1", buf.ToString());
            buf = new StringBuilder().Append("x");
            SchemeString.AsString(1.5, false, buf);
            Assert.AreEqual("x1.5", buf.ToString());
            buf = new StringBuilder().Append("x");
            SchemeString.AsString('a', false, buf);
            Assert.AreEqual("xa", buf.ToString());
            buf = new StringBuilder().Append("x");
            SchemeString.AsString(new SchemeString("abc"), false, buf);
            Assert.AreEqual("xabc", buf.ToString());
        }

        /// <summary>
        /// A test for Sym.
        /// Sym objects are just strings.
        /// </summary>
        [TestMethod]
        public void SymTest()
        {
            Assert.AreEqual("abc", SchemeUtils.Sym("abc"));
            AssertEx.Throws(() => SchemeUtils.Sym(1));
        }

        /// <summary>
        /// A test for Vec
        /// </summary>
        [TestMethod]
        public void VecTest()
        {
            var test = new Vector(new object[] { 1, 2 });
            Assert.AreEqual(2, Vector.Vec(test).Length);
            Assert.AreEqual(1, Vector.Vec(test)[0]);
            Assert.AreEqual(2, Vector.Vec(test)[1]);
            AssertEx.Throws(() => Vector.Vec(1));
        }

        /// <summary>
        /// A test for VectorToList
        /// </summary>
        [TestMethod]
        public void VectorToListTest()
        {
            var test = new Vector(new object[] { 1, 2, 3 });
            var actual = Vector.VectorToList(test);
            Assert.AreEqual(3, SchemeUtils.Length(actual));
            Assert.AreEqual(1, SchemeUtils.First(actual));
            Assert.AreEqual(2, SchemeUtils.Second(actual));
            Assert.AreEqual(3, SchemeUtils.Third(actual));
        }

        /// <summary>
        /// A test for Warn
        /// </summary>
        [TestMethod]
        public void WarnTest()
        {
            Assert.AreEqual("<warn>", SchemeUtils.Warn("message"));
        }

        /// <summary>
        /// A test for Write
        /// </summary>
        [TestMethod]
        public void WriteTest()
        {
            using (StringWriter writer = new StringWriter())
            {
                OutputPort w = new OutputPort(writer);
                var actual = OutputPort.Write("abc", w, false);
                Assert.AreEqual("abc", actual);
                Assert.AreEqual("abc", writer.ToString());
            }
        }

        /// <summary>
        /// Extended Assert.
        /// </summary>
        public static class AssertEx
        {
            /// <summary>
            /// Verifies that the action throws an exception.
            /// </summary>
            /// <param name="exceptionType">The type of exception expected.</param>
            /// <param name="act">The code to test.</param>
            public static void Throws(Type exceptionType, Action act)
            {
                bool failed = false;
                try
                {
                    act.Invoke();
                }
                catch (Exception ex)
                {
                    Assert.IsInstanceOfType(ex, exceptionType, "Unexpected exception type: " + ex.GetType());
                    failed = true;
                }

                if (! failed)
                {
                    Assert.Fail("Did not throw exception");
                }
            }

            /// <summary>
            /// Verifies that the action throws a SchemeException
            /// </summary>
            /// <param name="act">The code to test.</param>
            public static void Throws(Action act)
            {
                Throws(typeof(SchemeUtils.SchemeException), act);
            }
        }
    }
}
