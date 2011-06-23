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
    using Obj = System.Object;

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
        /// A test for Chr
        /// </summary>
        [TestMethod]
        public void ChrTest()
        {
            Assert.AreEqual('a', Character.Chr('a'));
            AssertEx.Throws(() => Character.Chr(0));
        }

        /// <summary>
        /// A test for Cons, First, Rest
        /// </summary>
        [TestMethod]
        public void ConsTest()
        {
            var actual = ListPrimitives.Cons(1, 2);
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(1, ListPrimitives.First(actual));
            Assert.AreEqual(2, ListPrimitives.Rest(actual));
        }

        /// <summary>
        /// A test for Second
        /// </summary>
        [TestMethod]
        public void SecondTest()
        {
            var actual = ListPrimitives.Cons(1, ListPrimitives.Cons(2, 3));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(2, ListPrimitives.Second(actual));
        }

        /// <summary>
        /// A test for Third
        /// </summary>
        [TestMethod]
        public void ThirdTest()
        {
            var actual = ListPrimitives.Cons(1, ListPrimitives.Cons(2, ListPrimitives.Cons(3, 4)));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(3, ListPrimitives.Third(actual));
        }

        /// <summary>
        /// A test for SetFirst
        /// </summary>
        [TestMethod]
        public void SetFirstTest()
        {
            var actual = ListPrimitives.Cons(1, 2);
            List_Accessor.SetFirst(actual, 10);
            Assert.AreEqual(10, ListPrimitives.First(actual));
            AssertEx.Throws(() => List_Accessor.SetFirst(1, 10));
        }

        /// <summary>
        /// A test for SetRest
        /// </summary>
        [TestMethod]
        public void SetRestTest()
        {
            var actual = ListPrimitives.Cons(1, 2);
            List_Accessor.SetRest(actual, 10);
            Assert.AreEqual(10, ListPrimitives.Rest(actual));
            AssertEx.Throws(() => List_Accessor.SetRest(1, 10));
        }

        /// <summary>
        /// A test for Equal
        /// </summary>
        [TestMethod]
        public void EqualTest()
        {
            Assert.IsTrue(SchemeBoolean.Equal(List.Empty, List.Empty));
            Assert.IsFalse(SchemeBoolean.Equal(List.Empty, 1));
            Assert.IsTrue(SchemeBoolean.Equal("abc", "abc"));
            Assert.IsFalse(SchemeBoolean.Equal("abc", "ab"));
            Assert.IsFalse(SchemeBoolean.Equal("abc", 1));
            var vec1 = new Obj[] { 1, 2, 3 };
            var vec2 = new Obj[] { 1, 2, 3 };
            var vec3 = new Obj[] { 1, 2 };
            var vec4 = new Obj[] { 1, 2, 4 };
            Assert.IsTrue(SchemeBoolean.Equal(vec1, vec1));
            Assert.IsTrue(SchemeBoolean.Equal(vec1, vec2));
            Assert.IsFalse(SchemeBoolean.Equal(vec1, vec3));
            Assert.IsFalse(SchemeBoolean.Equal(vec1, vec4));
            Assert.IsTrue(SchemeBoolean.Equal(1, 1));
            Assert.IsFalse(SchemeBoolean.Equal(1, 2));
            Assert.IsTrue(SchemeBoolean.Equal(1.0, 1.0));
            Assert.IsFalse(SchemeBoolean.Equal(1.0, 2.0));
            Assert.IsTrue(SchemeBoolean.Equal(true, true));
            Assert.IsFalse(SchemeBoolean.Equal(true, false));
            Assert.IsTrue(SchemeBoolean.Equal('a', 'a'));
            Assert.IsFalse(SchemeBoolean.Equal('a', 'b'));
        }

        /// <summary>
        /// A test for Eqv
        /// </summary>
        [TestMethod]
        public void EqvTest()
        {
            Assert.IsTrue(SchemeBoolean.Eqv(null, null));
            Assert.IsFalse(SchemeBoolean.Eqv(null, 1));
            Assert.IsTrue(SchemeBoolean.Eqv("abc", "abc"));
            Assert.IsFalse(SchemeBoolean.Eqv("abc", "ab"));
            Assert.IsFalse(SchemeBoolean.Eqv("abc", 1));
            Obj[] vec1 = { 1, 2, 3 };
            Obj[] vec2 = { 1, 2, 3 };
            Obj[] vec3 = { 1, 2 };
            Obj[] vec4 = { 1, 2, 4 };
            Assert.IsTrue(SchemeBoolean.Eqv(vec1, vec1));
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec2));
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec3));
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec4));
            Assert.IsTrue(SchemeBoolean.Eqv(1, 1));
            Assert.IsFalse(SchemeBoolean.Eqv(1, 2));
            Assert.IsTrue(SchemeBoolean.Eqv(1.0, 1.0));
            Assert.IsFalse(SchemeBoolean.Eqv(1.0, 2.0));
            Assert.IsTrue(SchemeBoolean.Eqv(true, true));
            Assert.IsFalse(SchemeBoolean.Eqv(true, false));
            Assert.IsTrue(SchemeBoolean.Eqv('a', 'a'));
            Assert.IsFalse(SchemeBoolean.Eqv('a', 'b'));
        }

        /// <summary>
        /// A test for InPort
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void InPortTest()
        {
            var files = new string[0];
            var accessor = new Interpreter_Accessor(false, null, files);
            Interpreter interpreter = accessor.Target as Interpreter;
            Assert.IsNotNull(interpreter);
            Assert.AreEqual(accessor.Input, InputPort.InPort(List.Empty, interpreter.Input));
            using (StringReader reader = new StringReader("abc"))
            {
                InputPort input = new InputPort(reader);
                Assert.AreEqual(input, InputPort.InPort(input, interpreter.Input));
            }

            AssertEx.Throws(() => InputPort.InPort(1, interpreter.Input));
        }

        /// <summary>
        /// A test for OutPort
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void OutPortTest()
        {
            var files = new string[0];
            var accessor = new Interpreter_Accessor(false, null, files);
            Interpreter interpreter = accessor.Target as Interpreter;
            Assert.IsNotNull(interpreter);
            Assert.AreEqual(accessor.Output, OutputPort_Accessor.OutPort(List.Empty, interpreter.Output));
            using (StringWriter writer = new StringWriter())
            {
                OutputPort output = new OutputPort(writer);
                Assert.AreEqual(output, OutputPort_Accessor.OutPort(output, interpreter.Output));
            }

            AssertEx.Throws(() => OutputPort_Accessor.OutPort(1, interpreter.Output));
        }

        /// <summary>
        /// A test for IsFalse
        /// </summary>
        [TestMethod]
        public void IsFalseTest()
        {
            Assert.IsTrue(SchemeBoolean.IsFalse(false));
            Assert.IsFalse(SchemeBoolean.IsFalse(true));
            Assert.IsFalse(SchemeBoolean.IsFalse(0));
        }

        /// <summary>
        /// A test for IsTrue
        /// </summary>
        [TestMethod]
        public void IsTrueTest()
        {
            Assert.IsTrue(SchemeBoolean.IsTrue(true));
            Assert.IsFalse(SchemeBoolean.IsTrue(false));
            Assert.IsFalse(SchemeBoolean.IsTrue(0));
        }

        /// <summary>
        /// A test for Truth
        /// </summary>
        [TestMethod]
        public void TruthTest()
        {
            Assert.IsTrue(SchemeBoolean.Truth(true));
            Assert.IsFalse(SchemeBoolean.Truth(false));
            Assert.IsTrue(SchemeBoolean.Truth(0));
        }

        /// <summary>
        /// A test for StringLength
        /// </summary>
        [TestMethod]
        public void LengthTest()
        {
            Assert.AreEqual(0, ListPrimitives.Length(null));
            Assert.AreEqual(0, ListPrimitives.Length(0));
            var actual = ListPrimitives.Cons(1, 2);
            Assert.AreEqual(1, ListPrimitives.Length(actual));
            actual = ListPrimitives.Cons(1, ListPrimitives.Cons(2, 3));
            Assert.AreEqual(2, ListPrimitives.Length(actual));
        }

        /// <summary>
        /// A test for MakeList (one arg)
        /// </summary>
        [TestMethod]
        public void ListTest1()
        {
            var actual = ListPrimitives.MakeList(10);
            Assert.AreEqual(1, ListPrimitives.Length(actual));
            Assert.AreEqual(10, ListPrimitives.First(actual));
            Assert.AreEqual(List.Empty, ListPrimitives.Rest(actual));
        }

        /// <summary>
        /// A test for MakeList (two args)
        /// </summary>
        [TestMethod]
        public void ListTest2()
        {
            var actual = ListPrimitives.MakeList(10, 11);
            Assert.AreEqual(2, ListPrimitives.Length(actual));
            Assert.AreEqual(10, ListPrimitives.First(actual));
            Assert.AreEqual(11, ListPrimitives.First(ListPrimitives.Rest(actual)));
            Assert.AreEqual(List.Empty, ListPrimitives.Rest(ListPrimitives.Rest(actual)));
        }

        /// <summary>
        /// A test for ListStar
        /// </summary>
        [TestMethod]
        public void ListStarTest()
        {
            var actual = ListPrimitives.ListStar(ListPrimitives.MakeList(10));
            Assert.AreEqual(10, actual);
            actual = ListPrimitives.ListStar(ListPrimitives.MakeList(10, 11));
            Assert.AreEqual(10, ListPrimitives.First(actual));
            Assert.AreEqual(11, ((Pair)actual).RestCell);
            actual = ListPrimitives.ListStar(ListPrimitives.Cons(10, ListPrimitives.Cons(11, ListPrimitives.MakeList(12))));
            Assert.AreEqual(10, ListPrimitives.First(actual));
            Assert.AreEqual(11, ListPrimitives.Second(actual));
            Assert.AreEqual(List.Empty, ListPrimitives.Third(actual));
        }

        /// <summary>
        /// A test for ListToString
        /// </summary>
        [TestMethod]
        public void ListToStringTest()
        {
            var expected = new[] { 'a', 'b' };
            var actual = SchemeString.ListToString(ListPrimitives.MakeList('a', 'b'));
            for (int i = 0; i < expected.Length; i++)
            {
                Assert.AreEqual(expected[i], actual[i]);
            }

            actual = SchemeString.ListToString(1);
            Assert.AreEqual(0, actual.Length);
            AssertEx.Throws(() => SchemeString.ListToString(ListPrimitives.MakeList(1, 2)));
        }

        /// <summary>
        /// A test for ListToVector
        /// </summary>
        [TestMethod]
        public void ListToVectorTest()
        {
            var actual = Vector.MakeVector(ListPrimitives.MakeList(1, 2));
            var expected = new Obj[] { 1, 2 };
            Assert.AreEqual(2, actual.Length);
            Assert.AreEqual(2, expected.Length);
            Assert.AreEqual(expected[0], actual[0]);
            Assert.AreEqual(expected[1], actual[1]);
            actual = Vector.MakeVector(1);
            Assert.AreEqual(0, actual.Length);
        }

        /// <summary>
        /// A test for Num
        /// </summary>
        [TestMethod]
        public void NumTest()
        {
            Assert.AreEqual(0.0, Number.Num(0.0));
            Assert.AreEqual(0.0, Number.Num(0));
            Assert.AreEqual(0.0, Number.Num("0"));
            Assert.AreEqual(1.0, Number.Num(1.0));
            Assert.AreEqual(1.0, Number.Num(1));
            Assert.AreEqual(1.0, Number.Num("1"));
            Assert.AreEqual(0.0, Number.Num(false));
            AssertEx.Throws(() => Number.Num('a'));
            AssertEx.Throws(() => Number.Num("xxx"));
            AssertEx.Throws(() => Number.Num("false"));
        }

        /// <summary>
        /// A test for Reverse
        /// </summary>
        [TestMethod]
        public void ReverseTest()
        {
            var test = ListPrimitives.MakeList(1, 2);
            var actual = List_Accessor.Reverse(test);
            Assert.AreEqual(2, ListPrimitives.First(actual));
            Assert.AreEqual(1, ListPrimitives.First(ListPrimitives.Rest(actual)));
        }

        /// <summary>
        /// A test for Str
        /// </summary>
        [TestMethod]
        public void StrTest()
        {
            var actual = SchemeString_Accessor.Str(SchemeString.MakeString("abc"));
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
            Assert.AreEqual("()", SchemeString.AsString(List.Empty));
            Assert.AreEqual("1", SchemeString.AsString(1.0));
            Assert.AreEqual("1.5", SchemeString.AsString(1.5));
            Assert.AreEqual("#\\a", SchemeString.AsString('a'));
            Assert.AreEqual("(1 . 2)", SchemeString.AsString(new Pair(1, 2)));
            Assert.AreEqual("(1 2)", SchemeString.AsString(ListPrimitives.MakeList(1, 2)));
            Assert.AreEqual(@"abc", SchemeString.AsString("abc"));
            char[] empty = new char[0];
            Assert.AreEqual(@"""""", SchemeString.AsString(empty));
            var test = new Obj[] { 1, 2 };
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
            Assert.AreEqual("()", SchemeString.AsString(List.Empty, false));
            Assert.AreEqual("1", SchemeString.AsString(1.0, false));
            Assert.AreEqual("1.5", SchemeString.AsString(1.5, false));
            Assert.AreEqual("a", SchemeString.AsString('a', false));
            Assert.AreEqual("(1 . 2)", SchemeString.AsString(new Pair(1, 2), false));
            Assert.AreEqual("(1 2)", SchemeString.AsString(ListPrimitives.MakeList(1, 2), false));
            Assert.AreEqual("abc", SchemeString.AsString("abc", false));
            Assert.AreEqual(@"""", SchemeString.AsString(@"""", false));
            var test = new Obj[] { 1, 2 };
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
            Assert.AreEqual("x", buf.ToString());
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
            SchemeString.AsString("abc", false, buf);
            Assert.AreEqual("xabc", buf.ToString());
        }

        /// <summary>
        /// A test for Sym.
        /// Sym objects are just strings.
        /// </summary>
        [TestMethod]
        public void SymTest()
        {
            Assert.AreEqual("abc", Symbol.Sym("abc"));
            AssertEx.Throws(() => Symbol.Sym(1));
        }

        /// <summary>
        /// A test for Vec
        /// </summary>
        [TestMethod]
        public void VecTest()
        {
            var test = new Obj[] { 1, 2 };
            Assert.AreEqual(2, Vector_Accessor.Vec(test).Length);
            Assert.AreEqual(1, Vector_Accessor.Vec(test)[0]);
            Assert.AreEqual(2, Vector_Accessor.Vec(test)[1]);
            AssertEx.Throws(() => Vector_Accessor.Vec(1));
        }

        /// <summary>
        /// A test for VectorToList
        /// </summary>
        [TestMethod]
        public void VectorToListTest()
        {
            var test = new Obj[] { 1, 2, 3 };
            var actual = Vector_Accessor.VectorToList(test);
            Assert.AreEqual(3, ListPrimitives.Length(actual));
            Assert.AreEqual(1, ListPrimitives.First(actual));
            Assert.AreEqual(2, ListPrimitives.Second(actual));
            Assert.AreEqual(3, ListPrimitives.Third(actual));
        }

        /// <summary>
        /// A test for Warn
        /// </summary>
        [TestMethod]
        public void WarnTest()
        {
            ErrorHandlers.Warn("message");
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
                var actual = OutputPort_Accessor.Write("abc", w, false);
                Assert.AreEqual(Undefined.Instance, actual);
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
                Throws(typeof(ErrorHandlers.SchemeException), act);
            }
        }
    }
}
