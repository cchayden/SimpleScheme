// <copyright file="SchemeUtilsTest.cs" company="Charles Hayden">
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
        /// A scheme interpreter, created for each test.
        /// </summary>
        private Interpreter interpreter;

        /// <summary>
        /// Initialize each test.
        /// </summary>
        [TestInitialize]
        public void MyTestInitialize()
        {
            this.interpreter = new Interpreter();
        }

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
            var actual = List.Cons(1, 2);
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(1, List.First(actual));
            Assert.AreEqual(2, List.Rest(actual));
        }

        /// <summary>
        /// A test for Second
        /// </summary>
        [TestMethod]
        public void SecondTest()
        {
            var actual = List.Cons(1, List.Cons(2, 3));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(2, List.Second(actual));
        }

        /// <summary>
        /// A test for Third
        /// </summary>
        [TestMethod]
        public void ThirdTest()
        {
            var actual = List.Cons(1, List.Cons(2, List.Cons(3, 4)));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(3, List.Third(actual));
        }

        /// <summary>
        /// A test for SetFirst
        /// </summary>
        [TestMethod]
        public void SetFirstTest()
        {
            var actual = List.Cons(1, 2);
            List_Accessor.SetFirst(actual, 10);
            Assert.AreEqual(10, List.First(actual));
            AssertEx.Throws(() => List_Accessor.SetFirst(1, 10));
        }

        /// <summary>
        /// A test for SetRest
        /// </summary>
        [TestMethod]
        public void SetRestTest()
        {
            var actual = List.Cons(1, 2);
            List_Accessor.SetRest(actual, 10);
            Assert.AreEqual(10, List.Rest(actual));
            AssertEx.Throws(() => List_Accessor.SetRest(1, 10));
        }

        /// <summary>
        /// A test for Equal
        /// </summary>
        [TestMethod]
        public void EqualTest()
        {
            Assert.IsTrue(SchemeBoolean_Accessor.Equal(EmptyList_Accessor.Instance, EmptyList_Accessor.Instance));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal(EmptyList_Accessor.Instance, 1));
            Assert.IsTrue(SchemeBoolean_Accessor.Equal("abc", "abc"));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal("abc", "ab"));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal("abc", 1));
            var vec1 = new Obj[] { 1, 2, 3 };
            var vec2 = new Obj[] { 1, 2, 3 };
            var vec3 = new Obj[] { 1, 2 };
            var vec4 = new Obj[] { 1, 2, 4 };
            Assert.IsTrue(SchemeBoolean_Accessor.Equal(vec1, vec1));
            Assert.IsTrue(SchemeBoolean_Accessor.Equal(vec1, vec2));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal(vec1, vec3));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal(vec1, vec4));
            Assert.IsTrue(SchemeBoolean_Accessor.Equal(1, 1));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal(1, 2));
            Assert.IsTrue(SchemeBoolean_Accessor.Equal(1.0, 1.0));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal(1.0, 2.0));
            Assert.IsTrue(SchemeBoolean_Accessor.Equal(true, true));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal(true, false));
            Assert.IsTrue(SchemeBoolean_Accessor.Equal('a', 'a'));
            Assert.IsFalse(SchemeBoolean_Accessor.Equal('a', 'b'));
        }

        /// <summary>
        /// A test for Eqv
        /// </summary>
        [TestMethod]
        public void EqvTest()
        {
            Assert.IsTrue(SchemeBoolean_Accessor.Eqv(null, null));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv(null, 1));
            Assert.IsTrue(SchemeBoolean_Accessor.Eqv("abc", "abc"));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv("abc", "ab"));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv("abc", 1));
            Obj[] vec1 = { 1, 2, 3 };
            Obj[] vec2 = { 1, 2, 3 };
            Obj[] vec3 = { 1, 2 };
            Obj[] vec4 = { 1, 2, 4 };
            Assert.IsTrue(SchemeBoolean_Accessor.Eqv(vec1, vec1));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv(vec1, vec2));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv(vec1, vec3));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv(vec1, vec4));
            Assert.IsTrue(SchemeBoolean_Accessor.Eqv(1, 1));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv(1, 2));
            Assert.IsTrue(SchemeBoolean_Accessor.Eqv(1.0, 1.0));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv(1.0, 2.0));
            Assert.IsTrue(SchemeBoolean_Accessor.Eqv(true, true));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv(true, false));
            Assert.IsTrue(SchemeBoolean_Accessor.Eqv('a', 'a'));
            Assert.IsFalse(SchemeBoolean_Accessor.Eqv('a', 'b'));
        }

        /// <summary>
        /// A test for IsFalse
        /// </summary>
        [TestMethod]
        public void IsFalseTest()
        {
            Assert.IsTrue(SchemeBoolean_Accessor.IsFalse(false));
            Assert.IsFalse(SchemeBoolean_Accessor.IsFalse(true));
            Assert.IsFalse(SchemeBoolean_Accessor.IsFalse(0));
        }

        /// <summary>
        /// A test for IsTrue
        /// </summary>
        [TestMethod]
        public void IsTrueTest()
        {
            Assert.IsTrue(SchemeBoolean_Accessor.IsTrue(true));
            Assert.IsFalse(SchemeBoolean_Accessor.IsTrue(false));
            Assert.IsFalse(SchemeBoolean_Accessor.IsTrue(0));
        }

        /// <summary>
        /// A test for Truth
        /// </summary>
        [TestMethod]
        public void TruthTest()
        {
            Assert.IsTrue(SchemeBoolean_Accessor.Truth(true));
            Assert.IsFalse(SchemeBoolean_Accessor.Truth(false));
            Assert.IsTrue(SchemeBoolean_Accessor.Truth(0));
        }

        /// <summary>
        /// A test for StringLength
        /// </summary>
        [TestMethod]
        public void LengthTest()
        {
            Assert.AreEqual(0, List.Length(null));
            Assert.AreEqual(0, List.Length(0));
            var actual = List.Cons(1, 2);
            Assert.AreEqual(1, List.Length(actual));
            actual = List.Cons(1, List.Cons(2, 3));
            Assert.AreEqual(2, List.Length(actual));
        }

        /// <summary>
        /// A test for New (one arg)
        /// </summary>
        [TestMethod]
        public void ListTest1()
        {
            var actual = List.New(10);
            Assert.AreEqual(1, List.Length(actual));
            Assert.AreEqual(10, List.First(actual));
            Assert.AreEqual(EmptyList_Accessor.Instance, List.Rest(actual));
        }

        /// <summary>
        /// A test for New (two args)
        /// </summary>
        [TestMethod]
        public void ListTest2()
        {
            var actual = List.New(10, 11);
            Assert.AreEqual(2, List.Length(actual));
            Assert.AreEqual(10, List.First(actual));
            Assert.AreEqual(11, List.First(List.Rest(actual)));
            Assert.AreEqual(EmptyList_Accessor.Instance, List.Rest(List.Rest(actual)));
        }

        /// <summary>
        /// A test for ListStar
        /// </summary>
        [TestMethod]
        public void ListStarTest()
        {
            var actual = List_Accessor.ListStar(List.New(10));
            Assert.AreEqual(10, actual);
            actual = List_Accessor.ListStar(List.New(10, List.New(11)));
            Assert.AreEqual(10, List.First(actual));
            Assert.AreEqual(11, List.First(List.Rest(actual)));
            actual = List_Accessor.ListStar(List.Cons(10, List.New(11, List.New(12))));
            Assert.AreEqual(10, List.First(actual));
            Assert.AreEqual(11, List.Second(actual));
            Assert.AreEqual(12, List.Third(actual));
            Assert.AreEqual(EmptyList_Accessor.Instance, List.Rest(List.Third(actual)));
        }

        /// <summary>
        /// A test for ListToString
        /// </summary>
        [TestMethod]
        public void ListToStringTest()
        {
            var expected = new[] { 'a', 'b' };
            var actual = SchemeString_Accessor.ListToString(List.New('a', 'b'));
            for (int i = 0; i < expected.Length; i++)
            {
                Assert.AreEqual(expected[i], actual[i]);
            }

            actual = SchemeString_Accessor.ListToString(1);
            Assert.AreEqual(0, actual.Length);
            AssertEx.Throws(() => SchemeString_Accessor.ListToString(List.New(1, 2)));
        }

        /// <summary>
        /// A test for ListToVector
        /// </summary>
        [TestMethod]
        public void ListToVectorTest()
        {
            var actual = Vector_Accessor.FromList(List.New(1, 2));
            var expected = new Obj[] { 1, 2 };
            Assert.AreEqual(2, actual.Length);
            Assert.AreEqual(2, expected.Length);
            Assert.AreEqual(expected[0], actual[0]);
            Assert.AreEqual(expected[1], actual[1]);
            actual = Vector_Accessor.FromList(1);
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
            AssertEx.Throws(() => Number.Num("0"));
            Assert.AreEqual(1.0, Number.Num(1.0));
            Assert.AreEqual(1.0, Number.Num(1));
            AssertEx.Throws(() => Number.Num("1"));
            AssertEx.Throws(() => Number.Num(false));
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
            var test = List.New(1, 2);
            var actual = List_Accessor.Reverse(test);
            Assert.AreEqual(2, List.First(actual));
            Assert.AreEqual(1, List.First(List.Rest(actual)));
        }

        /// <summary>
        /// A test for Str
        /// </summary>
        [TestMethod]
        public void StrTest()
        {
            var actual = SchemeString.Str(SchemeString_Accessor.MakeString("abc"));
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
            Assert.AreEqual("()", Printer.AsString(EmptyList_Accessor.Instance));
            Assert.AreEqual("1", Printer.AsString(1.0));
            Assert.AreEqual("1.5", Printer.AsString(1.5));
            Assert.AreEqual("#\\a", Printer.AsString('a'));
            Assert.AreEqual("(1 . 2)", Printer.AsString(new Pair_Accessor(1, 2)));
            Assert.AreEqual("(1 2)", Printer.AsString(List.New(1, 2)));
            Assert.AreEqual(@"abc", Printer.AsString("abc"));
            char[] empty = new char[0];
            Assert.AreEqual(@"""""", Printer.AsString(empty));
            var test = new Obj[] { 1, 2 };
            Assert.AreEqual("#(1 2)", Printer.AsString(test));
            Assert.AreEqual("#t", Printer.AsString(true));
            Assert.AreEqual("#f", Printer.AsString(false));
            Assert.AreEqual("1", Printer.AsString(1));
        }

        /// <summary>
        /// A test for AsString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithQuote()
        {
            Assert.AreEqual("()", Printer.AsString(EmptyList_Accessor.Instance, false));
            Assert.AreEqual("1", Printer.AsString(1.0, false));
            Assert.AreEqual("1.5", Printer.AsString(1.5, false));
            Assert.AreEqual("a", Printer.AsString('a', false));
            Assert.AreEqual("(1 . 2)", Printer.AsString(new Pair_Accessor(1, 2), false));
            Assert.AreEqual("(1 2)", Printer.AsString(List.New(1, 2), false));
            Assert.AreEqual("abc", Printer.AsString("abc", false));
            Assert.AreEqual(@"""", Printer.AsString(@"""", false));
            var test = new Obj[] { 1, 2 };
            Assert.AreEqual("#(1 2)", Printer.AsString(test, false));
            Assert.AreEqual("#t", Printer.AsString(true, false));
            Assert.AreEqual("#f", Printer.AsString(false, false));
            Assert.AreEqual("1", Printer.AsString(1));
        }

        /// <summary>
        /// A test for AsString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithBuf()
        {
            StringBuilder buf = new StringBuilder().Append("x");
            Printer_Accessor.AsString(null, false, buf);
            Assert.AreEqual("x", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer_Accessor.AsString(1.0, false, buf);
            Assert.AreEqual("x1", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer_Accessor.AsString(1.5, false, buf);
            Assert.AreEqual("x1.5", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer_Accessor.AsString('a', false, buf);
            Assert.AreEqual("xa", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer_Accessor.AsString("abc", false, buf);
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
            var test = new Obj[] { 1, 2, 3 };
            var actual = Vector_Accessor.VectorToList(test);
            Assert.AreEqual(3, List.Length(actual));
            Assert.AreEqual(1, List.First(actual));
            Assert.AreEqual(2, List.Second(actual));
            Assert.AreEqual(3, List.Third(actual));
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
                var outp = new OutputPort(writer, this.interpreter);
                OutputPort_Accessor.WriteObj("abc", outp);
                Assert.AreEqual("abc", writer.ToString());
            }
        }

        /// <summary>
        /// A test for type name.
        /// </summary>
        [TestMethod]
        public void TypeNameTest()
        {
            Assert.AreEqual("boolean", TypePrimitives_Accessor.TypeName(true));
            Assert.AreEqual("symbol", TypePrimitives_Accessor.TypeName("sym"));
            Assert.AreEqual("character", TypePrimitives_Accessor.TypeName('c'));
            Assert.AreEqual("vector", TypePrimitives_Accessor.TypeName(new Obj[] { 1, 2, 3 }));
            Assert.AreEqual("pair", TypePrimitives_Accessor.TypeName(new Pair_Accessor(null, null)));
            Assert.AreEqual("number", TypePrimitives_Accessor.TypeName(1.0d));
            Assert.AreEqual("string", TypePrimitives_Accessor.TypeName(new[] { 'a', 'b', 'c' }));
            Assert.AreEqual("procedure", TypePrimitives_Accessor.TypeName(new Primitive_Accessor((args, caller) => null, 0, 0)));
            Assert.AreEqual("input port", TypePrimitives_Accessor.TypeName(new InputPort_Accessor(new StringReader(string.Empty), this.interpreter)));
            Assert.AreEqual("output port", TypePrimitives_Accessor.TypeName(new OutputPort(new StringWriter(), this.interpreter)));
            Assert.AreEqual("empty list", TypePrimitives_Accessor.TypeName(EmptyList_Accessor.Instance));
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
