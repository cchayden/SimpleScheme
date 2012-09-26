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
        /// A test for Chr
        /// </summary>
        [TestMethod]
        public void ChrTest()
        {
            Assert.AreEqual('a', Character.New('a').AsCharacter().C);
            AssertEx.Throws(() => 0.AsCharacter());
        }

        /// <summary>
        /// A test for Cons, First, Rest
        /// </summary>
        [TestMethod]
        public void ConsTest()
        {
            var actual = 1.Cons(2);
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(1, actual.First());
            Assert.AreEqual(2, actual.Rest());
        }

        /// <summary>
        /// A test for Second
        /// </summary>
        [TestMethod]
        public void SecondTest()
        {
            var actual = 1.Cons(List.Cons(2, 3));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(2, actual.Second());
        }

        /// <summary>
        /// A test for Third
        /// </summary>
        [TestMethod]
        public void ThirdTest()
        {
            var actual = 1.Cons(List.Cons(2, List.Cons(3, 4)));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(3, actual.Third());
        }

        /// <summary>
        /// A test for SetFirst
        /// </summary>
        [TestMethod]
        public void SetFirstTest()
        {
            var actual = 1.Cons(2);
            actual.SetFirst(10);
            Assert.AreEqual(10, actual.First());
            AssertEx.Throws(() => 1.SetFirst(10));
        }

        /// <summary>
        /// A test for SetRest
        /// </summary>
        [TestMethod]
        public void SetRestTest()
        {
            var actual = 1.Cons(2);
            actual.SetRest(10);
            Assert.AreEqual(10, actual.Rest());
            AssertEx.Throws(() => 1.SetRest(10));
        }

        /// <summary>
        /// A test for Equal
        /// </summary>
        [TestMethod]
        public void EqualTest()
        {
            Assert.IsTrue(SchemeBoolean.Equal(EmptyList.New(), EmptyList.New()).Value);
            Assert.IsFalse(SchemeBoolean.Equal(EmptyList.New(), 1).Value);
            Assert.IsTrue(SchemeBoolean.Equal("abc", "abc").Value);
            Assert.IsFalse(SchemeBoolean.Equal("abc", "ab").Value);
            Assert.IsFalse(SchemeBoolean.Equal("abc", 1).Value);
            var vec1 = Vector.New(3);
            vec1[0] = 1;
            vec1[1] = 2;
            vec1[2] = 3;
            var vec2 = Vector.New(3);
            vec2[0] = 1;
            vec2[1] = 2;
            vec2[2] = 3;
            var vec3 = Vector.New(2);
            vec3[0] = 1;
            vec3[1] = 2;
            var vec4 = Vector.New(3);
            vec4[0] = 1;
            vec4[1] = 2;
            vec4[2] = 4;
            Assert.IsTrue(SchemeBoolean.Equal(vec1, vec1).Value);
            Assert.IsTrue(SchemeBoolean.Equal(vec1, vec2).Value);
            Assert.IsFalse(SchemeBoolean.Equal(vec1, vec3).Value);
            Assert.IsFalse(SchemeBoolean.Equal(vec1, vec4).Value);
            Assert.IsTrue(SchemeBoolean.Equal(1, 1).Value);
            Assert.IsFalse(SchemeBoolean.Equal(1, 2).Value);
            Assert.IsTrue(SchemeBoolean.Equal(1.0, 1.0).Value);
            Assert.IsFalse(SchemeBoolean.Equal(1.0, 2.0).Value);
            Assert.IsTrue(SchemeBoolean.Equal(true, true).Value);
            Assert.IsFalse(SchemeBoolean.Equal(true, false).Value);
            Assert.IsTrue(SchemeBoolean.Equal('a', 'a').Value);
            Assert.IsFalse(SchemeBoolean.Equal('a', 'b').Value);
        }

        /// <summary>
        /// A test for Eqv
        /// </summary>
        [TestMethod]
        public void EqvTest()
        {
            Assert.IsTrue(SchemeBoolean.Eqv(null, null).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(null, Number.New(1)).Value);
            var abc = SchemeString.New("abc");
            var ab = SchemeString.New("ab");
            var n1 = Number.New(1);
            Assert.IsTrue(SchemeBoolean.Eqv(abc, abc).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(abc, ab).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(abc, n1).Value);
            Obj[] vec1 = { 1, 2, 3 };
            Obj[] vec2 = { 1, 2, 3 };
            Obj[] vec3 = { 1, 2 };
            Obj[] vec4 = { 1, 2, 4 };
            Assert.IsTrue(SchemeBoolean.Eqv(vec1, vec1).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec2).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec3).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec4).Value);
            Assert.IsTrue(SchemeBoolean.Eqv(Number.New(1), Number.New(1)).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(Number.New(1), Number.New(2)).Value);
            Assert.IsTrue(SchemeBoolean.Eqv(Number.New(1.0), Number.New(1.0)).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(Number.New(1.0), Number.New(2.0)).Value);
            Assert.IsTrue(SchemeBoolean.Eqv(SchemeBoolean.True, SchemeBoolean.True).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(SchemeBoolean.True, SchemeBoolean.False).Value);
            Assert.IsTrue(SchemeBoolean.Eqv(Character.New('a'), Character.New('a')).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(Character.New('a'), Character.New('b')).Value);
        }

        /// <summary>
        /// A test for IsFalse
        /// </summary>
        [TestMethod]
        public void IsFalseTest()
        {
            Assert.IsTrue(SchemeBoolean.IsFalse(SchemeBoolean.False));
            Assert.IsFalse(SchemeBoolean.IsFalse(SchemeBoolean.True));
            Assert.IsFalse(SchemeBoolean.IsFalse(0));
        }

        /// <summary>
        /// A test for IsTrue
        /// </summary>
        [TestMethod]
        public void IsTrueTest()
        {
            Assert.IsTrue(SchemeBoolean.IsTrue(SchemeBoolean.True));
            Assert.IsFalse(SchemeBoolean.IsTrue(SchemeBoolean.False));
            Assert.IsFalse(SchemeBoolean.IsTrue(0));
        }

        /// <summary>
        /// A test for Truth
        /// </summary>
        [TestMethod]
        public void TruthTest()
        {
            Assert.IsTrue(SchemeBoolean.Truth(SchemeBoolean.True).Value);
            Assert.IsFalse(SchemeBoolean.Truth(SchemeBoolean.False).Value);
            Assert.IsTrue(SchemeBoolean.Truth(0).Value);
        }

        /// <summary>
        /// A test for StringLength
        /// </summary>
        [TestMethod]
        public void LengthTest()
        {
            Assert.AreEqual(0, List.ListLength(null));
            Assert.AreEqual(0, 0.ListLength());
            var actual = 1.Cons(2);
            Assert.AreEqual(1, actual.ListLength());
            actual = 1.Cons(List.Cons(2, 3));
            Assert.AreEqual(2, actual.ListLength());
        }

        /// <summary>
        /// A test for MakeList (one arg)
        /// </summary>
        [TestMethod]
        public void ListTest1()
        {
            var actual = 10.MakeList();
            Assert.AreEqual(1, actual.ListLength());
            Assert.AreEqual(10, actual.First());
            Assert.AreEqual(EmptyList.New(), actual.Rest());
        }

        /// <summary>
        /// A test for MakeList (two args)
        /// </summary>
        [TestMethod]
        public void ListTest2()
        {
            var actual = 10.MakeList(11);
            Assert.AreEqual(2, actual.ListLength());
            Assert.AreEqual(10, actual.First());
            Assert.AreEqual(11, actual.Rest().First());
            Assert.AreEqual(EmptyList.New(), actual.Rest().Rest());
        }

        /// <summary>
        /// A test for ListStar
        /// </summary>
        [TestMethod]
        public void ListStarTest()
        {
            var actual = 10.MakeList().ListStar();
            Assert.AreEqual(10, actual);
            actual = 10.MakeList(11.MakeList()).ListStar();
            Assert.AreEqual(10, actual.First());
            Assert.AreEqual(11, actual.Rest().First());
            actual = 10.Cons(11.MakeList(12.MakeList())).ListStar();
            Assert.AreEqual(10, actual.First());
            Assert.AreEqual(11, actual.Second());
            Assert.AreEqual(12, actual.Third());
            Assert.AreEqual(EmptyList.New(), actual.Third().Rest());
        }

        /// <summary>
        /// A test for ListToString
        /// </summary>
        [TestMethod]
        public void ListToStringTest()
        {
            var expected = new[] { 'a', 'b' };
            var actual = SchemeString.ListToString(Character.New('a').MakeList(Character.New('b')));
            for (int i = 0; i < expected.Length; i++)
            {
                Assert.AreEqual(expected[i], actual.Str[i]);
            }

            actual = SchemeString.ListToString(1);
            Assert.AreEqual(0, actual.Str.Length);
            AssertEx.Throws(() => SchemeString.ListToString(1.MakeList(2)));
        }

        /// <summary>
        /// A test for ListToVector
        /// </summary>
        [TestMethod]
        public void ListToVectorTest()
        {
            var actual = Vector.FromList(1.MakeList(2));
            var expected = new Obj[] { 1, 2 };
            Assert.AreEqual(2, actual.Length);
            Assert.AreEqual(2, expected.Length);
            Assert.AreEqual(expected[0], actual[0]);
            Assert.AreEqual(expected[1], actual[1]);
            actual = Vector.FromList(1);
            Assert.AreEqual(0, actual.Length);
        }

        /// <summary>
        /// A test for Num
        /// </summary>
        [TestMethod]
        public void NumTest()
        {
            Assert.AreEqual(0.0, Number.New(0.0).AsNumber().N);
            Assert.AreEqual(0.0, Number.New(0).AsNumber().N);
            AssertEx.Throws(() => "0".AsNumber());
            Assert.AreEqual(1.0, Number.New(1.0).AsNumber().N);
            Assert.AreEqual(1.0, Number.New(1).AsNumber().N);
            AssertEx.Throws(() => "1".AsNumber());
            AssertEx.Throws(() => false.AsNumber());
            AssertEx.Throws(() => 'a'.AsNumber());
            AssertEx.Throws(() => "xxx".AsNumber());
            AssertEx.Throws(() => "false".AsNumber());
        }

        /// <summary>
        /// A test for ReverseList
        /// </summary>
        [TestMethod]
        public void ReverseTest()
        {
            var test = 1.MakeList(2);
            var actual = test.ReverseList();
            Assert.AreEqual(2, actual.First());
            Assert.AreEqual(1, actual.Rest().First());
        }

        /// <summary>
        /// A test for Str
        /// </summary>
        [TestMethod]
        public void StrTest()
        {
            var actual = SchemeString.New(Symbol.New("abc")).AsSchemeString();
            Assert.AreEqual(3, actual.Str.Length);
            Assert.AreEqual('a', actual.Str[0]);
            Assert.AreEqual('b', actual.Str[1]);
            Assert.AreEqual('c', actual.Str[2]);
        }

        /// <summary>
        /// A test for PrintString
        /// </summary>
        [TestMethod]
        public void AsStringTest()
        {
            Assert.AreEqual("()", Printer.AsString(EmptyList.New()));
            Assert.AreEqual("1", Printer.AsString(Number.New(1.0)));
            Assert.AreEqual("1.5", Printer.AsString(Number.New(1.5)));
            Assert.AreEqual("#\\a", Printer.AsString(Character.New('a')));
            Assert.AreEqual("(1 . 2)", Printer.AsString(new Pair(1, 2)));
            Assert.AreEqual("(1 2)", Printer.AsString(1.MakeList(2)));
            Assert.AreEqual(@"abc", Printer.AsString("abc"));
            SchemeString empty = SchemeString.New(0);
            Assert.AreEqual(@"""""", Printer.AsString(empty));
            var test = Vector.New(2);
            test[0] = 1;
            test[1] = 2;
            Assert.AreEqual("#(1 2)", Printer.AsString(test));
            Assert.AreEqual("#t", Printer.AsString(SchemeBoolean.True));
            Assert.AreEqual("#f", Printer.AsString(SchemeBoolean.False));
            Assert.AreEqual("1", Printer.AsString(1));
        }

        /// <summary>
        /// A test for PrintString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithQuote()
        {
            Assert.AreEqual("()", Printer.AsString(EmptyList.New(), false));
            Assert.AreEqual("1", Printer.AsString(1.0, false));
            Assert.AreEqual("1.5", Printer.AsString(1.5, false));
            Assert.AreEqual("a", Printer.AsString('a', false));
            Assert.AreEqual("(1 . 2)", Printer.AsString(new Pair(1, 2), false));
            Assert.AreEqual("(1 2)", Printer.AsString(1.MakeList(2), false));
            Assert.AreEqual("abc", Printer.AsString("abc", false));
            Assert.AreEqual(@"""", Printer.AsString(@"""", false));
            var test = Vector.New(2);
            test[0] = 1;
            test[1] = 2;
            Assert.AreEqual("#(1 2)", Printer.AsString(test, false));
            Assert.AreEqual("#t", Printer.AsString(SchemeBoolean.True, false));
            Assert.AreEqual("#f", Printer.AsString(SchemeBoolean.False, false));
            Assert.AreEqual("1", Printer.AsString(1));
        }

        /// <summary>
        /// A test for PrintString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithBuf()
        {
            StringBuilder buf = new StringBuilder().Append("x");
            Printer.PrintString(null, false, buf);
            Assert.AreEqual("x", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer.PrintString(1.0, false, buf);
            Assert.AreEqual("x1", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer.PrintString(1.5, false, buf);
            Assert.AreEqual("x1.5", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer.PrintString('a', false, buf);
            Assert.AreEqual("xa", buf.ToString());
            buf = new StringBuilder().Append("x");
            Printer.PrintString("abc", false, buf);
            Assert.AreEqual("xabc", buf.ToString());
        }

        /// <summary>
        /// A test for Sym.
        /// Sym objects are just strings.
        /// </summary>
        [TestMethod]
        public void SymTest()
        {
            Assert.AreEqual("abc", Symbol.New("abc").AsSymbol().ToString());
            AssertEx.Throws(() => 1.AsSymbol());
        }

        /// <summary>
        /// A test for Vec
        /// </summary>
        [TestMethod]
        public void VecTest()
        {
            var test = Vector.New(2);
            test[0] = 1;
            test[1] = 2;
            Assert.AreEqual(2, test.AsVector().Length);
            Assert.AreEqual(1, test.AsVector()[0]);
            Assert.AreEqual(2, test.AsVector()[1]);
            AssertEx.Throws(() => 1.AsVector());
        }

        /// <summary>
        /// A test for VectorToList
        /// </summary>
        [TestMethod]
        public void VectorToListTest()
        {
            var test = Vector.New(3);
            test[0] = 1;
            test[1] = 2;
            test[2] = 3;
            var actual = Vector.ToList(test);
            Assert.AreEqual(3, actual.ListLength());
            Assert.AreEqual(1, actual.First());
            Assert.AreEqual(2, actual.Second());
            Assert.AreEqual(3, actual.Third());
        }

        /// <summary>
        /// A test for Warn
        /// </summary>
        [TestMethod]
        public void WarnTest()
        {
            ErrorHandlers.Warn("message");
            Assert.IsTrue(true, "threw exception");
        }

        /// <summary>
        /// A test for Write
        /// </summary>
        [TestMethod]
        public void WriteTest()
        {
            using (StringWriter writer = new StringWriter())
            {
                var outp = OutputPort.New(writer, (Interpreter)this.interpreter);
                outp.Write("abc");
                Assert.AreEqual("abc", writer.ToString());
            }
        }

        /// <summary>
        /// A test for type name.
        /// </summary>
        [TestMethod]
        public void TypeNameTest()
        {
            Assert.AreEqual("boolean", TypePrimitives.TypeName(SchemeBoolean.True));
            Assert.AreEqual("symbol", TypePrimitives.TypeName(Symbol.New("sym")));
            Assert.AreEqual("character", TypePrimitives.TypeName(Character.New('c')));
            Assert.AreEqual("vector", TypePrimitives.TypeName(Vector.New(Number.New(3), Number.New(0))));
            Assert.AreEqual("pair", TypePrimitives.TypeName(new Pair(null, null)));
            Assert.AreEqual("number", TypePrimitives.TypeName(Number.New(1.0d)));
            Assert.AreEqual("string", TypePrimitives.TypeName(SchemeString.New("abc")));
            Assert.AreEqual("primitive", TypePrimitives.TypeName(Primitive.New((args, caller) => null, 0, 0, new Primitive.ValueType[0])));
            Assert.AreEqual("input-port", TypePrimitives.TypeName(InputPort.New(new StringReader(string.Empty), (Interpreter)this.interpreter)));
            Assert.AreEqual("output-port", TypePrimitives.TypeName(OutputPort.New(new StringWriter(), (Interpreter)this.interpreter)));
            Assert.AreEqual("empty-list", TypePrimitives.TypeName(EmptyList.New()));
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
