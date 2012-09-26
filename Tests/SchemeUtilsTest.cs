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
            Assert.AreEqual('a', ((Character)'a').C);
        }

        /// <summary>
        /// A test for Cons, First, Rest
        /// </summary>
        [TestMethod]
        public void ConsTest()
        {
            var actual = List.Cons((Number)1, (Number)2);
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(1.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(2.0, ((Number)List.Rest(actual)).N);
        }

        /// <summary>
        /// A test for Second
        /// </summary>
        [TestMethod]
        public void SecondTest()
        {
            var actual = List.Cons((Number)1, List.Cons((Number)2, (Number)3));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(2, ((Number)List.Second(actual)).N);
        }

        /// <summary>
        /// A test for Third
        /// </summary>
        [TestMethod]
        public void ThirdTest()
        {
            var actual = List.Cons((Number)1, List.Cons((Number)2, List.Cons((Number)3, (Number)4)));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(3, ((Number)List.Third(actual)).N);
        }

        /// <summary>
        /// A test for Nth
        /// </summary>
        [TestMethod]
        public void NthTest()
        {
            var actual = List.Cons((Number)1, List.Cons((Number)2, List.Cons((Number)3, (Number)4)));
            Assert.IsInstanceOfType(actual, typeof(Pair));
            Assert.AreEqual(1, ((Number)List.Nth(actual, (Number)0)).N);
            Assert.AreEqual(2, ((Number)List.Nth(actual, (Number)1)).N);
            Assert.AreEqual(3, ((Number)List.Nth(actual, (Number)2)).N);
            Assert.AreEqual(EmptyList.Instance, List.Nth(actual, (Number)3));
        }

        /// <summary>
        /// A test for SetFirst
        /// </summary>
        [TestMethod]
        public void SetFirstTest()
        {
            var actual = List.Cons((Number)1, (Number)2);
            actual.SetFirst((Number)10);
            Assert.AreEqual(10, ((Number)List.First(actual)).N);
            AssertEx.Throws(() => List.SetFirst((Number)1, (Number)10));
        }

        /// <summary>
        /// A test for SetRest
        /// </summary>
        [TestMethod]
        public void SetRestTest()
        {
            var actual = List.Cons((Number)1, (Number)2);
            actual.SetRest((Number)10);
            Assert.AreEqual(10, ((Number)List.Rest(actual)).N);
            AssertEx.Throws(() => List.SetRest((Number)1, (Number)10));
        }

        /// <summary>
        /// A test for Equal
        /// </summary>
        [TestMethod]
        public void EqualTest()
        {
            Assert.IsTrue(SchemeBoolean.Equal(EmptyList.Instance, EmptyList.Instance).Value);
            Assert.IsFalse(SchemeBoolean.Equal(EmptyList.Instance, (Number)1).Value);
            Assert.IsTrue(SchemeBoolean.Equal((Symbol)"abc", (Symbol)"abc").Value);
            Assert.IsFalse(SchemeBoolean.Equal((Symbol)"abc", (Symbol)"ab").Value);
            Assert.IsFalse(SchemeBoolean.Equal((Symbol)"abc", (Number)1).Value);
            var vec1 = new Vector(3);
            vec1[0] = (Number)1;
            vec1[1] = (Number)2;
            vec1[2] = (Number)3;
            var vec2 = new Vector(3);
            vec2[0] = (Number)1;
            vec2[1] = (Number)2;
            vec2[2] = (Number)3;
            var vec3 = new Vector(2);
            vec3[0] = (Number)1;
            vec3[1] = (Number)2;
            var vec4 = new Vector(3);
            vec4[0] = (Number)1;
            vec4[1] = (Number)2;
            vec4[2] = (Number)4;
            Assert.IsTrue(SchemeBoolean.Equal(vec1, vec1).Value);
            Assert.IsTrue(SchemeBoolean.Equal(vec1, vec2).Value);
            Assert.IsFalse(SchemeBoolean.Equal(vec1, vec3).Value);
            Assert.IsFalse(SchemeBoolean.Equal(vec1, vec4).Value);
            Assert.IsTrue(SchemeBoolean.Equal((Number)1, (Number)1).Value);
            Assert.IsFalse(SchemeBoolean.Equal((Number)1, (Number)2).Value);
            Assert.IsTrue(SchemeBoolean.Equal((Number)1.0, (Number)1.0).Value);
            Assert.IsFalse(SchemeBoolean.Equal((Number)1.0, (Number)2.0).Value);
            Assert.IsTrue(SchemeBoolean.Equal((Character)'a', (Character)'a').Value);
            Assert.IsFalse(SchemeBoolean.Equal((Character)'a', (Character)'b').Value);
        }

        /// <summary>
        /// A test for Eqv
        /// </summary>
        [TestMethod]
        public void EqvTest()
        {
            Assert.IsTrue(SchemeBoolean.Eqv(Undefined.Instance, Undefined.Instance).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(Undefined.Instance, (Number)1).Value);
            SchemeString abc = "abc";
            SchemeString ab = "ab";
            var n1 = (Number)1;
            Assert.IsTrue(SchemeBoolean.Eqv(abc, abc).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(abc, ab).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(abc, n1).Value);
            var vec1 = new Vector(3);
            vec1[0] = (Number)1;
            vec1[1] = (Number)2;
            vec1[2] = (Number)3;
            var vec2 = new Vector(3);
            vec2[0] = (Number)1;
            vec2[1] = (Number)2;
            vec2[2] = (Number)3;
            var vec3 = new Vector(2);
            vec3[0] = (Number)1;
            vec3[1] = (Number)2;
            var vec4 = new Vector(3);
            vec4[0] = (Number)1;
            vec4[1] = (Number)2;
            vec4[2] = (Number)4;
            Assert.IsTrue(SchemeBoolean.Eqv(vec1, vec1).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec2).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec3).Value);
            Assert.IsFalse(SchemeBoolean.Eqv(vec1, vec4).Value);
            Assert.IsTrue(SchemeBoolean.Eqv((Number)1, (Number)1).Value);
            Assert.IsFalse(SchemeBoolean.Eqv((Number)1, (Number)2).Value);
            Assert.IsTrue(SchemeBoolean.Eqv((Number)1.0, (Number)1.0).Value);
            Assert.IsFalse(SchemeBoolean.Eqv((Number)1.0, (Number)2.0).Value);
            Assert.IsTrue(SchemeBoolean.Eqv((SchemeBoolean)true, (SchemeBoolean)true).Value);
            Assert.IsFalse(SchemeBoolean.Eqv((SchemeBoolean)true, (SchemeBoolean)false).Value);
            Assert.IsTrue(SchemeBoolean.Eqv((Character)'a', (Character)'a').Value);
            Assert.IsFalse(SchemeBoolean.Eqv((Character)'a', (Character)'b').Value);
        }

        /// <summary>
        /// A test for IsFalse
        /// </summary>
        [TestMethod]
        public void IsFalseTest()
        {
            Assert.IsTrue(SchemeBoolean.IsFalse((SchemeBoolean)false));
            Assert.IsFalse(SchemeBoolean.IsFalse((SchemeBoolean)true));
            Assert.IsFalse(SchemeBoolean.IsFalse((Number)0));
        }

        /// <summary>
        /// A test for IsTrue
        /// </summary>
        [TestMethod]
        public void IsTrueTest()
        {
            Assert.IsTrue(SchemeBoolean.IsTrue((SchemeBoolean)true));
            Assert.IsFalse(SchemeBoolean.IsTrue((SchemeBoolean)false));
            Assert.IsFalse(SchemeBoolean.IsTrue((Number)0));
        }

        /// <summary>
        /// A test for Truth
        /// </summary>
        [TestMethod]
        public void TruthTest()
        {
            Assert.IsTrue(SchemeBoolean.Truth((SchemeBoolean)true).Value);
            Assert.IsFalse(SchemeBoolean.Truth((SchemeBoolean)false).Value);
            Assert.IsTrue(SchemeBoolean.Truth((Number)0).Value);
        }

        /// <summary>
        /// A test for StringLength
        /// </summary>
        [TestMethod]
        public void LengthTest()
        {
            Assert.AreEqual(0, List.ListLength(Undefined.Instance));
            Assert.AreEqual(0, List.ListLength((Number)0));
            var actual = List.Cons((Number)1, (Number)2);
            Assert.AreEqual(1, List.ListLength(actual));
            actual = List.Cons((Number)1, List.Cons((Number)2, (Number)3));
            Assert.AreEqual(2, List.ListLength(actual));
        }

        /// <summary>
        /// A test for MakeList (one arg)
        /// </summary>
        [TestMethod]
        public void ListTest1()
        {
            var actual = List.MakeList((Number)10);
            Assert.AreEqual(1, List.ListLength(actual));
            Assert.AreEqual(10.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(EmptyList.Instance, List.Rest(actual));
        }

        /// <summary>
        /// A test for MakeList (two args)
        /// </summary>
        [TestMethod]
        public void ListTest2()
        {
            var actual = List.MakeList((Number)10, (Number)11);
            Assert.AreEqual(2, List.ListLength(actual));
            Assert.AreEqual(10.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(11.0, ((Number)List.First(List.Rest(actual))).N);
            Assert.AreEqual(EmptyList.Instance, List.Rest(List.Rest(actual)));
        }

        /// <summary>
        /// A test for ListStar
        /// </summary>
        [TestMethod]
        public void ListStarTest()
        {
            var actual = List.ListStar(List.MakeList((Number)10));
            Assert.AreEqual(10.0, ((Number)actual).N);
            actual = List.ListStar(List.MakeList((Number)10, List.MakeList((Number)11)));
            Assert.AreEqual(10.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(11.0, ((Number)List.First(List.Rest(actual))).N);
            actual = List.ListStar(List.Cons((Number)10, List.MakeList((Number)11, List.MakeList((Number)12))));
            Assert.AreEqual(10.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(11.0, ((Number)List.Second(actual)).N);
            Assert.AreEqual(12.0, ((Number)List.Third(actual)).N);
            Assert.AreEqual(EmptyList.Instance, List.Rest(List.Third(actual)));
        }

        /// <summary>
        /// A test for ListToString
        /// </summary>
        [TestMethod]
        public void ListToStringTest()
        {
            var expected = new[] { 'a', 'b' };
            var actual = SchemeString.ListToString(List.MakeList((Character)'a', (Character)'b'));
            for (int i = 0; i < expected.Length; i++)
            {
                Assert.AreEqual(expected[i], actual.Str[i]);
            }

            actual = SchemeString.ListToString((Number)1);
            Assert.AreEqual(0, actual.Str.Length);
            AssertEx.Throws(() => SchemeString.ListToString(List.MakeList((Number)1, (Number)2)));
        }

        /// <summary>
        /// A test for ListToVector
        /// </summary>
        [TestMethod]
        public void ListToVectorTest()
        {
            var actual = Vector.FromList(List.MakeList((Number)1, (Number)2));
            var expected = new Vector(2);
            expected[0] = (Number)1;
            expected[1] = (Number)2;
            Assert.AreEqual(2, actual.Length);
            Assert.AreEqual(2, expected.Length);
            Assert.AreEqual(expected[0].ToString(), actual[0].ToString());
            Assert.AreEqual(expected[1].ToString(), actual[1].ToString());
            actual = Vector.FromList((Number)1);
            Assert.AreEqual(0, actual.Length);
        }

        /// <summary>
        /// A test for Num
        /// </summary>
        [TestMethod]
        public void NumTest()
        {
            Assert.AreEqual(0.0, ((Number)0.0).N);
            Assert.AreEqual(0.0, ((Number)0).N);
            Assert.AreEqual(1.0, ((Number)1.0).N);
            Assert.AreEqual(1.0, ((Number)1).N);
        }

        /// <summary>
        /// A test for ReverseList
        /// </summary>
        [TestMethod]
        public void ReverseTest()
        {
            var test = List.MakeList((Number)1, (Number)2);
            var actual = List.ReverseList(test);
            Assert.AreEqual(2.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(1.0, ((Number)List.First(List.Rest(actual))).N);
        }

        /// <summary>
        /// A test for ReverseListInPlace
        /// </summary>
        [TestMethod]
        public void ReverseTestInPlace()
        {
            var test = List.MakeList((Number)1, (Number)2);
            var actual = Pair.ReverseListInPlace(test);
            Assert.AreEqual(2.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(1.0, ((Number)List.First(List.Rest(actual))).N);
        }

        /// <summary>
        /// A test for Str
        /// </summary>
        [TestMethod]
        public void StrTest()
        {
            var actual = (SchemeString)"abc";
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
            Assert.AreEqual("()", EmptyList.Instance.ToString(true));
            Assert.AreEqual("1", ((Number)1.0).ToString(true));
            Assert.AreEqual("1.5", ((Number)1.5).ToString(true));
            Assert.AreEqual("1", ((Number)1).ToString(true));
            Assert.AreEqual("#\\a", ((Character)'a').ToString(true));
            Assert.AreEqual("(1 . 2)", (new Pair((Number)1, (Number)2)).ToString(true));
            Assert.AreEqual("(1 2)", (List.MakeList((Number)1, (Number)2)).ToString(true));
            Assert.AreEqual("abc", ((Symbol)"abc").ToString(true));
            Assert.AreEqual(@"""abc""", ((SchemeString)"abc").ToString(true));
            SchemeString empty = new SchemeString(0);
            Assert.AreEqual(@"""""", empty.ToString(true));
            var test = new Vector(2);
            test[0] = (Number)1;
            test[1] = (Number)2;
            Assert.AreEqual("#(1 2)", test.ToString(true));
            Assert.AreEqual("#t", ((SchemeBoolean)true).ToString(true));
            Assert.AreEqual("#f", ((SchemeBoolean)false).ToString(true));
        }

        /// <summary>
        /// A test for PrintString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithQuote()
        {
            Assert.AreEqual("()", EmptyList.Instance.ToString(false));
            Assert.AreEqual("1", ((Number)1.0).ToString(false));
            Assert.AreEqual("1.5", ((Number)1.5).ToString(false));
            Assert.AreEqual("1", ((Number)1).ToString());
            Assert.AreEqual("a", ((Character)'a').ToString(false));
            Assert.AreEqual("(1 . 2)", (new Pair((Number)1, (Number)2)).ToString(false));
            Assert.AreEqual("(1 2)", (List.MakeList((Number)1, (Number)2)).ToString(false));
            Assert.AreEqual("abc", ((Symbol)"abc").ToString(false));
            Assert.AreEqual(@"""abc""", ((SchemeString)"abc").ToString(true));
            Assert.AreEqual("abc", ((SchemeString)"abc").ToString(false));
            Assert.AreEqual(@"""", ((SchemeString)@"""").ToString(false));
            var test = new Vector(2);
            test[0] = (Number)1;
            test[1] = (Number)2;
            Assert.AreEqual("#(1 2)", test.ToString(false));
            Assert.AreEqual("#t", ((SchemeBoolean)true).ToString(false));
            Assert.AreEqual("#f", ((SchemeBoolean)false).ToString(false));
        }

        /// <summary>
        /// A test for ToString with quote flag false.
        /// </summary>
        [TestMethod]
        public void AsStringTestWithBuf()
        {
            Assert.AreEqual(string.Empty, Undefined.Instance.ToString(false));
            Assert.AreEqual("1", ((Number)1.0).ToString(false));
            Assert.AreEqual("1.5", ((Number)1.5).ToString(false));
            Assert.AreEqual("a", ((Character)'a').ToString(false));
            Assert.AreEqual("abc", ((Symbol)"abc").ToString(false));
            Assert.AreEqual("abc", ((SchemeString)"abc").ToString(false));
        }

        /// <summary>
        /// A test for Sym.
        /// Sym objects are just strings.
        /// </summary>
        [TestMethod]
        public void SymTest()
        {
            Assert.AreEqual("abc", ((Symbol)"abc").ToString());
            SchemeObject x = (Number)1;
        }

        private void Dummy(Symbol d) {}

        /// <summary>
        /// A test for Vec
        /// </summary>
        [TestMethod]
        public void VecTest()
        {
            var test = new Vector(2);
            test[0] = (Number)1;
            test[1] = (Number)2;
            Assert.AreEqual(2, test.Length);
            Assert.AreEqual(1, ((Number)test[0]).N);
            Assert.AreEqual(2, ((Number)test[1]).N);
        }

        /// <summary>
        /// A test for VectorToList
        /// </summary>
        [TestMethod]
        public void VectorToListTest()
        {
            var test = new Vector(3);
            test[0] = (Number)1;
            test[1] = (Number)2;
            test[2] = (Number)3;
            var actual = Vector.ToList(test);
            Assert.AreEqual(3, List.ListLength(actual));
            Assert.AreEqual(1.0, ((Number)List.First(actual)).N);
            Assert.AreEqual(2.0, ((Number)List.Second(actual)).N);
            Assert.AreEqual(3.0, ((Number)List.Third(actual)).N);
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
                var outp = new OutputPort(writer, (Interpreter)this.interpreter);
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
            Assert.AreEqual("boolean", ((SchemeBoolean)true).SchemeTypeName());
            Assert.AreEqual("symbol", ((Symbol)"sym").SchemeTypeName());
            Assert.AreEqual("character", ((Character)'c').SchemeTypeName());
            Assert.AreEqual("vector", new Vector((Number)3, (Number)0).SchemeTypeName());
            Assert.AreEqual("pair", new Pair(Undefined.Instance, Undefined.Instance).SchemeTypeName());
            Assert.AreEqual("number", ((Number)1.0d).SchemeTypeName());
            Assert.AreEqual("string", ((SchemeString)"abc").SchemeTypeName());
            Assert.AreEqual("primitive", (new Primitive("primitive", (args, env, caller) => null, new[] {string.Empty}, new ArgsInfo(0, 0, false, new ArgType[0]))).SchemeTypeName());
            Assert.AreEqual("input-port", new InputPort(new StringReader(string.Empty), (Interpreter)this.interpreter).SchemeTypeName());
            Assert.AreEqual("output-port", new OutputPort(new StringWriter(), (Interpreter)this.interpreter).SchemeTypeName());
            Assert.AreEqual("empty-list", EmptyList.Instance.SchemeTypeName());
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
