// <copyright file="InputPortTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System.IO;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;
    using Obj = System.Object;

    /// <summary>
    /// This is a test class for InputPortTest and is intended
    /// to contain all InputPortTest Unit Tests
    /// </summary>
    [TestClass]
    public class InputPortTest
    {
        /// <summary>
        /// Gets or sets the test context which provides
        /// information about and functionality for the current test run.
        /// </summary>
        public TestContext TestContext { get; set; }

        /// <summary>
        /// A test for IsEof
        /// </summary>
        [TestMethod]
        public void IsEofTest()
        {
            Assert.IsTrue(InputPort_Accessor.IsEof(InputPort_Accessor.Eof));
        }

        /// <summary>
        /// A test for ReadChar
        /// </summary>
        [TestMethod]
        public void ReadCharTest()
        {
            using (StringReader reader = new StringReader("abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                Assert.AreEqual('a', port.Parser.ReadChar());
                Assert.AreEqual('b', port.Parser.ReadChar());
                Assert.AreEqual('c', port.Parser.ReadChar());
                Assert.AreEqual(InputPort_Accessor.Eof, port.Parser.ReadChar());
            }
        }

        /// <summary>
        /// A test for ReadChar with PeekCh
        /// </summary>
        [TestMethod]
        public void ReadCharTestWithPeekCh()
        {
            using (StringReader reader = new StringReader("a"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                Assert.AreEqual('a', port.Parser.PeekChar());
                Assert.AreEqual('a', port.Parser.ReadChar());
                Assert.AreEqual(InputPort_Accessor.Eof, port.Parser.ReadChar());
            }
        }

        /// <summary>
        /// A test for ReadChar with PeekCh at the Eof
        /// </summary>
        [TestMethod]
        public void ReadCharTestWithPeekEof()
        {
            using (StringReader reader = new StringReader("a"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                Assert.AreEqual('a', port.Parser.PeekChar());
                Assert.AreEqual('a', port.Parser.ReadChar());
                port.Parser.PeekChar();
                Assert.AreEqual(InputPort_Accessor.Eof, port.Parser.ReadChar());
            }
        }

        /// <summary>
        /// A test for NextToken
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void NextTokenTest()
        {
            TestNextToken("   abc", "abc");
            TestNextToken("abc   ", "abc");
            TestNextToken("abc def", "abc");
            TestNextToken(string.Empty, InputPort_Accessor.Eof);
            TestNextToken("abc(", "abc");
            TestNextToken("abc)", "abc");
            TestNextToken("abc'", "abc");
            TestNextToken("abc;", "abc");
            TestNextToken("abc,", "abc");
            TestNextToken("abc`", "abc");
            TestNextToken(@"abc""", "abc");
            TestNextToken("0", 0.0);
            TestNextToken("1", 1.0);
            TestNextToken("1.0", 1.0);
            TestNextToken("123456789", 123456789.0);
            TestNextToken(".5", 0.5);
            TestNextToken("+1", 1.0);
            TestNextToken("-1", -1.0);
            TestNextToken("+abc", "+abc");
            TestNextToken("-abc", "-abc");
            TestNextToken(".abc", ".abc");
            TestNextToken("1abc", "1abc");
            TestNextToken("(abc", "(");
            TestNextToken("(abc", "(");
            TestNextToken(")abc", ")");
            TestNextToken("'abc", "'");
            TestNextToken("`abc", "`");
            TestNextToken(",abc", ",");
            TestNextToken(",@abc", ",@");
            TestNextToken(";abc", InputPort_Accessor.Eof);
            TestNextToken(";\nabc", "abc");
            TestNextToken(@"""abc def""", "abc def");
            TestNextToken(@"""abc", "abc");
            TestNextToken(@"""abc\d""", "abcd");
            TestNextToken(@"""abc\""d""", @"abc""d");
            TestNextToken("#t", true);
            TestNextToken("#T", true);
            TestNextToken("#f", false);
            TestNextToken("#F", false);
            TestNextToken("#e10", 10.0);
            TestNextToken("#i10", 10.0);
            TestNextToken("#d10", 10.0);
            TestNextToken("#d 10", 10.0);
            TestNextToken("#b10", 10.0);
            TestNextToken("#o10", 10.0);
            TestNextToken("#x10", 10.0);
            TestNextToken("#z10", 10.0);
            TestNextToken("#\\ ", " ");
            TestNextToken("#\\space", " ");
            TestNextToken("#\\Space", " ");
            TestNextToken("#\\SPACE", " ");
            TestNextToken("#\\newline", "\n");
            TestNextToken("#\\Newline", "\n");
            TestNextToken("#\\stop", "s");
            TestNextToken("#\\nop", "n");
            TestNextToken("#\\quit", "q");
            var expected = new Obj[] { "a", "b", "c" };
            TestNextToken("#( a b c)", expected);
        }

        /// <summary>
        /// A test for PopToken
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void PopTokenTest1()
        {
            using (StringReader reader = new StringReader("#\\stop"))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                Assert.AreEqual('s', accessor.Parser.NextToken());
                Assert.AreEqual("stop", accessor.Parser.NextToken());
            }

            using (StringReader reader = new StringReader("#\\s top"))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                Assert.AreEqual('s', accessor.Parser.NextToken());
                Assert.AreEqual("top", accessor.Parser.NextToken());
            }
        }

        /// <summary>
        /// A test for Read
        /// </summary>
        [TestMethod]
        public void ReadTest()
        {
            using (StringReader reader = new StringReader(string.Empty))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                Assert.AreEqual(InputPort_Accessor.Eof, port.Read());
            }

            using (StringReader reader = new StringReader("abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                Assert.AreEqual("abc", port.Read());
            }

            using (StringReader reader = new StringReader("(1 2 3)"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual(1.0, ListPrimitives.First(actual));
                Assert.AreEqual(2.0, ListPrimitives.Second(actual));
                Assert.AreEqual(3.0, ListPrimitives.Third(actual));
            }

            using (StringReader reader = new StringReader("('a 'b 'c)"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual("quote", ListPrimitives.First(ListPrimitives.First(actual)));
                Assert.AreEqual("a", ListPrimitives.Second(ListPrimitives.First(actual)));
                Assert.AreEqual("quote", ListPrimitives.First(ListPrimitives.Second(actual)));
                Assert.AreEqual("b", ListPrimitives.Second(ListPrimitives.Second(actual)));
                Assert.AreEqual("c", ListPrimitives.Second(ListPrimitives.Third(actual)));
            }

            using (StringReader reader = new StringReader(")abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual("abc", actual);
            }

            using (StringReader reader = new StringReader(". abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual("abc", actual);
            }

            using (StringReader reader = new StringReader("'abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual("quote", ListPrimitives.First(actual));
                Assert.AreEqual("abc", ListPrimitives.First(ListPrimitives.Rest(actual)));
            }

            using (StringReader reader = new StringReader("`abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual("quasiquote", ListPrimitives.First(actual));
                Assert.AreEqual("abc", ListPrimitives.First(ListPrimitives.Rest(actual)));
            }

            using (StringReader reader = new StringReader(",abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual("unquote", ListPrimitives.First(actual));
                Assert.AreEqual("abc", ListPrimitives.First(ListPrimitives .Rest(actual)));
            }

            using (StringReader reader = new StringReader(",@abc"))
            {
                InputPort_Accessor port = new InputPort_Accessor(reader);
                var actual = port.Read();
                Assert.AreEqual("unquote-splicing", ListPrimitives.First(actual));
                Assert.AreEqual("abc", ListPrimitives.First(ListPrimitives.Rest(actual)));
            }
        }

        /// <summary>
        /// Tests one case of NextToken
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected token</param>
        private static void TestNextToken(string input, string expected)
        {
            using (StringReader reader = new StringReader(input))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                var actual = accessor.Parser.NextToken();
                if (actual is string)
                {
                    Assert.AreEqual(expected, actual);
                } 
                else if (actual is char)
                {
                    Assert.AreEqual(1, expected.Length);
                    Assert.AreEqual(expected[0], actual);
                }
                else if (actual is char[])
                {
                    for (int i = 0; i < expected.Length; i++)
                    {
                        Assert.AreEqual(expected[i], ((char[])actual)[i]);
                    }
                }
                else
                {
                    Assert.Fail("NextToken returned unexpected type: " + expected + " " + actual);
                }
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in numeric
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private static void TestNextToken(string input, double expected)
        {
            using (StringReader reader = new StringReader(input))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                Assert.AreEqual(expected, accessor.Parser.NextToken());
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in vector
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private static void TestNextToken(string input, Obj[] expected)
        {
            using (StringReader reader = new StringReader(input))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                var actual = accessor.Parser.NextToken() as object[];
                Assert.IsNotNull(actual);
                Assert.AreEqual(expected.Length, actual.Length);
                for (int i = 0; i < expected.Length; i++)
                {
                    Assert.AreEqual(expected[i], actual[i]);
                }
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in boolean
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private static void TestNextToken(string input, bool expected)
        {
            using (StringReader reader = new StringReader(input))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                Assert.AreEqual(expected, accessor.Parser.NextToken());
            }
        }
    }
}
