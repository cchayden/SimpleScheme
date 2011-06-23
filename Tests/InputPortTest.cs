﻿// <copyright file="InputPortTest.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace Tests
{
    using System;
    using System.IO;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using SimpleScheme;

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

        #region Additional test attributes
        // 
        //You can use the following additional attributes as you write your tests:
        //
        //Use ClassInitialize to run code before running the first test in the class
        //[ClassInitialize()]
        //public static void MyClassInitialize(TestContext testContext)
        //{
        //}
        //
        //Use ClassCleanup to run code after all tests in a class have run
        //[ClassCleanup()]
        //public static void MyClassCleanup()
        //{
        //}
        //
        //Use TestInitialize to run code before running each test
        //[TestInitialize()]
        //public void MyTestInitialize()
        //{
        //}
        //
        //Use TestCleanup to run code after each test has run
        //[TestCleanup()]
        //public void MyTestCleanup()
        //{
        //}
        //
        #endregion

        /// <summary>
        /// A test for IsEOF
        /// </summary>
        [TestMethod]
        public void IsEofTest()
        {
            Assert.IsTrue(InputPort.IsEOF(InputPort.Eof));
        }

        /// <summary>
        /// A test for ReadChar
        /// </summary>
        [TestMethod]
        public void ReadCharTest()
        {
            using (StringReader reader = new StringReader("abc"))
            {
                InputPort port = new InputPort(reader);
                Assert.AreEqual('a', port.ReadChar());
                Assert.AreEqual('b', port.ReadChar());
                Assert.AreEqual('c', port.ReadChar());
                Assert.AreEqual(InputPort.Eof, port.ReadChar());
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
                InputPort port = new InputPort(reader);
                Assert.AreEqual('a', port.PeekCh());
                Assert.AreEqual('a', port.ReadChar());
                Assert.AreEqual(InputPort.Eof, port.ReadChar());
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
                InputPort port = new InputPort(reader);
                Assert.AreEqual('a', port.PeekCh());
                Assert.AreEqual('a', port.ReadChar());
                port.PeekCh();
                Assert.AreEqual(InputPort.Eof, port.ReadChar());
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
            TestNextToken(String.Empty, InputPort.Eof);
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
            TestNextToken(";abc", InputPort.Eof);
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
            var expected = new object[] { "a", "b", "c" };
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
                Assert.AreEqual('s', accessor.NextToken());
                Assert.AreEqual("stop", accessor.NextToken());
            }

            using (StringReader reader = new StringReader("#\\s top"))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                Assert.AreEqual('s', accessor.NextToken());
                Assert.AreEqual("top", accessor.NextToken());
            }
        }

        /// <summary>
        ///A test for Read
        ///</summary>
        [TestMethod]
        public void ReadTest()
        {
            using (StringReader reader = new StringReader(""))
            {
                InputPort port = new InputPort(reader);
                Assert.AreEqual(InputPort.Eof, port.Read());
            }
            using (StringReader reader = new StringReader("abc"))
            {
                InputPort port = new InputPort(reader);
                Assert.AreEqual("abc", port.Read());
            }
            using (StringReader reader = new StringReader("(1 2 3)"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual(1.0, SchemeUtils.First(actual));
                Assert.AreEqual(2.0, SchemeUtils.Second(actual));
                Assert.AreEqual(3.0, SchemeUtils.Third(actual));
            }
            using (StringReader reader = new StringReader("('a 'b 'c)"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual("quote", SchemeUtils.First(SchemeUtils.First(actual)));
                Assert.AreEqual("a", SchemeUtils.Second(SchemeUtils.First(actual)));
                Assert.AreEqual("quote", SchemeUtils.First(SchemeUtils.Second(actual)));
                Assert.AreEqual("b", SchemeUtils.Second(SchemeUtils.Second(actual)));
                Assert.AreEqual("c", SchemeUtils.Second(SchemeUtils.Third(actual)));
            }
            using (StringReader reader = new StringReader(")abc"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual("abc", actual);
            }
            using (StringReader reader = new StringReader(". abc"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual("abc", actual);
            }
            using (StringReader reader = new StringReader("'abc"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual("quote", SchemeUtils.First(actual));
                Assert.AreEqual("abc", SchemeUtils.First(SchemeUtils.Rest(actual)));
            }
            using (StringReader reader = new StringReader("`abc"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual("quasiquote", SchemeUtils.First(actual));
                Assert.AreEqual("abc", SchemeUtils.First(SchemeUtils.Rest(actual)));
            }
            using (StringReader reader = new StringReader(",abc"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual("unquote", SchemeUtils.First(actual));
                Assert.AreEqual("abc", SchemeUtils.First(SchemeUtils.Rest(actual)));
            }
            using (StringReader reader = new StringReader(",@abc"))
            {
                InputPort port = new InputPort(reader);
                var actual = port.Read();
                Assert.AreEqual("unquote-splicing", SchemeUtils.First(actual));
                Assert.AreEqual("abc", SchemeUtils.First(SchemeUtils.Rest(actual)));
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
                object actual = accessor.NextToken();
                if (actual is string)
                {
                    Assert.AreEqual(expected, actual);
                } 
                else if (actual is char[])
                {
                    char[] chars = actual as char[];
                    Assert.AreEqual(expected.Length, chars.Length);
                    for (int i = 0; i < expected.Length; i++)
                    {
                        Assert.AreEqual(expected[i], chars[i]);
                    }
                }
                else if (actual is char)
                {
                    Assert.AreEqual(1, expected.Length);
                    Assert.AreEqual(expected[0], actual);
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
                Assert.AreEqual(expected, accessor.NextToken());
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in vector
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private static void TestNextToken(string input, object[] expected)
        {
            using (StringReader reader = new StringReader(input))
            {
                InputPort_Accessor accessor = new InputPort_Accessor(reader);
                var actual = accessor.NextToken() as object[];
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
                Assert.AreEqual(expected, accessor.NextToken());
            }
        }
    }
}