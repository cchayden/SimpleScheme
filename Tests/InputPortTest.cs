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
        /// A test for IsEof
        /// </summary>
        [TestMethod]
        public void IsEofTest()
        {
            Assert.IsTrue(InputPort.IsEof(InputPort.Eof));
        }

        /// <summary>
        /// A test for ReadChar
        /// </summary>
        [TestMethod]
        public void ReadCharTest()
        {
            using (var reader = new StringReader("abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('a', port.Parser.ReadChar(null));
                Assert.AreEqual('b', port.Parser.ReadChar(null));
                Assert.AreEqual('c', port.Parser.ReadChar(null));
                Assert.AreEqual(InputPort.Eof, port.Parser.ReadChar(null));
            }
        }

        /// <summary>
        /// A test for ReadChar with PeekCh
        /// </summary>
        [TestMethod]
        public void ReadCharTestWithPeekCh()
        {
            using (var reader = new StringReader("a"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('a', port.Parser.PeekChar());
                Assert.AreEqual('a', port.Parser.ReadChar(null));
                Assert.AreEqual(InputPort.Eof, port.Parser.ReadChar(null));
            }
        }

        /// <summary>
        /// A test for ReadChar with PeekCh at the Eof
        /// </summary>
        [TestMethod]
        public void ReadCharTestWithPeekEof()
        {
            using (var reader = new StringReader("a"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('a', port.Parser.PeekChar());
                Assert.AreEqual('a', port.Parser.ReadChar(null));
                port.Parser.PeekChar();
                Assert.AreEqual(InputPort.Eof, port.Parser.ReadChar(null));
            }
        }

        /// <summary>
        /// A test for NextToken
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void NextTokenTest()
        {
            this.TestNextToken("   abc", "abc");
            this.TestNextToken("abc   ", "abc");
            this.TestNextToken("abc def", "abc");
            this.TestNextToken(string.Empty, InputPort.Eof);
            this.TestNextToken("abc(", "abc");
            this.TestNextToken("abc)", "abc");
            this.TestNextToken("abc'", "abc");
            this.TestNextToken("abc;", "abc");
            this.TestNextToken("abc,", "abc");
            this.TestNextToken("abc`", "abc");
            this.TestNextToken(@"abc""", "abc");
            this.TestNextToken("0", 0.0);
            this.TestNextToken("1", 1.0);
            this.TestNextToken("1.0", 1.0);
            this.TestNextToken("123456789", 123456789.0);
            this.TestNextToken(".5", 0.5);
            this.TestNextToken("+1", 1.0);
            this.TestNextToken("-1", -1.0);
            this.TestNextToken("+abc", "+abc");
            this.TestNextToken("-abc", "-abc");
            this.TestNextToken(".abc", ".abc");
            this.TestNextToken("1abc", "1abc");
            this.TestNextToken("(abc", "(");
            this.TestNextToken("(abc", "(");
            this.TestNextToken(")abc", ")");
            this.TestNextToken("'abc", "'");
            this.TestNextToken("`abc", "`");
            this.TestNextToken(",abc", ",");
            this.TestNextToken(",@abc", ",@");
            this.TestNextToken(";abc", InputPort.Eof);
            this.TestNextToken(";\nabc", "abc");
            this.TestNextToken(@"""abc def""", "abc def");
            this.TestNextToken(@"""abc", "abc");
            this.TestNextToken(@"""abc\d""", "abcd");
            this.TestNextToken(@"""abc\""d""", @"abc""d");
            this.TestNextToken("#t", true);
            this.TestNextToken("#T", true);
            this.TestNextToken("#f", false);
            this.TestNextToken("#F", false);
            this.TestNextToken("#e10", 10.0);
            this.TestNextToken("#i10", 10.0);
            this.TestNextToken("#d10", 10.0);
            this.TestNextToken("#d 10", 10.0);
            this.TestNextToken("#b10", 2);
            this.TestNextToken("#o10", 8);
            this.TestNextToken("#x10", 16);
            this.TestNextToken("#z10", 10.0);
            this.TestNextToken("#\\ ", " ");
            this.TestNextToken("#\\space", " ");
            this.TestNextToken("#\\Space", " ");
            this.TestNextToken("#\\SPACE", " ");
            this.TestNextToken("#\\newline", "\n");
            this.TestNextToken("#\\Newline", "\n");
            this.TestNextToken("#\\stop", "s");
            this.TestNextToken("#\\nop", "n");
            this.TestNextToken("#\\quit", "q");
            var expected = new Obj[] { "a", "b", "c" };
            this.TestNextToken("#( a b c)", expected);
        }

        /// <summary>
        /// A test for PopToken
        /// </summary>
        [TestMethod]
        [DeploymentItem("SimpleScheme.dll")]
        public void PopTokenTest()
        {
            using (var reader = new StringReader("#\\stop"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('s', port.Parser.NextToken());
                Assert.AreEqual("top", port.Parser.NextToken());
            }

            using (var reader = new StringReader("#\\stop#t"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('s', port.Parser.NextToken());
                Assert.AreEqual("top", port.Parser.NextToken());
                Assert.AreEqual(true, port.Parser.NextToken());
            }

            using (var reader = new StringReader("#\\s top"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('s', port.Parser.NextToken());
                Assert.AreEqual("top", port.Parser.NextToken().AsSymbol().ToString());
            }
        }

        /// <summary>
        /// A test for Read
        /// </summary>
        [TestMethod]
        public void ReadTest()
        {
            using (var reader = new StringReader(string.Empty))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual(InputPort.Eof, port.Read());
            }

            using (var reader = new StringReader("abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual("abc", port.Read().AsSymbol().ToString());
            }

            using (var reader = new StringReader("(1 2 3)"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual(1.0, actual.First());
                Assert.AreEqual(2.0, actual.Second());
                Assert.AreEqual(3.0, actual.Third());
            }

            using (var reader = new StringReader("('a 'b 'c)"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("quote", actual.First().First().AsSymbol().ToString());
                Assert.AreEqual("a", actual.First().Second().AsSymbol().ToString());
                Assert.AreEqual("quote", actual.Second().First().AsSymbol().ToString());
                Assert.AreEqual("b", actual.Second().Second().AsSymbol().ToString());
                Assert.AreEqual("c", actual.Third().Second().AsSymbol().ToString());
            }

            using (var reader = new StringReader(")abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("abc", actual.AsSymbol().ToString());
            }

            using (var reader = new StringReader(". abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("abc", actual.AsSymbol().ToString());
            }

            using (var reader = new StringReader("'abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("quote", actual.First().AsSymbol().ToString());
                Assert.AreEqual("abc", actual.Rest().First().AsSymbol().ToString());
            }

            using (var reader = new StringReader("`abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("quasiquote", actual.First().AsSymbol().ToString());
                Assert.AreEqual("abc", actual.Rest().First().AsSymbol().ToString());
            }

            using (var reader = new StringReader(",abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("unquote", actual.First().AsSymbol().ToString());
                Assert.AreEqual("abc", actual.Rest().First().AsSymbol().ToString());
            }

            using (var reader = new StringReader(",@abc"))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("unquote-splicing", actual.First().AsSymbol().ToString());
                Assert.AreEqual("abc", actual.Rest().First().AsSymbol().ToString());
            }
        }

        /// <summary>
        /// Tests one case of NextToken
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected token</param>
        private void TestNextToken(string input, string expected)
        {
            using (var reader = new StringReader(input))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Parser.NextToken();
                if (actual is string)
                {
                    Assert.AreEqual(expected, actual);
                }
                else if (actual.IsSymbol())
                {
                    Assert.AreEqual(expected, actual.AsSymbol().ToString());
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
        private void TestNextToken(string input, double expected)
        {
            using (var reader = new StringReader(input))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual(expected, port.Parser.NextToken());
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in numeric
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private void TestNextToken(string input, int expected)
        {
            using (var reader = new StringReader(input))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual(expected, port.Parser.NextToken());
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in vector
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private void TestNextToken(string input, Obj[] expected)
        {
            using (var reader = new StringReader(input))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Parser.NextToken() as object[];
                Assert.IsNotNull(actual);
                Assert.AreEqual(expected.Length, actual.Length);
                for (int i = 0; i < expected.Length; i++)
                {
                    var a = actual[i].IsSymbol() ? actual[i].AsSymbol().ToString() : actual[i];
                    Assert.AreEqual(expected[i], a);
                }
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in boolean
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private void TestNextToken(string input, bool expected)
        {
            using (var reader = new StringReader(input))
            {
                InputPort port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual(expected, port.Parser.NextToken());
            }
        }
    }
}
