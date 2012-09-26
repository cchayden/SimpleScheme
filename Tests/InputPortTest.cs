// <copyright file="InputPortTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace Tests
{
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
        /// A test for ReadChar
        /// </summary>
        [TestMethod]
        public void ReadCharTest()
        {
            using (var reader = new StringReader("abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('a', port.Parser.ReadChar(null).AsCharacter().C);
                Assert.AreEqual('b', port.Parser.ReadChar(null).AsCharacter().C);
                Assert.AreEqual('c', port.Parser.ReadChar(null).AsCharacter().C);
                Assert.AreEqual(Eof.Instance, port.Parser.ReadChar(null));
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('a', port.Parser.PeekChar().AsCharacter().C);
                Assert.AreEqual('a', port.Parser.ReadChar(null).AsCharacter().C);
                Assert.AreEqual(Eof.Instance, port.Parser.ReadChar(null));
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('a', port.Parser.PeekChar().AsCharacter().C);
                Assert.AreEqual('a', port.Parser.ReadChar(null).AsCharacter().C);
                port.Parser.PeekChar();
                Assert.AreEqual(Eof.Instance, port.Parser.ReadChar(null));
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
            this.TestNextToken(string.Empty, Token.Eof);
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
            this.TestNextToken(";abc", Token.Eof.ToString());
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
            var expected = new[] { (Symbol)"a", (Symbol)"b", (Symbol)"c" };
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('s', port.Parser.NextToken().AsCharacter().C);
                Assert.AreEqual("top", port.Parser.NextToken().ToString());
            }

            using (var reader = new StringReader("#\\stop#t"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('s', port.Parser.NextToken().AsCharacter().C);
                Assert.AreEqual("top", port.Parser.NextToken().ToString());
                Assert.AreEqual(true, port.Parser.NextToken().AsSchemeBoolean().Value);
            }

            using (var reader = new StringReader("#\\s top"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual('s', port.Parser.NextToken().AsCharacter().C);
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual(InputPort.Eof, port.Read());
            }

            using (var reader = new StringReader("abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual("abc", port.Read().AsSymbol().ToString());
            }

            using (var reader = new StringReader("(1 2 3)"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual(1.0, List.First(actual).AsNumber().N);
                Assert.AreEqual(2.0, List.Second(actual).AsNumber().N);
                Assert.AreEqual(3.0, List.Third(actual).AsNumber().N);
            }

            using (var reader = new StringReader("('a 'b 'c)"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("quote", List.First(List.First(actual)).AsSymbol().ToString());
                Assert.AreEqual("a", List.Second(List.First(actual)).AsSymbol().ToString());
                Assert.AreEqual("quote", List.First(List.Second(actual)).AsSymbol().ToString());
                Assert.AreEqual("b", List.Second(List.Second(actual)).AsSymbol().ToString());
                Assert.AreEqual("c", List.Second(List.Third(actual)).AsSymbol().ToString());
            }

            using (var reader = new StringReader(")abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("abc", actual.AsSymbol().ToString());
            }

            using (var reader = new StringReader(". abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("abc", actual.AsSymbol().ToString());
            }

            using (var reader = new StringReader("'abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("quote", List.First(actual).AsSymbol().ToString());
                Assert.AreEqual("abc", List.First(List.Rest(actual)).AsSymbol().ToString());
            }

            using (var reader = new StringReader("`abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("quasiquote", List.First(actual).AsSymbol().ToString());
                Assert.AreEqual("abc", List.First(List.Rest(actual)).AsSymbol().ToString());
            }

            using (var reader = new StringReader(",abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("unquote", List.First(actual).AsSymbol().ToString());
                Assert.AreEqual("abc", List.First(List.Rest(actual)).AsSymbol().ToString());
            }

            using (var reader = new StringReader(",@abc"))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Read();
                Assert.AreEqual("unquote-splicing", List.First(actual).AsSymbol().ToString());
                Assert.AreEqual("abc", List.First(List.Rest(actual)).AsSymbol().ToString());
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Parser.NextToken();
                if (actual is Symbol)
                {
                    Assert.AreEqual(expected, actual.AsSymbol().ToString());
                }
                else if (actual is Character)
                {
                    Assert.AreEqual(1, expected.Length);
                    Assert.AreEqual(expected[0], actual.AsCharacter().C);
                }
                else if (actual is Token)
                {
                    string tok = actual.ToString();
                    Assert.AreEqual(expected, tok);
                }
                else if (actual is SchemeString)
                {
                    for (int i = 0; i < expected.Length; i++)
                    {
                        Assert.AreEqual(expected[i], actual.AsSchemeString().Str[i]);
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual(expected, port.Parser.NextToken().AsNumber().N);
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                Assert.AreEqual(expected, port.Parser.NextToken().AsInt());
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in vector
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private void TestNextToken(string input, Symbol[] expected)
        {
            using (var reader = new StringReader(input))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                var actual = port.Parser.NextToken() as Vector;
                Assert.IsNotNull(actual);
                Assert.AreEqual(expected.Length, actual.Length);
                for (int i = 0; i < expected.Length; i++)
                {
                    Assert.AreEqual(expected[i].ToString(), actual[i].ToString());
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
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                ISchemeObject res = port.Parser.NextToken();
                Assert.IsInstanceOfType(res, typeof(SchemeBoolean));
                Assert.AreEqual(expected, res.AsSchemeBoolean().Value);
            }
        }

        /// <summary>
        /// Tests one case of NextToken when token in Token
        /// </summary>
        /// <param name="input">The input string</param>
        /// <param name="expected">Expected value</param>
        private void TestNextToken(string input, Token expected)
        {
            using (var reader = new StringReader(input))
            {
                var port = InputPort.New(reader, (Interpreter)this.interpreter);
                ISchemeObject res = port.Parser.NextToken();
                Assert.IsInstanceOfType(res, typeof(Token));
                Assert.AreEqual(expected.ToString(), res.ToString());
            }
        }
    }
}
