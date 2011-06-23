﻿// <copyright file="InputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;

    /// <summary>
    /// Represents an input port, a mechanism for reading input.
    /// </summary>
    public sealed class InputPort
    {
        /// <summary>
        /// Marks the end of the input file.
        /// </summary>
        public const string Eof = "#!EOF";

        /// <summary>
        /// The character stream to read from.
        /// </summary>
        private readonly CharacterStream inStream;

        /// <summary>
        /// The token stream to read from.
        /// </summary>
        private readonly TokenStream tokStream;

        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// </summary>
        /// <param name="inp">A text reader</param>
        public InputPort(TextReader inp)
        {
            this.inStream = new CharacterStream(inp);
            this.tokStream = new TokenStream();
        }

        /// <summary>
        /// Initializes a new instance of the InputPort class.
        /// </summary>
        /// <param name="inp">An input stream.</param>
        public InputPort(Stream inp)
            : this(new StreamReader(inp))
        {
        }

        /// <summary>
        /// Define the input primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            env
                .DefinePrimitive("eof-object?", (parent, args) => SchemeBoolean.Truth(IsEOF(List.First(args))), 1)
                .DefinePrimitive("call-with-input-file", (parent, args) => EvaluateCallWithInputFile.Call(parent, args), 2)
                .DefinePrimitive("close-input-port", (parent, args) => InPort(List.First(args), parent.Env.Interp).Close(), 1)
                .DefinePrimitive("current-input-port", (parent, args) => parent.Env.Interp.Input, 0)
                .DefinePrimitive("eof-object?", (parent, args) => SchemeBoolean.Truth(IsEOF(List.First(args))), 1)
                .DefinePrimitive("input-port?", (parent, args) => SchemeBoolean.Truth(List.First(args) is InputPort), 1)
                .DefinePrimitive("load", (parent, args) => parent.Env.Interp.LoadFile(List.First(args)), 1)
                .DefinePrimitive("open-input-file", (parent, args) => EvaluateCallWithInputFile.OpenInputFile(List.First(args)), 1)
                .DefinePrimitive("peek-char", (parent, args) => InPort(List.First(args), parent.Env.Interp).PeekChar(), 0, 1)
                .DefinePrimitive("read", (parent, args) => InPort(List.First(args), parent.Env.Interp).Read(), 0, 1)
                .DefinePrimitive("read-char", (parent, args) => InPort(List.First(args), parent.Env.Interp).ReadChar(), 0, 1);
        }

        /// <summary>
        /// Tests the object against EOF.
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if the object is EOF.</returns>
        public static bool IsEOF(object x)
        {
            return x as string == Eof;
        }

        /// <summary>
        /// Convert an object (containing an input port) into an InputPort.
        /// If the given object is null, return the interpreter's input port.
        /// </summary>
        /// <param name="x">The object containing the input port.</param>
        /// <param name="interp">The interpreter with the default input port.</param>
        /// <returns>An input port.</returns>
        public static InputPort InPort(object x, Interpreter interp)
        {
            if (x == null)
            {
                return interp.Input;
            }

            if (x is InputPort)
            {
                return (InputPort)x;
            }

            return InPort(ErrorHandlers.Error("Expected an input port, got: " + x), interp);
        }

        /// <summary>
        /// Close the input port.
        /// </summary>
        /// <returns>True if the port was closed.</returns>
        public object Close()
        {
            return this.inStream.Close();
        }

        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        public object Read()
        {
            try
            {
                object token = this.NextToken();

                switch (token as string)
                {
                    case "(": 
                        return this.ReadTail(false);
                    case ")":
                        ErrorHandlers.Warn("Extra ) ignored.");
                        return this.Read();
                    case ".":
                        ErrorHandlers.Warn("Extra . ignored.");
                        return this.Read();
                    case "'": 
                        return List.MakeList("quote", this.Read());
                    case "`":
                        return List.MakeList("quasiquote", this.Read());
                    case ",": 
                        return List.MakeList("unquote", this.Read());
                    case ",@": 
                        return List.MakeList("unquote-splicing", this.Read());
                    default:
                        return token;
                }
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception:" + ex);
                return Eof;
            }
        }

        /// <summary>
        /// Take a peek at the next character, without consuming it.
        /// </summary>
        /// <returns>The next character (as a cell).</returns>
        private object PeekChar()
        {
            int p = this.inStream.PeekCh();
            if (p == -1)
            {
                return Eof;
            }

            return SchemeString.Chr((char)p);
        }

        /// <summary>
        /// Read a character from the input port.
        /// Gets a pushed character, if present.
        /// </summary>
        /// <returns>The character read, or EOF.</returns>
        private object ReadChar()
        {
            try
            {
                int ch = this.inStream.GetPushedChar();
                if (ch == -2)
                {
                    ch = this.inStream.Read();
                }

                if (ch == -1)
                {
                    return Eof;
                }

                return SchemeString.Chr((char)ch);
            }
            catch (IOException ex)
            {
                ErrorHandlers.Warn("On input, exception: " + ex);
                return Eof;
            }
        }

        /// <summary>
        /// Gets the next token from the input port.
        /// Gets a pushed token if there is one, otherwise reads from the input.
        /// </summary>
        /// <returns>the next token.</returns>
        private object NextToken()
        {
            // See if we should re-use a pushed token or character
            object token = this.tokStream.GetPushedToken();
            if (token != null)
            {
                return token;
            }

            int ch = this.inStream.ReadOrPop();

            // Skip whitespace
            while (char.IsWhiteSpace((char)ch))
            {
                ch = this.inStream.Read();
            }

            // See what kind of non-whitespace character we got
            switch (ch)
            {
                case -1:
                    return Eof;

                case '(':
                    return "(";

                case ')':
                    return ")";

                case '\'':
                    return "'";

                case '`':
                    return "`";

                case ',':
                    ch = this.inStream.Read();
                    if (ch == '@')
                    {
                        return ",@";
                    }

                    this.inStream.PushChar(ch);
                    return ",";

                case ';':
                    while (ch != -1 && ch != '\n' && ch != '\r')
                    {
                        ch = this.inStream.Read();
                    }

                    return this.NextToken();

                case '"':
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        while ((ch = this.inStream.Read()) != '"' & ch != -1)
                        {
                            buff.Append((char)((ch == '\\') ? this.inStream.Read() : ch));
                        }

                        if (ch == -1)
                        {
                            ErrorHandlers.Warn("EOF inside of a string.");
                        }

                        return new SchemeString(buff);
                    }

                case '#':
                    switch (ch = this.inStream.Read())
                    {
                        case 't':
                        case 'T':
                            return SchemeBoolean.True;

                        case 'f':
                        case 'F':
                            return SchemeBoolean.False;

                        case '(':
                            this.inStream.PushChar('(');
                            return new Vector(this.Read());

                        case '\\':
                            ch = this.inStream.Read();
                            if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N')
                            {
                                this.inStream.PushChar(ch);
                                token = this.NextToken();
                                if (token is string && (token as string).Length == 1)
                                {
                                    return SchemeString.Chr((char)ch);
                                }

                                switch (token as string)
                                {
                                    case "space":
                                        return SchemeString.Chr(' ');
                                    case "newline":
                                        return SchemeString.Chr('\n');
                                    default:
                                        // this isn't really right
                                        // #\<char> is required to have delimiter after char
                                        ErrorHandlers.Warn("#\\<char> must be followed by delimiter");
                                        this.tokStream.PushToken(token);
                                        return SchemeString.Chr((char)ch);
                                }
                            }

                            return SchemeString.Chr((char)ch);

                        case 'e':
                        case 'i':
                        case 'd':
                            return this.NextToken();

                        case 'b':
                        case 'o':
                        case 'x':
                            ErrorHandlers.Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken();

                        default:
                            ErrorHandlers.Warn("#" + ((char)ch) + " not implemented, ignored.");
                            return this.NextToken();
                    }

                default:
                    {
                        StringBuilder buff = new StringBuilder { Length = 0 };
                        int c = ch;
                        do
                        {
                            buff.Append((char)ch);
                            ch = this.inStream.Read();
                        } 
                        while (!char.IsWhiteSpace((char)ch) && ch != -1 && 
                            ch != '(' && ch != ')' && ch != '\'' &&
                                 ch != ';' && ch != '"' && ch != ',' && ch != '`');

                        this.inStream.PushChar(ch);
                        if (c == '.' || c == '+' || c == '-' || (c >= '0' && c <= '9'))
                        {
                            double value;
                            if (double.TryParse(buff.ToString(), out value))
                            {
                                return value;
                            }
                        }

                        return string.Intern(buff.ToString().ToLower());
                    }
            }
        }

        /// <summary>
        /// Read the tail of a list.
        /// The opening left paren has been read, also perhaps some of the list.
        /// </summary>
        /// <param name="dotOk">True if a dot is OK at this point.</param>
        /// <returns>A list of the tokens read.</returns>
        private object ReadTail(bool dotOk)
        {
            object token = this.NextToken();
            if (token as string == Eof)
            {
                return ErrorHandlers.Error("EOF during read.");
            }

            if (token as string == ")")
            {
                return null;  // there was no more
            }

            if (token as string == ".")
            {
                if (! dotOk)
                {
                    ErrorHandlers.Warn("Dot not allowed here, ignored.");
                    return this.ReadTail(false);
                }

                object result = this.Read();
                token = this.NextToken();
                if (token as string != ")")
                {
                    ErrorHandlers.Warn("Expecting ')' got " + token + " after dot");
                }

                return result;
            }

            this.tokStream.PushToken(token);
            return List.Cons(this.Read(), this.ReadTail(true));
        }

        /// <summary>
        /// Manages a character stream where it is possible to peek ahead one
        ///   character, or to push back a character that was already read.
        /// </summary>
        private class CharacterStream
        {
            /// <summary>
            /// The input port to read from.
            /// </summary>
            private readonly TextReader inp;

            /// <summary>
            /// True if there is a pushed character.
            /// </summary>
            private bool isPushedChar; // = false;

            /// <summary>
            /// If there is a pushed character, this is it.
            /// </summary>
            private int pushedChar = -1;

            /// <summary>
            /// Initializes a new instance of the InputPort.CharacterStream class.
            /// </summary>
            /// <param name="inp">The input port to use for reading.</param>
            public CharacterStream(TextReader inp)
            {
                this.inp = inp;
            }

            /// <summary>
            /// Read a character from the input, bypassing the pushed char.
            /// </summary>
            /// <returns>The next character in the stream.</returns>
            public int Read()
            {
                if (this.isPushedChar)
                {
                    ErrorHandlers.Error("Read bypassed pushed char.");
                    this.isPushedChar = false;
                }

                return this.inp.Read();
            }

            /// <summary>
            /// Push a character back into the input.
            /// </summary>
            /// <param name="ch">The character to push.</param>
            /// <returns>The character that was pushed.</returns>
            public int PushChar(int ch)
            {
                this.isPushedChar = true;
                this.pushedChar = ch;
                return ch;
            }

            /// <summary>
            /// Get a pushed char, if present, or else read one.
            /// </summary>
            /// <returns>The next character in the stream.</returns>
            public int ReadOrPop()
            {
                return this.isPushedChar ? this.PopChar() : this.inp.Read();
            }

            /// <summary>
            /// Check to see if there is a pushed character.
            /// If so, return it.
            /// </summary>
            /// <returns>The pushed character, or -1 if EOF, or -2 if there is no pushed character.</returns>
            public int GetPushedChar()
            {
                if (this.isPushedChar)
                {
                    this.isPushedChar = false;
                    if (this.pushedChar == -1)
                    {
                        return -1;
                    }

                    return SchemeString.Chr((char)this.pushedChar);
                }

                return -2;
            }

            /// <summary>
            /// Take a peek at the next character, without consuming it.
            /// Either read and save a character, to take a look at an already saved character.
            /// </summary>
            /// <returns>The next character (as a character).</returns>
            public int PeekCh()
            {
                try
                {
                    return this.isPushedChar ? this.pushedChar : this.PushChar(this.inp.Read());
                }
                catch (IOException ex)
                {
                    ErrorHandlers.Warn("On input, exception: " + ex);
                    return -1;
                }
            }

            /// <summary>
            /// Close the input port.
            /// </summary>
            /// <returns>True if the port was closed.</returns>
            public object Close()
            {
                try
                {
                    this.inp.Close();
                    return SchemeBoolean.True;
                }
                catch (IOException ex)
                {
                    return ErrorHandlers.Error("IOException: " + ex);
                }
            }

            /// <summary>
            /// Get the character that had been pushed.
            /// </summary>
            /// <returns>The character that was pushed.</returns>
            private int PopChar()
            {
                this.isPushedChar = false;
                return this.pushedChar;
            }
        }

        /// <summary>
        /// Handles tokens that have been read but pushed back on the input stream.
        /// </summary>
        private class TokenStream
        {
            /// <summary>
            /// True if there is a pushed token.
            /// </summary>
            private bool isPushedToken; // = false;

            /// <summary>
            /// If there is a pushed token, this is it.
            /// </summary>
            private object pushedToken;

            /// <summary>
            /// Push the token back on the input.
            /// </summary>
            /// <param name="token">The token to push.</param>
            public void PushToken(object token)
            {
                this.isPushedToken = true;
                this.pushedToken = token;
            }

            /// <summary>
            /// Get the pushed token, if available.
            /// If not, return null.
            /// </summary>
            /// <returns>The pushed token, if available, otherwise null.</returns>
            public object GetPushedToken()
            {
                if (this.isPushedToken)
                {
                    return this.PopToken();
                }

                return null;
            }

            /// <summary>
            /// Get the token that had been pushed.
            /// </summary>
            /// <returns>The pushed token.</returns>
            private object PopToken()
            {
                this.isPushedToken = false;
                return this.pushedToken;
            }
        }
    }
}