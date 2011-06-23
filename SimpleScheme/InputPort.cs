// <copyright file="InputPort.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.IO;
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// Represents an input port, a mechanism for reading input.
    /// </summary>
    public sealed class InputPort : ListPrimitives
    {
        #region Fields
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
        #endregion

        #region Construcors
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
        #endregion

        #region Define Primitives
        /// <summary>
        /// Define the input primitives.
        /// </summary>
        /// <param name="env">The environment to define the primitives into.</param>
        public static void DefinePrimitives(Environment env)
        {
            // TODO not implemented
            //// <r4rs section="6.10.1">(with-input-from-file <string> <thunk>)</r4rs>
            //// <r4rs section="6.10.2">(char-ready?)</r4rs>
            //// <r4rs section="6.10.2">(char-ready? <port>)</r4rs>
            //// <r4rs section="6.10.4">(transcript-on <filename>)</r4rs>
            //// <r4rs section="6.10.4">(transcript-off)</r4rs>

            env
                //// <r4rs section="6.10.2">(eof-object? <obj>)</r4rs>
                .DefinePrimitive("eof-object?", (args, caller) => SchemeBoolean.Truth(IsEof(First(args))), 1)
                ////// <r4rs section="6.10.1">(call-with-input-file <string> <proc>)</r4rs>
                .DefinePrimitive("call-with-input-file", (args, caller) => EvaluateCallWithInputFile.Call(args, caller), 2)
                //// <r4rs section="6.10.1">(close-input-port <port>)</r4rs>
                .DefinePrimitive("close-input-port", (args, caller) => InPort(First(args), caller.Env.Interp.Input).Close(), 1)
                //// <r4rs section="6.10.1">(current-input-port)</r4rs>
                .DefinePrimitive("current-input-port", (args, caller) => caller.Env.Interp.Input, 0)
                //// <r4rs section="6.10.1">(input-port? <obj>)</r4rs>
                .DefinePrimitive("input-port?", (args, caller) => SchemeBoolean.Truth(First(args) is InputPort), 1)
                //// <r4rs section="6.10.4">(load <filename>)</r4rs>
                .DefinePrimitive("load", (args, caller) => caller.Env.Interp.LoadFile(First(args)), 1)
                //// <r4rs section="6.10.1">(open-input-file <filename>)</r4rs>
                .DefinePrimitive("open-input-file", (args, caller) => EvaluateCallWithInputFile.OpenInputFile(First(args)), 1)
                //// <r4rs section="6.10.2">(peek-char)</r4rs>
                //// <r4rs section="6.10.2">(peek-char <port>)</r4rs>
                .DefinePrimitive("peek-char", (args, caller) => InPort(First(args), caller.Env.Interp.Input).PeekChar(), 0, 1)
                //// <r4rs section="6.10.2">(read)</r4rs>
                //// <r4rs section="6.10.2">(read <port>)</r4rs>
                .DefinePrimitive("read", (args, caller) => InPort(First(args), caller.Env.Interp.Input).Read(), 0, 1)
                //// <r4rs section="6.10.2">(read-char)</r4rs>
                //// <r4rs section="6.10.2">(read-char <port>)</r4rs>
                .DefinePrimitive("read-char", (args, caller) => InPort(First(args), caller.Env.Interp.Input).ReadChar(), 0, 1);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests the obj against EOF.
        /// </summary>
        /// <param name="x">The obj to test.</param>
        /// <returns>True if the obj is EOF.</returns>
        public static bool IsEof(Obj x)
        {
            return x as string == Eof;
        }

        /// <summary>
        /// Convert an obj (containing an input port) into an InputPort.
        /// If the given obj is the empty list, return the interpreter's input port.
        /// </summary>
        /// <param name="x">The obj containing the input port.</param>
        /// <param name="inPort">The default input port.</param>
        /// <returns>An input port.</returns>
        public static InputPort InPort(Obj x, InputPort inPort)
        {
            if (x == List.Empty)
            {
                return inPort;
            }

            if (x is InputPort)
            {
                return (InputPort)x;
            }

            return InPort(ErrorHandlers.Error("Expected an input port, got: " + x), null);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Close the input port.
        /// </summary>
        /// <returns>True if the port was closed.</returns>
        public Obj Close()
        {
            return this.inStream.Close();
        }

        /// <summary>
        /// Read a whole expression.
        /// Handles parentheses and the various kinds of quote syntax shortcuts.
        /// Warns about extra right parentheses and dots.
        /// </summary>
        /// <returns>The expression as a list.</returns>
        public Obj Read()
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
                        return MakeList("quote", this.Read());
                    case "`":
                        return MakeList("quasiquote", this.Read());
                    case ",": 
                        return MakeList("unquote", this.Read());
                    case ",@": 
                        return MakeList("unquote-splicing", this.Read());
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
        #endregion

        #region Private Methods
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

            return Character.Chr((char)p);
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

                return Character.Chr((char)ch);
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

                        return SchemeString.MakeString(buff);
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
                            return Vector.MakeVector(this.Read());

                        case '\\':
                            ch = this.inStream.Read();
                            if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N')
                            {
                                this.inStream.PushChar(ch);
                                token = this.NextToken();
                                if (token is string && (token as string).Length == 1)
                                {
                                    return Character.Chr((char)ch);
                                }

                                switch (token as string)
                                {
                                    case "space":
                                        return Character.Chr(' ');
                                    case "newline":
                                        return Character.Chr('\n');
                                    default:
                                        // this isn't really right
                                        // #\<char> is required to have delimiter after char
                                        ErrorHandlers.Warn("#\\<char> must be followed by delimiter");
                                        this.tokStream.PushToken(token);
                                        return Character.Chr((char)ch);
                                }
                            }

                            return Character.Chr((char)ch);

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
        private Obj ReadTail(bool dotOk)
        {
            object token = this.NextToken();
            if (token as string == Eof)
            {
                return ErrorHandlers.Error("EOF during read.");
            }

            if (token as string == ")")
            {
                return List.Empty;  // there was no more
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
            return Cons(this.Read(), this.ReadTail(true));
        }
#endregion

        #region Private Classes
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

                    return Character.Chr((char)this.pushedChar);
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
            public Obj Close()
            {
                try
                {
                    this.inp.Close();
                    return Undefined.Instance;
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
                return this.isPushedToken ? this.PopToken() : null;
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
        #endregion
    }
}