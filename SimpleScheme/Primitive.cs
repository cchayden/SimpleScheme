// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2008 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics;
    using System.IO;
    using System.Reflection;
    using System.Text;

    /// <summary>
    /// Primitive procedures.
    /// This contains implementations for all primitive procedures.
    /// Each primitive knows its operCode, and the min and max number of arguments it expects.
    /// Each instance of Primitive is immutable.  In practice, we have one instance for each operCode.
    /// </summary>
    public sealed class Primitive : Procedure
    {
        /// <summary>
        /// The specific operation that the primitive performs is determined by this enum value.
        /// </summary>
        private readonly OpCode operCode;

        /// <summary>
        /// The minimum number of arguments expected.
        /// </summary>
        private readonly int minArgs;

        /// <summary>
        /// The maximum number of args expected.
        /// </summary>
        private readonly int maxArgs;

        /// <summary>
        /// Initializes a new instance of the Primitive class.
        /// </summary>
        /// <param name="operCode">The operCode, encodes what operation to perform.</param>
        /// <param name="minArgs">The minimum number of arguments.</param>
        /// <param name="maxArgs">The maximum number of arguments.</param>
        public Primitive(OpCode operCode, int minArgs, int maxArgs)
        {
            this.operCode = operCode;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }

        /// <summary>
        /// These are the specific operation codes.
        /// </summary>
        public enum OpCode
        {
            EQ,
            LT,
            GT,
            GE,
            LE,
            ABS,
            EOF_OBJECT,
            EQQ,
            EQUALQ,
            FORCE,
            CAR,
            FLOOR,
            CEILING,
            CONS,
            DIVIDE,
            LENGTH,
            LIST,
            LISTQ,
            APPLY,
            MAX,
            MIN,
            MINUS,
            NEWLINE,
            NOT,
            NULLQ,
            NUMBERQ,
            PAIRQ,
            PLUS,
            PROCEDUREQ,
            READ,
            CDR,
            ROUND,
            SECOND,
            SYMBOLQ,
            TIMES,
            TRUNCATE,
            WRITE,
            APPEND,
            BOOLEANQ,
            SQRT,
            EXPT,
            REVERSE,
            ASSOC,
            ASSQ,
            ASSV,
            MEMBER,
            MEMQ,
            MEMV,
            EQVQ,
            LISTREF,
            LISTTAIL,
            STRINQ,
            MAKESTRING,
            STRING,
            STRINGLENGTH,
            STRINGREF,
            STRINGSET,
            SUBSTRING,
            STRINGAPPEND,
            STRINGTOLIST,
            LISTTOSTRING,
            SYMBOLTOSTRING,
            STRINGTOSYMBOL,
            EXP,
            LOG,
            SIN,
            COS,
            TAN,
            ACOS,
            ASIN,
            ATAN,
            NUMBERTOSTRING,
            STRINGTONUMBER,
            CHARQ,
            CHARALPHABETICQ,
            CHARNUMERICQ,
            CHARWHITESPACEQ,
            CHARUPPERCASEQ,
            CHARLOWERCASEQ,
            CHARTOINTEGER,
            INTEGERTOCHAR,
            CHARUPCASE,
            CHARDOWNCASE,
            STRINGQ,
            VECTORQ,
            MAKEVECTOR,
            VECTOR,
            VECTORLENGTH,
            VECTORREF,
            VECTORSET,
            LISTTOVECTOR,
            MAP,
            FOREACH,
            CALLCC,
            VECTORTOLIST,
            LOAD,
            DISPLAY,
            INPUTPORTQ,
            CURRENTINPUTPORT,
            OPENINPUTFILE,
            CLOSEINPUTPORT,
            OUTPUTPORTQ,
            CURRENTOUTPUTPORT,
            OPENOUTPUTFILE,
            CLOSEOUTPUTPORT,
            READCHAR,
            PEEKCHAR,
            EVAL,
            QUOTIENT,
            REMAINDER,
            MODULO,
            THIRD,
            EOFOBJECTQ,
            GCD,
            LCM,
            CXR,
            ODDQ,
            EVENQ,
            ZEROQ,
            POSITIVEQ,
            NEGATIVEQ,
            CHARCMPEQ,
            CHARCMPLT,
            CHARCMPGT,
            CHARCMPGE,
            CHARCMPLE,
            CHARCICMPEQ,
            CHARCICMPLT,
            CHARCICMPGT,
            CHARCICMPGE,
            CHARCICMPLE,
            STRINGCMPEQ,
            STRINGCMPLT,
            STRINGCMPGT,
            STRINGCMPGE,
            STRINGCMPLE,
            STRINGCICMPEQ,
            STRINGCICMPLT,
            STRINGCICMPGT,
            STRINGCICMPGE,
            STRINGCICMPLE,
            EXACTQ,
            INEXACTQ,
            INTEGERQ,
            CALLWITHINPUTFILE,
            CALLWITHOUTPUTFILE,

            NEW = -1,
            CLASS = -2,
            METHOD = -3,
            EXIT = -4,
            SETCAR = -5,
            SETCDR = -6,
            TIMECALL = -11,
            ERROR = -13,
            LISTSTAR = -14,
        }

        /// <summary>
        /// Sets up the primitives.
        /// </summary>
        /// <param name="env">The environment to install the primitives into.</param>
        /// <returns>The environment they were installed into.</returns>
        public static Environment InstallPrimitives(Environment env)
        {
            const int MaxInt = int.MaxValue;

            env
                .DefPrim("=", OpCode.EQ, 2, MaxInt)
                .DefPrim("*", OpCode.TIMES, 0, MaxInt)
                .DefPrim("+", OpCode.PLUS, 0, MaxInt)
                .DefPrim("-", OpCode.MINUS, 1, MaxInt)
                .DefPrim("/", OpCode.DIVIDE, 1, MaxInt)
                .DefPrim("<", OpCode.LT, 2, MaxInt)
                .DefPrim(">", OpCode.GT, 2, MaxInt)
                .DefPrim("<=", OpCode.LE, 2, MaxInt)
                .DefPrim(">=", OpCode.GE, 2, MaxInt)
                .DefPrim("abs", OpCode.ABS, 1)
                .DefPrim("acos", OpCode.ACOS, 1)
                .DefPrim("append", OpCode.APPEND, 0, MaxInt)
                .DefPrim("apply", OpCode.APPLY, 2, MaxInt)
                .DefPrim("asin", OpCode.ASIN, 1)
                .DefPrim("assoc", OpCode.ASSOC, 2)
                .DefPrim("assq", OpCode.ASSQ, 2)
                .DefPrim("assv", OpCode.ASSV, 2)
                .DefPrim("atan", OpCode.ATAN, 1)
                .DefPrim("boolean?", OpCode.BOOLEANQ, 1)
                .DefPrim("caaaar", OpCode.CXR, 1)
                .DefPrim("caaadr", OpCode.CXR, 1)
                .DefPrim("caaar", OpCode.CXR, 1)
                .DefPrim("caadar", OpCode.CXR, 1)
                .DefPrim("caaddr", OpCode.CXR, 1)
                .DefPrim("caar", OpCode.CXR, 1)
                .DefPrim("cadaar", OpCode.CXR, 1)
                .DefPrim("cadadr", OpCode.CXR, 1)
                .DefPrim("cadar", OpCode.CXR, 1)
                .DefPrim("caddar", OpCode.CXR, 1)
                .DefPrim("cadddr", OpCode.CXR, 1)
                .DefPrim("caddr", OpCode.CXR, 1)
                .DefPrim("cadr", OpCode.CXR, 1)
                .DefPrim("call-with-current-continuation", OpCode.CALLCC, 1)
                .DefPrim("call/cc", OpCode.CALLCC, 1)
                .DefPrim("call-with-input-file", OpCode.CALLWITHINPUTFILE, 2)
                .DefPrim("call-with-output-file", OpCode.CALLWITHOUTPUTFILE, 2)
                .DefPrim("car", OpCode.CAR, 1)
                .DefPrim("first", OpCode.CAR, 1)
                .DefPrim("second", OpCode.SECOND, 1)
                .DefPrim("third", OpCode.THIRD, 1)
                .DefPrim("cdaaar,", OpCode.CXR, 1)
                .DefPrim("cdaadr", OpCode.CXR, 1)
                .DefPrim("cdaar", OpCode.CXR, 1)
                .DefPrim("cdadar", OpCode.CXR, 1)
                .DefPrim("cdaddr", OpCode.CXR, 1)
                .DefPrim("cdadr", OpCode.CXR, 1)
                .DefPrim("cdar", OpCode.CXR, 1)
                .DefPrim("cddaar", OpCode.CXR, 1)
                .DefPrim("cddadr", OpCode.CXR, 1)
                .DefPrim("cddar", OpCode.CXR, 1)
                .DefPrim("cdddar", OpCode.CXR, 1)
                .DefPrim("cddddr", OpCode.CXR, 1)
                .DefPrim("cdddr", OpCode.CXR, 1)
                .DefPrim("cddr", OpCode.CXR, 1)
                .DefPrim("cdr", OpCode.CDR, 1)
                .DefPrim("rest", OpCode.CDR, 1)
                .DefPrim("char->integer", OpCode.CHARTOINTEGER, 1)
                .DefPrim("char-alphabetic?", OpCode.CHARALPHABETICQ, 1)
                .DefPrim("char-ci<=?", OpCode.CHARCICMPLE, 2)
                .DefPrim("char-ci<?", OpCode.CHARCICMPLT, 2)
                .DefPrim("char-ci=?", OpCode.CHARCICMPEQ, 2)
                .DefPrim("char-ci>=?", OpCode.CHARCICMPGE, 2)
                .DefPrim("char-ci>?", OpCode.CHARCICMPGT, 2)
                .DefPrim("char-downcase", OpCode.CHARDOWNCASE, 1)
                .DefPrim("char-lower-case?", OpCode.CHARLOWERCASEQ, 1)
                .DefPrim("char-numeric?", OpCode.CHARNUMERICQ, 1)
                .DefPrim("char-upcase", OpCode.CHARUPCASE, 1)
                .DefPrim("char-upper-case?", OpCode.CHARUPPERCASEQ, 1)
                .DefPrim("char-whitespace?", OpCode.CHARWHITESPACEQ, 1)
                .DefPrim("char<=?", OpCode.CHARCMPLE, 2)
                .DefPrim("char<?", OpCode.CHARCMPLT, 2)
                .DefPrim("char=?", OpCode.CHARCMPEQ, 2)
                .DefPrim("char>=?", OpCode.CHARCMPGE, 2)
                .DefPrim("char>?", OpCode.CHARCMPGT, 2)
                .DefPrim("char?", OpCode.CHARQ, 1)
                .DefPrim("close-input-port", OpCode.CLOSEINPUTPORT, 1)
                .DefPrim("close-output-port", OpCode.CLOSEOUTPUTPORT, 1)
                .DefPrim("complex", OpCode.NUMBERQ, 1)
                .DefPrim("cons", OpCode.CONS, 2)
                .DefPrim("cos", OpCode.COS, 1)
                .DefPrim("current-input-port", OpCode.CURRENTINPUTPORT, 0)
                .DefPrim("current-output-port", OpCode.CURRENTOUTPUTPORT, 0)
                .DefPrim("display", OpCode.DISPLAY, 1, 2)
                .DefPrim("eof-object?", OpCode.EOFOBJECTQ, 1)
                .DefPrim("eq?", OpCode.EQQ, 2)
                .DefPrim("equal?", OpCode.EQUALQ, 2)
                .DefPrim("eqv?", OpCode.EQVQ, 2)
                .DefPrim("eval", OpCode.EVAL, 1, 2)
                .DefPrim("even?", OpCode.EVENQ, 1)
                .DefPrim("exact?", OpCode.INTEGERQ, 1)
                .DefPrim("exp", OpCode.EXP, 1)
                .DefPrim("expt", OpCode.EXPT, 2)
                .DefPrim("force", OpCode.FORCE, 1)
                .DefPrim("for-each", OpCode.FOREACH, 1, MaxInt)
                .DefPrim("gcd", OpCode.GCD, 0, MaxInt)
                .DefPrim("inexact?", OpCode.INEXACTQ, 1)
                .DefPrim("input-port?", OpCode.INPUTPORTQ, 1)
                .DefPrim("integer->char", OpCode.INTEGERTOCHAR, 1)
                .DefPrim("integer?", OpCode.INTEGERQ, 1)
                .DefPrim("lcm", OpCode.LCM, 0, MaxInt)
                .DefPrim("length", OpCode.LENGTH, 1)
                .DefPrim("list", OpCode.LIST, 0, MaxInt)
                .DefPrim("list->string", OpCode.LISTTOSTRING, 1)
                .DefPrim("list->vector", OpCode.LISTTOVECTOR, 1)
                .DefPrim("list-ref", OpCode.LISTREF, 2)
                .DefPrim("list-tail", OpCode.LISTTAIL, 2)
                .DefPrim("list?", OpCode.LISTQ, 1)
                .DefPrim("load", OpCode.LOAD, 1)
                .DefPrim("log", OpCode.LOG, 1)
                .DefPrim("make-string", OpCode.MAKESTRING, 1, 2)
                .DefPrim("make-vector", OpCode.MAKEVECTOR, 1, 2)
                .DefPrim("map", OpCode.MAP, 1, MaxInt)
                .DefPrim("max", OpCode.MAX, 1, MaxInt)
                .DefPrim("member", OpCode.MEMBER, 2)
                .DefPrim("memq", OpCode.MEMQ, 2)
                .DefPrim("memv", OpCode.MEMV, 2)
                .DefPrim("min", OpCode.MIN, 1, MaxInt)
                .DefPrim("modulo", OpCode.MODULO, 2)
                .DefPrim("negative?", OpCode.NEGATIVEQ, 1)
                .DefPrim("newline", OpCode.NEWLINE, 0, 1)
                .DefPrim("not", OpCode.NOT, 1)
                .DefPrim("null?", OpCode.NULLQ, 1)
                .DefPrim("number->string", OpCode.NUMBERTOSTRING, 1, 2)
                .DefPrim("number?", OpCode.NUMBERQ, 1)
                .DefPrim("odd?", OpCode.ODDQ, 1)
                .DefPrim("open-input-file", OpCode.OPENINPUTFILE, 1)
                .DefPrim("open-output-file", OpCode.OPENOUTPUTFILE, 1)
                .DefPrim("output-port?", OpCode.OUTPUTPORTQ, 1)
                .DefPrim("pair?", OpCode.PAIRQ, 1)
                .DefPrim("peek-char", OpCode.PEEKCHAR, 0, 1)
                .DefPrim("positive?", OpCode.POSITIVEQ, 1)
                .DefPrim("procedure?", OpCode.PROCEDUREQ, 1)
                .DefPrim("quotient", OpCode.QUOTIENT, 2)
                .DefPrim("rational?", OpCode.INTEGERQ, 1)
                .DefPrim("read", OpCode.READ, 0, 1)
                .DefPrim("read-char", OpCode.READCHAR, 0, 1)
                .DefPrim("real?", OpCode.INTEGERQ, 1)
                .DefPrim("remainder", OpCode.REMAINDER, 2)
                .DefPrim("reverse", OpCode.REVERSE, 1)
                .DefPrim("round", OpCode.ROUND, 1)
                .DefPrim("set-car!", OpCode.SETCAR, 2)
                .DefPrim("set-first!", OpCode.SETCAR, 2)
                .DefPrim("set-cdr!", OpCode.SETCDR, 2)
                .DefPrim("set-rest!", OpCode.SETCDR, 2)
                .DefPrim("sin", OpCode.SIN, 1)
                .DefPrim("sqrt", OpCode.SQRT, 1)
                .DefPrim("string", OpCode.STRING, 0, MaxInt)
                .DefPrim("string->list", OpCode.STRINGTOLIST, 1)
                .DefPrim("string->number", OpCode.STRINGTONUMBER, 1, 2)
                .DefPrim("string->symbol", OpCode.STRINGTOSYMBOL, 1)
                .DefPrim("string-append", OpCode.STRINGAPPEND, 0, MaxInt)
                .DefPrim("string-ci<=?", OpCode.STRINGCICMPLE, 2)
                .DefPrim("string-ci<?", OpCode.STRINGCICMPLT, 2)
                .DefPrim("string-ci=?", OpCode.STRINGCICMPEQ, 2)
                .DefPrim("string-ci>=?", OpCode.STRINGCICMPGE, 2)
                .DefPrim("string-ci>?", OpCode.STRINGCICMPGT, 2)
                .DefPrim("string-length", OpCode.STRINGLENGTH, 1)
                .DefPrim("string-ref", OpCode.STRINGREF, 2)
                .DefPrim("string-set!", OpCode.STRINGSET, 3)
                .DefPrim("string<=?", OpCode.STRINGCMPLE, 2)
                .DefPrim("string<?", OpCode.STRINGCMPLT, 2)
                .DefPrim("string=?", OpCode.STRINGCMPEQ, 2)
                .DefPrim("string>=?", OpCode.STRINGCMPGE, 2)
                .DefPrim("string>?", OpCode.STRINGCMPGT, 2)
                .DefPrim("string?", OpCode.STRINGQ, 1)
                .DefPrim("substring", OpCode.SUBSTRING, 3)
                .DefPrim("symbol->string", OpCode.SYMBOLTOSTRING, 1)
                .DefPrim("symbol?", OpCode.SYMBOLQ, 1)
                .DefPrim("tan", OpCode.TAN, 1)
                .DefPrim("vector", OpCode.VECTOR, 0, MaxInt)
                .DefPrim("vector->list", OpCode.VECTORTOLIST, 1)
                .DefPrim("vector-length", OpCode.VECTORLENGTH, 1)
                .DefPrim("vector-ref", OpCode.VECTORREF, 2)
                .DefPrim("vector-set!", OpCode.VECTORSET, 3)
                .DefPrim("vector?", OpCode.VECTORQ, 1)
                .DefPrim("write", OpCode.WRITE, 1, 2)
                .DefPrim("write-char", OpCode.DISPLAY, 1, 2)
                .DefPrim("zero?", OpCode.ZEROQ, 1)

                // EXTENSIONS
                .DefPrim("new", OpCode.NEW, 1)
                .DefPrim("class", OpCode.CLASS, 1)
                .DefPrim("method", OpCode.METHOD, 2, MaxInt)
                .DefPrim("exit", OpCode.EXIT, 0, 1)
                .DefPrim("error", OpCode.ERROR, 0, MaxInt)
                .DefPrim("time-call", OpCode.TIMECALL, 1, 2)
                .DefPrim("_list*", OpCode.LISTSTAR, 0, MaxInt);

            return env;
        }

        /// <summary>
        /// Apply the primitive to the arguments, giving a result.
        /// This may return a result or an Evaluator, which can be used to get a result.
        /// If parent is null, then it will not return an Evaluator, so when that is not
        ///   acceptable, pass null for parent.
        /// </summary>
        /// <param name="interp">The interpreter context.</param>
        /// <param name="parent">The calling Evaluator.</param>
        /// <param name="args">The arguments to the primitive.</param>
        /// <returns>The result of the application.</returns>
        public override object Apply(Scheme interp, Evaluator parent, object args)
        {
            int numArgs = Length(args);
            if (numArgs < this.minArgs)
            {
                return Error("too few args, " + numArgs + ", for " + 
                    this.Name + ": " + args);
            }

            if (numArgs > this.maxArgs)
            {
                return Error("too many args, " + numArgs + ", for " + 
                    this.Name + ": " + args);
            }

            object x = First(args);
            object y = Second(args);

            switch (this.operCode)
            {
                    // 6.1 BOOLEANS
                case OpCode.NOT:
                    return Truth(x is bool && (bool)x == false);

                case OpCode.BOOLEANQ:
                    return Truth(x is bool);

                case OpCode.EQVQ:
                    return Truth(Eqv(x, y));

                case OpCode.EQQ:
                    // return Truth(x == y);
                    return Truth(Eqv(x, y));

                case OpCode.EQUALQ:
                    // return Truth(x.Equals(y));
                    return Truth(Equal(x, y));

                    // 6.2 EQUIVALENCE PREDICATES
                case OpCode.PAIRQ:
                    return Truth(x is Pair);

                case OpCode.LISTQ:
                    return Truth(IsList(x));

                case OpCode.CXR:
                    for (int i = this.Name.Length - 2; i >= 1; i--)
                    {
                        x = this.Name[i] == 'a' ? First(x) : Rest(x);
                    }

                    return x;

                case OpCode.CONS:
                    return Cons(x, y);

                case OpCode.CAR:
                    return First(x);

                case OpCode.CDR:
                    return Rest(x);

                case OpCode.SETCAR:
                    return SetFirst(x, y);

                case OpCode.SETCDR:
                    return SetRest(x, y);

                case OpCode.SECOND:
                    return Second(x);

                case OpCode.THIRD:
                    return Third(x);

                case OpCode.NULLQ:
                    return Truth(x == null);

                case OpCode.LIST:
                    return args;

                case OpCode.LENGTH:
                    return Num(Length(x));

                case OpCode.APPEND:
                    return args == null ? null : Append(args);

                case OpCode.REVERSE:
                    return Reverse(x);

                case OpCode.LISTTAIL:
                    for (int k = (int)Num(y); k > 0; k--)
                    {
                        x = Rest(x);
                    }

                    return x;

                case OpCode.LISTREF:
                    for (int k = (int)Num(y); k > 0; k--)
                    {
                        x = Rest(x);
                    }

                    return First(x);

                case OpCode.MEMQ:
                    return MemberAssoc(x, y, 'm', 'q');

                case OpCode.MEMV:
                    return MemberAssoc(x, y, 'm', 'v');

                case OpCode.MEMBER:
                    return MemberAssoc(x, y, 'm', ' ');

                case OpCode.ASSQ:
                    return MemberAssoc(x, y, 'a', 'q');

                case OpCode.ASSV:
                    return MemberAssoc(x, y, 'a', 'v');

                case OpCode.ASSOC:
                    return MemberAssoc(x, y, 'a', ' ');

                    // 6.4 SYMBOLS
                case OpCode.SYMBOLQ:
                    return Truth(x is string);

                case OpCode.SYMBOLTOSTRING:
                    return Sym(x).ToCharArray();

                case OpCode.STRINGTOSYMBOL:
                    return string.Intern(new string(Str(x)));

                    // 6.5 NUMBERS
                case OpCode.NUMBERQ:
                    return Truth(x is byte || x is int || x is long || x is float || x is double);

                case OpCode.ODDQ:
                    return Truth(Math.Abs(Num(x)) % 2 != 0);

                case OpCode.EVENQ:
                    return Truth(Math.Abs(Num(x)) % 2 == 0);

                case OpCode.ZEROQ:
                    return Truth(Num(x) == 0);

                case OpCode.POSITIVEQ:
                    return Truth(Num(x) > 0);

                case OpCode.NEGATIVEQ:
                    return Truth(Num(x) < 0);

                case OpCode.INTEGERQ:
                    return Truth(IsExact(x));

                case OpCode.INEXACTQ:
                    return Truth(!IsExact(x));

                case OpCode.LT:
                    return NumCompare(args, '<');

                case OpCode.GT:
                    return NumCompare(args, '>');

                case OpCode.EQ:
                    return NumCompare(args, '=');

                case OpCode.LE:
                    return NumCompare(args, 'L');

                case OpCode.GE:
                    return NumCompare(args, 'G');

                case OpCode.MAX:
                    return NumCompute(args, 'X', Num(x));

                case OpCode.MIN:
                    return NumCompute(args, 'N', Num(x));

                case OpCode.PLUS:
                    return NumCompute(args, '+', 0.0);

                case OpCode.MINUS:
                    return NumCompute(Rest(args), '-', Num(x));

                case OpCode.TIMES:
                    return NumCompute(args, '*', 1.0);

                case OpCode.DIVIDE:
                    return NumCompute(Rest(args), '/', Num(x));

                case OpCode.QUOTIENT:
                    double d = Num(x) / Num(y);
                    return Num(d > 0 ? Math.Floor(d) : Math.Ceiling(d));

                case OpCode.REMAINDER:
                    return Num((long)Num(x) % (long)Num(y));

                case OpCode.MODULO:
                    long xi = (long)Num(x);
                    long yi = (long)Num(y);
                    long m = xi % yi;
                    return Num(xi * yi > 0 || m == 0 ? m : m + yi);

                case OpCode.ABS:
                    return Num(Math.Abs(Num(x)));

                case OpCode.FLOOR:
                    return Num(Math.Floor(Num(x)));

                case OpCode.CEILING:
                    return Num(Math.Ceiling(Num(x)));

                case OpCode.TRUNCATE:
                    d = Num(x);
                    return Num(d < 0.0D ? Math.Ceiling(d) : Math.Floor(d));

                case OpCode.ROUND:
                    return Num(Math.Round(Num(x)));

                case OpCode.EXP:
                    return Num(Math.Exp(Num(x)));

                case OpCode.LOG:
                    return Num(Math.Log(Num(x)));

                case OpCode.SIN:
                    return Num(Math.Sin(Num(x)));

                case OpCode.COS:
                    return Num(Math.Cos(Num(x)));

                case OpCode.TAN:
                    return Num(Math.Tan(Num(x)));

                case OpCode.ASIN:
                    return Num(Math.Asin(Num(x)));

                case OpCode.ACOS:
                    return Num(Math.Acos(Num(x)));

                case OpCode.ATAN:
                    return Num(Math.Atan(Num(x)));

                case OpCode.SQRT:
                    return Num(Math.Sqrt(Num(x)));

                case OpCode.EXPT:
                    if (Num(x) == 0.0 && Num(y) < 0.0)
                    {
                        // Math.Pow gives infinity for this case
                        return Num(0.0);
                    }

                    return Num(Math.Pow(Num(x), Num(y)));

                case OpCode.NUMBERTOSTRING:
                    return NumberToString(x, y);

                case OpCode.STRINGTONUMBER:
                    return StringToNumber(x, y);

                case OpCode.GCD:
                    return args == null ? Zero : Gcd(args);

                case OpCode.LCM:
                    return args == null ? One : Lcm(args);

                    // 6.6 CHARACTERS
                case OpCode.CHARQ:
                    return Truth(x is char);

                case OpCode.CHARALPHABETICQ:
                    return Truth(char.IsLetter(Chr(x)));

                case OpCode.CHARNUMERICQ:
                    return Truth(char.IsDigit(Chr(x)));

                case OpCode.CHARWHITESPACEQ:
                    return Truth(char.IsWhiteSpace(Chr(x)));

                case OpCode.CHARUPPERCASEQ:
                    return Truth(char.IsUpper(Chr(x)));

                case OpCode.CHARLOWERCASEQ:
                    return Truth(char.IsLower(Chr(x)));

                case OpCode.CHARTOINTEGER:
                    return (double)Chr(x);

                case OpCode.INTEGERTOCHAR:
                    return Chr((char)(int)Num(x));

                case OpCode.CHARUPCASE:
                    return Chr(char.ToUpper(Chr(x)));

                case OpCode.CHARDOWNCASE:
                    return Chr(char.ToLower(Chr(x)));

                case OpCode.CHARCMPEQ:
                    return Truth(CharCompare(x, y, false) == 0);

                case OpCode.CHARCMPLT:
                    return Truth(CharCompare(x, y, false) < 0);

                case OpCode.CHARCMPGT:
                    return Truth(CharCompare(x, y, false) > 0);

                case OpCode.CHARCMPGE:
                    return Truth(CharCompare(x, y, false) >= 0);

                case OpCode.CHARCMPLE:
                    return Truth(CharCompare(x, y, false) <= 0);

                case OpCode.CHARCICMPEQ:
                    return Truth(CharCompare(x, y, true) == 0);

                case OpCode.CHARCICMPLT:
                    return Truth(CharCompare(x, y, true) < 0);

                case OpCode.CHARCICMPGT:
                    return Truth(CharCompare(x, y, true) > 0);

                case OpCode.CHARCICMPGE:
                    return Truth(CharCompare(x, y, true) >= 0);

                case OpCode.CHARCICMPLE:
                    return Truth(CharCompare(x, y, true) <= 0);

                case OpCode.ERROR:
                    return Error(Stringify(args));

                    // 6.7 STRINGS
                case OpCode.STRINGQ:
                    return Truth(x is char[]);

                case OpCode.MAKESTRING:
                    char[] str = new char[(int)Num(x)];
                    if (y != null)
                    {
                        char c = Chr(y);
                        for (int i = str.Length - 1; i >= 0; i--)
                        {
                            str[i] = c;
                        }
                    }

                    return str;

                case OpCode.STRING:
                    return ListToString(args);

                case OpCode.STRINGLENGTH:
                    return Num(Str(x).Length);

                case OpCode.STRINGREF:
                    return Chr(Str(x)[(int)Num(y)]);

                case OpCode.STRINGSET:
                    object z = Third(args);
                    Str(x)[(int)Num(y)] = Chr(z);
                    return z;

                case OpCode.SUBSTRING:
                    int start = (int)Num(y);
                    int end = (int)Num(Third(args));
                    return new string(Str(x)).Substring(start, end - start).ToCharArray();

                case OpCode.STRINGAPPEND:
                    return StringAppend(args);

                case OpCode.STRINGTOLIST:
                    Pair result = null;
                    char[] str2 = Str(x);
                    for (int i = str2.Length - 1; i >= 0; i--)
                    {
                        result = Cons(Chr(str2[i]), result);
                    }

                    return result;

                case OpCode.LISTTOSTRING:
                    return ListToString(x);

                case OpCode.STRINGCMPEQ:
                    return Truth(StringCompare(x, y, false) == 0);

                case OpCode.STRINGCMPLT:
                    return Truth(StringCompare(x, y, false) < 0);

                case OpCode.STRINGCMPGT:
                    return Truth(StringCompare(x, y, false) > 0);

                case OpCode.STRINGCMPGE:
                    return Truth(StringCompare(x, y, false) >= 0);

                case OpCode.STRINGCMPLE:
                    return Truth(StringCompare(x, y, false) <= 0);

                case OpCode.STRINGCICMPEQ:
                    return Truth(StringCompare(x, y, true) == 0);

                case OpCode.STRINGCICMPLT:
                    return Truth(StringCompare(x, y, true) < 0);

                case OpCode.STRINGCICMPGT:
                    return Truth(StringCompare(x, y, true) > 0);

                case OpCode.STRINGCICMPGE:
                    return Truth(StringCompare(x, y, true) >= 0);

                case OpCode.STRINGCICMPLE:
                    return Truth(StringCompare(x, y, true) <= 0);

                    // 6.8 VECTORS
                case OpCode.VECTORQ:
                    return Truth(x is object[]);

                case OpCode.MAKEVECTOR:
                    object[] vec = new object[(int)Num(x)];
                    if (y != null)
                    {
                        for (int i = 0; i < vec.Length; i++)
                        {
                            vec[i] = y;
                        }
                    }

                    return vec;

                case OpCode.VECTOR:
                    return ListToVector(args);

                case OpCode.VECTORLENGTH:
                    return Num(Vec(x).Length);

                case OpCode.VECTORREF:
                    return Vec(x)[(int)Num(y)];

                case OpCode.VECTORSET:
                    return Vec(x)[(int)Num(y)] = Third(args);

                case OpCode.VECTORTOLIST:
                    return VectorToList(x);

                case OpCode.LISTTOVECTOR:
                    return ListToVector(x);

                    // 6.9 CONTROL FEATURES
                case OpCode.EVAL:
                    // Instead of returning a value, return an evaulator that can be run to get the value
                    return Evaluator.CallMain(interp, parent, x, parent.Env);

                case OpCode.FORCE:
                    return !(x is Procedure) ? x : Proc(x).Apply(interp, parent, null);

                case OpCode.PROCEDUREQ:
                    return Truth(x is Procedure);

                case OpCode.APPLY:
                    return Proc(x).Apply(interp, parent, ListStar(Rest(args)));

                case OpCode.MAP:
                    return Evaluator.CallMap(interp, parent, Rest(args), parent.Env, Proc(x), List(null));

                case OpCode.FOREACH:
                    return Evaluator.CallMap(interp, parent, Rest(args), parent.Env, Proc(x), null);

                case OpCode.CALLCC:
                    Exception cc = new Exception();
                    Continuation proc = new Continuation(cc);
                    try
                    {
                        // TODO apply must not return evaluator here, otherwise we will lose the
                        //    context of catch
                        return Proc(x).Apply(interp, null, List(proc));
                    }
                    catch (Exception ex)
                    {
                        if (ex == cc)
                        {
                            return proc.Value;
                        }

                        throw;
                    }

                    // 6.10 INPUT AND OUTPUT
                case OpCode.EOFOBJECTQ:
                    return Truth(InputPort.IsEOF(x));

                case OpCode.INPUTPORTQ:
                    return Truth(x is InputPort);

                case OpCode.CURRENTINPUTPORT:
                    return interp.Input;

                case OpCode.OPENINPUTFILE:
                    return OpenInputFile(x);

                case OpCode.CLOSEINPUTPORT:
                    return InPort(x, interp).Close();

                case OpCode.OUTPUTPORTQ:
                    return Truth(x is PrintWriter);

                case OpCode.CURRENTOUTPUTPORT:
                    return interp.Output;

                case OpCode.OPENOUTPUTFILE:
                    return OpenOutputFile(x);

                case OpCode.CALLWITHOUTPUTFILE:
                    PrintWriter p = null;
                    try
                    {
                        p = OpenOutputFile(x);
                        // TODO apply can be delayed, in which case Close needs to be delayed as well.
                        z = Proc(y).Apply(interp, null, List(p));
                    }
                    finally
                    {
                        if (p != null)
                        {
                            p.Close();
                        }
                    }

                    return z;

                case OpCode.CALLWITHINPUTFILE:
                    InputPort p2 = null;
                    try
                    {
                        p2 = OpenInputFile(x);
                        // TODO apply can be delayed, in which case Close needs to be delayed as well.
                        z = Proc(y).Apply(interp, null, List(p2));
                    }
                    finally
                    {
                        if (p2 != null)
                        {
                            p2.Close();
                        }
                    }

                    return z;

                case OpCode.CLOSEOUTPUTPORT:
                    OutPort(x, interp).Close();
                    return True;

                case OpCode.READCHAR:
                    return InPort(x, interp).ReadChar();

                case OpCode.PEEKCHAR:
                    return InPort(x, interp).PeekChar();

                case OpCode.LOAD:
                    return interp.Load(x);

                case OpCode.READ:
                    return InPort(x, interp).Read();

                case OpCode.EOF_OBJECT:
                    return Truth(InputPort.IsEOF(x));

                case OpCode.WRITE:
                    return Write(x, OutPort(y, interp), true);

                case OpCode.DISPLAY:
                    return Write(x, OutPort(y, interp), false);

                case OpCode.NEWLINE:
                    OutPort(x, interp).Println();
                    OutPort(x, interp).Flush();
                    return True;

                    // EXTENSIONS
                case OpCode.CLASS:
                    try
                    {
                        return Type.GetType(Stringify(x, false));
                    }
                    catch (TypeLoadException)
                    {
                    }

                    return False;

                case OpCode.NEW:
                    try
                    {
                        return ClrMethod.CreateInstance(x);
                    }
                    catch (ArgumentNullException)
                    {
                    }
                    catch (ArgumentException)
                    {
                    }
                    catch (BadImageFormatException)
                    {
                    }
                    catch (MissingMethodException)
                    {
                    }
                    catch (FileLoadException)
                    {
                    }
                    catch (FileNotFoundException)
                    {
                    }
                    catch (TargetInvocationException)
                    {
                    }

                    return False;

                case OpCode.METHOD:
                    return new ClrMethod(Stringify(x, false), y, Rest(Rest(args)));

                case OpCode.EXIT:
                    System.Environment.Exit(x == null ? 0 : (int)Num(x));
                    return False; // required by style cop -- unnecessary

                case OpCode.LISTSTAR:
                    return ListStar(args);

                case OpCode.TIMECALL:
                    long startMem = GC.GetTotalMemory(true);
                    Stopwatch stopwatch = Stopwatch.StartNew();
                    object ans = False;
                    int counter = y == null ? 1 : (int)Num(y);
                    for (int i = 0; i < counter; i++)
                    {
                        // TODO if this returns Evaluator, then it has not actually done anything
                        ans = Proc(x).Apply(interp, parent, null);
                    }

                    stopwatch.Stop();
                    long time = stopwatch.ElapsedMilliseconds;
                    long mem = GC.GetTotalMemory(false) - startMem;
                    return Cons(ans, List(List(Num(time), "msec"), List(Num(mem), "bytes")));

                default:
                    return Error("internal error: unknown primitive: " + this + 
                        " applied to " + args);
            }
        }

        /// <summary>
        /// Append a list of lists, making one longer list.
        /// The appending only goes one level deep.
        /// </summary>
        /// <param name="args">A list of lists.</param>
        /// <returns>A list of the given list elements.</returns>
        private static object Append(object args)
        {
            if (Rest(args) == null)
            {
                return First(args);
            }

            return Append2(First(args), Append(Rest(args)));
        }

        /// <summary>
        /// Appends two lists.
        /// </summary>
        /// <param name="x">The first list.</param>
        /// <param name="y">The second list.</param>
        /// <returns>One list containing the elements in the first list followed by 
        /// those in the second list.</returns>
        private static object Append2(object x, object y)
        {
            return x is Pair ? Cons(First(x), Append2(Rest(x), y)) : y;
        }

        /// <summary>
        /// Compares two characters.
        /// </summary>
        /// <param name="x">The first char.</param>
        /// <param name="y">The second char.</param>
        /// <param name="ci">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if x is before y, positive if x is after y, 
        /// or 0 if they are the same.</returns>
        private static int CharCompare(object x, object y, bool ci)
        {
            char xc = Chr(x);
            char yc = Chr(y);
            if (ci)
            {
                xc = char.ToLower(xc);
                yc = char.ToLower(yc);
            }

            return xc - yc;
        }

        /// <summary>
        /// Compute the greatest common divisor of a list of numbers.
        /// </summary>
        /// <param name="args">The list of numbers.</param>
        /// <returns>The greatest common divisor.</returns>
        private static object Gcd(object args)
        {
            long gcd = 0;
            while (args is Pair)
            {
                gcd = Gcd2(Math.Abs((long)Num(First(args))), gcd);
                args = Rest(args);
            }

            return Num(gcd);
        }

        /// <summary>
        /// Compute the greatest common divisor of two numbers at a time.
        /// </summary>
        /// <param name="a">The first number.</param>
        /// <param name="b">The second number.</param>
        /// <returns>The GCD of the two numbers.</returns>
        private static long Gcd2(long a, long b)
        {
            if (b == 0)
            {
                return a;
            }

            return Gcd2(b, a % b);
        }

        /// <summary>
        /// Tests if the number is exact.
        /// TODO why is this the correct algorithm?
        /// </summary>
        /// <param name="x">The number to test.</param>
        /// <returns>True if the number is exact.</returns>
        private static bool IsExact(object x)
        {
            if (!(x is double))
            {
                return false;
            }

            double d = Num(x);
            return d == Math.Round(d) && Math.Abs(d) < 102962884861573423.0;
        }

        /// <summary>
        /// Tests to see if the given ofject is a list.
        /// TODO simplify by undoing the loop unrolling
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if the object is a list.</returns>
        private static bool IsList(object x)
        {
            object slow = x;
            object fast = x;
            while (true)
            {
                if (fast == null)
                {
                    return true;
                }

                if (slow == Rest(fast) || !(fast is Pair) || !(slow is Pair))
                {
                    return false;
                }

                fast = Rest(fast);
                if (fast == null)
                {
                    return true;
                }

                if (!(fast is Pair))
                {
                    return false;
                }

                fast = Rest(fast);
            }
        }

        /// <summary>
        /// Compute the least common multiple of a set of numbers.
        /// </summary>
        /// <param name="args">The numbers to use.</param>
        /// <returns>The LCM of these numbers.</returns>
        private static object Lcm(object args)
        {
            long lcm = 1;
            while (args is Pair)
            {
                long n = Math.Abs((long)Num(First(args)));
                long g = Gcd2(n, lcm);
                lcm = g == 0 ? g : (n / g) * lcm;
                args = Rest(args);
            }

            return Num(lcm);
        }

        /// <summary>
        /// Searches lists, used by memq, memv, and member.
        /// Also used by assq, assv, and assoc.
        /// </summary>
        /// <param name="obj">The object to search for.</param>
        /// <param name="list">The list to search in.</param>
        /// <param name="m">If 'm', do member, if 'a' do assoc.</param>
        /// <param name="eq">This gives the type of equality test to use.</param>
        /// <returns>The results that wer found.</returns>
        private static object MemberAssoc(object obj, object list, char m, char eq)
        {
            while (list is Pair)
            {
                object target = m == 'm' ? First(list) : First(First(list));
                bool found;
                switch (eq)
                {
                    case 'q':
                        found = target == obj;
                        break;

                    case 'v':
                        found = Eqv(target, obj);
                        break;

                    case ' ':
                        found = Equal(target, obj);
                        break;

                    default:
                        Warn("Bad option to memberAssoc: " + eq);
                        return False;
                }

                if (found)
                {
                    return m == 'm' ? list : First(list);
                }

                list = Rest(list);
            }

            return False;
        }

        /// <summary>
        /// Convert a number to its string equivalent.
        /// Optionally, use a number base different from 10.
        /// </summary>
        /// <param name="x">The number to convert.</param>
        /// <param name="y">The number base.</param>
        /// <returns>A string version of the number.</returns>
        private static object NumberToString(object x, object y)
        {
            int numberBase = y is double ? (int)Num(y) : 10;
            if (numberBase != 10 || Num(x) == Math.Round(Num(x)))
            {
                return Convert.ToString((long)Num(x), numberBase).ToCharArray();
            }

            return x.ToString().ToCharArray();
        }

        /// <summary>
        /// Compare a set of numbers using a given comparison operator.
        /// Comparison stops when one of the results yields false.
        /// </summary>
        /// <param name="args">A list of numbers to compare.</param>
        /// <param name="op">The operation to apply successively to pairs of 
        ///     adjacent numbers.</param>
        /// <returns>True only if all comparisons are true.</returns>
        private static object NumCompare(object args, char op)
        {
            while (Rest(args) is Pair)
            {
                double x = Num(First(args));
                args = Rest(args);

                double y = Num(First(args));

                switch (op)
                {
                    case '>':
                        if (!(x > y))
                        {
                            return False;
                        }

                        break;

                    case '<':
                        if (!(x < y))
                        {
                            return False;
                        }

                        break;

                    case '=':
                        if (x != y)
                        {
                            return False;
                        }

                        break;

                    case 'L':
                        if (!(x <= y))
                        {
                            return False;
                        }

                        break;

                    case 'G':
                        if (!(x >= y))
                        {
                            return False;
                        }

                        break;

                    default:
                        Error("internal error: unrecognized op: " + op);
                        break;
                }
            }

            return True;
        }

        /// <summary>
        /// Compute an operation on a list of numbers.
        /// In addition to usual arithmetic operators, can also do unary - and /
        ///   and also max and min.
        /// </summary>
        /// <param name="args">The list of numbers to operate on.</param>
        /// <param name="op">The operation to apply</param>
        /// <param name="result">The starting value.</param>
        /// <returns>The result of applying the operation to the list, starting 
        /// with the starting value.</returns>
        private static object NumCompute(object args, char op, double result)
        {
            if (args == null)
            {
                // If there are no numbers, apply a unary operation on the starting value.
                switch (op)
                {
                    case '-':
                        return Num(0 - result);

                    case '/':
                        return Num(1 / result);

                    default:
                        return Num(result);
                }
            }

            while (args is Pair)
            {
                double x = Num(First(args));
                args = Rest(args);

                switch (op)
                {
                    case 'X': // max
                        if (x > result)
                        {
                            result = x;
                        }

                        break;

                    case 'N': // min
                        if (x < result)
                        {
                            result = x;
                        }

                        break;

                    case '+':
                        result += x;
                        break;

                    case '-':
                        result -= x;
                        break;

                    case '*':
                        result *= x;
                        break;

                    case '/':
                        result /= x;
                        break;

                    default:
                        Error("internal error: unrecognized op: " + op);
                        break;
                }
            }

            return Num(result);
        }

        /// <summary>
        /// Open a file for input.
        /// </summary>
        /// <param name="filename">The filename of the file to open.</param>
        /// <returns>The input port, used for reading.</returns>
        private static InputPort OpenInputFile(object filename)
        {
            try
            {
                return new InputPort(new StreamReader(Stringify(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (InputPort)Error("No such file: " + Stringify(filename));
            }
            catch (IOException ex)
            {
                return (InputPort)Error("IOException: " + ex.Message);
            }
        }

        /// <summary>
        /// Open a file for output.
        /// </summary>
        /// <param name="filename">The filename.</param>
        /// <returns>The output port, used for writing.</returns>
        private static PrintWriter OpenOutputFile(object filename)
        {
            try
            {
                return new PrintWriter(new StreamWriter(Stringify(filename, false)));
            }
            catch (FileNotFoundException)
            {
                return (PrintWriter)Error("No such file: " + Stringify(filename));
            }
            catch (IOException ex)
            {
                return (PrintWriter)Error("IOException: " + ex.Message);
            }
        }

        /// <summary>
        /// Convert all the elements of a list to strings and append them.
        /// </summary>
        /// <param name="args">The list of items.</param>
        /// <returns>A character array of all the elements, converted to strings 
        /// and appended.</returns>
        private static char[] StringAppend(object args)
        {
            StringBuilder result = new StringBuilder();
            while (args is Pair)
            {
                result.Append(Stringify(First(args), false));
                args = Rest(args);
            }

            return result.ToString().ToCharArray();
        }

        /// <summary>
        /// Compare two strings.  Comparison may be case insensitive.
        /// Return value indicating their relative order.
        /// </summary>
        /// <param name="x">The first string.</param>
        /// <param name="y">The second string.</param>
        /// <param name="ci">If true, make the comparison case insensitive.</param>
        /// <returns>Negative if first string less then second, zero if they are equal, 
        /// positive if first is greater.</returns>
        private static int StringCompare(object x, object y, bool ci)
        {
            if (x is char[] && y is char[])
            {
                char[] xc = (char[])x;
                char[] yc = (char[])y;
                for (int i = 0; i < xc.Length; i++)
                {
                    int diff = !ci ? 
                        xc[i] - yc[i] : 
                        char.ToLower(xc[i]) - char.ToLower(yc[i]);
                    if (diff != 0)
                    {
                        return diff;
                    }
                }

                return xc.Length - yc.Length;
            }

            Error("expected two strings, got: " + Stringify(List(x, y)));
            return 0;
        }

        /// <summary>
        /// Convert a string into a number, in a given number base.
        /// </summary>
        /// <param name="x">The value to convert.  This is first converted to a string, 
        ///     then parsed as a number.</param>
        /// <param name="y">The number base.  If not a number, then base 10 is used.</param>
        /// <returns>The number represented by the string.</returns>
        private static object StringToNumber(object x, object y)
        {
            int numberBase = y is double ? (int)Num(y) : 10;
            try
            {
                return numberBase == 10
                           ? double.Parse(Stringify(x, false))
                           : Num(Convert.ToInt64(Stringify(x, false), numberBase));
            }
            catch (FormatException)
            {
                return False;
            }
            catch (ArgumentException)
            {
                return False;
            }
        }
    }
}