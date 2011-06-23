#define OLD
// <copyright file="Primitive.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.CodeAnalysis;
    using System.IO;
    using System.Reflection;

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
        [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1602:EnumerationItemsMustBeDocumented",
                Justification = "These are just the op codes for the primitives, documented in the switch statement.")]

        // ReSharper disable InconsistentNaming
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

        // ReSharper restore InconsistentNaming

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
        /// This may return a result or a Stepper, which can be used to get a result.
        /// If parent is null, then it will not return a Stepper, so when that is not
        ///   acceptable, pass null for parent.
        /// </summary>
        /// <param name="interp">The interpreter context.</param>
        /// <param name="parent">The calling Stepper.</param>
        /// <param name="args">The arguments to the primitive.</param>
        /// <returns>The result of the application.</returns>
        public override object Apply(Scheme interp, Stepper parent, object args)
        {
            int numArgs = Length(args);
            if (numArgs < this.minArgs)
            {
                return Error("Primitive: too few args, " + numArgs + ", for " +
                             this.Name + ": " + args);
            }

            if (numArgs > this.maxArgs)
            {
                return Error("Primitive: too many args, " + numArgs + ", for " +
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
                    return NumberUtils.Num(Length(x));

                case OpCode.APPEND:
                    return args == null ? null : Append(args);

                case OpCode.REVERSE:
                    return Reverse(x);

                case OpCode.LISTTAIL:
                    for (int k = (int)NumberUtils.Num(y); k > 0; k--)
                    {
                        x = Rest(x);
                    }

                    return x;

                case OpCode.LISTREF:
                    for (int k = (int)NumberUtils.Num(y); k > 0; k--)
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
                    return string.Intern(new string(StringUtils.Str(x)));

                    // 6.5 NUMBERS
                case OpCode.NUMBERQ:
                    return Truth(x is byte || x is int || x is long || x is float || x is double);

                case OpCode.ODDQ:
                    return Truth(Math.Abs(NumberUtils.Num(x)) % 2 != 0);

                case OpCode.EVENQ:
                    return Truth(Math.Abs(NumberUtils.Num(x)) % 2 == 0);

                case OpCode.ZEROQ:
                    return Truth(NumberUtils.Num(x) == 0);

                case OpCode.POSITIVEQ:
                    return Truth(NumberUtils.Num(x) > 0);

                case OpCode.NEGATIVEQ:
                    return Truth(NumberUtils.Num(x) < 0);

                case OpCode.INTEGERQ:
                    return Truth(NumberUtils.IsExact(x));

                case OpCode.INEXACTQ:
                    return Truth(!NumberUtils.IsExact(x));

                case OpCode.LT:
                    return NumberUtils.NumCompare(args, '<');

                case OpCode.GT:
                    return NumberUtils.NumCompare(args, '>');

                case OpCode.EQ:
                    return NumberUtils.NumCompare(args, '=');

                case OpCode.LE:
                    return NumberUtils.NumCompare(args, 'L');

                case OpCode.GE:
                    return NumberUtils.NumCompare(args, 'G');

                case OpCode.MAX:
                    return NumberUtils.NumCompute(args, 'X', NumberUtils.Num(x));

                case OpCode.MIN:
                    return NumberUtils.NumCompute(args, 'N', NumberUtils.Num(x));

                case OpCode.PLUS:
                    return NumberUtils.NumCompute(args, '+', 0.0);

                case OpCode.MINUS:
                    return NumberUtils.NumCompute(Rest(args), '-', NumberUtils.Num(x));

                case OpCode.TIMES:
                    return NumberUtils.NumCompute(args, '*', 1.0);

                case OpCode.DIVIDE:
                    return NumberUtils.NumCompute(Rest(args), '/', NumberUtils.Num(x));

                case OpCode.QUOTIENT:
                    double d = NumberUtils.Num(x) / NumberUtils.Num(y);
                    return NumberUtils.Num(d > 0 ? Math.Floor(d) : Math.Ceiling(d));

                case OpCode.REMAINDER:
                    return NumberUtils.Num((long)NumberUtils.Num(x) % (long)NumberUtils.Num(y));

                case OpCode.MODULO:
                    long xi = (long)NumberUtils.Num(x);
                    long yi = (long)NumberUtils.Num(y);
                    long m = xi % yi;
                    return NumberUtils.Num(xi * yi > 0 || m == 0 ? m : m + yi);

                case OpCode.ABS:
                    return NumberUtils.Num(Math.Abs(NumberUtils.Num(x)));

                case OpCode.FLOOR:
                    return NumberUtils.Num(Math.Floor(NumberUtils.Num(x)));

                case OpCode.CEILING:
                    return NumberUtils.Num(Math.Ceiling(NumberUtils.Num(x)));

                case OpCode.TRUNCATE:
                    d = NumberUtils.Num(x);
                    return NumberUtils.Num(d < 0.0D ? Math.Ceiling(d) : Math.Floor(d));

                case OpCode.ROUND:
                    return NumberUtils.Num(Math.Round(NumberUtils.Num(x)));

                case OpCode.EXP:
                    return NumberUtils.Num(Math.Exp(NumberUtils.Num(x)));

                case OpCode.LOG:
                    return NumberUtils.Num(Math.Log(NumberUtils.Num(x)));

                case OpCode.SIN:
                    return NumberUtils.Num(Math.Sin(NumberUtils.Num(x)));

                case OpCode.COS:
                    return NumberUtils.Num(Math.Cos(NumberUtils.Num(x)));

                case OpCode.TAN:
                    return NumberUtils.Num(Math.Tan(NumberUtils.Num(x)));

                case OpCode.ASIN:
                    return NumberUtils.Num(Math.Asin(NumberUtils.Num(x)));

                case OpCode.ACOS:
                    return NumberUtils.Num(Math.Acos(NumberUtils.Num(x)));

                case OpCode.ATAN:
                    return NumberUtils.Num(Math.Atan(NumberUtils.Num(x)));

                case OpCode.SQRT:
                    return NumberUtils.Num(Math.Sqrt(NumberUtils.Num(x)));

                case OpCode.EXPT:
                    if (NumberUtils.Num(x) == 0.0 && NumberUtils.Num(y) < 0.0)
                    {
                        // Math.Pow gives infinity for this case
                        return NumberUtils.Num(0.0);
                    }

                    return NumberUtils.Num(Math.Pow(NumberUtils.Num(x), NumberUtils.Num(y)));

                case OpCode.NUMBERTOSTRING:
                    return NumberUtils.NumberToString(x, y);

                case OpCode.STRINGTONUMBER:
                    return StringUtils.StringToNumber(x, y);

                case OpCode.GCD:
                    return args == null ? Zero : NumberUtils.Gcd(args);

                case OpCode.LCM:
                    return args == null ? One : NumberUtils.Lcm(args);

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
                    return Chr((char)(int)NumberUtils.Num(x));

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
                    return Error(StringUtils.AsString(args));

                    // 6.7 STRINGS
                case OpCode.STRINGQ:
                    return Truth(x is char[]);

                case OpCode.MAKESTRING:
                    {
                        char[] str = new char[(int)NumberUtils.Num(x)];
                        if (y != null)
                        {
                            char c = Chr(y);
                            for (int i = str.Length - 1; i >= 0; i--)
                            {
                                str[i] = c;
                            }
                        }

                        return str;
                    }

                case OpCode.STRING:
                    return StringUtils.ListToString(args);

                case OpCode.STRINGLENGTH:
                    return NumberUtils.Num(StringUtils.Str(x).Length);

                case OpCode.STRINGREF:
                    return Chr(StringUtils.Str(x)[(int)NumberUtils.Num(y)]);

                case OpCode.STRINGSET:
                    object z = Third(args);
                    StringUtils.Str(x)[(int)NumberUtils.Num(y)] = Chr(z);
                    return z;

                case OpCode.SUBSTRING:
                    int start = (int)NumberUtils.Num(y);
                    int end = (int)NumberUtils.Num(Third(args));
                    return new string(StringUtils.Str(x)).Substring(start, end - start).ToCharArray();

                case OpCode.STRINGAPPEND:
                    return StringUtils.StringAppend(args);

                case OpCode.STRINGTOLIST:
                    {
                        Pair result = null;
                        char[] str = StringUtils.Str(x);
                        for (int i = str.Length - 1; i >= 0; i--)
                        {
                            result = Cons(Chr(str[i]), result);
                        }

                        return result;
                    }

                case OpCode.LISTTOSTRING:
                    return StringUtils.ListToString(x);

                case OpCode.STRINGCMPEQ:
                    return Truth(StringUtils.StringCompare(x, y, false) == 0);

                case OpCode.STRINGCMPLT:
                    return Truth(StringUtils.StringCompare(x, y, false) < 0);

                case OpCode.STRINGCMPGT:
                    return Truth(StringUtils.StringCompare(x, y, false) > 0);

                case OpCode.STRINGCMPGE:
                    return Truth(StringUtils.StringCompare(x, y, false) >= 0);

                case OpCode.STRINGCMPLE:
                    return Truth(StringUtils.StringCompare(x, y, false) <= 0);

                case OpCode.STRINGCICMPEQ:
                    return Truth(StringUtils.StringCompare(x, y, true) == 0);

                case OpCode.STRINGCICMPLT:
                    return Truth(StringUtils.StringCompare(x, y, true) < 0);

                case OpCode.STRINGCICMPGT:
                    return Truth(StringUtils.StringCompare(x, y, true) > 0);

                case OpCode.STRINGCICMPGE:
                    return Truth(StringUtils.StringCompare(x, y, true) >= 0);

                case OpCode.STRINGCICMPLE:
                    return Truth(StringUtils.StringCompare(x, y, true) <= 0);

                    // 6.8 VECTORS
                case OpCode.VECTORQ:
                    return Truth(x is object[]);

                case OpCode.MAKEVECTOR:
                    object[] vec = new object[(int)NumberUtils.Num(x)];
                    if (y != null)
                    {
                        for (int i = 0; i < vec.Length; i++)
                        {
                            vec[i] = y;
                        }
                    }

                    return vec;

                case OpCode.VECTOR:
                    return VectorUtils.ListToVector(args);

                case OpCode.VECTORLENGTH:
                    return NumberUtils.Num(VectorUtils.Vec(x).Length);

                case OpCode.VECTORREF:
                    return VectorUtils.Vec(x)[(int)NumberUtils.Num(y)];

                case OpCode.VECTORSET:
                    return VectorUtils.Vec(x)[(int)NumberUtils.Num(y)] = Third(args);

                case OpCode.VECTORTOLIST:
                    return VectorUtils.VectorToList(x);

                case OpCode.LISTTOVECTOR:
                    return VectorUtils.ListToVector(x);

                    // 6.9 CONTROL FEATURES
                case OpCode.EVAL:
                    // Instead of returning a value, return an evaulator that can be run to get the value
                    return Stepper.CallEvaluate(interp, parent, x, parent.Env);

                case OpCode.FORCE:
                    return !(x is Procedure) ? x : Proc(x).Apply(interp, parent, null);

                case OpCode.PROCEDUREQ:
                    return Truth(x is Procedure);

                case OpCode.APPLY:
                    return Proc(x).Apply(interp, parent, ListStar(Rest(args)));

                case OpCode.MAP:
                    return Stepper.CallMap(interp, parent, Rest(args), parent.Env, Proc(x), List(null));

                case OpCode.FOREACH:
                    return Stepper.CallMap(interp, parent, Rest(args), parent.Env, Proc(x), null);

                case OpCode.CALLCC:
                    return Proc(x).Apply(
                        interp, 
                        parent,
                        List(new Continuation(Stepper.CallContinuation(interp, parent, x, parent.Env))));

                    // 6.10 INPUT AND OUTPUT
                case OpCode.EOFOBJECTQ:
                    return Truth(InputPort.IsEOF(x));

                case OpCode.INPUTPORTQ:
                    return Truth(x is InputPort);

                case OpCode.CURRENTINPUTPORT:
                    return interp.Input;

                case OpCode.OPENINPUTFILE:
                    return Stepper.OpenInputFile(x);

                case OpCode.CLOSEINPUTPORT:
                    return FileUtils.InPort(x, interp).Close();

                case OpCode.OUTPUTPORTQ:
                    return Truth(x is OutputPort);

                case OpCode.CURRENTOUTPUTPORT:
                    return interp.Output;

                case OpCode.OPENOUTPUTFILE:
                    return Stepper.OpenOutputFile(x);

                case OpCode.CALLWITHOUTPUTFILE:
                    return Stepper.CallWithOutputFile(interp, parent, args, parent.Env);

                case OpCode.CALLWITHINPUTFILE:
                    return Stepper.CallWithInputFile(interp, parent, args, parent.Env);

                case OpCode.CLOSEOUTPUTPORT:
                    FileUtils.OutPort(x, interp).Close();
                    return True;

                case OpCode.READCHAR:
                    return FileUtils.InPort(x, interp).ReadChar();

                case OpCode.PEEKCHAR:
                    return FileUtils.InPort(x, interp).PeekChar();

                case OpCode.LOAD:
                    return interp.Load(x);

                case OpCode.READ:
                    return FileUtils.InPort(x, interp).Read();

                case OpCode.EOF_OBJECT:
                    return Truth(InputPort.IsEOF(x));

                case OpCode.WRITE:
                    return FileUtils.Write(x, FileUtils.OutPort(y, interp), true);

                case OpCode.DISPLAY:
                    return FileUtils.Write(x, FileUtils.OutPort(y, interp), false);

                case OpCode.NEWLINE:
                    FileUtils.OutPort(x, interp).Println();
                    FileUtils.OutPort(x, interp).Flush();
                    return True;

                    // EXTENSIONS
                case OpCode.CLASS:
                    try
                    {
                        return Type.GetType(StringUtils.AsString(x, false));
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
                    return new ClrMethod(StringUtils.AsString(x, false), y, Rest(Rest(args)));

                case OpCode.EXIT:
                    System.Environment.Exit(x == null ? 0 : (int)NumberUtils.Num(x));
                    return False; // required by style cop -- unnecessary

                case OpCode.LISTSTAR:
                    return ListStar(args);

                case OpCode.TIMECALL:
                    return Stepper.CallTimeCall(interp, parent, args, parent.Env);

                default:
                    return Error("internal error: unknown primitive: " + this +
                                 " applied to " + args);
            }
        }

        /// <summary>
        /// Append a list of lists, making one longer list.
        /// The appending only goes one level deep.
        /// The very last list is not copied, but is not copied, but is instead shared.
        /// </summary>
        /// <param name="args">A list of lists.  Each of the lists in this list is appended together.</param>
        /// <returns>A list of the given list elements.</returns>
        private static object Append(object args)
        {
            Pair result = List(null);
            Pair accum = result;
            while (Rest(args) != null)
            {
                accum = Append(accum, First(args));
                args = Rest(args);
            }

            accum.Rest = First(args);
            return Rest(result);
        }

        /// <summary>
        /// Append one list to the tail of another.
        /// The tail is modified destructively.
        /// The appended list is copied.
        /// </summary>
        /// <param name="tail">The end of the first list, destructively appended to.</param>
        /// <param name="toCopy">The second list, copied onto the first.</param>
        /// <returns>The end of the second list, suitable for another call to this function. </returns>
        private static Pair Append(Pair tail, object toCopy)
        {
            while (toCopy != null)
            {
                tail.Rest = List(First(toCopy));
                toCopy = Rest(toCopy);
                tail = (Pair)Rest(tail);
            }

            return tail;
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
        /// Tests to see if the given object is a list.
        /// </summary>
        /// <param name="x">The object to test.</param>
        /// <returns>True if the object is a list.</returns>
        private static bool IsList(object x)
        {
            while (true)
            {
                if (x == null)
                {
                    return true;
                }

                if (!(x is Pair))
                {
                    return false;
                }

                object rest = Rest(x);
                if (rest == x)
                {
                    return false;
                }

                x = rest;
            }
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
    }
}