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
            P,
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
            TIMECALL = -7,
            ERROR = -8,
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
                .DefPrim("p", OpCode.P, 1, 1)
                .DefPrim("write-char", OpCode.DISPLAY, 1, 2)
                .DefPrim("zero?", OpCode.ZEROQ, 1)

                // EXTENSIONS
                .DefPrim("new", OpCode.NEW, 1)
                .DefPrim("class", OpCode.CLASS, 1)
                .DefPrim("method", OpCode.METHOD, 2, MaxInt)
                .DefPrim("exit", OpCode.EXIT, 0, 1)
                .DefPrim("error", OpCode.ERROR, 0, MaxInt)
                .DefPrim("time-call", OpCode.TIMECALL, 1, 2);

            return env;
        }

        /// <summary>
        /// Apply the primitive to the arguments, giving a result.
        /// This may return a result or a Stepper, which can be used to get a result.
        /// If parent is null, then it will not return a Stepper, so when that is not
        ///   acceptable, pass null for parent.
        /// </summary>
        /// <param name="parent">The calling Stepper.</param>
        /// <param name="args">The arguments to the primitive.</param>
        /// <returns>The result of the application.</returns>
        public override object Apply(Stepper parent, object args)
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

            object head = First(args);
            object tail = Second(args);
            Scheme interp = parent.Env.Interp;

            switch (this.operCode)
            {
                    // 6.1 BOOLEANS
                case OpCode.NOT:
                    return Truth(head is bool && (bool)head == false);

                case OpCode.BOOLEANQ:
                    return Truth(head is bool);

                case OpCode.EQVQ:
                    return Truth(Eqv(head, tail));

                case OpCode.EQQ:
                    // return Truth(x == y);
                    return Truth(Eqv(head, tail));

                case OpCode.EQUALQ:
                    // return Truth(x.Equals(y));
                    return Truth(Equal(head, tail));

                    // 6.2 EQUIVALENCE PREDICATES
                case OpCode.PAIRQ:
                    return Truth(head is Pair);

                case OpCode.LISTQ:
                    return Truth(ListUtils.IsList(head));

                case OpCode.CXR:
                    for (int i = this.Name.Length - 2; i >= 1; i--)
                    {
                        head = this.Name[i] == 'a' ? First(head) : Rest(head);
                    }

                    return head;

                case OpCode.CONS:
                    return Cons(head, tail);

                case OpCode.CAR:
                    return First(head);

                case OpCode.CDR:
                    return Rest(head);

                case OpCode.SETCAR:
                    return SetFirst(head, tail);

                case OpCode.SETCDR:
                    return SetRest(head, tail);

                case OpCode.SECOND:
                    return Second(head);

                case OpCode.THIRD:
                    return Third(head);

                case OpCode.NULLQ:
                    return Truth(head == null);

                case OpCode.LIST:
                    return args;

                case OpCode.LENGTH:
                    return NumberUtils.Num(Length(head));

                case OpCode.APPEND:
                    return args == null ? null : ListUtils.Append(args);

                case OpCode.REVERSE:
                    return Reverse(head);

                case OpCode.LISTTAIL:
                    for (int k = (int)NumberUtils.Num(tail); k > 0; k--)
                    {
                        head = Rest(head);
                    }

                    return head;

                case OpCode.LISTREF:
                    for (int k = (int)NumberUtils.Num(tail); k > 0; k--)
                    {
                        head = Rest(head);
                    }

                    return First(head);

                case OpCode.MEMQ:
                    return ListUtils.MemberAssoc(head, tail, 'm', 'q');

                case OpCode.MEMV:
                    return ListUtils.MemberAssoc(head, tail, 'm', 'v');

                case OpCode.MEMBER:
                    return ListUtils.MemberAssoc(head, tail, 'm', ' ');

                case OpCode.ASSQ:
                    return ListUtils.MemberAssoc(head, tail, 'a', 'q');

                case OpCode.ASSV:
                    return ListUtils.MemberAssoc(head, tail, 'a', 'v');

                case OpCode.ASSOC:
                    return ListUtils.MemberAssoc(head, tail, 'a', ' ');

                    // 6.4 SYMBOLS
                case OpCode.SYMBOLQ:
                    return Truth(head is string);

                case OpCode.SYMBOLTOSTRING:
                    return Sym(head).ToCharArray();

                case OpCode.STRINGTOSYMBOL:
                    return string.Intern(new string(StringUtils.Str(head)));

                    // 6.5 NUMBERS
                case OpCode.NUMBERQ:
                    return Truth(head is byte || head is int || head is long || head is float || head is double);

                case OpCode.ODDQ:
                    return Truth(Math.Abs(NumberUtils.Num(head)) % 2 != 0);

                case OpCode.EVENQ:
                    return Truth(Math.Abs(NumberUtils.Num(head)) % 2 == 0);

                case OpCode.ZEROQ:
                    return Truth(NumberUtils.Num(head) == 0);

                case OpCode.POSITIVEQ:
                    return Truth(NumberUtils.Num(head) > 0);

                case OpCode.NEGATIVEQ:
                    return Truth(NumberUtils.Num(head) < 0);

                case OpCode.INTEGERQ:
                    return Truth(NumberUtils.IsExact(head));

                case OpCode.INEXACTQ:
                    return Truth(!NumberUtils.IsExact(head));

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
                    return NumberUtils.NumCompute(args, 'X', NumberUtils.Num(head));

                case OpCode.MIN:
                    return NumberUtils.NumCompute(args, 'N', NumberUtils.Num(head));

                case OpCode.PLUS:
                    return NumberUtils.NumCompute(args, '+', 0.0);

                case OpCode.MINUS:
                    return NumberUtils.NumCompute(Rest(args), '-', NumberUtils.Num(head));

                case OpCode.TIMES:
                    return NumberUtils.NumCompute(args, '*', 1.0);

                case OpCode.DIVIDE:
                    return NumberUtils.NumCompute(Rest(args), '/', NumberUtils.Num(head));

                case OpCode.QUOTIENT:
                    double d = NumberUtils.Num(head) / NumberUtils.Num(tail);
                    return NumberUtils.Num(d > 0 ? Math.Floor(d) : Math.Ceiling(d));

                case OpCode.REMAINDER:
                    return NumberUtils.Num((long)NumberUtils.Num(head) % (long)NumberUtils.Num(tail));

                case OpCode.MODULO:
                    long xi = (long)NumberUtils.Num(head);
                    long yi = (long)NumberUtils.Num(tail);
                    long m = xi % yi;
                    return NumberUtils.Num(xi * yi > 0 || m == 0 ? m : m + yi);

                case OpCode.ABS:
                    return NumberUtils.Num(Math.Abs(NumberUtils.Num(head)));

                case OpCode.FLOOR:
                    return NumberUtils.Num(Math.Floor(NumberUtils.Num(head)));

                case OpCode.CEILING:
                    return NumberUtils.Num(Math.Ceiling(NumberUtils.Num(head)));

                case OpCode.TRUNCATE:
                    d = NumberUtils.Num(head);
                    return NumberUtils.Num(d < 0.0D ? Math.Ceiling(d) : Math.Floor(d));

                case OpCode.ROUND:
                    return NumberUtils.Num(Math.Round(NumberUtils.Num(head)));

                case OpCode.EXP:
                    return NumberUtils.Num(Math.Exp(NumberUtils.Num(head)));

                case OpCode.LOG:
                    return NumberUtils.Num(Math.Log(NumberUtils.Num(head)));

                case OpCode.SIN:
                    return NumberUtils.Num(Math.Sin(NumberUtils.Num(head)));

                case OpCode.COS:
                    return NumberUtils.Num(Math.Cos(NumberUtils.Num(head)));

                case OpCode.TAN:
                    return NumberUtils.Num(Math.Tan(NumberUtils.Num(head)));

                case OpCode.ASIN:
                    return NumberUtils.Num(Math.Asin(NumberUtils.Num(head)));

                case OpCode.ACOS:
                    return NumberUtils.Num(Math.Acos(NumberUtils.Num(head)));

                case OpCode.ATAN:
                    return NumberUtils.Num(Math.Atan(NumberUtils.Num(head)));

                case OpCode.SQRT:
                    return NumberUtils.Num(Math.Sqrt(NumberUtils.Num(head)));

                case OpCode.EXPT:
                    if (NumberUtils.Num(head) == 0.0 && NumberUtils.Num(tail) < 0.0)
                    {
                        // Math.Pow gives infinity for this case
                        return NumberUtils.Num(0.0);
                    }

                    return NumberUtils.Num(Math.Pow(NumberUtils.Num(head), NumberUtils.Num(tail)));

                case OpCode.NUMBERTOSTRING:
                    return NumberUtils.NumberToString(head, tail);

                case OpCode.STRINGTONUMBER:
                    return StringUtils.StringToNumber(head, tail);

                case OpCode.GCD:
                    return args == null ? Zero : NumberUtils.Gcd(args);

                case OpCode.LCM:
                    return args == null ? One : NumberUtils.Lcm(args);

                    // 6.6 CHARACTERS
                case OpCode.CHARQ:
                    return Truth(head is char);

                case OpCode.CHARALPHABETICQ:
                    return Truth(char.IsLetter(Chr(head)));

                case OpCode.CHARNUMERICQ:
                    return Truth(char.IsDigit(Chr(head)));

                case OpCode.CHARWHITESPACEQ:
                    return Truth(char.IsWhiteSpace(Chr(head)));

                case OpCode.CHARUPPERCASEQ:
                    return Truth(char.IsUpper(Chr(head)));

                case OpCode.CHARLOWERCASEQ:
                    return Truth(char.IsLower(Chr(head)));

                case OpCode.CHARTOINTEGER:
                    return (double)Chr(head);

                case OpCode.INTEGERTOCHAR:
                    return Chr((char)(int)NumberUtils.Num(head));

                case OpCode.CHARUPCASE:
                    return Chr(char.ToUpper(Chr(head)));

                case OpCode.CHARDOWNCASE:
                    return Chr(char.ToLower(Chr(head)));

                case OpCode.CHARCMPEQ:
                    return Truth(CharCompare(head, tail, false) == 0);

                case OpCode.CHARCMPLT:
                    return Truth(CharCompare(head, tail, false) < 0);

                case OpCode.CHARCMPGT:
                    return Truth(CharCompare(head, tail, false) > 0);

                case OpCode.CHARCMPGE:
                    return Truth(CharCompare(head, tail, false) >= 0);

                case OpCode.CHARCMPLE:
                    return Truth(CharCompare(head, tail, false) <= 0);

                case OpCode.CHARCICMPEQ:
                    return Truth(CharCompare(head, tail, true) == 0);

                case OpCode.CHARCICMPLT:
                    return Truth(CharCompare(head, tail, true) < 0);

                case OpCode.CHARCICMPGT:
                    return Truth(CharCompare(head, tail, true) > 0);

                case OpCode.CHARCICMPGE:
                    return Truth(CharCompare(head, tail, true) >= 0);

                case OpCode.CHARCICMPLE:
                    return Truth(CharCompare(head, tail, true) <= 0);

                case OpCode.ERROR:
                    return Error(StringUtils.AsString(args));

                    // 6.7 STRINGS
                case OpCode.STRINGQ:
                    return Truth(head is char[]);

                case OpCode.MAKESTRING:
                    {
                        char[] str = new char[(int)NumberUtils.Num(head)];
                        if (tail != null)
                        {
                            char c = Chr(tail);
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
                    return NumberUtils.Num(StringUtils.Str(head).Length);

                case OpCode.STRINGREF:
                    return Chr(StringUtils.Str(head)[(int)NumberUtils.Num(tail)]);

                case OpCode.STRINGSET:
                    object z = Third(args);
                    StringUtils.Str(head)[(int)NumberUtils.Num(tail)] = Chr(z);
                    return z;

                case OpCode.SUBSTRING:
                    int start = (int)NumberUtils.Num(tail);
                    int end = (int)NumberUtils.Num(Third(args));
                    return new string(StringUtils.Str(head)).Substring(start, end - start).ToCharArray();

                case OpCode.STRINGAPPEND:
                    return StringUtils.StringAppend(args);

                case OpCode.STRINGTOLIST:
                    {
                        Pair result = null;
                        char[] str = StringUtils.Str(head);
                        for (int i = str.Length - 1; i >= 0; i--)
                        {
                            result = Cons(Chr(str[i]), result);
                        }

                        return result;
                    }

                case OpCode.LISTTOSTRING:
                    return StringUtils.ListToString(head);

                case OpCode.STRINGCMPEQ:
                    return Truth(StringUtils.StringCompare(head, tail, false) == 0);

                case OpCode.STRINGCMPLT:
                    return Truth(StringUtils.StringCompare(head, tail, false) < 0);

                case OpCode.STRINGCMPGT:
                    return Truth(StringUtils.StringCompare(head, tail, false) > 0);

                case OpCode.STRINGCMPGE:
                    return Truth(StringUtils.StringCompare(head, tail, false) >= 0);

                case OpCode.STRINGCMPLE:
                    return Truth(StringUtils.StringCompare(head, tail, false) <= 0);

                case OpCode.STRINGCICMPEQ:
                    return Truth(StringUtils.StringCompare(head, tail, true) == 0);

                case OpCode.STRINGCICMPLT:
                    return Truth(StringUtils.StringCompare(head, tail, true) < 0);

                case OpCode.STRINGCICMPGT:
                    return Truth(StringUtils.StringCompare(head, tail, true) > 0);

                case OpCode.STRINGCICMPGE:
                    return Truth(StringUtils.StringCompare(head, tail, true) >= 0);

                case OpCode.STRINGCICMPLE:
                    return Truth(StringUtils.StringCompare(head, tail, true) <= 0);

                    // 6.8 VECTORS
                case OpCode.VECTORQ:
                    return Truth(head is object[]);

                case OpCode.MAKEVECTOR:
                    object[] vec = new object[(int)NumberUtils.Num(head)];
                    if (tail != null)
                    {
                        for (int i = 0; i < vec.Length; i++)
                        {
                            vec[i] = tail;
                        }
                    }

                    return vec;

                case OpCode.VECTOR:
                    return VectorUtils.ListToVector(args);

                case OpCode.VECTORLENGTH:
                    return NumberUtils.Num(VectorUtils.Vec(head).Length);

                case OpCode.VECTORREF:
                    return VectorUtils.Vec(head)[(int)NumberUtils.Num(tail)];

                case OpCode.VECTORSET:
                    return VectorUtils.Vec(head)[(int)NumberUtils.Num(tail)] = Third(args);

                case OpCode.VECTORTOLIST:
                    return VectorUtils.VectorToList(head);

                case OpCode.LISTTOVECTOR:
                    return VectorUtils.ListToVector(head);

                    // 6.9 CONTROL FEATURES
                case OpCode.EVAL:
                    // Instead of returning a value, return an evaulator that can be run to get the value
                    return Stepper.CallEvaluate(parent, head, parent.Env);

                case OpCode.FORCE:
                    return !(head is Procedure) ? head : Proc(head).Apply(parent, null);

                case OpCode.PROCEDUREQ:
                    return Truth(head is Procedure);

                case OpCode.APPLY:
                    return Proc(head).Apply(parent, ListStar(Rest(args)));

                case OpCode.MAP:
                    return Stepper.CallMap(parent, Rest(args), parent.Env, Proc(head), List(null));

                case OpCode.FOREACH:
                    return Stepper.CallMap(parent, Rest(args), parent.Env, Proc(head), null);

                case OpCode.CALLCC:
                    return Proc(head).Apply(
                        parent,
                        List(new Continuation(Stepper.CallContinuation(parent, head, parent.Env))));

                    // 6.10 INPUT AND OUTPUT
                case OpCode.EOFOBJECTQ:
                    return Truth(InputPort.IsEOF(head));

                case OpCode.INPUTPORTQ:
                    return Truth(head is InputPort);

                case OpCode.CURRENTINPUTPORT:
                    return interp.Input;

                case OpCode.OPENINPUTFILE:
                    return Stepper.OpenInputFile(head);

                case OpCode.CLOSEINPUTPORT:
                    return FileUtils.InPort(head, interp).Close();

                case OpCode.OUTPUTPORTQ:
                    return Truth(head is OutputPort);

                case OpCode.CURRENTOUTPUTPORT:
                    return interp.Output;

                case OpCode.OPENOUTPUTFILE:
                    return Stepper.OpenOutputFile(head);

                case OpCode.CALLWITHOUTPUTFILE:
                    return Stepper.CallWithOutputFile(parent, args, parent.Env);

                case OpCode.CALLWITHINPUTFILE:
                    return Stepper.CallWithInputFile(parent, args, parent.Env);

                case OpCode.CLOSEOUTPUTPORT:
                    FileUtils.OutPort(head, interp).Close();
                    return True;

                case OpCode.READCHAR:
                    return FileUtils.InPort(head, interp).ReadChar();

                case OpCode.PEEKCHAR:
                    return FileUtils.InPort(head, interp).PeekChar();

                case OpCode.LOAD:
                    return interp.Load(head);

                case OpCode.READ:
                    return FileUtils.InPort(head, interp).Read();

                case OpCode.EOF_OBJECT:
                    return Truth(InputPort.IsEOF(head));

                case OpCode.WRITE:
                    return FileUtils.Write(head, FileUtils.OutPort(tail, interp), true);

                case OpCode.P:
                    return FileUtils.P(head);

                case OpCode.DISPLAY:
                    return FileUtils.Write(head, FileUtils.OutPort(tail, interp), false);

                case OpCode.NEWLINE:
                    FileUtils.OutPort(head, interp).Println();
                    FileUtils.OutPort(head, interp).Flush();
                    return True;

                    // EXTENSIONS
                case OpCode.CLASS:
                    try
                    {
                        return Type.GetType(StringUtils.AsString(head, false));
                    }
                    catch (TypeLoadException)
                    {
                    }

                    return False;

                case OpCode.NEW:
                    try
                    {
                        return ClrMethod.CreateInstance(head);
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
                    return new ClrMethod(StringUtils.AsString(head, false), tail, Rest(Rest(args)));

                case OpCode.EXIT:
                    System.Environment.Exit(head == null ? 0 : (int)NumberUtils.Num(head));
                    return False; // required by style cop -- unnecessary

                case OpCode.TIMECALL:
                    return Stepper.CallTimeCall(parent, args, parent.Env);

                default:
                    return Error("Internal error: unknown primitive: " + this +
                                 " applied to " + args);
            }
        }
    }
}