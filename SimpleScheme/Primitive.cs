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
            METHODSYNC = -3,
            METHODASYNC = -4,
            EXIT = -5,
            SETCAR = -6,
            SETCDR = -7,
            TIMECALL = -8,
            ERROR = -9,
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
                .DefinePrimitive("=", OpCode.EQ, 2, MaxInt)
                .DefinePrimitive("*", OpCode.TIMES, 0, MaxInt)
                .DefinePrimitive("+", OpCode.PLUS, 0, MaxInt)
                .DefinePrimitive("-", OpCode.MINUS, 1, MaxInt)
                .DefinePrimitive("/", OpCode.DIVIDE, 1, MaxInt)
                .DefinePrimitive("<", OpCode.LT, 2, MaxInt)
                .DefinePrimitive(">", OpCode.GT, 2, MaxInt)
                .DefinePrimitive("<=", OpCode.LE, 2, MaxInt)
                .DefinePrimitive(">=", OpCode.GE, 2, MaxInt)
                .DefinePrimitive("abs", OpCode.ABS, 1)
                .DefinePrimitive("acos", OpCode.ACOS, 1)
                .DefinePrimitive("append", OpCode.APPEND, 0, MaxInt)
                .DefinePrimitive("apply", OpCode.APPLY, 2, MaxInt)
                .DefinePrimitive("asin", OpCode.ASIN, 1)
                .DefinePrimitive("assoc", OpCode.ASSOC, 2)
                .DefinePrimitive("assq", OpCode.ASSQ, 2)
                .DefinePrimitive("assv", OpCode.ASSV, 2)
                .DefinePrimitive("atan", OpCode.ATAN, 1)
                .DefinePrimitive("boolean?", OpCode.BOOLEANQ, 1)
                .DefinePrimitive("caaaar", OpCode.CXR, 1)
                .DefinePrimitive("caaadr", OpCode.CXR, 1)
                .DefinePrimitive("caaar", OpCode.CXR, 1)
                .DefinePrimitive("caadar", OpCode.CXR, 1)
                .DefinePrimitive("caaddr", OpCode.CXR, 1)
                .DefinePrimitive("caar", OpCode.CXR, 1)
                .DefinePrimitive("cadaar", OpCode.CXR, 1)
                .DefinePrimitive("cadadr", OpCode.CXR, 1)
                .DefinePrimitive("cadar", OpCode.CXR, 1)
                .DefinePrimitive("caddar", OpCode.CXR, 1)
                .DefinePrimitive("cadddr", OpCode.CXR, 1)
                .DefinePrimitive("caddr", OpCode.CXR, 1)
                .DefinePrimitive("cadr", OpCode.CXR, 1)
                .DefinePrimitive("call-with-current-continuation", OpCode.CALLCC, 1)
                .DefinePrimitive("call/cc", OpCode.CALLCC, 1)
                .DefinePrimitive("call-with-input-file", OpCode.CALLWITHINPUTFILE, 2)
                .DefinePrimitive("call-with-output-file", OpCode.CALLWITHOUTPUTFILE, 2)
                .DefinePrimitive("car", OpCode.CAR, 1)
                .DefinePrimitive("first", OpCode.CAR, 1)
                .DefinePrimitive("second", OpCode.SECOND, 1)
                .DefinePrimitive("third", OpCode.THIRD, 1)
                .DefinePrimitive("cdaaar,", OpCode.CXR, 1)
                .DefinePrimitive("cdaadr", OpCode.CXR, 1)
                .DefinePrimitive("cdaar", OpCode.CXR, 1)
                .DefinePrimitive("cdadar", OpCode.CXR, 1)
                .DefinePrimitive("cdaddr", OpCode.CXR, 1)
                .DefinePrimitive("cdadr", OpCode.CXR, 1)
                .DefinePrimitive("cdar", OpCode.CXR, 1)
                .DefinePrimitive("cddaar", OpCode.CXR, 1)
                .DefinePrimitive("cddadr", OpCode.CXR, 1)
                .DefinePrimitive("cddar", OpCode.CXR, 1)
                .DefinePrimitive("cdddar", OpCode.CXR, 1)
                .DefinePrimitive("cddddr", OpCode.CXR, 1)
                .DefinePrimitive("cdddr", OpCode.CXR, 1)
                .DefinePrimitive("cddr", OpCode.CXR, 1)
                .DefinePrimitive("cdr", OpCode.CDR, 1)
                .DefinePrimitive("rest", OpCode.CDR, 1)
                .DefinePrimitive("char->integer", OpCode.CHARTOINTEGER, 1)
                .DefinePrimitive("char-alphabetic?", OpCode.CHARALPHABETICQ, 1)
                .DefinePrimitive("char-ci<=?", OpCode.CHARCICMPLE, 2)
                .DefinePrimitive("char-ci<?", OpCode.CHARCICMPLT, 2)
                .DefinePrimitive("char-ci=?", OpCode.CHARCICMPEQ, 2)
                .DefinePrimitive("char-ci>=?", OpCode.CHARCICMPGE, 2)
                .DefinePrimitive("char-ci>?", OpCode.CHARCICMPGT, 2)
                .DefinePrimitive("char-downcase", OpCode.CHARDOWNCASE, 1)
                .DefinePrimitive("char-lower-case?", OpCode.CHARLOWERCASEQ, 1)
                .DefinePrimitive("char-numeric?", OpCode.CHARNUMERICQ, 1)
                .DefinePrimitive("char-upcase", OpCode.CHARUPCASE, 1)
                .DefinePrimitive("char-upper-case?", OpCode.CHARUPPERCASEQ, 1)
                .DefinePrimitive("char-whitespace?", OpCode.CHARWHITESPACEQ, 1)
                .DefinePrimitive("char<=?", OpCode.CHARCMPLE, 2)
                .DefinePrimitive("char<?", OpCode.CHARCMPLT, 2)
                .DefinePrimitive("char=?", OpCode.CHARCMPEQ, 2)
                .DefinePrimitive("char>=?", OpCode.CHARCMPGE, 2)
                .DefinePrimitive("char>?", OpCode.CHARCMPGT, 2)
                .DefinePrimitive("char?", OpCode.CHARQ, 1)
                .DefinePrimitive("close-input-port", OpCode.CLOSEINPUTPORT, 1)
                .DefinePrimitive("close-output-port", OpCode.CLOSEOUTPUTPORT, 1)
                .DefinePrimitive("complex", OpCode.NUMBERQ, 1)
                .DefinePrimitive("cons", OpCode.CONS, 2)
                .DefinePrimitive("cos", OpCode.COS, 1)
                .DefinePrimitive("current-input-port", OpCode.CURRENTINPUTPORT, 0)
                .DefinePrimitive("current-output-port", OpCode.CURRENTOUTPUTPORT, 0)
                .DefinePrimitive("display", OpCode.DISPLAY, 1, 2)
                .DefinePrimitive("eof-object?", OpCode.EOFOBJECTQ, 1)
                .DefinePrimitive("eq?", OpCode.EQQ, 2)
                .DefinePrimitive("equal?", OpCode.EQUALQ, 2)
                .DefinePrimitive("eqv?", OpCode.EQVQ, 2)
                .DefinePrimitive("eval", OpCode.EVAL, 1, 2)
                .DefinePrimitive("even?", OpCode.EVENQ, 1)
                .DefinePrimitive("exact?", OpCode.INTEGERQ, 1)
                .DefinePrimitive("exp", OpCode.EXP, 1)
                .DefinePrimitive("expt", OpCode.EXPT, 2)
                .DefinePrimitive("force", OpCode.FORCE, 1)
                .DefinePrimitive("for-each", OpCode.FOREACH, 1, MaxInt)
                .DefinePrimitive("gcd", OpCode.GCD, 0, MaxInt)
                .DefinePrimitive("inexact?", OpCode.INEXACTQ, 1)
                .DefinePrimitive("input-port?", OpCode.INPUTPORTQ, 1)
                .DefinePrimitive("integer->char", OpCode.INTEGERTOCHAR, 1)
                .DefinePrimitive("integer?", OpCode.INTEGERQ, 1)
                .DefinePrimitive("lcm", OpCode.LCM, 0, MaxInt)
                .DefinePrimitive("length", OpCode.LENGTH, 1)
                .DefinePrimitive("list", OpCode.LIST, 0, MaxInt)
                .DefinePrimitive("list->string", OpCode.LISTTOSTRING, 1)
                .DefinePrimitive("list->vector", OpCode.LISTTOVECTOR, 1)
                .DefinePrimitive("list-ref", OpCode.LISTREF, 2)
                .DefinePrimitive("list-tail", OpCode.LISTTAIL, 2)
                .DefinePrimitive("list?", OpCode.LISTQ, 1)
                .DefinePrimitive("load", OpCode.LOAD, 1)
                .DefinePrimitive("log", OpCode.LOG, 1)
                .DefinePrimitive("make-string", OpCode.MAKESTRING, 1, 2)
                .DefinePrimitive("make-vector", OpCode.MAKEVECTOR, 1, 2)
                .DefinePrimitive("map", OpCode.MAP, 1, MaxInt)
                .DefinePrimitive("max", OpCode.MAX, 1, MaxInt)
                .DefinePrimitive("member", OpCode.MEMBER, 2)
                .DefinePrimitive("memq", OpCode.MEMQ, 2)
                .DefinePrimitive("memv", OpCode.MEMV, 2)
                .DefinePrimitive("min", OpCode.MIN, 1, MaxInt)
                .DefinePrimitive("modulo", OpCode.MODULO, 2)
                .DefinePrimitive("negative?", OpCode.NEGATIVEQ, 1)
                .DefinePrimitive("newline", OpCode.NEWLINE, 0, 1)
                .DefinePrimitive("not", OpCode.NOT, 1)
                .DefinePrimitive("null?", OpCode.NULLQ, 1)
                .DefinePrimitive("number->string", OpCode.NUMBERTOSTRING, 1, 2)
                .DefinePrimitive("number?", OpCode.NUMBERQ, 1)
                .DefinePrimitive("odd?", OpCode.ODDQ, 1)
                .DefinePrimitive("open-input-file", OpCode.OPENINPUTFILE, 1)
                .DefinePrimitive("open-output-file", OpCode.OPENOUTPUTFILE, 1)
                .DefinePrimitive("output-port?", OpCode.OUTPUTPORTQ, 1)
                .DefinePrimitive("pair?", OpCode.PAIRQ, 1)
                .DefinePrimitive("peek-char", OpCode.PEEKCHAR, 0, 1)
                .DefinePrimitive("positive?", OpCode.POSITIVEQ, 1)
                .DefinePrimitive("procedure?", OpCode.PROCEDUREQ, 1)
                .DefinePrimitive("quotient", OpCode.QUOTIENT, 2)
                .DefinePrimitive("rational?", OpCode.INTEGERQ, 1)
                .DefinePrimitive("read", OpCode.READ, 0, 1)
                .DefinePrimitive("read-char", OpCode.READCHAR, 0, 1)
                .DefinePrimitive("real?", OpCode.INTEGERQ, 1)
                .DefinePrimitive("remainder", OpCode.REMAINDER, 2)
                .DefinePrimitive("reverse", OpCode.REVERSE, 1)
                .DefinePrimitive("round", OpCode.ROUND, 1)
                .DefinePrimitive("set-car!", OpCode.SETCAR, 2)
                .DefinePrimitive("set-first!", OpCode.SETCAR, 2)
                .DefinePrimitive("set-cdr!", OpCode.SETCDR, 2)
                .DefinePrimitive("set-rest!", OpCode.SETCDR, 2)
                .DefinePrimitive("sin", OpCode.SIN, 1)
                .DefinePrimitive("sqrt", OpCode.SQRT, 1)
                .DefinePrimitive("string", OpCode.STRING, 0, MaxInt)
                .DefinePrimitive("string->list", OpCode.STRINGTOLIST, 1)
                .DefinePrimitive("string->number", OpCode.STRINGTONUMBER, 1, 2)
                .DefinePrimitive("string->symbol", OpCode.STRINGTOSYMBOL, 1)
                .DefinePrimitive("string-append", OpCode.STRINGAPPEND, 0, MaxInt)
                .DefinePrimitive("string-ci<=?", OpCode.STRINGCICMPLE, 2)
                .DefinePrimitive("string-ci<?", OpCode.STRINGCICMPLT, 2)
                .DefinePrimitive("string-ci=?", OpCode.STRINGCICMPEQ, 2)
                .DefinePrimitive("string-ci>=?", OpCode.STRINGCICMPGE, 2)
                .DefinePrimitive("string-ci>?", OpCode.STRINGCICMPGT, 2)
                .DefinePrimitive("string-length", OpCode.STRINGLENGTH, 1)
                .DefinePrimitive("string-ref", OpCode.STRINGREF, 2)
                .DefinePrimitive("string-set!", OpCode.STRINGSET, 3)
                .DefinePrimitive("string<=?", OpCode.STRINGCMPLE, 2)
                .DefinePrimitive("string<?", OpCode.STRINGCMPLT, 2)
                .DefinePrimitive("string=?", OpCode.STRINGCMPEQ, 2)
                .DefinePrimitive("string>=?", OpCode.STRINGCMPGE, 2)
                .DefinePrimitive("string>?", OpCode.STRINGCMPGT, 2)
                .DefinePrimitive("string?", OpCode.STRINGQ, 1)
                .DefinePrimitive("substring", OpCode.SUBSTRING, 3)
                .DefinePrimitive("symbol->string", OpCode.SYMBOLTOSTRING, 1)
                .DefinePrimitive("symbol?", OpCode.SYMBOLQ, 1)
                .DefinePrimitive("tan", OpCode.TAN, 1)
                .DefinePrimitive("vector", OpCode.VECTOR, 0, MaxInt)
                .DefinePrimitive("vector->list", OpCode.VECTORTOLIST, 1)
                .DefinePrimitive("vector-length", OpCode.VECTORLENGTH, 1)
                .DefinePrimitive("vector-ref", OpCode.VECTORREF, 2)
                .DefinePrimitive("vector-set!", OpCode.VECTORSET, 3)
                .DefinePrimitive("vector?", OpCode.VECTORQ, 1)
                .DefinePrimitive("write", OpCode.WRITE, 1, 2)
                .DefinePrimitive("p", OpCode.P, 1, 1)
                .DefinePrimitive("write-char", OpCode.DISPLAY, 1, 2)
                .DefinePrimitive("zero?", OpCode.ZEROQ, 1)

                // EXTENSIONS
                .DefinePrimitive("new", OpCode.NEW, 1)
                .DefinePrimitive("class", OpCode.CLASS, 1)
                .DefinePrimitive("method", OpCode.METHODSYNC, 2, MaxInt)
                .DefinePrimitive("method-async", OpCode.METHODASYNC, 2, MaxInt)
                .DefinePrimitive("exit", OpCode.EXIT, 0, 1)
                .DefinePrimitive("error", OpCode.ERROR, 0, MaxInt)
                .DefinePrimitive("time-call", OpCode.TIMECALL, 1, 2);

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

            object first = First(args);
            object second = Second(args);
            Interpreter interp = parent.Env.Interp;

            switch (this.operCode)
            {
                    // 6.1 BOOLEANS
                case OpCode.NOT:
                    return Truth(first is bool && (bool)first == false);

                case OpCode.BOOLEANQ:
                    return Truth(first is bool);

                case OpCode.EQVQ:
                    return Truth(Eqv(first, second));

                case OpCode.EQQ:
                    // return Truth(x == y);
                    return Truth(Eqv(first, second));

                case OpCode.EQUALQ:
                    // return Truth(x.Equals(y));
                    return Truth(Equal(first, second));

                    // 6.2 EQUIVALENCE PREDICATES
                case OpCode.PAIRQ:
                    return Truth(first is Pair);

                case OpCode.LISTQ:
                    return Truth(ListUtils.IsList(first));

                case OpCode.CXR:
                    for (int i = this.Name.Length - 2; i >= 1; i--)
                    {
                        first = this.Name[i] == 'a' ? First(first) : Rest(first);
                    }

                    return first;

                case OpCode.CONS:
                    return Cons(first, second);

                case OpCode.CAR:
                    return First(first);

                case OpCode.CDR:
                    return Rest(first);

                case OpCode.SETCAR:
                    return SetFirst(first, second);

                case OpCode.SETCDR:
                    return SetRest(first, second);

                case OpCode.SECOND:
                    return Second(first);

                case OpCode.THIRD:
                    return Third(first);

                case OpCode.NULLQ:
                    return Truth(first == null);

                case OpCode.LIST:
                    return args;

                case OpCode.LENGTH:
                    return NumberUtils.Num(Length(first));

                case OpCode.APPEND:
                    return args == null ? null : ListUtils.Append(args);

                case OpCode.REVERSE:
                    return Reverse(first);

                case OpCode.LISTTAIL:
                    for (int k = (int)NumberUtils.Num(second); k > 0; k--)
                    {
                        first = Rest(first);
                    }

                    return first;

                case OpCode.LISTREF:
                    for (int k = (int)NumberUtils.Num(second); k > 0; k--)
                    {
                        first = Rest(first);
                    }

                    return First(first);

                case OpCode.MEMQ:
                    return ListUtils.MemberAssoc(first, second, 'm', 'q');

                case OpCode.MEMV:
                    return ListUtils.MemberAssoc(first, second, 'm', 'v');

                case OpCode.MEMBER:
                    return ListUtils.MemberAssoc(first, second, 'm', ' ');

                case OpCode.ASSQ:
                    return ListUtils.MemberAssoc(first, second, 'a', 'q');

                case OpCode.ASSV:
                    return ListUtils.MemberAssoc(first, second, 'a', 'v');

                case OpCode.ASSOC:
                    return ListUtils.MemberAssoc(first, second, 'a', ' ');

                    // 6.4 SYMBOLS
                case OpCode.SYMBOLQ:
                    return Truth(first is string);

                case OpCode.SYMBOLTOSTRING:
                    return new SchemeString(Sym(first));

                case OpCode.STRINGTOSYMBOL:
                    return string.Intern(SchemeString.Str(first).AsString());

                    // 6.5 NUMBERS
                case OpCode.NUMBERQ:
                    return Truth(first is byte || first is int || first is long || first is float || first is double);

                case OpCode.ODDQ:
                    return Truth(Math.Abs(NumberUtils.Num(first)) % 2 != 0);

                case OpCode.EVENQ:
                    return Truth(Math.Abs(NumberUtils.Num(first)) % 2 == 0);

                case OpCode.ZEROQ:
                    return Truth(NumberUtils.Num(first) == 0);

                case OpCode.POSITIVEQ:
                    return Truth(NumberUtils.Num(first) > 0);

                case OpCode.NEGATIVEQ:
                    return Truth(NumberUtils.Num(first) < 0);

                case OpCode.INTEGERQ:
                    return Truth(NumberUtils.IsExact(first));

                case OpCode.INEXACTQ:
                    return Truth(!NumberUtils.IsExact(first));

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
                    return NumberUtils.NumCompute(args, 'X', NumberUtils.Num(first));

                case OpCode.MIN:
                    return NumberUtils.NumCompute(args, 'N', NumberUtils.Num(first));

                case OpCode.PLUS:
                    return NumberUtils.NumCompute(args, '+', 0.0);

                case OpCode.MINUS:
                    return NumberUtils.NumCompute(Rest(args), '-', NumberUtils.Num(first));

                case OpCode.TIMES:
                    return NumberUtils.NumCompute(args, '*', 1.0);

                case OpCode.DIVIDE:
                    return NumberUtils.NumCompute(Rest(args), '/', NumberUtils.Num(first));

                case OpCode.QUOTIENT:
                    double d = NumberUtils.Num(first) / NumberUtils.Num(second);
                    return NumberUtils.Num(d > 0 ? Math.Floor(d) : Math.Ceiling(d));

                case OpCode.REMAINDER:
                    return NumberUtils.Num((long)NumberUtils.Num(first) % (long)NumberUtils.Num(second));

                case OpCode.MODULO:
                    long xi = (long)NumberUtils.Num(first);
                    long yi = (long)NumberUtils.Num(second);
                    long m = xi % yi;
                    return NumberUtils.Num(xi * yi > 0 || m == 0 ? m : m + yi);

                case OpCode.ABS:
                    return NumberUtils.Num(Math.Abs(NumberUtils.Num(first)));

                case OpCode.FLOOR:
                    return NumberUtils.Num(Math.Floor(NumberUtils.Num(first)));

                case OpCode.CEILING:
                    return NumberUtils.Num(Math.Ceiling(NumberUtils.Num(first)));

                case OpCode.TRUNCATE:
                    d = NumberUtils.Num(first);
                    return NumberUtils.Num(d < 0.0D ? Math.Ceiling(d) : Math.Floor(d));

                case OpCode.ROUND:
                    return NumberUtils.Num(Math.Round(NumberUtils.Num(first)));

                case OpCode.EXP:
                    return NumberUtils.Num(Math.Exp(NumberUtils.Num(first)));

                case OpCode.LOG:
                    return NumberUtils.Num(Math.Log(NumberUtils.Num(first)));

                case OpCode.SIN:
                    return NumberUtils.Num(Math.Sin(NumberUtils.Num(first)));

                case OpCode.COS:
                    return NumberUtils.Num(Math.Cos(NumberUtils.Num(first)));

                case OpCode.TAN:
                    return NumberUtils.Num(Math.Tan(NumberUtils.Num(first)));

                case OpCode.ASIN:
                    return NumberUtils.Num(Math.Asin(NumberUtils.Num(first)));

                case OpCode.ACOS:
                    return NumberUtils.Num(Math.Acos(NumberUtils.Num(first)));

                case OpCode.ATAN:
                    return NumberUtils.Num(Math.Atan(NumberUtils.Num(first)));

                case OpCode.SQRT:
                    return NumberUtils.Num(Math.Sqrt(NumberUtils.Num(first)));

                case OpCode.EXPT:
                    if (NumberUtils.Num(first) == 0.0 && NumberUtils.Num(second) < 0.0)
                    {
                        // Math.Pow gives infinity for this case
                        return NumberUtils.Num(0.0);
                    }

                    return NumberUtils.Num(Math.Pow(NumberUtils.Num(first), NumberUtils.Num(second)));

                case OpCode.NUMBERTOSTRING:
                    return NumberUtils.NumberToString(first, second);

                case OpCode.STRINGTONUMBER:
                    return SchemeString.StringToNumber(first, second);

                case OpCode.GCD:
                    return args == null ? Zero : NumberUtils.Gcd(args);

                case OpCode.LCM:
                    return args == null ? One : NumberUtils.Lcm(args);

                    // 6.6 CHARACTERS
                case OpCode.CHARQ:
                    return Truth(first is char);

                case OpCode.CHARALPHABETICQ:
                    return Truth(char.IsLetter(Chr(first)));

                case OpCode.CHARNUMERICQ:
                    return Truth(char.IsDigit(Chr(first)));

                case OpCode.CHARWHITESPACEQ:
                    return Truth(char.IsWhiteSpace(Chr(first)));

                case OpCode.CHARUPPERCASEQ:
                    return Truth(char.IsUpper(Chr(first)));

                case OpCode.CHARLOWERCASEQ:
                    return Truth(char.IsLower(Chr(first)));

                case OpCode.CHARTOINTEGER:
                    return (double)Chr(first);

                case OpCode.INTEGERTOCHAR:
                    return Chr((char)(int)NumberUtils.Num(first));

                case OpCode.CHARUPCASE:
                    return Chr(char.ToUpper(Chr(first)));

                case OpCode.CHARDOWNCASE:
                    return Chr(char.ToLower(Chr(first)));

                case OpCode.CHARCMPEQ:
                    return Truth(CharCompare(first, second, false) == 0);

                case OpCode.CHARCMPLT:
                    return Truth(CharCompare(first, second, false) < 0);

                case OpCode.CHARCMPGT:
                    return Truth(CharCompare(first, second, false) > 0);

                case OpCode.CHARCMPGE:
                    return Truth(CharCompare(first, second, false) >= 0);

                case OpCode.CHARCMPLE:
                    return Truth(CharCompare(first, second, false) <= 0);

                case OpCode.CHARCICMPEQ:
                    return Truth(CharCompare(first, second, true) == 0);

                case OpCode.CHARCICMPLT:
                    return Truth(CharCompare(first, second, true) < 0);

                case OpCode.CHARCICMPGT:
                    return Truth(CharCompare(first, second, true) > 0);

                case OpCode.CHARCICMPGE:
                    return Truth(CharCompare(first, second, true) >= 0);

                case OpCode.CHARCICMPLE:
                    return Truth(CharCompare(first, second, true) <= 0);

                case OpCode.ERROR:
                    return Error(SchemeString.AsString(args));

                    // 6.7 STRINGS
                case OpCode.STRINGQ:
                    return Truth(first is SchemeString);

                case OpCode.MAKESTRING:
                    return new SchemeString(first, second);

                case OpCode.STRING:
                    return SchemeString.ListToString(args);

                case OpCode.STRINGLENGTH:
                    return NumberUtils.Num(SchemeString.Str(first).Length);

                case OpCode.STRINGREF:
                    return Chr(SchemeString.Str(first)[(int)NumberUtils.Num(second)]);

                case OpCode.STRINGSET:
                    object z = Third(args);
                    SchemeString.Str(first)[(int)NumberUtils.Num(second)] = Chr(z);
                    return z;

                case OpCode.SUBSTRING:
                    int start = (int)NumberUtils.Num(second);
                    int end = (int)NumberUtils.Num(Third(args));
                    return SchemeString.Str(first).Substring(start, end - start);

                case OpCode.STRINGAPPEND:
                    return SchemeString.StringAppend(args);

                case OpCode.STRINGTOLIST:
                    return SchemeString.StringToList(first);

                case OpCode.LISTTOSTRING:
                    return SchemeString.ListToString(first);

                case OpCode.STRINGCMPEQ:
                    return Truth(SchemeString.StringCompare(first, second, false) == 0);

                case OpCode.STRINGCMPLT:
                    return Truth(SchemeString.StringCompare(first, second, false) < 0);

                case OpCode.STRINGCMPGT:
                    return Truth(SchemeString.StringCompare(first, second, false) > 0);

                case OpCode.STRINGCMPGE:
                    return Truth(SchemeString.StringCompare(first, second, false) >= 0);

                case OpCode.STRINGCMPLE:
                    return Truth(SchemeString.StringCompare(first, second, false) <= 0);

                case OpCode.STRINGCICMPEQ:
                    return Truth(SchemeString.StringCompare(first, second, true) == 0);

                case OpCode.STRINGCICMPLT:
                    return Truth(SchemeString.StringCompare(first, second, true) < 0);

                case OpCode.STRINGCICMPGT:
                    return Truth(SchemeString.StringCompare(first, second, true) > 0);

                case OpCode.STRINGCICMPGE:
                    return Truth(SchemeString.StringCompare(first, second, true) >= 0);

                case OpCode.STRINGCICMPLE:
                    return Truth(SchemeString.StringCompare(first, second, true) <= 0);

                    // 6.8 VECTORS
                case OpCode.VECTORQ:
                    return Truth(first is Vector);

                case OpCode.MAKEVECTOR:
                    return new Vector(first, second);

                case OpCode.VECTOR:
                    return new Vector(args);

                case OpCode.VECTORLENGTH:
                    return NumberUtils.Num(Vector.Vec(first).Length);

                case OpCode.VECTORREF:
                    return Vector.Vec(first)[(int)NumberUtils.Num(second)];

                case OpCode.VECTORSET:
                    return Vector.Vec(first)[(int)NumberUtils.Num(second)] = Third(args);

                case OpCode.VECTORTOLIST:
                    return Vector.VectorToList(first);

                case OpCode.LISTTOVECTOR:
                    return new Vector(first);

                    // 6.9 CONTROL FEATURES
                case OpCode.EVAL:
                    // Instead of returning a value, return an evaulator that can be run to get the value
                    return Stepper.CallEvaluate(first, parent.Env, parent);

                case OpCode.FORCE:
                    return !(first is Procedure) ? first : Proc(first).Apply(parent, null);

                case OpCode.PROCEDUREQ:
                    return Truth(first is Procedure);

                case OpCode.APPLY:
                    return Proc(first).Apply(parent, ListStar(Rest(args)));

                case OpCode.MAP:
                    return parent.CallMap(Rest(args), Proc(first), List(null));

                case OpCode.FOREACH:
                    return parent.CallMap(Rest(args), Proc(first), null);

                case OpCode.CALLCC:
                    return Proc(first).Apply(
                        parent,
                        List(new Continuation(parent.CallContinuation(first))));

                    // 6.10 INPUT AND OUTPUT
                case OpCode.EOFOBJECTQ:
                    return Truth(InputPort.IsEOF(first));

                case OpCode.INPUTPORTQ:
                    return Truth(first is InputPort);

                case OpCode.CURRENTINPUTPORT:
                    return interp.Input;

                case OpCode.OPENINPUTFILE:
                    return EvaluateCallWithInputFile.OpenInputFile(first);

                case OpCode.CLOSEINPUTPORT:
                    return InputPort.InPort(first, interp).Close();

                case OpCode.OUTPUTPORTQ:
                    return Truth(first is OutputPort);

                case OpCode.CURRENTOUTPUTPORT:
                    return interp.Output;

                case OpCode.OPENOUTPUTFILE:
                    return EvaluateCallWithOutputFile.OpenOutputFile(first);

                case OpCode.CALLWITHOUTPUTFILE:
                    return parent.CallWithOutputFile(args);

                case OpCode.CALLWITHINPUTFILE:
                    return parent.CallWithInputFile(args);

                case OpCode.CLOSEOUTPUTPORT:
                    OutputPort.OutPort(first, interp).Close();
                    return True;

                case OpCode.READCHAR:
                    return InputPort.InPort(first, interp).ReadChar();

                case OpCode.PEEKCHAR:
                    return InputPort.InPort(first, interp).PeekChar();

                case OpCode.LOAD:
                    return interp.LoadFile(first);

                case OpCode.READ:
                    return InputPort.InPort(first, interp).Read();

                case OpCode.EOF_OBJECT:
                    return Truth(InputPort.IsEOF(first));

                case OpCode.WRITE:
                    return OutputPort.Write(first, OutputPort.OutPort(second, interp), true);

                case OpCode.P:
                    return OutputPort.P(first);

                case OpCode.DISPLAY:
                    return OutputPort.Write(first, OutputPort.OutPort(second, interp), false);

                case OpCode.NEWLINE:
                    OutputPort.OutPort(first, interp).Println();
                    OutputPort.OutPort(first, interp).Flush();
                    return True;

                    // EXTENSIONS
                case OpCode.CLASS:
                    try
                    {
                        return Type.GetType(SchemeString.AsString(first, false));
                    }
                    catch (TypeLoadException)
                    {
                    }

                    return False;

                case OpCode.NEW:
                    try
                    {
                        return ClrProcedure.CreateInstance(first);
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

                case OpCode.METHODSYNC:
                    return new SynchronousClrProcedure(first, SchemeString.AsString(second, false), Rest(Rest(args)));

                case OpCode.METHODASYNC:
                    return new AsynchronousClrProcedure(first, SchemeString.AsString(second, false), Rest(Rest(args)));

                case OpCode.EXIT:
                    System.Environment.Exit(first == null ? 0 : (int)NumberUtils.Num(first));
                    return False; // required by style cop -- unnecessary

                case OpCode.TIMECALL:
                    return parent.CallTimeCall(args);

                default:
                    return Error("Internal error: unknown primitive: " + this +
                                 " applied to " + args);
            }
        }
    }
}