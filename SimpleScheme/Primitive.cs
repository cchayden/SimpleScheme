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
        /// ReSharper disable InconsistentNaming
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
            int numArgs = List.Length(args);
            if (numArgs < this.minArgs)
            {
                return ErrorHandlers.Error("Primitive: too few args, " + numArgs + ", for " +
                             this.Name + ": " + args);
            }

            if (numArgs > this.maxArgs)
            {
                return ErrorHandlers.Error("Primitive: too many args, " + numArgs + ", for " +
                             this.Name + ": " + args);
            }

            object first = List.First(args);
            object second = List.Second(args);
            Interpreter interp = parent.Env.Interp;

            switch (this.operCode)
            {
                    // 6.1 BOOLEANS
                case OpCode.NOT:
                    return SchemeBoolean.Truth(first is bool && (bool)first == false);

                case OpCode.BOOLEANQ:
                    return SchemeBoolean.Truth(first is bool);

                case OpCode.EQVQ:
                    return SchemeBoolean.Truth(SchemeBoolean.Eqv(first, second));

                case OpCode.EQQ:
                    // return Truth(x == y);
                    return SchemeBoolean.Truth(SchemeBoolean.Eqv(first, second));

                case OpCode.EQUALQ:
                    // return Truth(x.Equals(y));
                    return SchemeBoolean.Truth(SchemeBoolean.Equal(first, second));

                    // 6.2 EQUIVALENCE PREDICATES
                case OpCode.PAIRQ:
                    return SchemeBoolean.Truth(first is Pair);

                case OpCode.LISTQ:
                    return SchemeBoolean.Truth(List.IsList(first));

                case OpCode.CXR:
                    for (int i = this.Name.Length - 2; i >= 1; i--)
                    {
                        first = this.Name[i] == 'a' ? List.First(first) : List.Rest(first);
                    }

                    return first;

                case OpCode.CONS:
                    return List.Cons(first, second);

                case OpCode.CAR:
                    return List.First(first);

                case OpCode.CDR:
                    return List.Rest(first);

                case OpCode.SETCAR:
                    return List.SetFirst(first, second);

                case OpCode.SETCDR:
                    return List.SetRest(first, second);

                case OpCode.SECOND:
                    return List.Second(first);

                case OpCode.THIRD:
                    return List.Third(first);

                case OpCode.NULLQ:
                    return SchemeBoolean.Truth(first == null);

                case OpCode.LIST:
                    return args;

                case OpCode.LENGTH:
                    return Number.Num(List.Length(first));

                case OpCode.APPEND:
                    return args == null ? null : List.Append(args);

                case OpCode.REVERSE:
                    return List.Reverse(first);

                case OpCode.LISTTAIL:
                    for (int k = (int)Number.Num(second); k > 0; k--)
                    {
                        first = List.Rest(first);
                    }

                    return first;

                case OpCode.LISTREF:
                    for (int k = (int)Number.Num(second); k > 0; k--)
                    {
                        first = List.Rest(first);
                    }

                    return List.First(first);

                case OpCode.MEMQ:
                    return List.MemberAssoc(first, second, 'm', 'q');

                case OpCode.MEMV:
                    return List.MemberAssoc(first, second, 'm', 'v');

                case OpCode.MEMBER:
                    return List.MemberAssoc(first, second, 'm', ' ');

                case OpCode.ASSQ:
                    return List.MemberAssoc(first, second, 'a', 'q');

                case OpCode.ASSV:
                    return List.MemberAssoc(first, second, 'a', 'v');

                case OpCode.ASSOC:
                    return List.MemberAssoc(first, second, 'a', ' ');

                    // 6.4 SYMBOLS
                case OpCode.SYMBOLQ:
                    return SchemeBoolean.Truth(first is string);

                case OpCode.SYMBOLTOSTRING:
                    return new SchemeString(SchemeString.Sym(first));

                case OpCode.STRINGTOSYMBOL:
                    return string.Intern(SchemeString.Str(first).AsString());

                    // 6.5 NUMBERS
                case OpCode.NUMBERQ:
                    return SchemeBoolean.Truth(first is byte || first is int || first is long || first is float || first is double);

                case OpCode.ODDQ:
                    return SchemeBoolean.Truth(Math.Abs(Number.Num(first)) % 2 != 0);

                case OpCode.EVENQ:
                    return SchemeBoolean.Truth(Math.Abs(Number.Num(first)) % 2 == 0);

                case OpCode.ZEROQ:
                    return SchemeBoolean.Truth(Number.Num(first) == 0);

                case OpCode.POSITIVEQ:
                    return SchemeBoolean.Truth(Number.Num(first) > 0);

                case OpCode.NEGATIVEQ:
                    return SchemeBoolean.Truth(Number.Num(first) < 0);

                case OpCode.INTEGERQ:
                    return SchemeBoolean.Truth(Number.IsExact(first));

                case OpCode.INEXACTQ:
                    return SchemeBoolean.Truth(!Number.IsExact(first));

                case OpCode.LT:
                    return Number.NumCompare(args, '<');

                case OpCode.GT:
                    return Number.NumCompare(args, '>');

                case OpCode.EQ:
                    return Number.NumCompare(args, '=');

                case OpCode.LE:
                    return Number.NumCompare(args, 'L');

                case OpCode.GE:
                    return Number.NumCompare(args, 'G');

                case OpCode.MAX:
                    return Number.NumCompute(args, 'X', Number.Num(first));

                case OpCode.MIN:
                    return Number.NumCompute(args, 'N', Number.Num(first));

                case OpCode.PLUS:
                    return Number.NumCompute(args, '+', 0.0);

                case OpCode.MINUS:
                    return Number.NumCompute(List.Rest(args), '-', Number.Num(first));

                case OpCode.TIMES:
                    return Number.NumCompute(args, '*', 1.0);

                case OpCode.DIVIDE:
                    return Number.NumCompute(List.Rest(args), '/', Number.Num(first));

                case OpCode.QUOTIENT:
                    double d = Number.Num(first) / Number.Num(second);
                    return Number.Num(d > 0 ? Math.Floor(d) : Math.Ceiling(d));

                case OpCode.REMAINDER:
                    return Number.Num((long)Number.Num(first) % (long)Number.Num(second));

                case OpCode.MODULO:
                    long xi = (long)Number.Num(first);
                    long yi = (long)Number.Num(second);
                    long m = xi % yi;
                    return Number.Num(xi * yi > 0 || m == 0 ? m : m + yi);

                case OpCode.ABS:
                    return Number.Num(Math.Abs(Number.Num(first)));

                case OpCode.FLOOR:
                    return Number.Num(Math.Floor(Number.Num(first)));

                case OpCode.CEILING:
                    return Number.Num(Math.Ceiling(Number.Num(first)));

                case OpCode.TRUNCATE:
                    d = Number.Num(first);
                    return Number.Num(d < 0.0D ? Math.Ceiling(d) : Math.Floor(d));

                case OpCode.ROUND:
                    return Number.Num(Math.Round(Number.Num(first)));

                case OpCode.EXP:
                    return Number.Num(Math.Exp(Number.Num(first)));

                case OpCode.LOG:
                    return Number.Num(Math.Log(Number.Num(first)));

                case OpCode.SIN:
                    return Number.Num(Math.Sin(Number.Num(first)));

                case OpCode.COS:
                    return Number.Num(Math.Cos(Number.Num(first)));

                case OpCode.TAN:
                    return Number.Num(Math.Tan(Number.Num(first)));

                case OpCode.ASIN:
                    return Number.Num(Math.Asin(Number.Num(first)));

                case OpCode.ACOS:
                    return Number.Num(Math.Acos(Number.Num(first)));

                case OpCode.ATAN:
                    return Number.Num(Math.Atan(Number.Num(first)));

                case OpCode.SQRT:
                    return Number.Num(Math.Sqrt(Number.Num(first)));

                case OpCode.EXPT:
                    if (Number.Num(first) == 0.0 && Number.Num(second) < 0.0)
                    {
                        // Math.Pow gives infinity for this case
                        return Number.Num(0.0);
                    }

                    return Number.Num(Math.Pow(Number.Num(first), Number.Num(second)));

                case OpCode.NUMBERTOSTRING:
                    return Number.NumberToString(first, second);

                case OpCode.STRINGTONUMBER:
                    return SchemeString.StringToNumber(first, second);

                case OpCode.GCD:
                    return args == null ? Number.Zero : Number.Gcd(args);

                case OpCode.LCM:
                    return args == null ? Number.One : Number.Lcm(args);

                    // 6.6 CHARACTERS
                case OpCode.CHARQ:
                    return SchemeBoolean.Truth(first is char);

                case OpCode.CHARALPHABETICQ:
                    return SchemeBoolean.Truth(char.IsLetter(SchemeString.Chr(first)));

                case OpCode.CHARNUMERICQ:
                    return SchemeBoolean.Truth(char.IsDigit(SchemeString.Chr(first)));

                case OpCode.CHARWHITESPACEQ:
                    return SchemeBoolean.Truth(char.IsWhiteSpace(SchemeString.Chr(first)));

                case OpCode.CHARUPPERCASEQ:
                    return SchemeBoolean.Truth(char.IsUpper(SchemeString.Chr(first)));

                case OpCode.CHARLOWERCASEQ:
                    return SchemeBoolean.Truth(char.IsLower(SchemeString.Chr(first)));

                case OpCode.CHARTOINTEGER:
                    return (double)SchemeString.Chr(first);

                case OpCode.INTEGERTOCHAR:
                    return SchemeString.Chr((char)(int)Number.Num(first));

                case OpCode.CHARUPCASE:
                    return SchemeString.Chr(char.ToUpper(SchemeString.Chr(first)));

                case OpCode.CHARDOWNCASE:
                    return SchemeString.Chr(char.ToLower(SchemeString.Chr(first)));

                case OpCode.CHARCMPEQ:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, false) == 0);

                case OpCode.CHARCMPLT:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, false) < 0);

                case OpCode.CHARCMPGT:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, false) > 0);

                case OpCode.CHARCMPGE:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, false) >= 0);

                case OpCode.CHARCMPLE:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, false) <= 0);

                case OpCode.CHARCICMPEQ:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, true) == 0);

                case OpCode.CHARCICMPLT:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, true) < 0);

                case OpCode.CHARCICMPGT:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, true) > 0);

                case OpCode.CHARCICMPGE:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, true) >= 0);

                case OpCode.CHARCICMPLE:
                    return SchemeBoolean.Truth(SchemeString.ChrCompare(first, second, true) <= 0);

                case OpCode.ERROR:
                    return ErrorHandlers.Error(SchemeString.AsString(args));

                    // 6.7 STRINGS
                case OpCode.STRINGQ:
                    return SchemeBoolean.Truth(first is SchemeString);

                case OpCode.MAKESTRING:
                    return new SchemeString(first, second);

                case OpCode.STRING:
                    return SchemeString.ListToString(args);

                case OpCode.STRINGLENGTH:
                    return Number.Num(SchemeString.Str(first).Length);

                case OpCode.STRINGREF:
                    return SchemeString.Chr(SchemeString.Str(first)[(int)Number.Num(second)]);

                case OpCode.STRINGSET:
                    object z = List.Third(args);
                    SchemeString.Str(first)[(int)Number.Num(second)] = SchemeString.Chr(z);
                    return z;

                case OpCode.SUBSTRING:
                    int start = (int)Number.Num(second);
                    int end = (int)Number.Num(List.Third(args));
                    return SchemeString.Str(first).Substring(start, end - start);

                case OpCode.STRINGAPPEND:
                    return SchemeString.StringAppend(args);

                case OpCode.STRINGTOLIST:
                    return SchemeString.StringToList(first);

                case OpCode.LISTTOSTRING:
                    return SchemeString.ListToString(first);

                case OpCode.STRINGCMPEQ:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, false) == 0);

                case OpCode.STRINGCMPLT:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, false) < 0);

                case OpCode.STRINGCMPGT:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, false) > 0);

                case OpCode.STRINGCMPGE:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, false) >= 0);

                case OpCode.STRINGCMPLE:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, false) <= 0);

                case OpCode.STRINGCICMPEQ:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, true) == 0);

                case OpCode.STRINGCICMPLT:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, true) < 0);

                case OpCode.STRINGCICMPGT:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, true) > 0);

                case OpCode.STRINGCICMPGE:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, true) >= 0);

                case OpCode.STRINGCICMPLE:
                    return SchemeBoolean.Truth(SchemeString.StringCompare(first, second, true) <= 0);

                    // 6.8 VECTORS
                case OpCode.VECTORQ:
                    return SchemeBoolean.Truth(first is Vector);

                case OpCode.MAKEVECTOR:
                    return new Vector(first, second);

                case OpCode.VECTOR:
                    return new Vector(args);

                case OpCode.VECTORLENGTH:
                    return Number.Num(Vector.Vec(first).Length);

                case OpCode.VECTORREF:
                    return Vector.Vec(first)[(int)Number.Num(second)];

                case OpCode.VECTORSET:
                    return Vector.Vec(first)[(int)Number.Num(second)] = List.Third(args);

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
                    return SchemeBoolean.Truth(first is Procedure);

                case OpCode.APPLY:
                    return Proc(first).Apply(parent, List.ListStar(List.Rest(args)));

                case OpCode.MAP:
                    return parent.CallMap(List.Rest(args), Proc(first), List.MakeList(null));

                case OpCode.FOREACH:
                    return parent.CallMap(List.Rest(args), Proc(first), null);

                case OpCode.CALLCC:
                    return Proc(first).Apply(
                        parent,
                        List.MakeList(new Continuation(parent.CallContinuation(first))));

                    // 6.10 INPUT AND OUTPUT
                case OpCode.EOFOBJECTQ:
                    return SchemeBoolean.Truth(InputPort.IsEOF(first));

                case OpCode.INPUTPORTQ:
                    return SchemeBoolean.Truth(first is InputPort);

                case OpCode.CURRENTINPUTPORT:
                    return interp.Input;

                case OpCode.OPENINPUTFILE:
                    return EvaluateCallWithInputFile.OpenInputFile(first);

                case OpCode.CLOSEINPUTPORT:
                    return InputPort.InPort(first, interp).Close();

                case OpCode.OUTPUTPORTQ:
                    return SchemeBoolean.Truth(first is OutputPort);

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
                    return SchemeBoolean.True;

                case OpCode.READCHAR:
                    return InputPort.InPort(first, interp).ReadChar();

                case OpCode.PEEKCHAR:
                    return InputPort.InPort(first, interp).PeekChar();

                case OpCode.LOAD:
                    return interp.LoadFile(first);

                case OpCode.READ:
                    return InputPort.InPort(first, interp).Read();

                case OpCode.EOF_OBJECT:
                    return SchemeBoolean.Truth(InputPort.IsEOF(first));

                case OpCode.WRITE:
                    return OutputPort.Write(first, OutputPort.OutPort(second, interp), true);

                case OpCode.P:
                    return OutputPort.P(first);

                case OpCode.DISPLAY:
                    return OutputPort.Write(first, OutputPort.OutPort(second, interp), false);

                case OpCode.NEWLINE:
                    OutputPort.OutPort(first, interp).Println();
                    OutputPort.OutPort(first, interp).Flush();
                    return SchemeBoolean.True;

                    // EXTENSIONS
                case OpCode.CLASS:
                    try
                    {
                        return Type.GetType(SchemeString.AsString(first, false));
                    }
                    catch (TypeLoadException)
                    {
                    }

                    return SchemeBoolean.False;

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

                    return SchemeBoolean.False;

                case OpCode.METHODSYNC:
                    return new SynchronousClrProcedure(first, SchemeString.AsString(second, false), List.Rest(List.Rest(args)));

                case OpCode.METHODASYNC:
                    return new AsynchronousClrProcedure(first, SchemeString.AsString(second, false), List.Rest(List.Rest(args)));

                case OpCode.EXIT:
                    System.Environment.Exit(first == null ? 0 : (int)Number.Num(first));
                    return SchemeBoolean.False; // required by style cop -- unnecessary

                case OpCode.TIMECALL:
                    return parent.CallTimeCall(args);

                default:
                    return ErrorHandlers.Error("Internal error: unknown primitive: " + this +
                                 " applied to " + args);
            }
        }
    }

    /// <summary>
    /// This part of the environment is here to keep the primitive stuff in one file.
    /// </summary>
    [SuppressMessage("Microsoft.StyleCop.CSharp.MaintainabilityRules", "SA1402:FileMayOnlyContainASingleClass",
        Justification = "Reviewed. Suppression is OK here.")]
    public partial class Environment
    {
        /// <summary>
        /// Install primitives into the environment.
        /// </summary>
        /// <returns>The environment.</returns>
        public Environment InstallPrimitives()
        {
            const int MaxInt = int.MaxValue;

            this
                .DefinePrimitive("=", Primitive.OpCode.EQ, 2, MaxInt)
                .DefinePrimitive("*", Primitive.OpCode.TIMES, 0, MaxInt)
                .DefinePrimitive("+", Primitive.OpCode.PLUS, 0, MaxInt)
                .DefinePrimitive("-", Primitive.OpCode.MINUS, 1, MaxInt)
                .DefinePrimitive("/", Primitive.OpCode.DIVIDE, 1, MaxInt)
                .DefinePrimitive("<", Primitive.OpCode.LT, 2, MaxInt)
                .DefinePrimitive(">", Primitive.OpCode.GT, 2, MaxInt)
                .DefinePrimitive("<=", Primitive.OpCode.LE, 2, MaxInt)
                .DefinePrimitive(">=", Primitive.OpCode.GE, 2, MaxInt)
                .DefinePrimitive("abs", Primitive.OpCode.ABS, 1)
                .DefinePrimitive("acos", Primitive.OpCode.ACOS, 1)
                .DefinePrimitive("append", Primitive.OpCode.APPEND, 0, MaxInt)
                .DefinePrimitive("apply", Primitive.OpCode.APPLY, 2, MaxInt)
                .DefinePrimitive("asin", Primitive.OpCode.ASIN, 1)
                .DefinePrimitive("assoc", Primitive.OpCode.ASSOC, 2)
                .DefinePrimitive("assq", Primitive.OpCode.ASSQ, 2)
                .DefinePrimitive("assv", Primitive.OpCode.ASSV, 2)
                .DefinePrimitive("atan", Primitive.OpCode.ATAN, 1)
                .DefinePrimitive("boolean?", Primitive.OpCode.BOOLEANQ, 1)
                .DefinePrimitive("caaaar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("caaadr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("caaar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("caadar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("caaddr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("caar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cadaar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cadadr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cadar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("caddar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cadddr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("caddr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cadr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("call-with-current-continuation", Primitive.OpCode.CALLCC, 1)
                .DefinePrimitive("call/cc", Primitive.OpCode.CALLCC, 1)
                .DefinePrimitive("call-with-input-file", Primitive.OpCode.CALLWITHINPUTFILE, 2)
                .DefinePrimitive("call-with-output-file", Primitive.OpCode.CALLWITHOUTPUTFILE, 2)
                .DefinePrimitive("car", Primitive.OpCode.CAR, 1)
                .DefinePrimitive("first", Primitive.OpCode.CAR, 1)
                .DefinePrimitive("second", Primitive.OpCode.SECOND, 1)
                .DefinePrimitive("third", Primitive.OpCode.THIRD, 1)
                .DefinePrimitive("cdaaar,", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdaadr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdaar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdadar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdaddr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdadr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cddaar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cddadr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cddar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdddar", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cddddr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdddr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cddr", Primitive.OpCode.CXR, 1)
                .DefinePrimitive("cdr", Primitive.OpCode.CDR, 1)
                .DefinePrimitive("rest", Primitive.OpCode.CDR, 1)
                .DefinePrimitive("char->integer", Primitive.OpCode.CHARTOINTEGER, 1)
                .DefinePrimitive("char-alphabetic?", Primitive.OpCode.CHARALPHABETICQ, 1)
                .DefinePrimitive("char-ci<=?", Primitive.OpCode.CHARCICMPLE, 2)
                .DefinePrimitive("char-ci<?", Primitive.OpCode.CHARCICMPLT, 2)
                .DefinePrimitive("char-ci=?", Primitive.OpCode.CHARCICMPEQ, 2)
                .DefinePrimitive("char-ci>=?", Primitive.OpCode.CHARCICMPGE, 2)
                .DefinePrimitive("char-ci>?", Primitive.OpCode.CHARCICMPGT, 2)
                .DefinePrimitive("char-downcase", Primitive.OpCode.CHARDOWNCASE, 1)
                .DefinePrimitive("char-lower-case?", Primitive.OpCode.CHARLOWERCASEQ, 1)
                .DefinePrimitive("char-numeric?", Primitive.OpCode.CHARNUMERICQ, 1)
                .DefinePrimitive("char-upcase", Primitive.OpCode.CHARUPCASE, 1)
                .DefinePrimitive("char-upper-case?", Primitive.OpCode.CHARUPPERCASEQ, 1)
                .DefinePrimitive("char-whitespace?", Primitive.OpCode.CHARWHITESPACEQ, 1)
                .DefinePrimitive("char<=?", Primitive.OpCode.CHARCMPLE, 2)
                .DefinePrimitive("char<?", Primitive.OpCode.CHARCMPLT, 2)
                .DefinePrimitive("char=?", Primitive.OpCode.CHARCMPEQ, 2)
                .DefinePrimitive("char>=?", Primitive.OpCode.CHARCMPGE, 2)
                .DefinePrimitive("char>?", Primitive.OpCode.CHARCMPGT, 2)
                .DefinePrimitive("char?", Primitive.OpCode.CHARQ, 1)
                .DefinePrimitive("close-input-port", Primitive.OpCode.CLOSEINPUTPORT, 1)
                .DefinePrimitive("close-output-port", Primitive.OpCode.CLOSEOUTPUTPORT, 1)
                .DefinePrimitive("complex", Primitive.OpCode.NUMBERQ, 1)
                .DefinePrimitive("cons", Primitive.OpCode.CONS, 2)
                .DefinePrimitive("cos", Primitive.OpCode.COS, 1)
                .DefinePrimitive("current-input-port", Primitive.OpCode.CURRENTINPUTPORT, 0)
                .DefinePrimitive("current-output-port", Primitive.OpCode.CURRENTOUTPUTPORT, 0)
                .DefinePrimitive("display", Primitive.OpCode.DISPLAY, 1, 2)
                .DefinePrimitive("eof-object?", Primitive.OpCode.EOFOBJECTQ, 1)
                .DefinePrimitive("eq?", Primitive.OpCode.EQQ, 2)
                .DefinePrimitive("equal?", Primitive.OpCode.EQUALQ, 2)
                .DefinePrimitive("eqv?", Primitive.OpCode.EQVQ, 2)
                .DefinePrimitive("eval", Primitive.OpCode.EVAL, 1, 2)
                .DefinePrimitive("even?", Primitive.OpCode.EVENQ, 1)
                .DefinePrimitive("exact?", Primitive.OpCode.INTEGERQ, 1)
                .DefinePrimitive("exp", Primitive.OpCode.EXP, 1)
                .DefinePrimitive("expt", Primitive.OpCode.EXPT, 2)
                .DefinePrimitive("force", Primitive.OpCode.FORCE, 1)
                .DefinePrimitive("for-each", Primitive.OpCode.FOREACH, 1, MaxInt)
                .DefinePrimitive("gcd", Primitive.OpCode.GCD, 0, MaxInt)
                .DefinePrimitive("inexact?", Primitive.OpCode.INEXACTQ, 1)
                .DefinePrimitive("input-port?", Primitive.OpCode.INPUTPORTQ, 1)
                .DefinePrimitive("integer->char", Primitive.OpCode.INTEGERTOCHAR, 1)
                .DefinePrimitive("integer?", Primitive.OpCode.INTEGERQ, 1)
                .DefinePrimitive("lcm", Primitive.OpCode.LCM, 0, MaxInt)
                .DefinePrimitive("length", Primitive.OpCode.LENGTH, 1)
                .DefinePrimitive("list", Primitive.OpCode.LIST, 0, MaxInt)
                .DefinePrimitive("list->string", Primitive.OpCode.LISTTOSTRING, 1)
                .DefinePrimitive("list->vector", Primitive.OpCode.LISTTOVECTOR, 1)
                .DefinePrimitive("list-ref", Primitive.OpCode.LISTREF, 2)
                .DefinePrimitive("list-tail", Primitive.OpCode.LISTTAIL, 2)
                .DefinePrimitive("list?", Primitive.OpCode.LISTQ, 1)
                .DefinePrimitive("load", Primitive.OpCode.LOAD, 1)
                .DefinePrimitive("log", Primitive.OpCode.LOG, 1)
                .DefinePrimitive("make-string", Primitive.OpCode.MAKESTRING, 1, 2)
                .DefinePrimitive("make-vector", Primitive.OpCode.MAKEVECTOR, 1, 2)
                .DefinePrimitive("map", Primitive.OpCode.MAP, 1, MaxInt)
                .DefinePrimitive("max", Primitive.OpCode.MAX, 1, MaxInt)
                .DefinePrimitive("member", Primitive.OpCode.MEMBER, 2)
                .DefinePrimitive("memq", Primitive.OpCode.MEMQ, 2)
                .DefinePrimitive("memv", Primitive.OpCode.MEMV, 2)
                .DefinePrimitive("min", Primitive.OpCode.MIN, 1, MaxInt)
                .DefinePrimitive("modulo", Primitive.OpCode.MODULO, 2)
                .DefinePrimitive("negative?", Primitive.OpCode.NEGATIVEQ, 1)
                .DefinePrimitive("newline", Primitive.OpCode.NEWLINE, 0, 1)
                .DefinePrimitive("not", Primitive.OpCode.NOT, 1)
                .DefinePrimitive("null?", Primitive.OpCode.NULLQ, 1)
                .DefinePrimitive("number->string", Primitive.OpCode.NUMBERTOSTRING, 1, 2)
                .DefinePrimitive("number?", Primitive.OpCode.NUMBERQ, 1)
                .DefinePrimitive("odd?", Primitive.OpCode.ODDQ, 1)
                .DefinePrimitive("open-input-file", Primitive.OpCode.OPENINPUTFILE, 1)
                .DefinePrimitive("open-output-file", Primitive.OpCode.OPENOUTPUTFILE, 1)
                .DefinePrimitive("output-port?", Primitive.OpCode.OUTPUTPORTQ, 1)
                .DefinePrimitive("pair?", Primitive.OpCode.PAIRQ, 1)
                .DefinePrimitive("peek-char", Primitive.OpCode.PEEKCHAR, 0, 1)
                .DefinePrimitive("positive?", Primitive.OpCode.POSITIVEQ, 1)
                .DefinePrimitive("procedure?", Primitive.OpCode.PROCEDUREQ, 1)
                .DefinePrimitive("quotient", Primitive.OpCode.QUOTIENT, 2)
                .DefinePrimitive("rational?", Primitive.OpCode.INTEGERQ, 1)
                .DefinePrimitive("read", Primitive.OpCode.READ, 0, 1)
                .DefinePrimitive("read-char", Primitive.OpCode.READCHAR, 0, 1)
                .DefinePrimitive("real?", Primitive.OpCode.INTEGERQ, 1)
                .DefinePrimitive("remainder", Primitive.OpCode.REMAINDER, 2)
                .DefinePrimitive("reverse", Primitive.OpCode.REVERSE, 1)
                .DefinePrimitive("round", Primitive.OpCode.ROUND, 1)
                .DefinePrimitive("set-car!", Primitive.OpCode.SETCAR, 2)
                .DefinePrimitive("set-first!", Primitive.OpCode.SETCAR, 2)
                .DefinePrimitive("set-cdr!", Primitive.OpCode.SETCDR, 2)
                .DefinePrimitive("set-rest!", Primitive.OpCode.SETCDR, 2)
                .DefinePrimitive("sin", Primitive.OpCode.SIN, 1)
                .DefinePrimitive("sqrt", Primitive.OpCode.SQRT, 1)
                .DefinePrimitive("string", Primitive.OpCode.STRING, 0, MaxInt)
                .DefinePrimitive("string->list", Primitive.OpCode.STRINGTOLIST, 1)
                .DefinePrimitive("string->number", Primitive.OpCode.STRINGTONUMBER, 1, 2)
                .DefinePrimitive("string->symbol", Primitive.OpCode.STRINGTOSYMBOL, 1)
                .DefinePrimitive("string-append", Primitive.OpCode.STRINGAPPEND, 0, MaxInt)
                .DefinePrimitive("string-ci<=?", Primitive.OpCode.STRINGCICMPLE, 2)
                .DefinePrimitive("string-ci<?", Primitive.OpCode.STRINGCICMPLT, 2)
                .DefinePrimitive("string-ci=?", Primitive.OpCode.STRINGCICMPEQ, 2)
                .DefinePrimitive("string-ci>=?", Primitive.OpCode.STRINGCICMPGE, 2)
                .DefinePrimitive("string-ci>?", Primitive.OpCode.STRINGCICMPGT, 2)
                .DefinePrimitive("string-length", Primitive.OpCode.STRINGLENGTH, 1)
                .DefinePrimitive("string-ref", Primitive.OpCode.STRINGREF, 2)
                .DefinePrimitive("string-set!", Primitive.OpCode.STRINGSET, 3)
                .DefinePrimitive("string<=?", Primitive.OpCode.STRINGCMPLE, 2)
                .DefinePrimitive("string<?", Primitive.OpCode.STRINGCMPLT, 2)
                .DefinePrimitive("string=?", Primitive.OpCode.STRINGCMPEQ, 2)
                .DefinePrimitive("string>=?", Primitive.OpCode.STRINGCMPGE, 2)
                .DefinePrimitive("string>?", Primitive.OpCode.STRINGCMPGT, 2)
                .DefinePrimitive("string?", Primitive.OpCode.STRINGQ, 1)
                .DefinePrimitive("substring", Primitive.OpCode.SUBSTRING, 3)
                .DefinePrimitive("symbol->string", Primitive.OpCode.SYMBOLTOSTRING, 1)
                .DefinePrimitive("symbol?", Primitive.OpCode.SYMBOLQ, 1)
                .DefinePrimitive("tan", Primitive.OpCode.TAN, 1)
                .DefinePrimitive("vector", Primitive.OpCode.VECTOR, 0, MaxInt)
                .DefinePrimitive("vector->list", Primitive.OpCode.VECTORTOLIST, 1)
                .DefinePrimitive("vector-length", Primitive.OpCode.VECTORLENGTH, 1)
                .DefinePrimitive("vector-ref", Primitive.OpCode.VECTORREF, 2)
                .DefinePrimitive("vector-set!", Primitive.OpCode.VECTORSET, 3)
                .DefinePrimitive("vector?", Primitive.OpCode.VECTORQ, 1)
                .DefinePrimitive("write", Primitive.OpCode.WRITE, 1, 2)
                .DefinePrimitive("p", Primitive.OpCode.P, 1, 1)
                .DefinePrimitive("write-char", Primitive.OpCode.DISPLAY, 1, 2)
                .DefinePrimitive("zero?", Primitive.OpCode.ZEROQ, 1)

                // EXTENSIONS
                .DefinePrimitive("new", Primitive.OpCode.NEW, 1)
                .DefinePrimitive("class", Primitive.OpCode.CLASS, 1)
                .DefinePrimitive("method", Primitive.OpCode.METHODSYNC, 2, MaxInt)
                .DefinePrimitive("method-async", Primitive.OpCode.METHODASYNC, 2, MaxInt)
                .DefinePrimitive("exit", Primitive.OpCode.EXIT, 0, 1)
                .DefinePrimitive("error", Primitive.OpCode.ERROR, 0, MaxInt)
                .DefinePrimitive("time-call", Primitive.OpCode.TIMECALL, 1, 2);

            return this;
        }
    }
}