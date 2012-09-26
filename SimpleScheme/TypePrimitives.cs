// <copyright file="TypePrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;

    using Obj = System.Object;

    /// <summary>
    /// Generic operations on Obj, for tracing, debugging, and interfacing with the CLR.
    /// Includes a method that returns "friendly" names for objects of given types.
    /// Also includes a methat that turns a "friendly" name into a full .NET name.
    /// </summary>
    public static class TypePrimitives
    {
        /// <summary>
        /// Used to translate from full qualified type names into the friendly name.
        /// </summary>
        private readonly static Dictionary<string, string> typeTranslator;

        /// <summary>
        /// Used to translate the internal type name into the CLR type name.
        /// </summary>
        private readonly static Dictionary<string, string> clrTranslator;

        /// <summary>
        /// The string names of the enum members.
        /// </summary>
        private readonly static string[] valueTypeNames = new string[(int)ValueType.Undefined + 1];


        static TypePrimitives()
        {
            typeTranslator = new Dictionary<string, string>
                {
                    { "SimpleScheme.AsynchronousClrProcedure", "asynchronous-clr-procedure" },
                    { "SimpleScheme.Character", "character" },
                    { "SimpleScheme.ClrConstructor", "clr-constructor" },
                    { "SimpleScheme.ClrProcedure", "clr-procedure" },
                    { "SimpleScheme.Continuation", "continuation" },
                    { "SimpleScheme.EmptyList", "empty-list" },
                    { "SimpleScheme.InputPort", "input-port" },
                    { "SimpleScheme.Lambda", "lambda" },
                    { "SimpleScheme.Macro", "macro" },
                    { "SimpleScheme.Number", "number" },
                    { "SimpleScheme.OutputPort", "output-port" },
                    { "SimpleScheme.Pair", "pair" },
                    { "SimpleScheme.Primitive", "primitive" },
                    { "SimpleScheme.Procedure", "procedure" },
                    { "SimpleScheme.SchemeBoolean", "boolean" },
                    { "SimpleScheme.SchemeString", "string" },
                    { "SimpleScheme.Symbol", "symbol" },
                    { "SimpleScheme.SynchronousClrProcedure", "synchronous-clr-procedure" },
                    { "SimpleScheme.Undefined", "undefined" },
                    { "SimpleScheme.Vector", "vector" },
                    { "SimpleScheme.EndedEvaluator", "ended-evaluator" },
                    { "SimpleScheme.EvaluateAnd", "evaluate-and" },
                    { "SimpleScheme.EvaluateCallWithInputFile", "call-with-input-file" },
                    { "SimpleScheme.EvaluateCallWithOutputFile", "evaluate-call-with-output-file" },
                    { "SimpleScheme.EvaluateCase", "evaluate-case" },
                    { "SimpleScheme.EvaluateCond", "evaluate-cond" },
                    { "SimpleScheme.EvaluateDefine", "evaluate-define" },
                    { "SimpleScheme.EvaluateDo", "evaluate-do" },
                    { "SimpleScheme.EvaluateExpandMacro", "evaluate-expand-macro" },
                    { "SimpleScheme.EvaluateExpression", "evaluate-expression" },
                    { "SimpleScheme.EvaluateIf", "evaluate-if" },
                    { "SimpleScheme.EvaluateLet", "evaluate-let" },
                    { "SimpleScheme.EvaluateLetRec", "evaluate-letrec" },
                    { "SimpleScheme.EvaluateLetStar", "evaluate-let*" },
                    { "SimpleScheme.EvaluateList", "evaluate-list" },
                    { "SimpleScheme.EvaluateMap", "evaluate-map" },
                    { "SimpleScheme.EvaluateOr", "evaluate-or" },
                    { "SimpleScheme.EvaluateParallel", "evaluate-parallel" },
                    { "SimpleScheme.EvaluateProc", "evaluate-proc" },
                    { "SimpleScheme.EvaluateSequence", "evaluate-sequence" },
                    { "SimpleScheme.EvaluateSet", "evaluate-set" },
                    { "SimpleScheme.EvaluateTime", "evaluate-time" },
                    { "SimpleScheme.HaltedEvaluator", "halted-evaluator" },
                    { "SimpleScheme.SuspendedEvaluator", "suspended-evaluator" }
                };

            clrTranslator = new Dictionary<string, string>
                {
                    { "bool", "System.Boolean" },
                    { "char", "System.Char" },
                    { "string", "System.String" },
                    { "byte", "System.Byte" },
                    { "short", "System.Int16" },                   
                    { "int", "System.Int32" },                   
                    { "long", "System.Int64" },                   
                    { "float", "System.Single" },                   
                    { "double", "System.Double" },                   
                    { "object", "System.Object" },                   
                    { "bool[]", "System.Boolean[]" },                   
                    { "char[]", "System.Char[]" },                   
                    { "string[]", "System.String[]" },                    
                    { "byte[]", "System.Byte[]" },                   
                    { "short[]", "System.Int16[]" },                   
                    { "int[]", "System.Int32[]" },                   
                    { "long[]", "System.Int64[]" },                   
                    { "float[]", "System.Single[]" },                   
                    { "double[]", "System.Doubl[]e" },                   
                    { "object[]", "System.Object[]" },                   
                };

            for (var i = (int)ValueType.Obj; i < (int)ValueType.Undefined + 1; i++)
            {
                valueTypeNames[i] = ((ValueType)i).ToString();
            }
        }

        #region Enums
        /// <summary>
        /// This enum contains all the types that values can have.  It also contains
        /// a few combination types that are used in checking arguments to
        /// primitives.
        /// </summary>
        public enum ValueType
        {
            /// <summary>
            /// Any object is required.
            /// </summary>
            Obj,

            /// <summary>
            /// A pair is required..
            /// </summary>
            Pair,

            /// <summary>
            /// A pair or the empty object is required.
            /// </summary>
            PairOrEmpty,

            /// <summary>
            /// A pair or a symbol is required.
            /// </summary>
            PairOrSymbol,

            /// <summary>
            /// A number is required.
            /// </summary>
            Number,

            /// <summary>
            /// A character is required.
            /// </summary>
            Char,

            /// <summary>
            /// A string is required.
            /// </summary>
            String,

            /// <summary>
            /// A procedure is required.  This includes macros and primitives, as well
            /// as lambdas.
            /// </summary>
            Proc,

            /// <summary>
            /// A vector is required.
            /// </summary>
            Vector,

            /// <summary>
            /// A symbol is required.
            /// </summary>
            Symbol,

            /// <summary>
            /// A boolean is required.
            /// </summary>
            Boolean,

            /// <summary>
            /// A port is required.
            /// </summary>
            Port,

            //// The following are not used as primitive arguments.

            /// <summary>
            /// The empty list.
            /// </summary>
            Empty,

            /// <summary>
            /// An asynchronous clr procedure.
            /// </summary>
            AsynchronousClrProcedure,

            /// <summary>
            /// A synchronous clr procedure.
            /// </summary>
            SynchronousClrProcedure,

            /// <summary>
            /// A CLR constructor.
            /// </summary>
            ClrConstructor,

            /// <summary>
            /// A continuation.
            /// </summary>
            Continuation,

            /// <summary>
            /// A lambda.
            /// </summary>
            Lambda,

            /// <summary>
            /// A macro
            /// </summary>
            Macro,

            /// <summary>
            /// The undefined object.
            /// </summary>
            Undefined
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Find the scheme type name or the evaluator name for a given object.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>The scheme type name.</returns>
        public static string SchemeTypeName(Obj obj)
        {
            if (obj == null)
            {
                return "null";
            }

            string fullName = obj.GetType().FullName;
            if (fullName == null)
            {
                return "unknown";
            }

            string name;
            return typeTranslator.TryGetValue(fullName, out name) ? name : fullName;
        }

        /// <summary>
        /// Find the scheme type name given the type.
        /// </summary>
        /// <param name="t">The object type.</param>
        /// <returns>The scheme type name.</returns>
        public static string SchemeTypeName(Type t)
        {
            string fullName = t.FullName;
            if (fullName == null)
            {
                return "unknown";
            }

            string name;
            return typeTranslator.TryGetValue(fullName, out name) ? name : fullName;
        }

        /// <summary>
        /// Gets the string name of the given value type.
        /// </summary>
        /// <param name="t">The value type.</param>
        /// <returns>The type's string name.</returns>
        public static string ValueTypeName(ValueType t) 
        {
            return valueTypeNames[(int)t];
        }


        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="arg">A value type or a type name.</param>
        /// <returns>The type corresponding to the name.</returns>
        public static Type ToClass(Obj arg)
        {
            if (arg is Type)
            {
                return (Type)arg;
            }

            string abbrev = Printer.AsString(arg, false);
            string typeName;
            if (clrTranslator.TryGetValue(abbrev, out typeName))
            {
                return Type.GetType(typeName);
            }

            return typeName == "void" ? typeof(void) : Type.GetType(abbrev);
        }
        #endregion
    }
}
