// <copyright file="SchemeObjectExtensions.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;

    /// <summary>
    /// Extensions for SchemeObject
    /// </summary>
    internal static class SchemeObjectExtensions
    {
        /// <summary>
        /// Used to translate from full qualified type names into the friendly name.
        /// </summary>
        private static readonly Dictionary<string, string> typeTranslator;

        /// <summary>
        /// Used to translate the internal type name into the CLR type name.
        /// </summary>
        private static readonly Dictionary<string, string> clrTranslator;

        /// <summary>
        /// Initializes static members of the <see cref="SchemeObjectExtensions"/> class. 
        /// Maps between CLR type names and scheme object types.
        /// </summary>
        static SchemeObjectExtensions()
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
                    { "double[]", "System.Double[]" },                   
                    { "object[]", "System.Object[]" },                   
                };
        }

        /// <summary>
        /// Find the scheme type name or the evaluator name for a given object.
        /// Use the extension instead of this.
        /// </summary>
        /// <param name="obj">The object whose type name is desired.</param>
        /// <returns>The scheme type name.</returns>
        public static string SchemeTypeName(this object obj)
        {
            return SchemeTypeName(obj.GetType());
        }

        /// <summary>
        /// Find the scheme type name given the type.
        /// Use the extension instead of this.
        /// </summary>
        /// <param name="t">The object type.</param>
        /// <returns>The scheme type name.</returns>
        public static string SchemeTypeName(this Type t)
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
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="obj">Get the class of this object.</param>
        /// <returns>The type corresponding to the name.</returns>
        public static Type ToClass(this object obj)
        {
            string abbrev = obj.ToString();
            string typeName;
            if (clrTranslator.TryGetValue(abbrev, out typeName))
            {
                return Type.GetType(typeName);
            }

            return typeName == "void" ? typeof(void) : Type.GetType(abbrev);
        }
    }
}