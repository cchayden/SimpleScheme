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
    public class TypePrimitives
    {
        /// <summary>
        /// Used to translate from full qualified type names into the friendly name.
        /// </summary>
        private readonly static Dictionary<string, string> typeTranslator;

        /// <summary>
        /// Used to translate the internal type name into the CLR type name.
        /// </summary>
        private readonly static Dictionary<string, string> clrTranslator;


        static TypePrimitives()
        {
            typeTranslator = new Dictionary<string, string>
                {
                    { "SimpleScheme.AsynchronousClrProcedure", AsynchronousClrProcedure.Name },
                    { "SimpleScheme.Character", Character.Name },
                    { "SimpleScheme.ClrConstructor", ClrConstructor.Name },
                    { "SimpleScheme.ClrProcedure", ClrProcedure.Name },
                    { "SimpleScheme.Continuation", Continuation.Name },
                    { "SimpleScheme.EmptyList", EmptyList.Name },
                    { "SimpleScheme.InputPort", InputPort.Name },
                    { "SimpleScheme.Lambda", Lambda.Name },
                    { "SimpleScheme.Macro", Macro.Name },
                    { "SimpleScheme.Number", Number.Name },
                    { "SimpleScheme.OutputPort", OutputPort.Name },
                    { "SimpleScheme.Pair", Pair.Name },
                    { "SimpleScheme.Primitive", Primitive.Name },
                    { "SimpleScheme.Procedure", Procedure.Name },
                    { "SimpleScheme.SchemeBoolean", SchemeBoolean.Name },
                    { "SimpleScheme.SchemeString", SchemeString.Name },
                    { "SimpleScheme.Symbol", Symbol.Name },
                    { "SimpleScheme.SynchronousClrProcedure", SynchronousClrProcedure.Name },
                    { "SimpleScheme.Undefined", Undefined.Name },
                    { "SimpleScheme.Vector", Vector.Name },
                    { "SimpleScheme.EndedEvaluator", EndedEvaluator.EvaluatorName },
                    { "SimpleScheme.EvaluateAnd", EvaluateAnd.EvaluatorName },
                    { "SimpleScheme.EvaluateCallWithInputFile", EvaluateCallWithInputFile.EvaluatorName },
                    { "SimpleScheme.EvaluateCallWithOutputFile", EvaluateCallWithOutputFile.EvaluatorName },
                    { "SimpleScheme.EvaluateCase", EvaluateCase.EvaluatorName },
                    { "SimpleScheme.EvaluateCond", EvaluateCond.EvaluatorName },
                    { "SimpleScheme.EvaluateDefine", EvaluateDefine.EvaluatorName },
                    { "SimpleScheme.EvaluateDo", EvaluateDo.EvaluatorName },
                    { "SimpleScheme.EvaluateExpandMacro", EvaluateExpandMacro.EvaluatorName },
                    { "SimpleScheme.EvaluateExpression", EvaluateExpression.EvaluatorName },
                    { "SimpleScheme.EvaluateIf", EvaluateIf.EvaluatorName },
                    { "SimpleScheme.EvaluateLet", EvaluateLet.EvaluatorName },
                    { "SimpleScheme.EvaluateLetRec", EvaluateLetRec.EvaluatorName },
                    { "SimpleScheme.EvaluateLetStar", EvaluateLetStar.EvaluatorName },
                    { "SimpleScheme.EvaluateList", EvaluateList.EvaluatorName },
                    { "SimpleScheme.EvaluateMap", EvaluateMap.EvaluatorName },
                    { "SimpleScheme.EvaluateOr", EvaluateOr.EvaluatorName },
                    { "SimpleScheme.EvaluateParallel", EvaluateParallel.EvaluatorName },
                    { "SimpleScheme.EvaluateProc", EvaluateProc.EvaluatorName },
                    { "SimpleScheme.EvaluateSequence", EvaluateSequence.EvaluatorName },
                    { "SimpleScheme.EvaluateSet", EvaluateSet.EvaluatorName },
                    { "SimpleScheme.EvaluateTime", EvaluateTime.EvaluatorName },
                    { "SimpleScheme.HaltedEvaluator", HaltedEvaluator.EvaluatorName },
                    { "SimpleScheme.SuspendedEvaluator", SuspendedEvaluator.EvaluatorName }
                };

            clrTranslator = new Dictionary<string, string>()
                {
                    {"bool", "System.Boolean"},                   
                    {"char", "System.Char"},                    
                    {"string", "System.String"},                    
                    {"byte", "System.Byte"},                   
                    {"short", "System.Int16"},                   
                    {"int", "System.Int32"},                   
                    {"long", "System.Int64"},                   
                    {"float", "System.Single"},                   
                    {"double", "System.Double"},                   
                    {"object", "System.Object"},                   
                    {"boolean[]", "System.Boolean[]"},                   
                    {"char[]", "System.Char[]"},                   
                    {"string[]", "System.String[]"},                    
                    {"byte[]", "System.Byte[]"},                   
                    {"short[]", "System.Int16[]"},                   
                    {"int[]", "System.Int32[]"},                   
                    {"long[]", "System.Int64[]"},                   
                    {"float[]", "System.Single[]"},                   
                    {"double[]", "System.Doubl[]e"},                   
                    {"object[]", "System.Object[]"},                   
                };
        }

        #region Public Static Methods
        /// <summary>
        /// Find the scheme type name or the evaluator name for a given object.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>The scheme type name.</returns>
        public static string TypeName(Obj obj)
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
