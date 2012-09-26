// <copyright file="TypePrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using Obj = System.Object;

    /// <summary>
    /// Generic operations on Obj, for tracing, debugging, and interfacing with the CLR.
    /// Includes a method that returns "friendly" names for objects of given types.
    /// Also includes a methat that turns a "friendly" name into a full .NET name.
    /// <br/>
    /// We could have done this with a couple of Dictionaries, initialized by the various classes, but then
    ///   it would have been necessary to find an environment to store them.
    /// If it becomes important to make these extensible, that would be the right way to do it.
    /// </summary>
    public class TypePrimitives
    {
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
            switch (fullName)
            {
                // Names for types implementing Scheme values, used for error messages.
                case "SimpleScheme.AsynchronousClrProcedure":
                    return AsynchronousClrProcedure.Name;
                case "System.Char":
                    return Character.Name;
                case "SimpleScheme.ClrConstructor":
                    return ClrConstructor.Name;
                case "SimpleScheme.ClrProcedure":
                    return ClrProcedure.Name;
                case "SimpleScheme.Continuation":
                    return Continuation.Name;
                case "SimpleScheme.EmptyList":
                    return EmptyList.Name;
                case "SimpleScheme.InputPort":
                    return InputPort.Name;
                case "SimpleScheme.Lambda":
                    return Lambda.Name;
                case "SimpleScheme.Macro":
                    return Macro.Name;
                case "System.Byte":
                case "System.Int32":
                case "System.Int16":
                case "System.Int64":
                case "System.Single": 
                case "System.Double":
                    return Number.Name;
                case "SimpleScheme.OutputPort":
                    return OutputPort.Name;
                case "SimpleScheme.Pair":
                    return Pair.Name;
                case "SimpleScheme.Primitive":
                    return Primitive.Name;
                case "SimpleScheme.Procedure":
                    return Procedure.Name;
                case "System.Boolean":
                    return SchemeBoolean.Name;
                case "System.Char[]":
                    return SchemeString.Name;
                case "System.String":
                    return Symbol.Name;
                case "SimpleScheme.SynchronousClrProcedure":
                    return SynchronousClrProcedure.Name;
                case "SimpleScheme.Undefined":
                    return Undefined.Name;
                case "System.Object[]":
                    return Vector.Name;

                // Evaluator names, used for tracing.
                case "SimpleScheme.EndedEvaluator":
                    return EndedEvaluator.EvaluatorName;
                case "SimpleScheme.EvaluateAnd":
                    return EvaluateAnd.EvaluatorName;
                case "SimpleScheme.EvaluateCallWithInputFile":
                    return EvaluateCallWithInputFile.EvaluatorName;
                case "SimpleScheme.EvaluateCallWithOutputFile":
                    return EvaluateCallWithOutputFile.EvaluatorName;
                case "SimpleScheme.EvaluateCase":
                    return EvaluateCase.EvaluatorName;
                case "SimpleScheme.EvaluateCond":
                    return EvaluateCond.EvaluatorName;
                case "SimpleScheme.EvaluateDefine":
                    return EvaluateDefine.EvaluatorName;
                case "SimpleScheme.EvaluateDo":
                    return EvaluateDo.EvaluatorName;
                case "SimpleScheme.EvaluateExpandMacro":
                    return EvaluateExpandMacro.EvaluatorName;
                case "SimpleScheme.EvaluateExpression":
                    return EvaluateExpression.EvaluatorName;
                case "SimpleScheme.EvaluateIf":
                    return EvaluateIf.EvaluatorName;
                case "SimpleScheme.EvaluateLet":
                    return EvaluateLet.EvaluatorName;
                case "SimpleScheme.EvaluateLetRec":
                    return EvaluateLetRec.EvaluatorName;
                case "SimpleScheme.EvaluateLetStar":
                    return EvaluateLetStar.EvaluatorName;
                case "SimpleScheme.EvaluateList":
                    return EvaluateList.EvaluatorName;
                case "SimpleScheme.EvaluateMap":
                    return EvaluateMap.EvaluatorName;
                case "SimpleScheme.EvaluateOr":
                    return EvaluateOr.EvaluatorName;
                case "SimpleScheme.EvaluateParallel":
                    return EvaluateParallel.EvaluatorName;
                case "SimpleScheme.EvaluateProc":
                    return EvaluateProc.EvaluatorName;
                case "SimpleScheme.EvaluateSequence":
                    return EvaluateSequence.EvaluatorName;
                case "SimpleScheme.EvaluateSet":
                    return EvaluateSet.EvaluatorName;
                case "SimpleScheme.EvaluateTime":
                    return EvaluateTime.EvaluatorName;
                case "SimpleScheme.HaltedEvaluator":
                    return HaltedEvaluator.EvaluatorName;
                case "SimpleScheme.SuspendedEvaluator":
                    return SuspendedEvaluator.EvaluatorName;

                // anything else
                default:
                    return fullName;
            }
        }

        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="arg">A ValueType or a type name.</param>
        /// <returns>The type corresponding to the name.</returns>
        public static Type ToClass(Obj arg)
        {
            if (arg is Type)
            {
                return (Type)arg;
            }

            string abbrev = Printer.AsString(arg, false);
            string typeName;
            switch (abbrev)
            {
                case "boolean":
                    typeName = "System.Boolean";
                    break;
                case "char":
                    typeName = "System.Char";
                    break;
                case "string":
                    typeName = "System.String";
                    break;
                case "byte":
                    typeName = "System.Byte";
                    break;
                case "short":
                    typeName = "System.Int16";
                    break;
                case "int":
                    typeName = "System.Int32";
                    break;
                case "long":
                    typeName = "System.Int64";
                    break;
                case "float":
                    typeName = "System.Single";
                    break;
                case "double":
                    typeName = "System.Double";
                    break;
                case "object":
                    typeName = "System.Object";
                    break;
               case "boolean[]":
                    typeName = "System.Boolean[]";
                    break;
                case "char[]":
                    typeName = "System.Char[]";
                    break;
                case "string[]":
                    typeName = "System.String[]";
                    break;
                case "byte[]":
                    typeName = "System.Byte[]";
                    break;
                case "short[]":
                    typeName = "System.Int16[]";
                    break;
                case "int[]":
                    typeName = "System.Int32[]";
                    break;
                case "long[]":
                    typeName = "System.Int64[]";
                    break;
                case "float[]":
                    typeName = "System.Single[]";
                    break;
                case "double[]":
                    typeName = "System.Double[]";
                    break;
                case "object[]":
                    typeName = "System.Object[]";
                    break;
                case "void":
                    return typeof(void);
                default:
                    typeName = abbrev;
                    break;
            }

            return Type.GetType(typeName);
        }
        #endregion
    }
}
