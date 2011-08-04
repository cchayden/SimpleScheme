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
    ///
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
            switch(obj.GetType().FullName)
            {
                // Names for types implementing Scheme values, used for error messages.
                case "System.Boolean":
                    return SchemeBoolean.Name;
                case "System.String":
                    return Symbol.Name;
                case "System.Char":
                    return Character.Name;
                case "System.Object[]":
                    return Vector.Name;
                case "SimpleScheme.Pair":
                    return Pair.Name;
                case "System.Byte":
                case "System.Int32":
                case "System.Int16":
                case "System.Int64":
                case "System.Single": 
                case "System.Double":
                    return Number.Name;
                case "System.Char[]":
                    return SchemeString.Name;
                case "SimpleScheme.Procedure":
                    return Procedure.Name;
                case "SimpleScheme.Primitive":
                    return Primitive.Name;
                case "SimpleScheme.Closure":
                    return Closure.Name;
                case "SimpleScheme.Macro":
                    return Macro.Name;
                case "SimpleScheme.ClrProcedure":
                    return ClrProcedure.Name;
                case "SimpleScheme.SynchronousClrProcedure":
                    return SynchronousClrProcedure.Name;
                case "SimpleScheme.AsynchronousClrProcedure":
                    return AsynchronousClrProcedure.Name;
                case "SimpleScheme.InputPort":
                    return InputPort.Name;
                case "SimpleScheme.OutputPort":
                    return OutputPort.Name;
                case "SimpleScheme.EmptyList":
                    return EmptyList.Name;

                // Evaluator names, used for tracing.
                case "SimpleScheme.EvaluateAnd":
                    return EvaluateAnd.StepperName;
                case "SimpleScheme.EvaluateCallWithInputFile":
                    return EvaluateCallWithInputFile.StepperName;
                case "SimpleScheme.EvaluateCallWithOutputFile":
                    return EvaluateCallWithOutputFile.StepperName;
                case "SimpleScheme.EvaluateCase":
                    return EvaluateCase.StepperName;
                case "SimpleScheme.EvaluateCond":
                    return EvaluateCond.StepperName;
                case "SimpleScheme.EvaluateContinuation":
                    return EvaluateContinuation.StepperName;
                case "SimpleScheme.EvaluateDefine":
                    return EvaluateDefine.StepperName;
                case "SimpleScheme.EvaluateDo":
                    return EvaluateDo.StepperName;
                case "SimpleScheme.EvaluateExpandMacro":
                    return EvaluateExpandMacro.StepperName;
                case "SimpleScheme.EvaluateExpression":
                    return EvaluateExpression.StepperName;
                case "SimpleScheme.EvaluateIf":
                    return EvaluateIf.StepperName;
                case "SimpleScheme.EvaluateLet":
                    return EvaluateLet.StepperName;
                case "SimpleScheme.EvaluateLetRec":
                    return EvaluateLetRec.StepperName;
                case "SimpleScheme.EvaluateLetStar":
                    return EvaluateLetStar.StepperName;
                case "SimpleScheme.EvaluateList":
                    return EvaluateList.StepperName;
                case "SimpleScheme.EvaluateMap":
                    return EvaluateMap.StepperName;
                case "SimpleScheme.EvaluateOr":
                    return EvaluateOr.StepperName;
                case "SimpleScheme.EvaluateProc":
                    return EvaluateProc.StepperName;
                case "SimpleScheme.EvaluateProcQuoted":
                    return EvaluateProcQuoted.StepperName;
                case "SimpleScheme.EvaluateSequence":
                    return EvaluateSequence.StepperName;
                case "SimpleScheme.EvaluateParallel":
                    return EvaluateParallel.StepperName;
                case "SimpleScheme.EvaluateSet":
                    return EvaluateSet.StepperName;
                case "SimpleScheme.EvaluateTime":
                    return EvaluateTime.StepperName;
                default:
                    return obj.GetType().FullName;
            }
        }

        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="arg">A Type or a type name.</param>
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
