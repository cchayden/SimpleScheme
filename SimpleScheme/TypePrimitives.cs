// <copyright file="TypePrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using Obj = System.Object;

    /// <summary>
    /// Primitive operations on built-in scheme types.
    /// </summary>
    public class TypePrimitives
    {
        #region Constants

        /// <summary>
        /// In primitiveTypes, the abbreviated type name.
        /// </summary>
        private const int Abbrev = 0;

        /// <summary>
        /// In primitiveTypes, the full type name.
        /// </summary>
        private const int Full = 1;
        #endregion

        #region Private Static Tables
        /// <summary>
        /// The primitive types that can be used as args for Clr methods.
        /// Both bare and array versions of each of these are defined.
        /// </summary>
        private static readonly string[][] primitiveTypes = 
        {
            // simple types
            new[] { "boolean", "System.Boolean" },
            new[] { "char", "System.Char" }, 
            new[] { "string", "System.String" }, 
            new[] { "byte", "System.Byte" },
            new[] { "short", "System.Int16" }, 
            new[] { "int", "System.Int32" }, 
            new[] { "long", "System.Int64" }, 
            new[] { "float", "System.Single" }, 
            new[] { "double", "System.Double" }, 

            // arrays
            new[] { "boolean[]", "System.Boolean[]" },
            new[] { "char[]", "System.Char[]" }, 
            new[] { "string[]", "System.String[]" }, 
            new[] { "byte[]", "System.Byte[]" },
            new[] { "short[]", "System.Int16[]" }, 
            new[] { "int[]", "System.Int32[]" }, 
            new[] { "long[]", "System.Int64[]" }, 
            new[] { "float[]", "System.Single[]" }, 
            new[] { "double[]", "System.Double[]" }, 
        };

        /// <summary>
        /// Table of entries used to map the type to a name.
        /// These are the classes that correspond to scheme's primitive types.
        /// </summary>
        private static readonly TypeNameEntry[] typeNames = 
        {
            new TypeNameEntry(obj => SchemeBoolean.IsBoolean(obj), "boolean"),
            new TypeNameEntry(obj => Symbol.IsSymbol(obj), "symbol"),
            new TypeNameEntry(obj => Character.IsCharacter(obj), "character"),
            new TypeNameEntry(obj => Vector.IsVector(obj), "vector"),
            new TypeNameEntry(obj => Pair.IsPair(obj), "pair"),
            new TypeNameEntry(obj => Number.IsNumber(obj), "number"),
            new TypeNameEntry(obj => SchemeString.IsString(obj), "string"),
            new TypeNameEntry(obj => Procedure.IsProcedure(obj), "procedure"),
            new TypeNameEntry(obj => InputPort.IsInputPort(obj), "input port"),
            new TypeNameEntry(obj => OutputPort.IsOutputPort(obj), "output port"),
            new TypeNameEntry(obj => EmptyList.IsEmptyList(obj), "empty list"),
        };

        /// <summary>
        /// Table of entries used to map an evaluator to a name.
        /// These are the classes that correspond to scheme's special forms
        /// </summary>
        private static readonly TypeNameEntry[] evaluatorNames = 
        {
            new TypeNameEntry(obj => obj is EvaluateAnd, EvaluateAnd.StepperName),
            new TypeNameEntry(obj => obj is EvaluateCallWithInputFile, EvaluateCallWithInputFile.StepperName),
            new TypeNameEntry(obj => obj is EvaluateCallWithOutputFile, EvaluateCallWithOutputFile.StepperName),
            new TypeNameEntry(obj => obj is EvaluateCase, EvaluateCase.StepperName),
            new TypeNameEntry(obj => obj is EvaluateClosure, EvaluateClosure.StepperName),
            new TypeNameEntry(obj => obj is EvaluateCond, EvaluateCond.StepperName),
            new TypeNameEntry(obj => obj is EvaluateContinuation, EvaluateContinuation.StepperName),
            new TypeNameEntry(obj => obj is EvaluateDefine, EvaluateDefine.StepperName),
            new TypeNameEntry(obj => obj is EvaluateDo, EvaluateDo.StepperName),
            new TypeNameEntry(obj => obj is EvaluateExpandMacro, EvaluateExpandMacro.StepperName),
            new TypeNameEntry(obj => obj is EvaluateExpression, EvaluateExpression.StepperName),
            new TypeNameEntry(obj => obj is EvaluateIf, EvaluateIf.StepperName),
            new TypeNameEntry(obj => obj is EvaluateLet, EvaluateLet.StepperName),
            new TypeNameEntry(obj => obj is EvaluateLetRec, EvaluateLetRec.StepperName),
            new TypeNameEntry(obj => obj is EvaluateLetStar, EvaluateLetStar.StepperName),
            new TypeNameEntry(obj => obj is EvaluateList, EvaluateList.StepperName),
            new TypeNameEntry(obj => obj is EvaluateMap, EvaluateMap.StepperName),
            new TypeNameEntry(obj => obj is EvaluateOr, EvaluateOr.StepperName),
            new TypeNameEntry(obj => obj is EvaluateProc, EvaluateProc.StepperName),
            new TypeNameEntry(obj => obj is EvaluateProcQuoted, EvaluateProcQuoted.StepperName),
            new TypeNameEntry(obj => obj is EvaluateSequence, EvaluateSequence.StepperName),
            new TypeNameEntry(obj => obj is EvaluateSet, EvaluateSet.StepperName),
            new TypeNameEntry(obj => obj is EvaluateTime, EvaluateTime.StepperName)
        };
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Find the scheme type name for a given object.
        /// Used to format error messages.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>The scheme type name.</returns>
        internal static string TypeName(Obj obj)
        {
            return LookupName(obj, typeNames);
        }

        /// <summary>
        /// Find the scheme type name for a given object.
        /// Used to format trace messages.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>The scheme type name.</returns>
        internal static string EvaluatorName(Obj obj)
        {
            return LookupName(obj, evaluatorNames);
        }

        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <param name="arg">A Type or a type name.</param>
        /// <returns>The type corresponding to the name.</returns>
        internal static Type ToClass(Obj arg)
        {
            if (arg is Type)
            {
                return (Type)arg;
            }

            var typeName = Printer.AsString(arg, false);
            foreach (var type in primitiveTypes)
            {
                if (typeName == type[Abbrev])
                {
                    return Type.GetType(type[Full]);
                }
            }

            if (typeName == "void")
            {
                return typeof(void);
            }

            return Type.GetType(typeName);
        }
        #endregion

        #region Private Static Methods
        /// <summary>
        /// Find the scheme type name for a given object.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <param name="table">The lookup table.</param>
        /// <returns>The scheme type name.</returns>
        private static string LookupName(Obj obj, IEnumerable<TypeNameEntry> table)
        {
            foreach (var entry in table)
            {
                if (entry.TypePredicate(obj))
                {
                    return entry.Name;
                }
            }

            return obj.GetType().ToString();
        }
        #endregion

        #region Internal Structs
        /// <summary>
        /// Table entry for type name mapper.
        /// </summary>
        private struct TypeNameEntry
        {
            /// <summary>
            /// The type tester for this entry.
            /// </summary>
            public readonly Func<Obj, bool> TypePredicate;

            /// <summary>
            /// The function that gets the type name.
            /// </summary>
            public readonly string Name;

            /// <summary>
            /// Initializes a new instance of the TypePrimitives.TypeNameEntry struct.
            /// </summary>
            /// <param name="typePredicate">Type tester.</param>
            /// <param name="name">Type name generator.</param>
            public TypeNameEntry(Func<Obj, bool> typePredicate, string name)
            {
                this.TypePredicate = typePredicate;
                this.Name = name;
            }
        }
        #endregion
    }
}
