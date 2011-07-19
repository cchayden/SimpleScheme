// <copyright file="TypePrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
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
        #endregion

        /// <summary>
        /// Table of entries used to map the type to a name.
        /// These are the classes that correspond to scheme's primitive types.
        /// </summary>
        private static readonly TypeNameEntry[] typeNames = 
        {
            new TypeNameEntry(obj => IsBoolean(obj), "boolean"),
            new TypeNameEntry(obj => IsSymbol(obj), "symbol"),
            new TypeNameEntry(obj => IsCharacter(obj), "character"),
            new TypeNameEntry(obj => IsVector(obj), "vector"),
            new TypeNameEntry(obj => IsPair(obj), "pair"),
            new TypeNameEntry(obj => IsNumber(obj), "number"),
            new TypeNameEntry(obj => IsString(obj), "string"),
            new TypeNameEntry(obj => IsProcedure(obj), "procedure"),
            new TypeNameEntry(obj => IsInputPort(obj), "input port"),
            new TypeNameEntry(obj => IsOutputPort(obj), "output port"),
            new TypeNameEntry(obj => IsEmptyList(obj), "empty list"),
        };

        #region Scheme Builtin Type Name Accessors
        /// <summary>
        /// Gets the printable name of the scheme symbol type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string SymbolName
        {
            get { return "symbol"; }
        }

        /// <summary>
        /// Gets the printable name of the scheme character type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string CharacterName
        {
            get { return "character"; }
        }

        /// <summary>
        /// Gets the printable name of the scheme vector type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string VectorName
        {
            get { return "vector"; }
        }

        /// <summary>
        /// Gets the printable name of the scheme number type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string NumberName
        {
            get { return "number"; }
        }

        /// <summary>
        /// Gets the printable name of the scheme string type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string StringName
        {
            get { return "string"; }
        }

        /// <summary>
        /// Gets the printable name of the scheme procedure type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string ProcedureName
        {
            get { return "procedure"; }
        }

        /// <summary>
        /// Gets the printable name of the scheme boolean type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string InputPortName
        {
            get { return "input port"; }
        }

        /// <summary>
        /// Gets the printable name of the scheme boolean type.
        /// </summary>
        /// <returns>The type name.</returns>
        internal static string OutputPortName
        {
            get { return "output port"; }
        }
        #endregion

        #region Scheme Builtin Type Predicates
        /// <summary>
        /// Tests whether to given object is a scheme boolean.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme boolean.</returns>
        public static bool IsBoolean(Obj obj)
        {
            return obj is bool;
        }

        /// <summary>
        /// Tests whether to given object is a scheme symbol.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme symbol.</returns>
        public static bool IsSymbol(Obj obj)
        {
            return obj is string;
        }

        /// <summary>
        /// Tests whether to given object is a scheme character.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme character.</returns>
        public static bool IsCharacter(Obj obj)
        {
            return obj is char;
        }

        /// <summary>
        /// Tests whether to given object is a scheme vector.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme vector.</returns>
        public static bool IsVector(Obj obj)
        {
            return obj is Obj[];
        }

        /// <summary>
        /// Tests whether to given object is a scheme pair.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme pair.</returns>
        public static bool IsPair(Obj obj)
        {
            return obj is Pair;
        }

        /// <summary>
        /// Tests whether to given object is a scheme number.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme number.</returns>
        public static bool IsNumber(Obj obj)
        {
            return obj is double;
        }

        /// <summary>
        /// Tests whether to given object is a scheme string.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme string.</returns>
        public static bool IsString(Obj obj)
        {
            return obj is char[];
        }

        /// <summary>
        /// Tests whether to given object is a scheme procedure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme procedure.</returns>
        public static bool IsProcedure(Obj obj)
        {
            return obj is Procedure;
        }

        /// <summary>
        /// Tests whether to given object is a scheme closure.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme closure.</returns>
        public static bool IsClosure(Obj obj)
        {
            return obj is Closure;
        }

        /// <summary>
        /// Tests whether to given object is a scheme macro.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme macro.</returns>
        public static bool IsMacro(Obj obj)
        {
            return obj is Macro;
        }

        /// <summary>
        /// Tests whether to given object is a scheme input port.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme input port.</returns>
        public static bool IsInputPort(Obj obj)
        {
            return obj is InputPort;
        }

        /// <summary>
        /// Tests whether to given object is a scheme output port.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme output port.</returns>
        public static bool IsOutputPort(Obj obj)
        {
            return obj is OutputPort;
        }

        /// <summary>
        /// Tests whether to given object is a scheme empty list.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme empty list.</returns>
        public static bool IsEmptyList(Obj obj)
        {
            return obj is EmptyList;
        }

        /// <summary>
        /// Tests whether to given object is a scheme stepper.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme stepper.</returns>
        public static bool IsStepper(Obj obj)
        {
            return obj is Stepper;
        }

        /// <summary>
        /// Tests whether to given object is a scheme undefined object.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is a scheme undefined object.</returns>
        public static bool IsUndefined(Obj obj)
        {
            return obj is Undefined;
        }
        #endregion

        /// <summary>
        /// Find the scheme type name for a given object.
        /// Used to format error messages.
        /// </summary>
        /// <param name="obj">The object to use.</param>
        /// <returns>The scheme type name.</returns>
        internal static string TypeName(Obj obj)
        {
            foreach (var entry in typeNames)
            {
                if (entry.TypePredicate(obj))
                {
                    return entry.Name;
                }
            }

            return obj.GetType().ToString();
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
    }
}
