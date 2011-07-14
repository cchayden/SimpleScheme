// <copyright file="TypePrimitives.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
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
            new TypeNameEntry(obj => SchemeBoolean.IsType(obj), () => SchemeBoolean.TypeName()),
            new TypeNameEntry(obj => Symbol.IsType(obj), () => Symbol.TypeName()),
            new TypeNameEntry(obj => Character.IsType(obj), () => Character.TypeName()),
            new TypeNameEntry(obj => Vector.IsType(obj), () => Vector.TypeName()),
            new TypeNameEntry(obj => Pair.IsType(obj), () => Pair.TypeName()),
            new TypeNameEntry(obj => Number.IsType(obj), () => Number.TypeName()),
            new TypeNameEntry(obj => SchemeString.IsType(obj), () => SchemeString.TypeName()),
            new TypeNameEntry(obj => Procedure.IsType(obj), () => Procedure.TypeName()),
            new TypeNameEntry(obj => InputPort.IsType(obj), () => InputPort.TypeName()),
            new TypeNameEntry(obj => OutputPort.IsType(obj), () => OutputPort.TypeName()),
            new TypeNameEntry(obj => EmptyList.IsType(obj), () => EmptyList.TypeName()),
        };

        /// <summary>
        /// Table of entries used to map the type to a name.
        /// These include the scheme types as well as additional implementation types.
        /// </summary>
        private static readonly AsStringEntry[] asStringEntries = 
        {
            new AsStringEntry(obj => SchemeBoolean.IsType(obj), (x, quoted, buf) => SchemeBoolean.AsString(x, quoted, buf)),
            new AsStringEntry(obj => Symbol.IsType(obj), (x, quoted, buf) => Symbol.AsString(x, quoted, buf)),
            new AsStringEntry(obj => Character.IsType(obj), (x, quoted, buf) => Character.AsString(x, quoted, buf)),
            new AsStringEntry(obj => Vector.IsType(obj), (x, quoted, buf) => Vector.AsString(x, quoted, buf)),
            new AsStringEntry(obj => Pair.IsType(obj), (x, quoted, buf) => Pair.AsString(x, quoted, buf)),
            new AsStringEntry(obj => Number.IsType(obj), (x, quoted, buf) => Number.AsString(x, quoted, buf)),
            new AsStringEntry(obj => SchemeString.IsType(obj), (x, quoted, buf) => SchemeString.AsString(x, quoted, buf)),
            new AsStringEntry(obj => Procedure.IsType(obj), (x, quoted, buf) => Procedure.AsString(x, quoted, buf)),
            new AsStringEntry(obj => InputPort.IsType(obj), (x, quoted, buf) => InputPort.AsString(x, quoted, buf)),
            new AsStringEntry(obj => OutputPort.IsType(obj), (x, quoted, buf) => OutputPort.AsString(x, quoted, buf)),
            new AsStringEntry(obj => EmptyList.IsType(obj), (x, quoted, buf) => EmptyList.AsString(x, quoted, buf)),
            new AsStringEntry(obj => obj is Stepper, (x, quoted, buf) => Stepper.AsString(x, quoted, buf)),
            new AsStringEntry(obj => obj is Undefined, (x, quoted, buf) => Undefined.AsString(x, quoted, buf)),
        };

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
                    return entry.Name();
                }
            }

            return obj.GetType().ToString();
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        internal static void AsString(Obj x, bool quoted, StringBuilder buf)
        {
            foreach (var entry in asStringEntries)
            {
                if (entry.TypePredicate(x))
                {
                    entry.AsStringFun(x, quoted, buf);
                    return;
                }
            }

            buf.Append(x);
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

            var typeName = SchemeString.AsString(arg, false);
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
            public readonly Func<string> Name;

            /// <summary>
            /// Initializes a new instance of the TypePrimitives.TypeNameEntry struct.
            /// </summary>
            /// <param name="typePredicate">Type tester.</param>
            /// <param name="name">Type name generator.</param>
            public TypeNameEntry(Func<Obj, bool> typePredicate, Func<string> name)
            {
                this.TypePredicate = typePredicate;
                this.Name = name;
            }
        }

        /// <summary>
        /// Table entry for type name mapper.
        /// </summary>
        private struct AsStringEntry
        {
            /// <summary>
            /// The type tester for this entry.
            /// </summary>
            public readonly Func<Obj, bool> TypePredicate;

            /// <summary>
            /// The function that gets the type name.
            /// </summary>
            public readonly Action<Obj, bool, StringBuilder> AsStringFun;

            /// <summary>
            /// Initializes a new instance of the TypePrimitives.AsStringEntry struct.
            /// </summary>
            /// <param name="typePredicate">Type tester.</param>
            /// <param name="asStringFun">Type string generator.</param>
            public AsStringEntry(Func<Obj, bool> typePredicate, Action<Obj, bool, StringBuilder> asStringFun)
            {
                this.TypePredicate = typePredicate;
                this.AsStringFun = asStringFun;
            }
        }
    }
}
