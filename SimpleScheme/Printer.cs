// <copyright file="Printer.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Text;
    using System.Collections.Generic;
    using Obj = System.Object;

    /// <summary>
    /// In charge of printing values.
    /// Can print in one of two forms: quoted or unquoted.
    /// Quoted values are more verbose, and are appropriate for logs or traces.
    /// Unquoted values are for use internally (for instance to get a filename from either a 
    ///    string or a symbol.)
    /// </summary>
    public static class Printer
    {
        #region Static Fields
        private static TypeTable types = new TypeTable();
        #endregion

        #region Static Initializer
        static Printer()
        {
            types
               .Add(Pair.Is, Pair.TypeName)
               .Add(EmptyList.Is, EmptyList.TypeName)
               .Add(Number.Is, Number.TypeName)
               .Add(Character.Is, Character.TypeName)
               .Add(SchemeString.Is, SchemeString.TypeName)
               .Add(Procedure.Is, Procedure.TypeName)
               .Add(Vector.Is, Vector.TypeName)
               .Add(Symbol.Is, Symbol.TypeName)
               .Add(SchemeBoolean.Is, SchemeBoolean.TypeName)
               .Add(InputPort.Is, InputPort.TypeName)
               .Add(OutputPort.Is, OutputPort.TypeName)
               .Add(AsynchronousClrProcedure.Is, AsynchronousClrProcedure.TypeName)
               .Add(SynchronousClrProcedure.Is, SynchronousClrProcedure.TypeName)
               .Add(ClrConstructor.Is, ClrConstructor.TypeName)
               .Add(Continuation.Is, Continuation.TypeName)
               .Add(Lambda.Is, Lambda.TypeName)
               .Add(Macro.Is, Macro.TypeName)
               .Add(Undefined.Is, Undefined.TypeName);
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <returns>The string representing the obj.</returns>
        public static string AsString(Obj x)
        {
            return AsString(x, true);
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        public static string AsString(Obj x, bool quoted)
        {
            var buf = new StringBuilder();
            PrintString(x, quoted, buf);
            return buf.ToString();
        }

        /// <summary>
        /// Convert an obj into a string representation and write to string builder.
        /// First gets the actual type, then calls a type-specific routine.
        /// If not one of the predefined types, use generic ToString.
        /// </summary>
        /// <param name="x">The obj to convert.</param>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <param name="buf">The buffer to accumulate the string into.</param>
        public static void PrintString(Obj x, bool quoted, StringBuilder buf)
        {
            if (x == null)
            {
                return;
            }

            if (x is IPrintable)
            {
                ((IPrintable)x).PrintString(quoted, buf);
            }
            else
            {
                // use the built-in ToString
                buf.Append(x);   
            }
        }

        /// <summary>
        /// Gets the primitive type name of an object.
        /// </summary>
        /// <param name="obj">The object to get the type name of.</param>
        /// <returns>The type name.</returns>
        public static string TypeName(Obj obj)
        {
            var ent = types.Find(obj);
            return ent != null ? ent.TypeName : "Unknown";
        }
        #endregion

        #region TypeTable
        /// <summary>
        /// Type table entry
        /// </summary>
        private class TypeEntry
        {
            /// <summary>
            /// Tests an object to see if it of a given type.
            /// </summary>
            public Predicate<object> TypePredicate;

            /// <summary>
            /// Gives the scheme type name.
            /// </summary>
            public string TypeName;

            /// <summary>
            /// Initializes a new instance of the TypeEntry class.
            /// </summary>
            /// <param name="typePredicate">The type predicate.</param>
            /// <param name="typeName">The scheme type name.</param>
            public TypeEntry(Predicate<object> typePredicate, string typeName)
            {
                this.TypePredicate = typePredicate;
                this.TypeName = typeName;
            }
        }

        /// <summary>
        /// The type table.
        /// Contains a predicate for testing the type of an object, and the corresponding type name.
        /// </summary>
        private class TypeTable
        {
            /// <summary>
            /// The list of scheme types supported by the interpreter.
            /// </summary>
            private List<TypeEntry> typeList = new List<TypeEntry>();

            /// <summary>
            /// Add an element to the type table.
            /// </summary>
            /// <param name="typePredicate">The type predicate.</param>
            /// <param name="typeName">The corresponding type name.</param>
            /// <returns></returns>
            public TypeTable Add(Predicate<object> typePredicate, string typeName)
            {
                this.typeList.Add(new TypeEntry(typePredicate, typeName));
                return this;
            }

            /// <summary>
            /// Find an entry in the type list.
            /// </summary>
            /// <param name="obj">The object whose type is to be found.</param>
            /// <returns>The entry that is found, or null.</returns>
            public TypeEntry Find(Obj obj)
            {
                return this.typeList.Find(entry => entry.TypePredicate(obj));
            }
        }
        #endregion
    }
}