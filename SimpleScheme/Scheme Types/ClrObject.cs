// <copyright file="ClrObject.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>

namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.IO;

    /// <summary>
    /// Wraps a CLR object returned by invoking a constructor or CLR method.
    /// Provides a means of converting to/from Scheme objects.
    /// </summary>
    public class ClrObject : ISchemeObject
    {
        /// <summary>
        /// Maps a type to a function that converts an instance of that type to a corresponding clr type.
        /// </summary>
        private static readonly Dictionary<Type, Func<ISchemeObject, object>> toClrMap;

        private static readonly Dictionary<Type, Func<object, ISchemeObject>> fromClrMap;

        /// <summary>
        /// The wrapped clrObject.
        /// </summary>
        private readonly object clrObject;

        /// <summary>
        /// Initializes static members of the <see cref="ClrObject"/> class. 
        /// </summary>
        static ClrObject()
        {
            toClrMap = new Dictionary<Type, Func<ISchemeObject, object>>
                {
                    { typeof(int), elem => elem.AsInt() },
                    { typeof(string), elem => Printer.AsString(elem, false) },
                    { typeof(bool), elem => elem.AsBoolean() },
                    { typeof(double), elem => elem.AsDouble() },
                    { typeof(float), elem => elem.AsFloat() },
                    { typeof(short), elem => elem.AsShort() },
                    { typeof(byte), elem => elem.AsByte() },
                    { typeof(char), elem => elem.AsChar() },
                    { typeof(TextReader), elem => elem.AsTextReader() },
                    { typeof(TextWriter), elem => elem.AsTextWriter() },
                    { typeof(char[]), elem => Printer.AsString(elem, false).ToCharArray() },
                    { typeof(object[]), elem => elem.AsVector().AsObjectArray() },
                    { typeof(bool[]), elem => elem.AsVector().AsBoolArray() },
                    { typeof(int[]), elem => elem.AsVector().AsIntArray() },
                    { typeof(byte[]), elem => elem.AsVector().AsByteArray() },
                    { typeof(short[]), elem => elem.AsVector().AsShortArray() },
                    { typeof(long[]), elem => elem.AsVector().AsLongArray() },
                    { typeof(float[]), elem => elem.AsVector().AsFloatArray() },
                    { typeof(double[]), elem => elem.AsVector().AsDoubleArray() }
                };

            fromClrMap = new Dictionary<Type, Func<object, ISchemeObject>>
                {
                    { typeof(int), elem => (Number)(int)elem },
                    { typeof(string), elem => SchemeString.New((string)elem) },
                    { typeof(bool), elem => (SchemeBoolean)(bool)elem },
                    { typeof(double), elem => (Number)(double)elem },
                    { typeof(float), elem => (Number)(float)elem },
                    { typeof(long), elem => (Number)(long)elem },
                    { typeof(short), elem => (Number)(short)elem },
                    { typeof(byte), elem => (Number)(byte)elem },
                    { typeof(char), elem => (Character)(char)elem },
                    { typeof(int[]), elem => (Vector)(object[])elem },
                    { typeof(string[]), elem => (Vector)(object[])elem },
                    { typeof(bool[]), elem => (Vector)(object[])elem },
                    { typeof(double[]), elem => (Vector)(object[])elem },
                    { typeof(float[]), elem => (Vector)(object[])elem },
                    { typeof(long[]), elem => (Vector)(object[])elem },
                    { typeof(short[]), elem => (Vector)(object[])elem },
                    { typeof(byte[]), elem => (Vector)(object[])elem },
                    { typeof(char[]), elem => (Vector)(object[])elem },
                    { typeof(object[]), elem => (Vector)(object[])elem },
                };
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ClrObject"/> class.
        /// </summary>
        /// <param name="clrObject">The wrapped clr object.</param>
        private ClrObject(object clrObject)
        {
            this.clrObject = clrObject;
        }

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.ClrObject); }
        }
        #endregion
        /// <summary>
        /// Gets the clr object itself.
        /// </summary>
        public object Value
        {
            get { return this.clrObject; }
        }

        /// <summary>
        /// Creates a new instance of the CrlObject.
        /// </summary>
        /// <param name="clrObject">The clr object to wrap.</param>
        /// <returns>The ClrObject wrapper.</returns>
        public static ClrObject New(object clrObject)
        {
            return new ClrObject(clrObject);
        }

        /// <summary>
        /// Convert an scheme object to the given type of clr object.
        /// </summary>
        /// <param name="elem">The object to convert.</param>
        /// <param name="clrClass">The desired clr class to convert it to.</param>
        /// <returns>The converted object.</returns>
        public static object ToClrObject(ISchemeObject elem, Type clrClass)
        {
            if (toClrMap.ContainsKey(clrClass))
            {
                return toClrMap[clrClass](elem);
            }

            return elem;
        }

        /// <summary>
        /// Convert a CLR object into a scheme object.
        /// </summary>
        /// <param name="elem">The CLR object.</param>
        /// <returns>Corresponding scheme object.</returns>
        public static ISchemeObject FromClrObject(object elem)
        {
            Type elemType = elem.GetType();
            if (fromClrMap.ContainsKey(elemType))
            {
                return fromClrMap[elemType](elem);
            }

            return new ClrObject(elem);
        }

        /// <summary>
        /// Print the enclosed clr object
        /// </summary>
        /// <returns>The enclosed object as a string.</returns>
        public override string ToString()
        {
            return this.Value.ToString();
        }
    }
}
