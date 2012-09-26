// <copyright file="ClrObject.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>

namespace SimpleScheme
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;
    using System.IO;

    /// <summary>
    /// Wraps a CLR object returned by invoking a constructor or CLR method.
    /// Provides a means of converting to/from Scheme objects.
    /// </summary>
    public class ClrObject : SchemeObject
    {
        #region Fields
        /// <summary>
        /// Maps a type to a function that converts an instance of that type to a corresponding clr type.
        /// Here, we know the type that we need and have to convert the argument to that type.
        /// </summary>
        private static readonly Dictionary<Type, Func<SchemeObject, object>> toClrMap;

        /// <summary>
        /// Map a type to a function that converts to an instance of that type.
        /// We got back some CLR object from a CLR call, and we need to convert it to something reasonable.
        /// </summary>
        private static readonly Dictionary<Type, Func<object, SchemeObject>> fromClrMap;

        /// <summary>
        /// The wrapped clrObject.
        /// </summary>
        private readonly object clrObject;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes static members of the <see cref="ClrObject"/> class. 
        /// </summary>
        static ClrObject()
        {
            toClrMap = new Dictionary<Type, Func<SchemeObject, object>>
                {
                    { typeof(int), elem => Number.AsInt(elem) },
                    { typeof(string), elem => elem.ToString() },
                    { typeof(bool), elem => SchemeBoolean.AsBool(elem) },
                    { typeof(double), elem => Number.AsDouble(elem) },
                    { typeof(float), elem => Number.AsFloat(elem) },
                    { typeof(short), elem => Number.AsShort(elem) },
                    { typeof(byte), elem => Number.AsByte(elem) },
                    { typeof(char), elem => Character.AsChar(elem) },
                    { typeof(TextReader), elem => InputPort.AsTextReader(elem) },
                    { typeof(TextWriter), elem => OutputPort.AsTextWriter(elem) },
                    { typeof(char[]), elem => elem.ToString().ToCharArray() },
                    { typeof(object[]), elem => Vector.AsObjectArray(elem) },
                    { typeof(bool[]), elem => Vector.AsBoolArray(elem) },
                    { typeof(int[]), elem => Vector.AsIntArray(elem) },
                    { typeof(byte[]), elem => Vector.AsByteArray(elem) },
                    { typeof(short[]), elem => Vector.AsShortArray(elem) },
                    { typeof(long[]), elem => Vector.AsLongArray(elem) },
                    { typeof(float[]), elem => Vector.AsFloatArray(elem) },
                    { typeof(double[]), elem => Vector.AsDoubleArray(elem) }
                };

            fromClrMap = new Dictionary<Type, Func<object, SchemeObject>>
                {
                    { typeof(Number), elem => (Number)elem },
                    { typeof(int), elem => (Number)(int)elem },
                    { typeof(SchemeString), elem => (SchemeString)elem },
                    { typeof(string), elem => new SchemeString((string)elem) },
                    { typeof(SchemeBoolean), elem => (SchemeBoolean)elem },
                    { typeof(bool), elem => (SchemeBoolean)(bool)elem },
                    { typeof(double), elem => (Number)(double)elem },
                    { typeof(float), elem => (Number)(float)elem },
                    { typeof(long), elem => (Number)(long)elem },
                    { typeof(short), elem => (Number)(short)elem },
                    { typeof(byte), elem => (Number)(byte)elem },
                    { typeof(Character), elem => (Character)elem },
                    { typeof(char), elem => (Character)(char)elem },
                    { typeof(EmptyList), elem => EmptyList.Instance },
                    { typeof(Symbol), elem => new SchemeString(elem.ToString()) },
                    { typeof(int[]), elem => Vector.New(elem) },
                    { typeof(string[]), elem => Vector.New(elem) },
                    { typeof(bool[]), elem => Vector.New(elem) },
                    { typeof(double[]), elem => Vector.New(elem) },
                    { typeof(float[]), elem => Vector.New(elem) },
                    { typeof(long[]), elem => Vector.New(elem) },
                    { typeof(short[]), elem => Vector.New(elem) },
                    { typeof(byte[]), elem => Vector.New(elem) },
                    { typeof(char[]), elem => SchemeString.New((char[])elem) },
                    { typeof(object[]), elem => Vector.New(elem) },
                };
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ClrObject"/> class.
        /// </summary>
        /// <param name="clrObject">The wrapped clr object.</param>
        public ClrObject(object clrObject)
        {
            Contract.Requires(clrObject != null);
            this.clrObject = clrObject;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the clr object itself.
        /// </summary>
        public object Value
        {
            get
            {
                Contract.Ensures(Contract.Result<object>() != null);
                return this.clrObject;
            }
        }
        #endregion

        #region Static Methods
        /// <summary>
        /// Convert an scheme object to the given type of clr object.
        /// </summary>
        /// <param name="elem">The object to convert.</param>
        /// <param name="clrClass">The desired clr class to convert it to.</param>
        /// <returns>The converted object.</returns>
        public static object ToClrObject(SchemeObject elem, Type clrClass)
        {
            // elem can be null (for static methods)
            // clrClass can be null (for constructors)
            if (elem is ClrObject)
            {
                var value = ((ClrObject)elem).Value;
                if (value.GetType() == clrClass)
                {
                    return value;
                }
            }

            if (clrClass != null && toClrMap.ContainsKey(clrClass))
            {
                var map = toClrMap[clrClass];
                Contract.Assume(map != null);
                return map(elem);
            }

            return elem;
        }

        /// <summary>
        /// Convert a CLR object into a scheme object.
        /// </summary>
        /// <param name="elem">The CLR object.</param>
        /// <returns>Corresponding scheme object.</returns>
        public static SchemeObject FromClrObject(object elem)
        {
            Contract.Requires(elem != null);
            Contract.Ensures(Contract.Result<SchemeObject>() != null);
            Type elemType = elem.GetType();
            if (elemType == typeof(ClrObject))
            {
                elem = ((ClrObject)elem).Value;
                elemType = elem.GetType();
            }

            if (fromClrMap.ContainsKey(elemType))
            {
                var map = fromClrMap[elemType];
                Contract.Assume(map != null);
                var res = map(elem);
                Contract.Assume(res != null);
                return res;
            }

            return new ClrObject(elem);
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Print the enclosed clr object
        /// </summary>
        /// <returns>The enclosed object as a string.</returns>
        public override string ToString()
        {
            return this.Value.ToString();
        }
        #endregion

        #region Contract Invariant
        /// <summary>
        /// Describes invariants on the member variables.
        /// </summary>
        [ContractInvariantMethod]
        private void ContractInvariant()
        {
            Contract.Invariant(this.clrObject != null);
        }
        #endregion
    }
}
