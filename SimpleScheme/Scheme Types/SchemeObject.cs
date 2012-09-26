// <copyright file="SchemeObject.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;

    /// <summary>
    /// All scheme types implement this interface.
    /// This inherits from List so that the static methods defined there will all be available without having
    ///   to be qualified.
    /// Every SchemeObject has a line number.  This field is set when the object is created by read().
    /// So objects that are part of a program are associated with the line number in the program where they 
    ///   originated.  Other objects that are created at run time do not have a line number (set to 0).
    /// This class also provides some type primitives, mostly as static methods.
    /// The only method a subclass has to implement is ToString(bool).
    /// </summary>
    public abstract class SchemeObject : EvaluatorOrObject
    {
        #region Fields
        /// <summary>
        /// For symbols created by read, the line number where they were read from.
        /// </summary>
        private readonly int lineNumber;
        #endregion

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeObject"/> class.
        /// </summary>
        protected SchemeObject()
        {
            this.lineNumber = 0;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SchemeObject"/> class.
        /// </summary> <param name="lineNumber">The line number.
        /// </param>
        protected SchemeObject(int lineNumber)
        {
            this.lineNumber = lineNumber;
        }

        /// <summary>
        /// Gets the line number where the object was read.
        /// </summary>
        public int LineNumber
        {
            get { return this.lineNumber; }
        }

        /// <summary>
        /// Gets the CLR type name of the object.
        /// </summary>
        internal string ClrTypeName 
        { 
            get { return this.GetType().FullName; }
        }

        /// <summary>
        /// Print the value into the given buffer.
        /// This is the default implementation.
        /// Subclasses can redefine if they desire.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <returns>The object as a string.</returns>
        internal override string ToString(bool quoted)
        {
            return this.ToString();
        }

        /// <summary>
        /// Clean the object.
        /// Removes cached access information from symbols.
        /// The cached information makes access faster, but can lead to errors
        ///   if the location of the symbol, relative to the caller, has changed.
        /// </summary>
        internal virtual void Clean()
        {
            return;
        }

        /// <summary>
        /// Gets a string describing the object.
        /// </summary>
        /// <returns>The object description.</returns>
        internal virtual string Describe()
        {
            return string.Empty;
        }

        /// <summary>
        /// Gets a CLR type from the given arg.
        /// Either it already holds a type, or else it holds a type name.
        /// If it is a name, then create the type from the name.
        /// </summary>
        /// <returns>The type corresponding to the name.</returns>
        internal Type ToClass()
        {
            return SchemeObjectExtensions.ToClass(this);
        }
    }

    #region Extension Class

    #endregion
}
