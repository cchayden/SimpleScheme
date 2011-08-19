// <copyright file="AsyncReturnValue.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// Represents an async return value.
    /// </summary>
    public class AsyncReturnValue
    {
        #region Constants
        /// <summary>
        /// The printable name of the type.
        /// </summary>
        public const string Name = "async-return-value";
        #endregion

        #region Fields
        /// <summary>
        /// An internal value that may be used to pass information back to caller.
        /// </summary>
        private readonly CatchCode value;

        /// <summary>
        /// The value that otherwise would have been returned.
        /// </summary>
        private readonly Obj returnedExpr;
        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the AsyncReturnValue class.
        /// Sets the internal value.
        /// </summary>
        /// <param name="value">The value to use.</param>
        /// <param name="returnedExpr">The returned value.</param>
        public AsyncReturnValue(CatchCode value, Obj returnedExpr)
        {
            this.value = value;
            this.returnedExpr = returnedExpr;
        }
        #endregion

        #region Enums
        /// <summary>
        /// Codes to pass back in the value to tell the caller what happened.
        /// </summary>
        public enum CatchCode
        {
            /// <summary>
            /// The evaluation suspended due to async call.
            /// </summary>
            CaughtSuspended,

            /// <summary>
            /// The evaluation returned a value after previously suspending.
            /// </summary>
            ReturnAfterSuspended
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets the value.
        /// </summary>
        public CatchCode Value
        {
            get { return this.value; }
        }

        /// <summary>
        /// Gets the returned value.
        /// </summary>
        public Obj ReturnedExpr
        {
            get { return this.returnedExpr; }
        }
        #endregion

        #region Public Static Methods
        /// <summary>
        /// Tests whether to given object is an async return value.
        /// </summary>
        /// <param name="obj">The object to test</param>
        /// <returns>True if the object is an async return value.</returns>
        public static bool Is(Obj obj)
        {
            return obj is AsyncReturnValue;
        }

        /// <summary>
        /// Convert object to async return value.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as an async return value.</returns>
        public static AsyncReturnValue As(Obj obj)
        {
            return (AsyncReturnValue)obj;
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Display the type and value as a string.
        /// </summary>
        /// <returns>The type name and value.</returns>
        public override string ToString()
        {
            return "<" + Name + "> " + this.value;
        }
        #endregion
    }
}

