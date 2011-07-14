// <copyright file="CompletedAsyncResult.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Threading;

    /// <summary>
    /// An IAsyncResult used to return synchronously.
    /// </summary>
    /// <typeparam name="TResult">The result type.</typeparam>
    public class CompletedAsyncResult<TResult> : IAsyncResult
    {
        /// <summary>
        /// Storage for the result.
        /// </summary>
        private readonly TResult result;

        /// <summary>
        /// Initializes a new instance of the CompletedAsyncResult class.
        /// </summary>
        /// <param name="result">The result toreturn.</param>
        public CompletedAsyncResult(TResult result)
        {
            this.result = result;
        }

        /// <summary>
        /// Gets a value indicating whether the result is completed (it is).
        /// </summary>
        public bool IsCompleted
        {
            get { return true; }
        }

        /// <summary>
        /// Gets the wait handle.
        /// </summary>
        public WaitHandle AsyncWaitHandle
        {
            get { return null; }
        }

        /// <summary>
        /// Gets the async state, which carries the result.
        /// </summary>
        public object AsyncState
        {
            get { return this.result; }
        }

        /// <summary>
        /// Gets a value indicating whether the call completed synchronously (it did).
        /// </summary>
        public bool CompletedSynchronously
        {
            get { return true; }
        }
    }
}