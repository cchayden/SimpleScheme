// <copyright file="CompletedAsyncResult.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Diagnostics.Contracts;
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
        /// Needed even though it complete synchronously.
        /// </summary>
        private ManualResetEvent asyncWaitHandle;

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
            get
            {
                Contract.Ensures(Contract.Result<bool>() == true);
                return true;
            }
        }

        /// <summary>
        /// Gets the wait handle.
        /// </summary>
        public WaitHandle AsyncWaitHandle
        {
            get
            {
                // Lazy creation, since WaitHandler is not always needed.
                if (this.asyncWaitHandle == null)
                {
                    ManualResetEvent mre = new ManualResetEvent(true);
                    if (Interlocked.CompareExchange(ref this.asyncWaitHandle, mre, null) != null)
                    {
                        // Another thread created this object's event; dispose 
                        // the event we just created
                        mre.Close();
                    }
                }

                Contract.Assume(this.asyncWaitHandle != null);
                return this.asyncWaitHandle;
            }
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
            get
            {
                Contract.Ensures(Contract.Result<bool>() == true);
                return true;
            }
        }
    }
}