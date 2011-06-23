// <copyright file="AsyncResult.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System;
    using System.Threading;

    /// <summary>
    /// This implementation of IAsyncResult is used to result a result from BebinEval.
    /// </summary>
    /// <typeparam name="TResult">The result of the evaluation.</typeparam>
    public class AsyncResult<TResult> : IAsyncResult
    {
        /// <summary>
        /// The evaluation is still taking place.
        /// </summary>
        private const int StatePending = 0;

        /// <summary>
        /// The evaluation returned without suspending itself.
        /// </summary>
        private const int StateCompletedSynchronously = 1;

        /// <summary>
        /// The evaluation completed, but blocked before doing it.
        /// </summary>
        private const int StateCompletedAsynchronously = 2;

        /// <summary>
        /// The callback function that was passed in BeginEval.
        /// </summary>
        private readonly AsyncCallback ayncCallback;

        /// <summary>
        /// The state object that was passed in BeginEval.
        /// </summary>
        private readonly object asyncState;

        /// <summary>
        /// The current completion state, changed as the evaluation takes place.
        /// </summary>
        private int completedState = StatePending;

        /// <summary>
        /// If the operation does not complete synchronously, this is used to wait for the
        /// final result.
        /// </summary>
        private ManualResetEvent asyncWaitHandle;

        /// <summary>
        /// The evaluation result is stored here when it is ready.
        /// </summary>
        private TResult result;

        /// <summary>
        /// If evaluation caused an exception, it is stored here.
        /// </summary>
        private Exception exception;

        /// <summary>
        /// Initializes a new instance of the AsyncResult class.
        /// </summary>
        /// <param name="asyncCallback">The async callback.</param>
        /// <param name="state">The state.</param>
        public AsyncResult(AsyncCallback asyncCallback, object state)
        {
            this.ayncCallback = asyncCallback;
            this.asyncState = state;
        }

        /// <summary>
        /// Gets the AsyncState object.
        /// </summary>
        /// <value>The AsyncState obect.</value>
        public object AsyncState
        {
            get
            {
                return this.asyncState;
            }
        }

        /// <summary>
        /// Gets a value indicating whether the Async operation completed synchronously.
        /// </summary>
        /// <value>
        /// <c>true</c> if completed synchronously; otherwise, <c>false</c>.
        /// </value>
        public bool CompletedSynchronously
        {
            get
            {
                return Thread.VolatileRead(ref this.completedState) == StateCompletedSynchronously;
            }
        }

        /// <summary>
        /// Gets the async wait handle.
        /// </summary>
        /// <value>The async wait handle.</value>
        public WaitHandle AsyncWaitHandle
        {
            get
            {
                // Lazy creation, since WaitHandler is not always needed.
                if (this.asyncWaitHandle == null)
                {
                    bool done = this.IsCompleted;
                    ManualResetEvent mre = new ManualResetEvent(done);
                    if (Interlocked.CompareExchange(ref this.asyncWaitHandle, mre, null) != null)
                    {
                        // Another thread created this object's event; dispose 
                        // the event we just created
                        mre.Close();
                    }
                    else
                    {
                        if (!done && this.IsCompleted)
                        {
                            // If the operation wasn't done when we created 
                            // the event but now it is done, set the event
                            this.asyncWaitHandle.Set();
                        }
                    }
                }

                return this.asyncWaitHandle;
            }
        }

        /// <summary>
        /// Gets a value indicating whether this instance has completed.
        /// </summary>
        /// <value>
        /// True if this instance has completed; otherwise, false.
        /// </value>
        public bool IsCompleted
        {
            get
            {
                return Thread.VolatileRead(ref this.completedState) != StatePending;
            }
        }

        /// <summary>
        /// Sets AsyncResult as completed.
        /// </summary>
        /// <param name="taskResult">The result.</param>
        /// <param name="completedSynchronously">if set to <c>true</c> completed synchronously.</param>
        public void SetAsCompleted(TResult taskResult, bool completedSynchronously)
        {
            // Save the asynchronous operation's result);
            this.result = taskResult;

            // Tell the base class that the operation completed 
            // sucessfully (no exception)
            this.SetAsCompleted(completedSynchronously);
        }

        /// <summary>
        /// Sets AsyncResult as completed with exception.
        /// </summary>
        /// <param name="e">The exception.</param>
        /// <param name="completedSynchronously">if set to <c>true</c> completed synchronously.</param>
        public void SetAsCompleted(Exception e, bool completedSynchronously)
        {
            // Passing null for exception means no error occurred. 
            // This is the common case
            this.exception = e;

            // The completedState field MUST be set prior calling the callback
            this.SetAsCompleted(completedSynchronously);
        }

        /// <summary>
        /// Ends the invoke.
        /// </summary>
        /// <returns>The result of the operation</returns>
        public TResult EndInvoke()
        {
            // This method assumes that only 1 thread calls EndInvoke 
            // for this object
            if (!this.IsCompleted)
            {
                // If the operation isn't done, wait for it
                this.AsyncWaitHandle.WaitOne();
                this.AsyncWaitHandle.Close();
                this.asyncWaitHandle = null;  // Allow early GC
            }

            // Operation is done: if an exception occured, throw it);
            if (this.exception != null)
            {
                throw this.exception;
            }

            return this.result;
        }

        /// <summary>
        /// Sets the AsyncResult as completed. 
        /// </summary>
        /// <param name="completedSynchronously">if set to <c>true</c> [completed synchronously].</param>
        private void SetAsCompleted(bool completedSynchronously)
        {
            int prevState = Interlocked.Exchange(
                ref this.completedState,
                completedSynchronously ? StateCompletedSynchronously : StateCompletedAsynchronously);

            if (prevState != StatePending)
            {
                throw new InvalidOperationException("You can set a result only once");
            }

            // If the event exists, set it
            if (this.asyncWaitHandle != null)
            {
                this.asyncWaitHandle.Set();
            }

            // If a callback method was set, call it
            if (this.ayncCallback != null)
            {
                this.ayncCallback(this);
            }
        }
    }
}
