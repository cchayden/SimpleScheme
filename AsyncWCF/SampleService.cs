// <copyright file="SampleService.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace AsyncWCF
{
    using System;
    using System.Net;
    using System.ServiceModel;

    /// <summary>
    /// The service contract implementation.
    /// This class gets instantiated so that it can receive a call.
    /// </summary>
    [ServiceBehavior]
    internal class SampleService : IService
    {
        /// <summary>
        /// The web request we need to make to get the content length.
        /// </summary>
        private WebRequest request;

        /// <summary>
        /// Start the operation.  It turns around and calls another async operation.
        /// When GetContentLength completes, it calls the WCF completion callback with the WCF state.
        /// Then WCF calls this object's End function, passing it a WCF AsyncResult
        /// </summary>
        /// <param name="uri">The URI of the web page to measure.</param>
        /// <param name="callback">Callback notified when the result is available.</param>
        /// <param name="state">State to pass to callback.</param>
        /// <returns>An AsyncResult, used to wait foe result.</returns>
        public IAsyncResult BeginGetContentLength(string uri, AsyncCallback callback, object state)
        {
            this.request = WebRequest.Create(uri);
            return this.request.BeginGetResponse(callback, state);    
        }

        /// <summary>
        /// Called by WCF when operation is ending.
        /// We know the WebRequest is done, because it's completion callback notified WCF.
        /// Since the WebRequest is done, we can get its result out and return it as
        ///   the WCF return value.
        /// </summary>
        /// <param name="result">The async result, used to get operation result.</param>
        /// <returns>The content length.</returns>
        public long EndGetContentLength(IAsyncResult result)
        {
            using (WebResponse response = this.request.EndGetResponse(result))
            {
                return response.ContentLength;
            }
        }
    }
}
