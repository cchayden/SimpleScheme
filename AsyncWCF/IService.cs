// <copyright file="IService.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace AsyncWCF
{
    using System;
    using System.ServiceModel;

    /// <summary>
    /// The service contract connecting server and client.
    /// </summary>
    [ServiceContract]
    public interface IService
    {
        /// <summary>
        /// Gets the content length of the web page at the given URI.
        /// </summary>
        /// <param name="uri">The URI of the page to measure.</param>
        /// <param name="callback">Async callback to call when the length is available.</param>
        /// <param name="asyncState">Additional state to pass to callback.</param>
        /// <returns>Async result, used to wait for result.</returns>
        [OperationContract(AsyncPattern = true)]
        IAsyncResult BeginGetContentLength(string uri, AsyncCallback callback, object asyncState);

        /// <summary>
        /// Releases resources of async operation.
        /// </summary>
        /// <param name="result">The async result associated wit the operation.</param>
        /// <returns>The content length.</returns>
        long EndGetContentLength(IAsyncResult result);
    }
}