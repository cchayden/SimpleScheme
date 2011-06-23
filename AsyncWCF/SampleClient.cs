// <copyright file="SampleClient.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace AsyncWCF
{
    using System;

    /// <summary>
    /// Sample client asks server to get content length of a uri.
    /// </summary>
    public class SampleClient
    {
        /// <summary>
        /// The service proxy used to reach the server.
        /// </summary>
        private readonly IService proxy;

        /// <summary>
        /// The URI of the website to measure.
        /// </summary>
        private string uri;

        /// <summary>
        /// Initializes a new instance of the SampleClient class.
        /// </summary>
        /// <param name="proxy">The proxy to use.</param>
        public SampleClient(IService proxy)
        {
            this.proxy = proxy;
            this.ResultAvailable = false;
        }

        /// <summary>
        /// Gets the content length that was fetched from the server.
        /// </summary>
        public long ContentLength { get; private set; }

        /// <summary>
        /// Gets a value indicating whether the result is available yet.
        /// </summary>
        public bool ResultAvailable { get; private set; }

        /// <summary>
        /// Initiate a request to get the content length of a page named by the URI.
        /// </summary>
        /// <param name="websiteToTest">The URI of the website to test.</param>
        public void GetContentLength(string websiteToTest)
        {
            // Call server, receive async callback when finished.
            AsyncCallback ac = this.Callback;
            this.uri = websiteToTest;
            this.proxy.BeginGetContentLength(websiteToTest, ac, null);
        }

        /// <summary>
        /// This is called when the content length is available.
        /// </summary>
        /// <param name="ar">The async result.</param>
        private void Callback(IAsyncResult ar)
        {
            this.ContentLength = this.proxy.EndGetContentLength(ar);
            this.ResultAvailable = true;
            Console.WriteLine("Content length of {0}: {1}", this.uri, this.ContentLength);
        }
    }
}
