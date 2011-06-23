// <copyright file="AsyncTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace AsyncWCF
{
    using System;
    using System.ServiceModel;
    
    /// <summary>
    /// Test the async WCF calls.
    /// This places an async WCF service call, which itself makes an async call.
    /// So the async resurn from WCF is really async, and is made when underlying async call returns.,
    /// </summary>
    public class AsyncTest
    {
        /// <summary>
        /// The service address for the WCF server.
        /// </summary>
        private const string ServiceAddress = "http://localhost:8000/IService";

        // have to enable: netsh http add urlacl url=http://+:8000/IService user=chayden

        /// <summary>
        /// The service host.
        /// </summary>
        private readonly ServiceHost host;

        /// <summary>
        /// Initializes a new instance of the AsyncTest class.
        /// </summary>
        public AsyncTest()
        {
            // Set up the service
            this.host = new ServiceHost(typeof(SampleService), new Uri(ServiceAddress));
            this.host.AddServiceEndpoint(typeof(IService), new BasicHttpBinding(), ServiceAddress);
            this.host.Open();

            Console.WriteLine("Service is listening.");
        }

        /// <summary>
        /// Run the server
        /// </summary>
        /// <returns>The class.</returns>
        public AsyncTest Run()
        {
            // Make a client request
            SampleClient client = new SampleClient(ServiceAddress);
            RunTest(client);

           // Have to delay here to let async operations finish.
           Console.ReadLine();
           return this;
        }

        /// <summary>
        /// Close the server.
        /// </summary>
        public void Close()
        {
            if (this.host.State == CommunicationState.Opened)
            {
                this.host.Close();
            }
        }

        /// <summary>
        /// Run the test.
        /// </summary>
        /// <param name="client"> The WCF client to use.</param>
        private static void RunTest(SampleClient client)
        {
            string[] websitesToTest = { "http://microsoft.com/", "http://Wintellect.com", "http://live.com" };
            foreach (var ws in websitesToTest)
            {
                Console.WriteLine("Requesting: {0}", ws);
                client.GetContentLength(ws);
            }
        }
    }
}
