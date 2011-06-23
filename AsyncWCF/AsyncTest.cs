// <copyright file="AsyncTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace AsyncWCF
{
    using System;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    
    /// <summary>
    /// Test the async WCF calls.
    /// </summary>
    public class AsyncTest
    {
        /// <summary>
        /// The service address for the WCF server.
        /// </summary>
        private const string ServiceAddress = "http://localhost:8000/IService";

        // have to enable: netsh http add urlacl url=http://+:8000/IService user=chayden

        /// <summary>
        /// The service binding.
        /// </summary>
        private readonly Binding binding = new BasicHttpBinding();

        /// <summary>
        /// The service host.
        /// </summary>
        private readonly ServiceHost host;

        /// <summary>
        /// The service client.
        /// </summary>
        private SampleClient client;

        /// <summary>
        /// Initializes a new instance of the AsyncTest class.
        /// </summary>
        public AsyncTest()
        {
            // Set up the service
            this.host = new ServiceHost(typeof(SampleService), new Uri(ServiceAddress));
            this.host.AddServiceEndpoint(typeof(IService), this.binding, ServiceAddress);
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
            EndpointAddress endpoint = new EndpointAddress(ServiceAddress);
            ChannelFactory<IService> factory = new ChannelFactory<IService>(this.binding, endpoint);
            IService proxy = factory.CreateChannel();
            this.client = new SampleClient(proxy);

            this.RunTest();

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
        private void RunTest()
        {
            string[] websitesToTest = { "http://microsoft.com/", "http://Wintellect.com", "http://live.com" };
            foreach (var ws in websitesToTest)
            {
                Console.WriteLine("Requesting: {0}", ws);
                this.client.GetContentLength(ws);
            }
        }
    }
}
