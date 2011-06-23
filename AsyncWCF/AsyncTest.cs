namespace AsyncWCF
{
    using System;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    
    public class AsyncTest
    {
        private const String address = "http://localhost:8000/IService";
         // have to enable: netsh http add urlacl url=http://+:8000/IService user=chayden
        private  Binding binding = new BasicHttpBinding();
        private ServiceHost host;
        private SampleClient client;

        public AsyncTest()
        {

            // Setup the service
            this.host = new ServiceHost(typeof(SampleService), new Uri(address));
            this.host.AddServiceEndpoint(typeof(IService), binding, address);
            this.host.Open();

            Console.WriteLine("Service is listening.");
        }

        private void RunTest()
        {
            string[] websitesToTest = { "http://microsoft.com/", "http://Wintellect.com", "http://live.com" };
            foreach (var ws in websitesToTest)
            {
                Console.WriteLine("Requesting: {0}", ws);
                this.client.GetUriLength(ws);
            }

        }

        public AsyncTest Run()
        {
            // Make a client request
            EndpointAddress endpoint = new EndpointAddress(address);
            ChannelFactory<IService> factory = new ChannelFactory<IService>(binding, endpoint);
            IService proxy = factory.CreateChannel();
            this.client = new SampleClient(proxy);

            RunTest();

           // Have to delay here to let async operations finish.
           Console.ReadLine();
           return this;
        }

        public void Close()
        {
            if (this.host.State == CommunicationState.Opened)
            {
                this.host.Close();
            }
        }
    }
}
