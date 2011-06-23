namespace AsyncWCF
{
    using System;

    class SampleClient
    {
        private readonly IService proxy;

        public SampleClient(IService proxy)
        {
            this.proxy = proxy;
        }

        public void GetUriLength(string websiteToTest)
        {
            // Call server, receive async callback when finished.
            proxy.BeginGetUriLength(websiteToTest, 
                ar => Console.WriteLine(proxy.EndGetUriLength(ar)), null);
        }
    }
}
