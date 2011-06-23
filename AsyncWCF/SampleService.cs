using System;
using System.Net;
using System.ServiceModel;

namespace AsyncWCF
{
        // Service Contrace
        [ServiceContract]
        internal interface IService
        {
            [OperationContract(AsyncPattern = true)]
            IAsyncResult BeginGetUriLength(string input, AsyncCallback callback, object asyncState);
            string EndGetUriLength(IAsyncResult result);
        }

        // Service Contract Implementation
        // This class gets instantiated so that it can receive a call.
        [ServiceBehavior]
        internal class SampleService : IService
        {
            private WebRequest request;

            public IAsyncResult BeginGetUriLength(String uri, AsyncCallback callback, Object state)
            {
                request = WebRequest.Create(uri);
                return request.BeginGetResponse(callback, state);
            }

            public String EndGetUriLength(IAsyncResult result)
            {
                using (WebResponse response = request.EndGetResponse(result))
                {
                    return String.Format("ContentLength of {0} = {1}",
                        request.RequestUri, response.ContentLength);
                }
            }
        }
}
