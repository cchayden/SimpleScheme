// <copyright file="CouchDb.cs" company="Charles Hayden">
// Copyright © 2011 Charles Hayden
// </copyright>
//
// SharpCouch - a simple wrapper class for the CouchDB HTTP API
// Copyright 2007 Ciaran Gultnieks
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

namespace CouchLib
{
    using System.Collections.Generic;
    using System.IO;
    using System.Net;
    using System.Text;
    using System.Web;
    using LitJson;

    /// <summary>
    /// A simple wrapper class for the CouchDB HTTP API. No
    /// initialisation is necessary, just create an instance and
    /// call the appropriate methods to interact with CouchDB.
    /// </summary>
    public class CouchDb
    {
        /// <summary>
        /// Success return value.
        /// </summary>
        private const string Success = "{\"ok\":true}";

        /// <summary>
        /// The HTTP methods.
        /// </summary>
        private static readonly string[] methodName = { "GET", "PUT", "POST", "DELETE" };

        /// <summary>
        /// Username credentials.  May be null if none are required.
        /// </summary>
        private readonly string username;

        /// <summary>
        /// Password credentials.  May be null if none are required.
        /// </summary>
        private readonly string password;
        
        /// <summary>
        /// Initializes a new instance of the CouchDb class.
        /// </summary>
        /// <param name="server">The server URL.</param>
        /// <param name="defaultDb">The default database name.</param>
        /// <param name="username">The username credential.</param>
        /// <param name="password">The password credential.</param>
        private CouchDb(string server, string defaultDb, string username, string password)
        {
            this.Server = server;
            this.DbName = defaultDb;
            this.DefaultTimeout = 10000;
            this.username = username;
            this.password = password;
        }

        /// <summary>
        /// Names for the methods.  These are indexes into methodName above.
        /// </summary>
        private enum Method
        {
            /// <summary>
            /// The GET method.
            /// </summary>
            Get = 0,

            /// <summary>
            /// The PUT method.
            /// </summary>
            Put = 1,

            /// <summary>
            /// The POST method.
            /// </summary>
            Post = 2,

            /// <summary>
            /// The DELETE method.
            /// </summary>
            Delete = 3
        }

        /// <summary>
        /// Gets the server URL.
        /// </summary>
        public string Server { get; private set; }

        /// <summary>
        /// Gets the default database name.
        /// </summary>
        public string DbName { get; private set; }

        /// <summary>
        /// Gets or sets the default timeout for most commands, in milliseconds.
        /// </summary>
        public int DefaultTimeout { get; set; }

        /// <summary>
        /// Create a instance with the given server name, database, and credentials
        /// </summary>
        /// <param name="server">The server name.</param>
        /// <param name="defaultDb">The database name.</param>
        /// <param name="username">The username credential.</param>
        /// <param name="password">The password credential.</param>
        /// <returns>A CoudhDb instance.</returns>
        public static CouchDb New(string server, string defaultDb, string username, string password)
        {
            return new CouchDb(server, defaultDb, username, password);
        }

        /// <summary>
        /// Create a instance with the given server name and database.
        /// </summary>
        /// <param name="server">The server name.</param>
        /// <param name="defaultDb">The database name.</param>
        /// <returns>A CoudhDb instance.</returns>
        public static CouchDb New(string server, string defaultDb)
        {
            return new CouchDb(server, defaultDb, null, null);
        }

        /// <summary>
        /// Get a list of database on the server.
        /// </summary>
        /// <returns>A string array containing the database names
        /// </returns>
        public string[] GetDatabases()
        {
            string result = this.DoRequest(this.Server + "/_all_dbs", Method.Get);
            if (result == null)
            {
                return new string[0];
            }

            List<string> list = new List<string>();
            if (result != "[]")
            {
                JsonData d = JsonMapper.ToObject(result);
                foreach (JsonData db in d)
                {
                    list.Add(db.ToString());
                }
            }

            return list.ToArray();
        }

        /// <summary>
        /// Get the document count for the given database.
        /// </summary>
        /// <returns>The number of documents in the database</returns>
        public int CountDocuments()
        {
            // Get information about the database...
            string result = this.DoRequest(this.BaseUrl(), Method.Get);
            if (result == null)
            {
                return 0;
            }

            // The document count is a field within...
            JsonData d = JsonMapper.ToObject(result);
            int count = int.Parse(d["doc_count"].ToString());
            return count;
        }

        /// <summary>
        /// Get information on all the documents in the given database.
        /// </summary>
        /// <returns>An array of DocInfo instances</returns>
        public DocInfo[] GetAllDocuments()
        {
            string result = this.DoRequest(this.BaseUrl() + "/_all_docs", Method.Get);
            if (result == null)
            {
                return new DocInfo[0];
            }

            List<DocInfo> list = new List<DocInfo>();

            JsonData d = JsonMapper.ToObject(result);
            foreach (JsonData row in d["rows"])
            {
                DocInfo doc = new DocInfo
                    {
                        Id = row["id"].ToString(),
                        Revision = row["value"]["rev"].ToString()
                    };
                list.Add(doc);
            }

            return list.ToArray();
        }

        /// <summary>
        /// Create a new database.
        /// </summary>
        /// <returns>True if the request succeeded.  If the database already exists, returns false.</returns>
        public bool CreateDatabase()
        {
            string result = this.DoRequest(this.BaseUrl(), Method.Put);
            return result == null ? false : result.Contains(Success);
        }

        /// <summary>
        /// Delete a database
        /// <returns>True if the request succeeded.  If the database did not exist, returns false.</returns>
        /// </summary>
        /// <returns>True if the command succeeded.  False could indicate that the database did not exist before.</returns>
        public bool DeleteDatabase()
        {
            string result = this.DoRequest(this.BaseUrl(), Method.Delete);
            return result == null ? false : result.Contains(Success);
        }

        /// <summary>
        /// Execute a temporary view and return the results.
        /// </summary>
        /// <param name="map">The javascript map function</param>
        /// <param name="reduce">The javascript reduce function or
        ///     null if not required</param>
        /// <param name="startkey">The startkey or null not to use</param>
        /// <param name="endkey">The endkey or null not to use</param>
        /// <returns>The result (JSON format)</returns>
        public string ExecTempView(string map, string reduce = null, string startkey = null, string endkey = null)
        {
            // Generate the JSON view definition from the supplied
            // map and optional reduce functions...
            StringBuilder viewdef = new StringBuilder();
            viewdef.Append("{ \"map\":\"" + map + "\"");
            if (reduce != null)
            {
                viewdef.Append(",\"reduce\":\"" + reduce + "\"");
            }

            viewdef.Append("}");

            string url = AddStartEndKeys(this.BaseUrl() + "/_temp_view", startkey, endkey);

            // Set an infinite timeout on this for now, because
            // executing a temporary view can take a very long time...
            return this.DoRequest(url, Method.Post, viewdef.ToString(), "application/json", System.Threading.Timeout.Infinite);
        }

        /// <summary>
        /// Create a new document. If the document has no ID field,
        /// it will be assigned one by the server.
        /// </summary>
        /// <param name="content">The document contents (JSON).</param>
        /// <returns>True if the command succeeded.</returns>
        public bool CreateDocument(string content)
        {
            return this.DoRequest(this.BaseUrl(), Method.Post, content, "application/json") != null;
        }

        /// <summary>
        /// Get a document.
        /// </summary>
        /// <param name="docid">The document ID.</param>
        /// <param name="startkey">The startkey or null not to use</param>
        /// <param name="endkey">The endkey or null not to use</param>
        /// <returns>The document contents (JSON)</returns>
        public string GetDocument(string docid, string startkey, string endkey)
        {
            string url = AddStartEndKeys(this.BaseUrl() + "/" + docid, startkey, endkey);
            return this.DoRequest(url, Method.Get);
        }

        /// <summary>
        /// Get the whole document.
        /// </summary>
        /// <param name="docid">The document ID.</param>
        /// <returns>The document contents (JSON)</returns>
        public string GetDocument(string docid)
        {
            return this.GetDocument(docid, null, null);
        }

        /// <summary>
        /// Delete a document given its id and revision..
        /// </summary>
        /// <param name="docid">The document ID.</param>
        /// <param name="rev">The document revision.</param>
        /// <returns>True if the command succeeded.</returns>
        public bool DeleteDocument(string docid, string rev)
        {
            string docIdRev = docid + "?rev=" + rev;
            return this.DoRequest(this.BaseUrl() + "/" + docIdRev, Method.Delete) != null;
        }

        /// <summary>
        /// Delete a document, given its DocInfo.
        /// </summary>
        /// <param name="doc">The document info of the document to delete.</param>
        public void DeleteDocument(DocInfo doc)
        {
            this.DeleteDocument(doc.Id, doc.Revision);
        }

        /// <summary>
        /// Add start and end keys to the URL if given.
        /// </summary>
        /// <param name="url">The base URL.</param>
        /// <param name="startKey">The start key (can be null).</param>
        /// <param name="endKey">The end key (can be null).</param>
        /// <returns>The augmented URL.</returns>
        private static string AddStartEndKeys(string url, string startKey, string endKey)
        {
            if (startKey != null)
            {
                url += "?startkey=%22" + HttpUtility.UrlEncode(startKey) + "%22";
            }

            if (endKey != null)
            {
                url += startKey == null ? "?" : "&";
                url += "endkey=%22" + HttpUtility.UrlEncode(endKey) + "%22";
            }

            return url;
        }

        /// <summary>
        /// Internal helper to make an HTTP request and return the
        /// response. Returns null in the event of any kind
        /// of failure. 
        /// </summary>
        /// <param name="url">The URL to use in the request.</param>
        /// <param name="methodIndex">The method to use (one of the enum values).</param>
        /// <param name="postdata">Data to be posted with the request,
        ///    or null if not required.</param>
        /// <param name="contenttype">The content type to send, or null
        ///    if not required.</param>
        /// <param name="timeout">The request timeout, in milliseconds.  If not given, use the default timeout.</param>
        /// <returns>The server's response</returns>
        private string DoRequest(
            string url, Method methodIndex, string postdata = null, string contenttype = null, int timeout = 0)
        {
            HttpWebRequest req = WebRequest.Create(url) as HttpWebRequest;
            if (req == null)
            {
                return null;
            }

            // add credentials if supplied
            if (this.username != null && this.password != null)
            {
                req.Credentials = new NetworkCredential(this.username, this.password);
            }

            req.Method = methodName[(int)methodIndex];

            req.Timeout = timeout == 0 ? this.DefaultTimeout : timeout;
            if (contenttype != null)
            {
                req.ContentType = contenttype;
            }

            if (postdata != null)
            {
                byte[] bytes = Encoding.UTF8.GetBytes(postdata);
                req.ContentLength = bytes.Length;
                using (Stream ps = req.GetRequestStream())
                {
                    ps.Write(bytes, 0, bytes.Length);
                }
            }

            HttpWebResponse resp = null;
            try
            {
                resp = req.GetResponse() as HttpWebResponse;
            } 
            catch (WebException)
            {
            }

            if (resp == null)
            {
                return null;
            }

            using (Stream s = resp.GetResponseStream())
            {
                if (s == null)
                {
                    return null;
                }

                using (StreamReader reader = new StreamReader(s))
                {
                    return reader.ReadToEnd();
                }
            }
        }

        /// <summary>
        /// Return the base URL for resources that have a db name.
        /// </summary>
        /// <returns>The base URL to access that db.</returns>
        private string BaseUrl()
        {
            return this.Server + "/" + this.DbName;
        }

        /// <summary>
        /// Used to return metadata about a document.
        /// </summary>
        public class DocInfo
        {
            /// <summary>
            /// Gets or sets the document ID.
            /// </summary>
            public string Id { get; set; }

            /// <summary>
            /// Gets or sets the document revision.
            /// </summary>
            public string Revision { get; set; }
        }
    }
}
