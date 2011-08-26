// <copyright file="CouchDbTest.cs" company="Charles Hayden">
// Copyright © 2011 Charles Hayden
// </copyright>
namespace CouchDbTests
{
    using System;
    using System.Collections.Generic;

    using CouchLib;
    using LitJson;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    /// <summary>
    /// This is a test class for CouchDbTest and is intended
    /// to contain all CouchDbTest Unit Tests
    /// </summary>
    [TestClass]
    public class CouchDbTest
    {
        /// <summary>
        /// The database to use.
        /// </summary>
        private CouchDb db;

        /// <summary>
        /// The database server string.
        /// </summary>
        private string server;

        /// <summary>
        /// The default database name.
        /// </summary>
        private string database;

        #region Additional test attributes
        /// <summary>
        /// Run before each test.
        /// </summary>
        [TestInitialize]
        public void MyTestInitialize()
        {
            this.server = "http://localhost:5984";
            this.database = "test_db";
            this.db = CouchDb.New(this.server, this.database);
        }
        #endregion

        /// <summary>
        /// A test for CouchDb Constructor
        /// </summary>
        [TestMethod]
        public void CouchDbConstructorTest()
        {
            Assert.IsNotNull(this.db);
            Assert.AreEqual(this.server, this.db.Server);
            Assert.AreEqual(this.database, this.db.DbName);
            Assert.AreEqual(10000, this.db.DefaultTimeout);
        }

        /// <summary>
        /// A test for GetDatabases
        /// </summary>
        [TestMethod]
        public void GetDatabasesTest()
        {
            this.db.GetDatabases();
        }

        /// <summary>
        /// A test for CreateDatabase
        /// </summary>
        [TestMethod]
        public void CreateDatabaseTest()
        {
            this.db.DeleteDatabase();

            this.db.CreateDatabase();
            var dbs = this.db.GetDatabases();
            Assert.IsTrue(new List<string>(dbs).Contains(this.database));
        }

        /// <summary>
        /// A test for DeleteDatabase
        /// </summary>
        [TestMethod]
        public void DeleteDatabaseTest()
        {
            this.db.CreateDatabase();
            this.db.DeleteDatabase();
            var dbs = this.db.GetDatabases();
            Assert.IsFalse(new List<string>().Contains(this.database));
        }

        /// <summary>
        /// A test for CreateDocument
        /// </summary>
        [TestMethod]
        public void CreateDocumentTest()
        {
            this.SetupDatabase();
            this.FillDatabase(5);
            Assert.AreEqual(5, this.db.CountDocuments());
        }

        /// <summary>
        /// A test for CountDocuments
        /// </summary>
        [TestMethod]
        public void CountDocumentsTest()
        {
            this.SetupDatabase();
            this.FillDatabase(5);
            Assert.AreEqual(5, this.db.CountDocuments());
            this.FillDatabase(5);
            Assert.AreEqual(10, this.db.CountDocuments());
        }

        /// <summary>
        /// A test for GetAllDocuments
        /// </summary>
        [TestMethod]
        public void GetAllDocumentsTest()
        {
            this.SetupDatabase();
            this.FillDatabase(5);
            var docs = this.db.GetAllDocuments();
            Assert.AreEqual(5, docs.Length);
        }

        /// <summary>
        /// A test for DeleteDocument
        /// </summary>
        [TestMethod]
        public void DeleteDocumentTest()
        {
            this.SetupDatabase();
            this.FillDatabase(5);
            var docs = this.db.GetAllDocuments();
            Assert.AreEqual(5, docs.Length);
            this.db.DeleteDocument(docs[0].Id, docs[0].Revision);
            Assert.AreEqual(4, this.db.CountDocuments());
            this.db.DeleteDocument(docs[1]);
            Assert.AreEqual(3, this.db.CountDocuments());
        }

        /// <summary>
        /// A test for GetDocument
        /// </summary>
        [TestMethod]
        public void GetDocumentTest()
        {
            this.SetupDatabase();
            this.FillDatabase(5);
            var docs = this.db.GetAllDocuments();
            string doc = this.db.GetDocument(docs[0].Id);
            JsonData d = JsonMapper.ToObject(doc);
            Assert.IsTrue(d.IsObject);
            Assert.AreEqual(3, d.Count);
            Assert.IsTrue(d["_id"].IsString);
            Assert.AreEqual(docs[0].Id, (string)d["_id"]);
            Assert.AreEqual("user0", (string)d["name"]);
        }

        /// <summary>
        /// A test for ExecTempView
        /// </summary>
        [TestMethod]
        public void ExecTempViewTest()
        {
        }

        /// <summary>
        /// Set up a database.
        /// Delete, then create.
        /// </summary>
        private void SetupDatabase()
        {
            this.db.DeleteDatabase();
            this.db.CreateDatabase();
        }

        /// <summary>
        /// Put records into the database.
        /// </summary>
        /// <param name="n">The number of records to add.</param>
        private void FillDatabase(int n)
        {
            for (int i = 0; i < n; i++)
            {
                string content = String.Format("{{\"name\" : \"user{0}\"}}", i);
                this.db.CreateDocument(content);
            }
        }
    }
}
