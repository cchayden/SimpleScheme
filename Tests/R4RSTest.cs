﻿// -----------------------------------------------------------------------
// <copyright file="R4RSTest.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
// -----------------------------------------------------------------------

namespace Tests
{
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    using SimpleScheme;

    /// <summary>
    /// Run the unmodified r4rstest
    /// </summary>
    [TestClass]
    public class R4RSTest
    {
        /// <summary>
        /// A scheme interpreter, created for each test.
        /// </summary>
        private IInterpreter interpreter;

        /// <summary>
        /// Initialize each test.
        /// </summary>
        [TestInitialize]
        public void MyTestInitialize()
        {
            this.interpreter = Interpreter.New();
        }

        /// <summary>
        /// Run the r4rstest
        /// </summary>
        [DeploymentItem("r4rstest.scm"), TestMethod]
        public void FullTest()
        {
            this.interpreter.LoadFile((Symbol)"r4rstest.scm", null);
        }
    }
}
