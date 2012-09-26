// <copyright file="IEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// The environment interface.
    /// The only operation available on the interface is to define an object in the environment.
    /// </summary>
    [ContractClass(typeof(IEnvironmentContract))]
    public interface IEnvironment
    {
        /// <summary>
        /// Define a variable in the environment.
        /// </summary>
        /// <param name="var">This must be a symbol.</param>
        /// <param name="val">The value of the variable.</param>
        void Define(string var, SchemeObject val);

        /// <summary>
        /// Set the value of a variable in the environment to a new value.
        /// Var is the variable name.
        /// Searches the chain of environments looking for a binding.
        /// </summary>
        /// <param name="var">The variable name.</param>
        /// <param name="val">The new value for the variable.</param>
        void Set(string var, SchemeObject val);

            /// <summary>
        /// Look up a variable in the environment.
        /// </summary>
        /// <param name="var">This must be a symbol.</param>
        /// <returns>The value of the symbol</returns>
        SchemeObject Lookup(string var);
    }

    /// <summary>
    /// Define the contract for IEnvironment
    /// </summary>
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1611:ElementParametersMustBeDocumented", Justification = "Contract.")]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1615:ElementReturnValueMustBeDocumented", Justification = "Contract.")]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1600:ElementsMustBeDocumented", Justification = "Contract.")]
    [ContractClassFor(typeof(IEnvironment))]
    internal abstract class IEnvironmentContract : IEnvironment
    {
        public void Define(string var, SchemeObject val)
        {
            Contract.Requires(var != null);
            Contract.Requires(val != null);
        }

        public void Set(string var, SchemeObject val)
        {
            Contract.Requires(var != null);
            Contract.Requires(val != null);
        }

        public SchemeObject Lookup(string var)
        {
            Contract.Requires(var != null);
            return null;
        }
    }
}
