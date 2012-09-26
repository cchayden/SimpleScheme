// -----------------------------------------------------------------------
// <copyright file="EvaluatorOrObject.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
// -----------------------------------------------------------------------

namespace SimpleScheme
{
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Base class of Evaluator and SchemeObject
    /// </summary>
    [ContractClass(typeof(EvaluatorOrObjectContract))]
    public abstract class EvaluatorOrObject : List
    {
        /// <summary>
        /// Make sure the EvaluatorOrObject is actually a SchemeObject.
        /// It would be nice to define a conversion, but it is not allowed to 
        /// define a conversion between base and derived classes.
        /// <returns>The object, otherwise throws an error.</returns>
        /// </summary>
        /// <returns>The scheme object.</returns>
        internal SchemeObject ToSchemeObject()
        {
            if (this is SchemeObject)
            {
                return (SchemeObject)this;
            }

            ErrorHandlers.TypeError(typeof(SchemeObject), this);
            return null;
        }

        /// <summary>
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        internal abstract string ToString(bool quoted);
    }

    /// <summary>
    /// Define the contract for EvaluatorOrObject
    /// </summary>
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1611:ElementParametersMustBeDocumented", Justification = "Contract.")]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1615:ElementReturnValueMustBeDocumented", Justification = "Contract.")]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1600:ElementsMustBeDocumented", Justification = "Contract.")]
    [ContractClassFor(typeof(EvaluatorOrObject))]
    public abstract class EvaluatorOrObjectContract : EvaluatorOrObject
    {
        internal new SchemeObject ToSchemeObject()
        {
            return null;
        }

        internal override string ToString(bool quoted)
        {
            Contract.Ensures(Contract.Result<string>() != null);
            return null;
        }
    }
}
