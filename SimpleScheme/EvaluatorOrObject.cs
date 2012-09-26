// -----------------------------------------------------------------------
// <copyright file="EvaluatorOrObject.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
// -----------------------------------------------------------------------

namespace SimpleScheme
{
    /// <summary>
    /// Base class of Evaluator and SchemeObject
    /// </summary>
    public abstract class EvaluatorOrObject : List
    {
        /// <summary>
        /// Make sure the EvaluatorOrObject is actually a SchemeObject.
        /// It would be nice to define a conversion, but it is not allowed to 
        /// define a conversion between base and derived classes.
        /// <returns>The object, otherwise throws an error.</returns>
        /// </summary>
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
}
