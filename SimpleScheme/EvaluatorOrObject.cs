// -----------------------------------------------------------------------
// <copyright file="EvaluatorOrObject.cs" company="Microsoft">
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
        /// Convert an obj into a string representation.
        /// </summary>
        /// <param name="quoted">If true, quote strings and chars.</param>
        /// <returns>The string representing the obj.</returns>
        public abstract string ToString(bool quoted);

        /// <summary>
        /// Make sure the EvaluatorOrObject is actually a SchemeObject.
        /// In general, evaluation can produce either
        /// (1) a value, or
        /// (2) an Evaluator, which can be run to produce a value.
        /// When the context demands an actual value, use this to ensure that this is the case.
        /// </summary>
        /// <param name="x">An evaluator or an object.</param>
        /// <returns>The object, otherwise throws an error.</returns>
        public static SchemeObject EnsureSchemeObject(EvaluatorOrObject x)
        {
            if (x is SchemeObject)
            {
                return (SchemeObject)x;
            }

            ErrorHandlers.TypeError(typeof(SchemeObject), x);
            return null;
        }
    }
}
