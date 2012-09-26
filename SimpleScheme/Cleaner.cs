// <copyright file="Cleaner.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// In charge of cleaning values.
    /// Recurse into objects that have substructure.
    /// Also clean Symbol, which is the only thing that really needs cleaning.
    /// </summary>
    public static class Cleaner
    {
        /// <summary>
        /// Clean the given object.
        /// Removes cached access information from symbols.
        /// The cached information makes access faster, but can lead to errors
        ///   if the location of the symbol, relative to the caller, has changed.
        /// </summary>
        /// <param name="x">The object to clean.</param>
        /// <returns>The cleaned object.</returns>
        public static SchemeObject Clean(SchemeObject x)
        {
            if (x == null)
            {
                return x;
            }

            switch (x.GetType().FullName)
            {
                case "SimpleScheme.Vector":
                    ((Vector)x).Clean();
                    return x;
                case "SimpleScheme.Symbol":
                case "SimpleScheme.Pair":
                    ((ICleanable)x).Clean();
                    return x;
                default:
                    return x;
            }
        }
    }
}
