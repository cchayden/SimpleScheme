// <copyright file="Pair.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using Obj = System.Object;

    /// <summary>
    /// A pair consists of two cells, named First and Rest.
    /// These are used to build the linked-list structures.
    /// </summary>
    public sealed class Pair
    {
        #region Constructor
        /// <summary>
        /// Initializes a new instance of the Pair class.
        /// </summary>
        /// <param name="first">The first object.</param>
        /// <param name="rest">The rest of the objs in the list are 
        /// referenced by this.</param>
        internal Pair(Obj first, Obj rest)
        {
            this.First = first;
            this.Rest = rest;
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Gets or sets the first obj of the pair.
        /// </summary>
        internal Obj First { get; set; }

        /// <summary>
        /// Gets or sets the rest of the objs in the list.
        /// </summary>
        internal Obj Rest { get; set; }
        #endregion

        #region Public Methods
        /// <summary>
        /// Turn the pair into a string for display.
        /// </summary>
        /// <returns>A string representing the pair.</returns>
        public override string ToString()
        {
            return Printer.AsString(this, true);
        }
        #endregion

        #region Internal Static Methods
        /// <summary>
        /// Tests whether two pairs are equal.
        /// The first object must be a pair.
        /// If the list is circulr, this will loop forever.
        /// </summary>
        /// <param name="obj1">The first object (must be a pair).</param>
        /// <param name="obj2">The other object.</param>
        /// <returns>True if they are both pairs and all elements are equal.</returns>
        internal static bool Equal(Obj obj1, Obj obj2)
        {
            if (!TypePrimitives.IsPair(obj2))
            {
                return false;
            }

            Pair pair1 = (Pair)obj1;
            Pair pair2 = (Pair)obj2;

            while (true)
            {
                if (!SchemeBoolean.Equal(List.First(pair1), List.First(pair2)))
                {
                    return false;
                }

                obj1 = List.Rest(pair1);
                obj2 = List.Rest(pair2);

                if (!TypePrimitives.IsPair(obj1) || !TypePrimitives.IsPair(obj2))
                {
                    return SchemeBoolean.Equal(obj1, obj2);
                }

                pair1 = (Pair)obj1;
                pair2 = (Pair)obj2;
            }
        }
        #endregion
    }
}