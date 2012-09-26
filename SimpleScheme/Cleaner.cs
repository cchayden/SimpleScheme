// <copyright file="Cleaner.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;
    using Obj = System.Object;

    /// <summary>
    /// In charge of cleaning values.
    /// Recurse into objects that have substructure.
    /// Also clean Symbol, which is the only thing that really needs cleaning.
    /// </summary>
    public static class Cleaner
    {
        public static Obj Clean(Obj x)
        {
            if (x == null)
            {
                return x;
            }

            switch (x.GetType().FullName)
            {
                case "System.Object[]":
                    x.AsVector().Clean();
                    return x;
                case "SimpleScheme.Symbol":
                case "SimpleScheme.Pair":
                    ((Cleanable)x).Clean();
                    return x;
                default:
                    return x;
            }
        }
    }
}
