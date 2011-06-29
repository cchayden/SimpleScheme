// <copyright file="IEnvironment.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    /// <summary>
    /// The interface for the SimpleScheme environment.
    /// </summary>
    public interface IEnvironment
    {
        void InstallPrimitives();
        object Define(object var, object val);
        IEnvironment DefinePrim(string name, Primitive.Op operation, int minArgs, int maxArgs);
        IEnvironment DefinePrim(string name, Primitive.Op operation, int numberOfArgs);
    }
}
