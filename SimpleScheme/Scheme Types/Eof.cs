// <copyright file="Eof.cs" company="Charles Hayden">
// Copyright © 2011 by Charles Hayden.
// </copyright>
namespace SimpleScheme
{
    using System.Text;

    /// <summary>
    /// Represents the Eof object
    /// This type is immutable.
    /// </summary>
    public class Eof : IPrintable, ISchemeObject
    {
        #region Constructors

        /// <summary>
        /// The single Eof instance.
        /// </summary>
        private static readonly Eof eof = new Eof();

        /// <summary>
        /// Prevents a default instance of the <see cref="Eof"/> class from being created. 
        /// </summary>
        private Eof()
        {
        }
        #endregion

        #region Instance
        /// <summary>
        /// Gets the Eof object.
        /// </summary>
        /// <returns>A new Eof object.</returns>
        public static Eof Instance
        {
            get { return eof; }
        }
        #endregion

        #region SchemeType Accessors
        /// <summary>
        /// Gets the name of the type.
        /// </summary>
        public string TypeName
        {
            get { return TypePrimitives.ValueTypeName(TypePrimitives.ValueType.Eof); }
        }
        #endregion

        #region Public Methods
        /// <summary>
        /// Write the Eof object to the string builder.
        /// If not quoted, write nothing.
        /// </summary>
        /// <param name="quoted">Whether to quote.</param>
        /// <param name="buf">The string builder to write to.</param>
        public void PrintString(bool quoted, StringBuilder buf)
        {
            if (quoted)
            {
                buf.Append(this.ToString());
            }
        }

        /// <summary>
        /// Display the value as a string.
        /// Since there is nothing to show, at least give the type.
        /// </summary>
        /// <returns>The undefined type name.</returns>
        public override string ToString()
        {
            return "<eof>";
        }

        #endregion
    }

    /// <summary>
    /// Extensions for Eof
    /// </summary>
    public static class EofExtension
    {
        /// <summary>
        /// Convert object to eof object.
        /// </summary>
        /// <param name="obj">The object to convert.</param>
        /// <returns>The object as an eof object.</returns>
        public static Eof AsEof(this ISchemeObject obj)
        {
            if (obj is Eof)
            {
                return (Eof)obj;
            }

            ErrorHandlers.TypeError(typeof(Eof), obj);
            return null;
        }
    }    
}
