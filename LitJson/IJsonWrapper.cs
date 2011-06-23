#region Header
/**
 * IJsonWrapper.cs
 *   Interface that represents a type capable of handling all kinds of JSON
 *   data. This is mainly used when mapping objects through JsonMapper, and
 *   it's implemented by JsonData.
 *
 * The authors disclaim copyright to this source code. For more details, see
 * the COPYING file included with this distribution.
 **/
#endregion


using System.Collections;
using System.Collections.Specialized;


namespace LitJson
{
    public interface IJsonWrapper : IList, IOrderedDictionary
    {
        bool IsArray   { get; }
        bool IsBoolean { get; }
        bool IsDouble  { get; }
        bool IsInt     { get; }
        bool IsLong    { get; }
        bool IsObject  { get; }
        bool IsString  { get; }

        bool   GetBoolean ();
        double GetDouble ();
        int    GetInt ();
        long   GetLong ();
        string GetString ();

        void SetBoolean (bool val);
        void SetDouble  (double val);
        void SetInt     (int val);
        void SetLong    (long val);
        void SetString  (string val);

        string ToJson ();
        void   ToJson (JsonWriter writer);
    }
}