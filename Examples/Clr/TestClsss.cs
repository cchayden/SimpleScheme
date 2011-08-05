public class TestClass
{
    private int lastIndex = 0;
    public string Attr { get; set; }
    public int this[int i]
    {
        get { return lastIndex; }
        set { lastIndex = value; }
    }

    public static string StaticMethod(string str)
    {
        return "Static method: " + str;
    }

    public string MemberMethod(int n)
    {
        return "Member method: " + n;
    }
}