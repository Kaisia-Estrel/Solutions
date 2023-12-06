using System.Text;

static class Utils
{
    public static string ShowEnumerable<T>(IEnumerable<T> xs)
    {
        if (xs.Count() == 0)
        {
            return "[]";
        }

        StringBuilder str = new StringBuilder();
        str.Append("[");
        foreach (var i in xs.SkipLast(1))
        {
            if (i == null) return str.ToString();

            if (i.GetType() == typeof(string))
            {
                str.Append($"\"{i}\", ");
            }
            else
            {
                str.Append($"{i}, ");
            }
        }
        str.Append(xs.Last() + "]");
        return str.ToString();
    }

    public static void PrintEnumerable<T>(IEnumerable<T> xs)
    {
        Console.WriteLine(ShowEnumerable(xs));
    }
}
