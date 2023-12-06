using System.Diagnostics;
using System.Text.RegularExpressions;

internal class Program
{
    private static long Solve1(long[] seeds, string[] maps)
    {
        maps.Skip(1).ToList().ForEach(map =>
        {
            bool[] transformed = seeds.Select(_ => false).ToArray();
            foreach (var line in map.Split('\n').Where(x => x.Length > 0))
            {
                var x_ = line.Split().Select(long.Parse);
                long dest = x_.ElementAt(0);
                long src = x_.ElementAt(1);
                long range = x_.ElementAt(2);
                long transformation = dest - src;
                for (long i = 0; i < seeds.Count(); i++)
                {
                    if (!transformed[i] && seeds[i] >= src && seeds[i] < src + range)
                    {
                        seeds[i] += transformation;
                        transformed[i] = true;
                    }
                }
            }
        });
        return seeds.Min();
    }

    public class Range
    {
        public Range(long start, long length)
        {
            Start = start;
            Length = length;
        }
        public Range(Range toCopy)
        {
            Start = toCopy.Start;
            Length = toCopy.Length;
        }
        ///<summary>
        ///Returns an inclusive range from start..end
        ///</summary>
        ///
        public static Range FromTo(long start, long end)
        {
            return new Range(start, end - start + 1);
        }

        public long Start { get; set; }
        public long Length { get; set; }
        public long End()
        {
            return this.Start + this.Length - 1;
        }

        /// <returns>
        /// 1-3 ranges based on an intersection. 
        /// Returns an empty array if there is none. The first element
        /// of the returned array will always be the intersection
        /// </returns>
        public Range[] splitOnUnion(Range target)
        {

            // -------<--------->---------
            // -------L#########R---------
            if (this.Start >= target.Start && this.End() <= target.End())
            {
                return [new Range(this)];
            }

            // <--L-->--------R---------->
            // -------L#########R---------
            if (this.Start < target.Start)
            {
                // <-L-R->--------------------
                // -------L#########R---------
                if (this.End() < target.Start)
                {
                    return [];
                    // <--L--><----R---->---------
                    // -------L#########R---------
                }
                else if (this.End() <= target.End())
                {
                    return [FromTo(target.Start, this.End()), FromTo(this.Start, target.Start - 1)];
                    // <--L-->-----------<---R--->
                    // -------L#########R---------
                }
                else
                {
                    return [new Range(target), FromTo(this.Start, target.Start - 1), FromTo(target.End() + 1, this.End())];
                }
            }

            // <--------L--------<---R--->
            // -------L#########R---------
            if (this.End() > target.End())
            {
                // ------------------<--L-R-->
                // -------L#########R---------
                if (this.Start > target.End())
                {
                    return [];
                    // -------<----L----><---R--->
                    // -------L#########R---------
                }
                else if (this.Start >= target.Start)
                {
                    return [FromTo(this.Start, target.End()), FromTo(target.End() + 1, this.End())];
                    // <--L-->-----------<---R--->
                    // -------L#########R---------
                }
                else
                {
                    return [new Range(target), FromTo(this.Start, target.Start - 1), FromTo(target.End() + 1, this.End())];
                }
            }

            throw new UnreachableException($"{this} {target}");
        }
    }

    public class Option<T>
    {
        private readonly T[] data;

        private Option(T[] data)
        {
            this.data = data;
        }

        public static Option<T> Some(T value)
        {
            return new Option<T>([value]);
        }

        public static Option<T> None()
        {
            return new Option<T>([]);
        }

        public bool IsSome()
        {
            return data.Length == 1;
        }
        public bool IsNone()
        {
            return data.Length == 0;
        }

        public T GetValue()
        {
            return data[0];
        }
    }

    public class Tree<T>
    {
        public Option<List<Tree<T>>> children { get; } = Option<List<Tree<T>>>.None();
        public Option<T> leaf { get; } = Option<T>.None();

        public Tree(T leaf)
        {
            this.leaf = Option<T>.Some(leaf);
        }

        public Tree(List<Tree<T>> children)
        {
            this.children = Option<List<Tree<T>>>.Some(children);
        }

        public IEnumerable<T> flatten()
        {
            if (this.leaf.IsSome())
            {
                return [this.leaf.GetValue()];
            }
            else
            {
                return this.children.GetValue().SelectMany(x => x.flatten());
            }
        }
    }

    private static long Solve2(List<Range> seeds, List<List<(long, long, long)>> maps)
    {
        foreach (var map in maps)
        {
            //second tuple argument signifies if a range got updated
            //false if no, true if yes
            var seedRanges = seeds.Select(x => (x, false)).ToList();

            foreach ((long dest, long src, long range) in map)
            {
                long transformation = dest - src;
                Range srcRange = new Range(src, range);
                var rangesToAdd = new List<(Range, bool)> { };

                for (int i = 0; i < seedRanges.Count(); i++) 
                {
                  Range seedRange = seedRanges[i].Item1;
                  bool updated = seedRanges[i].Item2;

                  if (updated) continue;

                  var splits = seedRange.splitOnUnion(srcRange);
                  if (splits.Count() == 0) continue; 

                  seedRanges[i] = (new Range(splits[0].Start + transformation, splits[0].Length) , true);
                  rangesToAdd.AddRange(splits.Skip(1).Select(x => (x,false)));
                }

                foreach (var i in rangesToAdd)
                {
                    seedRanges.Add(i);
                }
            }
            seeds = seedRanges.Select(x => x.Item1).ToList();
        }
        return seeds.MinBy(x => x.Start)!.Start;
    }

    private static void Main(string[] args)
    {
        string input;
        using (var sr = new StreamReader("input.txt"))
        {
            input = sr.ReadToEnd();
        }
        var maps = Regex.Split(input, "^\\s.*$", RegexOptions.Multiline);
        long[] seeds = Regex.Replace(maps[0], "seeds:", "").Split().SelectMany(s =>
        {
            long n;
            if (long.TryParse(s, out n))
            {
                return new long[] { n };
            }
            return [];
        }).ToArray();

        List<List<(long, long, long)>> mapValues = maps.Skip(1).Select(map =>
        {
            return map.Split('\n').Where(x => x.Length > 0).Select(x =>
            {
                var x_ = x.Split().Select(long.Parse);
                long dest = x_.ElementAt(0);
                long src = x_.ElementAt(1);
                long range = x_.ElementAt(2);
                return (dest, src, range);
            }).ToList();
        }).ToList();

        List<Range> seedRanges = seeds.Chunk(2).Select(x => new Range(x[0], x[1])).ToList();
        Console.WriteLine($"Part1: {Solve1(seeds, maps)}");

        Console.WriteLine($"Part2: {Solve2(seedRanges, mapValues)}");
    }
}
