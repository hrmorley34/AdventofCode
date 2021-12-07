using System;
using System.Linq;

namespace Day07cs
{
    class Program
    {
        static int Triangular(int n) => n * (n + 1) / 2;

        static (int, int) FindMin(int minx, int maxx, Func<int, int> f)
        {

            (int, int) best = (-1, 0);
            for (int x = minx; x <= maxx; x++)
            {
                int value = f(x);
                if (best.Item1 == -1 || value < best.Item2)
                {
                    best = (x, value);
                }
            }
            return best;
        }

        static void Main(string[] args)
        {
            int[] initialState = Console.ReadLine().Split(',').Select(i => Convert.ToInt32(i)).ToArray();
            int min = initialState.Min(), max = initialState.Max();

            Func<int, int> sum1 = x => initialState.Sum(n => Math.Abs(n - x));
            Console.WriteLine($"Part 1: {FindMin(min, max, sum1).Item2}");

            Func<int, int> sum2 = x => initialState.Sum(n => Triangular(Math.Abs(n - x)));
            Console.WriteLine($"Part 2: {FindMin(min, max, sum2).Item2}");
        }
    }
}
