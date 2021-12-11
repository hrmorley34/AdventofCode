using System;
using System.Collections.Generic;
using System.Linq;

namespace Day11cs
{
    static class PairExtensions
    {
        public static (int, int) Add(this (int, int) a, (int, int) b)
            => (a.Item1 + b.Item1, a.Item2 + b.Item2);
    }

    class Grid
    {
        public byte[][] grid;
        public int FlashCount = 0;
        public int StepCount = 0;

        protected static readonly (int, int)[] Adjacents = { (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1) };

        public Grid(IEnumerable<IEnumerable<byte>> inputGrid)
        {
            grid = inputGrid.Select(e => e.ToArray()).ToArray();
        }

        public bool InGrid(int x, int y) => y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length;
        public bool InGrid((int, int) xy) => InGrid(xy.Item1, xy.Item2);

        public IEnumerable<(int, int)> FindAdjacents(int x, int y) => FindAdjacents((x, y));
        public IEnumerable<(int, int)> FindAdjacents((int, int) xy)
        {
            foreach ((int, int) adj in Adjacents)
            {
                (int, int) add = xy.Add(adj);
                if (InGrid(add)) yield return add;
            }
        }

        public IEnumerable<(int, int)> IterateOver()
        {
            for (int y = 0; y < grid.Length; y++)
                for (int x = 0; x < grid[y].Length; x++)
                    yield return (x, y);
        }

        public byte Get(int x, int y) => grid[y][x];
        public byte Get((int, int) xy) => Get(xy.Item1, xy.Item2);
        public byte Increment(int x, int y) => ++grid[y][x];
        public byte Increment((int, int) xy) => Increment(xy.Item1, xy.Item2);
        public byte Set(int x, int y, byte value) => grid[y][x] = value;
        public byte Set((int, int) xy, byte value) => Set(xy.Item1, xy.Item2, value);

        public void IncrementFlash((int, int) xy)
        {
            if (Increment(xy) == 10)
            {
                FlashCount++;
                foreach (var newxy in FindAdjacents(xy))
                    IncrementFlash(newxy);
            }
        }

        public void IncreaseAll()
        {
            foreach (var xy in IterateOver())
                IncrementFlash(xy);
        }

        public void HandleZeroes()
        {
            foreach (var xy in IterateOver())
                if (Get(xy) > 9)
                    Set(xy, 0);
        }

        public void Step()
        {
            IncreaseAll();
            HandleZeroes();
            StepCount++;
        }
        public void Step(int n)
        {
            for (int i = 0; i < n; i++)
                Step();
        }

        public bool AllZeroes() => grid.All(e => e.All(b => b == 0));

        public override string ToString()
        {
            return string.Join("\n", grid.Select(e => string.Concat(e.Select(i => i > 9 ? "+" : i.ToString()))));
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            List<IEnumerable<byte>> gridstrings = new List<IEnumerable<byte>>();
            string gridline;
            while ((gridline = Console.ReadLine()) != null)
            {
                gridstrings.Add(gridline.Select(c => byte.Parse(c.ToString())));
            }
            Grid grid = new Grid(gridstrings);

            bool Part1 = false, Part2 = false;
            while (!(Part1 && Part2))
            {
                grid.Step();
                if (grid.StepCount == 100)
                {
                    Console.WriteLine($"Part 1: {grid.FlashCount}");
                    Part1 = true;
                }
                if (grid.AllZeroes())
                {
                    Console.WriteLine($"Part 2: {grid.StepCount}");
                    Part2 = true;
                }
            }
        }
    }
}
