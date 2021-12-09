using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Day09cs
{
    class Grid : IEnumerable<(int, int)>
    {
        public int[,] Items;
        public int Width { get => Items.GetLength(0); }
        public int Height { get => Items.GetLength(1); }

        public static readonly (int, int)[] Directions = new (int, int)[] { (0, 1), (1, 0), (0, -1), (-1, 0) };

        public Grid(string[] gridstrings)
        {
            Items = new int[gridstrings[0].Length, gridstrings.Length];
            for (int y = 0; y < Height; y++)
                for (int x = 0; x < Width; x++)
                    Items[x, y] = gridstrings[y][x] - 0x30;  // '0'-'9' => 0-9
        }

        public int Get(int x, int y) => Items[x, y];
        public int Get((int, int) xy) => Items[xy.Item1, xy.Item2];

        public bool InRange(int x, int y) => x >= 0 && y >= 0 && x < Width && y < Height;
        public bool InRange((int, int) xy) => InRange(xy.Item1, xy.Item2);

        private IEnumerable<int> TryYieldCell(int x, int y)
        {
            if (InRange(x, y)) yield return Items[x, y];
        }

        public IEnumerable<(int, int)> Neighbours(int x, int y)
        {
            foreach ((int dx, int dy) in Directions)
                if (InRange(x + dx, y + dy)) yield return (x + dx, y + dy);
        }
        public IEnumerable<(int, int)> Neighbours((int, int) xy) => Neighbours(xy.Item1, xy.Item2);

        public IEnumerable<int> NeighbourItems(int x, int y) => Neighbours(x, y).Select(Get);
        public IEnumerable<int> NeighbourItems((int, int) xy) => NeighbourItems(xy.Item1, xy.Item2);

        public bool IsLowPoint(int x, int y) => NeighbourItems(x, y).All(i => i > Items[x, y]);
        public bool IsLowPoint((int, int) xy) => IsLowPoint(xy.Item1, xy.Item2);

        public int RiskLevel(int x, int y) => IsLowPoint(x, y) ? Items[x, y] + 1 : 0;
        public int RiskLevel((int, int) xy) => RiskLevel(xy.Item1, xy.Item2);

        public int FindBasin(int x, int y)
        {
            HashSet<(int, int)> SeenLocations = new HashSet<(int, int)>();
            HashSet<(int, int)> ToCheckLocations = new HashSet<(int, int)>();
            Action<(int, int)> AppendQueue = pair => { if (!SeenLocations.Contains(pair)) ToCheckLocations.Add(pair); };

            AppendQueue((x, y));
            int count = 0;
            while (ToCheckLocations.Count != 0)
            {
                (int, int) tpair = ToCheckLocations.OrderBy(Get).First();
                ToCheckLocations.Remove(tpair);
                if (Get(tpair) >= 9) break;
                if (SeenLocations.Contains(tpair)) continue;
                SeenLocations.Add(tpair);
                count++;
                foreach ((int, int) npair in Neighbours(tpair))
                    AppendQueue(npair);
            }
            return count;
        }
        public int FindBasin((int, int) xy) => FindBasin(xy.Item1, xy.Item2);

        public IEnumerator<(int, int)> GetEnumerator()
        {
            for (int y = 0; y < Height; y++)
                for (int x = 0; x < Width; x++)
                    yield return (x, y);
        }

        IEnumerator IEnumerable.GetEnumerator() => (IEnumerator)GetEnumerator();
    }

    class Program
    {
        static void Main(string[] args)
        {
            List<string> gridstrings = new List<string>();
            string gridline;
            while ((gridline = Console.ReadLine()) != null)
            {
                gridstrings.Add(gridline);
            }
            Grid grid = new Grid(gridstrings.ToArray());

            Console.WriteLine($"Part 1: {grid.Sum(pos => grid.RiskLevel(pos))}");
            int p2 = grid.Where(grid.IsLowPoint).Select(grid.FindBasin).OrderByDescending(i => i).Take(3).Aggregate((a, b) => a * b);
            Console.WriteLine($"Part 2: {p2}");
        }
    }
}
