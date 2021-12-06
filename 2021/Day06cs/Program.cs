using System;
using System.Collections.Generic;
using System.Linq;

namespace Day06cs
{
    class LanternManager
    {
        /// <summary>Mapping of fish's internal timer to number of fish on that stage</summary>
        Dictionary<int, long> Days;

        public LanternManager(int[] initialState)
        {
            Days = new Dictionary<int, long>();
            foreach (int i in initialState)
                AddOne(i);
        }

        public long AddOne(int key) => AddN(key, 1);

        public long AddN(int key, long value)
        {
            long i = 0;
            Days.TryGetValue(key, out i);
            i += value;
            Days[key] = i;
            return i;
        }

        public void Step()
        {
            var OldDays = Days;
            Days = new Dictionary<int, long>();
            foreach (var pair in OldDays)
            {
                if (pair.Key > 0)
                {
                    AddN(pair.Key - 1, pair.Value);
                }
                else
                {
                    AddN(6, pair.Value);
                    AddN(8, pair.Value);
                }
            }
        }

        public long Count()
        {
            return Days.Sum(pair => pair.Value);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            int[] initialState = Console.ReadLine().Split(',').Select(i => Convert.ToInt32(i)).ToArray();
            LanternManager lm = new LanternManager(initialState);

            int iterations = 0;
            while (iterations < 80) { lm.Step(); iterations++; }
            Console.WriteLine($"Part 1: {lm.Count()}");
            while (iterations < 256) { lm.Step(); iterations++; }
            Console.WriteLine($"Part 2: {lm.Count()}");
        }
    }
}
