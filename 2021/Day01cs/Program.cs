using System;
using System.Collections.Generic;
using System.Linq;

namespace Day01
{
    class Program
    {
        static void Main(string[] args)
        {
            List<int> numbers = new List<int>();
            string line;
            while ((line = Console.ReadLine()) != null)
            {
                numbers.Add(Convert.ToInt32(line));
            }

            Console.WriteLine($"Part 1: {SumGts(numbers)}");
            Console.WriteLine($"Part 1: {SumGts(GroupArgs(numbers))}");
        }

        static int SumGts(IEnumerable<int> collection)
        {
            int total = 0;
            int lasti = collection.First();
            foreach (int item in collection.Skip(1))
            {
                if (item > lasti) total++;
                lasti = item;
            }
            return total;
        }

        static List<int> GroupArgs(IEnumerable<int> collection)
        {
            List<int> active = new List<int>() { collection.First(), collection.Skip(1).First(), 0 };
            List<int> groups = new List<int>();
            foreach (int item in collection.Skip(2))
            {
                for (int i = 0; i < active.Count; i++)
                {
                    active[i] += item;
                }
                groups.Add(active[0]);
                active.RemoveAt(0);
                active.Add(0);
            }
            return groups;
        }
    }
}
