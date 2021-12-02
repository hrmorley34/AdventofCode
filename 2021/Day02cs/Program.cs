using System;
using System.Collections.Generic;
using System.Linq;

namespace Day02cs
{
    interface IPosition<T> // where T : IPosition
    {
        T ApplyHorizontal(int size);
        T ApplyDepth(int size);
    }

    class Position : IPosition<Position>
    {
        public readonly int Horizontal;
        public readonly int Depth;
        public Position() { Horizontal = 0; Depth = 0; }
        public Position(int h, int d) { Horizontal = h; Depth = d; }
        public Position ApplyHorizontal(int size) => new Position(Horizontal + size, Depth);
        public Position ApplyDepth(int size) => new Position(Horizontal, Depth + size);
        public static Position operator +(Position position, Instruction instruction)
            => instruction.Apply(position);
    }

    class Position2 : IPosition<Position2>
    {
        public readonly int Horizontal;
        public readonly int Depth;
        public readonly int Aim;
        public Position2() { Horizontal = 0; Depth = 0; Aim = 0; }
        public Position2(int h, int d, int a) { Horizontal = h; Depth = d; Aim = a; }
        public Position2 ApplyHorizontal(int size) => new Position2(Horizontal + size, Depth + (size * Aim), Aim);
        public Position2 ApplyDepth(int size) => new Position2(Horizontal, Depth, Aim + size);
        public static Position2 operator +(Position2 position, Instruction instruction)
            => instruction.Apply(position);
    }

    interface Instruction
    {
        TPosition Apply<TPosition>(TPosition position) where TPosition : IPosition<TPosition>;
    }

    class Forward : Instruction
    {
        public readonly int N;
        public Forward(int n) => N = n;
        public TPosition Apply<TPosition>(TPosition position) where TPosition : IPosition<TPosition>
            => position.ApplyHorizontal(N);
    }

    class Down : Instruction
    {
        public readonly int N;
        public Down(int n) => N = n;
        public TPosition Apply<TPosition>(TPosition position) where TPosition : IPosition<TPosition>
            => position.ApplyDepth(N);
    }

    class Up : Instruction
    {
        public readonly int N;
        public Up(int n) => N = n;
        public TPosition Apply<TPosition>(TPosition position) where TPosition : IPosition<TPosition>
            => position.ApplyDepth(-N);
    }

    class Program
    {
        static void Main(string[] args)
        {
            List<Instruction> instructions = new List<Instruction>();
            string line;
            while ((line = Console.ReadLine()) != null)
            {
                string[] lineparts = line.Split();
                if (lineparts[0] == "forward")
                    instructions.Add(new Forward(Convert.ToInt32(lineparts[1])));
                else if (lineparts[0] == "down")
                    instructions.Add(new Down(Convert.ToInt32(lineparts[1])));
                else if (lineparts[0] == "up")
                    instructions.Add(new Up(Convert.ToInt32(lineparts[1])));
                else
                    throw new Exception();
            }
            Position part1 = instructions.Aggregate(new Position(), (p, i) => p + i);
            Position2 part2 = instructions.Aggregate(new Position2(), (p, i) => p + i);
            Console.WriteLine($"Part 1: {part1.Depth * part1.Horizontal}");
            Console.WriteLine($"Part 2: {part2.Depth * part2.Horizontal}");
        }
    }
}