intxt = """\
Before: [2, 3, 2, 2]
0 3 3 0
After:  [0, 3, 2, 2]

Before: [1, 1, 2, 3]
6 0 2 0
After:  [0, 1, 2, 3]

Before: [1, 0, 2, 2]
6 0 2 0
After:  [0, 0, 2, 2]

Before: [1, 1, 1, 1]
11 2 1 0
After:  [2, 1, 1, 1]

Before: [3, 0, 0, 2]
0 3 3 2
After:  [3, 0, 0, 2]

Before: [1, 1, 2, 2]
9 1 0 2
After:  [1, 1, 1, 2]

Before: [3, 2, 1, 1]
5 2 1 1
After:  [3, 2, 1, 1]

Before: [1, 1, 0, 3]
7 1 3 0
After:  [0, 1, 0, 3]

Before: [1, 2, 1, 3]
5 2 1 0
After:  [2, 2, 1, 3]

Before: [0, 2, 2, 0]
8 0 0 0
After:  [0, 2, 2, 0]

Before: [2, 0, 0, 1]
3 0 3 0
After:  [1, 0, 0, 1]

Before: [3, 1, 2, 2]
4 1 3 1
After:  [3, 0, 2, 2]

Before: [2, 2, 1, 1]
5 2 1 1
After:  [2, 2, 1, 1]

Before: [1, 1, 2, 2]
6 0 2 2
After:  [1, 1, 0, 2]

Before: [1, 1, 1, 2]
4 1 3 0
After:  [0, 1, 1, 2]

Before: [2, 1, 3, 1]
13 1 3 0
After:  [1, 1, 3, 1]

Before: [0, 1, 2, 1]
13 1 3 1
After:  [0, 1, 2, 1]

Before: [2, 1, 0, 2]
4 1 3 1
After:  [2, 0, 0, 2]

Before: [2, 1, 0, 1]
2 0 1 3
After:  [2, 1, 0, 1]

Before: [3, 1, 2, 1]
12 1 2 2
After:  [3, 1, 0, 1]

Before: [1, 1, 3, 2]
4 1 3 3
After:  [1, 1, 3, 0]

Before: [2, 2, 1, 3]
7 1 3 0
After:  [0, 2, 1, 3]

Before: [1, 3, 2, 1]
6 0 2 1
After:  [1, 0, 2, 1]

Before: [2, 1, 2, 1]
13 1 3 1
After:  [2, 1, 2, 1]

Before: [2, 1, 3, 0]
14 2 0 3
After:  [2, 1, 3, 1]

Before: [1, 1, 2, 3]
6 0 2 3
After:  [1, 1, 2, 0]

Before: [1, 1, 1, 3]
11 2 1 2
After:  [1, 1, 2, 3]

Before: [2, 2, 3, 2]
0 3 3 0
After:  [0, 2, 3, 2]

Before: [1, 2, 0, 2]
1 0 2 3
After:  [1, 2, 0, 0]

Before: [2, 1, 0, 0]
2 0 1 3
After:  [2, 1, 0, 1]

Before: [0, 2, 1, 1]
5 2 1 3
After:  [0, 2, 1, 2]

Before: [0, 3, 2, 1]
10 3 2 3
After:  [0, 3, 2, 1]

Before: [3, 3, 2, 2]
0 3 3 0
After:  [0, 3, 2, 2]

Before: [1, 1, 2, 0]
12 1 2 0
After:  [0, 1, 2, 0]

Before: [0, 2, 1, 3]
5 2 1 0
After:  [2, 2, 1, 3]

Before: [0, 3, 2, 1]
8 0 0 0
After:  [0, 3, 2, 1]

Before: [1, 1, 1, 3]
11 2 1 1
After:  [1, 2, 1, 3]

Before: [0, 1, 1, 2]
11 2 1 2
After:  [0, 1, 2, 2]

Before: [1, 1, 1, 1]
13 1 3 1
After:  [1, 1, 1, 1]

Before: [1, 3, 0, 0]
1 0 2 1
After:  [1, 0, 0, 0]

Before: [2, 2, 3, 1]
14 2 0 1
After:  [2, 1, 3, 1]

Before: [0, 3, 0, 3]
8 0 0 3
After:  [0, 3, 0, 0]

Before: [0, 0, 1, 1]
8 0 0 2
After:  [0, 0, 0, 1]

Before: [0, 3, 2, 1]
8 0 0 2
After:  [0, 3, 0, 1]

Before: [2, 1, 2, 3]
12 1 2 1
After:  [2, 0, 2, 3]

Before: [3, 2, 2, 3]
14 2 1 2
After:  [3, 2, 1, 3]

Before: [2, 2, 3, 0]
15 2 2 2
After:  [2, 2, 1, 0]

Before: [2, 3, 3, 2]
15 2 2 0
After:  [1, 3, 3, 2]

Before: [1, 1, 0, 0]
1 0 2 3
After:  [1, 1, 0, 0]

Before: [3, 2, 2, 2]
0 3 3 3
After:  [3, 2, 2, 0]

Before: [1, 3, 2, 2]
6 0 2 3
After:  [1, 3, 2, 0]

Before: [2, 1, 0, 1]
3 0 3 1
After:  [2, 1, 0, 1]

Before: [3, 3, 1, 3]
7 2 3 0
After:  [0, 3, 1, 3]

Before: [0, 2, 1, 0]
5 2 1 3
After:  [0, 2, 1, 2]

Before: [1, 1, 1, 2]
4 1 3 2
After:  [1, 1, 0, 2]

Before: [0, 3, 1, 2]
8 0 0 1
After:  [0, 0, 1, 2]

Before: [2, 1, 3, 3]
7 1 3 0
After:  [0, 1, 3, 3]

Before: [3, 2, 2, 1]
10 3 2 0
After:  [1, 2, 2, 1]

Before: [2, 1, 0, 1]
3 0 3 3
After:  [2, 1, 0, 1]

Before: [2, 1, 1, 1]
13 1 3 2
After:  [2, 1, 1, 1]

Before: [2, 2, 0, 3]
7 1 3 1
After:  [2, 0, 0, 3]

Before: [2, 2, 0, 1]
3 0 3 0
After:  [1, 2, 0, 1]

Before: [2, 2, 3, 1]
3 0 3 3
After:  [2, 2, 3, 1]

Before: [1, 2, 0, 0]
1 0 2 1
After:  [1, 0, 0, 0]

Before: [2, 2, 2, 2]
14 3 2 1
After:  [2, 0, 2, 2]

Before: [3, 1, 1, 2]
4 1 3 1
After:  [3, 0, 1, 2]

Before: [2, 1, 1, 1]
2 0 1 3
After:  [2, 1, 1, 1]

Before: [1, 1, 0, 0]
1 0 2 1
After:  [1, 0, 0, 0]

Before: [1, 3, 0, 2]
1 0 2 1
After:  [1, 0, 0, 2]

Before: [1, 1, 1, 3]
9 1 0 2
After:  [1, 1, 1, 3]

Before: [3, 1, 2, 2]
12 1 2 2
After:  [3, 1, 0, 2]

Before: [0, 1, 2, 1]
12 1 2 2
After:  [0, 1, 0, 1]

Before: [3, 2, 0, 3]
7 1 3 3
After:  [3, 2, 0, 0]

Before: [2, 1, 2, 3]
7 2 3 2
After:  [2, 1, 0, 3]

Before: [3, 1, 3, 1]
13 1 3 0
After:  [1, 1, 3, 1]

Before: [2, 1, 1, 1]
11 2 1 0
After:  [2, 1, 1, 1]

Before: [0, 1, 1, 0]
11 2 1 3
After:  [0, 1, 1, 2]

Before: [2, 1, 3, 3]
7 1 3 2
After:  [2, 1, 0, 3]

Before: [2, 3, 2, 1]
10 3 2 1
After:  [2, 1, 2, 1]

Before: [1, 1, 2, 2]
4 1 3 1
After:  [1, 0, 2, 2]

Before: [1, 3, 0, 1]
1 0 2 0
After:  [0, 3, 0, 1]

Before: [1, 3, 0, 3]
1 0 2 3
After:  [1, 3, 0, 0]

Before: [2, 3, 3, 1]
3 0 3 1
After:  [2, 1, 3, 1]

Before: [2, 1, 1, 2]
11 2 1 3
After:  [2, 1, 1, 2]

Before: [2, 1, 1, 1]
2 0 1 1
After:  [2, 1, 1, 1]

Before: [3, 1, 2, 2]
4 1 3 0
After:  [0, 1, 2, 2]

Before: [2, 0, 2, 1]
10 3 2 1
After:  [2, 1, 2, 1]

Before: [1, 3, 0, 1]
1 0 2 1
After:  [1, 0, 0, 1]

Before: [1, 1, 0, 2]
9 1 0 0
After:  [1, 1, 0, 2]

Before: [2, 3, 2, 1]
3 0 3 2
After:  [2, 3, 1, 1]

Before: [1, 2, 2, 1]
0 3 3 3
After:  [1, 2, 2, 0]

Before: [3, 1, 2, 2]
12 1 2 1
After:  [3, 0, 2, 2]

Before: [0, 2, 3, 1]
8 0 0 1
After:  [0, 0, 3, 1]

Before: [0, 0, 2, 1]
10 3 2 2
After:  [0, 0, 1, 1]

Before: [3, 2, 1, 3]
15 0 0 3
After:  [3, 2, 1, 1]

Before: [1, 3, 2, 2]
6 0 2 2
After:  [1, 3, 0, 2]

Before: [1, 2, 2, 3]
6 0 2 3
After:  [1, 2, 2, 0]

Before: [1, 1, 3, 2]
4 1 3 2
After:  [1, 1, 0, 2]

Before: [1, 2, 2, 1]
10 3 2 3
After:  [1, 2, 2, 1]

Before: [1, 2, 2, 1]
6 0 2 2
After:  [1, 2, 0, 1]

Before: [1, 2, 1, 3]
7 2 3 1
After:  [1, 0, 1, 3]

Before: [1, 2, 2, 1]
10 3 2 0
After:  [1, 2, 2, 1]

Before: [2, 3, 3, 1]
3 0 3 3
After:  [2, 3, 3, 1]

Before: [2, 3, 2, 3]
14 2 0 2
After:  [2, 3, 1, 3]

Before: [2, 1, 3, 1]
2 0 1 3
After:  [2, 1, 3, 1]

Before: [0, 3, 3, 0]
8 0 0 1
After:  [0, 0, 3, 0]

Before: [2, 1, 1, 3]
7 2 3 2
After:  [2, 1, 0, 3]

Before: [0, 2, 2, 1]
10 3 2 3
After:  [0, 2, 2, 1]

Before: [3, 2, 1, 3]
5 2 1 3
After:  [3, 2, 1, 2]

Before: [3, 1, 1, 2]
0 3 3 2
After:  [3, 1, 0, 2]

Before: [0, 3, 1, 3]
7 2 3 3
After:  [0, 3, 1, 0]

Before: [2, 0, 2, 1]
10 3 2 3
After:  [2, 0, 2, 1]

Before: [2, 2, 1, 0]
5 2 1 2
After:  [2, 2, 2, 0]

Before: [2, 1, 2, 2]
4 1 3 3
After:  [2, 1, 2, 0]

Before: [1, 3, 1, 1]
0 2 3 2
After:  [1, 3, 0, 1]

Before: [1, 1, 0, 3]
1 0 2 3
After:  [1, 1, 0, 0]

Before: [1, 0, 0, 3]
1 0 2 2
After:  [1, 0, 0, 3]

Before: [2, 1, 1, 0]
11 2 1 0
After:  [2, 1, 1, 0]

Before: [2, 0, 0, 1]
3 0 3 3
After:  [2, 0, 0, 1]

Before: [3, 3, 0, 1]
14 0 2 2
After:  [3, 3, 1, 1]

Before: [0, 1, 2, 0]
8 0 0 1
After:  [0, 0, 2, 0]

Before: [2, 0, 1, 1]
3 0 3 2
After:  [2, 0, 1, 1]

Before: [1, 3, 2, 0]
6 0 2 1
After:  [1, 0, 2, 0]

Before: [3, 3, 2, 0]
2 0 2 3
After:  [3, 3, 2, 1]

Before: [2, 1, 0, 1]
13 1 3 2
After:  [2, 1, 1, 1]

Before: [1, 1, 2, 1]
13 1 3 2
After:  [1, 1, 1, 1]

Before: [1, 3, 2, 0]
6 0 2 2
After:  [1, 3, 0, 0]

Before: [3, 1, 3, 2]
4 1 3 1
After:  [3, 0, 3, 2]

Before: [2, 3, 2, 2]
15 0 0 3
After:  [2, 3, 2, 1]

Before: [2, 3, 2, 1]
3 0 3 3
After:  [2, 3, 2, 1]

Before: [2, 1, 1, 2]
4 1 3 0
After:  [0, 1, 1, 2]

Before: [1, 1, 1, 1]
13 1 3 0
After:  [1, 1, 1, 1]

Before: [3, 1, 1, 0]
11 2 1 2
After:  [3, 1, 2, 0]

Before: [3, 1, 1, 1]
11 2 1 0
After:  [2, 1, 1, 1]

Before: [3, 1, 0, 2]
4 1 3 0
After:  [0, 1, 0, 2]

Before: [3, 3, 1, 3]
15 0 0 3
After:  [3, 3, 1, 1]

Before: [1, 2, 2, 1]
10 3 2 1
After:  [1, 1, 2, 1]

Before: [1, 1, 1, 0]
11 2 1 3
After:  [1, 1, 1, 2]

Before: [1, 1, 1, 2]
11 2 1 0
After:  [2, 1, 1, 2]

Before: [3, 2, 2, 2]
14 2 1 2
After:  [3, 2, 1, 2]

Before: [0, 0, 3, 3]
15 2 2 3
After:  [0, 0, 3, 1]

Before: [0, 3, 2, 2]
0 3 3 0
After:  [0, 3, 2, 2]

Before: [1, 0, 2, 1]
10 3 2 1
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 2]
14 3 2 1
After:  [2, 0, 2, 2]

Before: [1, 0, 0, 3]
1 0 2 1
After:  [1, 0, 0, 3]

Before: [3, 2, 1, 3]
7 2 3 1
After:  [3, 0, 1, 3]

Before: [3, 1, 1, 2]
11 2 1 0
After:  [2, 1, 1, 2]

Before: [1, 3, 2, 1]
6 0 2 0
After:  [0, 3, 2, 1]

Before: [2, 0, 3, 1]
3 0 3 0
After:  [1, 0, 3, 1]

Before: [3, 1, 2, 2]
12 1 2 0
After:  [0, 1, 2, 2]

Before: [3, 1, 2, 0]
12 1 2 3
After:  [3, 1, 2, 0]

Before: [2, 1, 2, 0]
2 0 1 3
After:  [2, 1, 2, 1]

Before: [1, 1, 3, 1]
14 2 3 2
After:  [1, 1, 0, 1]

Before: [1, 3, 2, 3]
6 0 2 0
After:  [0, 3, 2, 3]

Before: [1, 1, 2, 3]
12 1 2 0
After:  [0, 1, 2, 3]

Before: [3, 0, 2, 1]
10 3 2 1
After:  [3, 1, 2, 1]

Before: [1, 0, 2, 0]
6 0 2 1
After:  [1, 0, 2, 0]

Before: [2, 3, 1, 3]
7 2 3 2
After:  [2, 3, 0, 3]

Before: [1, 1, 1, 1]
11 2 1 3
After:  [1, 1, 1, 2]

Before: [2, 1, 2, 2]
2 0 1 0
After:  [1, 1, 2, 2]

Before: [1, 2, 1, 3]
7 2 3 3
After:  [1, 2, 1, 0]

Before: [1, 1, 2, 2]
12 1 2 0
After:  [0, 1, 2, 2]

Before: [2, 0, 2, 1]
10 3 2 2
After:  [2, 0, 1, 1]

Before: [0, 1, 2, 3]
12 1 2 2
After:  [0, 1, 0, 3]

Before: [2, 1, 1, 3]
11 2 1 0
After:  [2, 1, 1, 3]

Before: [2, 1, 3, 1]
13 1 3 3
After:  [2, 1, 3, 1]

Before: [0, 2, 1, 1]
8 0 0 1
After:  [0, 0, 1, 1]

Before: [1, 0, 0, 2]
1 0 2 1
After:  [1, 0, 0, 2]

Before: [2, 1, 3, 3]
2 0 1 1
After:  [2, 1, 3, 3]

Before: [0, 1, 2, 2]
4 1 3 2
After:  [0, 1, 0, 2]

Before: [1, 1, 2, 1]
13 1 3 0
After:  [1, 1, 2, 1]

Before: [1, 1, 3, 0]
9 1 0 1
After:  [1, 1, 3, 0]

Before: [1, 1, 0, 1]
1 0 2 1
After:  [1, 0, 0, 1]

Before: [2, 2, 3, 1]
3 0 3 1
After:  [2, 1, 3, 1]

Before: [3, 2, 1, 2]
5 2 1 0
After:  [2, 2, 1, 2]

Before: [1, 1, 2, 0]
12 1 2 1
After:  [1, 0, 2, 0]

Before: [3, 0, 2, 3]
2 0 2 3
After:  [3, 0, 2, 1]

Before: [2, 1, 3, 3]
2 0 1 2
After:  [2, 1, 1, 3]

Before: [3, 1, 3, 1]
15 0 0 0
After:  [1, 1, 3, 1]

Before: [0, 1, 3, 2]
4 1 3 1
After:  [0, 0, 3, 2]

Before: [3, 2, 3, 3]
15 2 0 0
After:  [1, 2, 3, 3]

Before: [1, 3, 3, 1]
0 3 3 0
After:  [0, 3, 3, 1]

Before: [0, 0, 2, 3]
7 2 3 0
After:  [0, 0, 2, 3]

Before: [0, 2, 1, 3]
7 2 3 2
After:  [0, 2, 0, 3]

Before: [3, 0, 2, 1]
2 0 2 0
After:  [1, 0, 2, 1]

Before: [2, 2, 2, 1]
10 3 2 2
After:  [2, 2, 1, 1]

Before: [1, 2, 0, 1]
1 0 2 0
After:  [0, 2, 0, 1]

Before: [1, 2, 0, 0]
1 0 2 2
After:  [1, 2, 0, 0]

Before: [3, 1, 2, 1]
2 0 2 1
After:  [3, 1, 2, 1]

Before: [0, 0, 3, 1]
8 0 0 1
After:  [0, 0, 3, 1]

Before: [0, 1, 1, 2]
11 2 1 3
After:  [0, 1, 1, 2]

Before: [0, 1, 3, 1]
13 1 3 2
After:  [0, 1, 1, 1]

Before: [1, 1, 1, 2]
11 2 1 2
After:  [1, 1, 2, 2]

Before: [2, 0, 3, 1]
3 0 3 3
After:  [2, 0, 3, 1]

Before: [0, 2, 1, 2]
8 0 0 0
After:  [0, 2, 1, 2]

Before: [1, 0, 2, 1]
6 0 2 1
After:  [1, 0, 2, 1]

Before: [1, 1, 0, 2]
4 1 3 3
After:  [1, 1, 0, 0]

Before: [2, 2, 1, 1]
3 0 3 2
After:  [2, 2, 1, 1]

Before: [1, 2, 1, 2]
5 2 1 2
After:  [1, 2, 2, 2]

Before: [2, 0, 2, 1]
3 0 3 3
After:  [2, 0, 2, 1]

Before: [2, 1, 0, 1]
3 0 3 2
After:  [2, 1, 1, 1]

Before: [2, 2, 1, 2]
5 2 1 1
After:  [2, 2, 1, 2]

Before: [1, 1, 2, 2]
9 1 0 3
After:  [1, 1, 2, 1]

Before: [2, 2, 1, 3]
15 0 0 3
After:  [2, 2, 1, 1]

Before: [3, 1, 0, 1]
13 1 3 3
After:  [3, 1, 0, 1]

Before: [3, 3, 2, 1]
10 3 2 2
After:  [3, 3, 1, 1]

Before: [0, 1, 3, 2]
4 1 3 3
After:  [0, 1, 3, 0]

Before: [0, 1, 1, 0]
11 2 1 2
After:  [0, 1, 2, 0]

Before: [3, 1, 3, 1]
14 3 1 0
After:  [0, 1, 3, 1]

Before: [0, 1, 3, 3]
8 0 0 3
After:  [0, 1, 3, 0]

Before: [0, 1, 2, 1]
10 3 2 0
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 1]
3 0 3 2
After:  [2, 1, 1, 1]

Before: [0, 2, 1, 3]
5 2 1 3
After:  [0, 2, 1, 2]

Before: [1, 0, 0, 3]
1 0 2 0
After:  [0, 0, 0, 3]

Before: [2, 3, 0, 1]
3 0 3 0
After:  [1, 3, 0, 1]

Before: [2, 1, 2, 1]
12 1 2 1
After:  [2, 0, 2, 1]

Before: [2, 1, 3, 2]
4 1 3 0
After:  [0, 1, 3, 2]

Before: [1, 2, 1, 0]
5 2 1 3
After:  [1, 2, 1, 2]

Before: [3, 1, 3, 1]
13 1 3 1
After:  [3, 1, 3, 1]

Before: [1, 2, 1, 0]
5 2 1 1
After:  [1, 2, 1, 0]

Before: [3, 1, 2, 1]
10 3 2 1
After:  [3, 1, 2, 1]

Before: [1, 1, 1, 1]
13 1 3 2
After:  [1, 1, 1, 1]

Before: [2, 1, 2, 1]
13 1 3 2
After:  [2, 1, 1, 1]

Before: [1, 2, 1, 3]
7 1 3 1
After:  [1, 0, 1, 3]

Before: [0, 0, 2, 2]
14 3 2 3
After:  [0, 0, 2, 0]

Before: [2, 2, 1, 3]
15 0 0 1
After:  [2, 1, 1, 3]

Before: [2, 1, 3, 2]
4 1 3 1
After:  [2, 0, 3, 2]

Before: [1, 2, 1, 3]
5 2 1 2
After:  [1, 2, 2, 3]

Before: [2, 2, 1, 0]
5 2 1 3
After:  [2, 2, 1, 2]

Before: [2, 0, 2, 1]
3 0 3 2
After:  [2, 0, 1, 1]

Before: [1, 0, 0, 1]
1 0 2 0
After:  [0, 0, 0, 1]

Before: [2, 1, 1, 0]
15 0 0 0
After:  [1, 1, 1, 0]

Before: [0, 0, 3, 3]
8 0 0 0
After:  [0, 0, 3, 3]

Before: [1, 1, 1, 2]
4 1 3 3
After:  [1, 1, 1, 0]

Before: [1, 2, 0, 3]
1 0 2 1
After:  [1, 0, 0, 3]

Before: [1, 1, 0, 2]
9 1 0 1
After:  [1, 1, 0, 2]

Before: [3, 1, 1, 1]
11 2 1 3
After:  [3, 1, 1, 2]

Before: [1, 1, 0, 3]
7 1 3 1
After:  [1, 0, 0, 3]

Before: [1, 1, 1, 3]
7 1 3 2
After:  [1, 1, 0, 3]

Before: [1, 1, 2, 3]
6 0 2 1
After:  [1, 0, 2, 3]

Before: [2, 1, 1, 2]
4 1 3 3
After:  [2, 1, 1, 0]

Before: [2, 2, 2, 3]
7 1 3 2
After:  [2, 2, 0, 3]

Before: [1, 3, 2, 1]
0 3 3 3
After:  [1, 3, 2, 0]

Before: [0, 0, 3, 3]
8 0 0 3
After:  [0, 0, 3, 0]

Before: [3, 1, 3, 1]
15 0 0 1
After:  [3, 1, 3, 1]

Before: [1, 0, 0, 2]
1 0 2 2
After:  [1, 0, 0, 2]

Before: [0, 0, 0, 1]
0 3 3 1
After:  [0, 0, 0, 1]

Before: [1, 1, 1, 2]
9 1 0 0
After:  [1, 1, 1, 2]

Before: [1, 3, 0, 1]
1 0 2 2
After:  [1, 3, 0, 1]

Before: [1, 1, 3, 3]
9 1 0 0
After:  [1, 1, 3, 3]

Before: [2, 1, 3, 1]
13 1 3 2
After:  [2, 1, 1, 1]

Before: [2, 1, 3, 2]
4 1 3 3
After:  [2, 1, 3, 0]

Before: [2, 1, 2, 1]
13 1 3 3
After:  [2, 1, 2, 1]

Before: [1, 0, 2, 2]
6 0 2 1
After:  [1, 0, 2, 2]

Before: [1, 1, 2, 1]
10 3 2 2
After:  [1, 1, 1, 1]

Before: [3, 2, 1, 3]
5 2 1 2
After:  [3, 2, 2, 3]

Before: [0, 1, 2, 0]
12 1 2 2
After:  [0, 1, 0, 0]

Before: [2, 1, 1, 3]
2 0 1 0
After:  [1, 1, 1, 3]

Before: [1, 2, 2, 3]
14 2 1 2
After:  [1, 2, 1, 3]

Before: [1, 2, 0, 3]
1 0 2 0
After:  [0, 2, 0, 3]

Before: [0, 1, 2, 2]
8 0 0 2
After:  [0, 1, 0, 2]

Before: [0, 2, 1, 0]
5 2 1 1
After:  [0, 2, 1, 0]

Before: [2, 0, 0, 1]
15 0 0 2
After:  [2, 0, 1, 1]

Before: [2, 2, 1, 3]
5 2 1 0
After:  [2, 2, 1, 3]

Before: [3, 2, 2, 1]
10 3 2 2
After:  [3, 2, 1, 1]

Before: [0, 3, 2, 2]
14 3 2 2
After:  [0, 3, 0, 2]

Before: [1, 2, 0, 1]
1 0 2 2
After:  [1, 2, 0, 1]

Before: [0, 1, 1, 0]
11 2 1 0
After:  [2, 1, 1, 0]

Before: [1, 2, 2, 3]
14 2 1 3
After:  [1, 2, 2, 1]

Before: [2, 1, 3, 1]
3 0 3 3
After:  [2, 1, 3, 1]

Before: [0, 1, 2, 3]
7 1 3 3
After:  [0, 1, 2, 0]

Before: [2, 1, 2, 2]
2 0 1 1
After:  [2, 1, 2, 2]

Before: [2, 2, 1, 0]
5 2 1 1
After:  [2, 2, 1, 0]

Before: [3, 2, 1, 3]
5 2 1 0
After:  [2, 2, 1, 3]

Before: [1, 1, 2, 1]
0 3 3 1
After:  [1, 0, 2, 1]

Before: [1, 0, 2, 1]
6 0 2 3
After:  [1, 0, 2, 0]

Before: [1, 3, 0, 2]
1 0 2 0
After:  [0, 3, 0, 2]

Before: [0, 1, 1, 3]
11 2 1 2
After:  [0, 1, 2, 3]

Before: [1, 1, 3, 3]
9 1 0 1
After:  [1, 1, 3, 3]

Before: [3, 1, 2, 3]
12 1 2 1
After:  [3, 0, 2, 3]

Before: [0, 1, 1, 1]
13 1 3 0
After:  [1, 1, 1, 1]

Before: [1, 1, 2, 3]
9 1 0 1
After:  [1, 1, 2, 3]

Before: [0, 3, 1, 3]
7 2 3 0
After:  [0, 3, 1, 3]

Before: [3, 1, 2, 1]
13 1 3 2
After:  [3, 1, 1, 1]

Before: [1, 0, 1, 3]
7 2 3 1
After:  [1, 0, 1, 3]

Before: [1, 1, 0, 3]
1 0 2 0
After:  [0, 1, 0, 3]

Before: [2, 1, 2, 2]
12 1 2 2
After:  [2, 1, 0, 2]

Before: [3, 0, 1, 3]
14 3 0 0
After:  [1, 0, 1, 3]

Before: [3, 1, 3, 3]
7 1 3 3
After:  [3, 1, 3, 0]

Before: [1, 1, 0, 0]
1 0 2 0
After:  [0, 1, 0, 0]

Before: [1, 1, 1, 1]
0 2 3 2
After:  [1, 1, 0, 1]

Before: [2, 1, 0, 1]
2 0 1 2
After:  [2, 1, 1, 1]

Before: [1, 1, 2, 1]
14 3 1 1
After:  [1, 0, 2, 1]

Before: [0, 0, 2, 3]
7 2 3 3
After:  [0, 0, 2, 0]

Before: [3, 2, 0, 0]
14 0 2 1
After:  [3, 1, 0, 0]

Before: [0, 0, 2, 3]
8 0 0 0
After:  [0, 0, 2, 3]

Before: [3, 1, 1, 0]
11 2 1 1
After:  [3, 2, 1, 0]

Before: [1, 2, 1, 1]
5 2 1 2
After:  [1, 2, 2, 1]

Before: [0, 2, 1, 3]
7 2 3 3
After:  [0, 2, 1, 0]

Before: [3, 1, 2, 2]
15 0 0 3
After:  [3, 1, 2, 1]

Before: [0, 0, 0, 2]
8 0 0 2
After:  [0, 0, 0, 2]

Before: [3, 1, 3, 1]
13 1 3 2
After:  [3, 1, 1, 1]

Before: [1, 1, 2, 3]
9 1 0 2
After:  [1, 1, 1, 3]

Before: [1, 2, 0, 2]
1 0 2 2
After:  [1, 2, 0, 2]

Before: [2, 1, 2, 3]
2 0 1 3
After:  [2, 1, 2, 1]

Before: [1, 2, 0, 3]
1 0 2 2
After:  [1, 2, 0, 3]

Before: [1, 0, 2, 0]
6 0 2 3
After:  [1, 0, 2, 0]

Before: [1, 0, 3, 1]
0 3 3 2
After:  [1, 0, 0, 1]

Before: [1, 3, 2, 1]
6 0 2 3
After:  [1, 3, 2, 0]

Before: [1, 1, 1, 1]
9 1 0 3
After:  [1, 1, 1, 1]

Before: [0, 3, 2, 1]
0 3 3 1
After:  [0, 0, 2, 1]

Before: [1, 1, 3, 1]
13 1 3 3
After:  [1, 1, 3, 1]

Before: [2, 2, 0, 3]
7 1 3 0
After:  [0, 2, 0, 3]

Before: [0, 3, 2, 1]
0 3 3 0
After:  [0, 3, 2, 1]

Before: [1, 0, 0, 1]
1 0 2 2
After:  [1, 0, 0, 1]

Before: [2, 1, 2, 1]
2 0 1 2
After:  [2, 1, 1, 1]

Before: [1, 2, 2, 2]
6 0 2 2
After:  [1, 2, 0, 2]

Before: [0, 1, 1, 1]
13 1 3 3
After:  [0, 1, 1, 1]

Before: [2, 1, 1, 0]
11 2 1 2
After:  [2, 1, 2, 0]

Before: [0, 1, 3, 1]
13 1 3 1
After:  [0, 1, 3, 1]

Before: [3, 2, 0, 2]
0 3 3 1
After:  [3, 0, 0, 2]

Before: [1, 1, 2, 1]
10 3 2 3
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 1]
13 1 3 0
After:  [1, 1, 2, 1]

Before: [2, 1, 0, 1]
13 1 3 1
After:  [2, 1, 0, 1]

Before: [2, 1, 2, 2]
12 1 2 3
After:  [2, 1, 2, 0]

Before: [0, 1, 2, 0]
12 1 2 1
After:  [0, 0, 2, 0]

Before: [3, 1, 2, 2]
4 1 3 2
After:  [3, 1, 0, 2]

Before: [1, 1, 0, 2]
1 0 2 1
After:  [1, 0, 0, 2]

Before: [0, 2, 1, 1]
0 2 3 2
After:  [0, 2, 0, 1]

Before: [1, 1, 2, 0]
6 0 2 0
After:  [0, 1, 2, 0]

Before: [0, 3, 1, 2]
8 0 0 3
After:  [0, 3, 1, 0]

Before: [1, 3, 0, 0]
1 0 2 2
After:  [1, 3, 0, 0]

Before: [1, 1, 2, 0]
12 1 2 2
After:  [1, 1, 0, 0]

Before: [2, 1, 0, 2]
0 3 3 1
After:  [2, 0, 0, 2]

Before: [0, 3, 3, 3]
8 0 0 1
After:  [0, 0, 3, 3]

Before: [3, 3, 0, 1]
0 3 3 0
After:  [0, 3, 0, 1]

Before: [3, 1, 1, 2]
4 1 3 3
After:  [3, 1, 1, 0]

Before: [2, 1, 2, 3]
12 1 2 3
After:  [2, 1, 2, 0]

Before: [3, 1, 2, 1]
12 1 2 3
After:  [3, 1, 2, 0]

Before: [1, 0, 2, 2]
6 0 2 3
After:  [1, 0, 2, 0]

Before: [1, 1, 0, 1]
0 3 3 1
After:  [1, 0, 0, 1]

Before: [1, 1, 0, 3]
9 1 0 2
After:  [1, 1, 1, 3]

Before: [3, 0, 2, 1]
10 3 2 3
After:  [3, 0, 2, 1]

Before: [2, 2, 3, 3]
14 3 2 3
After:  [2, 2, 3, 1]

Before: [3, 1, 2, 2]
12 1 2 3
After:  [3, 1, 2, 0]

Before: [0, 1, 2, 1]
10 3 2 1
After:  [0, 1, 2, 1]

Before: [0, 1, 3, 0]
8 0 0 2
After:  [0, 1, 0, 0]

Before: [3, 1, 2, 0]
12 1 2 1
After:  [3, 0, 2, 0]

Before: [1, 3, 2, 0]
6 0 2 3
After:  [1, 3, 2, 0]

Before: [2, 0, 1, 3]
7 2 3 3
After:  [2, 0, 1, 0]

Before: [3, 2, 2, 1]
10 3 2 3
After:  [3, 2, 2, 1]

Before: [1, 2, 0, 0]
1 0 2 3
After:  [1, 2, 0, 0]

Before: [2, 1, 1, 1]
0 2 3 0
After:  [0, 1, 1, 1]

Before: [3, 2, 1, 1]
5 2 1 3
After:  [3, 2, 1, 2]

Before: [3, 1, 3, 1]
14 2 3 0
After:  [0, 1, 3, 1]

Before: [2, 1, 1, 3]
14 2 1 1
After:  [2, 0, 1, 3]

Before: [0, 1, 1, 2]
8 0 0 0
After:  [0, 1, 1, 2]

Before: [2, 3, 3, 2]
15 2 2 2
After:  [2, 3, 1, 2]

Before: [0, 1, 2, 3]
7 2 3 1
After:  [0, 0, 2, 3]

Before: [1, 1, 0, 2]
4 1 3 2
After:  [1, 1, 0, 2]

Before: [0, 2, 3, 0]
8 0 0 2
After:  [0, 2, 0, 0]

Before: [0, 1, 1, 1]
11 2 1 1
After:  [0, 2, 1, 1]

Before: [2, 1, 1, 1]
13 1 3 0
After:  [1, 1, 1, 1]

Before: [2, 3, 1, 3]
7 2 3 0
After:  [0, 3, 1, 3]

Before: [2, 1, 2, 3]
12 1 2 2
After:  [2, 1, 0, 3]

Before: [2, 2, 1, 3]
5 2 1 3
After:  [2, 2, 1, 2]

Before: [3, 1, 1, 3]
11 2 1 0
After:  [2, 1, 1, 3]

Before: [0, 0, 1, 3]
7 2 3 1
After:  [0, 0, 1, 3]

Before: [1, 3, 2, 1]
10 3 2 2
After:  [1, 3, 1, 1]

Before: [3, 2, 1, 2]
15 0 0 2
After:  [3, 2, 1, 2]

Before: [1, 2, 1, 1]
0 2 3 1
After:  [1, 0, 1, 1]

Before: [1, 1, 1, 3]
9 1 0 3
After:  [1, 1, 1, 1]

Before: [1, 1, 0, 3]
9 1 0 3
After:  [1, 1, 0, 1]

Before: [0, 1, 1, 1]
11 2 1 2
After:  [0, 1, 2, 1]

Before: [0, 1, 2, 1]
13 1 3 2
After:  [0, 1, 1, 1]

Before: [1, 1, 2, 2]
4 1 3 2
After:  [1, 1, 0, 2]

Before: [3, 1, 1, 2]
11 2 1 3
After:  [3, 1, 1, 2]

Before: [2, 2, 3, 2]
0 3 3 3
After:  [2, 2, 3, 0]

Before: [0, 0, 1, 1]
0 2 3 1
After:  [0, 0, 1, 1]

Before: [0, 1, 2, 2]
12 1 2 1
After:  [0, 0, 2, 2]

Before: [2, 0, 3, 1]
3 0 3 2
After:  [2, 0, 1, 1]

Before: [1, 0, 2, 0]
6 0 2 0
After:  [0, 0, 2, 0]

Before: [0, 2, 1, 1]
5 2 1 0
After:  [2, 2, 1, 1]

Before: [1, 3, 3, 0]
15 2 2 0
After:  [1, 3, 3, 0]

Before: [0, 3, 2, 0]
8 0 0 2
After:  [0, 3, 0, 0]

Before: [2, 2, 2, 1]
0 3 3 1
After:  [2, 0, 2, 1]

Before: [3, 1, 1, 2]
4 1 3 0
After:  [0, 1, 1, 2]

Before: [1, 2, 1, 0]
5 2 1 0
After:  [2, 2, 1, 0]

Before: [2, 2, 3, 3]
15 0 0 0
After:  [1, 2, 3, 3]

Before: [2, 1, 0, 0]
2 0 1 1
After:  [2, 1, 0, 0]

Before: [1, 2, 2, 3]
6 0 2 2
After:  [1, 2, 0, 3]

Before: [1, 0, 0, 1]
1 0 2 1
After:  [1, 0, 0, 1]

Before: [2, 2, 0, 1]
3 0 3 1
After:  [2, 1, 0, 1]

Before: [3, 2, 1, 2]
5 2 1 1
After:  [3, 2, 1, 2]

Before: [2, 1, 3, 2]
14 2 0 1
After:  [2, 1, 3, 2]

Before: [1, 1, 0, 0]
9 1 0 2
After:  [1, 1, 1, 0]

Before: [2, 2, 3, 3]
15 2 2 2
After:  [2, 2, 1, 3]

Before: [0, 2, 1, 0]
8 0 0 2
After:  [0, 2, 0, 0]

Before: [1, 1, 0, 1]
9 1 0 0
After:  [1, 1, 0, 1]

Before: [0, 1, 2, 2]
4 1 3 0
After:  [0, 1, 2, 2]

Before: [1, 1, 0, 0]
9 1 0 0
After:  [1, 1, 0, 0]

Before: [2, 3, 2, 1]
3 0 3 1
After:  [2, 1, 2, 1]

Before: [1, 2, 1, 3]
5 2 1 3
After:  [1, 2, 1, 2]

Before: [2, 1, 1, 3]
11 2 1 2
After:  [2, 1, 2, 3]

Before: [1, 1, 3, 0]
9 1 0 2
After:  [1, 1, 1, 0]

Before: [2, 1, 1, 3]
11 2 1 1
After:  [2, 2, 1, 3]

Before: [2, 1, 3, 2]
2 0 1 2
After:  [2, 1, 1, 2]

Before: [0, 2, 1, 3]
5 2 1 2
After:  [0, 2, 2, 3]

Before: [1, 0, 0, 2]
1 0 2 3
After:  [1, 0, 0, 0]

Before: [1, 1, 1, 2]
9 1 0 3
After:  [1, 1, 1, 1]

Before: [2, 1, 3, 2]
4 1 3 2
After:  [2, 1, 0, 2]

Before: [1, 0, 2, 2]
6 0 2 2
After:  [1, 0, 0, 2]

Before: [3, 1, 1, 3]
11 2 1 1
After:  [3, 2, 1, 3]

Before: [3, 1, 2, 3]
2 0 2 0
After:  [1, 1, 2, 3]

Before: [1, 2, 0, 2]
1 0 2 0
After:  [0, 2, 0, 2]

Before: [3, 1, 2, 1]
10 3 2 2
After:  [3, 1, 1, 1]

Before: [1, 0, 2, 3]
7 2 3 0
After:  [0, 0, 2, 3]

Before: [3, 1, 2, 3]
12 1 2 0
After:  [0, 1, 2, 3]

Before: [2, 1, 1, 3]
7 2 3 1
After:  [2, 0, 1, 3]

Before: [0, 2, 1, 2]
5 2 1 3
After:  [0, 2, 1, 2]

Before: [3, 1, 1, 0]
11 2 1 0
After:  [2, 1, 1, 0]

Before: [1, 1, 3, 1]
9 1 0 0
After:  [1, 1, 3, 1]

Before: [1, 1, 2, 2]
9 1 0 1
After:  [1, 1, 2, 2]

Before: [2, 1, 1, 3]
11 2 1 3
After:  [2, 1, 1, 2]

Before: [1, 1, 1, 2]
4 1 3 1
After:  [1, 0, 1, 2]

Before: [3, 1, 0, 1]
13 1 3 0
After:  [1, 1, 0, 1]

Before: [1, 2, 2, 3]
6 0 2 0
After:  [0, 2, 2, 3]

Before: [1, 3, 0, 3]
1 0 2 0
After:  [0, 3, 0, 3]

Before: [2, 1, 1, 0]
2 0 1 2
After:  [2, 1, 1, 0]

Before: [0, 1, 2, 1]
12 1 2 3
After:  [0, 1, 2, 0]

Before: [2, 3, 1, 1]
3 0 3 3
After:  [2, 3, 1, 1]

Before: [2, 1, 3, 3]
2 0 1 3
After:  [2, 1, 3, 1]

Before: [1, 3, 2, 1]
10 3 2 3
After:  [1, 3, 2, 1]

Before: [1, 1, 3, 3]
9 1 0 3
After:  [1, 1, 3, 1]

Before: [1, 1, 3, 2]
9 1 0 1
After:  [1, 1, 3, 2]

Before: [1, 1, 0, 1]
13 1 3 2
After:  [1, 1, 1, 1]

Before: [3, 0, 2, 0]
2 0 2 1
After:  [3, 1, 2, 0]

Before: [2, 0, 0, 0]
14 0 1 2
After:  [2, 0, 1, 0]

Before: [0, 1, 2, 1]
13 1 3 3
After:  [0, 1, 2, 1]

Before: [2, 1, 3, 0]
14 2 0 1
After:  [2, 1, 3, 0]

Before: [2, 1, 0, 1]
13 1 3 0
After:  [1, 1, 0, 1]

Before: [2, 1, 0, 1]
2 0 1 1
After:  [2, 1, 0, 1]

Before: [0, 3, 2, 1]
10 3 2 0
After:  [1, 3, 2, 1]

Before: [0, 1, 3, 1]
0 3 3 2
After:  [0, 1, 0, 1]

Before: [0, 2, 1, 1]
5 2 1 1
After:  [0, 2, 1, 1]

Before: [2, 1, 1, 2]
15 0 0 3
After:  [2, 1, 1, 1]

Before: [1, 1, 2, 0]
6 0 2 2
After:  [1, 1, 0, 0]

Before: [1, 1, 2, 1]
6 0 2 0
After:  [0, 1, 2, 1]

Before: [0, 2, 1, 3]
7 1 3 1
After:  [0, 0, 1, 3]

Before: [1, 0, 0, 0]
1 0 2 3
After:  [1, 0, 0, 0]

Before: [2, 1, 2, 3]
2 0 1 2
After:  [2, 1, 1, 3]

Before: [0, 2, 0, 2]
0 3 3 1
After:  [0, 0, 0, 2]

Before: [0, 2, 3, 0]
15 2 2 2
After:  [0, 2, 1, 0]

Before: [1, 2, 2, 2]
14 2 1 3
After:  [1, 2, 2, 1]

Before: [0, 1, 3, 1]
8 0 0 2
After:  [0, 1, 0, 1]

Before: [3, 3, 3, 2]
15 0 0 3
After:  [3, 3, 3, 1]

Before: [3, 3, 0, 2]
14 0 2 1
After:  [3, 1, 0, 2]

Before: [0, 1, 1, 3]
11 2 1 0
After:  [2, 1, 1, 3]

Before: [1, 1, 0, 1]
9 1 0 2
After:  [1, 1, 1, 1]

Before: [0, 1, 2, 1]
10 3 2 3
After:  [0, 1, 2, 1]

Before: [2, 2, 2, 1]
10 3 2 1
After:  [2, 1, 2, 1]

Before: [0, 1, 2, 2]
4 1 3 3
After:  [0, 1, 2, 0]

Before: [1, 2, 2, 1]
10 3 2 2
After:  [1, 2, 1, 1]

Before: [2, 1, 1, 2]
11 2 1 1
After:  [2, 2, 1, 2]

Before: [1, 1, 2, 1]
12 1 2 3
After:  [1, 1, 2, 0]

Before: [3, 3, 1, 1]
0 2 3 1
After:  [3, 0, 1, 1]

Before: [0, 1, 2, 2]
4 1 3 1
After:  [0, 0, 2, 2]

Before: [0, 3, 2, 2]
8 0 0 3
After:  [0, 3, 2, 0]

Before: [2, 1, 2, 1]
2 0 1 0
After:  [1, 1, 2, 1]

Before: [1, 1, 0, 3]
1 0 2 1
After:  [1, 0, 0, 3]

Before: [3, 3, 3, 2]
15 0 0 0
After:  [1, 3, 3, 2]

Before: [0, 1, 1, 2]
4 1 3 2
After:  [0, 1, 0, 2]

Before: [1, 3, 0, 3]
1 0 2 1
After:  [1, 0, 0, 3]

Before: [1, 1, 0, 1]
1 0 2 2
After:  [1, 1, 0, 1]

Before: [2, 1, 0, 2]
4 1 3 0
After:  [0, 1, 0, 2]

Before: [3, 2, 2, 2]
2 0 2 2
After:  [3, 2, 1, 2]

Before: [0, 2, 2, 1]
10 3 2 1
After:  [0, 1, 2, 1]

Before: [0, 1, 0, 2]
4 1 3 2
After:  [0, 1, 0, 2]

Before: [0, 1, 0, 2]
4 1 3 3
After:  [0, 1, 0, 0]

Before: [1, 1, 2, 1]
10 3 2 1
After:  [1, 1, 2, 1]

Before: [1, 1, 0, 1]
13 1 3 0
After:  [1, 1, 0, 1]

Before: [1, 3, 2, 2]
6 0 2 1
After:  [1, 0, 2, 2]

Before: [0, 1, 2, 1]
13 1 3 0
After:  [1, 1, 2, 1]

Before: [0, 1, 1, 3]
11 2 1 1
After:  [0, 2, 1, 3]

Before: [3, 2, 1, 0]
5 2 1 3
After:  [3, 2, 1, 2]

Before: [2, 1, 2, 3]
7 2 3 3
After:  [2, 1, 2, 0]

Before: [1, 1, 1, 1]
11 2 1 2
After:  [1, 1, 2, 1]

Before: [2, 1, 1, 1]
3 0 3 2
After:  [2, 1, 1, 1]

Before: [0, 1, 1, 3]
8 0 0 1
After:  [0, 0, 1, 3]

Before: [3, 2, 3, 3]
7 1 3 3
After:  [3, 2, 3, 0]

Before: [0, 3, 0, 0]
8 0 0 2
After:  [0, 3, 0, 0]

Before: [1, 1, 2, 1]
6 0 2 1
After:  [1, 0, 2, 1]

Before: [0, 1, 1, 2]
4 1 3 0
After:  [0, 1, 1, 2]

Before: [1, 1, 2, 1]
9 1 0 1
After:  [1, 1, 2, 1]

Before: [3, 1, 2, 0]
12 1 2 0
After:  [0, 1, 2, 0]

Before: [1, 3, 0, 3]
1 0 2 2
After:  [1, 3, 0, 3]

Before: [1, 1, 0, 3]
9 1 0 1
After:  [1, 1, 0, 3]

Before: [0, 2, 2, 2]
8 0 0 1
After:  [0, 0, 2, 2]

Before: [0, 1, 1, 1]
13 1 3 1
After:  [0, 1, 1, 1]

Before: [1, 1, 3, 1]
13 1 3 0
After:  [1, 1, 3, 1]

Before: [0, 1, 2, 1]
8 0 0 0
After:  [0, 1, 2, 1]

Before: [2, 1, 2, 1]
12 1 2 2
After:  [2, 1, 0, 1]

Before: [1, 0, 2, 3]
6 0 2 1
After:  [1, 0, 2, 3]

Before: [3, 0, 3, 1]
15 2 0 2
After:  [3, 0, 1, 1]

Before: [0, 1, 1, 1]
0 2 3 0
After:  [0, 1, 1, 1]

Before: [3, 0, 0, 3]
14 0 2 1
After:  [3, 1, 0, 3]

Before: [3, 1, 1, 1]
0 2 3 1
After:  [3, 0, 1, 1]

Before: [0, 1, 2, 3]
7 2 3 3
After:  [0, 1, 2, 0]

Before: [3, 1, 0, 1]
13 1 3 1
After:  [3, 1, 0, 1]

Before: [0, 0, 3, 0]
8 0 0 1
After:  [0, 0, 3, 0]

Before: [1, 1, 0, 2]
1 0 2 3
After:  [1, 1, 0, 0]

Before: [2, 1, 1, 2]
4 1 3 1
After:  [2, 0, 1, 2]

Before: [3, 2, 3, 0]
15 2 2 3
After:  [3, 2, 3, 1]

Before: [0, 2, 0, 3]
7 1 3 0
After:  [0, 2, 0, 3]

Before: [1, 1, 3, 2]
9 1 0 2
After:  [1, 1, 1, 2]

Before: [0, 3, 1, 3]
8 0 0 1
After:  [0, 0, 1, 3]

Before: [3, 1, 2, 1]
2 0 2 0
After:  [1, 1, 2, 1]

Before: [1, 1, 3, 1]
9 1 0 2
After:  [1, 1, 1, 1]

Before: [2, 1, 3, 0]
2 0 1 3
After:  [2, 1, 3, 1]

Before: [2, 1, 1, 0]
11 2 1 1
After:  [2, 2, 1, 0]

Before: [3, 1, 1, 1]
13 1 3 0
After:  [1, 1, 1, 1]

Before: [2, 2, 1, 3]
5 2 1 1
After:  [2, 2, 1, 3]

Before: [0, 0, 2, 1]
10 3 2 3
After:  [0, 0, 2, 1]

Before: [3, 3, 0, 2]
0 3 3 1
After:  [3, 0, 0, 2]

Before: [0, 2, 1, 0]
8 0 0 0
After:  [0, 2, 1, 0]

Before: [3, 3, 0, 2]
15 0 0 3
After:  [3, 3, 0, 1]

Before: [1, 0, 2, 3]
6 0 2 0
After:  [0, 0, 2, 3]

Before: [0, 0, 1, 1]
8 0 0 1
After:  [0, 0, 1, 1]

Before: [1, 0, 2, 1]
10 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 2, 1, 2]
5 2 1 1
After:  [1, 2, 1, 2]

Before: [2, 1, 3, 1]
14 2 0 1
After:  [2, 1, 3, 1]

Before: [2, 1, 2, 0]
2 0 1 0
After:  [1, 1, 2, 0]

Before: [1, 1, 2, 2]
6 0 2 3
After:  [1, 1, 2, 0]

Before: [2, 1, 1, 3]
2 0 1 2
After:  [2, 1, 1, 3]

Before: [2, 3, 3, 2]
14 2 0 2
After:  [2, 3, 1, 2]

Before: [1, 0, 0, 2]
1 0 2 0
After:  [0, 0, 0, 2]

Before: [3, 3, 2, 2]
15 0 0 0
After:  [1, 3, 2, 2]

Before: [0, 1, 1, 2]
4 1 3 3
After:  [0, 1, 1, 0]

Before: [2, 2, 1, 2]
5 2 1 3
After:  [2, 2, 1, 2]

Before: [2, 1, 2, 0]
12 1 2 0
After:  [0, 1, 2, 0]

Before: [3, 1, 0, 1]
13 1 3 2
After:  [3, 1, 1, 1]

Before: [1, 2, 1, 1]
5 2 1 1
After:  [1, 2, 1, 1]

Before: [2, 1, 2, 2]
4 1 3 2
After:  [2, 1, 0, 2]

Before: [0, 1, 0, 2]
4 1 3 0
After:  [0, 1, 0, 2]

Before: [3, 1, 0, 2]
4 1 3 2
After:  [3, 1, 0, 2]

Before: [1, 1, 3, 2]
4 1 3 1
After:  [1, 0, 3, 2]

Before: [3, 1, 1, 1]
13 1 3 2
After:  [3, 1, 1, 1]

Before: [0, 0, 2, 0]
8 0 0 3
After:  [0, 0, 2, 0]

Before: [1, 1, 3, 2]
9 1 0 0
After:  [1, 1, 3, 2]

Before: [3, 2, 1, 0]
5 2 1 1
After:  [3, 2, 1, 0]

Before: [1, 1, 0, 2]
1 0 2 0
After:  [0, 1, 0, 2]

Before: [2, 1, 0, 1]
13 1 3 3
After:  [2, 1, 0, 1]

Before: [3, 1, 2, 0]
12 1 2 2
After:  [3, 1, 0, 0]

Before: [3, 2, 2, 3]
2 0 2 0
After:  [1, 2, 2, 3]

Before: [1, 1, 1, 0]
11 2 1 1
After:  [1, 2, 1, 0]

Before: [0, 0, 1, 2]
8 0 0 3
After:  [0, 0, 1, 0]

Before: [1, 1, 0, 0]
9 1 0 3
After:  [1, 1, 0, 1]

Before: [1, 1, 3, 0]
9 1 0 3
After:  [1, 1, 3, 1]

Before: [1, 1, 1, 1]
11 2 1 1
After:  [1, 2, 1, 1]

Before: [3, 0, 0, 0]
14 0 2 3
After:  [3, 0, 0, 1]

Before: [2, 1, 1, 3]
7 1 3 3
After:  [2, 1, 1, 0]

Before: [0, 3, 3, 2]
8 0 0 2
After:  [0, 3, 0, 2]

Before: [3, 1, 2, 1]
12 1 2 1
After:  [3, 0, 2, 1]

Before: [3, 0, 2, 3]
7 2 3 0
After:  [0, 0, 2, 3]

Before: [3, 1, 1, 1]
14 3 1 1
After:  [3, 0, 1, 1]

Before: [1, 1, 1, 3]
9 1 0 0
After:  [1, 1, 1, 3]

Before: [0, 0, 3, 3]
8 0 0 2
After:  [0, 0, 0, 3]

Before: [3, 1, 3, 3]
7 1 3 1
After:  [3, 0, 3, 3]

Before: [1, 1, 2, 2]
12 1 2 1
After:  [1, 0, 2, 2]

Before: [1, 1, 0, 1]
1 0 2 3
After:  [1, 1, 0, 0]

Before: [2, 2, 2, 1]
3 0 3 2
After:  [2, 2, 1, 1]

Before: [2, 0, 3, 0]
14 0 1 1
After:  [2, 1, 3, 0]

Before: [1, 1, 2, 2]
4 1 3 3
After:  [1, 1, 2, 0]

Before: [1, 1, 2, 3]
12 1 2 2
After:  [1, 1, 0, 3]

Before: [1, 2, 1, 3]
7 2 3 2
After:  [1, 2, 0, 3]

Before: [3, 0, 0, 1]
14 0 2 2
After:  [3, 0, 1, 1]

Before: [3, 2, 1, 0]
5 2 1 0
After:  [2, 2, 1, 0]

Before: [2, 3, 2, 1]
3 0 3 0
After:  [1, 3, 2, 1]

Before: [0, 1, 3, 2]
8 0 0 3
After:  [0, 1, 3, 0]

Before: [2, 2, 1, 1]
3 0 3 3
After:  [2, 2, 1, 1]

Before: [3, 2, 3, 1]
0 3 3 3
After:  [3, 2, 3, 0]

Before: [2, 1, 1, 0]
14 2 1 3
After:  [2, 1, 1, 0]

Before: [2, 2, 1, 3]
7 2 3 1
After:  [2, 0, 1, 3]

Before: [2, 3, 3, 1]
3 0 3 2
After:  [2, 3, 1, 1]

Before: [1, 1, 2, 1]
9 1 0 2
After:  [1, 1, 1, 1]

Before: [0, 3, 2, 1]
10 3 2 1
After:  [0, 1, 2, 1]

Before: [0, 1, 0, 1]
13 1 3 3
After:  [0, 1, 0, 1]

Before: [1, 1, 1, 3]
11 2 1 3
After:  [1, 1, 1, 2]

Before: [3, 1, 1, 2]
11 2 1 2
After:  [3, 1, 2, 2]

Before: [1, 3, 2, 3]
6 0 2 3
After:  [1, 3, 2, 0]

Before: [0, 1, 2, 3]
8 0 0 2
After:  [0, 1, 0, 3]

Before: [3, 0, 1, 3]
14 3 0 2
After:  [3, 0, 1, 3]

Before: [2, 1, 2, 0]
12 1 2 3
After:  [2, 1, 2, 0]

Before: [0, 1, 1, 1]
11 2 1 0
After:  [2, 1, 1, 1]

Before: [2, 3, 2, 1]
0 3 3 2
After:  [2, 3, 0, 1]

Before: [1, 1, 0, 2]
0 3 3 3
After:  [1, 1, 0, 0]

Before: [1, 0, 0, 1]
1 0 2 3
After:  [1, 0, 0, 0]

Before: [3, 2, 1, 3]
7 2 3 3
After:  [3, 2, 1, 0]

Before: [3, 1, 1, 3]
11 2 1 2
After:  [3, 1, 2, 3]

Before: [0, 1, 2, 2]
12 1 2 3
After:  [0, 1, 2, 0]

Before: [3, 3, 2, 1]
10 3 2 0
After:  [1, 3, 2, 1]

Before: [1, 1, 3, 1]
13 1 3 1
After:  [1, 1, 3, 1]

Before: [2, 2, 1, 1]
3 0 3 1
After:  [2, 1, 1, 1]

Before: [2, 1, 2, 2]
4 1 3 0
After:  [0, 1, 2, 2]

Before: [1, 1, 1, 1]
9 1 0 2
After:  [1, 1, 1, 1]

Before: [1, 3, 2, 1]
10 3 2 0
After:  [1, 3, 2, 1]

Before: [2, 0, 2, 1]
10 3 2 0
After:  [1, 0, 2, 1]

Before: [1, 1, 0, 3]
1 0 2 2
After:  [1, 1, 0, 3]

Before: [1, 2, 0, 1]
1 0 2 3
After:  [1, 2, 0, 0]

Before: [1, 3, 0, 0]
1 0 2 0
After:  [0, 3, 0, 0]

Before: [2, 1, 1, 3]
14 2 1 0
After:  [0, 1, 1, 3]

Before: [1, 1, 1, 2]
9 1 0 1
After:  [1, 1, 1, 2]

Before: [1, 1, 0, 1]
13 1 3 1
After:  [1, 1, 0, 1]

Before: [2, 0, 0, 2]
15 0 0 0
After:  [1, 0, 0, 2]

Before: [2, 3, 1, 1]
3 0 3 0
After:  [1, 3, 1, 1]

Before: [0, 1, 2, 0]
12 1 2 3
After:  [0, 1, 2, 0]

Before: [1, 2, 1, 2]
5 2 1 0
After:  [2, 2, 1, 2]

Before: [2, 0, 2, 2]
14 3 2 2
After:  [2, 0, 0, 2]

Before: [0, 2, 2, 1]
10 3 2 0
After:  [1, 2, 2, 1]

Before: [2, 1, 0, 2]
4 1 3 3
After:  [2, 1, 0, 0]

Before: [1, 3, 0, 2]
1 0 2 2
After:  [1, 3, 0, 2]

Before: [0, 0, 2, 3]
8 0 0 1
After:  [0, 0, 2, 3]

Before: [2, 1, 1, 3]
7 1 3 0
After:  [0, 1, 1, 3]

Before: [3, 1, 2, 1]
13 1 3 0
After:  [1, 1, 2, 1]

Before: [2, 0, 1, 1]
3 0 3 1
After:  [2, 1, 1, 1]

Before: [1, 1, 2, 1]
13 1 3 1
After:  [1, 1, 2, 1]

Before: [0, 1, 2, 1]
12 1 2 1
After:  [0, 0, 2, 1]

Before: [2, 2, 3, 3]
14 3 2 2
After:  [2, 2, 1, 3]

Before: [3, 1, 1, 1]
13 1 3 3
After:  [3, 1, 1, 1]

Before: [3, 3, 3, 2]
15 2 0 1
After:  [3, 1, 3, 2]

Before: [2, 1, 2, 1]
3 0 3 1
After:  [2, 1, 2, 1]

Before: [3, 1, 2, 0]
2 0 2 3
After:  [3, 1, 2, 1]

Before: [1, 2, 1, 2]
5 2 1 3
After:  [1, 2, 1, 2]

Before: [3, 2, 1, 1]
5 2 1 0
After:  [2, 2, 1, 1]

Before: [0, 1, 2, 1]
12 1 2 0
After:  [0, 1, 2, 1]

Before: [2, 1, 1, 1]
3 0 3 0
After:  [1, 1, 1, 1]

Before: [3, 1, 1, 2]
11 2 1 1
After:  [3, 2, 1, 2]

Before: [1, 1, 1, 3]
11 2 1 0
After:  [2, 1, 1, 3]

Before: [1, 1, 2, 0]
9 1 0 3
After:  [1, 1, 2, 1]

Before: [0, 2, 2, 3]
8 0 0 3
After:  [0, 2, 2, 0]

Before: [0, 0, 2, 1]
10 3 2 1
After:  [0, 1, 2, 1]

Before: [0, 2, 3, 3]
14 3 2 0
After:  [1, 2, 3, 3]

Before: [2, 1, 0, 3]
2 0 1 2
After:  [2, 1, 1, 3]

Before: [3, 1, 2, 0]
2 0 2 0
After:  [1, 1, 2, 0]

Before: [3, 1, 0, 2]
14 0 2 0
After:  [1, 1, 0, 2]

Before: [2, 1, 3, 0]
2 0 1 1
After:  [2, 1, 3, 0]

Before: [1, 1, 1, 0]
9 1 0 3
After:  [1, 1, 1, 1]

Before: [1, 0, 0, 0]
1 0 2 1
After:  [1, 0, 0, 0]

Before: [0, 3, 2, 2]
8 0 0 2
After:  [0, 3, 0, 2]

Before: [3, 3, 2, 2]
2 0 2 0
After:  [1, 3, 2, 2]

Before: [0, 2, 1, 2]
5 2 1 1
After:  [0, 2, 1, 2]

Before: [3, 3, 2, 2]
2 0 2 3
After:  [3, 3, 2, 1]

Before: [0, 2, 1, 2]
5 2 1 0
After:  [2, 2, 1, 2]

Before: [1, 0, 2, 1]
10 3 2 2
After:  [1, 0, 1, 1]

Before: [0, 1, 0, 1]
13 1 3 2
After:  [0, 1, 1, 1]

Before: [3, 1, 1, 1]
14 2 1 1
After:  [3, 0, 1, 1]

Before: [0, 1, 0, 1]
13 1 3 1
After:  [0, 1, 0, 1]

Before: [2, 2, 0, 1]
3 0 3 2
After:  [2, 2, 1, 1]

Before: [3, 2, 1, 3]
14 3 0 0
After:  [1, 2, 1, 3]

Before: [1, 1, 2, 2]
4 1 3 0
After:  [0, 1, 2, 2]

Before: [3, 1, 2, 3]
7 1 3 1
After:  [3, 0, 2, 3]

Before: [3, 0, 3, 0]
15 2 2 1
After:  [3, 1, 3, 0]

Before: [0, 2, 2, 2]
14 2 1 3
After:  [0, 2, 2, 1]

Before: [1, 1, 2, 3]
12 1 2 1
After:  [1, 0, 2, 3]

Before: [3, 1, 1, 1]
13 1 3 1
After:  [3, 1, 1, 1]

Before: [2, 1, 1, 1]
13 1 3 3
After:  [2, 1, 1, 1]

Before: [2, 2, 2, 3]
7 2 3 3
After:  [2, 2, 2, 0]

Before: [2, 3, 3, 3]
15 0 0 2
After:  [2, 3, 1, 3]

Before: [3, 1, 2, 1]
13 1 3 3
After:  [3, 1, 2, 1]

Before: [3, 3, 3, 2]
15 0 2 0
After:  [1, 3, 3, 2]

Before: [3, 1, 0, 2]
0 3 3 0
After:  [0, 1, 0, 2]

Before: [2, 0, 3, 2]
14 0 1 1
After:  [2, 1, 3, 2]

Before: [1, 0, 2, 1]
10 3 2 3
After:  [1, 0, 2, 1]

Before: [1, 3, 3, 1]
0 3 3 2
After:  [1, 3, 0, 1]

Before: [0, 2, 2, 1]
10 3 2 2
After:  [0, 2, 1, 1]

Before: [2, 2, 1, 0]
5 2 1 0
After:  [2, 2, 1, 0]

Before: [2, 3, 0, 1]
3 0 3 2
After:  [2, 3, 1, 1]

Before: [1, 2, 2, 2]
6 0 2 1
After:  [1, 0, 2, 2]

Before: [0, 1, 2, 2]
12 1 2 0
After:  [0, 1, 2, 2]

Before: [1, 1, 0, 2]
9 1 0 2
After:  [1, 1, 1, 2]

Before: [0, 1, 2, 2]
12 1 2 2
After:  [0, 1, 0, 2]

Before: [2, 1, 2, 0]
12 1 2 2
After:  [2, 1, 0, 0]

Before: [2, 3, 3, 0]
15 0 0 3
After:  [2, 3, 3, 1]

Before: [2, 2, 0, 1]
15 0 0 0
After:  [1, 2, 0, 1]

Before: [2, 0, 3, 2]
0 3 3 2
After:  [2, 0, 0, 2]

Before: [3, 0, 3, 2]
15 2 2 3
After:  [3, 0, 3, 1]

Before: [2, 3, 2, 1]
10 3 2 3
After:  [2, 3, 2, 1]

Before: [2, 1, 2, 1]
3 0 3 3
After:  [2, 1, 2, 1]

Before: [1, 3, 0, 0]
1 0 2 3
After:  [1, 3, 0, 0]

Before: [3, 1, 2, 3]
2 0 2 3
After:  [3, 1, 2, 1]

Before: [2, 1, 1, 2]
11 2 1 2
After:  [2, 1, 2, 2]

Before: [1, 3, 2, 3]
7 2 3 1
After:  [1, 0, 2, 3]

Before: [0, 0, 0, 0]
8 0 0 3
After:  [0, 0, 0, 0]

Before: [1, 0, 3, 1]
14 2 3 2
After:  [1, 0, 0, 1]

Before: [3, 2, 0, 3]
14 0 2 3
After:  [3, 2, 0, 1]

Before: [3, 2, 2, 1]
2 0 2 1
After:  [3, 1, 2, 1]

Before: [2, 1, 2, 1]
3 0 3 0
After:  [1, 1, 2, 1]

Before: [2, 2, 0, 1]
3 0 3 3
After:  [2, 2, 0, 1]

Before: [0, 3, 3, 2]
8 0 0 0
After:  [0, 3, 3, 2]

Before: [3, 2, 0, 1]
14 0 2 1
After:  [3, 1, 0, 1]

Before: [1, 1, 1, 3]
9 1 0 1
After:  [1, 1, 1, 3]

Before: [0, 1, 0, 1]
13 1 3 0
After:  [1, 1, 0, 1]

Before: [1, 1, 1, 0]
9 1 0 1
After:  [1, 1, 1, 0]

Before: [1, 3, 2, 2]
6 0 2 0
After:  [0, 3, 2, 2]

Before: [2, 1, 1, 1]
14 3 1 0
After:  [0, 1, 1, 1]

Before: [1, 1, 3, 0]
9 1 0 0
After:  [1, 1, 3, 0]

Before: [2, 1, 3, 1]
3 0 3 2
After:  [2, 1, 1, 1]

Before: [2, 1, 1, 1]
3 0 3 1
After:  [2, 1, 1, 1]

Before: [3, 2, 1, 3]
7 1 3 3
After:  [3, 2, 1, 0]

Before: [2, 0, 3, 3]
15 0 0 2
After:  [2, 0, 1, 3]

Before: [3, 0, 2, 1]
10 3 2 2
After:  [3, 0, 1, 1]

Before: [1, 1, 2, 3]
9 1 0 0
After:  [1, 1, 2, 3]

Before: [1, 2, 1, 1]
5 2 1 0
After:  [2, 2, 1, 1]

Before: [0, 1, 2, 3]
12 1 2 1
After:  [0, 0, 2, 3]

Before: [1, 3, 0, 1]
1 0 2 3
After:  [1, 3, 0, 0]

Before: [2, 1, 0, 1]
2 0 1 0
After:  [1, 1, 0, 1]

Before: [3, 2, 2, 3]
2 0 2 1
After:  [3, 1, 2, 3]

Before: [1, 2, 0, 1]
1 0 2 1
After:  [1, 0, 0, 1]

Before: [1, 2, 2, 0]
6 0 2 0
After:  [0, 2, 2, 0]

Before: [2, 1, 1, 2]
11 2 1 0
After:  [2, 1, 1, 2]

Before: [3, 1, 1, 3]
7 1 3 2
After:  [3, 1, 0, 3]

Before: [2, 2, 1, 3]
5 2 1 2
After:  [2, 2, 2, 3]

Before: [3, 1, 1, 1]
11 2 1 1
After:  [3, 2, 1, 1]

Before: [2, 1, 2, 2]
4 1 3 1
After:  [2, 0, 2, 2]

Before: [1, 1, 2, 1]
12 1 2 0
After:  [0, 1, 2, 1]

Before: [1, 1, 0, 2]
9 1 0 3
After:  [1, 1, 0, 1]

Before: [3, 3, 2, 3]
2 0 2 0
After:  [1, 3, 2, 3]

Before: [1, 1, 2, 3]
9 1 0 3
After:  [1, 1, 2, 1]

Before: [2, 1, 2, 1]
12 1 2 0
After:  [0, 1, 2, 1]

Before: [1, 1, 0, 2]
4 1 3 1
After:  [1, 0, 0, 2]

Before: [1, 2, 2, 0]
6 0 2 3
After:  [1, 2, 2, 0]

Before: [2, 1, 1, 0]
11 2 1 3
After:  [2, 1, 1, 2]

Before: [1, 1, 0, 1]
13 1 3 3
After:  [1, 1, 0, 1]

Before: [3, 1, 2, 3]
7 1 3 3
After:  [3, 1, 2, 0]

Before: [0, 2, 1, 3]
8 0 0 0
After:  [0, 2, 1, 3]

Before: [3, 2, 1, 3]
7 1 3 0
After:  [0, 2, 1, 3]

Before: [1, 2, 2, 2]
6 0 2 3
After:  [1, 2, 2, 0]

Before: [1, 1, 1, 1]
13 1 3 3
After:  [1, 1, 1, 1]

Before: [2, 1, 3, 2]
15 2 2 1
After:  [2, 1, 3, 2]

Before: [2, 1, 0, 3]
2 0 1 0
After:  [1, 1, 0, 3]

Before: [1, 1, 2, 1]
12 1 2 2
After:  [1, 1, 0, 1]

Before: [1, 1, 3, 2]
4 1 3 0
After:  [0, 1, 3, 2]

Before: [2, 3, 2, 3]
7 2 3 2
After:  [2, 3, 0, 3]

Before: [2, 2, 1, 1]
5 2 1 3
After:  [2, 2, 1, 2]

Before: [0, 0, 2, 1]
10 3 2 0
After:  [1, 0, 2, 1]

Before: [3, 1, 0, 3]
7 1 3 2
After:  [3, 1, 0, 3]

Before: [2, 1, 3, 2]
2 0 1 1
After:  [2, 1, 3, 2]

Before: [2, 3, 1, 1]
3 0 3 1
After:  [2, 1, 1, 1]

Before: [2, 2, 1, 3]
7 1 3 3
After:  [2, 2, 1, 0]

Before: [3, 3, 3, 1]
15 0 2 1
After:  [3, 1, 3, 1]

Before: [0, 1, 1, 0]
11 2 1 1
After:  [0, 2, 1, 0]

Before: [1, 1, 1, 0]
11 2 1 0
After:  [2, 1, 1, 0]

Before: [3, 1, 3, 1]
13 1 3 3
After:  [3, 1, 3, 1]

Before: [0, 1, 3, 2]
8 0 0 1
After:  [0, 0, 3, 2]

Before: [2, 2, 0, 3]
7 1 3 3
After:  [2, 2, 0, 0]

Before: [1, 0, 2, 1]
6 0 2 2
After:  [1, 0, 0, 1]

Before: [1, 3, 0, 2]
0 3 3 3
After:  [1, 3, 0, 0]

Before: [1, 1, 0, 1]
9 1 0 3
After:  [1, 1, 0, 1]

Before: [1, 2, 2, 3]
7 1 3 1
After:  [1, 0, 2, 3]

Before: [1, 1, 2, 2]
12 1 2 3
After:  [1, 1, 2, 0]

Before: [1, 1, 2, 0]
12 1 2 3
After:  [1, 1, 2, 0]

Before: [0, 1, 0, 2]
4 1 3 1
After:  [0, 0, 0, 2]

Before: [1, 1, 1, 0]
9 1 0 0
After:  [1, 1, 1, 0]

Before: [1, 1, 2, 0]
9 1 0 0
After:  [1, 1, 2, 0]

Before: [1, 2, 1, 1]
5 2 1 3
After:  [1, 2, 1, 2]

Before: [3, 0, 3, 2]
15 2 2 0
After:  [1, 0, 3, 2]

Before: [2, 2, 1, 3]
7 2 3 3
After:  [2, 2, 1, 0]

Before: [3, 1, 2, 2]
4 1 3 3
After:  [3, 1, 2, 0]

Before: [3, 1, 2, 1]
15 0 0 1
After:  [3, 1, 2, 1]

Before: [2, 3, 2, 1]
10 3 2 0
After:  [1, 3, 2, 1]

Before: [2, 1, 2, 2]
0 3 3 1
After:  [2, 0, 2, 2]

Before: [1, 2, 0, 2]
1 0 2 1
After:  [1, 0, 0, 2]

Before: [3, 3, 2, 0]
2 0 2 0
After:  [1, 3, 2, 0]

Before: [0, 1, 1, 2]
11 2 1 1
After:  [0, 2, 1, 2]

Before: [3, 1, 2, 1]
13 1 3 1
After:  [3, 1, 2, 1]

Before: [3, 1, 3, 3]
15 2 0 3
After:  [3, 1, 3, 1]

Before: [0, 1, 0, 1]
8 0 0 3
After:  [0, 1, 0, 0]

Before: [2, 3, 2, 1]
0 3 3 3
After:  [2, 3, 2, 0]

Before: [2, 1, 1, 2]
4 1 3 2
After:  [2, 1, 0, 2]

Before: [0, 1, 3, 1]
13 1 3 0
After:  [1, 1, 3, 1]

Before: [2, 2, 1, 1]
5 2 1 0
After:  [2, 2, 1, 1]

Before: [3, 1, 2, 0]
15 0 0 0
After:  [1, 1, 2, 0]

Before: [1, 1, 1, 1]
9 1 0 0
After:  [1, 1, 1, 1]

Before: [1, 1, 2, 2]
12 1 2 2
After:  [1, 1, 0, 2]

Before: [1, 1, 2, 1]
10 3 2 0
After:  [1, 1, 2, 1]

Before: [2, 0, 1, 1]
3 0 3 3
After:  [2, 0, 1, 1]



8 0 0 2
5 2 2 2
6 3 1 1
8 0 0 3
5 3 0 3
9 2 3 1
8 1 3 1
8 1 2 1
11 0 1 0
10 0 0 1
8 0 0 2
5 2 3 2
6 3 0 3
8 2 0 0
5 0 1 0
12 3 2 0
8 0 3 0
11 1 0 1
10 1 1 3
6 1 2 0
8 1 0 1
5 1 0 1
6 0 0 2
5 0 1 1
8 1 3 1
11 3 1 3
10 3 3 2
6 3 0 3
6 2 1 1
4 3 1 0
8 0 3 0
11 0 2 2
10 2 0 1
8 1 0 0
5 0 1 0
8 3 0 2
5 2 0 2
6 2 1 3
8 0 2 3
8 3 3 3
8 3 3 3
11 3 1 1
10 1 1 3
6 3 3 2
6 0 0 1
8 0 2 0
8 0 1 0
8 0 1 0
11 3 0 3
10 3 0 0
6 2 2 1
6 1 0 3
13 1 2 1
8 1 2 1
11 1 0 0
10 0 3 2
6 2 0 0
6 3 1 1
6 2 0 3
9 0 3 1
8 1 1 1
8 1 3 1
11 1 2 2
10 2 0 0
6 3 1 1
6 2 1 2
6 0 2 3
7 3 2 1
8 1 2 1
8 1 2 1
11 1 0 0
10 0 3 3
6 3 1 2
6 1 2 0
6 0 1 1
6 2 1 1
8 1 1 1
11 3 1 3
10 3 3 1
6 2 0 3
8 0 0 2
5 2 0 2
6 2 2 0
15 0 3 0
8 0 1 0
8 0 2 0
11 0 1 1
10 1 0 0
6 3 1 1
6 0 0 3
6 2 1 2
7 3 2 2
8 2 2 2
11 2 0 0
10 0 1 1
6 1 0 3
8 0 0 0
5 0 2 0
8 0 0 2
5 2 0 2
3 0 3 2
8 2 3 2
8 2 2 2
11 1 2 1
10 1 1 3
8 2 0 2
5 2 3 2
6 3 2 0
6 1 3 1
8 1 2 2
8 2 1 2
11 2 3 3
6 1 1 0
8 1 0 2
5 2 0 2
6 2 0 0
8 0 3 0
11 3 0 3
10 3 3 2
6 2 1 0
6 2 2 3
6 0 1 1
9 0 3 0
8 0 1 0
8 0 1 0
11 0 2 2
10 2 3 3
6 3 1 1
8 3 0 2
5 2 1 2
6 1 3 0
5 0 1 0
8 0 2 0
11 0 3 3
10 3 0 0
6 1 3 3
8 0 0 2
5 2 0 2
6 0 1 1
5 3 1 2
8 2 2 2
11 0 2 0
10 0 2 3
6 2 1 1
6 2 0 2
6 3 3 0
13 1 0 1
8 1 2 1
11 1 3 3
10 3 2 2
6 1 1 3
6 0 2 1
6 0 2 0
5 3 1 3
8 3 2 3
11 2 3 2
10 2 3 3
6 1 3 0
6 0 0 2
6 3 1 1
8 0 2 1
8 1 3 1
11 3 1 3
10 3 0 1
6 2 2 0
6 2 1 3
6 3 3 2
9 0 3 2
8 2 2 2
8 2 3 2
11 2 1 1
10 1 3 3
6 0 3 2
8 2 0 1
5 1 3 1
2 0 1 1
8 1 1 1
11 1 3 3
10 3 3 2
6 3 0 1
6 1 1 3
3 0 3 1
8 1 3 1
11 2 1 2
10 2 1 0
6 2 1 1
6 1 3 2
6 2 0 3
9 1 3 3
8 3 3 3
11 3 0 0
10 0 3 2
6 2 3 0
6 1 0 3
3 0 3 3
8 3 2 3
11 2 3 2
10 2 2 1
6 1 2 3
6 2 3 2
3 0 3 2
8 2 1 2
8 2 2 2
11 2 1 1
10 1 0 2
6 3 2 1
6 3 3 0
11 3 3 1
8 1 2 1
8 1 2 1
11 2 1 2
6 2 1 1
13 1 0 0
8 0 3 0
8 0 1 0
11 0 2 2
10 2 1 3
8 1 0 0
5 0 1 0
6 0 0 2
8 0 2 0
8 0 3 0
8 0 2 0
11 3 0 3
10 3 3 1
6 3 3 2
6 2 1 0
6 2 2 3
15 0 3 3
8 3 2 3
11 3 1 1
10 1 3 2
6 3 1 1
8 2 0 3
5 3 0 3
4 1 0 1
8 1 2 1
11 2 1 2
10 2 0 1
6 3 1 2
6 1 1 3
6 1 0 0
8 3 2 3
8 3 2 3
11 1 3 1
10 1 2 3
6 2 1 1
6 2 1 2
10 0 2 2
8 2 1 2
11 3 2 3
6 0 0 1
6 2 1 2
10 0 2 0
8 0 3 0
11 3 0 3
10 3 1 2
8 2 0 3
5 3 0 3
6 1 1 0
11 0 0 3
8 3 2 3
11 3 2 2
10 2 3 3
6 2 1 1
6 3 2 2
6 2 2 0
0 0 2 0
8 0 1 0
11 0 3 3
10 3 3 2
6 2 1 3
6 2 0 0
6 3 0 1
15 0 3 3
8 3 1 3
8 3 3 3
11 3 2 2
10 2 3 1
8 1 0 0
5 0 1 0
6 3 3 2
8 0 0 3
5 3 1 3
8 0 2 3
8 3 1 3
11 3 1 1
10 1 1 3
6 0 0 1
6 2 1 2
10 0 2 1
8 1 2 1
11 1 3 3
10 3 2 0
8 0 0 2
5 2 0 2
6 3 2 3
6 3 3 1
12 3 2 1
8 1 1 1
11 1 0 0
10 0 0 3
6 3 2 1
6 2 2 0
6 3 3 2
0 0 2 0
8 0 3 0
8 0 1 0
11 0 3 3
10 3 3 2
6 1 3 1
8 1 0 3
5 3 2 3
8 0 0 0
5 0 2 0
1 1 3 0
8 0 1 0
11 2 0 2
6 3 1 1
6 2 0 0
4 1 0 1
8 1 2 1
11 2 1 2
10 2 1 1
6 3 0 2
6 1 2 3
3 0 3 0
8 0 1 0
11 0 1 1
8 3 0 2
5 2 2 2
6 2 0 3
8 2 0 0
5 0 3 0
2 2 0 2
8 2 1 2
11 1 2 1
10 1 2 0
6 3 0 1
8 0 0 2
5 2 0 2
14 2 3 1
8 1 2 1
11 1 0 0
10 0 2 3
6 3 3 2
6 1 3 0
8 3 0 1
5 1 1 1
11 1 0 2
8 2 3 2
8 2 3 2
11 3 2 3
10 3 3 1
6 2 2 2
6 0 2 3
7 3 2 0
8 0 2 0
8 0 2 0
11 1 0 1
10 1 2 3
6 3 2 0
8 2 0 2
5 2 0 2
8 1 0 1
5 1 3 1
0 2 0 0
8 0 3 0
11 0 3 3
10 3 2 1
6 1 1 0
8 3 0 3
5 3 0 3
6 1 1 2
6 3 0 2
8 2 2 2
11 2 1 1
10 1 3 2
8 3 0 1
5 1 1 1
6 3 1 3
11 0 0 1
8 1 1 1
11 1 2 2
10 2 1 1
6 1 2 2
6 3 2 0
6 2 1 3
12 0 2 0
8 0 3 0
11 1 0 1
10 1 1 2
6 2 3 0
8 2 0 3
5 3 1 3
6 3 1 1
5 3 1 3
8 3 2 3
11 2 3 2
6 0 3 3
6 3 0 0
8 2 0 1
5 1 2 1
13 1 0 0
8 0 1 0
11 0 2 2
10 2 3 1
6 2 1 2
6 1 2 3
6 3 0 0
2 2 0 3
8 3 3 3
11 1 3 1
10 1 2 3
6 1 3 0
6 3 0 1
5 0 1 1
8 1 2 1
11 1 3 3
10 3 3 2
6 1 3 3
8 0 0 1
5 1 0 1
8 1 0 0
5 0 2 0
3 0 3 1
8 1 2 1
11 2 1 2
6 1 1 0
6 0 2 1
8 3 0 3
5 3 2 3
5 0 1 0
8 0 3 0
8 0 2 0
11 0 2 2
6 1 0 1
6 2 1 0
15 0 3 3
8 3 3 3
11 3 2 2
6 0 2 1
6 2 3 3
6 3 3 0
4 0 3 1
8 1 2 1
11 2 1 2
6 0 1 3
8 0 0 0
5 0 2 0
8 3 0 1
5 1 2 1
6 3 0 0
8 0 2 0
8 0 3 0
11 0 2 2
10 2 1 3
8 2 0 2
5 2 3 2
6 1 1 1
6 2 3 0
1 1 0 2
8 2 3 2
11 3 2 3
10 3 0 0
6 0 3 3
8 3 0 1
5 1 0 1
6 2 0 2
7 3 2 2
8 2 2 2
11 0 2 0
10 0 0 3
6 2 3 1
8 3 0 2
5 2 0 2
6 3 3 0
13 1 0 2
8 2 3 2
8 2 3 2
11 3 2 3
8 3 0 2
5 2 2 2
2 2 0 1
8 1 3 1
11 3 1 3
10 3 2 2
6 1 3 3
6 3 3 1
6 2 0 0
3 0 3 3
8 3 3 3
11 3 2 2
10 2 2 0
6 1 1 1
6 2 1 3
8 2 0 2
5 2 0 2
14 2 3 2
8 2 2 2
8 2 3 2
11 2 0 0
10 0 1 3
6 3 0 1
6 3 2 2
6 2 0 0
6 2 1 0
8 0 2 0
11 3 0 3
10 3 0 1
6 0 1 3
6 2 3 2
8 3 0 0
5 0 0 0
7 3 2 3
8 3 3 3
8 3 2 3
11 1 3 1
10 1 1 2
6 2 0 0
8 0 0 3
5 3 1 3
6 1 1 1
1 3 0 1
8 1 2 1
11 1 2 2
10 2 2 3
6 0 2 1
6 3 1 0
6 2 2 2
2 2 0 1
8 1 3 1
8 1 1 1
11 1 3 3
10 3 1 2
6 1 0 1
6 2 2 3
1 1 3 0
8 0 2 0
8 0 3 0
11 2 0 2
10 2 3 1
6 2 0 0
6 2 0 2
15 0 3 3
8 3 1 3
11 3 1 1
10 1 0 3
6 3 0 2
6 1 3 1
0 0 2 2
8 2 3 2
11 3 2 3
10 3 3 0
6 1 2 3
6 3 1 1
6 0 0 2
12 1 2 3
8 3 3 3
8 3 3 3
11 3 0 0
10 0 3 1
8 1 0 0
5 0 2 0
8 1 0 2
5 2 2 2
6 0 2 3
7 3 2 2
8 2 2 2
11 2 1 1
10 1 2 3
6 2 3 1
6 3 1 2
13 1 2 0
8 0 3 0
11 0 3 3
10 3 1 1
6 1 2 3
6 3 1 0
8 3 2 0
8 0 1 0
11 1 0 1
6 1 3 0
6 2 2 2
6 3 0 3
10 0 2 0
8 0 1 0
11 0 1 1
6 3 0 0
2 2 0 3
8 3 1 3
11 3 1 1
10 1 2 3
6 1 2 2
8 1 0 1
5 1 1 1
6 2 2 0
1 1 0 0
8 0 3 0
11 3 0 3
10 3 1 0
6 2 0 3
6 2 0 2
1 1 3 3
8 3 1 3
11 3 0 0
10 0 0 3
6 3 2 1
8 3 0 0
5 0 2 0
6 3 2 2
0 0 2 2
8 2 1 2
11 3 2 3
10 3 3 0
8 0 0 3
5 3 0 3
6 1 3 1
6 2 2 2
7 3 2 3
8 3 1 3
8 3 3 3
11 0 3 0
10 0 0 3
6 3 2 2
6 2 1 0
0 0 2 1
8 1 3 1
8 1 2 1
11 1 3 3
8 1 0 2
5 2 2 2
6 3 1 0
6 3 3 1
2 2 1 1
8 1 1 1
11 3 1 3
10 3 1 1
6 3 2 3
6 2 0 0
6 3 0 2
13 0 2 3
8 3 3 3
8 3 1 3
11 3 1 1
10 1 2 3
8 2 0 0
5 0 1 0
8 3 0 2
5 2 2 2
8 0 0 1
5 1 0 1
11 0 0 1
8 1 2 1
11 3 1 3
10 3 3 0
6 0 1 3
6 3 3 1
6 3 1 2
14 3 2 3
8 3 3 3
11 0 3 0
10 0 2 3
6 1 0 1
6 1 0 0
6 2 3 2
10 0 2 1
8 1 1 1
8 1 2 1
11 1 3 3
6 1 1 2
6 0 3 1
5 0 1 2
8 2 3 2
11 3 2 3
10 3 2 1
8 3 0 3
5 3 2 3
8 1 0 0
5 0 2 0
6 3 3 2
0 0 2 3
8 3 2 3
8 3 1 3
11 3 1 1
10 1 2 0
6 1 3 3
8 3 0 2
5 2 1 2
6 0 1 1
5 3 1 3
8 3 1 3
11 0 3 0
10 0 3 1
6 1 3 0
6 0 3 3
8 1 0 2
5 2 2 2
7 3 2 3
8 3 2 3
11 3 1 1
6 0 0 2
6 2 3 3
6 0 3 0
14 2 3 0
8 0 1 0
8 0 1 0
11 0 1 1
10 1 0 3
6 3 0 2
6 2 2 0
6 2 0 1
13 0 2 1
8 1 3 1
8 1 1 1
11 3 1 3
10 3 2 1
6 3 1 3
6 1 2 0
8 0 2 2
8 2 3 2
11 1 2 1
6 1 0 3
6 3 3 2
11 0 0 2
8 2 2 2
11 1 2 1
10 1 2 3
6 0 3 0
8 1 0 2
5 2 2 2
6 3 2 1
2 2 1 2
8 2 1 2
11 3 2 3
6 0 0 2
6 1 3 1
8 1 2 0
8 0 3 0
8 0 2 0
11 0 3 3
10 3 2 2
6 2 2 0
6 1 2 3
3 0 3 0
8 0 2 0
11 2 0 2
10 2 3 3
6 0 0 1
6 2 0 0
6 3 3 2
0 0 2 2
8 2 1 2
8 2 1 2
11 2 3 3
10 3 1 0
6 2 3 1
6 2 3 3
8 3 0 2
5 2 0 2
14 2 3 2
8 2 3 2
11 2 0 0
10 0 0 1
8 2 0 2
5 2 0 2
6 1 0 0
1 0 3 3
8 3 2 3
8 3 2 3
11 1 3 1
6 2 2 0
6 2 0 3
6 1 3 2
15 0 3 0
8 0 2 0
11 1 0 1
10 1 2 0
6 3 0 1
6 0 3 2
6 1 1 3
12 1 2 1
8 1 1 1
11 1 0 0
10 0 1 2
6 3 1 1
6 3 1 0
5 3 1 3
8 3 3 3
11 2 3 2
10 2 1 0
6 0 2 3
6 2 3 2
8 1 0 1
5 1 0 1
7 3 2 3
8 3 1 3
8 3 1 3
11 3 0 0
6 2 1 1
6 3 0 2
6 0 1 3
14 3 2 2
8 2 2 2
11 2 0 0
8 2 0 2
5 2 0 2
6 1 1 3
6 2 3 1
8 1 3 1
11 0 1 0
10 0 0 1
6 2 1 0
6 3 1 2
1 3 0 0
8 0 2 0
11 0 1 1
10 1 3 0
6 0 3 3
6 1 1 1
14 3 2 1
8 1 2 1
11 0 1 0
10 0 3 2
6 2 3 1
6 2 0 0
6 1 1 3
1 3 0 0
8 0 1 0
11 0 2 2
10 2 0 1
6 0 2 2
6 2 0 3
6 2 1 0
15 0 3 3
8 3 2 3
8 3 3 3
11 1 3 1
6 3 0 0
8 0 0 3
5 3 1 3
11 3 3 3
8 3 1 3
8 3 1 3
11 3 1 1
10 1 3 3
6 2 0 0
6 3 0 2
8 0 0 1
5 1 2 1
0 0 2 1
8 1 1 1
8 1 1 1
11 1 3 3
10 3 1 2
6 2 0 1
6 2 3 3
15 0 3 1
8 1 1 1
8 1 1 1
11 1 2 2
10 2 1 0
6 0 2 3
6 3 3 1
6 2 1 2
7 3 2 3
8 3 3 3
11 3 0 0
10 0 0 2
6 2 3 0
6 0 0 3
2 0 1 3
8 3 3 3
11 2 3 2
10 2 2 3
6 1 0 2
6 1 0 1
1 1 0 1
8 1 1 1
11 3 1 3
8 1 0 1
5 1 3 1
2 0 1 1
8 1 3 1
11 3 1 3
10 3 0 2
6 2 2 3
8 2 0 1
5 1 3 1
15 0 3 1
8 1 1 1
8 1 1 1
11 2 1 2
10 2 3 0
6 3 0 1
6 1 0 3
6 2 1 2
2 2 1 2
8 2 2 2
8 2 3 2
11 0 2 0
10 0 0 1
6 1 0 0
6 2 2 2
10 0 2 2
8 2 1 2
11 1 2 1
10 1 2 0
6 3 1 1
6 2 0 3
6 2 0 2
9 2 3 2
8 2 1 2
11 0 2 0
10 0 2 2
6 1 1 3
6 0 2 1
8 0 0 0
5 0 1 0
5 3 1 3
8 3 2 3
11 3 2 2
10 2 0 1
6 1 2 2
6 2 1 3
1 0 3 2
8 2 2 2
11 2 1 1
6 3 3 2
6 2 3 0
15 0 3 2
8 2 2 2
11 2 1 1
10 1 2 0
6 2 1 1
6 3 3 2
9 1 3 3
8 3 3 3
11 0 3 0
10 0 2 1
6 0 0 2
6 2 0 0
6 1 1 3
1 3 0 2
8 2 1 2
8 2 1 2
11 2 1 1
10 1 0 0
6 2 2 1
6 0 0 3
6 3 2 2
13 1 2 2
8 2 3 2
11 0 2 0
10 0 0 2
6 2 2 3
8 2 0 0
5 0 0 0
6 3 1 1
6 3 0 0
8 0 3 0
11 0 2 2
10 2 0 1
6 0 1 2
6 3 2 0
6 0 1 3
12 0 2 3
8 3 1 3
8 3 2 3
11 3 1 1
10 1 3 2
6 2 2 1
6 2 3 0
6 3 2 3
4 3 1 1
8 1 2 1
11 2 1 2
10 2 0 0
8 3 0 3
5 3 1 3
6 1 1 1
6 3 3 2
8 3 2 3
8 3 2 3
11 0 3 0
6 2 3 2
6 2 0 1
6 2 0 3
9 1 3 3
8 3 1 3
11 0 3 0
10 0 1 2
6 1 3 3
6 2 2 0
11 3 3 3
8 3 2 3
11 2 3 2
6 2 0 3
6 3 1 1
15 0 3 0
8 0 2 0
8 0 2 0
11 2 0 2
10 2 2 3
8 2 0 0
5 0 1 0
8 3 0 1
5 1 1 1
6 0 0 2
8 1 2 2
8 2 2 2
8 2 1 2
11 3 2 3
10 3 2 0"""

befaft, test = intxt.split("\n"*4)
befaft = befaft.split("\n"*2)
test = test.split("\n")

class Device:
    _reg = [0, 0, 0, 0]
    def __init__(self, A=0, B=0, C=0, D=0):
        self._reg = [A, B, C, D]
    def __repr__(self):
        return(", ".join(map(str, self._reg)))
    def __iter__(self):
        return(iter(self._reg))
    def addr(self, A, B, C):
        self._reg[C] = self._reg[A] + self._reg[B]
    def addi(self, A, B, C):
        self._reg[C] = self._reg[A] + B
    def mulr(self, A, B, C):
        self._reg[C] = self._reg[A] * self._reg[B]
    def muli(self, A, B, C):
        self._reg[C] = self._reg[A] * B
    def banr(self, A, B, C):
        self._reg[C] = self._reg[A] & self._reg[B]
    def bani(self, A, B, C):
        self._reg[C] = self._reg[A] & B
    def borr(self, A, B, C):
        self._reg[C] = self._reg[A] | self._reg[B]
    def bori(self, A, B, C):
        self._reg[C] = self._reg[A] | B
    def setr(self, A, B, C):
        self._reg[C] = self._reg[A]
    def seti(self, A, B, C):
        self._reg[C] = A
    def gtir(self, A, B, C):
        self._reg[C] = int(A > self._reg[B])
    def gtri(self, A, B, C):
        self._reg[C] = int(self._reg[A] > B)
    def gtrr(self, A, B, C):
        self._reg[C] = int(self._reg[A] > self._reg[B])
    def eqir(self, A, B, C):
        self._reg[C] = int(A == self._reg[B])
    def eqri(self, A, B, C):
        self._reg[C] = int(self._reg[A] == B)
    def eqrr(self, A, B, C):
        self._reg[C] = int(self._reg[A] == self._reg[B])
    opsl = [addr, addi, mulr, muli,
           banr, bani, borr, bori,
           setr, seti, gtri, gtir,
           gtrr, eqir, eqri, eqrr,
           ]
    opsd = {}
    for f in opsl:
        opsd[f.__name__] = f

if __name__ == "__main__":
    totals = []
    for befaftsect in befaft:
        bef, code, aft = befaftsect.split("\n")
        bef = list(map(int, bef[9:-1].split(", ")))
        code = list(map(int, code.split(" ")))
        aft = list(map(int, aft[9:-1].split(", ")))
        com, A, B, C = code
        ctotal = [0, com, []]
        for op in Device.opsl:
            x = Device(*bef)
            op(x, A, B, C)
            if list(x) == aft:
                ctotal[0] += 1
                ctotal[2].append(op.__name__)
        totals.append(ctotal)

    print(len(list(filter(lambda x: x[0]>=3, totals))))

    def modes(l):
        counts = {}
        for i in l:
            counts[i] = counts.get(i, 0) + 1
        tops = [[None, float("-inf")]]
        for k, v in counts.items():
            if v > tops[0][1]:
                tops = [[k, v]]
            elif v == tops[0][1]:
                tops.append([k, v])
        return(tops)

    comsets = {}
    compfuncs = {}
    comfunc = {}
    usedfuncs = []
    for n, com, possibles in totals:
        comsets[com] = comsets.get(com, []) + possibles
    for com in range(0, 16):
        compfuncs[com] = modes(comsets[com])
    for x in range(0, 40):
        compfuncs.items()
        new_compfuncs = {}
        for com, fnames in compfuncs.items():
            if len(fnames) == 1:
                comfunc[com] = fnames[0][0]
                usedfuncs.append(fnames[0][0])
            elif len(fnames) == 0:
                raise Warning(str(com)+" has no possible functions!")
            else:
                new_fnames = [n for n in fnames if n[0] not in usedfuncs]
                new_compfuncs[com] = new_fnames
        compfuncs = new_compfuncs

    test = map(lambda x: map(int, x.split(" ")), test)
    dev = Device()
    for c, A, B, C in test:
        dev.opsd[comfunc[c]](dev, A, B, C)
    print(dev)
