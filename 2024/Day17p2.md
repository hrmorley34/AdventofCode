# Day 17 Part 2

## Example

The script is
```
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
```
which looks like:
- `0,3` - `adv`: `a >>= 3`
- `5,4` - `out a%8`
- `3,0` - `jnz 0`
Therefore, it loops through A, dividing by 8 and outputting mod 8 until A is 0. Hence, we need `A = 0o0345300` (with the script digits written backwards and an extra 0 to be removed at the start).

## Puzzle Input
```
Register A: 41644071
Register B: 0
Register C: 0

Program: 2,4,1,2,7,5,1,7,4,4,0,3,5,5,3,0
```
- `2,4` - `bst A`: `b = a`
- `1,2` - `bxl 2`: `b = b ^ 2 = a ^ 0b010`
- `7,5` - `cdv B`: `c = a >> b = a >> (a ^ 0b010)`
- `1,7` - `bxl 7`: `b = b ^ 7 = (a^0b010)^0b111 = a ^ 0b101`
- `4,4` - `bxc`: `b = b ^ c = a ^ 0b101 ^ (a >> (a ^ 0b010))`
- `0,3` - `adv 3`: `a = a >> 3`
- `5,5` - `out B`: `out a0 ^ 0b101 ^ (a0 >> (a0 ^ 0b010))`
- `3,0` - `jnz 0`: Repeat if A is non-zero.

So on each loop, we get
```
a(n+1) = a(n) >> 3
b(n+1) = a(n) ^ 0b101 ^ (a(n) >> (a(n) ^ 0b010))
c(n+1) = a(n) >> b(n)
OUT [b(n+1)]
```
C is unused between loops.

Note that `a(n)` depends on the _later_ values of the output.
Working backwards, we want
```
b(N) = 0 = a(N-1) ^ 0b101 ^ (a(N-1) >> (a(N-1) ^ 0b010))
    => 0 ^ 0b101 = a(N-1) ^ (a(N-1) >> (0 ^ 2))
    => 5 = a(N-1) ^ (a(N-1)>>2)
a = 0o5...
```
If we keep working in this direction, the next octal digit cannot affect the previous.
