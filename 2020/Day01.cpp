#include <iostream>
using namespace std;

const int length = 200;
int numbers [length];

int main() {
  int a, b, c, anum, bnum, cnum, i, num;

  for (int i = 0; i < length; i++) {
    cout << i + 1 << ": ";
    cin >> numbers[i];
  }

  cout << "Part 1:\n";
  for (int a = 0; a < length; a++) {
    anum = numbers[a];
    for (int b = a; b < length; b++) {
      bnum = numbers[b];
      if (anum + bnum == 2020) {
        cout << anum << " + " << bnum;
        cout << " -> " << anum * bnum << "\n";
      }
    }
  }

  cout << "Part 2:\n";
  for (int a = 0; a < length; a++) {
    anum = numbers[a];
    for (int b = a; b < length; b++) {
      bnum = numbers[b];
      for (int c = b; c < length; c++) {
        cnum = numbers[c];
        if (anum + bnum + cnum == 2020) {
          cout << anum << " + " << bnum << " + " << cnum;
          cout << " -> " << anum * bnum * cnum << "\n";
        }
      }
    }
  }
}
