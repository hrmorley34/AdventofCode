var Day02_regex = /^(\d+)-(\d+) ([a-z]): ([a-z]*)$/;

function Day03_count(string, letter) {
  var count = 0;
  for (var i = 0; i < string.length; i++) {
    if (string[i] == letter) {
      count++;
    }
  }
  return count;
}

function Day02() {
  rows = document.getElementById("Day02_id").value.split("\n");

  var count_p1 = 0;
  var count_p2 = 0;
  for (var i = 0; i < rows.length; i++) {
    match = rows[i].match(Day02_regex);
    if (!match) {
      continue;
    }

    lettercount = Day03_count(match[4], match[3]);
    if (match[1] <= lettercount && lettercount <= match[2]) {
      count_p1++;
    }

    if ((match[4][match[1] - 1] == match[3]) ^ (match[4][match[2] - 1] == match[3])) {
      count_p2++;
    }
  }

  document.getElementById("Day02_p1").innerHTML = count_p1;
  document.getElementById("Day02_p2").innerHTML = count_p2;
}
