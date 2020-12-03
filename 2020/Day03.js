function Day03() {
  rows = document.getElementById("Day03_id").value.split("\n");

  var i;
  var count = 0;

  for (i = 0; i < rows.length; i++) {
    if (rows[i][(i * 3) % rows[i].length] == "#") {
      count++;
    }
  }
  document.getElementById("Day03_p1").innerHTML = count

  var i, n, count;
  var product = 1;

  for (n = 1; n < 8; n = n + 2) {
    count = 0;
    for (i = 0; i < rows.length; i++) {
      if (rows[i][(i * n) % rows[i].length] == "#") {
        count++;
      }
    }
    product = product * count;
  }

  count = 0;
  for (i = 0; i < rows.length; i = i + 2) {
    if (rows[i][(i / 2) % rows[i].length] == "#") {
      count++;
    }
  }
  product = product * count;

  document.getElementById("Day03_p2").innerHTML = product
}
