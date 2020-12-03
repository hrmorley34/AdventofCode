function Day01() {
  numbers = document.getElementById("Day01_id").value.split("\n");

  var a, b, c;

  document.getElementById("Day01_p1").innerHTML = ""
  for (a = 0; a < numbers.length; a++) {
    for (b = a + 1; b < numbers.length; b++) {
      if (Number(numbers[a]) + Number(numbers[b]) == 2020) {
        document.getElementById("Day01_p1").innerHTML += Number(numbers[a]) * Number(numbers[b])
        document.getElementById("Day01_p1").innerHTML += "<br>"
      }
    }
  }

  document.getElementById("Day01_p2").innerHTML = ""
  for (a = 0; a < numbers.length; a++) {
    for (b = a + 1; b < numbers.length; b++) {
      for (c = b + 1; c < numbers.length; c++) {
        if (Number(numbers[a]) + Number(numbers[b]) + Number(numbers[c]) == 2020) {
          document.getElementById("Day01_p2").innerHTML += Number(numbers[a]) * Number(numbers[b]) * Number(numbers[c])
          document.getElementById("Day01_p2").innerHTML += "<br>"
        }
      }
    }
  }
}
