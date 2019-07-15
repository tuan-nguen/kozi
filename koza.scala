object Koza{
  def NK(input: String): (Int, Int) = {
    var strArray = input.split(" ")
    (strArray(0).toInt, strArray(1).toInt)
  }
  
  def stringToArray(input: String): Array[Int] = {
    var strArray = input.split(" ")
    var result = strArray.map(_.toInt)
    result
  }

  def sort(a: Array[Int], min: Int, max: Int): Array[Int] = {
    def key(value: Int): Int = {
      return value - min
    }

    val result: Array[Int] = new Array[Int](a.length)

    // Count how many of each key we have
    val count: Array[Int] = new Array[Int](max - min + 1)
    a.foreach( (e: Int) => count(key(e)) += 1)

    // Add preceding counts to compute offset for each key
    for (i <- 1 to (max-min)) {
      count(i) += count(i-1)
    }

    // Assemble results using offset and sorted keys
    for (e <- a.reverseIterator) {
      count(key(e)) -= 1
      result(count(key(e))) = e
    }
    return result
  }

  def main(args: Array[String]): Unit = {
    print("Input n and k: ")
    val input = scala.io.StdIn.readLine()
    val (n, k) = NK(input)
    
    print("Input kozi: ")
    val inputStr = scala.io.StdIn.readLine()
    val neshto = stringToArray(inputStr)
    var sortedInput = sort(neshto, neshto.min, neshto.max)
    var capacity = sortedInput(sortedInput.length - 1)

    var left = true
    while(left){
      var numberIter = k; var bitMap = new Array[Boolean](n)
      while(numberIter > 0){
        numberIter -= 1 
        var sum = 0; var i = n - 1
        // i = -1 if there are no elements left to be added to the sum in this iteration
        while(i != -1){
          i = n - 1
          // Decrease index if the requirements are not met
          while(i >= 0 && !(bitMap(i) == false && sum + sortedInput(i) <= capacity)) i -= 1
          if(i != -1){ sum += sortedInput(i); bitMap(i) = true }
        }
        // Checks whether there are any kozi left
        var iter = 0
        while(iter < n && bitMap(iter)) iter += 1
        if(iter == n) left = false 
      }
      //println("Capacity is: " + capacity)
      if(left) capacity += 1
    }
    println(capacity)
  } 
}