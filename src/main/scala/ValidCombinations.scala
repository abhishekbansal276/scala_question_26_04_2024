class ValidCombinations {
  def generateParenthesis(n: Int): List[String] = {
    def backtrack(left: Int, right: Int, current: String, result: List[String]): List[String] = {
      if (left == 0 && right == 0) {
        current :: result
      } else {
        var newResult = result
        if (left > 0) {
          newResult = backtrack(left - 1, right, current + "(", newResult)
        }
        if (right > left) {
          newResult = backtrack(left, right - 1, current + ")", newResult)
        }
        newResult
      }
    }

    backtrack(n, n, "", List.empty[String])
  }
}

object Main extends App{
  private val validCombinations = new ValidCombinations
  val ans = validCombinations.generateParenthesis(3)
  println(ans)
}