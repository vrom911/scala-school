package homework.l2


/**
  * Вам нужно реализовать функции sumOfSquares и multiplicationOfCubes
  * при помощи ListFunctions.fold, композиции и частичного применения функций, sum, multiply и pow
  * Можно добовлять промежуточные функции.
  * Также вам может понадобится функция Function.uncurry,
  * которая из карированной функции делает функцию с несколькими аргументами
  */
object ListHomework {

  val sum = (a: Int, b: Int) => a + b

  val multiply = (a: Int, b: Int) => a * b

  def pow(a: Int, p: Int): Int = if(p <= 0) 1 else a * pow(a, p - 1)

  def powN(n: Int)(a: Int): Int = pow(a, n)

  /**
    * Raises elements of list in given power and folds elements with combining function.
    * @param startValue  starting value
    * @param f           combining function
    * @param p           power
    * @tparam B          type of result
    * @return            result of folding
    */
  def aggregate[B](startValue: B, f: ((B, Int) => B), p: Int): (List[Int] => B) =
    ListFunctions.fold(startValue, _)(Function.uncurried(f.curried(_) compose powN(p)))

  /**
    * Сумма квадратов чисел в списке
    */
  lazy val sumOfSquares: List[Int] => Int = aggregate(0, sum, 2)

  /**
    * Сумма кубов чисел в списке
    */
  lazy val multiplicationOfCubes: List[Int] => Int = aggregate(1, multiply, 3)

}
