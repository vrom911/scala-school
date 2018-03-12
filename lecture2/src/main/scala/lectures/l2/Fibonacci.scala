package lectures.l2

/**
  * Цель упражнения: вычислить 9 - е число Фибоначчи
  * Для этого раскомментируйте строчку в методе fibs и исправьте ошибку компиляции.
  *
  * Данная реализация вычисления чисел фибоначчи крайне не оптимальна (имеет показатеьную сложность O(a.pow(n)) )
  * Для того, что бы в этом убедиться, Вы можете раскомментировать
  * строчку с вычислением 1000-ого числа фибоначчи
  *
  */
object Fibonacci extends App {

  // Task 2
  def fibs(num: Int) : Int = {
    if (num == 1 || num == 2) 1
    else fibs(num - 1) + fibs(num - 2)
  }

  println(fibs(9))
  //println(fibs(1000))
}

/**
  * Цель упражнения: используя приемы динамического программирования,
  * реализовать более оптимальный алгоритм подсчета чисел фибоначчи
  * Для этого нужно реализовать функцию fibsImpl.
  * Сигнатуру функции Вы можете расширять по своему усмотрению,
  * но реализация должна удовлетворять следующим требованиям
  * * * * метод fibsImpl - должен быть tail recursive
  * * * * параметр acc - аккумулятор посчитанных значений
  *
  */
object Fibonacci2 extends App {

  def fibs2(num: Int) =
    if (num <= 3) Array(1, 1, 2)(num - 1)
    else {
      val res : Array[Int] = fibsImpl(num, 3, Array(1, 2))
      res(0) + res(1)
    }
  private def fibsImpl(num: Int, cur: Int, acc: Array[Int]): Array[Int] =
    if (cur < num - 1) fibsImpl(num, cur + 1, Array(acc(1), acc(0) + acc(1)))
    else acc

  println(fibs2(16))
  //println(fibs(1000))
}




