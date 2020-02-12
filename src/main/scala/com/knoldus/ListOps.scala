package com.knoldus

class ListOps {

  def findLastElement(list: List[Int]) = {
    @scala.annotation.tailrec
    def findLastElementRecursive(list: List[Int], index: Int): List[Int] = {
      list match {
        case Nil => List(-1)
        case head :: Nil => List(head, index)
        case _ :: rest => findLastElementRecursive(rest, index + 1)
      }
    }

    findLastElementRecursive(list, 0)
  }

  def printTable(list: List[Int]) = {
    val element = for (i <- list) yield i
    val tableOfEachElement = element.map(i => for (j <- 1 to 10) yield i * j)
    element zip tableOfEachElement
  }

  def aggregateLists(list1: List[Int], list2: List[String]) = {
    list1 zip list2
  }

  def findSum(list: List[Int]) = {
    @scala.annotation.tailrec
    def findSumRecursive(list: List[Int], sum: Int): Int = {
      list match {
        case Nil => -1
        case head :: Nil => head + sum
        case head :: rest => findSumRecursive(rest, head + sum)
      }
    }

    findSumRecursive(list, 0)
  }

  def findMultiplication(list: List[Int]) = {
    @scala.annotation.tailrec
    def findMultiplicationRecursive(list: List[Int], multiply: Int): Int = {
      list match {
        case Nil => -1
        case head :: Nil => head * multiply
        case head :: rest => findMultiplicationRecursive(rest, head * multiply)
      }
    }

    findMultiplicationRecursive(list, 1)
  }

  def enqueue(list: List[Int], element: Int): List[Int] = {
    list :+ element
  }

  def dequeue(list: List[Int]): List[Int] = {
    list.tail
  }

  def push(list: List[Int], element: Int): List[Int] = {
    element +: list
  }

  def pop(list: List[Int]): List[Int] = {
    list.tail
  }

}

object ListOps extends App {
  val lOps = new ListOps
  println(lOps.findLastElement(List(10, 20, 30, 40)))
  println(lOps.printTable(List(1, 2, 3)))
  println(lOps.aggregateLists(List(1,2,3), List("a", "b", "c")))
  println(lOps.findSum(List(5, 7, 3, 9)))
  println(lOps.findMultiplication(List(2, 3, 4, 1)))
  println(lOps.enqueue(List(21, 44, 87, 33), 67))
  println(lOps.dequeue(List(21, 44, 87, 33, 67)))
  println(lOps.push(List(21, 44, 87, 33), 67))
  println(lOps.pop(List(21, 44, 87, 33, 67)))
}
