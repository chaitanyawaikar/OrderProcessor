import java.io.{BufferedReader, InputStream, InputStreamReader}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class Customer(timeAtWhichPizzaOrdered: Int, timeTakenToPreparePizza: Int)

object OrderProcessor {

  def main(args: Array[String]): Unit = {
    process(System.in) match {
      case Right(waitingTime) => println(waitingTime)
      case Left(errorMessage) => println(errorMessage)
    }
  }

  def process(in: InputStream): Either[String, Long] = {
    readInputFromConsole(System.in) match {
      case Right(list) =>
        Right(calculateAverageWaitingTime(list.sortBy(_.timeAtWhichPizzaOrdered)))
      case Left(ex) => Left("There was some problem processing the input")
    }
  }

  def readInputFromConsole(in: InputStream): Either[Throwable, ArrayBuffer[Customer]] = {
    val bufferedReader = new BufferedReader(new InputStreamReader(in))
    Try {
      val noOfCustomers = bufferedReader.readLine().toInt
      val arrayBuffer = ArrayBuffer[Customer]()
      for (i <- 0 until noOfCustomers) {
        val input = bufferedReader.readLine().split(" ")
        arrayBuffer += Customer(input(0).toInt, input(1).toInt)
      }
      arrayBuffer
    } match {
      case scala.util.Success(customerList) =>
        Right(customerList)
      case scala.util.Failure(ex) => Left(ex)
    }
  }

  def customerOrdering = new Ordering[Customer] {
    def compare(a: Customer, b: Customer) = {
      b.timeTakenToPreparePizza compare a.timeTakenToPreparePizza
    }
  }

  def calculateAverageWaitingTime(customers: ArrayBuffer[Customer]): Long = {
    val noOfCustomers = customers.size
    var totalTime = 0L
    var currTime = 0L
    var idx = 0
    val pQueue = new mutable.PriorityQueue[Customer]()(customerOrdering)

    while (customers.nonEmpty || pQueue.nonEmpty) {
      while (customers.nonEmpty && (pQueue.isEmpty || customers(0).timeAtWhichPizzaOrdered <= currTime)) {
        val c = customers.head
        customers -= c
        pQueue.enqueue(c)
        currTime = Math.max(currTime, c.timeAtWhichPizzaOrdered)
      }

      val c = pQueue.dequeue()
      currTime += c.timeTakenToPreparePizza
      totalTime += currTime - c.timeAtWhichPizzaOrdered
    }

    val avgWaitTime = totalTime / noOfCustomers
    avgWaitTime
  }
}

