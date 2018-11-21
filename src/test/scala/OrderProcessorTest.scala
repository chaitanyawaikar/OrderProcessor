import org.scalatest.FunSuite
import org.scalatest.mockito.MockitoSugar

import scala.collection.mutable.ArrayBuffer

class OrderProcessorTest extends FunSuite with MockitoSugar{

  test(" should calculate the average waiting time given the list of customers in the order in which they arrive for input1"){
    val customerList = ArrayBuffer(Customer(0,3), Customer(1,9), Customer(2,6))
    assert(OrderProcessor.calculateAverageWaitingTime(customerList) == 9)
  }

  test("should calculate the average waiting time given the list of customers in the order in which they arrive for input2"){
    val customerList = ArrayBuffer(Customer(0,3), Customer(1,9), Customer(2,5))
    assert(OrderProcessor.calculateAverageWaitingTime(customerList) == 8)
  }
}
