object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

// Task 1.1
class TransactionPool {

    // Data structure to hold transactions
    var pool = scala.collection.mutable.Queue[Transaction]()

     // Remove and the transaction from the pool
    def remove(t: Transaction): Boolean = {
      pool.synchronized {
        // Implemented like this to fit with the requirement of returning a boolean
        pool.dequeueFirst(_ == t) match {
          case Some(_) => true
          case None => false
        }
      }
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = {
      return pool.synchronized(pool.isEmpty)
    }

    // Return the size of the pool
    def size: Integer = {
      return pool.synchronized(pool.size)
    }

    // Add new element to the back of the queue
    def add(t: Transaction): Boolean = {
      return pool.synchronized {
        pool.enqueue(t)
        return true
      }
    }
    
    // Return an iterator to allow you to iterate over the queue
    def iterator : Iterator[Transaction] = {
      return pool.synchronized(pool.iterator)
    }

}

class Transaction(val from: String,
                  val to: String,
                  val amount: Double,
                  val retries: Int = 3) {

  private var status: TransactionStatus.Value = TransactionStatus.PENDING
  private var attempts = 0

  def getStatus() = status

  // TODO: Implement methods that change the status of the transaction
  def setStatus(newStatus: TransactionStatus.Value) = {
    status = newStatus
  }
}
