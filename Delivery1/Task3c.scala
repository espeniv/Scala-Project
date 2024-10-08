object ConcurrencyTroubles extends App {

    private var value1: Int = 1000
    private var value2: Int = 0
    private var sum: Int = 0

    def moveOneUnit(): Unit = synchronized {
        value1 -= 1
        value2 += 1
        if (value1 == 0) {
            value1 = 1000
            value2 = 0
        }
    }

    def updateSum(): Unit = synchronized {
        sum = value1 + value2
    }

    def execute(): Unit = {
        while (true) {
            moveOneUnit()
            updateSum()
            Thread.sleep(100)
        }
    }

    for (i <- 1 to 2) {
        val thread = new Thread {
            override def run = execute()
        }
        thread.start()
    }

    while (true) {
        updateSum()
        println(sum + " [" + value1 + " " + value2 + "]")
    }
}

/* The soltution i have implemented is the most simple syntax-wise, and utilizes the synchronized keyword, wrapping the blocks of code that has to be synchronized.
This is the parts of the code where the values shared between the threads are being updated. This makes sure that no operation is
executed on the values in one thread before the other thread has finished its operation. Another way to synchronize the threads is by using 
the java class AtomicInteger, which is specific for integers. This ensure that the operations performed on the values are atomic,
meaning that no other thread can interfere with the operation. It also ensures that the changes made by one thread are visible to other threads
immediately, and this way it prevents issues with incosistent data. */