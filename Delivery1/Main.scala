object Main extends App {
    
    //Task 1a
    def createArray() = {
        var array = new Array[Int](50)
        for(i <- 1 to 50) {
            array(i-1) = i
        }
        println(array.mkString(", "))
    }
    createArray()
    
    //Task 1b
    def sumArray(array: Array[Int]): Int = {
        var sum = 0
        for(i <- array) {
            sum += i
        }
        return sum
    }
    println(sumArray(Array(1, 2, 3, 4, 5)))

    //Task 1c
    def sumArrayRecursive(array: Array[Int], i: Int): Int = {
        if(i == array.length) {
            return 0
        }
        return array(i) + sumArrayRecursive(array, i + 1)
    }
    println(sumArrayRecursive(Array(1, 2, 3, 4, 5), 0))

    //Task 1d
    def getFibonacci(n: BigInt): BigInt = {
        if(n == 0) {
            return 0
        }
        if(n == 1) {
            return 1
        }
        return getFibonacci(n - 1) + getFibonacci(n - 2)
    }
    println(getFibonacci(10))
    /*The difference between BigInt and Int is that BigInt has a bigger maximum value that it can stoe compared to Int, 
    which is useful in exponential calculations like the Fibonacci sequence.*/



    //Task 2a
    def quadraticEquation(A: Double, B: Double, C: Double, realSol: Array[Boolean], x1: Array[Double], x2: Array[Double]): Unit = {
        val discriminant = B * B - 4.0 * A * C
        realSol(0) = discriminant >= 0.0
        if (realSol(0)) {
            val sqrtDiscriminant = Math.sqrt(discriminant)
            x1(0) = (-B + sqrtDiscriminant) / (2.0 * A)
            x2(0) = (-B - sqrtDiscriminant) / (2.0 * A)
        }
    }
    //Arrays are created as they can be passed and mutated inside the quadraticEquation
    val realSol = Array(false)
    val x1 = Array(0.0)
    val x2 = Array(0.0)
    quadraticEquation(2.0, 1.0, -1.0, realSol, x1, x2)
    println(s"${realSol(0)}, ${x1(0)}, ${x2(0)}")


    def quadratic(A: Double, B: Double, C: Double): Double => Double = {
        (x: Double) => A * x * x + B * x + C
    }
    val quadraticFunction = quadratic(3, 2, 1)
    println(quadraticFunction(2))


    //Task 2b
    /* The quadraticEquation function is implemented by creating arrays that can be mutated inside the function to mimic the bindings that occur inside the function in oz.
    This allows the function to mutate the values of the arrays and thus "return" the results without having to return multiple values explicitly.
    The quadratic function is implemented using a higher order function that returns a function that takes a double and returns a double, very similarly to the oz implementation,
    with the main difference being the syntax used to implement it. */



    //Task 3a
    def createThreadWithFunction(function: () => Unit): Thread = {
        //Runnable makes it possible to run custom behavior in the thread, such as a function
        val thread = new Thread(new Runnable {
            def run() = {
                //The function that is passed into this function is called here when the thread is started
                function()
            }
        })
        return thread
    }

    val simpleFunction = () => println("Function has been called")
    val thread = createThreadWithFunction(simpleFunction)
    println("Thread created, but not started yet")
    //Thread is started -> function is called with thread.start()
    thread.start()


    //Task 3b
    /* The code is supposed to increment the value of value1 while decreasing the value of value2, and then update the sum of the two values. This should happen until the value of value1 is 0,
    and at this point the values should be reset to 1000 and 0 respectively. The state of these values, in addition the sum, is logged to the console continously. The sum of the values should always be 1000.
    This is not working as expected, as there are errors with this code that leads to the state of the values being incosistent and the sum not being 1000. This is likely happening because there is no form 
    of synchronization between the two threads, this can for example lead to the threads reading from or writing to a value at the same time etc., which leads to invalidating eachothers read information and updates. 
    I assume that it would be possible to experience the same behavior in Oz, as concurrent threads can be created just as loosely as the ones we have in this example. I think it therefore in both cases important
    to use some form of synchronization to avoid these issues, regardless of the implementation language. Synchronization issues like these would be very fatal in a system like a banking system (like the one 
    in this scala project), as if a bank account has both an incoming and outgoing transaction at the same time without synchronization it could lead to the balance of the bank account being updated incorrectly.*/
    
    
    //Task 3c - See seperate file
}
