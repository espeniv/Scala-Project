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
    /*The difference between BigInt and Int is that BigInt has a bigger maximum value that it can store compared to Int, 
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
}