import collection.mutable.Map

//Task 2
class Bank(val allowedAttempts: Integer = 3) {

    //For account code generation
    val rand = new scala.util.Random

    //Map to store accounts
    private val accountsRegistry : Map[String,Account] = Map()

    val transactionsPool: TransactionPool = new TransactionPool()
    val completedTransactions: TransactionPool = new TransactionPool()

    //Returns whether there are transactions to process
    def processing : Boolean = !transactionsPool.isEmpty

    //Creates a transaction and adds it to the pool
    def transfer(from: String, to: String, amount: Double): Unit = {
        val newTransaction = new Transaction(from, to, amount)
        transactionsPool.add(newTransaction)
    }

    def processTransactions: Unit = {

        //Crate a list of threads to process transactions, one for each pending transaction in transactions pool
        val workers : List[Thread] = transactionsPool.iterator.toList
                                            .filter(t => t.getStatus() == TransactionStatus.PENDING)
                                            .map(t => processSingleTransaction(t))
        
        //Start each thread to process transactions concurrently
        workers.map(element => element.start())
        workers.map(element => element.join())

        //Filter successfull transactions
        val succeded : List[Transaction] = transactionsPool.iterator.toList
                                            .filter(t => t.getStatus() == TransactionStatus.SUCCESS)
        //Filter transactions that failed
        val failed : List[Transaction] = transactionsPool.iterator.toList
                                            .filter(t => t.getStatus() == TransactionStatus.FAILED)

        //Remove each of the successfull transactions and add them to the completed transactions pool
        succeded.foreach(t => transactionsPool.remove(t))
        succeded.foreach(t => completedTransactions.add(t))

        //For the failed transactions we have to check if they can be retried (have more attempts remaining)
        failed.foreach(t => {
        if (t.getAttempts < t.retries) {
            t.setStatus(TransactionStatus.PENDING)
            t.incrementAttempts()
        } else {
            //If not they are removed and added to completed transactions
            transactionsPool.remove(t)
            completedTransactions.add(t)
            }
        })

        //Looping until there are no more transactions to process
        if(!transactionsPool.isEmpty) {
            processTransactions
        }
    }

    //Function to process a single transaction
    private def processSingleTransaction(t : Transaction) : Thread =  {
        val transactionThread = new Thread {
            val fromAccount = getAccount(t.from)
            val toAccount = getAccount(t.to)

            if(fromAccount.isEmpty || toAccount.isEmpty) {
                t.setStatus(TransactionStatus.FAILED)
            }
            //Kinda hacky to conform with the previous requirement of returning the Either type from withdraw and deposit
            fromAccount.get.withdraw(t.amount) match {
                //Left for failures and Right for success
                case Right(updatedFromAccount) =>
                    toAccount.get.deposit(t.amount) match {
                        case Right(updatedToAccount) =>
                            //Adding the updated accounts to the registry, overwriting the old mappings
                            accountsRegistry += (updatedFromAccount.code -> updatedFromAccount)
                            accountsRegistry += (updatedToAccount.code -> updatedToAccount)
                            t.setStatus(TransactionStatus.SUCCESS)
                        case Left(_) =>
                            t.setStatus(TransactionStatus.FAILED)
                            //Depositing the withdrawed amount back to the original account if failed
                            fromAccount.get.deposit(t.amount)
                    }
                case Left(_) =>
                    t.setStatus(TransactionStatus.FAILED)
            }
        }
        return transactionThread
    }

    //Function to create accounts, returning the account code which is used to interact with the account
    def createAccount(initialBalance: Double) : String = {
        //Using scala.util.Random to generate a random code
        val randomCode: String = rand.nextInt().toString
        val newAccount: Account = new Account(randomCode, initialBalance)
        //Adding the new account to the map of accounts
        accountsRegistry += (randomCode -> newAccount)
        return randomCode
    }

    //Returns an account based on the provided code
    def getAccount(code : String) : Option[Account] = {
        if(!accountsRegistry.contains(code)) {
            return None
        } else {
            return Some(accountsRegistry(code))
        }
    }
}
