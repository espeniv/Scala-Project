import collection.mutable.Map

//Task 2
class Bank(val allowedAttempts: Integer = 3) {

    val rand = new scala.util.Random

    private val accountsRegistry : Map[String,Account] = Map()

    val transactionsPool: TransactionPool = new TransactionPool()
    val completedTransactions: TransactionPool = new TransactionPool()

    def processing : Boolean = !transactionsPool.isEmpty

    def transfer(from: String, to: String, amount: Double): Unit = {
        val newTransaction = new Transaction(from, to, amount)
        transactionsPool.add(newTransaction)
    }


    def processTransactions: Unit = {

        val workers : List[Thread] = transactionsPool.iterator.toList
                                            .filter(t => t.getStatus() == TransactionStatus.PENDING)
                                            .map(t => processSingleTransaction(t))

        workers.map(element => element.start())
        workers.map(element => element.join())

        val succeded : List[Transaction] = transactionsPool.iterator.toList
                                            .filter(t => t.getStatus() == TransactionStatus.SUCCESS)

        val failed : List[Transaction] = transactionsPool.iterator.toList
                                            .filter(t => t.getStatus() == TransactionStatus.FAILED)

        succeded.foreach(t => transactionsPool.remove(t))
        succeded.foreach(t => completedTransactions.add(t))

        failed.foreach(t => {
        if (t.getAttempts < t.retries) {
            t.setStatus(TransactionStatus.PENDING)
            t.incrementAttempts()
        } else {
            transactionsPool.remove(t)
            completedTransactions.add(t)
            }
        })

        if(!transactionsPool.isEmpty) {
            processTransactions
        }
    }


    private def processSingleTransaction(t : Transaction) : Thread =  {
        val transactionThread = new Thread {
            val fromAccount = getAccount(t.from)
            val toAccount = getAccount(t.to)

            if(fromAccount.isEmpty || toAccount.isEmpty) {
                t.setStatus(TransactionStatus.FAILED)
            }
            //Kinda hacky to conform with the previous requirement of returning the Either type from withdraw and deposit
            fromAccount.get.withdraw(t.amount) match {
                case Right(updatedFromAccount) =>
                    toAccount.get.deposit(t.amount) match {
                        case Right(updatedToAccount) =>
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


    def createAccount(initialBalance: Double) : String = {
        val randomCode: String = rand.nextInt().toString
        val newAccount: Account = new Account(randomCode, initialBalance)
        accountsRegistry += (randomCode -> newAccount)
        return randomCode
    }


    def getAccount(code : String) : Option[Account] = {
        if(!accountsRegistry.contains(code)) {
            return None
        } else {
            return Some(accountsRegistry(code))
        }
    }
}
