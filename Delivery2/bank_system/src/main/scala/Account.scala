//Task 1.2 & 1.3
class Account(val code : String, val balance: Double) {

    //Withdraw as long as the amount is positive and less than the balance
    def withdraw(amount: Double) : Either[String, Account] = {
        if (amount < 0 || amount > balance) {
            return Left("Can not withdraw this amount")
        } 
        return Right(new Account(code, balance - amount))
    }

    //Deposit as long as the amount is positive
    def deposit (amount: Double) : Either[String, Account] = {
        if (amount < 0) {
            return Left("Invalid amount")
        } 
        return Right(new Account(code, balance + amount))
    }

    //In both cases, on success (right), return a new account with the updated balance, but same code
}
