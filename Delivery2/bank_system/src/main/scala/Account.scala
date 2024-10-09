
//Task 1.2 & 1.3
class Account(val code : String, val balance: Double) {

    def withdraw(amount: Double) : Either[String, Account] = {
        if (amount < 0 || amount > balance) {
            return Left("Can not withdraw this amount")
        } 
        return Right(new Account(code, balance - amount))
    }

    def deposit (amount: Double) : Either[String, Account] = {
        if (amount < 0) {
            return Left("Invalid amount")
        } 
        return Right(new Account(code, balance + amount))
    }

}
