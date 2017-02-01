def fact(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
    	if (n <= 0) 1
	else go(n - 1, n * acc)
    }
    go(n, 1)
}

def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, current: Int, next: Int): Int {
    	if (n <= 0) current
	else go(n - 1, next, current + next)
    }
    go(n, 0, 1)
}