def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
    	if (as.length - n < 2) true
	else gt(as(0), as(1)) && go(n + 1)
    }
    go(0)
}