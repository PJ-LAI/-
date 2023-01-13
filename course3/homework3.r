# hw.3 find prime number in seq(2, 100, by = 1)
x <- c()
prime <- function(x) {
        y <- sort(x, decreasing = FALSE)
        ref <- seq(2, max(y), by = 1)
        i <- 1
        k <- 1
        done <- FALSE
        non_prime <- c()
        sample <- 1
        while (!done) {
            while (y[i] > ref[k]) {
                if ((y[i] %% ref[k]) == 0) {
                    non_prime[sample] <- y[i]
                    sample <- sample + 1
                    break
                } else {
                    k <- k + 1
                }
            }
            if (i == length(y)) {
                done <- TRUE
            }
            i <- i + 1
            k <- 1
        }
        return((setdiff(y, non_prime)))
    }
prime(seq(2, 100, by = 1))

# homework3 answer
Prime <- function(testno){
    done <- FALSE
    i <- 2
    while (!done){

        if (testno %% i == 0) {
            prime <- 0
            done <- TRUE
        }       

        if(i == testno) {
            prime <- 1
            done <- TRUE
        } 
        i <- i+1
    }
    return(prime)
}
non_prime <- c()
 for(i in 2:100) {
    if(Prime(i) == 1) {
        non_prime <- c(non_prime, i)
    }
}
non_prime

