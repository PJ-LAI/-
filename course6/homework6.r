# EX. 17-1 given 100 students, prob to pass exam is p, observed 20 students pass

# exact 95% CI
    # Pl : P(X = 0) + P(X = 1) + ...+ P(X = 19) = 0.975

        # find initial value
        p <- seq(0, 1, by = 0.001)
        fp <- function(p) {
            y <- -0.975
            for (k in 0:19) {
            y <- y + choose(100, k) * p^k * (1 - p)^(100 - k)
            }
            return(y)
        }
        png(filename = "hwk6.1.png", width = 1000, height = 1000, res = 200)
        plot(fp)
        dev.off()
       
        # find f(p), f'(p)
        ftn <- function(p) {
            y <- -0.975
            dydp <- 0

            for (k in 0:19) {
            y <- y + choose(100, k) * (p^k) * ((1 - p)^(100 - k))

            dydp <- dydp + choose(100, k) * ((k * p^(k - 1) * (1 - p)^(100 - k))  # nolint
            - (p^k * (100 - k) * (1 - p)^(99 - k)))
            }
            

            return(c(y, dydp))
        }

        # Newton-Raphson function
        root <- function(ftn, x0, tol, max_iter) {
                x <- x0
                y <- ftn(x)
                iter <- 0
                while ((abs(y[1]) > tol) && (iter < max_iter)) {
                    x <- x - y[1] / y[2]
                    y <- ftn(x)
                    iter <- iter + 1
                    cat("at iteration", iter, "value of x is", x, "\n")
                }
                if (abs(y[1]) > tol) {
                    cat("algorithm failed to converge\n")
                    return(NULL)
                } else {
                    cat("algorithm converges to \n")
                    return(x)
                }
            }

        root(ftn, x0 = 0.1, tol = 1e-09, max_iter = 100)
            # Pl = 0.1266556

    # Pu : P(X = 0) + P(X = 1) + ...+ P(X = 20) = 0.025

        # find initial value
        p <- seq(0, 1, by = 0.001)
        fp <- function(p) {
            y <- -0.025
            for (k in 0:20) {
            y <- y + choose(100, k) * p^k * (1 - p)^(100 - k)
            }
            return(y)
        }
        png(filename = "hwk6.2.png", width = 1000, height = 1000, res = 200)
        plot(fp)
        dev.off()

        # exact 95% CI
        norep <- 1000
        randomnum <- rep(NA, norep)
        for (i in 1 : norep) {
            set.seed(i)
            random <- rbinom(n = 1, size = 20, prob = 0.15)
            randomnum[i] <- random
        }
        randomnum
        upper95 <- rep(NA, norep)
        lower95 <- rep(NA, norep)
        for (i in 1 : norep) {
            r <- randomnum[i]
            ftnupper <- function(p, r) {
                y <- -0.025
                dydp <- 0

                for (k in 0:r) {
                    y <- y + choose(20, k) * (p^k) * ((1 - p)^(20 - k))
                    dydp <- dydp + choose(20, k) * ((k * p ^ (k - 1) * (1 - p) ^ (20 - k)) - (p ^ k * (20 - k) * (1 - p) ^ (19 - k))) #nolint
                }

                return(c(y, dydp))
            }
            ftnlower <- function(p, r) {
                y <- -0.025
                dydp <- 0

                for (k in r:20) {
                    y <- y + choose(20, k) * (p^k) * ((1 - p)^(20 - k))
                    dydp <- dydp + choose(20, k) * ((k * p ^ (k - 1) * (1 - p) ^ (20 - k)) - (p ^ k * (20 - k) * (1 - p)^(20 - k))) #nolint
                }
            

            return(c(y, dydp))
        }
            root <- function(ftn, x0, tol, max_iter, random) {
                x <- x0
                y <- ftn(p = x, r = random)
                iter <- 0
                while ((abs(y[1]) > tol) && (iter < max_iter)) {
                    x <- x - y[1] / y[2]
                    y <- ftn(p = x, r = random)
                    iter <- iter + 1
                }
                if (abs(y[1]) > tol) {
                    return(NA)
                } else {
                    return(x)
                }
            }
            lower95[i] <- root(ftn = ftnlower, x0 = 0.1, tol = 1e-9, max_iter = 1000, random = r)
            upper95[i] <- root(ftn = ftnupper, x0 = 0.3, tol = 1e-9, max_iter = 1000, random = r)
        }
        lower <- mean(lower95, na.rm = TRUE)
        upper <- mean(upper95, na.rm = TRUE)
        coverage <- mean((lower95 <= 0.15) & (upper95 >= 0.15), na.rm = TRUE)
        length <- mean(upper95 - lower95, na.rm = TRUE)
        (exact <- c(lower, upper, coverage, length))
        # step3 : Newton-Raphson function
        root <- function(ftn, x0, tol, max_iter) {
                x <- x0
                y <- ftn(x)
                iter <- 0
                while ((abs(y[1]) > tol) && (iter < max_iter)) {
                    x <- x - y[1] / y[2]
                    y <- ftn(x)
                    iter <- iter + 1
                    cat("at iteration", iter, "value of x is", x, "\n")
                }
                if (abs(y[1] > tol)) {
                    cat("algorithm failed to converge\n")
                    return(NULL)
                } else {
                    cat("algorithm converges to \n")
                    return(x)
                }
            }

        root(ftn, x0 = 0.3, tol = 1e-09, max_iter = 100)
            # Pu = 0.2918427

        # exact 95%CI : 0.1266556-0.2918427

# asymptotic 95% CI
    n <- 100
    phat <- (20 / n)

    pl <- phat - (qnorm(0.975) * sqrt(((phat) * (1 - phat)) / n))
    pu <- phat + (qnorm(0.975) * sqrt(((phat) * (1 - phat)) / n))
    (ci <- c(pl, pu))
        # asymptotic 95%CI : 0.1216014-0.2783986
        

randomnum[26]
randomnum[28]
library(binom)
binom.coverage(p = 0.15, n = 20, conf.level = 0.95, method = "all")
