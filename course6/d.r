# class6
# Newton-Raphson method : find roots of function
    # f'(x0) = dy / dx = f(x0) - 0 / x0 - x1
    # -> x1 = x0 - (f(x0) / f'(x0))
    # -> xn = x(n-1) - (f(x(n-1)) / f'(x(n-1)))
    # two requirment : f(x) should be differentiatable, and f'(xn) != 0

    # find root for log(x) - exp(-x) = 0

    x <- seq(-10, 10, by = 0.01)
    y <- log(x) - exp(-x)
    plot(x, y)

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

    ftn <- function(x) {
        fx <- log(x) - exp(-x)
        dfx <- (1 / x) + exp(-x)
        return(c(fx, dfx))
    }
    root(ftn, 2, 1e-06, 100)

    # find root of x^2 - 2 = 0
    x <- seq(-2, 2, by = 0.01)
    y <- x^2 - 2
    plot(x, y)

    ftn <- function(x) {
        fx <- x^2 - 2
        dfx <- 2 * x
        return(c(fx, dfx))
    }

    root(ftn, 2, 1e-09, 100)
    root(ftn, -2, 1e-09, 100)

    # find root of x^2 - x - 12

    x <- seq(-10, 10, by = 0.01)
    y <- x^2 - x - 12
    plot(x, y)

    ftn <- function(x) {
        fx <- x^2 - x - 12
        dfx <- 2 * x - 1
        return(c(fx, dfx))
    }

    root(ftn, 5, 1e-09, 100)
    root(ftn, -5, 1e-09, 100)

    # class with 20 students, if pass exam prob is p, and we observe 3 student pass exam #nolint
    # find 95 CI of p == [PL, PU] covers 95% prob
    # PL : P(X = 3) + ...P(X = 20) = 0.025
    # PU : P(X = 0) + P(X = 1) + ...PU(X = 3) = 0.025

    # PL : sigma(k = 0 to 2) : choose(20, k) * (p)^k * (1-p)^20-k - 0.975 = 0
    p <- seq(0, 1, by = 0.001)
    fp <- function(p) {
        y <- -0.975
        for (k in 0:2) {
        y <- y + choose(20, k) * p^k * (1 - p)^(20 - k)
        }
        return(y)
    }

    ftn <- function(p) {
        y <- -0.975
        dydp <- 0
        for (k in 0:2) {
        y <- y + choose(20, k) * (p^k) * ((1 - p)^(20 - k))

        dydp <- dydp + choose(20, k) * ((k * p^(k - 1) * (1 - p)^(20 - k)) 
        - (p^k * (20 - k) * (1 - p)^(19 - k)))
        }
        return(c(y, dydp))
    }

    root(ftn, 0.1, 1e-09, 100)

    # PH : sigma(k = 0 to 2) : choose(20, k) * (p)^k * (1-p)^20-k - 0.025 = 0
    # find inital number x0
    p <- seq(0, 1, by = 0.001)
    fp <- function(p) {
        y <- -0.025
        for (k in 0:3) {
        y <- y + choose(20, k) * p^k * (1 - p)^(20 - k)
        }
        return(y)
    }
    plot(fp)

    #find,f(p), f'(p)
    ftn <- function(p) {
        y <- -0.025
        dydp <- 0
        for (k in 0:3) {
        y <- y + choose(20, k) * (p^k) * ((1 - p)^(20 - k))

        dydp <- dydp + choose(20, k) * ((k * p^(k - 1) * (1 - p)^(20 - k)) 
        - (p^k * (20 - k) * (1 - p)^(19 - k)))
        }
        return(c(y, dydp))
    }

    root(ftn, 0.3, 1e-09, 100)

    install.packages("binom")
    library(binom)
    binom.confint(20, 100, conf.level = 0.95, methods = "all")
