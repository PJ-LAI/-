# class10
    # pseudo-random number
    set.seed(7)
    rnorm(10)
    set.seed(7)
    rnorm(10)
    rnorm(10)
    rnorm(10, 7, 2)
    rchisq(10, df = 3)
    rbinom(10, size = 10, p = 0.5)

    # confidence interval : mu = 7, sd = 2 n = 100, stimulation 1000 times
        # population sd is known --> z-test
        stimnorm <- function(n, mu, sigma, norep) {
            lower95 <- rep(NA, norep)
            upper95 <- rep(NA, norep)
            lower99 <- rep(NA, norep)
            upper99 <- rep(NA, norep)

            for (i in 1 : norep) {
                set.seed(i)
                x <- rnorm(n = n, mean = mu, sd = sigma)
                lower95[i] <- mean(x) - qnorm(0.975) * sqrt(sigma ^ 2 / n)
                upper95[i] <- mean(x) + qnorm(0.975) * sqrt(sigma ^ 2 / n)
                lower99[i] <- mean(x) - qnorm(0.995) * sqrt(sigma ^ 2 / n)
                upper99[i] <- mean(x) + qnorm(0.995) * sqrt(sigma ^ 2 / n)
            }
            return(cbind(lower95, upper95, lower99, upper99))
        }

        stimnorm1 <- function(n, mu, sigma, norep) {
            lower95 <- rep(NA, norep)
            upper95 <- rep(NA, norep)
            lower99 <- rep(NA, norep)
            upper99 <- rep(NA, norep)

            for (i in 1 : norep) {
                set.seed(i)
                x <- rnorm(n = n, mean = mu, sd = sigma)
                lower95[i] <- mean(x) - qnorm(0.975) * sqrt(sigma ^ 2 / n)
                upper95[i] <- mean(x) + qnorm(0.975) * sqrt(sigma ^ 2 / n)
                lower99[i] <- mean(x) - qnorm(0.995) * sqrt(sigma ^ 2 / n)
                upper99[i] <- mean(x) + qnorm(0.995) * sqrt(sigma ^ 2 / n)
            }
            return(c(mean((lower95 <= mu) & (upper95 >= mu)), mean((lower99 <= mu) & (upper99 >= mu)), mean(upper95 - lower95), mean(upper99 - lower99))) #nolint
        }
        stimnorm(n = 100, mu = 7, sigma = 2, norep = 1000)

        #populaiton sd unknown --> t-test

        stimtt <- function(n, mu, sigma, norep) {
            lower95 <- rep(NA, norep)
            upper95 <- rep(NA, norep)
            lower99 <- rep(NA, norep)
            upper99 <- rep(NA, norep)

            for (i in 1 : norep) {
                set.seed(i)
                x <- rnorm(n = n, mean = mu, sd = sigma)
                lower95[i] <- mean(x) - qt(0.975, n - 1) * sqrt(var(x) / n)
                upper95[i] <- mean(x) + qt(0.975, n - 1) * sqrt(var(x) / n)
                lower99[i] <- mean(x) - qt(0.995, n - 1) * sqrt(var(x) / n)
                upper99[i] <- mean(x) + qt(0.995, n - 1) * sqrt(var(x) / n)
            }
            return(cbind(lower95, upper95, lower99, upper99))
        }
        stimtt(n = 100, mu = 7, sigma = 2, norep = 1000)

        stimtt1 <- function(n, mu, sigma, norep) {
            lower95 <- rep(NA, norep)
            upper95 <- rep(NA, norep)
            lower99 <- rep(NA, norep)
            upper99 <- rep(NA, norep)

            for (i in 1 : norep) {
                set.seed(i)
                x <- rnorm(n = n, mean = mu, sd = sigma)
                lower95[i] <- mean(x) - qt(0.975, n - 1) * sqrt(var(x) / n)
                upper95[i] <- mean(x) + qt(0.975, n - 1) * sqrt(var(x) / n)
                lower99[i] <- mean(x) - qt(0.995, n - 1) * sqrt(var(x) / n)
                upper99[i] <- mean(x) + qt(0.995, n - 1) * sqrt(var(x) / n)
            }
            return(c(mean((lower95 <= mu) & (upper95 >= mu)), mean((lower99 <= mu) & (upper99 >= mu)), mean(upper95 - lower95), mean(upper99 - lower99))) #nolint
        }
        stimtt1(n = 100, mu = 7, sigma = 2, norep = 1000)
        # CI would be wider in t-test than in z-test, because more uncertainty

    # exercise
        q1 <- function(n, mu, sigma, norep) {
            coverage <- matrix(NA, length(n), 2)
            for (j in 1 : length(n)){
                lower95 <- rep(NA, norep)
                upper95 <- rep(NA, norep)
                lower99 <- rep(NA, norep)
                upper99 <- rep(NA, norep)

                for (i in 1 : norep) {
                    set.seed(i)
                    x <- rnorm(n = n[j], mean = mu, sd = sigma)
                    lower95[i] <- mean(x) - qnorm(0.975) * sqrt(var(x) / n[j])
                    upper95[i] <- mean(x) + qnorm(0.975) * sqrt(var(x) / n[j])
                    lower99[i] <- mean(x) - qnorm(0.995) * sqrt(var(x) / n[j])
                    upper99[i] <- mean(x) + qnorm(0.995) * sqrt(var(x) / n[j])
                }

                coverage[j, 1] <- mean((lower95 <= mu) & (upper95 >= mu))
                coverage[j, 2] <- mean((lower99 <= mu) & (upper99 >= mu))
            }
            return(coverage)
        }
        y <- q1(n = seq(5, 100, 5), mu = 7, sigma = 2, norep = 1000)

        matplot(x = seq(5, 100, by = 5), y = y, type = "l", lty = c(1, 2), col = c(1, 2), lwd = 2)
        abline(h = 0.95)
        abline(h = 0.99)

# homework 10
    # asymptotic 95% CI
    stimbinom <- function(n, size, p, norep) {
            lower95 <- rep(NA, norep)
            upper95 <- rep(NA, norep)

            for (i in 1 : norep) {
                set.seed(i)
                x <- rbinom(n = 1, size = 20, prob = p)
                lower95[i] <- (mean(x) / size) - qnorm(0.975) * sqrt((mean(x) / size) * (1 - (mean(x) / size)) / n)
                upper95[i] <- (mean(x) / size) + qnorm(0.975) * sqrt((mean(x) / size) * (1 - (mean(x) / size)) / n)
            }

            for (i in 1 : norep) {
                if (lower95[i] < 0) {
                    lower95[i] <- 0
                }
            }
            return(cbind(lower95, upper95))
            #return(c(mean((lower95 <= p) & (upper95 >= p)), mean(upper95 - lower95))) 
        }
        stimbinom(n = 1, size = 20, p = 0.15, norep = 1000)




# asympotic 95% CI

        asympbinom <- function(n, size, prob, norep) {
            lower95 <- rep(NA, norep)
            upper95 <- rep(NA, norep)

            for (i in 1 : norep) {
                set.seed(i)
                x <- rbinom(n = n, size = size, prob = prob)
                phat <- (x / size)
                lower95[i] <- phat - (qnorm(0.975) * sqrt(((phat) * (1 - phat)) * (1 / n)))
                upper95[i] <- phat + (qnorm(0.975) * sqrt(((phat) * (1 - phat)) / (1 / n)))
            }
            for (i in 1 : norep) {
                if (lower95[i] <= 0) {
                    lower95[i] <- 0
                }
            }
            lower <- mean(lower95)
            upper <- mean(upper95)
            coverage <- mean((lower95 <= 0.15) & (upper95 >= 0.15))
            length <- mean(upper95 - lower95)

            return(c(lower, upper, coverage, length))
        }

        asympbinom(n = 1, size = 20, prob = 0.15, norep = 1000)
        library(binom)
        binom.coverage(p = 0.15, n = 20, conf.level = 0.95, method = "all")
# exact 95% CI
    

            n <- 1
            size <- 20
            prob <- 0.15
            norep <- 1000
            randomnum <- rep(NA, norep)

            for (i in 1 : norep) {
                set.seed(i)
                numb <- rbinom(n = n, size = size, prob = prob)
                randomnum[i] <- numb
            }

            lower95 <- rep(NA, norep)
            upper95 <- rep(NA, norep)
   
    # lower
            for (i in 1 : norep) {
                r <- randomnum[i]
                rootlower <- function(x0, tol, max_iter, random) {
                    x <- x0
                    z0 <- random

                    ftn <- function(p, z) {
                        y <- -0.975
                        dydp <- 0
                        z1 <- z
                        z2 <- 0
                        if (z1 == 0) {
                            z2 <- 0
                        } else {
                            z2 <- z1
                        }

                        for (k in 0:z2) {
                            y <- y + choose(20, k) * (p^k) * ((1 - p)^(20 - k))

                            dydp <- dydp + choose(20, k) * ((k * p^(k - 1) * (1 - p)^(20 - k))  # nolint
                            - (p^k * (20 - k) * (1 - p)^(19 - k)))
                        }

                        return(c(y, dydp))
                    }

                    y <- ftn(p = x, z = z0)
                    iter <- 0

                    while ((abs(y[1]) > tol) && (iter < max_iter)) {
                        x <- x - y[1] / y[2]
                        y <- ftn(x, random)
                        iter <- iter + 1
                    }
                    if (abs(y[1]) > tol) {
                        return(x)
                    } else {
                        return(x)
                    }

                }

                lower95[i] <- rootlower(x0 = 0.15, tol = 1e-9, max_iter = 100, random = r)
            }
    #upper
            for (i in 1 : norep) {
                r <- randomnum[i]
                rootupper <- function(x0, tol, max_iter, random) {
                    x <- x0
                    z0 <- random

                    ftn <- function(p, z) {
                        y <- -0.025
                        dydp <- 0
                        z1 <- z

                        for (k in 0:z1) {
                            y <- y + choose(20, k) * (p^k) * ((1 - p)^(20 - k))

                            dydp <- dydp + choose(20, k) * ((k * p^(k - 1) * (1 - p)^(20 - k))  # nolint
                            - (p^k * (20 - k) * (1 - p)^(19 - k)))
                        }

                        return(c(y, dydp))
                    }

                    y <- ftn(p = x, z = z0)
                    iter <- 0

                    while ((abs(y[1]) > tol) && (iter < max_iter)) {
                        x <- x - y[1] / y[2]
                        y <- ftn(x, random)
                        iter <- iter + 1
                    }
                    if (abs(y[1]) > tol) {
                        return(x)
                    } else {
                        return(x)
                    }

                }

                upper95[i] <- rootupper(x0 = 0.15, tol = 1e-9, max_iter = 100, random = r)
            }
            lower <- mean(lower95)
            upper <- mean(upper95)
            coverage <- mean((lower95 <= prob) & (upper95 >= prob))
            length <- mean(upper95 - lower95)
            (exact <- c(lower, upper, coverage, length))
