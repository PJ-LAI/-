# class 7 : constructing a log-likelihood funciton
    # ber ~ ( 20 students in class, pass exam = p, observed 3 student pass)
    # likelihood funciton L(p)
        # first way
        p <- seq(0, 1, 0.001)
        fp <- (p ^ 3) * ((1 - p) ^ 17)
        plot(p, fp, ylab = "likelihood")
        abline(v = 3 / 20)  # plot a line

        # second way
        lik <- function(x, p) {
            likelihood <- 1
            for (i in length(x)) {
                if (x[i] == 1) {
                    likelihood <- likelihood * p
                }else {
                    likelihood <- likelihood * (1 - p)
                }
            }
            return(likelihood)
        }
        x <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0)
        fp2 <- lik(x, p)
        lines(p, fp2, col = 2)

        # find the root of 3 * (1 - p) - 17 * (1 - (1 - p))
        p <- seq(0, 1, 0.001)
        fp <- (3 / p) - (17 / (1 - p))
        plot(p, fp)

        ftn <- function(p) {
            fp <- (3 / p) - (17 / (1 - p))
            dfp <- (-3) * (p ^ (-2)) - 17 * ((1 - p) ^ (-2))
            return(c(fp, dfp))
        }

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

        root(ftn, x0 = 0.2, tol = 1e-09, max_iter = 100)
    
    # ~pois
    # seizure counts of 10 pa in 2 months as follow
    # c(3, 1, 2, 0, 6, 8, 4, 5, 7, 0)#nolint

        # q1 plot the likilihood function form mean seizure counts lambda
        like <- function(x, lambda) {
            ans <- 1
            for (i in 1 : length(x)) {
                ans <- ans * ((exp((-1) * lambda) * (lambda ^ x[i])) / factorial(x[i])) #nolint
            }
            return(ans)
        }
        x <- c(3, 1, 2, 0, 6, 8, 4, 5, 7, 0)
        lambda <- seq(0, 10, by = 0.01)
        fpois <- like(x, lambda)
        plot(lambda, fpois, col = 1)

        # q2 plot the log-likelihood funciton for mean seizure count lambda

        loglike <- function(x, lambda) {
            ans <- 0
            for (i in 1 : length(x)) {
                ans <- ans + log(((exp((-1) * lambda) * (lambda ^ x[i])) / factorial(x[i]))) #nolint
            }
            return(ans)
        }
        x <- c(3, 1, 2, 0, 6, 8, 4, 5, 7, 0)
        lambda <- seq(0, 10, by = 0.01)
        logfpois <- loglike(x, lambda)
        plot(lambda, logfpois, col = 1, type = "l")

        # q3 solve maximum likelihood estimate by hand
        lambdahat <- sum(x) / length(x)
        lambdahat # = mean(x)
        mean(x)
        
        # q4 solve maximum likelihood estimate by newton
        ftn <- function(lambda) {
            x <- c(3, 1, 2, 0, 6, 8, 4, 5, 7, 0)
            f <- (-length(x)) + sum(x) / lambda
            df <- -(lambda ^ (-2)) * sum(x)
            return(c(f, df))
        }
        x <- c(3, 1, 2, 0, 6, 8, 4, 5, 7, 0)
        f <- (-length(x)) + sum(x) / lambda
        lambda <- seq(0, 10, by = 0.01)
        plot(lambda, f, col = 3, type = "l")
        abline(h = 0, col = 4)
        abline(v = 0, col = 6)

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

        root(ftn, x0 = 2, tol = 1e-09, max_iter = 100) 

    # ~ norm

    normpdf <- (1 / sqrt(2 * pi * sigma)) * (exp(((x - mu) ^ 2) / 2 * (sigma ^ 2))) #nolint
    likenorm <- function(mu, sigma) {
        l <- 1
        for (i in 1 : length(x)) {
            l <- l * (1 / sqrt(2 * pi * sigma)) * (exp(((x[i] - mu) ^ 2) / 2 * (sigma ^ 2))) #nolint
        }
        return(l)
    }

    #loglikenorm
    loglikenorm1 <- function(mu, sigma) {
        logl <- 0
        for (i in 1 : length(x)) {
            logl <- logl + log((1 / sqrt(2 * pi * sigma)) * (exp(((x[i] - mu) ^ 2) / 2 * (sigma ^ 2)))) #nolint
        }
        return(logl)
    }

    llnorm <- function(data, param) {
        mu <- param[1]
        sigma <- param[2]
        return(sum(dnorm(data, mean = mu, sd = sigma, log = TRUE)))
    }
    x1 <- c(57,65,53,70,78,56,85,92,66,78,74,82,89,96,66,68,72,75,79,91,84,85,81,58,74,70,62,56,84,65) #nolint
    mu <- seq(min(x1), max(x1), by = 0.1)
    sigma <- seq((max(x1) - min(x1)) / 10, (max(x1) - min(x1)) / 2, by = 0.1)
    llnormma <- matrix(NA, length(mu) * length(sigma), 3)
    length(mu)
    length(sigma)

    cc <- 1
    for (i in 1 :length(mu)) {
        for (j in 1 : length(sigma)) {
            llnormma[cc, ] <- c(mu[i], sigma[j], llnorm(data = x1, param = c(mu[i], sigma[j]))) #nolint
            cc <- cc + 1
        }
    }
    dim(llnormma)
    head(llnormma)

    install.packages("scatterplot3d")
    library(scatterplot3d)
    scatterplot3d(llnormma[, 1], llnormma[, 2], llnormma[, 3], xlab = "mu", ylab = "sigma", zlab = "log likelihood", highlight.3d = TRUE) #nolint


    #ex19 If you observed 10 young women with BMI: 21.5,16,19,20,21.5,18.6,19.3,21.1,23.5,21.3
    #please plot the log Likelihood by assuming the BMI follows a normal distribution.

    llnorm <- function(data, param) {
        mu <- param[1]
        sigma <- param[2]
        return(sum(dnorm(data, mean = mu, sd = sigma, log = TRUE)))
    }

    bmi <- c(21.5, 16, 19, 20, 21.5, 18.6, 19.3, 21.1, 23.5, 21.3)
    mu <- seq(round(mean(bmi)) - 5, round(mean(bmi)) + 5, by = 0.01)
    sigma <- seq(round(sd(bmi)) - 1, round(sd(bmi)) + 1, by = 0.01)
    llnormma <- matrix(NA, length(mu) * length(sigma), 3)
    length(mu)
    length(sigma)

    cc <- 1
    for (i in 1 :length(mu)) {
        for (j in 1 : length(sigma)) {
            llnormma[cc, ] <- c(mu[i], sigma[j], llnorm(data = bmi, param = c(mu[i], sigma[j]))) #nolint
            cc <- cc + 1
        }
    }
    dim(llnormma)
    head(llnormma)
    scatterplot3d(llnormma[, 1], llnormma[, 2], llnormma[, 3], xlab = "mu", ylab = "sigma", zlab = "log likelihood", highlight.3d = TRUE) #nolint


    dlogdmu <- (1 / (sigma ^ 2)) * (sum(x) - (length(x) * mu))
    dlogdsigma <- ((-length(x) / 2) * (1 / (sigma ^ 2))) - ((1 / 2) * ((sum(x) ^ 2) - (2 * sum(x) * mu) + (length(x) * (mu ^ 2))))#nolint


# likelihood of a general linear model
setwd("/Users/raymond/Desktop/R/course/data/")
data <- read.csv("/Users/raymond/Desktop/R/course/data/seizure.csv")
dim(data)
head(data)

    #
    x <- cbind(rep(1, length(data$y)), data$trt, data$age, data$ltime)
    head(x)
    beta <- solve(t(x) %*% x) %*% t(x) %*% matrix(data$y, ncol = 1)
    residual <- data$y - x %*% beta
    varhat <- sum(residual ^ 2) / length(data$y)
    lldata <- (-length(data$y) / 2) * (log(2 * pi) + log(varhat) + 1)
    lldata

    model1 <- lm(y ~ trt + age + ltime, data = data)
    logLik(model1)
