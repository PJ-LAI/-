 # class 8
    # high dimensioal Newton-Raphson
        # grandiant of f is a 1 by d matrix
        # Hessian matrix is d by d matrix

    highnew <- function(ftn, x0, tol, maxiter) {
        x <- x0
        fx <- ftn(x)
        iter <- 0

        while ((max(abs(fx[[1]])) > tol) && (iter < maxiter)) {
            x <- x - (solve(fx[[2]]) %*% fx[[1]])
            fx <- ftn
            iter <- iter + 1
        }

        if (max(abs(fx[[1]])) > tol) {
            cat("algorithm failed to converge\n")
            return(NULL)
        } else {
            cat("algorithm converges to\n")
            return(x)
        }
    }

    # EX 20
    setwd("/Users/raymond/Desktop/")
    admit <- read.table("/Users/raymond/Desktop/admit.txt", header = TRUE)
    head(admit)
    dim(admit)
    tt <- glm(admit ~ gpa + gre, family = binomial, data = admit)
    summary(tt)

        # use NR to find maximum likelihood estimate of beta
        X <- cbind(rep(1, length(admit$admit)), admit$gpa, admit$gre) # nolint
        dim(X)
        Y <- admit$admit # nolint
        ftn <- function(betacoef) {
            pi1 <- exp(X %*% betacoef) / (1 + exp(X %*% betacoef))
            gradient <- t(X) %*% (Y - pi1)
            hessian <- - t(X) %*% diag(c(pi1 * (1 - pi1)), length(admit$admit)) %*% X # nolint

            return(list(gradient, hessian))
        }

        ftn(c(0, 0, 0))

        highnew <- function(ftn, x0, tol = 1e-9, maxiter = 100) {
            x <- x0
            fx <- ftn(x)
            iter <- 0

            while ((max(abs(fx[[1]])) > tol) && (iter < maxiter)) {
                x <- x - (solve(fx[[2]]) %*% fx[[1]])
                fx <- ftn(x)
                iter <- iter + 1
            }

            if (max(abs(fx[[1]])) > tol) {
                cat("algorithm failed to converge\n")
                return(NULL)
            } else {
                cat("algorithm converges to\n")
                return(x)
            }
        }
        highnew(ftn, x0 = c(0, 0, 0))
        model <- glm(admit ~ gpa + gre, family = binomial, data = admit)

        # variance-covariance matrix
        beta <- highnew(ftn, c(0, 0, 0))
        beta

        solve(-ftn(beta)[[2]])
        vcov(model)

        # log likelihood function
        X <- cbind(rep(1, length(admit$admit)), admit$gpa, admit$gre) # nolint
        Y <- admit$admit # nolint
        ftn <- function(betacoef) {
            pi1 <- exp(X %*% betacoef) / (1 + exp(X %*% betacoef))
            gradient <- t(X) %*% (Y - pi1)
            hessian <- - t(X) %*% diag(c(pi1 * (1 - pi1)), length(admit$admit)) %*% X # nolint
            loglike <- sum(Y * log(pi1 / (1 - pi1)) + log(1 - pi1))

            return(list(gradient, hessian, loglike))
        }

        ftn(beta)[[3]]

        logLik(model)

    # EX 20-1
    setwd("/Users/raymond/Desktop/")
    resp <- read.csv("/Users/raymond/Desktop/resp.csv", header = TRUE)
    head(resp)
    dim(resp)

    # Q1 : use NR to find maximum likelihood estimate of beta

        # convert treatment to numeric, setting "P" be 1, "A" be 0
            respma <- as.data.frame(resp)
            for (i in 1 : length(resp$treatment)) {
                if (respma[i, 3] == "P") {
                    respma[i, 3] <- 1
                } else {
                    respma[i, 3] <- 0
                }
            }
            head(respma)
            (treatment <- as.numeric(respma[, 3]))

       # constructing matrix
            X <- cbind(rep(1, length(resp$outcome)), treatment, resp$age, resp$baseline) # nolint
            Y <- resp$outcome # nolint

        # ftn
            ftn <- function(betacoef) {
                pi1 <- exp(X %*% betacoef) / (1 + exp(X %*% betacoef))
                gradient <- t(X) %*% (Y - pi1)
                hessian <- - t(X) %*% diag(c(pi1 * (1 - pi1)), length(resp$outcome)) %*% X # nolint

                return(list(gradient, hessian))
            }
ftn(c(0, 0, 0, 0))
        # highorder Newton-Raphson method
        highnew <- function(ftn, x0, tol, maxiter) {
            x <- x0
            fx <- ftn(x)
            iter <- 0

            while ((max(abs(fx[[1]])) > tol) && (iter < maxiter)) {
                x <- x - (solve(fx[[2]]) %*% fx[[1]])
                fx <- ftn(x)
                iter <- iter + 1
            }

            if (max(abs(fx[[1]])) > tol) {
                cat("algorithm failed to converge\n")
                return(NULL)
            } else {
                cat("algorithm converges to\n")
                return(x)
            }
        }
        highnew(ftn, x0 = c(0, 0, 0, 0), tol = 1e-9, maxiter = 100)
        (model <- glm(outcome ~ treatment + age + baseline, family = binomial, data = resp))

    # Q2 : variance-covariance matrix
     (beta <- highnew(ftn, c(0, 0, 0, 0), tol = 1e-9, maxiter = 100))
     solve(-ftn(beta)[[2]])
     vcov(model)

    # Q3 : log likelihood function
    X <- cbind(rep(1, length(resp$outcome)), treatment, resp$age, resp$baseline) # nolint
    Y <- resp$outcome # nolint
    ftn <- function(betacoef) {
        pi1 <- exp(X %*% betacoef) / (1 + exp(X %*% betacoef))
        gradient <- t(X) %*% (Y - pi1)
        hessian <- - t(X) %*% diag(c(pi1 * (1 - pi1)), length(resp$outcome)) %*% X # nolint
        loglike <- sum(Y * log(pi1 / (1 - pi1)) + log(1 - pi1))

        return(list(gradient, hessian, loglike))
    }

    ftn(beta)[[3]]

    logLik(model)
