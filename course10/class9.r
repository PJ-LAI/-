# class 8 : MLE for poisson regression
setwd("/Users/raymond/Desktop/R/course/data/")
data <- read.csv("/Users/raymond/Desktop/R/course/data/seizure.csv")
head(data)
model <- glm(y ~ trt + age, data = data, offset = ltime, family = poisson())

# use NR method to find MLE of the coefficeints of Poison regression
    y <- data$y
    x <- cbind(rep(1, length(y)), data$trt, data$age)
    ftn <- function(betacoef) {
        mu <- exp(x %*% betacoef + data$ltime)
        gradient <- t(x) %*% (y - mu)
        hessian <- -t(x) %*% diag(c(mu), length(y)) %*% x
        loglike <- sum(-mu + y * log(mu) - log(factorial(y)))

        return(list(gradient, hessian, loglike))
    }

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


    (beta <- highnew(ftn, x0 = c(0, 0, 0), tol = 1e-9, maxiter = 100))

    #variance covariance matrix
    solve(-ftn(beta)[[2]])
    vcov(model)

    #loglike
    (loglike <- ftn(beta)[[3]])
    logLik(model)

# rate
rate <- read.csv("/Users/raymond/Desktop/R/course/data/rate.csv", header = TRUE)
rate
(model <- glm(Death ~ Age + sex, data = rate, offset = log(PY / 100000), family = poisson))

        # use NR method to find MLE of the coefficeints of Poison regression
        y <- rate$Death
        x <- cbind(rep(1, length(y)), rate$Age, ifelse(rate$sex == "m", 1, 0))
        ftn <- function(betacoef) {
            mu <- exp(x %*% betacoef + log(rate$PY/100000))
            gradient <- t(x) %*% (y - mu)
            hessian <- -t(x) %*% diag(c(mu), length(y)) %*% x
            loglike <- sum(-mu + y * log(mu) - log(factorial(y)))

            return(list(gradient, hessian, loglike))
        }

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

        (beta <- highnew(ftn, x0 = c(0, 0, 0), tol = 1e-9, maxiter = 100))
        model
        #variance covariance matrix
        solve(-ftn(beta)[[2]])
        vcov(model)

        #loglike
        (loglike <- ftn(beta)[[3]])
        logLik(model)

        # not linear -> revise model, age as dummy variable
        rate$Age.f <- factor(rate$Age)
        glm(Death ~ Age.f  + sex, data = rate, offset = log(PY / 100000), family = poisson)
# homework9
rate <- read.csv("/Users/raymond/Desktop/R/course/data/rate.csv", header = TRUE)
rate

# creating data with dummy variables
(ratema <- as.data.frame(rate))
for (j in 2 : 12) {
    for (i in 1 : length(rate$Age)) {
        if (ratema[i, 1] == j) {
            ratema[i, j + 3] <- 1
        } else {
            ratema[i, j + 3] <- 0
        }
    }
}
ratema
write.csv(ratema, file = "/Users/raymond/Desktop/R/course/ratema.csv")
# use NR method to find MLE of the coefficeints of Poison regression
y <- rate$Death
x <- cbind(rep(1, length(y)),
            ratema$V5,
            ratema$V6,
            ratema$V7,
            ratema$V8,
            ratema$V9,
            ratema$V10,
            ratema$V11,
            ratema$V12,
            ratema$V13,
            ratema$V14,
            ratema$V15,
            ifelse(ratema$sex == "m", 1, 0))

ftn <- function(betacoef) {
    mu <- exp(x %*% betacoef + log(ratema$PY / 100000))
    gradient <- t(x) %*% (y - mu)
    hessian <- -t(x) %*% diag(c(mu), length(y)) %*% x
    loglike <- sum(-mu + y * log(mu) - log(factorial(y)))

    return(list(gradient, hessian, loglike))
}

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

(beta <- highnew(ftn, x0 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tol = 1e-9, maxiter = 100))#nolint
write.csv(beta, file = "/Users/raymond/Desktop/R/course/beta.csv")
#variance covariance matrix
(vcov <- solve(-ftn(beta)[[2]]))
vcov(model)
write.csv(vcov, file = "/Users/raymond/Desktop/R/course/vcov.csv")
#loglike
(loglike <- ftn(beta)[[3]])
write.csv(loglike, file = "/Users/raymond/Desktop/R/course/loglike.csv")
