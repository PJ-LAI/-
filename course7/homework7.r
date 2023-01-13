# HW7
# Ex 18_1
    # plot log-likelihood of for mean of a Poisson dis.
        llpois <- function(data, param) {
            ans <- 0
            param <- lambda
            for (i in 1 : length(data)) {
                ans <- ans + log(((exp((-1) * lambda) * (lambda ^ data[i])) / factorial(data[i]))) #nolint
            }
            return(ans)
        }

        x1 <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)
        lambda <- seq(0, 10, by = 0.01)
        llpois1 <- llpois(data = x1, lambda)

        png(filename = "hwk7.1.png", width = 1000, height = 1000, res = 200)
        plot(lambda, llpois1, col = 1, type = "l", lwd = 3)
        dev.off()

    # maximum likelihood estimate of lambda by Newton-Raphson
        # finding suitable x0
        x <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)
        f <- (-length(x)) + (sum(x) / lambda)
        lambda <- seq(0, 10, by = 0.01)

        png(filename = "hwk7.2.png", width = 1000, height = 1000, res = 200)
        plot(lambda, f, col = 1, type = "l")
        abline(h = 0, col = 2)
        dev.off()

        # constructing ftn
        ftn <- function(lambda) {
            x <- c(5, 5, 6, 0, 1, 2, 4, 4, 3, 5, 7, 10)
            f <- (-length(x)) + (sum(x) / lambda)
            df <- -(lambda ^ (-2)) * sum(x)
            return(c(f, df))
        }

        # Newton-Raphson method
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

        root(ftn, x0 = 3, tol = 1e-09, max_iter = 100)

# EX S5_1 BMI3 ~ SEX + AGE + Treatment
setwd("/Users/raymond/Desktop/R/course/data/")
data <- read.csv("/Users/raymond/Desktop/R/course/data/BMIrepeated.csv")
dim(data)
head(data)
mode(data$SEX) # data$SEX is character, so i have to convert it to numeric

    # convert sex to numeric, setting "M" be 1, "F" be 0
    datama <- as.data.frame(data)
    for (i in 1 : length(data$SEX)) {
        if (datama[i, 2] == "M") {
            datama[i, 2] <- 1
        } else {
            datama[i, 2] <- 0
        }
    }
    head(datama)
    (sex <- as.numeric(datama[, 2]))

    # design matrix
    x <- cbind(rep(1, length(data$BMI3)), sex, data$AGE, data$Treatment)
    head(x)

    # regression coefficients
    (beta <- solve(t(x) %*% x) %*% t(x) %*% matrix(data$BMI3, ncol = 1))

    # residual
    (residual <- data$BMI3 - x %*% beta)

    # sigma sqrare
    (sigmasqr <- sum(residual ^ 2) / length(data$BMI3))

    # log likelihood
    (lldata <- (-length(data$BMI3) / 2) * (log(2 * pi) + log(sigmasqr) + 1))

    model1 <- lm(BMI3 ~ SEX + AGE + Treatment, data = datama)
    logLik(model1)

    test <- model.matrix(data$BMI3 ~ data$SEX + data$AGE + data$Treatment)
    head(test)
    show(test)
    (regression <- lm(formula = BMI3 ~ SEX + AGE + Treatment, data = data))
    show(regression)
    var <- sigma(regression) ^ 2
    var
