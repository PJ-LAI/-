#class2 programing with funciton : var, lm, glm, user difine funciton
getwd()
setwd("/Users/raymond/Desktop/R")
seizure1 <- read.csv("seizure.csv")
head(seizure1)

# basic calculation
    mean(seizure1$y)    #calculating mean of "data name" $ "the column name"
    sum(seizure1$y) / length(seizure1$y)    #same result as funciton "mean"

    #calculating variance of y
    (var_y <- var(seizure1$y))
    (var_y <- sum((seizure1$y - mean(seizure1$y))^2) / (length(seizure1$y) - 1))

    #calculating sd of y
    (sd_y <- sd(seizure1$y))
    (sd_y <- sqrt(var_y))
    (sd_y <- (var_y)^0.5)

# missing data in R
    x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
    is.na(x)    # asking R " is there missing value ?"
    mean(x) # if there's missing value, R will give "NA" as answer
    mean(x, na.rm = TRUE)   # Tell R to remove na data, and run with other data
    var(x, na.rm = TRUE)

# logical expression in R

    # true or false, TRUE = 1, FALSE = 0
    5 > 3   # ask R if 5 larger than 3, R will give TRUE
    5 != 3  # ask R if 5 not equal to 3, R will give TRUE

    # "or" and "and"
        # & = "and", true if "A and B is true"
        # | = "or", true if " A or B is true"
        # xor(A, B) = exclusive disjunction, means "A or B is TURE, but (A and B) FALSE" # nolint

        #Ex.7
        x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)

            # how many receive more than 5000?
            sum(x > 5000, na.rm = TRUE) # since true = 1, false = 0

            # is num.3 and num.8 both receive more than 5000?
            x[3] > 5000 & x[8] > 5000

            # is num.3 or num.8 receive more than 5000?
            x[3] > 5000 | x[8] > 5000

            # only one of num.3 and num.8 receive > 5000?
            xor(x[3] > 5000, x[8] > 5000)

# answer Ex.7 with function
x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
function1 <- function(money) {
    answer <- c()
        answer[1] <- sum(x > money, na.rm = TRUE)
        answer[2] <- sum(x[3] > money & x[8] > money)
        answer[3] <- sum(x[3] > money | x[8] > money)
        answer[4] <- sum(xor(x[3] > money, x[8] > money))
    return(answer)
}
money6 <- seq(1000, 20000, by = 1000)
function1(5000)

# answer Ex.7 with function and loop, cutoff = 1000:20000, by 1000
x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
function1 <- function(money) {
    answer <- c()
        answer[1] <- sum(x > money, na.rm = TRUE)
        answer[2] <- sum(x[3] > money & x[8] > money)
        answer[3] <- sum(x[3] > money | x[8] > money)
        answer[4] <- sum(xor(x[3] > money, x[8] > money))
    return(answer)
}
money_vector <- seq(1000, 20000, by = 1000)
ans_matrix <- matrix(data = NA, nrow = length(money_vector), ncol = 4)
for (i in 1 : length(money_vector)){
    ans_matrix[i, ] <- function1(money_vector[i])
}
ans_matrix[9, 1] # see the answer of 9th row, 1 column

# sampling in R
    #sample
    sample(x = seq(1, 39, by = 1), size = 5, replace = FALSE, prob = NULL)
        # sample x, with size of 5, replece means "取後放回"
    sample(x = 1:100, size = 101, replace = FALSE, prob = NULL)
        # will be error, since sample size can't be larger than population.
    sample(x = 1:100, size = 101, replace = TRUE, prob = NULL)
        # this won't be error, since a point can be draw multiple times

    # set operations
    (y <- c(sort(sample(1:20, 9)), NA)) # sort() is for "小到大排列"
    (x <- c(sort(sample(3:23, 7)), NA))
    union(x, y) # elements appear in x "or" y
    intersect(x, y) # elements appear in x "and" y
    setdiff(x, y)   # elements in x but not in y
    setdiff(y, x)   # elements in y but not in x
    setequal(x, y)  # compare x and y, FALSE if they are not equal
    setequal(union(x, y), c(setdiff(x, y), intersect(x, y), setdiff(y, x)))
    is.element(x, y) # is elements of x also elements in y
    x %in% y # total equivelent to is.element(x, y)

# user-diffined functions
    # demostrate cental limit theorum
    par(mfrow = c(3, 2))
    clt <- function(n) {
            N <- 10000
            score <- rnorm(N, 75, 5)
            hist(score, xlim = c(50, 100), breaks = seq(50, 100, by = 1))
            mean.score <- c()
            for (i in 1:1000) {
                mean.score[i] <- mean(score[sample(1:N, size = n)])
            }
            hist(mean.score, xlim = c(50, 100), breaks = seq(50, 100, by = 1))
            return(c(mean(mean.score), var(mean.score)))
    }
clt(5)
clt(20)
clt(10000)