#class5
#apply family -> calculate dating dataset

    #apply(data, dimension, function)
        x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
        x
        rowSums(x)
        colSums(x)
        apply(x, 1, sum)    #1 means dimension, means row
        apply(x, 2, sum)    #2 means dimension, means column
        apply(x, 1, max)
        apply(x, 2, min)
        apply(x, 1, function(y) {
            y[1] ^ y[2]})

        # find p-value less than 0.05
        getwd()
        setwd("/Users/raymond/Desktop/")
        data <- read.table("/Users/raymond/Desktop/21methods.txt")
        dim(data)
        head(data)
        sum(data[,1] < 0.05) / length(data[,1])
        apply(data, 2, function(x) {
            sum(x < 0.05) / length(x)
        })  #assign data 1st column as x (because dimension is 2), and calculate with funcion, and then second column... #nolint

    # tapply(data, subjuect, grouped by) <- calculating under condition
        setwd("/Users/raymond/Desktop/R/course/data")
        seizure1 <- read.csv("/Users/raymond/Desktop/R/course/data/seizure.csv")
        seizure2 <- read.table("/Users/raymond/Desktop/R/course/data/Seizure.txt")
        head(seizure1)

        tapply(seizure1$y, seizure1$trt, mean) # calculate mean of y, grouped by trt
        mean(seizure1$y[which(seizure1$trt == 0)])
        mean(seizure1$y[which(seizure1$trt == 1)])  #same as above
        by(seizure1$y, seizure1$trt, mean)  #same as above

        tapply(seizure1$y, seizure1$visit, mean, na.rm = TRUE)

    # manipulating data
        seizure1[1, 2] <- NA    # first row, second column of data will be change to NA #nolint
        by(seizure1$y, seizure1$trt, mean, na.rm = TRUE)  #remove NA

        # remove whole column
        apply(seizure2[, -1], 1, sd) # [,-1] means remove first column, and cal sd of rows #nolint
        sd(seizure2[1, -1])

    # lapply
        attributes((seizure1))
        lapply(seizure1[, 4:6], mean) # will return as list
# recursive programming
    # a1 = 1, a2 = 2 * a1, a3 = 3 * a2 -> a(n) = a(n-1) * (n)
    nfact <- function(n) {
        if (n == 1) {
            cat("called nfact(1)\n")
            return(1)
        } else {
            cat("called nfact(", n, ")\n", sep = "")
            return(n * nfact(n - 1))
        }
    }
    nfact(10)

    factorial(10)

# matrix operation
    ma <- matrix(c(1, 2, 3, 4), 2, 2, byrow = FALSE)
    ma2 <- matrix(1:4, 2, 2, byrow = TRUE)

    # determinants
    det(ma)

    # transpose matrix
    t(ma)

    # matrix multiplication
    ma %*% ma2

    # return x such that ma %*% x = ma2
    solve(ma, ma2)

    # inverse matrix of a : a(-1) %*% a = I
    I2 <- diag(1, 2) # identity matrix for 2 * 2
    solve(ma, I2)
    solve(ma) # same result

    head(seizure1)

