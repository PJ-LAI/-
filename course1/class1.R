#basics
    #loop
    pp <- 0
    for (i in 1:100){
        pp <- pp + i
    }
    pp                 # = sum(1:100)

    #working directory
    getwd()            #showing current working directory
    setwd("Users/raymond/Desktop/R") #setting working directory

    #help
    help(mean)         #getting help, code = ?

    #calculating
    (1 + 1 / 100)^100      # ^ being the power, or **
    15 %% 5                # %% to show remainder
    15 %/% 5               # %/% to show quotion
    exp(1)                 # exp() to show explonatial
    options(digit = 2)     # changing the digit showed
    floor(pi)              # finding integin lower than pi
    ceiling(pi)            # finding integin higher than pi
    round(3.1)             # rounding this number to integin

    # object
    (x <- seq(from = 1, to = 100, by = 20)) # abbreviated seq(1, 100, 20)
    length(x)
    x <- c(1, 2, 3)          # c means combine, that x is assigned with 3 num
    y <- c(2, 4, 6)
    (z <- x * y)
    (z <- y ^ x)



#normal distribution
    #making a normal distribution plot：dnorm()
x <- seq(-3, 3, 0.01)   #seq(first number, last number, by what space)
length(x)               # how many number in sequence x, should be 600+1
y <- dnorm(x, mean = 0, sd = 1, log = FALSE) # let y = normal distribution of x
plot(x, y)              # draw the graph of x versus y
y [1]                   # meaning the y value of the first element, which is -3
dnorm(x = -3, mean = 0, sd = 1, log = FALSE) # this line is the meaning of line5
dnorm(-3)               #ok to type this only, since the default is normal distr

    #cummulative probability of normal ditribution : pnorm()
pnorm(0)                #cumulative prob form negative infinate to 0, =0.5
1 - pnorm(1.96)         #cumulative prob from 1.96 to infinate,=right tail prob
pnorm(1.96, lower = FALSE) #same as upper line, meaning left tail= false,=right

    #quantile function = inverse function of pnorm : qnorm
qnorm(0.025)            #meaning when left tail cummulative prob = 0.025 x=?
qnorm(pnorm(1.96))      #showing the inverse funciton feature

    #random number generator : rnorm()
rnorm(10, mean = 5, sd = 3) #generating 10 random number form the normal distri
rnorm(10, 0, 1)             #generating 10 random num form normal distri
rnorm(10)                   #same as upper line, but simpler

#chi-square

    # making a chi-square
    x <- seq(0, 10, 0.01)       #since x of chi-square should always be positive
    y <- dchisq(x, df = 1)      # making a chi-sq, with degree of freedom 1

    #cumulative prob of chi-sq
    options(digits = 3)
    y <- pchisq(27.77778, df = 1, lower.tail = FALSE)
    options(digits = 4)
    pchisq(7.779, df = 4, lower.tail = FALSE)
    
    
    #quantile function of chi-sq = inverse function of pchisq
    qchisq(0.08427008, df = 1)

    #generate random number form chi-sq distri
    rchisq(10, df = 1)

# F-test
    #making a F-test
    y_df <- df(x, df1 = 3, df2 = 194, log = FALSE)
    plot(y_df)
    y_df[600]

    #cumulative prob
    y_pf <- pf(3.2, df1 = 3, df2 = 194)
    plot(y_pf)
    y_pf

    #quantile prob = inverse of cumulative prob
    y_qf <- qf(0.003852968, df1 = 3, df2 = 194)
    y_qf

# t-test
    #making a t-test
    x <- seq(-10, 10, 0.1)
    y_dt <- dt(x, df = 136, log = FALSE)
    plot(x, y_dt)

    #cumulative prob
    y_pt <- pt(-2.08, df = 136)
    y_pt

    # p-value = y_pt*2 # nolint

    #quantile prob
    y_qt <- qt(0.020, df = 136)
    y_qt

#maling a plot：plot()
plot(x, y)              #maling a plot of x vs.y
plot(x, y, type = "l")  #making a line plot
plot(x, y, type = "l", lty = 2) #line type = 2 will make a dashline
plot(x, y, type = "l", lty = 3) #line type = 3 will make a dot line
plot(x, y, type = "l", lwd = 3) #lwd = line width

#matix
y_1 <- matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE) # matrix byrow
y_2 <- matrix(c(1, 2, 3, 4), 2, 2, byrow = FALSE) # matrix column
z_1 <- y_1 * y_2 # not matrix multiplication
z_2 <- y_1 * y_2 # matrix multiplication

#loop
ss <- 0
for (i in 1:100){
    ss <- ss + i
    }             # i being an index number(essential for loop)
ss                # ss equals sum(1:100)

#readfiles
getwd()                          #checking working directory
setwd("/Users/raymond/Desktop/") # setting working directory as file
seizure1 <- read.csv("seizure.csv") #reading file
head(seizure1)                      #see the first 6 rows of d file
tail(seizure1)                      #see the last 6 rows of d file
dim(seizure1)                      #see dimension, 290 rows, 6 columns

#example
oo <- lm(formula = y ~ trt + bline + age, data = seizure1)
summary(oo)

x <- cbind(rep(1, nrow(seizure1)), seizure1$trt, seizure1$bline, seizure1$age)
solve(t(x) %*% x) %*% t(x) %*% matrix(seizure1$y, ncol = 1)


r <- 0.6934
p <- 0.2210
q = 0.0856 # nolint
EM.function <- function(p, q, r){
    x1 <- c()
    x2 <- c()
    x3 <- c()
    x4 <- c()
    x5 <- c()
    x6 <- c()
    i <- 1
    a <- 200
    b <- 60
    o <- 300
    ab <- 40
    done <- FALSE
    while (!done) {
        if (i < 16) {
        x1[i] <- a * (p^2 / (p^2 + 2 * p * r))
        x2[i] <- a * (2 * p * r / (p^2 + 2 * p * r))
        x3[i] <- b * (q^2 / (q^2 + 2 * q * r))
        x4[i] <- b * (2 * q * r / (q^2 + 2 * p * r))
        x5[i] <- o
        x6[i] <- ab
        SUM <- sum(x1[i], x2[i], x3[i], x4[i], x5[i], x6[i])
        p <- (2 * x1[i] + x2[i] + x6[i]) / 2 * SUM # nolint
        q <- (2 * x3[i] + x4[i] + x6[i]) / 2 * SUM # nolint
        r <- (2 * x5[i] + x2[i] + x4[i]) / 2 * SUM # nolint
        i <- i + 1
        }else {
            done <- TRUE
        }
    }
answer <- cbind(x1, x2, x3, x4, x5, x6)
return(p, q, r)
}
EM.function(0.2210, 0.0856, 0.6935)
write.table(EM.function(0.2210, 0.0856, 0.6935), file ="m2.txt", quote = FALSE, row.names = TRUE, col.names = TRUE)
getwd()
setwd("/Users/raymond/Desktop/R")

options(digits = 5)
EM.function <- function(x1, x2, x3, x4, x5, x6){
    p <- c()
    q <- c()
    r <- c()
    i <- 1
    a <- 200
    b <- 60
    o <- 300
    ab <- 40
    done <- FALSE
    while (!done) {
        if (i < 16) {
        p[i] <- (2 * x1 + x2 + x6) / (2 * sum(x1, x2, x3, x4, x5, x6)) # nolint
        q[i] <- (2 * x3 + x4 + x6) / (2 * sum(x1, x2, x3, x4, x5, x6)) # nolint
        r[i] <- (2 * x5 + x2 + x4) / (2 * sum(x1, x2, x3, x4, x5, x6)) # nolint
        x1 <- a * (p[i]^2 / (p[i]^2 + 2 * p[i] * r[i]))
        x2 <- a * (2 * p[i] * r[i] / (p[i]^2 + 2 * p[i] * r[i]))
        x3 <- b * (q[i]^2 / (q[i]^2 + 2 * q[i] * r[i]))
        x4 <- b * (2 * q[i] * r[i] / (q[i]^2 + 2 * p[i] * r[i]))
        x5 <- o
        x6 <- ab
        i <- i + 1
        }else {
            done <- TRUE
        }
    }
answer <- cbind(p, q, r)
return(answer)
}
EM.function(27.48756, 172.5124, 3.487709, 22.69725, 300, 40)
write.table(EM.function(27.48756, 172.5124, 3.487709, 22.69725, 300, 40), file ="m4.txt", quote = FALSE, row.names = TRUE, col.names = TRUE)
print.default(EM.function(27.48756, 172.5124, 3.487709, 22.69725, 300, 40), digits = 5)
matplot(EM.function(27.48756, 172.5124, 3.487709, 22.69725, 300, 40), type = c("b"), pch = 1, col = 1:3) 
