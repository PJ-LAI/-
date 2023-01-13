# homework4
#Q1 : Calculare factorial(10), using while() and for() respectively
    # while(){}
        fa <- function(x) {
            if (x != round(x) || x < 0) {
                x <- "error, x should be nature number or 0"
            }else if (x == 0 || x == 1) {
                x <- 1
            }else {
                i <- x - 1
                while (i != 1) {
                        x <- x * i
                        i <- i - 1
                    }
            }
            return(x)
        }
        fa(10)     # 10! = 3628800

    # for(){}
        fac <- function(x) {
            if (x != round(x) || x < 0) {
                x <- "error, x should be nature number or 0"
            }else if (x == 0 || x == 1) {
                x <- 1
            }else {
                for (i in 1:(x - 1)) {
                        x <- x * i
                    }
            }
            return(x)
        }
        fac(10)     # 10! = 3628880


# Q2 use while(){} and cat() to find missing value
    find_na <- function(x) {
        done <- FALSE
        i <- 1
        while (!done) {
            if (is.na(x[i] == 1)) {
                done <- TRUE
            }else if (i == (length(x))) {
                i <- "NULL"
                done <- TRUE
            }else {
                i <- i + 1
            }
        }
        return(cat(i, "\n"))
    }
    x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
    find_na(x)
# Q3 : BMI curve ID51 - ID60
    setwd("/Users/raymond/Desktop/R/course/data")
    BMI <- read.csv("/Users/raymond/Desktop/R/course/data/BMIrepeated.csv")
    x <- seq(0, 9, by = 3)
    y <- cbind(BMI$BMI0, BMI$BMI1, BMI$BMI2, BMI$BMI3)
    png(filename = "hw4.3.2.png", width = 2000, height = 3000, res = 200)
    par(mfrow = c(1, 2))
    plot(x, y[1, ], type = "b",
            lwd = 1, col = 1, lty = 1, pch = 1,
            ylim = c(15, 50),
            axes = FALSE,
            xlab = "month", ylab = "BMI", main = "placebo group")
    for (i in 2:10) {
        lines(x, y[i, ], lty = 1, lwd = 1, col = i, type = "b", pch = i)
    }
    axis(1, at = x, labels = seq(0, 9, by = 3))
    axis(2, at = y)
    legend("top", bty = "n",
    c("ID1", "ID2", "ID3", "ID4", "ID5", "ID6", "ID7", "ID8", "ID9", "ID10"),
    lty = 1, col = (1:10), lwd = 1, pch = (1:10), cex = 0.8, pt.cex = 1.0)

    plot(x, y[51, ], type = "b",
            lwd = 1, col = 11, lty = 1, pch = 11,
            ylim = c(15, 50),
            axes = FALSE,
            xlab = "month", ylab = "BMI", main = "drug group")
    for (k in 52:60) {
        lines(x, y[k, ], lty = 1, lwd = 1, col = k - 40, type = "b", pch = k-40)
    }
    axis(1, at = x, labels = seq(0, 9, by = 3))
    axis(2, at = y)
    legend("top", bty = "n",
    c("ID51", "ID52", "ID53", "ID54", "ID55", "ID56", "ID57", "ID58", "ID59", "ID60"),
    lty = 1, col = (11:20), lwd = 1, pch = (11:20), cex = 0.8, pt.cex = 1.0)
    dev.off()

# Q4 : 3D PIE with percentage
        library(plotrix)
        par(mfrow = c(1, 1))
        subject <- c(10, 12, 4, 16, 8)
        lbls <- c("US", "UK", "Australia", "Germany", "France")
        percent <- round((subject / sum(subject)) * 100)
        lbls <- paste(lbls, percent)
        lbls <- paste(lbls, "%", sep = "")
        png(filename = "hw4.4.png", width = 3000, height = 2000, res = 200)
        pie3D(subject, labels = lbls, explode = 0.1, col = rainbow(length(lbls))
            , main = "pie chart of countries")
        dev.off()


