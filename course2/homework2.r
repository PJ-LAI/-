# Ex.8 : Given 50 students, odd-numbered students are boys, even are girls,
#(11, 16, 23, 31, 36, 47, 50) pass midterm
#(3, 9, 16, 20, 27, 31, 36, 49, 50) pass final

    # Ex.8-1 : boys who passed midterm and final
    midterm <- c(11, 16, 23, 31, 36, 47, 50)
    final <- c(3, 9, 16, 20, 27, 31, 36, 49, 50)
    boy <- seq(1, 50, by = 2)
    girl <- seq(2, 50, by = 2)
    intersect(intersect(midterm, final), boy)
        # no.31

    # Ex.8-2 : girls who passed midterm and final
    intersect(intersect(midterm, final), girl)
        # no.16, no.36, no.50

    # Ex.8-3 : boys who passed midterm but failed final
    setdiff(intersect(boy, midterm), final)
        # no.11, no.23, no.47

    # Ex.8-4 : girls who passed final but failed midterm
    setdiff(intersect(girl, final), midterm)
        # no.20

# Ex.9 write a function to estimate regression coefficients

getwd()
setwd("/Users/raymond/Desktop/R/course/")
seizure <- read.csv("seizure.csv")
x <- matrix()
y <- matrix()
simp_reg <- function(x, y) {
    X <- cbind(rep(1, nrow(seizure)), x)
    Y <- y
    return(solve(t(X) %*% X) %*% t(X) %*% Y)
}
simp_reg(seizure$ltime, seizure$y)
    #intercept : -3.922414
    #slope : 15.906957

    # solution2

    reg <- function(y, x) {
            b <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
            a <- mean(y) - b * mean(x)
            return(c(b, a))
    }
reg(seizure$y, seizure$ltime)
