# class4 : input, putput and plotting
    # input from a file
        # normal way
        setwd("/Users/raymond/Desktop/R/course/data")
        list.files()
        seizure1 <- read.csv("seizure.csv")
        seizure2 <- read.table("seizure.txt")
        dim(seizure1)
        dim(seizure2)

        # for large file
        install.packages("data.table")
        library(data.table)
        seizure1 <- fread("seizure.csv")
        seizure2 <- fread("seizure.txt")
        dim(seizure1)
        dim(seizure2)

        # check time difference
        starttime <- Sys.time()
        seizure1 <- read.table("/Users/raymond/Desktop/R/course/data/example.txt") # nolint
        Sys.time() - starttime
        starttime <- Sys.time()
        seizure1 <- fread("/Users/raymond/Desktop/R/course/data/example.txt")
        Sys.time() - starttime

        # scan() to read file
        scan(file = "seizure.txt", what = double(), n = -1, sep = "", skip = 0, quiet = FALSE)  # nolint
            # what = double is just better(?), n = number of data you want to read, n = -1 means read all data.  # nolint
            # sep means what to seperate # nolint
            # skip means if there's row you want to skip
            # quiet = false to show description for dataset, true will hide descriptions # nolint
        scan(file = "seizure.csv", what = double(), n = -1, sep = ",", skip = 1, quiet = FALSE)  # nolint

    # output
        x <- "jaylen"
        y <- "jessie"
        z <- "jaime"
        (name <- paste(x, y, z, sep = ", "))

        x <- 7
        n <- 5
        # display conjugations
        cat("powers of", x, "\n") #cat display conjugate things
        cat("exponent result\n\n") # \ for new line

        result <- 1
        for (i in 1:n) {
            result <- result * x
            cat(format(i, width = 8), format(result, width = 10), "\n", sep = "") # nolint
        }
        cat(paste(format(1:n, width = 8), format (x^(1:n), width = 10), "\n"), sep = "") # nolint

        # output to a file
        write.table(seizure1, "test1.txt", sep = "", row.name = false, col.name = false, quote = false, na = "x", append = false) # nolint
            # seizure1 is an exist data
            # "means output filename"
            # sep means what to seperate
            # quote is used when you have character data
            # na = x means replace missing value as x
            # append means if the file has already a test1.txt, replace it?

    # plotting in R
        # line plot
        setwd("/Users/raymond/Desktop/R/course/data")
        bmi <- read.csv("/Users/raymond/Desktop/R/course/data/BMIrepeated.csv")
        x <- seq(0, 9, by = 3)
        y <- cbind(bmi$BMI0, bmi$BMI1, bmi$BMI2, bmi$BMI3)

        par(mfrow = c(1, 2)) # par means figure, mfrow = multiple figures  by row # nolint
        plot(x, y[1, ], type = "b",
            lwd = 1, col = 1, lty = 1, pch = 1,
            ylim = c(15, 50),
            axes = FALSE,
            xlab = "month", ylab = "BMI", main = "placebo group")
            # [1,] means first row, all column
            # type = "b" = both line and dot
            # col = color, 1 is black
            # lty = line type
            # pch = plot character, 1 is dot
            # ylim = limit of y
            # axes = false = don't plot axes
        axis(1, at = x, labels = seq(0, 9, by = 3))
        axis(2, at = y)

        for (subj in 2:10) {
            lines(x, y[subj, ], lty = 1, lwd = 1, col = subj, type = "b", pch = subj) # nolint
        }

        legend("topright", bty = "n",
            c("ID1", "ID2", "ID3", "ID4", "ID5", "ID6", "ID7", "ID8", "ID9", "ID10"), # nolint
            lty = 1, col = (1:10), lwd = 1, pch = (1:10))

        # boxplot

        par(mfrow = c(1, 1))
        boxplot(bmi$BMI0, bmi$BMI1, bmi$BMI2, bmi$BMI3, col = "#fb00ff86")
        
        #histogram
        hist (bmi$BMI0, col = "#bebebe30",
        main = "BMI at baseline", xlab = "BMI")

        # multiple columns(matrix plot)
        x <- seq(0.5, 2, 0.01)
        y <- cbind(exp(x), log(x))
        matplot(x, y,
            col = (3:4), pch = (1:2), lty = (1:2), type = "l", frame = FALSE)
        
        # pie chart
        par(mfrow = c(1, 1))
        subject <- c(10, 12, 4, 16, 8)
        lbls <- c("heterosexual", "gay", "lesbian", "bisexual", "others")
        percent <- round((subject / sum(subject)) * 100)
        lbls <- paste(lbls, percent)
        lbls <- paste(lbls, "%", sep = "")
        pie(subject, labels = lbls, main = "pie chart of sexuality")

        #3D pie chart
        install.packages("plotrix")
        library(plotrix)
        par(mfrow = c(1, 1))
        subject <- c(10, 12, 4, 16, 8)
        lbls <- c("US", "UK", "Australia", "Germany", "France")
        percent <- round((subject / sum(subject)) * 100)
        lbls <- paste(lbls, percent)
        lbls <- paste(lbls, "%", sep = "")
        pie3D(subject, labels = lbls, explode = 0.1, col = rainbow(length(lbls))
            ,main = "pie chart of countries")