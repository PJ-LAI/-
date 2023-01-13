# class 3 <- object modes, logic and loops

# objects and class
    #objects <- only six of it
    a <- 5
    mode(a) # r will show "numeric", saying that a is a numeber

    a <- 5 + 6i
    mode(a) # r shows "complex"

    a <- TRUE
    mode(a) # r shows "logic"

    a <- "mary"
    mode(a) # r shows "character"

    a <- c(1, 2, 3, NA)
    mode(a) # r shows "numeric", because these are all number

    mode(plot) # r shows "function"

    # combining different modes <- list()
    aa <- list(x = seq(1, 10, by = 1), y = LETTERS[1:10])
    mode(aa)    # r shows "list"
    mode(aa$x)  # r shows "numeric"
    mode(aa$y)  # r shows "character"
    attributes(aa)  # r shows "x" "y", this show you objects in aa

    # turning list to matrix <- as.data.frame()
    as.data.frame(aa)   # aa is turned into matrix

    # how object is oriented in r
    a <- matrix(c(1:4), nrow = 2, ncol = 2)
    mode(a)     # r shows "numeric"
    class(a)    # r shows "matric", "array"
    # array is the larger set, matrix is smaller set, array includes matirx
    # bcz matrix only deals with 2 dimensions, array deals with higher dimention
    (a <- array(c(1:60), dim = c(3, 4, 5)))
    a[1, 2, 1]
    mode(a)     # "numeric“
    class(a)    # "array"

# force r to change the mode of an object
    as.data.frame() # <- change to matrix

    as.character(seq(1, 10, by = 1)) # <- change number as characters

    as.integer(3.7) # result will be 3, because it only keeps the integer part

    as.numeric((5 + 6i)) # show 5, discard 6i
    as.numeric(1 == 2)   # show 0

    as.integer(TRUE) # show 1, because TRUE can be convert to 1

# if{}else{}
    x <- c(3600, 5000, 12000, NA, 1000, 600, 7500, 1800, 9000)

    # correct command of if{}else{}
    if (mean(x, na.rm = TRUE) > 1000) {
        rich <- 1
    }else {
        rich <- 0
    }
    rich # r show 1, meaning mean > 1000

    # incorrect command of if{}else{}
    if (mean(x, na.rm = TRUE) > 1000) {
        rich <- 1
    }
    else{   # if put in other line # nolint
        rich <- 0
    }
        # it will be error, because r can't recognize else{} alone

# ifelse()
    # ifelse()
    x <- c(-2, -1, 1, 2, 0)
    ifelse(x > 0, print(x), "false") #ifelse(condition, do what when true, when false) # nolint

    #ifelse(ifelse())
     x <- c(-2, -1, 1, 2, 0)
     ifelse(x > 0, "positive", ifelse(x < 0, "negetive", "zero"))
     ifelse(x > 0, print(x),"" )

# loop <- while(), for()
    #for()
    ss <- 0
    for(i in 1:100) {
        ss <- ss + i
    }
    ss

    # while(), same meaning as above
    # all function written by for() can be rewritten by while()
    ss <- 0
    i <- 1
    while(i <= 100) {
        ss <- ss + i
        i <- i + 1
    }
    ss

    # use while to bulid "do until loop"
    grade <- as.integer(rnorm(n = 10000, mean = 75, sd = 3))
    length(grade)
    good.grade <- c()
    sample.size <- 1
    i <- 1
    done <- FALSE
    while(!done) {
        if(grade[i] > 80) {
            good.grade[sample.size] <- grade[i]
            sample.size <- sample.size + 1
        }
        if(sample.size > 100){
            done <- TRUE
        }
        i <- i + 1
    }
    good.grade[1]
    good.grade

    # do the samething as above
    which(grade > 80)[1:100]
    grade[which(grade > 80)[1:100]]

    # example
    getwd()
    setwd("/Users/raymond/Desktop/R/")
    example <- read.table("example.txt", header = TRUE)
    head(example[, 1:7])
        #find height of first 100 subjects > 60 y/o
        height <- c()
        sample <- 1
        i <- 1
        done <- FALSE
        while(!done) {
            if(example$Age[i] > 60){
                height[sample] <- example$Height[i]
                sample <- sample + 1
            }
            if(sample > 100) {
                done <- TRUE
            }
            i <- i+1
        }
        height

        # second way
        (height <- example$Height[which(example$Age > 60) [1:100]])


        mode(NA) == "numeric"
