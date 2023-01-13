# homework5

    # EX S2 recursive programming
    steps <- function(n) {
        if (n == 1) {
            return(1)
        } else {
            return(1 + 2 * steps(n - 1))
        }
    }
    steps(20)   #1048575

    # EX S3 : apply()
    x <- matrix(c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000,
                3600, 4500, 10000, 8500, 3000, 10000, 1000, NA, 1200, 10000,
                3800, 5500, 9000, 6000, 6600, 3000, 9600, 6500, 8200, 8000,
                5000, 6600, 13000, 4500, 5000, NA, 10600, 9500, 7600, 6000,
                6600, 8000, 17000, 3000, 7000, 1000, 12600, 8500, 6000, NA)
                , 5, 10, byrow = TRUE)
    head(x)
        # Me of each row
        apply(x, 1, median, na.rm = TRUE)
            # Median(r1, r2, r3, r4, r5) = (3600, 4500, 6550, 6600, 7000)

        # Me of each column
        apply(x, 2, median, na.rm = TRUE)
            # Median(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) = (3800, 5500, 12000, 5250, 5000, 2500, 9600, 8000, 6000, 8500)

        # Max of each row
        apply(x, 1, max, na.rm = TRUE)
            # Max(r1, r2, r3, r4, r5) = (12000, 10000, 9600, 13000, 17000)

        # Max of each column
        apply(x, 2, max, na.rm = TRUE)
            # Max(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) = (6600, 8000, 17000, 8500, 7000, 10000, 12600, 9500, 8200, 10000)

        # min of each row
        apply(x, 1, min, na.rm = TRUE)
             # min(r1, r2, r3, r4, r5) = (600, 1000, 3000, 4500, 1000)

        # min of each column
        apply(x, 2, min, na.rm = TRUE)
            # min(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) = (3600, 4500, 9000, 3000, 1000, 1000, 600, 6500, 1200, 6000)

    # EX. S8: solve()
    A <- matrix(c(1, -3, 1, 1, -2, 3, 1, -1, 1), 3, 3, byrow = TRUE)
    b <- c(4, 6, 4)
    solve(A, b)
        # x = 3, y = 0, z = 1
