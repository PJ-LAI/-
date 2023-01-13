betatv <- seq(0, 0.5, by = 0.1)
sig <- seq(0.01, 0.05, by = 0.01) #significant level
n <- 50
y <- c()
norep <- 1000
rejrate <- matrix(NA, 2 * length(betatv), length(sig))
for (betaloop in 1 : length(betatv)) {
    pvalue <- c()
    pvaluelr <- c()
    for (i in 1 : norep) {
        set.seed(i)
        age <- rnorm(n = n, mean = 35, sd = 5)
        watchtv <- rnorm(n = n, mean = 4, sd = 1)
        error <- rnorm(n = n, mean = 0, sd = 1)
        y <- 7 - (0.05 * age) + (betatv[betaloop] * watchtv) + error


        x <- cbind(rep(1, length(y)), age, watchtv)
        mle <- solve(t(x) %*% x) %*% t(x) %*% y
        risidual <- y - x %*% mle
        mse <- sum(risidual ^ 2) / (n - 3)
        varcov <- mse * solve(t(x) %*% x)
        semle <- sqrt(diag(varcov))
        ward <- mle / semle
        pvalue[i] <- ((1 - pt(abs(ward), n - 3)) * 2)[3]
        pvaluelm <- summary(lm(y ~ age + watchtv))$coef[3, 4]
        pvaluelr[i] <- pchisq(2 * (logLik(lm(y ~ age + watchtv)) - logLik(lm(y ~ age))), df = 1, lower.tail = FALSE) #nolint
        if (abs(pvalue[i] - pvaluelm) > 1e-4) {
            cat("error\n")
            break()
        }
    }
    for (k in 1:length(sig)) {
        rejrate[betaloop, k] <- sum(pvalue < sig[k]) / norep #rejection rate
        rejrate[betaloop + length(betatv), k] <- sum(pvaluelr < sig[k]) / norep
    }
}
rejrate
matplot(sig, t(rejrate),
    col = c(1:length(betatv)),
    pch = c(1:length(betatv)),
    lty = c(rep(1, 6), rep(3, 6)),
    type = "b",
    frame = FALSE,
    xlab = "Significance level",
    ylab = "Rejection rate")
abline(a = 0, b = 1, col = 8)
legend(0.04, rejrate[1, 4] + 0.05, expression(paste(beta, "=0")), bty = "n")
legend(0.04, rejrate[2, 4] + 0.05, expression(paste(beta, "=0.1")), bt = "n")
legend(0.04, rejrate[3, 4] + 0.05, expression(paste(beta, "=0.2")), bty = "n")
legend(0.04, rejrate[4, 4] + 0.05, expression(paste(beta, "=0.3")), bty = "n")
legend(0.04, rejrate[5, 4] + 0.05, expression(paste(beta, "=0.4")), bty = "n")
legend(0.04, rejrate[6, 4] + 0.05, expression(paste(beta, "=0.5")), bty = "n")
