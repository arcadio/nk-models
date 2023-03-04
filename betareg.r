library(betareg)

kill <- function(d) (d$dead / (d$dead + d$live)) - (d$sdea / (d$sdea + d$sliv))

meap <- function(p, d, b, t) mean(p[d[[t]] == b]) - mean(p[d[[t]] != b])

gsam <- function(d, u) d[d$id %in% sample(u, replace=T),]

isam <- function(d, u) d[sample(1:nrow(d), replace=T),]

read <- function(fil) {
    dat <- na.omit(read.csv(paste0("dat/", fil, ".csv")))
    dat$day <- as.factor(dat$day)
    dat$type <- as.factor(dat$type)
    dat$kill <- kill(dat)
    dat
}

boot <- function(dat, sam, bas, type, fml, b, q) {
    p <- numeric(b)
    u <- unique(dat$id)
    for (i in 1:b) {
        d <- sam(dat, u)
        m <- betareg(fml, data=d)
        r <- predict(m)
        p[i] <- meap(r, d, bas, type)
    }
    quantile(p, c((1-q)/2, 1-(1-q)/2))
}

run <- function(fil, bas, type="type", fml=kill~type+id, sam=gsam, b=1000, q=0.95) {
    dat <- read(fil)
    mod <- betareg(fml, data=dat)
    sry <- summary(mod)
    prd <- predict(mod)
    mea <- meap(prd, dat, bas, type)
    bot <- boot(dat, sam, bas, type, fml, b, q)
    list(sry=sry, mea=mea, bot=bot)
}

run("2b_a", "tcnv") # 4.56e-8 0.048 (0.026, 0.069)
run("2b_b", "tcnv") #   0.058 0.019 (-0.002, 0.041)

run("4c", "high") # <2e-16 0.080 (0.066, 0.095)
run("4d", "high") # <2e-16 0.071 (0.048, 0.092)
run("4e", "high") #  0.011 0.028 (0.005, 0.052)
run("4f", "high") #  0.053 0.015 (-0.004, 0.034)
