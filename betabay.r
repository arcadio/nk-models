library(rstanarm)
library(purrr)
source("data.r")

options(mc.cores=parallel::detectCores())

run <- function(dat, bas, prd=T, var="type", fml=kill~type+id) {
    fit <- stan_betareg(fml, dat, link="logit", link.phi="log", iter=5e3, seed=42)
    pst <- posterior_epred(fit)
    gm1 <- rowMeans(pst[,dat[[var]]==bas])
    gm2 <- rowMeans(pst[,dat[[var]]!=bas])
    if(prd) {
        dif <- rowMeans(pst[,dat[[var]]==bas] - pst[,dat[[var]]!=bas])
        fld <- rowMeans(pst[,dat[[var]]==bas] / pst[,dat[[var]]!=bas])
    } else {
        dif <- rowMeans(pst[,dat[[var]]==bas]) - rowMeans(pst[,dat[[var]]!=bas])
        fld <- rowMeans(pst[,dat[[var]]==bas]) / rowMeans(pst[,dat[[var]]!=bas])
    }
    dry <- summary(dif)
    fry <- summary(fld)
    fsr <- ifelse(dry[3] > 0, mean(dif < 0), mean(dif > 0))
    list(fit=fit, dif=dif, gm1=gm1, gm2=gm2, fld=fld, fry=fry, fsr=fsr)
}

smr <- function(res) {
    pnt <- c(0.025, 0.5, 0.975)
    c(res$fsr, quantile(res$fld, pnt), quantile(res$gm1, pnt), quantile(res$gm2, pnt))
}

dfm <- function(lst) {
    dat <- reduce(map(lst, smr), rbind)
    dat <- data.frame(dat, row.names=NULL)
    lev <- c(2, 50, 97)
    colnames(dat) <- c("fsr", paste0("f", lev), paste0("g1m", lev), paste0("g2m", lev))
    dat
}

d2c <- read("2c")
tcv <- d2c[d2c$type == "tcnv",]
trg <- d2c[d2c$type == "treg",]

dba <- run(read("2b_a"), "tcnv")
dbb <- run(read("2b_b"), "tcnv")

dca <- run(tcv, 2.5, prd=F, var="day", fml=kill~day)
dcb <- run(trg, 2.5, prd=F, var="day", fml=kill~day)
dcc <- run(d2c, "tcnv")

d4c <- run(read("4c"), "high")
d4d <- run(read("4d"), "high")
d4e <- run(read("4e"), "high")
d4f <- run(read("4f"), "high")

res <- dfm(list(dba, dbb, dca, dcb, dcc, d4c, d4d, d4e, d4f))
rownames(res) <- c("2b(a)", "2b(b)",
                   "2c(tcnv)", "2c(treg)", "2c(all)",
                   "4c", "4d", "4e", "4f")
