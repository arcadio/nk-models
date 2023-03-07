library(rstanarm)
source("data.r")

options(mc.cores=parallel::detectCores())

run <- function(dat, bas, fml=kill~type+id) {
    fit <- stan_betareg(fml, dat, link="logit", link.phi="log")
    pst <- posterior_epred(fit)
    dif <- rowMeans(pst[,dat$type==bas] - pst[,dat$type!=bas])
    fld <- rowMeans(pst[,dat$type==bas] / pst[,dat$type!=bas])
    dry <- summary(dif)
    fry <- summary(fld)
    fsr <- ifelse(dry[3] > 0, mean(dif < 0), mean(dif > 0))
    list(fit=fit, dif=dif, fld=fld, dry=dry, fry=fry, fsr=fsr)
}

run(read("2b_a"), "tcnv")
run(read("2b_b"), "tcnv")

run(read("4c"), "high")
run(read("4d"), "high")
run(read("4e"), "high")
run(read("4f"), "high")
