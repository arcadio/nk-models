kill <- function(d) (d$dead / (d$dead + d$live)) - (d$sdea / (d$sdea + d$sliv))

read <- function(fil) {
    dat <- na.omit(read.csv(paste0("dat/", fil, ".csv")))
    dat$type <- as.factor(dat$type)
    dat$id <- as.factor(dat$id)
    dat$kill <- kill(dat)
    dat
}
