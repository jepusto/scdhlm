
# further cleaning
dat$phase_pair <- unlist(by(dat, dat${user_case}, phase_pairs))
dat <- droplevels(dat)
