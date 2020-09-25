
#Clean Data
dat <- dat[,c("{user_parms}")]
names(dat) <- c("case","session","phase","outcome")
trt_phase <- levels(as.factor(dat$phase))[2]
dat$trt <- as.numeric(dat$phase==trt_phase) 