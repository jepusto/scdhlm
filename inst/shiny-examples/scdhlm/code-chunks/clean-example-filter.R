
#Clean Data

##filter
subset_vals <- sapply(c("{user_filtervars}"), function(x) levels(dat[[x]])[dat[[x]]] %in% input[[paste0("filter_",x)]])
dat <- dat[apply(subset_vals, 1, all),]
  
dat <- dat[,c("{user_parms}")]
names(dat) <- c("case","session","phase","outcome")
trt_phase <- levels(as.factor(dat$phase))[2]
dat$trt <- as.numeric(dat$phase==trt_phase) 






