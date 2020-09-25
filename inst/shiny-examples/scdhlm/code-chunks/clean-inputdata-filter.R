#Clean Data
dat${user_caseID} <- factor(dat${user_caseID}, levels = unique(dat${user_caseID}))
dat${user_session} <- as.numeric(dat${user_session}) 
dat${user_phaseID} <- factor(dat${user_phaseID}, levels = unique(dat${user_phaseID}))
dat${user_outcome} <- as.numeric(dat${user_outcome}) 

#filter data
subset_vals <- sapply(c("{user_filtervars}"), function(x) levels(dat[[x]])[dat[[x]]] %in% input[[paste0("filter_",x)]])
dat <- dat[apply(subset_vals, 1, all),]

# remove rows with missing outcome values
dat <- dat[!is.na({user_outcome}),]
trt_phase <- "{user_treatment}"

dat$trt <- as.numeric(dat${user_phaseID}==trt_phase)