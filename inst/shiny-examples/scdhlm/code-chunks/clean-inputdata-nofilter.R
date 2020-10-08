
# Clean data
dat${user_caseID} <- factor(dat${user_caseID}, levels = unique(dat${user_caseID}))
dat${user_session} <- as.numeric(dat${user_session}) 
dat${user_phaseID} <- factor(dat${user_phaseID}, levels = unique(dat${user_phaseID}))
dat${user_outcome} <- as.numeric(dat${user_outcome}) 

# remove rows with missing outcome values
dat <- dat[!is.na(dat${user_outcome}),]
trt_phase <- "{user_treatment}"

# create trt variable
dat$trt <- as.numeric(dat${user_phaseID}==trt_phase)
