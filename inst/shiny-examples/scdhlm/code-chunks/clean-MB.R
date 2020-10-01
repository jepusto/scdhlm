
dat$session_trt <- unlist(by(dat, dat${user_case}, function(x, trt_phase) pmax(0, x${user_session} - min(x${user_session}[x${user_phase}==trt_phase])), trt_phase = trt_phase)) 
dat${user_session} <- dat${user_session} - {user_model_center} # center
dat <- droplevels(dat)
