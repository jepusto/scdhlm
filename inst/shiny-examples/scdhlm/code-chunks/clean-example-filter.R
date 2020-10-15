# filter
subset_vals <- sapply(c("{user_filtervars}"), function(x) levels(dat[[x]])[dat[[x]]] %in% input[[paste0("filter_",x)]])
dat <- dat[apply(subset_vals, 1, all),]
