filter_vars <- c("A", "B", "C")
filter_vals <- list(A = c("apples","aardvarks"), B = "bananas", C = c("corn on the cob", "crackers"))

# subset(dat, A %in% c("apples","aardvarks") & B %in% c("bananas") & C %in% c("corn on the cob","crackers"))

filter_in <- lapply(filter_vals, function(x) paste0('"', x, '"', collapse = ","))
filter_in <- paste0("%in% c(", filter_in, ")")
filter_string <- paste(filter_vars, filter_vals, collapse = " & ")
filter_string
