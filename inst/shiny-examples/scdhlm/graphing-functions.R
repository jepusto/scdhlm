
graph_MB <- function(dat) {
  ggplot(dat, aes(session, outcome, color = as.factor(phase), shape = as.factor(phase))) + 
    geom_point() + 
    geom_line() + 
    facet_grid(case ~ .) + 
    theme_bw() + 
    labs(color = "Phase", shape = "Phase")

}