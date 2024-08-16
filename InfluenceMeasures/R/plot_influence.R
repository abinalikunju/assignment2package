plot_influence <- function(data, model, measure = "cooks") {
  influence_values <- calculate_influence(data, model, measure)
  plot(influence_values, type = "h", main = paste("Influence Measure:", measure),
       xlab = "Observation", ylab = "Influence")
  abline(h = 4/length(influence_values), col = "red", lty = 2)
}
