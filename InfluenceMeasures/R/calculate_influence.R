calculate_and_plot <- function(data, model, measure = "cooks", plot = TRUE) {

  if (!inherits(model, "lm")) {
    stop("Model must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  if (any(is.na(data))) {
    stop("Data contains NA values.")
  }
  if (any(is.infinite(data))) {
    stop("Data contains infinite values.")
  }
  if (!measure %in% c("cooks", "dffits", "hadi")) {
    stop("Invalid measure specified. Choose 'cooks', 'dffits', or 'hadi'.")
  }


  influence_values <- switch(measure,
                             cooks = cooks.distance(model),
                             dffits = dffits(model),
                             hadi = hadi_measure(model))  # Placeholder for Hadi's measure


  if (plot) {
    plot(influence_values, type = "h", main = paste("Influence Measure:", measure),
         xlab = "Observation", ylab = "Influence")
    abline(h = 4/length(influence_values), col = "red", lty = 2)
  }

  return(influence_values)
}
