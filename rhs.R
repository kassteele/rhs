rhs <- function(...) {
  
  kleuren <- unlist(list(...))

  # Controleer of kleuren geldig zijn
  geldig <- kleuren %in% names(rhs_kleuren)
  if (!(all(geldig))) {
    stop(
      paste(
        kleuren[!geldig], "is geen geldige rijkshuisstijlkleur. Geldige kleuren zijn:\n",
        paste(names(rhs_kleuren), collapse = ", "),
        "\n"))
  }
  
  # Return kleuren
  return(rhs_kleuren[kleuren])
}
