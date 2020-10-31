#' Rijkshuisstijlkleuren
#'
#' @param col Character vector met kleurnamen uit het Rijkshuisstijlpalet. Hoofdletterongevoelig.
#'
#' @details
#' Geldige kleurnamen zijn:
#'
#' Paars, Violet, Robijnrood, Roze, Rood, Oranje, Donkergeel, Geel, Donkerbruin, Bruin,
#' Donkergroen, Groen, Mosgroen,  Mintgroen, Donkerblauw, Hemelblauw, Lichtblauw
#'
#' Ieder kleur heeft ook twee lichtere tinten door ze aan te vullen met 1 of 2.
#'
#' Grijs 1 t/m 7 zijn neutrale tinten voor achtergrondkaders, grafieken en tabellen.
#'
#' @return Character vector met kleurdefinities.
#'
#' @references \url{https://www.rijkshuisstijl.nl/basiselementen/basiselementen-online/online-kleuren}
#'
#' @examples rhs(col = c("paars", "groen"))
#'
#' @export
rhs <- function(col) {

  # col in kleine letters
  col <- tolower(col)

  # Controleer of kleurnamen geldig zijn
  geldig <- col %in% names(rhs_kleuren)
  if (!(all(geldig))) {
    stop(
      paste(
        col[!geldig], "is geen geldige rijkshuisstijlkleur. Geldige kleurnamen zijn:\n",
        paste(names(rhs_kleuren), collapse = ", "),
        "\n"))
  }

  # Return kleuren
  return(unname(rhs_kleuren[col]))
}
