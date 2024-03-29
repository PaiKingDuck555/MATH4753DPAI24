#' Title
#'
#' @param v
#'
#' @return
#' @export
#'
#' @examples
get_mode <- function(v) {
  uniq_v <- unique(v)
  tab_v <- tabulate(match(v, uniq_v))
  uniq_v[tab_v == max(tab_v)]
}
