#' @title `{drake}` plan

#' @return
#' @author Shir Dekel
#' @export
get_plan <- function() {
  drake_plan(
    power = get_power()
  )
}
