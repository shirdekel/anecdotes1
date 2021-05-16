##' @title Rename participants with name typos.

##' @return
##' @author Shir Dekel
##' @export
rename_typos <- function(sona) {
  across(
    sona,
    ~ .x %>%
      recode(
        "zimou yaun" = "zimou Yuan",
        "zimou yuan" = "zimou Yuan",
        "ccro0304" = "Cecilia Croguennec",
        "sharri" = "sharri sarraj"
      ) %>%
      str_to_lower()
  )
}
