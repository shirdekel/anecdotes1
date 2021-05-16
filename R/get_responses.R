#' Extract responses from jsPsych data
#'
#' @param data
#' @param names_to
#' @param names_pattern
#' @param values_to
#'
#' @return
#' @export
#'
get_responses <- function(data, names_to, names_pattern, values_to = "value") {
  data %>%
    dplyr::pull(responses) %>%
    na.omit() %>%
    purrr::map(~rjson::fromJSON(.x)) %>%
    purrr::flatten() %>%
    as.data.frame(stringsAsFactors = F) %>%
    tidyr::pivot_longer(cols = (-(contact:business_edu)),
                        names_to = names_to,
                        names_pattern = names_pattern,
                        values_to = values_to)
}
