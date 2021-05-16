##' @title Import anecdotes 1
##'
##' Data was collected through both Pavlovia and the psych server

##' @return
##' @author Shir Dekel
##' @export
##' @param data_directory
import_data_anecdotes_1 <- function(data_directory) {
  data_server <-
    project_extdata_path("JSPsychData15-01-2021 12 39.csv") %>%
    read_csv(col_types = cols()) %>%
    select(sectionName, sectionValue, time_elapsed, dateCreated) %>%
    pivot_wider(
      names_from = "sectionName",
      values_from = "sectionValue"
    ) %>%
    filter(!is.na(anecdote_condition)) %>%
    mutate(
      across(dateCreated, ~ .x %>%
        dmy_hms(tz = "Australia/Sydney"))
    )

  data_local_dates <-
    data_directory %>%
    list.files() %>%
    str_match("(.*?)_(.*).csv") %>%
    .[, 2:3] %>%
    as_tibble(.name_repair = "minimal") %>%
    set_names("subject", "dateCreated") %>%
    mutate(across(dateCreated, as_datetime))

  data_local <-
    data_directory %>%
    list.files(full.names = TRUE) %>%
    map_dfr(~ .x %>%
      read_csv(col_types = cols())) %>%
    mutate(
      across(
        c(button_pressed, rt),
        as.character
      )
    ) %>%
    nest_by(subject) %>%
    ungroup() %>%
    full_join(data_local_dates, by = "subject") %>%
    unnest(data)

  data_server %>%
    bind_rows(data_local)
}
