##' @title Get power analysis
##'
##' @return
##' @author Shir Dekel
##' @export
get_power <- function() {
  ## Jaramillo et al. (2019)

  ## ## Download data file
  ## library(osfr)
  ## osf_retrieve_node("dkcwv") %>%
  ##   osf_ls_files(pattern = "Experiment 2") %>%
  ##   osf_ls_files(pattern = "xlsx") %>%
  ##   osf_download(path = file.path("inst", "jaramillo2019_data"))

  ### Import data
  dat_jaramillo <-
    file.path(
      "inst",
      "jaramillo2019_data",
      "Reasoning about Anecdotes - Study 2.xlsx"
    ) %>%
    read_excel() %>%
    filter(Exclude == "No")

  ### EMM
  anova_jaramillo <-
    dat_jaramillo %>%
    aov_ez(
      dv = "EffectivePostTest",
      id = "Subject",
      between = "Condition",
      anova_table = "pes",
      type = 2
    )

  em_jaramillo <-
    anova_jaramillo %>%
    emmeans(~Condition) %>%
    summary()

  N_jaramillo <-
    dat_jaramillo %>%
    nrow()

  n_jaramillo <-
    dat_jaramillo %>%
    group_by(Subject, Condition) %>%
    nest() %>%
    ungroup() %>%
    count(Condition)

  n_max_jaramillo <-
    n_jaramillo %>%
    pull(n) %>%
    max()

  sd_jaramillo <- em_jaramillo$SE * sqrt(N_jaramillo)
  cv_jaramillo <- sd_jaramillo / em_jaramillo$emmean

  ## Hoeken & Hustinx (2009)
  n_hoeken <- 88
  mu_hoeken <- c(0.676, -0.037) + 3.99
  sd_hoeken <- c(0.105, 0.124) * sqrt(n_hoeken) # SD of the means
  se_hoeken <- 0.148 # Specific contrast SE
  cv_hoeken <- sd_hoeken / mu_hoeken
  dif_hoeken <- mu_hoeken[1] - mu_hoeken[2]
  t_hoeken <- dif_hoeken / se_hoeken
  cohen_d_hoeken <-
    apa::cohens_d_(
      t = t_hoeken,
      paired = T,
      n = n_hoeken
    )

  ## Wainberg (2018)
  mu_wainberg <- list(
    combined = 42,
    enhanced = 78,
    statistics = 96
  )
  n_wainberg <- 92

  ## Power analysis
  ### Setup
  design <- "4b*2b"
  n <- 41
  mu <- c(
    c(mu_wainberg$combined * cohen_d_hoeken, mu_wainberg$combined) %>%
      rep(2),
    mu_wainberg$enhanced %>%
      rep(2),
    mu_wainberg$statistics %>%
      rep(2)
  )
  sd_predicted <- max(mu) * mean(c(cv_hoeken, cv_jaramillo))
  alpha_level <- 0.05
  labelnames <- c(
    "Evidence", "Anecdote", "Statistics + anecdote",
    "Enhanced statistics + anecdote", "Statistics", "Similarity",
    "Low", "High"
  )

  ## Power analysis
  design_result <-
    Superpower::ANOVA_design(
      design = design,
      n = n,
      mu = mu,
      sd = sd_predicted,
      labelnames = labelnames,
      plot = F
    )

  power <-
    Superpower::plot_power(
      design_result,
      max_n = 50,
      desired_power = 80,
    )

  n <-
    power %>%
    pluck("anova_n") %>%
    filter(variable == "Evidence:Similarity") %>%
    pull(n)

  n_total <- n * 7

  power %>%
    append(lst(n)) %>%
    append(lst(n_total))
}
