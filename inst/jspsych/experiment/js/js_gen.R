source("~/Dropbox (Sydney Uni)/Thesis/R/thesisR/src/packages.R")
source("~/Dropbox (Sydney Uni)/Thesis/R/thesisR/src/project_gen/project_gen_anecdotes.R")
options(scipen = 999)

## output setup
vars <- list(img_preload = str_c("img/", list.files(here("img"))),
             conditions = conditions,
             instructions = instructions) %>% 
  append(main_task) %>% 
  append(general)

getjs(vars, filename = here("js","input.js"))
