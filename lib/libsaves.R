require(dplyr)
require(shiny)
require(ggplot2)

game_thumbnail <- function(www) paste0(if (www) "www/", "saves/save.svg")
game_savefile <- "saves/save.rds"

game_save <- function(state) {
  state$mapplot %>%
    ggsave(filename = game_thumbnail(www = T))
  state %>%
    reactiveValuesToList() %>%
    .[c("zoom", "items", "hover", "sel")] %>%
    saveRDS(file = game_savefile)
}

game_load <- function(state) {
  for (n in names(state)) {
    state[[n]] <- NULL
  }
  aux <- readRDS(game_savefile)
  for (n in names(aux)) {
    if (n %in% c("zoom", "items", "hover", "sel")) {
      state[[n]] <- aux[[n]]
    }
  }
  state
}
