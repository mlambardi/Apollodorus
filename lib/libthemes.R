load_sf_items <- function(update = F) {
  readRDS("lib/items.rds")
}

menu_theme_item_select <- function(tree, select) {
  for (s in select) {
    tree <- tree[[s]]
  }
  tree
}

menu_theme_items_make <- function(df) {
  menu_from_df <- function(df, var) {
    if (nrow(df) > 1) {
      if (length(unique(df[[var[1]]])) < nrow(df)) {
        df %>%
          split(df[[var[1]]]) %>%
          lapply(menu_from_df, var = var[-1])
      } else {
        setNames(df$theme_item, df[[var[1]]])
      }
    } else {
      df$theme_item
    }
  }
  df %>%
    as.data.frame() %>%
    filter(theme != "ground") %>%
    summarize(.by = c(theme, item, theme_item)) %>%
    mutate(
      v = as.integer(stringr::str_extract(item, "\\d+$")),
      item = gsub("\\d+$", "", item)
    ) %>%
    arrange(theme, item, v) %>%
    menu_from_df(var = c("theme", "item", "v"))
}

menu_length_max <- function(df) {
  maxaux <- function(x) {
    max(length(x), if (is.list(x)) sapply(x, maxaux))
  }
  maxaux(df)
}

menu_depth_max <- function(df) {
  maxaux <- function(x, init) {
    max(init, if (is.list(x)) sapply(x, maxaux, init = init + 1))
  }
  maxaux(df, init = 1)
}

menu_find_path <- function(df, theme_item) {
  unlist(df) %>%
    {
      names(.)[. == theme_item]
    } %>%
    strsplit(split = "\\.") %>%
    unlist()
}

item_thumbnail <- function(theme_item, www = T) {
  paste0(if (www) "www/", "item_previews/", theme_item, ".svg")
}
