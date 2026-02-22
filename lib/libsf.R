make_sf <- function(df, models) {
  df %>%
    merge(y = models, by = "theme_item") %>%
    sf::st_as_sf() %>%
    mutate(geometry = geometry + c(x[1], y[1]), .by = c(x, y))
}

clicked_sf <- function(df, click) {
  click[c("x", "y")] %>%
    as.data.frame() %>%
    st_as_sf(coords = c("x", "y"), na.fail = F) %>%
    st_intersection(x = df)
}

eval_zoom <- function(brush) {
  x <- unlist(brush[c("xmin", "xmax")])
  y <- unlist(brush[c("ymin", "ymax")])
  d <- max(diff(x), diff(y)) * 2
  d <- max(d, 0.2)
  list(
    x = mean(x) + c(-1, +1) * d / 2,
    y = mean(y) + c(-1, +1) * d / 2
  )
}
