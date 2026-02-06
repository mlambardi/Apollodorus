require(shiny)
require(bslib)

require(dplyr)
require(sf)

require(ggplot2)

theme_set(
  theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect("transparent"),
      panel.background = element_rect("transparent")
    )
)

source("lib/all.R", chdir = T)

# source("../source_svg/lib.R")
# update_sf_items()

get_new_id <- function(v) min(setdiff(c(0, v) + 1, v))
item_models <- load_sf_items()
hex_ground <- item_models %>%
  filter(theme == "ground", part != "ground")
item_models <- item_models %>%
  filter(theme != "ground")

menu_themes_items <- item_models %>%
  menu_theme_items_make()

ui <- page_fillable(
  tags$style(
    type = 'text/css',
    ".modal-dialog { width: fit-content !important; }
    .recalculating { opacity: 1.0; }
    .card { border: none; }"
  ),
  uiOutput("cursor_css"),
  theme = bs_theme(
    bg = "#666666",
    fg = "#cccccc",
    primary = "#e8aa7d",
    secondary = "#de8c54",
    base_font = font_google("Caesar Dressing")
  ),
  layout_column_wrap(
    width = NULL,
    fill = FALSE,
    style = css(grid_template_columns = "1fr 2fr"),
    card(
      actionButton("menugame", "", icon = icon("file"), width = "5em"),
      popover(
        actionButton("modeadd", "", icon = icon("add")),
        uiOutput("buildingpicker"),
        plotOutput("buildingpreview"),
        placement = "bottom"
      ),
      actionButton("moderemove", "", icon = icon("trash"))
    ),
    card(
      class = "align-items-center",
      fill = F,
      plotOutput(
        "plot",
        click = "click",
        hover = "hover",
        brush = brushOpts(
          id = "brush",
          fill = "transparent", # Custom fill color
          stroke = "#000000" # Custom outline color
        ),
        width = "700px",
        height = "700px"
      ),
      # https://jokergoo.github.io/2021/02/20/differentiate-brush-and-click-event-in-shiny/
      # see input$action
      tags$script(HTML(script_click_brush))
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    mode = "idle",
    items = data.frame(
      id = integer(0),
      x = numeric(0),
      y = numeric(0),
      theme_item = character(0)
    ),
    zoom = list(
      x = c(-1, +1),
      y = c(-0.5, +1.5)
    ),
    hover = data.frame(x = as.numeric(NA), y = as.numeric(NA)),
    sel = integer(0)
  )

  output$cursor_css <- renderUI({
    tags$style(paste0(
      "body, body * { cursor: url('./icons/",
      state$mode,
      ".svg'), auto !important; }"
    ))
  })

  observeEvent(input$menugame, {
    showModal(modalDialog(
      tags$table(
        tags$tr(
          tags$td(
            img(src = game_thumbnail(www = F), width = "200px")
          ),
          tags$td(
            actionButton("savegame", label = icon("download"))
          ),
          tags$td(
            actionButton("loadgame", label = icon("upload"))
          )
        )
      ),
      easyClose = T,
      footer = NULL
    ))
  })

  observeEvent(input$savegame, {
    removeModal()
    game_save(state)
  })

  observeEvent(input$loadgame, {
    removeModal()
    state <- game_load(state)
    state$mode <- "idle"
  })

  output$buildingpicker <- renderUI({
    pickedcat <- state$pickedcat
    tagList(
      span(
        lapply(0:length(pickedcat), \(i) {
          list(
            "/",
            actionLink(
              paste0("pickbackward", i),
              label = c("all", pickedcat)[i + 1]
            )
          )
        })
      ),
      menu_theme_item_select(
        menu_themes_items,
        select = pickedcat
      ) %>%
        tibble(name = names(.), value = .) %>%
        mutate(id = seq_len(n())) %>%
        split(., seq_len(nrow(.))) %>%
        lapply(\(x) actionLink(paste0("picktheme", x$id), x$name)) %>%
        p()
    )
  })

  lapply(1:menu_length_max(menu_themes_items), \(i) {
    observeEvent(input[[paste0("picktheme", i)]], {
      pickedcat <- state$pickedcat
      mylist <- menu_theme_item_select(menu_themes_items, pickedcat)
      if (is.list(mylist)) {
        state$pickedcat <- c(pickedcat, names(mylist)[i])
      } else {
        state$picked <- mylist[i]
      }
    })
  })

  lapply(0:menu_depth_max(menu_themes_items), \(i) {
    observeEvent(
      input[[paste0("pickbackward", i)]],
      state$pickedcat <- head(state$pickedcat, i)
    )
  })

  output$buildingpreview <- renderPlot({
    req(state$picked)
    item_models %>%
      group_by(theme, sub("\\d+$", "", item)) %>%
      filter(any(theme_item == state$picked)) %>%
      mutate(part = ifelse(theme_item == state$picked, part, "transparent")) %>%
      ggplot(aes(fill = part)) +
      scale_fill_manual(values = part_colors) +
      geom_sf(linewidth = 0)
  })

  observeEvent(input$modeadd, {
    state$mode <- "add"
    state$sel <- integer(0)
  })

  observeEvent(input$moderemove, {
    state$mode <- "remove"
    state$hover <- state$hover %>%
      mutate(x = as.numeric(NA), y = as.numeric(NA))
  })

  observeEvent(input$brush, {
    brush <- input$brush
    session$resetBrush("brush")
    x <- unlist(brush[c("xmin", "xmax")])
    y <- unlist(brush[c("ymin", "ymax")])
    d <- max(diff(x), diff(y)) * 2
    d <- max(d, 0.2)
    state$zoom <- list(
      x = mean(x) + c(-1, +1) * d / 2,
      y = mean(y) + c(-1, +1) * d / 2
    )
  })

  observeEvent(input$hover, {
    switch(
      state$mode,
      add = {
        state$hover <- input$hover[c("x", "y")] %>%
          as.data.frame()
      },
      remove = {
        state$sel <- item_models %>%
          merge(y = state$items, by = "theme_item") %>%
          mutate(geometry = geometry + c(x[1], y[1]), .by = c(x, y)) %>%
          st_intersection(
            y = input$hover[c("x", "y")] %>%
              as.data.frame() %>%
              st_as_sf(coords = c("x", "y"))
          ) %>%
          arrange(startsWith(part, "ground"), desc(y)) %>%
          pull(id) %>%
          head(1)
      },
      idle = {
        writeLines("nothing to do")
      }
    )
  })

  observeEvent(input$action, {
    if (!(input$x1 == input$x2 && input$y1 == input$y2)) {
      return(NULL)
    }
    state$items <- switch(
      state$mode,
      add = select(state$preview, id, x, y, theme_item),
      remove = filter(state$items, !(id %in% state$sel))
    )
  })

  observe(tryCatch(
    state$preview <- state$hover %>%
      mutate(
        theme_item = state$picked,
        add = T,
        id = get_new_id(state$items$id)
      ) %>%
      filter(state$mode == "add") %>%
      na.omit() %>%
      bind_rows(mutate(state$items, add = F)) %>%
      arrange(desc(y)) %>%
      mutate(layer = seq_len(n())),
    error = \(e) NULL
  ))

  observe({
    tryCatch(
      state$map <- state$preview %>%
        merge(x = item_models, by = "theme_item") %>%
        mutate(geometry = geometry + c(x[1], y[1]), .by = c(x, y)) %>%
        arrange(!startsWith(part, "ground"), layer, part_index),
      error = \(e) NULL
    )
  })

  observe({
    state$mapplot <- tryCatch(
      state$map %>%
        filter(part != "ground") %>%
        ggplot(aes(fill = part)) +
        scale_fill_manual(values = part_colors) +
        geom_sf(data = hex_ground) +
        geom_sf(aes(linewidth = id %in% state$sel, alpha = !add), na.rm = T) +
        expand_limits(alpha = c(T, F), linewidth = c(T, F)) +
        scale_alpha(range = c(0.5, 1)) +
        scale_linewidth(range = c(0, 1)),
      error = \(e) ggplot()
    )
  })

  output$plot <- renderPlot({
    state$mapplot +
      coord_sf(xlim = state$zoom$x, ylim = state$zoom$y) +
      theme(panel.background = element_rect("#333333"))
  })
}

shinyApp(ui = ui, server = server)
