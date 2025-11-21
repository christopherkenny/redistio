#' Interactive Adjacency Graph Editing
#'
#' @param shp an `sf` tibble that you want to draw with
#' @param adj a zero-indexed adjacency graph
#' @param init_plan plan to initialize coloring
#' @param palette Color palette to fill shapes with. Default is `Polychrome 36` or,
#' if installed, `crayons::crayons$no_48`.
#' @param layers Named list of `sf` objects to overlay. Also takes column names in `shp` to group by.
#' @param hover_fn Function to generate tables for mouse hovering. Default is `hover_precinct()`.
#' @param opts list of options. Default is `redistio_options()`
#'
#' @return Shiny app
#' @export
#'
#' @examples
#' if (interactive()) {
#'   adj_editor(dc, init_plan = dc$ward)
#'   adj_editor(dc, init_plan = dc$ward, layers = list(neighborhoods = 'adv_nbr'))
#' }
adj_editor <- function(
    shp,
    adj = geomander::adjacency(shp),
    init_plan,
    palette = NULL,
    layers = NULL,
    hover_fn = hover_precinct,
    opts = redistio_options()
) {
  # defaults ----
  def_opts <- redistio_options()

  # run basic inputs ----
  if (missing(shp)) {
    stop('`shp` missing, but required.')
  }

  shp$redistio_id <- as.character(seq_len(length.out = nrow(shp)))

  # process shp components ----
  shp <- prep_shp(shp, crs = opts$crs %||% def_opts$crs)$no_list_cols
  edges_centers <- edge_center_df(shp, adj)
  edge_tracker <- init_edge_tracker(edges_centers$nb)

  # prep hover ----
  shp_tb <- shp |>
    tibble::as_tibble()

  hov <- hover_fn(
    shp_tb,
    pop = dplyr::starts_with('pop'),
    vap = dplyr::starts_with('vap')
  ) |>
    dplyr::bind_rows(.id = 'group') |>
    format_alarm_names()

  # handle colors ----
  if (missing(init_plan)) {
    init_plan <- rep(NA_integer_, nrow(shp))
  }
  if (is.character(init_plan)) {
    # TODO: think through allowing non 1, 2, ..., ndists inits
    init_plan <- as.integer(init_plan)
  }
  palette <- prep_palette(palette, length(unique(init_plan)))

  # prep layer colors ----
  if (!is.null(layers)) {
    layer_colors <- opts$layer_color %||% def_opts$layer_color
    if (length(layer_colors) == 1L) {
      layer_colors <- rep(layer_colors, length(layers))[seq_along(layers)]
    }
    layers <- prep_layers(layers, shp)
  }

  # init log ----
  log <- log_adj()

  # User Interface ----
  leaf_tiles <- opts$map_tiles %||% def_opts$map_tiles

  ui <- bslib::page_navbar(
    title = 'redistio',
    theme = bslib::bs_theme(preset = (opts$theme %||% def_opts$theme)),
    id = 'navbar',
    # editor panel ----
    bslib::nav_panel(
      title = 'adj editor',
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::accordion(
            bslib::accordion_panel(
              'Adjacency Editor',
              shiny::radioButtons(
                inputId = 'edge_mode',
                label = 'Edge Mode',
                choices = c('Add Edge' = 'add', 'Remove Edge' = 'remove'),
                selected = 'add'
              ),
              shiny::textInput(
                inputId = 'edit_comment',
                label = 'Comment (optional)',
                placeholder = 'e.g., River, County line, etc.'
              ),
              shiny::hr(),
              shiny::htmlOutput('selection_status'),
              shiny::actionButton(
                inputId = 'clear_selection',
                label = 'Clear Selection',
                width = '100%',
                class = 'btn-secondary'
              )
            ),
            bslib::accordion_panel(
              'Fill',
              shiny::sliderInput(
                inputId = 'fill_opacity',
                label = 'Fill opacity',
                min = 0,
                max = 1,
                step = 0.05,
                value = 0.2
              ),
              shiny::numericInput(
                inputId = 'precinct_border',
                label = 'Precinct border weight',
                min = 0,
                max = 100,
                value = 0.1
              ),
              colourpicker::colourInput(
                inputId = 'precinct_linecolor',
                label = 'Precinct border color',
                value = opts$border_color %||% def_opts$border_color
              ),
              icon = shiny::icon('palette')
            )
          )
        ),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            position = 'right',
            bslib::navset_bar(
              id = 'tabRight',
              bslib::nav_panel(title = 'Precinct', gt::gt_output('hover'))
            )
          ),
          bslib::card(
            # interactive mapper
            id = 'map-card',
            full_screen = TRUE,
            mapgl::maplibreOutput(
              outputId = 'map',
              height = opts$leaflet_height %||% def_opts$leaflet_height,
            )
          )
        )
      )
    ),
    # export panel ----
    bslib::nav_panel(
      title = 'Export',
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header('Edit Log'),
          full_screen = TRUE,
          gt::gt_output('edit_log_table')
        ),
        bslib::card(
          bslib::card_header('Code'),
          full_screen = TRUE,
          shiny::verbatimTextOutput('edit_code')
        )
      )
    )
  )

  # Server ----
  server <- function(input, output, session) {
    redistio_curr_plan <- shiny::reactiveValues(pl = init_plan)

    # Adjacency editing state
    adj_state <- shiny::reactiveValues(
      selected = character(0),
      tracker = edge_tracker,
      log = log
    )

    output$map <- mapgl::renderMaplibre({
      base_map <- mapgl::maplibre(
        bounds = shp,
        style = leaf_tiles
      ) |>
        mapgl::add_source(
          id = 'redistio',
          data = shp,
          promoteId = 'redistio_id'
        ) |>
        mapgl::add_source(
          id = 'lines',
          data = edges_centers$nb,
          promoteId = 'line_id'
        ) |>
        mapgl::add_fill_layer(
          source = 'redistio',
          id = 'precinct_fill',
          fill_color = discrete_palette(palette, init_plan),
          fill_opacity = 0.2,
          fill_outline_color = '#00000000'
        ) |>
        mapgl::add_line_layer(
          id = 'precinct_border',
          source = 'redistio',
          line_color = opts$border_color %||% def_opts$border_color,
          line_width = 0.1
        ) |>
        mapgl::add_line_layer(
          id = 'edges',
          source = 'lines',
          line_color = opts$border_color %||% def_opts$border_color,
          line_width = 1
        ) |>
        mapgl::set_projection(opts$projection %||% def_opts$projection)

      if (!is.null(layers)) {
        for (i in seq_along(layers)) {
          base_map <- base_map |>
            mapgl::add_line_layer(
              source = layers[[i]],
              id = names(layers)[i],
              line_width = opts$layer_weight %||% def_opts$layer_weight,
              line_color = layer_colors[i]
            )
        }
        base_map <- base_map |>
          mapgl::add_layers_control(
            layers = names(layers),
            collapsible = TRUE
          )
      }

      if (!is.null(hover_fn)) {
        base_map <- base_map |>
          mapgl::enable_shiny_hover(
            coordinates = FALSE,
            features = 'precinct_fill',
            layer_id = 'precinct_fill'
          )
      }

      base_map
    })

    # Track clicks reactively
    click_reac <- shiny::reactive({
      input$map_feature_click
    })

    # Handle clicks for adjacency editing
    shiny::observeEvent(
      click_reac(),
      {
        click_data <- click_reac()
        if (!is.null(click_data) && !is.null(click_data$id) & click_data$layer == 'precinct_fill') {
          clicked_id_char <- as.character(click_data$id)
          clicked_id <- as.integer(click_data$id)

          if (clicked_id_char %in% adj_state$selected) {
            # Deselect if already selected
            adj_state$selected <- setdiff(adj_state$selected, clicked_id_char)
          } else if (length(adj_state$selected) < 2) {
            # Add to selection if less than 2 selected
            adj_state$selected <- c(adj_state$selected, clicked_id_char)
          }

          # If two precincts selected, modify adjacency
          if (length(adj_state$selected) == 2) {

            state <- check_edge_state(
              adj_state$tracker,
              min(as.integer(adj_state$selected)),
              max(as.integer(adj_state$selected))
            )

            if (input$edge_mode == 'add') {
              # Add edge to adjacency list
              adj_state$tracker <- add_edge_to_tracker(
                adj_state$tracker,
                min(as.integer(adj_state$selected)),
                max(as.integer(adj_state$selected))
              )

              if (!state$exists || (isFALSE(state$original) && isFALSE(state$shown))) {
                mapgl::maplibre_proxy('map') |>
                  mapgl::add_line_layer(
                    id = paste0(sort(adj_state$selected), collapse = '-'),
                    source = new_single_edge(
                      edges_centers$centers,
                      min(as.integer(adj_state$selected)),
                      max(as.integer(adj_state$selected))
                    )
                  )
              } else if (isFALSE(state$shown) && isTRUE(state$original)) {
                # then we have to fix the filter
                current_edges <- get_current_edge_ids(adj_state$tracker)
                mapgl::maplibre_proxy('map') |>
                  mapgl::set_filter('edges', list('match', mapgl::get_column('line_id'), as.list(current_edges), TRUE, FALSE))
              }

              adj_state$log <- log_adj_update(adj_state$log, act = '+', p = sort(as.integer(adj_state$selected)), comment = input$edit_comment)
            } else {
              # Remove edge from adjacency list

              adj_state$tracker <- remove_edge_from_tracker(
                adj_state$tracker,
                min(as.integer(adj_state$selected)),
                max(as.integer(adj_state$selected))
              )

              if (state$exists) {
                if (isFALSE(state$original) && isTRUE(state$shown)) {
                  mapgl::maplibre_proxy('map') |>
                    mapgl::clear_layer(
                      layer_id = paste0(sort(adj_state$selected), collapse = '-')
                    )
                } else if (isTRUE(state$original) && isTRUE(state$shown)) {
                  current_edges <- get_current_edge_ids(adj_state$tracker)
                  mapgl::maplibre_proxy('map') |>
                    mapgl::set_filter('edges', list('match', mapgl::get_column('line_id'), as.list(current_edges), TRUE, FALSE))
                }
              }

              adj_state$log <- log_adj_update(adj_state$log, act = '-', p = sort(as.integer(adj_state$selected)), comment = input$edit_comment)
            }
            # Clear selection after operation
            adj_state$selected <- character(0)
          }
        }
      },
      ignoreInit = TRUE
    )

    # Clear selection button
    shiny::observeEvent(input$clear_selection, {
      adj_state$selected <- character(0)
    })

    # Selection status display
    output$selection_status <- shiny::renderUI({
      selected <- adj_state$selected
      if (length(selected) == 0) {
        shiny::HTML('<p style="color: #666;">No precincts selected</p>')
      } else if (length(selected) == 1) {
        shiny::HTML(paste0(
          '<p><strong>Selected:</strong> Precinct ', selected[1], '</p>',
          '<p style="color: #666;">Click another precinct to ',
          input$edge_mode, ' edge</p>'
        ))
      } else {
        shiny::HTML(paste0(
          '<p><strong>Selected:</strong> Precincts ',
          paste(selected, collapse = ', '), '</p>'
        ))
      }
    })

    # reactive mouseover
    hov_reac <- shiny::reactive({
      input$map_feature_hover
    })
    hov_reac_d <- shiny::debounce(hov_reac, opts$debounce %||% def_opts$debounce)

    # precinct stats ----
    shiny::observeEvent(hov_reac_d(), {
      if (!is.null(hov_reac_d())) {
        output$hover <- gt::render_gt({
          hov |>
            dplyr::select(dplyr::any_of(c(
              'group',
              'rowname',
              paste0('V', as.integer(hov_reac_d()$id))
            ))) |>
            gt::gt() |>
            gt::cols_label_with(
              columns = gt::starts_with('V'),
              fn = function(x) ''
            ) |>
            gt::tab_style(
              style = list(
                gt::cell_text(align = 'left')
              ),
              locations = gt::cells_stub(rows = TRUE)
            ) |>
            gt::tab_header(
              title = paste0(
                'Current District: ',
                redistio_curr_plan$pl[as.integer(hov_reac_d()$id)]
              ),
              subtitle = paste0('Precinct ID: ', hov_reac_d()$id)
            ) |>
            gt::tab_options(
              data_row.padding = gt::px(1),
              table.width = '100%',
              container.padding.y = '0',
              column_labels.padding = '0',
              table.background.color = '#fff0'
            ) |>
            gt::fmt_number(columns = gt::starts_with('V'), decimals = 0)
        })
      }
    })

    shiny::observeEvent(
      list(input$fill_opacity, input$precinct_border, input$precinct_linecolor),
      {
        mapgl::maplibre_proxy('map') |>
          mapgl::set_paint_property(
            layer_id = 'precinct_fill',
            name = 'fill-opacity',
            value = input$fill_opacity
          ) |>
          mapgl::set_paint_property(
            layer_id = 'precinct_border',
            name = 'line-color',
            value = input$precinct_linecolor
          ) |>
          mapgl::set_paint_property(
            layer_id = 'precinct_border',
            name = 'line-width',
            value = input$precinct_border
          )
      }
    )

    # Export outputs ----
    output$edit_log_table <- gt::render_gt({
      log_data <- adj_state$log
      non_empty <- which(log_data$action != '')

      if (length(non_empty) == 0) {
        return(
          data.frame(
            Action = character(0),
            i = integer(0),
            j = integer(0),
            Comment = character(0)
          ) |>
            gt::gt() |>
            gt::tab_header(title = 'No edits yet')
        )
      }

      data.frame(
        Action = log_data$action[non_empty],
        i = log_data$pair[non_empty, 1],
        j = log_data$pair[non_empty, 2],
        Comment = log_data$comment[non_empty]
      ) |>
        gt::gt() |>
        gt::tab_header(title = 'Adjacency Edits') |>
        gt::tab_options(
          table.width = '100%',
          container.padding.y = '10px'
        )
    })

    output$edit_code <- shiny::renderText({
      log_data <- adj_state$log
      non_empty <- which(log_data$action != '')

      if (length(non_empty) == 0) {
        return('adj')
      }

      # prep edits
      actions <- log_data$action[non_empty]
      a_vals <- log_data$pair[non_empty, 1]
      b_vals <- log_data$pair[non_empty, 2]
      comments <- log_data$comment[non_empty]

      # replace +/- with fn
      fn_names <- ifelse(actions == '+', 'add_edge', 'subtract_edge')
      fn_calls <- paste0('  ', fn_names, '(', a_vals, ', ', b_vals, ')')

      # add pipes, but not to last line
      pipes <- c(rep(' |>', length(fn_calls) - 1), '')

      # add comments
      comment_parts <- ifelse(nzchar(comments), paste0(' # ', comments), '')

      # combine all parts
      code_lines <- paste0(fn_calls, pipes, comment_parts)
      paste(c('library(geomander)', 'adj |>', code_lines), collapse = '\n')
    })
  }
  # run app ----
  shiny::shinyApp(ui = ui, server = server)
}
