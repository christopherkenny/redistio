#' Plans Browser Module
#'
#' @param id module id
#' @param opts options
#' @param def_opts default options
#'
#' @return shiny UI
#' @noRd
plansUI <- function(id, opts, def_opts) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'plans',
    bslib::layout_columns(
      col_widths = c(4, 6, 2),
      bslib::card(
        DT::DTOutput(outputId = ns('plans_table'), height = '80vh')
      ),
      bslib::card(
        mapgl::maplibreOutput(
          outputId = ns('plans_map'),
          height = opts$leaflet_height %||% def_opts$leaflet_height
        )
      ),
      bslib::card(
        shiny::h4('Adopt a plan'),
        shiny::p('Select a plan from the table to preview it. Click "Adopt plan" to use it.'),
        shiny::actionButton(
          inputId = ns('adopt_plan'),
          label = 'Adopt plan',
          icon = shiny::icon('file-import')
        )
      )
    )
  )
}

#' Plans Browser Module Server
#'
#' @param id module id
#' @param parent_session parent session
#' @param plans_reactive reactiveVal containing redist_plans object
#' @param shp sf object
#' @param shp_in sf object (redist_map)
#' @param redistio_curr_plan reactive plan
#' @param ndists number of districts
#' @param pal palette reactive (discrete district colors, i.e. palette_reactive)
#' @param fill_opacity fill opacity
#' @param precinct_border precinct border weight
#' @param precinct_linecolor precinct line color
#' @param leaf_tiles map tiles style
#' @param layers overlay layers
#' @param layer_colors layer colors
#' @param opts options
#' @param def_opts default options
#' @param val reactive value for population table
#' @param tot_pop total population
#' @param tgt_pop target population
#' @param pop_col population column name
#' @param undo_l undo log
#'
#' @return shiny server
#' @noRd
plansServer <- function(id, parent_session,
                        plans_reactive, shp, shp_in,
                        redistio_curr_plan, ndists, pal,
                        fill_opacity, precinct_border, precinct_linecolor,
                        leaf_tiles, layers, layer_colors, opts, def_opts,
                        val, tot_pop, tgt_pop, pop_col, undo_l) {
  shiny::moduleServer(id, function(input, output, session) {

    # store the selected plan vector for map preview
    selected_plan <- shiny::reactiveVal(NULL)

    plans_summary <- shiny::reactive({
      pl <- plans_reactive()
      shiny::req(pl)

      pl_tb <- tibble::as_tibble(pl)

      # identify plan-level columns (constant within each draw)
      skip_cols <- c('draw', 'district')
      candidate_cols <- setdiff(names(pl_tb), skip_cols)

      plan_level_cols <- vapply(candidate_cols, function(col) {
        if (!is.numeric(pl_tb[[col]]) && !is.character(pl_tb[[col]]) && !is.factor(pl_tb[[col]])) {
          return(FALSE)
        }
        grouped <- split(pl_tb[[col]], pl_tb$draw)
        all(vapply(grouped, function(x) length(unique(x)) == 1L, logical(1)))
      }, logical(1))

      plan_cols <- names(plan_level_cols)[plan_level_cols]

      summary_tb <- pl_tb |>
        dplyr::group_by(.data$draw) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(plan_cols), dplyr::first),
          .groups = 'drop'
        )

      # convert chain to integer if present
      if ('chain' %in% names(summary_tb) && is.numeric(summary_tb$chain)) {
        summary_tb$chain <- as.integer(summary_tb$chain)
      }

      summary_tb
    })

    # render the preview map (shows current plan initially)
    output$plans_map <- mapgl::renderMaplibre({
      district_centroids <- shp |>
        dplyr::mutate(district = redistio_curr_plan$pl) |>
        dplyr::filter(!is.na(.data$district)) |>
        dplyr::group_by(.data$district) |>
        dplyr::summarise(geometry = sf::st_union(.data$geometry)) |>
        sf::st_centroid() |>
        dplyr::mutate(label = as.character(.data$district)) |>
        suppressWarnings()

      map_plans <- mapgl::maplibre(
        style = leaf_tiles,
        bounds = shp
      ) |>
        mapgl::add_source(
          id = 'redistio',
          data = create_mapgl_source(shp),
          promoteId = 'redistio_id'
        ) |>
        mapgl::add_fill_layer(
          source = 'redistio',
          id = 'precinct_fill',
          fill_color = discrete_palette(pal(), redistio_curr_plan$pl, column = 'redistio_id'),
          fill_opacity = fill_opacity,
          fill_outline_color = '#cccccc'
        ) |>
        mapgl::add_source(
          id = 'district_labels',
          data = district_centroids
        ) |>
        mapgl::add_symbol_layer(
          id = 'district_label_layer',
          source = 'district_labels',
          text_field = list('get', 'label'),
          text_size = 16,
          text_color = '#000000',
          text_halo_color = '#FFFFFF',
          text_halo_width = 2
        )

      if (!is.null(layers)) {
        for (i in seq_along(layers)) {
          map_plans <- map_plans |>
            mapgl::add_line_layer(
              source = layers[[i]],
              line_width = opts$layer_weight %||% def_opts$layer_weight,
              line_color = layer_colors[i],
              id = names(layers)[i]
            )
          map_plans <- map_plans |>
            mapgl::add_layers_control(
              layers = names(layers),
              collapsible = TRUE
            )
        }
      }

      map_plans
    })

    output$plans_table <- DT::renderDT(
      {
        shiny::isolate(plans_summary()) |>
          DT::datatable(
            options = list(
              dom = 'tip',
              scrollY = '70vh',
              scrollX = TRUE,
              pageLength = 100
            ),
            style = 'bootstrap',
            rownames = FALSE,
            escape = FALSE,
            selection = list(target = 'row', mode = 'single', selected = 1),
            fillContainer = TRUE
          ) |>
          DT::formatRound(
            columns = which(vapply(plans_summary(), is.numeric, logical(1))),
            digits = 3
          )
      },
      server = TRUE
    )

    dt_plans_proxy <- DT::dataTableProxy('plans_table')

    shiny::observe({
      DT::replaceData(
        proxy = dt_plans_proxy, plans_summary(), rownames = FALSE,
        resetPaging = FALSE, clearSelection = 'none'
      )
    })

    # helper to get the plan column index from a draw label
    get_plan_col <- function(pl, selected_draw) {
      draw_levels <- levels(tibble::as_tibble(pl)$draw)
      if (is.null(draw_levels)) {
        draw_levels <- unique(tibble::as_tibble(pl)$draw)
      }
      col_idx <- which(draw_levels == selected_draw)
      if (length(col_idx) == 0L) {
        col_idx <- as.integer(selected_draw)
      }
      col_idx
    }

    # preview selected plan on the map
    shiny::observeEvent(input$plans_table_rows_selected, {
      shiny::req(input$plans_table_rows_selected)

      pl <- plans_reactive()
      shiny::req(pl)
      plans_mat <- redist::get_plans_matrix(pl)

      selected_draw <- plans_summary()$draw[input$plans_table_rows_selected]
      col_idx <- get_plan_col(pl, selected_draw)
      plan_vec <- as.integer(plans_mat[, col_idx])

      selected_plan(plan_vec)

      mapgl::maplibre_proxy('plans_map') |>
        mapgl::set_paint_property(
          layer_id = 'precinct_fill',
          name = 'fill-color',
          value = discrete_palette(pal(), plan_vec)
        )
    })

    # adopt the selected plan
    shiny::observeEvent(input$adopt_plan, {
      shiny::req(input$plans_table_rows_selected)

      new_plan <- selected_plan()
      shiny::req(new_plan)

      redistio_curr_plan$pl <- new_plan

      undo_l(undo_log(undo_l(), redistio_curr_plan$pl))

      mapgl::maplibre_proxy('map', session = parent_session) |>
        update_shape_style(
          'District', pal(), redistio_curr_plan$pl, shp,
          fill_opacity, precinct_border, precinct_linecolor
        )

      shiny::updateTabsetPanel(parent_session, 'navbar', 'draw')

      new_tb_pop <- val()
      new_tb_pop$Population <- distr_pop(
        shp[[pop_col]], total = tot_pop,
        plan = redistio_curr_plan$pl, ndists = ndists
      )
      new_tb_pop$Deviation <- as.integer(
        new_tb_pop$Population - c(0L, rep(tgt_pop, ndists))
      )
      val(new_tb_pop)
    })
  })
}
