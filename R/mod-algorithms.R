#' Algorithms Module
#'
#' @param id module id
#' @param opts options
#' @param def_opts default options
#'
#' @return shiny UI
#' @noRd
algorithmsUI <- function(id, opts, def_opts, ndists, shp) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'algorithms',
    bslib::layout_columns(
      col_widths = c(2, 8, 2),
      bslib::card( # selector
        shiny::selectizeInput(
          inputId = ns('alg_district'),
          label = paste0('Districts to redraw (up to ', min(opts$alg_max_districts %||% def_opts$alg_max_districts, ndists), ')'),
          choices = seq_len(ndists),
          options = list(maxItems = min(opts$alg_max_districts %||% def_opts$alg_max_districts, ndists))
        ),
        shiny::hr(),
        shiny::selectInput(
          inputId = ns('alg_algorithm'),
          label = 'Algorithm to use',
          choices = c('SMC', 'Merge Split', 'Flip')
        ),
        shiny::hr(),
        shiny::sliderInput(
          inputId = ns('alg_nsims'),
          label = 'Number of simulations',
          min = 1,
          max = (opts$alg_max_sims %||% def_opts$alg_max_sims),
          value = 10
        ),
        shiny::hr(),
        shiny::selectizeInput(ns('alg_counties_id'), 'Select county column:',
          choices = c('NONE', names(shp)), selected = NULL,
          multiple = FALSE
        ),
        shiny::hr(),
        shiny::actionButton(
          inputId = ns('alg_run'),
          label = 'Run algorithm',
          icon = shiny::icon('circle-play')
        )
      ),
      bslib::card( # interactive mapper
        mapgl::maplibreOutput(
          outputId = ns('alg_map'),
          height = opts$leaflet_height %||% def_opts$leaflet_height,
        )
      ),
      bslib::card( # details area
        DT::DTOutput(outputId = ns('alg_summary'), width = '30vh', height = '80vh'),
        shiny::tags$hr(),
        shiny::actionButton(
          inputId = ns('alg_accept'),
          label = 'Accept plan',
          icon = shiny::icon('file-export')
        ),
      )
    )
  )
}

#' Algorithms Module Server
#'
#' @param id module id
#' @param parent_session parent session
#' @param shp sf object
#' @param shp_in sf object
#' @param redistio_curr_plan reactive plan
#' @param ndists number of districts
#' @param pal palette
#' @param fill_opacity fill opacity
#' @param precinct_border precinct border
#' @param precinct_linecolor precinct line color
#' @param fill_column fill column
#' @param leaf_tiles leaflet tiles
#' @param layers layers
#' @param layer_colors layer colors
#' @param opts options
#' @param def_opts default options
#' @param val reactive value
#' @param tot_pop total population
#' @param tgt_pop target population
#' @param pop_col population column
#' @param undo_l undo log
#'
#' @return shiny server
#' @noRd
algorithmsServer <- function(id, parent_session,
                             shp, shp_in, redistio_curr_plan, ndists, pal,
                             fill_opacity, precinct_border, precinct_linecolor,
                             fill_column,
                             leaf_tiles, layers, layer_colors, opts, def_opts,
                             val, tot_pop, tgt_pop, pop_col, undo_l) {
  shiny::moduleServer(id, function(input, output, session) {
    map_sub <- shiny::reactiveVal(shp)
    map_sub_in <- shiny::reactiveVal(shp_in)
    redistio_alg_plan <- shiny::reactiveValues(pl = NULL, plans = NULL)
    alg_plans_static <- tibble::tibble(
      draw = factor(),
      dev = double()
    )
    alg_plans <- shiny::reactiveVal(alg_plans_static)
    base_id <- 1e6

    output$alg_map <- mapgl::renderMaplibre({
      map_alg <- mapgl::maplibre(
        style = leaf_tiles,
        bounds = shp
      ) |>
        mapgl::add_source(
          id = 'redistio',
          data = shp,
          promoteId = 'redistio_id'
        ) |>
        mapgl::add_fill_layer(
          source = 'redistio',
          id = 'precinct_fill',
          fill_color = discrete_palette(pal(), redistio_curr_plan$pl, column = 'redistio_id'),
          fill_opacity = fill_opacity,
          fill_outline_color = '#cccccc'
        )

      if (!is.null(layers)) {
        for (i in seq_along(layers)) {
          map_alg <- map_alg |>
            mapgl::add_line_layer(
              source = layers[[i]],
              line_width = opts$layer_weight %||% def_opts$layer_weight,
              line_color = layer_colors[i],
              id = names(layers)[i]
            )
          map_alg <- map_alg |>
            mapgl::add_layers_control(
              layers = names(layers),
              collapsible = TRUE
            )
        }
      }

      map_alg
    })

    shiny::outputOptions(output, 'alg_map')

    shiny::observeEvent(input$alg_run, {
      map_sub(shp |>
        dplyr::mutate(redistio_plan = redistio_curr_plan$pl) |>
        `attr<-`('existing_col', 'redistio_plan') |>
        redist::filter(.data$redistio_plan %in% input$alg_district) |>
        dplyr::mutate(
          redistio_sub_id = as.character(dplyr::row_number() + base_id)
        ))
      map_sub_in(shp_in |>
        dplyr::mutate(redistio_plan = redistio_curr_plan$pl) |>
        `attr<-`('existing_col', 'redistio_plan') |>
        redist::filter(.data$redistio_plan %in% input$alg_district) |>
        dplyr::mutate(
          redistio_sub_id = as.character(dplyr::row_number() + base_id)
        ))

      district_order <- map_sub()$redistio_plan |> unique()

      run_sims <- switch(input$alg_algorithm,
        'SMC' = redist::redist_smc,
        'Merge Split' = \(...) redist::redist_mergesplit(warmup = 0, ...),
        'Flip' = redist::redist_flip,
      )

      if (input$alg_algorithm %in% c('SMC', 'Merge Split')) {
        if (input$alg_counties_id != 'NONE') {
          sims <- run_sims(map_sub_in(),
            nsims = input$alg_nsims,
            counties = !!rlang::sym(input$alg_counties_id)
          )
        } else {
          sims <- run_sims(map_sub_in(), nsims = input$alg_nsims)
        }
      } else {
        if (input$alg_counties_id != 'NONE') {
          cons <- redist::redist_constr(map_sub_in()) |>
            redist::add_constr_edges_rem(0.4) |>
            redist::add_constr_splits(strength = 0.25, admin = !!rlang::sym(input$alg_counties_id))
          sims <- run_sims(map_sub_in(),
            nsims = input$alg_nsims,
            constraints = cons
          )
        } else {
          sims <- run_sims(map_sub_in(), nsims = input$alg_nsims)
        }
      }
      if (length(input$alg_district) > 1) {
        sims <- sims |>
          redist::match_numbers('redistio_plan')
      }

      redistio_alg_plan$plans <- redist::get_plans_matrix(sims) |>
        apply(MARGIN = 2, FUN = function(col) district_order[col])
      redistio_alg_plan$pl <- redistio_alg_plan$plans[, 2]

      sims_sum <- sims |>
        dplyr::mutate(
          dev = redist::plan_parity(map = map_sub_in())
        ) |>
        tibble::as_tibble() |>
        dplyr::group_by(draw) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::select(dplyr::all_of(c('draw', 'dev')))
      alg_plans(sims_sum)

      mapgl::maplibre_proxy('alg_map') |>
        mapgl::set_paint_property(
          layer_id = 'precinct_fill',
          name = 'fill-color',
          value = '#FFFFFF00'
        ) |>
        mapgl::add_source(
          data = map_sub(),
          id = 'redistio_sub',
          promoteId = 'redistio_sub_id'
        ) |>
        mapgl::add_fill_layer(
          source = 'redistio_sub',
          fill_color = discrete_palette(pal(), redistio_alg_plan$pl,
            column = 'redistio_sub_id', base = base_id
          ),
          fill_opacity = fill_opacity,
          id = 'alg_precincts'
        )
    })

    output$alg_summary <- DT::renderDT(
      {
        shiny::isolate(alg_plans()) |>
          DT::datatable(
            options = list(
              dom = 't', ordering = FALSE, scrollX = TRUE, scrollY = '70vh', # TODO make changeable
              pagingType = 'numbers', scrollCollapse = TRUE,
              pageLength = ndists * ((opts$alg_max_sims %||% def_opts$alg_max_sims) + 1)
            ),
            style = 'bootstrap',
            rownames = FALSE,
            escape = FALSE,
            selection = list(target = 'row', mode = 'single', selected = 2),
            fillContainer = TRUE
          ) |>
          DT::formatPercentage(columns = 'dev', digits = 1)
      },
      server = TRUE
    )

    dt_alg_proxy <- DT::dataTableProxy('alg_summary')

    shiny::observe({
      DT::replaceData(
        proxy = dt_alg_proxy, alg_plans(), rownames = FALSE,
        resetPaging = FALSE, clearSelection = 'none'
      )
    })

    shiny::observeEvent(input$alg_summary_rows_selected, {
      shiny::req(redistio_alg_plan$plans)
      mapgl::maplibre_proxy('alg_map') |>
        mapgl::set_paint_property(
          layer_id = 'alg_precincts',
          name = 'fill-color',
          value = discrete_palette(pal(), redistio_alg_plan$plans[, input$alg_summary_rows_selected],
            column = 'redistio_sub_id', base = base_id
          )
        )
    })


    shiny::observeEvent(input$alg_accept, {
      pl <- redistio_alg_plan$plans[, input$alg_summary_rows_selected]
      idx <- which(redistio_curr_plan$pl %in% input$alg_district)
      redistio_curr_plan$pl[idx] <- pl

      undo_l(undo_log(undo_l(), redistio_curr_plan$pl))

      mapgl::maplibre_proxy('map', session = parent_session) |>
        update_shape_style(
          'District', pal(), redistio_curr_plan$pl, shp,
          input$fill_opacity, precinct_border, precinct_linecolor
        )

      mapgl::maplibre_proxy('alg_map') |>
        mapgl::clear_layer(layer_id = 'alg_precincts')

      alg_plans(alg_plans_static)
      shiny::updateTabsetPanel(parent_session, 'navbar', 'draw')

      new_tb_pop <- val()
      new_tb_pop$Population <- distr_pop(shp[[pop_col]], total = tot_pop, plan = redistio_curr_plan$pl, ndists = ndists)
      new_tb_pop$Deviation <- as.integer(new_tb_pop$Population - c(0L, rep(tgt_pop, ndists)))
      val(new_tb_pop)

      DT::replaceData(
        proxy = dt_alg_proxy, alg_plans(), rownames = FALSE,
        resetPaging = FALSE, clearSelection = 'none'
      )
    })
  })
}
