#' Interactive Plan Drawing
#'
#' @param shp an `sf` tibble that you want to draw with
#' @param init_plan Plan to initialize with.
#' @param ndists Number of districts to draw if `init_plan` is not supplied.
#' @param palette Color palette to fill shapes with. Default is Polychrome 36.
#' @param pop_tol the population tolerance.
#' @param adj_col Name of column in `shp` that contains adjacency information.
#' @param split_cols Name of column in `shp` that contain administrative units
#' @param opts list of options. Default is `redistio_options()`
#' @param save_path Output path to save progress to.
#'
#' @return Shiny app
#' @export
#'
#' @examples
#' if (interactive()) {
#'   draw(dc, dc$ward)
#' }
#'
draw <- function(shp, init_plan, ndists, palette, pop_tol = 0.05,
                 adj_col = 'adj', split_cols = guess_admins,
                 opts = redistio_options(),
                 save_path = tempfile(fileext = '.csv')) {
  # defaults ----
  def_opts <- redistio_options()

  # run basic inputs ----
  if (missing(shp)) {
    stop('`shp` missing, but required.')
  }

  if (missing(init_plan)) {
    if (missing(ndists)) {
      stop('One of `init_plan` or `ndists` must be supplied.')
    } else {
      init_plan <- rep(NA_integer_, nrow(shp))
    }
  } else {
    ndists <- length(unique(init_plan))
  }

  if (!adj_col %in% names(shp)) {
    shp$adj <- geomander::adjacency(shp)
  }

  if (rlang::is_closure(split_cols)) {
    split_cols <- split_cols(shp)
  }

  if (!sf::st_is_longlat(shp)) {
    shp <- sf::st_transform(shp, opts$crs %||% def_opts$crs)
  }


  # handle palettes ----
  if (missing(palette)) {
    palette <- suppressWarnings(grDevices::palette.colors(n = ndists, 'Polychrome 36'))
  }
  palette <- as.character(palette)
  if (length(palette) != ndists) {
    if (length(palette) > ndists) {
      palette <- palette[seq_len(ndists)]
    } else {
      if (length(palette) < 1) {
        stop('`palette` must have at least one color.')
      }
      palette <- rep(palette, ceiling(ndists/length(palette)))[seq_len(ndists)]
    }
  }

  shp$redistio_id <- as.character(seq_len(length.out = nrow(shp)))
  shp$fmt_pop <- scales::label_comma()(shp$pop)

  # prep hover ----
  shp_tb <- shp |>
    tibble::as_tibble()
  # sf::st_drop_geometry()
  # Using sf::st_drop_geometry causes a
  # Error in glue(str, .envir = .envir, .transformer = transformer, .cli = TRUE,  :
  # Expecting '}'
  # when used on a redist_map twice in a row

  hov <- hover_precinct(
    shp_tb,
    pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap')
  ) |>
    dplyr::bind_rows(.id = 'group') |>
    format_alarm_names()

  # other prep ----

  tgt_pop <- sum(shp$pop) / ndists
  min_pop <- ceiling(tgt_pop * (1 - pop_tol))
  max_pop <- floor(tgt_pop * (1 + pop_tol))
  tgt_pop <- as.integer(round(tgt_pop))
  pretty_bounds <- paste0(
    'Population must be in [',
    scales::label_comma()(min_pop), ', ', scales::label_comma()(max_pop), '].'
  )

  use_algorithms <- inherits(shp, 'redist_map') &&
    opts$use_algorithms %||% def_opts$use_algorithms &&
    rlang::is_installed('redist')

  # User Interface ----
  if (!is.null(opts$select_color)) {
    selection_html <- shiny::tags$style(
      shiny::HTML(
        paste0(
          'table.dataTable tr.active td, table.dataTable tr.active ',
          '{box-shadow: inset 0 0 0 9999px ',
          opts$select_color %||% def_opts$select_color,
          '!important;}')
      )
    )
  } else {
    selection_html <- NULL
  }


  ui <- shiny::navbarPage(
    title = 'redistio',
    theme = bslib::bs_theme(preset = (opts$theme %||% def_opts$theme)),
    id = 'navbar',
    # draw panel ----
    shiny::tabPanel(
      title = 'draw',
      the_javascripts,
      selection_html,
      theme = bslib::bs_theme(preset = (opts$theme %||% def_opts$theme)),
      shiny::fluidRow(
        shiny::column( # color selector
          2,
          DT::DTOutput(outputId = 'district', width = '30vh', height = 'auto',
                       ),
        ),
        shiny::column( # interactive mapper
          8,
          leaflet::leafletOutput(
            outputId = 'map',
            height = opts$leaflet_height %||% def_opts$leaflet_height,
          )
        ),
        shiny::column( # details area
          2, shiny::tabsetPanel(
            id = 'tabRight',
            shiny::tabPanel('Population', gt::gt_output('tab_pop')),
            shiny::tabPanel('Precinct', gt::gt_output('hover')),
            shiny::tabPanel('Download', shiny::downloadButton('save_plan', label = 'Export plan')),
            selected = 'Precinct'
          )
        )
      )
    ),
    # demographics panel ----
    shiny::tabPanel(
      title = 'demographics',
      shiny::fluidRow(
        gt::gt_output('demographics')
      )
    ),
    # traditional redistricting panel ----
    shiny::tabPanel(
      title = 'integrity',
      shiny::fluidRow(
        gt::gt_output('integrity')
      )
    ),
    # elections panel ----
    shiny::tabPanel(
      title = 'elections',
      shiny::fluidRow(
        gt::gt_output('elections')
      )
    ),
    # algorithms panel ----
    if (use_algorithms) {
      shiny::tabPanel(
        title = 'algorithms',
        shiny::fluidRow(
          shiny::column( # selector
            2,
            shiny::selectizeInput(
              inputId = 'alg_district',
              label = paste0('Districts to redraw (up to ', min(opts$alg_max_districts %||% def_opts$alg_max_districts, ndists), ')'),
              choices = seq_len(ndists),
              options = list(maxItems = min(opts$alg_max_districts %||% def_opts$alg_max_districts, ndists))
            ),
            shiny::hr(),
            shiny::selectInput(
              inputId = 'alg_algorithm',
              label = 'Algorithm to use',
              choices = c('SMC', 'Merge Split', 'Flip')
            ),
            shiny::hr(),
            shiny::sliderInput(
              inputId = 'alg_nsims',
              label = 'Number of simulations',
              min = 1,
              max = (opts$alg_max_sims %||% def_opts$alg_max_sims),
              value = 10
            ),
            shiny::hr(),
            shiny::actionButton(
              inputId = 'alg_run',
              label = 'Run algorithm',
              icon = shiny::icon('circle-play')
            )
          ),
          shiny::column( # interactive mapper
            width = 8,
            leaflet::leafletOutput(
              outputId = 'alg_map',
              height = opts$leaflet_height %||% def_opts$leaflet_height,
            )
          ),
          shiny::column( # details area
            width = 2,
            DT::DTOutput(outputId = 'alg_summary', width = '30vh', height = '80vh'),
            shiny::tags$hr(),
            shiny::actionButton(
              inputId = 'alg_accept',
              label = 'Accept plan',
              icon = shiny::icon('file-export')
            ),
          )
        )
      )
    } else {
      NULL
    }
  )

  # Server ----
  server <- function(input, output, session) {
    redistio_curr_plan <- shiny::reactiveValues(pl = init_plan)
    redistio_alg_plan <- shiny::reactiveValues(pl = NULL, plans = NULL)
    clicked <- shiny::reactiveValues(clickedMarker = NULL)

    tab_pop_static <- dplyr::tibble(
      District = paste0("<p style='background-color:", palette[seq_len(ndists)], "; text-align:center;'> ", seq_len(ndists), " </p>"),
      Population = as.integer(tapply(shp$pop, init_plan, sum)),
      Deviation = as.integer(.data$Population - tgt_pop)
    )

    val <- shiny::reactiveVal(tab_pop_static)
    map_sub <- shiny::reactiveVal(shp)

    alg_plans_static <- tibble::tibble(
      draw = factor(),
      # district = integer(),
      # total_pop = double(),
      dev = double()
    )

    alg_plans <- shiny::reactiveVal(alg_plans_static)

    pal <- leaflet::colorFactor(
      palette = as.character(palette[seq_len(ndists)]),
      domain = seq_len(ndists)
    )

    # draw panel ----

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(data = shp) |>
        leaflet::addTiles() |>
        leaflet::addPolygons(
          layerId = ~redistio_id,
          weight = 1,
          label = ~fmt_pop
        )
    })

    shiny::observe({
      leaflet::leafletProxy('map', data = shp) |>
        setShapeStyle(
          layerId = ~redistio_id,
          fillColor = ~ pal(init_plan),
          # color = ~pal(init_plan),
          stroke = TRUE,
          weight = 0.5,
          color = '#000000',
          fillOpacity = 0.95
        )
    })


    shiny::observeEvent(input$map_shape_click, {
      clicked$map_shape_click <- input$map_shape_click
    })

    shiny::observeEvent(
      eventExpr = clicked$map_shape_click,
      handlerExpr = {
        click <- clicked$map_shape_click
        clicked$map_shape_click <- NULL
        if (is.null(click)) {
          return(NULL)
        }

        idx <- which(shp$redistio_id == click$id)
        redistio_curr_plan$pl[idx] <- input$district_rows_selected
        new_tb_pop <- val()
        new_tb_pop$Population <- as.integer(tapply(shp$pop, redistio_curr_plan$pl, sum))
        new_tb_pop$Deviation <- as.integer(new_tb_pop$Population - tgt_pop)
        val(new_tb_pop)

        leaflet::leafletProxy('map', data = shp) |>
          setShapeStyle(
            # data = shp,
            layerId = ~redistio_id,
            # line colors
            stroke = TRUE, weight = 1,
            color = '#000000',
            # fill control
            fillOpacity = 0.95,
            fillColor = ~ pal(redistio_curr_plan$pl)
          )
      }
    )

    # district stats ----
    output$district <- DT::renderDT({
        shiny::isolate(val()) |>
          DT::datatable(
            options = list(
              dom = 't', ordering = FALSE, scrollX = TRUE, scrollY = '80vh', #TODO make changeable
              pageLength = ndists
            ),
            style = 'bootstrap',
            rownames = FALSE,
            escape = FALSE,
            selection = list(target = 'row', mode = 'single', selected = 1),
            fillContainer = TRUE
          ) |>
          DT::formatRound(columns = c('Population', 'Deviation'), digits = 0)
      },
      server = TRUE
    )

    dt_proxy <- DT::dataTableProxy('district')

    shiny::observe({
      DT::replaceData(
        proxy = dt_proxy, val(), rownames = FALSE,
        resetPaging = FALSE, clearSelection = 'none'
      )
    })

    output$tab_pop <- gt::render_gt({
      val() |>
        dplyr::mutate(District = stringr::str_extract(.data$District, ' \\d+ ')) |>
        gt::gt() |>
        gt::tab_style(
          style = gt::cell_fill(color = 'red'),
          locations = gt::cells_body(
            rows = .data$Population > max_pop | .data$Population < min_pop
          )
        ) |>
        gt::cols_label(
          District = ''
        ) |>
        gt::fmt_number(columns = c('Population', 'Deviation'), decimals = 0) |>
        gt::tab_footnote(
          footnote = pretty_bounds
        )
    })

    # reactive mouseover
    hov_reac <- shiny::reactive({
      input$map_shape_mouseover
    })
    hov_reac_d <- shiny::debounce(hov_reac, 100)

    # precinct stats ----
    shiny::observeEvent(hov_reac_d(), {
      if (!is.null(hov_reac_d())) {
        if (input$tabRight == 'Precinct') {
          output$hover <- gt::render_gt({
            hov |>
              dplyr::select('group', 'rowname', paste0('V', hov_reac_d()$id)) |>
              gt::gt() |>
              gt::cols_label_with(columns = gt::starts_with('V'), fn = function(x) '') |>
              gt::tab_style(
                style = list(
                  gt::cell_text(align = 'left')
                ),
                locations = gt::cells_stub(rows = TRUE)
              ) |>
              gt::tab_options(
                data_row.padding = gt::px(0.5)
              ) |>
              gt::fmt_number(columns = gt::starts_with('V'), decimals = 0)
          })
        }
      }
    })

    # downloader ----
    output$save_plan <- shiny::downloadHandler(
      filename = function() {
        save_path
      },
      content = function(file) {
        df <- data.frame(
          row_id = seq_len(length(redistio_curr_plan$pl)),
          District = redistio_curr_plan$pl
        )

        utils::write.csv(
          x = df,
          file = file,
          row.names = FALSE
        )
      }
    )
    shiny::outputOptions(output, "save_plan", suspendWhenHidden = FALSE)

    # demographics panel ----

    output$demographics <- gt::render_gt({
      list(
        rict_population(shp, redistio_curr_plan$pl, as_gt = FALSE),
        tally_pop(shp, redistio_curr_plan$pl, normalize = TRUE),
        tally_vap(shp, redistio_curr_plan$pl, normalize = TRUE)
      ) |>
        purrr::reduce(.f = dplyr::left_join, by = 'District') |>
        gt::gt() |>
        gt::fmt_number(columns = c('Population', 'deviation', 'vap'), decimals = 0) |>
        gt::fmt_percent(columns = 'pct_deviation', decimals = 1) |>
        gt::fmt_percent(columns = dplyr::starts_with(c('pop_', 'vap_')), decimals = 1) |>
        gt::cols_hide(columns = 'pop') |>
        gt::cols_label(
          deviation = 'People',
          pct_deviation = '%',
          vap = 'Total'
        ) |>
        gt::cols_label_with(
          columns = dplyr::starts_with(c('pop_', 'vap_')),
          fn = function(x) format_demog_string(stringr::word(x, 2, sep = '_'))
        ) |>
        gt::tab_spanner(label = 'Deviation', columns = c('deviation', 'pct_deviation')) |>
        gt::tab_spanner(label = 'Total Population', columns = dplyr::starts_with(c('pop_'))) |>
        gt::tab_spanner(label = 'Voting Age Population', columns = dplyr::starts_with(c('vap')))
    })

    # integrity panel ----
    output$integrity <- gt::render_gt({
      if (adj_col %in% names(shp)) {
        int_l <- list(
          rict_population(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
          rict_contiguity(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
          rict_compactness(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
          rict_splits(shp, plan = redistio_curr_plan$pl,
                      admin = split_cols$admin, subadmin = split_cols$subadmin,
                      multi = split_cols$multi, total = split_cols$total,
                      as_gt = FALSE)
        )
      } else {
        int_l <- list(
          rict_population(shp, redistio_curr_plan$pl, as_gt = FALSE),
          rict_compactness(shp, redistio_curr_plan$pl, as_gt = FALSE),
          rict_splits(shp, plan = redistio_curr_plan$pl,
                      admin = split_cols$admin, subadmin = split_cols$subadmin,
                      multi = split_cols$multi, total = split_cols$total,
                      as_gt = FALSE)
        )
      }
      int_l |>
        purrr::reduce(.f = dplyr::left_join, by = 'District') |>
        gt::gt() |>
        gt::fmt_number(columns = c('Population', 'deviation'), decimals = 0) |>
        gt::fmt_percent(columns = 'pct_deviation', decimals = 1) |>
        gt::fmt_percent(columns = dplyr::starts_with('comp_'), decimals = 1) |>
        gt::tab_spanner(label = 'Deviation', columns = c('deviation', 'pct_deviation')) |>
        gt::tab_spanner(label = 'Contiguity', columns = c('Pieces')) |>
        gt::tab_spanner(label = 'Compactness', columns = dplyr::starts_with('comp_')) |>
        gt::tab_spanner(label = 'Splits',
                        columns = dplyr::starts_with(c('admin_', 'subadmin_'))) |>
        gt::tab_spanner(label = 'Multi Splits', columns = dplyr::starts_with('multi_')) |>
        gt::tab_spanner(label = 'Total Splits', columns = dplyr::starts_with('total_')) |>
        gt::cols_label(
          deviation = 'People',
          pct_deviation = '%'
        )  |>
        gt::cols_label_with(
          columns = dplyr::starts_with('comp_'),
          fn = function(x) format_compactness(x)
        ) |>
        gt::cols_label_with(
          columns = dplyr::starts_with(c('admin_', 'subadmin_', 'multi_', 'total_')),
          fn = function(x) x |>
            stringr::str_remove('^admin_|^subadmin_|^multi_|^total_')
        )

    })

    # elections panel ----
    output$elections <- gt::render_gt({
      rict_elections(shp_tb, plan = redistio_curr_plan$pl)
    })

    # algorithms panel ----
    if (use_algorithms) {

      output$alg_map <- leaflet::renderLeaflet({

        map_sub(shp |>
          dplyr::mutate(redistio_plan = redistio_curr_plan$pl) |>
          `attr<-`('existing_col', 'redistio_plan') |>
          redist::filter(.data$redistio_plan %in% input$alg_district))

        run_sims <- switch(input$alg_algorithm,
          'SMC' = redist::redist_smc,
          'Merge Split' = \(...) redist::redist_mergesplit(warmup = 0, ...),
          'Flip' = redist::redist_flip,
        )

        if (input$alg_algorithm %in% c('SMC', 'Merge Split')) {
          sims <- run_sims(map_sub(), nsims = input$alg_nsims)
        } else {
          sims <- run_sims(map_sub(), nsims = input$alg_nsims,
                           counties = !!rlang::ensym(opts$alg_counties %||% def_opts$alg_counties)
                           )
        }

        # TODO: plan selection vs taking last
        redistio_alg_plan$pl <- redist::last_plan(sims)
        redistio_alg_plan$plans <- redist::get_plans_matrix(sims)
        sims_sum <- sims |>
          dplyr::mutate(
            dev = redist::plan_parity(map = map_sub())
          ) |>
          tibble::as_tibble() |>
          dplyr::group_by(draw) |>
          dplyr::slice(1) |>
          dplyr::ungroup() |>
          dplyr::select(dplyr::all_of(c('draw', 'dev')))
        alg_plans(sims_sum)


        map_sub() |>
          leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::addPolygons(
            layerId = ~redistio_id,
            weight = 1,
            label = ~fmt_pop,
            fillColor = pal(redistio_alg_plan$pl),
            fillOpacity = 0.95,
            color = '#000000',
            stroke = 0.5
          )

      }) |>
      shiny::bindEvent(input$alg_run)
    }

    output$alg_summary <- DT::renderDT({
      shiny::isolate(alg_plans()) |>
        DT::datatable(
          options = list(
            dom = 't', ordering = FALSE, scrollX = TRUE, scrollY = '70vh', #TODO make changeable
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

    shiny::observeEvent(input$alg_summary_rows_selected,{
      shiny::req(redistio_alg_plan$plans)
      leaflet::leafletProxy('alg_map', data = map_sub()) |>
        setShapeStyle(
          # data = shp,
          layerId = ~redistio_id,
          # line colors
          stroke = TRUE, weight = 1,
          color = '#000000',
          # fill control
          fillOpacity = 0.95,
          fillColor = ~ pal(redistio_alg_plan$plans[, input$alg_summary_rows_selected])
        )
      })


    shiny::observeEvent(input$alg_accept, {
      pl <- redistio_alg_plan$plans[, input$alg_summary_rows_selected]
      pl <- sort(input$alg_district)[pl]
      idx <- which(redistio_curr_plan$pl %in% input$alg_district)
      redistio_curr_plan$pl[idx] <- pl

      leaflet::leafletProxy('map', data = shp) |>
        setShapeStyle(
          # data = shp,
          layerId = ~redistio_id,
          # line colors
          stroke = TRUE, weight = 1,
          color = '#000000',
          # fill control
          fillOpacity = 0.95,
          fillColor = ~ pal(redistio_curr_plan$pl)
        )

      leaflet::leafletProxy('alg_map') |>
        leaflet::clearTiles() |>
        leaflet::clearShapes()

      alg_plans(alg_plans_static)
      shiny::updateTabsetPanel(session, 'navbar', 'draw')
    })

  }

  # run app ----
  shiny::shinyApp(ui = ui, server = server)
}
