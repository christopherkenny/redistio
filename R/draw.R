#' Interactive Plan Drawing
#'
#' @param shp an `sf` tibble that you want to draw with
#' @param init_plan Plan to initialize with.
#' @param ndists Number of districts to draw if `init_plan` is not supplied.
#' @param layers Named list of `sf` objects to overlay. Also takes column names in `shp` to group by.
#' @param palette Color palette to fill shapes with. Default is Polychrome 36.
#' @param pop_tol the population tolerance.
#' @param adj_col Name of column in `shp` that contains adjacency information.
#' @param split_cols Names of column in `shp` that contain administrative units
#' @param elect_cols Names of column in `shp` that contain election data
#' @param demog_cols Names of column in `shp` that contain demographic data
#' @param opts list of options. Default is `redistio_options()`
#'
#' @return Shiny app
#' @export
#'
#' @examples
#' if (interactive()) {
#'   draw(dc, dc$ward)
#'   draw(dc, dc$ward, layers = list(neighborhoods = 'adv_nbr'))
#' }
#'
draw <- function(shp, init_plan, ndists, palette,
                 layers = NULL, pop_tol = 0.05,
                 adj_col = 'adj',
                 split_cols = guess_admins,
                 elect_cols = guess_elections,
                 demog_cols = guesstimate_demographics,
                 opts = redistio_options()) {
  # defaults ----
  def_opts <- redistio_options()
  poss_panels <- c('draw', 'demographics', 'integrity', 'elections', 'algorithms')

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
    if (missing(ndists)) {
      ndists <- length(unique(init_plan))
    }
  }

  shp$redistio_id <- as.character(seq_len(length.out = nrow(shp)))

  if (!adj_col %in% names(shp)) {
    shp$adj <- geomander::adjacency(shp)
  }

  if (rlang::is_closure(split_cols)) {
    split_cols <- split_cols(shp)
  }

  if (rlang::is_closure(elect_cols)) {
    elect_cols <- elect_cols(shp)
  }
  for (i in seq_along(elect_cols)) {
    nom <- names(elect_cols)[i]
    shp <- shp |>
      dplyr::mutate(
        {{ nom }} := !!rlang::sym(elect_cols[[i]]$dem) /
          (!!rlang::sym(elect_cols[[i]]$dem) + !!rlang::sym(elect_cols[[i]]$rep))
      )
  }
  if (rlang::is_closure(demog_cols)) {
    demog_cols_eval <- demog_cols(shp)
    demog_cols <- names(demog_cols_eval)
    demog_cols <- c(intersect(names(shp), c('pop', 'vap', 'cvap')), demog_cols)
    demog_cols <- unique(demog_cols)
  }
  if (is.data.frame(demog_cols_eval)) {
    shp <- shp |>
      dplyr::left_join(
        y = demog_cols_eval |>
          dplyr::mutate(
            redistio_id = as.character(dplyr::row_number())
          ),
        by = 'redistio_id'
      )
  }

  # process shp components ----
  if (!sf::st_is_longlat(shp)) {
    shp <- sf::st_transform(shp, opts$crs %||% def_opts$crs)
  }

  shp <- shp |>
    sf::st_make_valid()

  # handle palettes ----
  if (missing(palette)) {
    if (rlang::is_installed('crayons')) {
      palette <- crayons::crayons$no_48
    } else {
      palette <- suppressWarnings(grDevices::palette.colors(n = ndists, 'Polychrome 36'))
    }
  }
  palette <- as.character(palette)
  if (length(palette) != ndists) {
    if (length(palette) > ndists) {
      palette <- palette[seq_len(ndists)]
    } else {
      if (length(palette) < 1) {
        stop('`palette` must have at least one color.')
      }
      palette <- rep(palette, ceiling(ndists / length(palette)))[seq_len(ndists)]
    }
  }

  # prep layer colors ----
  if (!is.null(layers)) {
    layer_colors <- opts$layer_color %||% def_opts$layer_color
    if (length(layer_colors) == 1L) {
      layer_colors <- rep(layer_colors, length(layers))[seq_along(layers)]
    }
  }

  shp$fmt_pop <- scales::label_comma()(shp$pop)
  tot_pop <- sum(shp$pop)

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

  # prep layers -----
  if (!is.null(layers)) {
    sf_entries <- which(vapply(layers, function(x) inherits(x, 'sf'), logical(1)))
    char_entries <- which(vapply(layers, is.character, logical(1)))
    if ((length(sf_entries) + length(char_entries)) != length(layers)) {
      stop('`layers` must be a list of `sf` objects and character vectors.')
    }
    for (i in seq_along(char_entries)) {
      nom <- layers[[char_entries[i]]]
      layers[[char_entries[i]]] <- shp |>
        dplyr::group_by(!!rlang::sym(layers[[char_entries[i]]])) |>
        dplyr::summarize()
      if (is.null(names(layers)) || names(layers)[char_entries[i]] == '') {
        names(layers)[char_entries[i]] <- nom
      }
    }
    if (is.null(names(layers))) {
      names(layers) <- paste0('layer_', seq_along(layers))
    }
    if (any(names(layers) == '')) {
      names(layers)[names(layers) == ''] <- paste0('layer_', seq_along(layers))
    }
  }

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
          '!important;}'
        )
      )
    )
  } else {
    selection_html <- NULL
  }

  leaf_tiles <- opts$map_tiles %||% def_opts$map_tiles

  shiny::addResourcePath('assets', system.file('assets', package = 'redistio'))
  ui <- bslib::page_navbar(
    title = 'redistio',
    theme = bslib::bs_theme(
      preset = (opts$theme %||% def_opts$theme),
    ),
    id = 'navbar',
    header = shiny::tags$head(
      shiny::tags$link(
        rel = 'shortcut icon',
        href = 'https://raw.githubusercontent.com/christopherkenny/redistio/main/man/figures/logo.png'
      ),
      shiny::tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/styles.css')
    ),

    # draw panel ----
    bslib::nav_panel(
      title = 'draw',
      class = 'p-0',
      the_javascripts,
      selection_html,
      bslib::page_fillable(
        class = 'p-0',
        bslib::layout_sidebar(
          border = FALSE,
          border_radius = FALSE,
          fillable = TRUE,
          class = 'p-0',
          sidebar = bslib::sidebar( # color selector
            width = 300,
            id = 'map-left-sidebar',
            bslib::accordion(
              bslib::accordion_panel(
                'Edit districts',
                DT::DTOutput(outputId = 'district', fill = TRUE),
                icon = shiny::icon('paintbrush'),
                style = 'padding: 0 !important'
              ),
              bslib::accordion_panel(
                'Fill',
                shiny::h5('Select fill columns'),
                shiny::selectInput(
                  inputId = 'fill_input',
                  label = 'Precinct fill type',
                  choices = c('District', 'Demographics', 'Elections'),
                  selected = 'District'
                ),
                shiny::selectizeInput(
                  inputId = 'fill_column',
                  label = 'Precinct fill column',
                  choices = 'District',
                  selected = NULL,
                  multiple = FALSE
                ),
                shiny::sliderInput(
                  inputId = 'fill_opacity',
                  label = 'Fill opacity',
                  min = 0, max = 1, step = 0.05, value = 0.9
                ),
                shiny::numericInput(
                  inputId = 'precinct_border',
                  label = 'Precinct border weight',
                  min = 0, max = 100, value = 0.5
                ),
                icon = shiny::icon('palette'),
                style = 'min-height: 30vh'
              )
            )
          ),
          bslib::layout_sidebar(
            border = FALSE,
            class = 'p-0',
            bslib::card( # interactive mapper
              id = 'map-card',
              full_screen = TRUE,
              leaflet::leafletOutput(
                outputId = 'map',
                height = opts$leaflet_height %||% def_opts$leaflet_height,
              )
            ),
            sidebar = bslib::sidebar( # details area
              position = 'right',
              id = 'map-right-sidebar',
              width = 300,
              bslib::navset_bar(
                id = 'tabRight',
                bslib::nav_panel(title = 'Population', gt::gt_output('tab_pop')),
                bslib::nav_panel(title = 'Precinct', gt::gt_output('hover')),
                bslib::nav_menu(
                  title = 'More',
                  bslib::nav_panel(
                    'Download',
                    shiny::h5('Download assignment file'),
                    shiny::textInput('save_path',
                      label = 'Path to save assignment file',
                      value = opts$save_assignment_path %||% def_opts$save_assignment_path
                    ),
                    shiny::selectizeInput('download_id', 'Select identifier column:',
                      choices = NULL, selected = 'redistio_id',
                      multiple = FALSE
                    ),
                    shiny::downloadButton('save_plan', label = 'Export plan'),
                    shiny::h5('Download shapefile'),
                    shiny::textInput('save_shp_path',
                      label = 'Path to save shapefile',
                      value = opts$save_shape_path %||% def_opts$save_shape_path
                    ),
                    shiny::downloadButton('save_shp', label = 'Export shapefile')
                  ),
                  bslib::nav_panel(
                    'Lock',
                    shiny::tags$style(lock_css),
                    if (!rlang::is_installed('shinyWidgets')) {
                      shiny::checkboxGroupInput(
                        inputId = 'locks',
                        choices = seq_len(ndists),
                        label = 'Lock districts',
                        selected = opts$locked_districts %||% def_opts$locked_districts
                      )
                    } else {
                      shinyWidgets::checkboxGroupButtons(
                        inputId = 'locks',
                        label = 'Lock districts',
                        choices = seq_len(ndists),
                        status = 'lockClass',
                        checkIcon = list(
                          yes = shiny::icon('lock'),
                          no = shiny::icon('lock-open')
                        ),
                        direction = 'vertical'
                      )
                    }
                  ),
                  bslib::nav_panel(
                    'District colors',
                    lapply(seq_len(ndists), function(i) {
                      colourpicker::colourInput(
                        inputId = paste0('color_', i),
                        label = paste0('District ', i, ' color'),
                        value = palette[[i]] %||% palette[[i]]
                      )
                    }),
                    style = 'overflow-y: scroll; max-height: 70vh'
                  ),
                  bslib::nav_panel(
                    'Tools',
                    discontiguousUI('discontiguous'),
                    unassignedUI('unassigned')
                  ),
                  bslib::nav_panel(
                    'Color from file',
                    color_from_fileUI('colorFromFile')
                  ),
                  align = 'right'
                ),
                selected = 'Precinct'
              )
            )
          ) # end layout_sidebar
        ), # end layout_sidebar

        gap = 0,
        padding = 0
      ), # end page_fillable
    ),
    # demographics panel ----
    bslib::nav_panel(
      title = 'demographics',
      bslib::page_fillable(
        gt::gt_output('demographics')
      )
    ),
    # traditional redistricting panel ----
    bslib::nav_panel(
      title = 'integrity',
      bslib::page_fillable(
        gt::gt_output('integrity')
      )
    ),
    # elections panel ----
    bslib::nav_panel(
      title = 'elections',
      bslib::page_fillable(
        gt::gt_output('elections')
      )
    ),
    # algorithms panel ----
    if (use_algorithms) {
      bslib::nav_panel(
        title = 'algorithms',
        bslib::layout_columns(
          col_widths = c(2, 8, 2),
          bslib::card( # selector
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
            shiny::selectizeInput('alg_counties_id', 'Select county column:',
              choices = c('NONE', names(shp)), selected = NULL,
              multiple = FALSE
            ),
            shiny::hr(),
            shiny::actionButton(
              inputId = 'alg_run',
              label = 'Run algorithm',
              icon = shiny::icon('circle-play')
            )
          ),
          bslib::card( # interactive mapper
            leaflet::leafletOutput(
              outputId = 'alg_map',
              height = opts$leaflet_height %||% def_opts$leaflet_height,
            )
          ),
          bslib::card( # details area
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
    },
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionButton(
        inputId = 'undo',
        label = 'Undo last change',
        icon = shiny::icon('rotate-left'),
        class = 'btn-primary'
      ),
    )
  )

  # Server ----
  server <- function(input, output, session) {
    # panel controls ----
    req_panels <- opts$panels %||% def_opts$panels
    if (!use_algorithms) {
      poss_panels <- setdiff(poss_panels, 'algorithms')
      req_panels <- setdiff(req_panels, 'algorithms')
    }
    if (!all(req_panels %in% poss_panels)) {
      stop('Invalid panel selection')
    }
    hide_panels <- setdiff(poss_panels, c(req_panels, 'draw'))
    for (panel in hide_panels) {
      shiny::hideTab(inputId = 'navbar', target = panel)
    }

    # reactives ----
    redistio_curr_plan <- shiny::reactiveValues(pl = init_plan)
    redistio_alg_plan <- shiny::reactiveValues(pl = NULL, plans = NULL)
    clicked <- shiny::reactiveValues(clickedMarker = NULL)
    palette_reactive <- shiny::reactiveVal(palette)

    tab_pop_static <- dplyr::tibble(
      District = paste0(
        "<p class='distr-sel' style='background-color:",
        palette[c(NA_integer_, seq_len(ndists))],
        ";'> ",
        c(as.character(shiny::icon('eraser')), seq_len(ndists)),
        ' </p>'
      ),
      Population = distr_pop(shp$pop, total = tot_pop, plan = init_plan, ndists = ndists),
      Deviation = as.integer(distr_pop(shp$pop, total = tot_pop, plan = init_plan, ndists = ndists) - c(0L, rep(tgt_pop, ndists)))
    )
    val <- shiny::reactiveVal(tab_pop_static)
    map_sub <- shiny::reactiveVal(shp)

    alg_plans_static <- tibble::tibble(
      draw = factor(),
      dev = double()
    )

    alg_plans <- shiny::reactiveVal(alg_plans_static)

    pal <- shiny::reactiveVal(
      leaflet::colorFactor(
        palette = as.character(palette),
        domain = seq_len(ndists)
      )
    )

    # undo ----
    undo_l <- shiny::reactiveVal(undo_log(l = undo_init(10L), pl = init_plan))

    # draw panel ----

    output$map <- leaflet::renderLeaflet({
      base_map <- leaflet::leaflet(
        data = shp,
      ) |>
        leaf_tiles() |>
        leaflet::addPolygons(
          layerId = ~redistio_id,
          weight = 1,
          label = ~fmt_pop
        )
      if (!is.null(layers)) {
        for (i in seq_along(layers)) {
          base_map <- base_map |>
            leaflet::addPolygons(
              data = layers[[i]],
              fill = FALSE,
              weight = opts$layer_weight %||% def_opts$layer_weight,
              color = layer_colors[i],
              group = names(layers)[i],
              options = leaflet::pathOptions(interactive = FALSE)
            )
        }
        base_map <- base_map |>
          leaflet::addLayersControl(
            overlayGroups = names(layers),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      }
      base_map
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
        new_dist <- ifelse(input$district_rows_selected == 1, NA_integer_, input$district_rows_selected - 1L)
        if (redistio_curr_plan$pl[idx] %in% input$locks || new_dist %in% input$locks) {
          return(NULL)
        }

        redistio_curr_plan$pl[idx] <- new_dist

        undo_l(undo_log(undo_l(), redistio_curr_plan$pl))

        new_tb_pop <- val()
        new_tb_pop$Population <- distr_pop(shp$pop, total = tot_pop, plan = redistio_curr_plan$pl, ndists = ndists)
        new_tb_pop$Deviation <- as.integer(new_tb_pop$Population - c(0L, rep(tgt_pop, ndists)))
        val(new_tb_pop)

        leaflet::leafletProxy('map', data = shp) |>
          update_shape_style(input$fill_column, pal(), redistio_curr_plan$pl, shp,
                             input$fill_opacity, input$precinct_border)
      }
    )

    shiny::observeEvent(list(input$fill_opacity, input$precinct_border), {
      leaflet::leafletProxy('map', data = shp) |>
        update_shape_style(input$fill_column, pal(), redistio_curr_plan$pl, shp,
                           input$fill_opacity, input$precinct_border)
    })

    # district stats ----
    output$district <- DT::renderDT(
      {
        shiny::isolate(val()) |>
          DT::datatable(
            options = list(
              dom = 't', ordering = FALSE, scrollY = paste0(min(ndists * 8, 90), 'vh'), # scrollX = TRUE, #, # TODO make changeable
              pageLength = ndists + 1L
            ),
            style = 'bootstrap',
            rownames = FALSE,
            escape = FALSE,
            selection = list(target = 'row', mode = 'single', selected = 2),
            fillContainer = TRUE,
            colnames = c('District', 'Pop.', 'Dev.')
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

    shiny::outputOptions(output, 'district', suspendWhenHidden = FALSE)

    output$tab_pop <- gt::render_gt({
      val() |>
        # dplyr::slice(-1) |>
        dplyr::mutate(District = stringr::str_extract(.data$District, ' \\d+ ')) |>
        gt::gt() |>
        gt::tab_style(
          style = gt::cell_fill(color = 'red'),
          locations = gt::cells_body(
            rows = (.data$Population > max_pop | .data$Population < min_pop) & !is.na(.data$District)
          )
        ) |>
        gt::cols_label(
          District = ''
        ) |>
        gt::tab_options(
          data_row.padding = gt::px(2),
          table.width = '100%',
          container.padding.y = '0',
          heading.padding = '0',
          table.background.color = '#fff0'
        ) |>
        gt::fmt_number(columns = c('Population', 'Deviation'), decimals = 0) |>
        gt::tab_footnote(
          footnote = pretty_bounds
        )
    })

    # undo logic ----
    shiny::observeEvent(input$undo, {
      # don't do anything if there's nothing to do
      if (undo_l()$undo_index <= 1L) {
        return(NULL)
      }

      undo_l(undo_once(undo_l()))

      last_pl <- undo_l()$undo[[undo_l()$undo_index]]
      redistio_curr_plan$pl <- last_pl

      leaflet::leafletProxy('map', data = shp) |>
        update_shape_style(input$fill_column, pal(), redistio_curr_plan$pl, shp,
                           input$fill_opacity, input$precinct_border)

      new_tb_pop <- val()
      new_tb_pop$Population <- distr_pop(shp$pop, total = tot_pop, plan = redistio_curr_plan$pl, ndists = ndists)
      new_tb_pop$Deviation <- as.integer(new_tb_pop$Population - c(0L, rep(tgt_pop, ndists)))
      val(new_tb_pop)
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
            # produce hover tables ----
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
              gt::tab_header(
                title = paste0('Current District: ', redistio_curr_plan$pl[as.integer(hov_reac_d()$id)]),
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
      }
    })

    # downloader ----
    shiny::updateSelectizeInput(session, 'download_id',
      choices = names(shp), selected = 'redistio_id',
      server = TRUE
    )

    output$save_plan <- shiny::downloadHandler(
      filename = function() {
        input$save_path
      },
      content = function(file) {
        df <- tibble::tibble(
          !!input$download_id := shp[[input$download_id]],
          District = redistio_curr_plan$pl
        )

        utils::write.csv(
          x = df,
          file = file,
          row.names = FALSE
        )
      }
    )
    shiny::outputOptions(output, 'save_plan', suspendWhenHidden = FALSE)

    output$save_shp <- shiny::downloadHandler(
      filename = function() {
        input$save_shp_path
      },
      content = function(file) {
        shp |>
          tibble::as_tibble() |>
          sf::st_as_sf() |>
          dplyr::mutate(
            District = redistio_curr_plan$pl
          ) |>
          dplyr::group_by(.data$District) |>
          dplyr::summarise() |>
          sf::st_write(file)
      }
    )

    # fill mini panel ----
    shiny::observeEvent(input$fill_input, {
      if (input$fill_input == 'District') {
        shiny::updateSelectizeInput(session, 'fill_column',
          choices = c('District'),
          selected = NULL,
          server = FALSE
        )
        pal(leaflet::colorFactor(
          palette = as.character(palette_reactive()),
          domain = seq_len(ndists)
        ))
      } else if (input$fill_input == 'Elections') {
        shiny::updateSelectizeInput(session, 'fill_column',
          choices = names(elect_cols),
          selected = names(elect_cols)[1],
          server = FALSE
        )
        pal(leaflet::colorNumeric(
          palette = opts$palette_party %||% def_opts$palette_party,
          domain = c(0, 1),
          na.color = '#D3D3D3'
        ))
      } else {
        shiny::updateSelectizeInput(session, 'fill_column',
          choices = demog_cols,
          selected = demog_cols[1],
          server = FALSE
        )
        # have to update pal depending on fill column, no new pal
      }
    })

    shiny::observeEvent(input$fill_column,
      {
        if (input$fill_input == 'Demographics') {
          if (input$fill_column %in% c('pop', 'vap', 'cvap')) {
            pal(leaflet::colorNumeric(
              palette = opts$palette_pop %||% def_opts$palette_pop,
              domain = c(0, max(shp[[input$fill_column]], na.rm = TRUE)),
              na.color = '#D3D3D3'
            ))
          } else {
            pal(leaflet::colorNumeric(
              palette = opts$palette_pct %||% def_opts$palette_pct,
              domain = c(0, 1),
              na.color = '#D3D3D3'
            ))
          }
        }
        leaflet::leafletProxy('map', data = shp) |>
          update_shape_style(input$fill_column, pal(), redistio_curr_plan$pl, shp,
                             input$fill_opacity, input$precinct_border)
      },
      ignoreInit = FALSE
    )

    # district color mini panel ----
    shiny::observeEvent(
      eventExpr = lapply(seq_len(ndists), function(i) {
        input[[paste0('color_', i)]]
      }),
      handlerExpr = {
        # update palette
        palette_reactive(
          vapply(seq_len(ndists), function(i) {
            input[[paste0('color_', i)]]
          }, FUN.VALUE = character(1))
        )

        # update selection table
        new_color_tbl <- val()
        new_color_tbl$District <- paste0(
          "<p class='distr-sel' style='background-color:",
          palette_reactive()[c(NA_integer_, seq_len(ndists))],
          ";'> ",
          c(as.character(shiny::icon('eraser')), seq_len(ndists)),
          ' </p>'
        )
        val(new_color_tbl)

        if (input$fill_input == 'District') {
          pal(leaflet::colorFactor(
            palette = as.character(palette_reactive()),
            domain = seq_len(ndists),
            na.color = opts$na_color %||% def_opts$na_color,
            alpha = TRUE
          ))
          leaflet::leafletProxy('map', data = shp) |>
            update_shape_style(input$fill_column, pal(), redistio_curr_plan$pl, shp,
                               input$fill_opacity, input$precinct_border)
        }
      }
    )

    # tools mini panel ----
    discontiguousServer(
      'discontiguous', redistio_curr_plan, shp$adj, shp,
      shiny::reactive(leaflet::leafletProxy('map'))
    )

    unassignedServer(
      'unassigned', redistio_curr_plan, shp,
      shiny::reactive(leaflet::leafletProxy('map'))
    )

    color_from_fileServer(
      'colorFromFile'
    )

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
      if (!any(is.na(redistio_curr_plan$pl))) {
        if (adj_col %in% names(shp)) {
          int_l <- list(
            rict_population(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
            rict_contiguity(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
            rict_compactness(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
            rict_splits(shp,
              plan = redistio_curr_plan$pl,
              admin = split_cols$admin, subadmin = split_cols$subadmin,
              multi = split_cols$multi, total = split_cols$total,
              as_gt = FALSE
            )
          )
        } else {
          int_l <- list(
            rict_population(shp, redistio_curr_plan$pl, as_gt = FALSE),
            rict_compactness(shp, redistio_curr_plan$pl, as_gt = FALSE),
            rict_splits(shp,
              plan = redistio_curr_plan$pl,
              admin = split_cols$admin, subadmin = split_cols$subadmin,
              multi = split_cols$multi, total = split_cols$total,
              as_gt = FALSE
            )
          )
        }
      } else {
        int_l <- list(
          rict_population(shp, redistio_curr_plan$pl, as_gt = FALSE),
          rict_contiguity(shp, plan = redistio_curr_plan$pl, as_gt = FALSE)
        )
      }
      int_l |>
        purrr::reduce(.f = dplyr::left_join, by = 'District') |>
        gt::gt() |>
        gt::fmt_number(columns = c('Population', 'deviation'), decimals = 0) |>
        gt::fmt_percent(columns = 'pct_deviation', decimals = 1) |>
        gt::fmt_percent(columns = dplyr::starts_with('comp_'), decimals = 1) |>
        gt::tab_spanner(label = 'Deviation', columns = c('deviation', 'pct_deviation')) |>
        gt::tab_spanner(label = 'Contiguity', columns = dplyr::any_of('Pieces')) |>
        gt::tab_spanner(label = 'Compactness', columns = dplyr::starts_with('comp_')) |>
        gt::tab_spanner(
          label = 'Splits',
          columns = dplyr::starts_with(c('admin_', 'subadmin_'))
        ) |>
        gt::tab_spanner(label = 'Multi Splits', columns = dplyr::starts_with('multi_')) |>
        gt::tab_spanner(label = 'Total Splits', columns = dplyr::starts_with('total_')) |>
        gt::cols_label(
          deviation = 'People',
          pct_deviation = '%'
        ) |>
        gt::cols_label_with(
          columns = dplyr::starts_with('comp_'),
          fn = function(x) format_compactness(x)
        ) |>
        gt::cols_label_with(
          columns = dplyr::starts_with(c('admin_', 'subadmin_', 'multi_', 'total_')),
          fn = function(x) {
            x |>
              stringr::str_remove('^admin_|^subadmin_|^multi_|^total_')
          }
        )
    })

    # elections panel ----
    output$elections <- gt::render_gt({
      rict_elections(shp_tb, plan = redistio_curr_plan$pl)
    })

    # algorithms panel ----
    if (use_algorithms) {
      alg_pal <- leaflet::colorFactor(
        palette = as.character(palette[seq_len(ndists)]),
        domain = seq_len(ndists)
      )

      output$alg_map <- leaflet::renderLeaflet({
        map_sub(shp |>
          dplyr::mutate(redistio_plan = redistio_curr_plan$pl) |>
          `attr<-`('existing_col', 'redistio_plan') |>
          redist::filter(.data$redistio_plan %in% input$alg_district))

        district_order <- map_sub()$redistio_plan |> unique()

        run_sims <- switch(input$alg_algorithm,
          'SMC' = redist::redist_smc,
          'Merge Split' = \(...) redist::redist_mergesplit(warmup = 0, ...),
          'Flip' = redist::redist_flip,
        )

        if (input$alg_algorithm %in% c('SMC', 'Merge Split')
          # isTRUE(opts$alg_counties %||% def_opts$alg_counties %in% names(shp))
        ) {
          if (input$alg_counties_id != 'NONE') {
            sims <- run_sims(map_sub(),
              nsims = input$alg_nsims,
              counties = !!rlang::sym(input$alg_counties_id)
            )
          } else {
            sims <- run_sims(map_sub(), nsims = input$alg_nsims)
          }
        } else {
          if (input$alg_counties_id != 'NONE') {
            cons <- redist::redist_constr(map_sub()) |>
              redist::add_constr_edges_rem(0.4) |>
              redist::add_constr_splits(strength = 0.25, admin = !!rlang::sym(input$alg_counties_id))
            sims <- run_sims(map_sub(),
              nsims = input$alg_nsims,
              constraints = cons
            )
          } else {
            sims <- run_sims(map_sub(), nsims = input$alg_nsims)
          }
          sims <- run_sims(map_sub(), nsims = input$alg_nsims)
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
            dev = redist::plan_parity(map = map_sub())
          ) |>
          tibble::as_tibble() |>
          dplyr::group_by(draw) |>
          dplyr::slice(1) |>
          dplyr::ungroup() |>
          dplyr::select(dplyr::all_of(c('draw', 'dev')))
        alg_plans(sims_sum)

        map_alg <- map_sub() |>
          leaflet::leaflet() |>
          leaf_tiles() |>
          leaflet::addPolygons(
            layerId = ~redistio_id,
            weight = 1,
            label = ~fmt_pop,
            fillColor = alg_pal(redistio_alg_plan$pl),
            color = '#',
            stroke = TRUE
          )

        if (!is.null(layers)) {
          for (i in seq_along(layers)) {
            map_alg <- map_alg |>
              leaflet::addPolygons(
                data = layers[[i]],
                fill = FALSE,
                weight = opts$layer_weight %||% def_opts$layer_weight,
                color = layer_colors[i],
                group = names(layers)[i]
              )
          }
        }

        map_alg
      }) |>
        shiny::bindEvent(input$alg_run)
    }

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
      leaflet::leafletProxy('alg_map', data = map_sub()) |>
        setShapeStyle(
          # data = shp,
          layerId = ~redistio_id,
          # line colors
          stroke = TRUE, weight = 1.0,
          color = '#000',
          # fill control
          fillColor = ~ alg_pal(redistio_alg_plan$plans[, input$alg_summary_rows_selected])
        )
    })


    shiny::observeEvent(input$alg_accept, {
      pl <- redistio_alg_plan$plans[, input$alg_summary_rows_selected]
      idx <- which(redistio_curr_plan$pl %in% input$alg_district)
      redistio_curr_plan$pl[idx] <- pl

      undo_l(undo_log(undo_l(), redistio_curr_plan$pl))

      leaflet::leafletProxy('map', data = shp) |>
        update_shape_style(input$fill_column, pal(), redistio_curr_plan$pl, shp,
                           input$fill_opacity, input$precinct_border)

      leaflet::leafletProxy('alg_map') |>
        leaflet::clearTiles() |>
        leaflet::clearShapes()

      alg_plans(alg_plans_static)
      shiny::updateTabsetPanel(session, 'navbar', 'draw')

      new_tb_pop <- val()
      new_tb_pop$Population <- distr_pop(shp$pop, total = tot_pop, plan = redistio_curr_plan$pl, ndists = ndists)
      new_tb_pop$Deviation <- as.integer(new_tb_pop$Population - c(0L, rep(tgt_pop, ndists)))
      val(new_tb_pop)

      DT::replaceData(
        proxy = dt_alg_proxy, alg_plans(), rownames = FALSE,
        resetPaging = FALSE, clearSelection = 'none'
      )
    })
  }

  # run app ----
  shiny::shinyApp(ui = ui, server = server)
}
