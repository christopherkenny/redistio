#' Interactive Plan Drawing
#'
#' @param shp an `sf` tibble that you want to draw with
#' @param init_plan Plan to initialize with.
#' @param ndists Number of districts to draw if `init_plan` is not supplied.
#' @param layers Named list of `sf` objects to overlay. Also takes column names in `shp` to group by.
#' @param palette Color palette to fill shapes with. Default is `Polychrome 36` or,
#' if installed, `crayons::crayons$no_48`.
#' @param pop_tol the population tolerance.
#' @param pop_col Name of column in `shp` that contains population data.
#' @param adj_col Name of column in `shp` that contains adjacency information.
#' @param split_cols Names of column in `shp` that contain administrative units
#' @param elect_cols Names of column in `shp` that contain election data
#' @param demog_cols Names of column in `shp` that contain demographic data
#' @param hover_fn Function to generate tables for mouse hovering. Default is `hover_precinct()`.
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
draw <- function(
  shp,
  init_plan,
  ndists,
  palette = NULL,
  layers = NULL,
  pop_tol = 0.05,
  pop_col = 'pop',
  adj_col = 'adj',
  split_cols = guess_admins,
  elect_cols = guess_elections,
  demog_cols = guesstimate_demographics,
  hover_fn = hover_precinct,
  opts = redistio_options()
) {
  # defaults ----
  def_opts <- redistio_options()
  poss_panels <- c(
    'draw',
    'demographics',
    'integrity',
    'elections',
    'algorithms'
  )

  # run basic inputs ----
  if (missing(shp)) {
    stop('`shp` missing, but required.')
  }

  if (!pop_col %in% names(shp)) {
    stop('`shp` must have a `pop` column.')
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

  if (is.character(init_plan)) {
    # TODO: think through allowing non 1, 2, ..., ndists inits
    init_plan <- as.integer(init_plan)
  }

  # TODO handle non 1, ..., ndists ints

  shp$redistio_id <- as.character(seq_len(length.out = nrow(shp)))

  if (!adj_col %in% names(shp)) {
    adj <- geomander::adjacency(shp)
  } else {
    adj <- shp[[adj_col]]
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
          (!!rlang::sym(elect_cols[[i]]$dem) +
            !!rlang::sym(elect_cols[[i]]$rep))
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
  shp_proc <- prep_shp(shp, crs = opts$crs %||% def_opts$crs)
  shp_in <- shp_proc$all_cols
  shp <- shp_proc$no_list_cols

  # handle palettes ----
  palette <- prep_palette(palette, ndists)

  # prep layer colors ----
  if (!is.null(layers)) {
    layer_colors <- opts$layer_color %||% def_opts$layer_color
    if (length(layer_colors) == 1L) {
      layer_colors <- rep(layer_colors, length(layers))[seq_along(layers)]
    }
  }

  shp$fmt_pop <- scales::label_comma()(shp[[pop_col]])
  tot_pop <- sum(shp[[pop_col]])

  # prep hover ----
  shp_tb <- shp |>
    tibble::as_tibble()
  # sf::st_drop_geometry()
  # Using sf::st_drop_geometry causes a
  # Error in glue(str, .envir = .envir, .transformer = transformer, .cli = TRUE,  :
  # Expecting '}'
  # when used on a redist_map twice in a row

  hov <- hover_fn(
    shp_tb,
    pop = dplyr::starts_with('pop'),
    vap = dplyr::starts_with('vap')
  ) |>
    dplyr::bind_rows(.id = 'group') |>
    format_alarm_names()

  # prep layers -----
  layers <- prep_layers(layers, shp)

  # other prep ----

  tgt_pop <- sum(shp[[pop_col]]) / ndists
  min_pop <- ceiling(tgt_pop * (1 - pop_tol))
  max_pop <- floor(tgt_pop * (1 + pop_tol))
  tgt_pop <- as.integer(round(tgt_pop))
  pretty_bounds <- paste0(
    'Population must be in [',
    scales::label_comma()(min_pop),
    ', ',
    scales::label_comma()(max_pop),
    '].'
  )

  use_algorithms <- inherits(shp_in, 'redist_map') &&
    opts$use_algorithms %||% def_opts$use_algorithms &&
    rlang::is_installed('redist')

  use_planscore <- inherits(shp_in, 'redist_map') &&
    opts$use_planscore %||% def_opts$use_planscore &&
    rlang::is_installed('planscorer') &&
    planscorer::ps_has_key()

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
      shiny::tags$link(
        rel = 'stylesheet',
        type = 'text/css',
        href = 'assets/styles.css'
      )
    ),

    # draw panel ----
    bslib::nav_panel(
      title = 'draw',
      class = 'p-0',
      selection_html,
      bslib::page_fillable(
        class = 'p-0',
        bslib::layout_sidebar(
          border = FALSE,
          border_radius = FALSE,
          fillable = TRUE,
          class = 'p-0',
          sidebar = bslib::sidebar(
            # color selector
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
                  min = 0,
                  max = 1,
                  step = 0.05,
                  value = 0.9
                ),
                shiny::numericInput(
                  inputId = 'precinct_border',
                  label = 'Precinct border weight',
                  min = 0,
                  max = 100,
                  value = 0.5
                ),
                colourpicker::colourInput(
                  inputId = 'precinct_linecolor',
                  label = 'Precinct border color',
                  value = opts$border_color %||% def_opts$border_color
                ),
                icon = shiny::icon('palette'),
                style = 'min-height: 30vh'
              )
            )
          ),
          bslib::layout_sidebar(
            border = FALSE,
            class = 'p-0',
            bslib::card(
              # interactive mapper
              id = 'map-card',
              full_screen = TRUE,
              mapgl::maplibreOutput(
                outputId = 'map',
                height = opts$leaflet_height %||% def_opts$leaflet_height,
              )
            ),
            sidebar = bslib::sidebar(
              # details area
              position = 'right',
              id = 'map-right-sidebar',
              width = 300,
              bslib::navset_bar(
                id = 'tabRight',
                bslib::nav_panel(
                  title = 'Population',
                  gt::gt_output('tab_pop')
                ),
                bslib::nav_panel(title = 'Precinct', gt::gt_output('hover')),
                bslib::nav_menu(
                  title = 'More',
                  bslib::nav_panel(
                    'Download',
                    shiny::h5('Download assignment file'),
                    shiny::textInput(
                      'save_path',
                      label = 'Path to save assignment file',
                      value = opts$save_assignment_path %||%
                        def_opts$save_assignment_path
                    ),
                    shiny::selectizeInput(
                      'download_id',
                      'Select identifier column:',
                      choices = NULL,
                      selected = 'redistio_id',
                      multiple = FALSE
                    ),
                    shiny::downloadButton('save_plan', label = 'Export plan'),
                    shiny::h5('Download shapefile'),
                    shiny::textInput(
                      'save_shp_path',
                      label = 'Path to save shapefile',
                      value = opts$save_shape_path %||% def_opts$save_shape_path
                    ),
                    shiny::downloadButton(
                      'save_shp',
                      label = 'Export shapefile'
                    )
                  ),
                  bslib::nav_panel(
                    'Lock',
                    shiny::tags$style(lock_css),
                    if (!rlang::is_installed('shinyWidgets')) {
                      shiny::checkboxGroupInput(
                        inputId = 'locks',
                        choices = seq_len(ndists),
                        label = 'Lock districts',
                        selected = opts$locked_districts %||%
                          def_opts$locked_districts
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
                  bslib::nav_panel(
                    'Color from column',
                    color_from_columnUI('colorFromColumn')
                  ),
                  ,
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
    demographicsUI('demographics'),
    integrityUI('integrity'),
    electionsUI('elections'),
    if (use_algorithms) {
      algorithmsUI('algorithms', opts, def_opts, ndists, shp)
    } else {
      NULL
    },
    # planscore panel ----
    if (use_planscore) {
      bslib::nav_panel(
        title = 'PlanScore',
        bslib::page_fillable(
          planscoreUI('planscore')
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
    alg_pal <- scales::col_factor(palette = 'viridis', domain = NULL)
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
      Population = distr_pop(
        shp[[pop_col]],
        total = tot_pop,
        plan = init_plan,
        ndists = ndists
      ),
      Deviation = as.integer(
        distr_pop(
          shp[[pop_col]],
          total = tot_pop,
          plan = init_plan,
          ndists = ndists
        ) -
          c(0L, rep(tgt_pop, ndists))
      )
    )
    val <- shiny::reactiveVal(tab_pop_static)
    map_sub <- shiny::reactiveVal(shp)
    map_sub_in <- shiny::reactiveVal(shp_in)

    alg_plans_static <- tibble::tibble(
      draw = factor(),
      dev = double()
    )

    alg_plans <- shiny::reactiveVal(alg_plans_static)

    pal <- shiny::reactiveVal(
      as.character(palette)
    )

    # undo ----
    undo_l <- shiny::reactiveVal(undo_log(l = undo_init(10L), pl = init_plan))

    # draw panel ----

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
        mapgl::add_fill_layer(
          source = 'redistio',
          id = 'precinct_fill',
          fill_color = discrete_palette(palette, init_plan),
          fill_opacity = 0.9,
          fill_outline_color = '#00000000'
        ) |>
        mapgl::add_line_layer(
          id = 'precinct_border',
          source = 'redistio',
          line_color = opts$border_color %||% def_opts$border_color,
          line_width = 0.5
        )

      if (!is.null(hover_fn)) {
        base_map <- base_map |>
          mapgl::enable_shiny_hover(
            coordinates = FALSE,
            features = 'precinct_fill',
            layer_id = 'precinct_fill'
          )
      }

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
      base_map
    })

    shiny::observeEvent(input$map_feature_click, {
      clicked$map_feature_click <- input$map_feature_click
    })

    shiny::observeEvent(
      eventExpr = clicked$map_feature_click,
      handlerExpr = {
        click <- clicked$map_feature_click
        clicked$map_feature_click <- NULL
        if (is.null(click)) {
          return(NULL)
        }

        if (is.null(input$district_rows_selected)) {
          return(NULL)
        }

        if (as.integer(click$id) > nrow(shp)) {
          return(NULL)
        }

        idx <- which(shp$redistio_id == click$id)
        new_dist <- ifelse(
          input$district_rows_selected == 1,
          NA_integer_,
          input$district_rows_selected - 1L
        )
        if (
          isTRUE(redistio_curr_plan$pl[idx] %in% input$locks) ||
            isTRUE(new_dist %in% input$locks)
        ) {
          return(NULL)
        }

        redistio_curr_plan$pl[idx] <- new_dist

        undo_l(undo_log(undo_l(), redistio_curr_plan$pl))

        new_tb_pop <- val()
        new_tb_pop$Population <- distr_pop(
          shp[[pop_col]],
          total = tot_pop,
          plan = redistio_curr_plan$pl,
          ndists = ndists
        )
        new_tb_pop$Deviation <- as.integer(
          new_tb_pop$Population - c(0L, rep(tgt_pop, ndists))
        )
        val(new_tb_pop)

        mapgl::maplibre_proxy('map') |>
          update_shape_style(
            input$fill_column,
            pal(),
            redistio_curr_plan$pl,
            shp,
            input$fill_opacity,
            input$precinct_border,
            input$precinct_linecolor
          )
      }
    )

    shiny::observeEvent(
      list(input$fill_opacity, input$precinct_border, input$precinct_linecolor),
      {
        mapgl::maplibre_proxy('map') |>
          update_shape_style(
            input$fill_column,
            pal(),
            redistio_curr_plan$pl,
            shp,
            input$fill_opacity,
            input$precinct_border,
            input$precinct_linecolor
          )
      }
    )

    # district stats ----
    output$district <- DT::renderDT(
      {
        shiny::isolate(val()) |>
          DT::datatable(
            options = list(
              dom = 't',
              ordering = FALSE,
              scrollY = paste0(min(ndists * 6, 90), 'vh'),
              # scrollX = TRUE, # TODO make changeable
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
        proxy = dt_proxy,
        val(),
        rownames = FALSE,
        resetPaging = FALSE,
        clearSelection = 'none'
      )
    })

    shiny::outputOptions(output, 'district', suspendWhenHidden = FALSE)

    output$tab_pop <- gt::render_gt({
      val() |>
        # dplyr::slice(-1) |>
        dplyr::mutate(
          District = stringr::str_extract(.data$District, ' \\d+ ')
        ) |>
        gt::gt() |>
        gt::tab_style(
          style = gt::cell_fill(color = 'red'),
          locations = gt::cells_body(
            rows = (.data$Population > max_pop | .data$Population < min_pop) &
              !is.na(.data$District)
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

      mapgl::maplibre_proxy('map') |>
        update_shape_style(
          input$fill_column,
          pal(),
          redistio_curr_plan$pl,
          shp,
          input$fill_opacity,
          input$precinct_border,
          input$precinct_linecolor
        )

      new_tb_pop <- val()
      new_tb_pop$Population <- distr_pop(
        shp[[pop_col]],
        total = tot_pop,
        plan = redistio_curr_plan$pl,
        ndists = ndists
      )
      new_tb_pop$Deviation <- as.integer(
        new_tb_pop$Population - c(0L, rep(tgt_pop, ndists))
      )
      val(new_tb_pop)
    })

    # reactive mouseover
    hov_reac <- shiny::reactive({
      input$map_feature_hover
    })
    hov_reac_d <- shiny::debounce(hov_reac, 100)

    # precinct stats ----
    shiny::observeEvent(hov_reac_d(), {
      if (!is.null(hov_reac_d()) && hov_reac_d()$id <= nrow(shp)) {
        if (input$tabRight == 'Precinct') {
          output$hover <- gt::render_gt({
            # produce hover tables ----
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
      }
    })

    # downloader ----
    shiny::updateSelectizeInput(
      session,
      'download_id',
      choices = names(shp),
      selected = 'redistio_id',
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
        shiny::updateSelectizeInput(
          session,
          'fill_column',
          choices = c('District'),
          selected = NULL,
          server = FALSE
        )
        pal(as.character(palette_reactive()))
      } else if (input$fill_input == 'Elections') {
        shiny::updateSelectizeInput(
          session,
          'fill_column',
          choices = names(elect_cols),
          selected = names(elect_cols)[1],
          server = FALSE
        )
      } else {
        shiny::updateSelectizeInput(
          session,
          'fill_column',
          choices = demog_cols,
          selected = demog_cols[1],
          server = FALSE
        )
        # have to update pal depending on fill column, no new pal
      }
    })

    shiny::observeEvent(
      input$fill_column,
      {
        if (input$fill_input == 'Demographics') {
          if (input$fill_column %in% c('pop', 'vap', 'cvap')) {
            pal(
              mapgl::interpolate_palette(
                data = shp,
                column = input$fill_column,
                method = 'equal',
                n = length(as.character(
                  opts$palette_pop %||% def_opts$palette_pop
                )),
                colors = as.character(
                  opts$palette_pop %||% def_opts$palette_pop
                ),
                na_color = opts$na_color %||% def_opts$na_color
              )
            )
          } else {
            pal(
              percent_palette(
                column = input$fill_column,
                palette = as.character(
                  opts$palette_pct %||% def_opts$palette_pct
                ),
                na_color = opts$na_color %||% def_opts$na_color
              )
            )
          }
        } else if (input$fill_input == 'Elections') {
          pal(
            percent_palette(
              column = input$fill_column,
              palette = as.character(
                opts$palette_party %||% def_opts$palette_party
              ),
              na_color = opts$na_color %||% def_opts$na_color
            )
          )
        }
        mapgl::maplibre_proxy('map') |>
          update_shape_style(
            input$fill_column,
            pal(),
            redistio_curr_plan$pl,
            shp,
            input$fill_opacity,
            input$precinct_border,
            input$precinct_linecolor
          )
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
          vapply(
            seq_len(ndists),
            function(i) {
              input[[paste0('color_', i)]]
            },
            FUN.VALUE = character(1)
          )
        )
        pal(palette_reactive())

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
          mapgl::maplibre_proxy('map') |>
            update_shape_style(
              input$fill_column,
              pal(),
              redistio_curr_plan$pl,
              shp,
              input$fill_opacity,
              input$precinct_border,
              input$precinct_linecolor
            )
        }
      }
    )

    # tools mini panel ----
    discontiguousServer(
      'discontiguous',
      redistio_curr_plan,
      adj,
      shp,
      shiny::reactive(mapgl::maplibre_proxy('map'))
    )

    unassignedServer(
      'unassigned',
      redistio_curr_plan,
      shp,
      shiny::reactive(mapgl::maplibre_proxy('map'))
    )

    color_from_fileServer(
      'colorFromFile',
      redistio_curr_plan,
      shp,
      shiny::reactive(mapgl::maplibre_proxy('map')),
      input$fill_column,
      input$fill_opacity,
      input$precinct_border,
      input$precinct_linecolor,
      pal,
      undo_l,
      undo_log,
      val,
      tot_pop,
      ndists,
      tgt_pop
    )

    color_from_columnServer(
      'colorFromColumn',
      redistio_curr_plan,
      shp,
      shiny::reactive(mapgl::maplibre_proxy('map')),
      input$fill_column,
      input$fill_opacity,
      input$precinct_border,
      input$precinct_linecolor,
      pal,
      undo_l,
      undo_log,
      val,
      tot_pop,
      ndists,
      tgt_pop
    )

    # planscore nav panel ----
    planscoreServer(
      'planscore',
      redistio_curr_plan,
      shp
    )

    demographicsServer('demographics', shp, redistio_curr_plan)
    integrityServer('integrity', shp, redistio_curr_plan, adj, split_cols)
    electionsServer('elections', shp_tb, redistio_curr_plan)

    if (use_algorithms) {
      algorithmsServer(
        'algorithms',
        session,
        shp,
        shp_in,
        redistio_curr_plan,
        ndists,
        palette_reactive,
        input$fill_opacity,
        input$precinct_border,
        input$precinct_linecolor,
        input$fill_column,
        leaf_tiles,
        layers,
        layer_colors,
        opts,
        def_opts,
        val,
        tot_pop,
        tgt_pop,
        pop_col,
        undo_l
      )
    }
  }

  # run app ----
  shiny::shinyApp(ui = ui, server = server)
}
