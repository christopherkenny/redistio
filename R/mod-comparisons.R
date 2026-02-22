#' Comparisons Module
#'
#' @param id module id
#'
#' @return shiny UI
#' @noRd
comparisonsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'comparisons',
    class = 'p-0',
    bslib::page_fillable(
      class = 'p-0',
      bslib::layout_sidebar(
        fillable = TRUE,
        border = FALSE,
        sidebar = bslib::sidebar(
          width = 300,
          shiny::h4('Compare plans'),
          shiny::selectInput(
            inputId = ns('stat_choice'),
            label = 'Select statistic',
            choices = NULL
          ),
          shiny::actionButton(
            inputId = ns('refresh'),
            label = 'Refresh comparison',
            icon = shiny::icon('arrows-rotate')
          ),
          shiny::downloadButton(
            outputId = ns('download_plot'),
            label = 'Download plot'
          ),
          shiny::h4('Remember this plan'),
          shiny::textInput(
            inputId = ns('plan_name'),
            label = 'Plan name',
            value = 'My Plan'
          ),
          shiny::actionButton(
            inputId = ns('remember_plan'),
            label = 'Remember this plan',
            icon = shiny::icon('floppy-disk')
          )
        ),
        bslib::card(
          full_screen = TRUE,
          shiny::uiOutput(ns('plot_or_placeholder'))
        )
      )
    )
  )
}

#' Comparisons Module Server
#'
#' @param id module id
#' @param parent_session parent session
#' @param plans_reactive reactiveVal containing redist_plans object
#' @param plans_fn function to add reference plan
#' @param shp_in sf/redist_map object
#' @param redistio_curr_plan reactive plan
#' @param ndists number of districts
#' @param opts options
#' @param def_opts default options
#'
#' @return shiny server
#' @noRd
comparisonsServer <- function(id, parent_session,
                              plans_reactive, plans_fn, shp_in,
                              redistio_curr_plan, ndists,
                              opts, def_opts) {
  shiny::moduleServer(id, function(input, output, session) {
    # stores the plans with the current plan added as reference
    plans_with_ref <- shiny::reactiveVal(NULL)

    # stores classification of stats
    stat_types <- shiny::reactiveVal(list())

    # the current plot for download
    current_plot <- shiny::reactiveVal(NULL)

    # columns to hide from the stat dropdown
    hidden_cols <- c('chain')

    output$plot_or_placeholder <- shiny::renderUI({
      if (is.null(plans_with_ref())) {
        shiny::div(
          style = 'text-align: center; padding-top: 40vh;',
          shiny::p('Click "Refresh comparison" to generate a plot.')
        )
      } else {
        shiny::plotOutput(
          outputId = session$ns('comparison_plot'),
          height = '100%'
        )
      }
    })

    shiny::observeEvent(input$refresh, {
      shiny::withProgress(message = 'Computing comparison...', value = 0, {
        pl <- plans_reactive()
        shiny::req(pl)

        curr_plan <- redistio_curr_plan$pl
        shiny::req(curr_plan)

        shiny::incProgress(0.3, detail = 'Adding reference plan')

        # add current plan as reference using plans_fn
        pl_with_ref <- plans_fn(
          pl,
          ref_plan = curr_plan,
          map = shp_in,
          name = 'Current'
        )
        plans_with_ref(pl_with_ref)

        shiny::incProgress(0.4, detail = 'Classifying statistics')

        # identify available statistics
        pl_tb <- tibble::as_tibble(pl_with_ref)
        skip_cols <- c('draw', 'district', hidden_cols)
        candidate_cols <- setdiff(names(pl_tb), skip_cols)

        # only keep numeric columns
        candidate_cols <- candidate_cols[vapply(
          candidate_cols,
          function(col) is.numeric(pl_tb[[col]]),
          logical(1)
        )]

        # classify as plan-level or district-level
        types <- list()
        for (col in candidate_cols) {
          grouped <- split(pl_tb[[col]], pl_tb$draw)
          is_plan_level <- all(vapply(
            grouped,
            function(x) length(unique(x)) == 1L,
            logical(1)
          ))
          types[[col]] <- if (is_plan_level) 'plan' else 'district'
        }
        stat_types(types)

        # build choices with labels
        choices <- names(types)
        names(choices) <- vapply(choices, function(nm) {
          paste0(nm, ' (', types[[nm]], '-level)')
        }, character(1))

        shiny::updateSelectInput(
          session, 'stat_choice',
          choices = choices,
          selected = if (length(choices) > 0) choices[[1]] else NULL
        )

        shiny::incProgress(0.3, detail = 'Done')
      })
    })

    output$comparison_plot <- shiny::renderPlot({
      pl <- plans_with_ref()
      shiny::req(pl)
      shiny::req(input$stat_choice)

      types <- stat_types()
      stat <- input$stat_choice
      stat_type <- types[[stat]]
      shiny::req(stat_type)

      plot_theme <- opts$plot_theme %||% def_opts$plot_theme

      if (stat_type == 'district') {
        plot_geom <- opts$plot_geom %||% def_opts$plot_geom
        plot_ref_geom <- opts$plot_ref_geom %||% def_opts$plot_ref_geom

        geom_args <- list(plans = pl, qty = rlang::sym(stat), geom = plot_geom)
        if (!is.null(plot_ref_geom)) {
          geom_args$ref_geom <- plot_ref_geom
        }
        p <- do.call(redist::redist.plot.distr_qtys, geom_args) + plot_theme
      } else {
        # plan-level: summarize then histogram
        pl_summary <- pl |>
          dplyr::group_by(.data$draw) |>
          dplyr::summarize(
            !!stat := dplyr::first(!!rlang::sym(stat)),
            .groups = 'drop'
          )
        p <- redist::redist.plot.hist(pl_summary, !!rlang::sym(stat)) + plot_theme
      }

      current_plot(p)
      p
    })

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        paste0('redistio-comparison-', input$stat_choice, '.png')
      },
      content = function(file) {
        p <- current_plot()
        shiny::req(p)
        ggplot2::ggsave(
          filename = file, plot = p,
          width = 10, height = 7, dpi = 300
        )
      }
    )

    shiny::observeEvent(input$remember_plan, {
      pl <- plans_reactive()
      shiny::req(pl)

      curr_plan <- redistio_curr_plan$pl
      shiny::req(curr_plan)

      plan_name <- input$plan_name
      if (is.null(plan_name) || nchar(plan_name) == 0) {
        plan_name <- 'My Plan'
      }

      updated_plans <- plans_fn(
        pl,
        ref_plan = curr_plan,
        map = shp_in,
        name = plan_name
      )
      plans_reactive(updated_plans)
    })
  })
}
