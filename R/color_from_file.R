color_from_fileUI <- function(id) {
  shiny::tagList(
    shiny::fileInput(
      inputId = shiny::NS(id, 'file'),
      label = 'Select a file',
      multiple = 'FALSE',
      accept = '.csv'
    ),
    shiny::textOutput(shiny::NS(id, 'status'))
  )
}

color_from_fileServer <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$file, {

      dat <- read.csv(file = input$file$datapath)
      output$status <- shiny::renderText({
        print(head(dat))
        input$file$name
      })
      #req(input$file)
    })
  })
}
