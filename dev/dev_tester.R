devtools::load_all(".")

app = draw(dc, init_plan=dc$ward)
shiny::runApp(app)
