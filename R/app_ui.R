#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme page_navbar nav_panel card_image
#' @importFrom thematic thematic_shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(

      fillable = FALSE,

      theme = bs_theme(bootswatch = "simplex",
                       warning = "#83A096",
                       primary = "#174936",
                       secondary = "#C19D7B",
                       fg = "black",
                       bg = "white"
      ),
      # thematic_shiny(),

      ##

      title = tagList(#icon("home"),
        img(src="www/Inbestsoil-simbolo.png", width = "50px")
      ),

      footer = tagList(
        hr(),
        layout_column_wrap(
          width = 1/3,
          "Version 0.1.0 - September 2026",
          p(
            align = "center",
            "Link to the manual"
            # tags$a(
            #   href = "https://docs.google.com/document/d/1JQ7n-8D-NPpI-jW8znUBNIqN7W-wut0thtNoIJLIVT8/edit?usp=sharing",
            #   "User manual"
            # )
            # downloadLink("downloadManual", "User manual")
          ),
          p(
            align = "right",
            # "Report an issue",
            tags$a(
              "Report an Issue",
              href = "mailto:alberto.garre@upct.es?subject=%5BR4EU%5D%20SoilDiverAgroAppu&body=First%20Name%3A%0D%0ALast%20Name%3A%0D%0AOrganization%3A%0D%0ACountry%3A%0D%0ASoftware Version%3A%201.0.1%0D%0AFeedback%20or%20Issue%3A"
            )
          )
        )
      ),
      # tabPanel("Home",
      #          # mod_welcome_ui("welcome_1")
      # ),
      nav_panel(title = "Calculator",
               mod_intervalos_ui("intervalos_1")
      ),
      nav_panel(title = "Documentation",
                "aa"
               # mod_doc_ui("doc_1")
      ),
      nav_panel(title = "About",
                card_image(
                  file = system.file("extdata", "FUNDING InBestSoil HEADER.png", package = "jorjulator")
                ),
                card_image(
                  file = system.file("extdata", "banner.png", package = "jorjulator")
                )
               # mod_about_ui("about_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "jorjulator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
