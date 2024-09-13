# NC Wildlife Commission Science Base
# prototype
# v0.1
# 09/12/24
# Scott K. Anderson
# NC Wildlife Resources Commission

# This is a prototype implementation of a North Carolina version of the 
# USGS ScienceBase (https://www.sciencebase.gov/catalog/).

# It is intended to provide a platform for storing and documenting metadata
# and related materials for projects and
# publications produced by the NC Wildlife Resources Commission

if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages(
  "shinyWidgets", repos = "http://cran.us.r-project.org")

if(!require(shinydashboard)) install.packages(
  "shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(mongolite)) install.packages(
  "mongolite", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages(
  "htmltools", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages(
  "leaflet", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages(
  "jsonlite", repos = "http://cran.us.r-project.org")
# if(!require(shinyjs)) install.packages(
#   "shinyjs", repos = "http://cran.us.r-project.org")

source("utils.r")

# MAP CONSTANTS
nc_center_lat = 35.5
nc_center_lng = -79.2
nc_center_zoom = 7

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "NC Wildlife ScienceBase",
    titleWidth = 350
    ),
  dashboardSidebar(
    width = 350,
    sidebarSearchForm(
      textId = "searchText",
      buttonId = "searchButton",
      label = "Search Database",
      icon = shiny::icon("search")
    ),
    div(htmlOutput("searchResults"))
  ),
  dashboardBody(
    tags$link(rel="stylesheet", type = "text/css", href = "custom.css"),
    #boxes need to be put in a row or column
    fluidRow(
      id = "title-row",
      h1(
        htmlOutput(
        "entityTitle"
        )
      )
    ),
    fluidRow(
      column(
        width = 9,
        box(
          title = "Summary",
          color = "olive",
          width = 9,
          htmlOutput("entitySummary")
        ),
        infoBox(
          width = 3,
          "downloadFile"
        ),
        box(
          title = "Purpose",
          color = "olive",
          width = 12,
          htmlOutput("entityPurpose")
        ),
        box(
          title = "Citation",
          color = "olive",
          width = 12,
          htmlOutput(
            "entityCitation"
          )

        )
      ),
      column(
        width = 3,
        box(
          width = NULL,
          solidHeader = TRUE,
          leafletOutput("locMap", height = 400)

        ),
        box(
          width = NULL,
          solidHeader = TRUE,
          title = "Tags",
          htmlOutput(
            "entityTags"
          )
        )
      )

    )
  )
)


########################################################################
## Begin server code

server <- function(input, output, session) {
  ## GET SERVER URL PARAMETERS (if they exist)

  observe({
    query <- parseQueryString(session$clientData$url_search)

  if (!is.null(query[['id']])) {
    rv_entity$id <- query['id']
  }

  shiny::updateQueryString("", mode = "replace")
})

  rv_entity <- reactiveValues(id = NULL, content = NULL)

  searchResults <- reactive(
    {
      # when search changes, get hits.
      if (nchar(input$searchText) > 0){

          pipeline <- sprintf(
          paste0(
            '[',
            '{"$match" : {"$or":[',
            '{"title" : { "$regex" : "%s", "$options" : "i"}},',
            '{"description" : { "$regex" : "%s", "$options" : "i"}},',
            '{"body" : { "$regex" : "%s", "$options" : "i"}},',
            '{"subtitle" : { "$regex" : "%s", "$options" : "i"}},',
            '{"tags.name" : { "$regex" : "%s", "$options" : "i"}},',
            '{"files.name" : { "$regex" : "%s", "$options" : "i"}}',
            ']}},',
            '{"$project":{"title" : 1, "id" : 1}},',
            '{"$sort" : {"title" : 1}}',
            ']'
          ),
          input$searchText,
          input$searchText,
          input$searchText,
          input$searchText,
          input$searchText,
          input$searchText
        )
          
        r <- ncsb$aggregate(pipeline)

    }
    }
  )

  # OUTPUT SEARCH RESULTS
  output$searchResults <- renderUI({
    req(searchResults())
    
    if (length(searchResults())>0){
      titles <- searchResults()$title
      ids <- searchResults()$id
      lapply(1:length(searchResults()), function(i) {
            div(class="foundItem",
            a(
              href = paste0("/?id=",ids[i]),
              paste(titles[i])
              ))
          })
    } else {
      div("no results")
    }
  })

  # whenever entity changes, populate fields
  observeEvent(
    rv_entity$id,
    {
      if (!is.null(rv_entity$id)){
        
        filter <- sprintf(
            '{"id" : "%s"}',
            rv_entity$id
          )
        r <- ncsb$find(
            filter,
            paste0(
              '{',
              '"title" : 1,',
              '"summary" : 1,',
              '"files" : 1,',
              '"purpose" : 1,',
              '"citation" : 1,',
              '"link.url" : 1,',
              '"spatial" : 1',
            '}'
            )
          )
        
        # rec <- jsonlite::read_json(r)
        # print(rec)


        output$entityTitle <- renderUI(HTML(r$title))
        output$entitySummary <- renderUI(HTML(r$summary))
        # output$downloadFile <- renderInfoBox({
        #   infoBox(
        #     "Download",
        #     "",
        #     icon = icon("download"),
        #     color = "green"
        #     )
        # })
        output$entityPurpose <- renderUI(HTML(r$purpose))
        output$entityCitation <- renderUI(HTML(r$citation))
        output$entityTags <- renderUI(HTML(""))

        output$locMap <- renderLeaflet({
          leaflet() %>%
          setView(
            lat = nc_center_lat,
            lng = nc_center_lng,
            zoom = nc_center_zoom
          ) %>%
          addProviderTiles(
            "OpenStreetMap.Mapnik",
            options = providerTileOptions(opacity = 1),
            group = "Street Map"
          )
        })

      }
    })

  # output$entityDetails <- renderUI({
  #   print("entity changed")
  #   print(rv_entity$id)
  #   if (!is.null(rv_entity$id)){

      
  #     print(r)
  #     HTML(paste0(
  #       "<h1>",
  #       r$title,
  #       "</h1>",
  #       "<div>",
  #       r$summary,
  #       "</div>"
  #     ))
  #   }
  # })

}

shinyApp(ui, server)