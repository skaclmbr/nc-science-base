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
if(!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")
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

sidebar <- dashboardSidebar(
    width = 350,
    menuItem(
      "Search",
      tabName = "search",
      icon = shiny::icon("search")
      ),
    menuItem(
      "Categories/Tags",
      tabName = "categories",
      icon = shiny::icon("list")
    ),
    menuItem(
      "Research, Reports, and Data",
      tabName = "rrd",
      icon = shiny::icon("file")
    )
  )

body <- dashboardBody(
    ## SEARCH FORM
    tabItem(
      tabName = "search",
      sidebarSearchForm(
        textId = "searchText",
        buttonId = "searchButton",
        label = "Search Database",
        icon = shiny::icon("search")
      ),
      div(htmlOutput("searchResults"))
    ),
    ## CATEGORY/TAGS list
    tabItem(
      tabName = "categories",
      h2("Categories/Tags")
    ),
    ## REPORT DETAILS
    tabItem(
      tabName = "rrd",
      tags$link(rel="stylesheet", type = "text/css", href = "custom.css"),
      #boxes need to be put in a row or column
      fluidRow(
        id = "title-row",
        h1(
          htmlOutput("entityTitle")
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
          box(
            title = "Files",
            color = "olive",
            width = 3,
            htmlOutput(
              "entityFiles"
            )
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
            title = "Related Items",
            htmlOutput(
              "entityRelatedItems"
            )
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


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "NC Wildlife ScienceBase",
    titleWidth = 350
    ),
  sidebar,
  body
)


########################################################################
## Begin server code

server <- function(input, output, session) {
 
  ##########################################################################
  # WATCH FOR URL PARAMETER CHANGES
  ## GET SERVER URL PARAMETERS (if they exist)

  observe({
    query <- parseQueryString(session$clientData$url_search)

  if (!is.null(query[['id']])) {
    rv_entity$id <- query['id']
  } else if (!is.null(query[['tag']])) {
    rv_tag$id <- query['tag']
  }

  shiny::updateQueryString("", mode = "replace")
})

  rv_entity <- reactiveValues(id = NULL)

  rv_tag <- reactiveValues(id = NULL)

 
  ##########################################################################
  # OUTPUT SEARCH RESULTS
  searchResults <- reactive(
    {
      # clear out previous search
      # output$searchResults <- renderUI({})
      # when search changes, get hits.
      if (nchar(input$searchText) > 0){
        st <- input$searchText

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
          st,
          st,
          st,
          st,
          st,
          st
        )
        r <- ncsb$aggregate(pipeline)

    }
    }
  )

  output$searchResults <- renderUI({
    req(searchResults())
    
    if (length(searchResults())>0){
      titles <- searchResults()$title
      ids <- searchResults()$id
      # print(ids)
      # print(titles)
      lapply(1:length(searchResults()), function(i) {
            div(class="foundItem",
            a(
              # href = paste0("/?id=",ids[i]), # testing
              href = paste0("/nc_science_base/?id=",ids[i]), # production
              paste(titles[i])
              ))
          })
    } else {
      div("no results")
    }
  })

  ##########################################################################
  # whenever entity changes, populate fields
  observeEvent(
    rv_entity$id,
    {
      if (!is.null(rv_entity$id)) {
        
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
              '"tags" : 1,',
              '"relatedItems" : 1,',
              '"spatial" : 1',
            '}'
            )
          )
        
        # parse out tags
        tags <- as.data.frame(r$tags)
        tnames <- tags$name
        output$entityTags <- renderUI({
          lapply ( 1 : length(tnames), function(i){
            print(tnames[i])
            HTML(
              paste0(
                '<a href="',
                '/nc_science_base/?tag=', # PRODUCTION
                # '/?tag=', # TESTING
                tnames[i],
                '"">',
                tnames[i],
                '</a>, '
                )
              )
          } )
          
        })
        
        # parse out related items
        ri <- as.data.frame(r$relatedItems)
        rinames <- ri$name
        riids <- ri$id
        print(ri$name)
        output$entityRelatedItems <- renderUI({
          lapply ( 1 : length(rinames), function(i){
            # print(names[i])
            span(
                a(
                  href = paste0(
                    '/nc_science_base/?tag=', # PRODUCTION 
                    # '/?id=', # TESTING 
                    riids[i]
                  ),
                  rinames[i]
                )
                )
          } )
          
        })

        # parse files
        f <- as.data.frame(r$files)
        fnames <- f$name
        furls <- f$url
        ftitles <- f$title
        ftypes <- f$type
        fsizes <- f$size
        output$entityFiles <- renderUI({
          lapply( 1 : length(furls), function(i){
            div(
              class="file-item",
              a(
                href=furls[i],
                target="_blank",
                paste0(
                  fnames[i],
                  " (",
                  fsizes[i]/1000,
                  "MB)"
                  )
              )
            )
          })
        })

        output$entityTitle <- renderUI(HTML(r$title))
        output$entitySummary <- renderUI(HTML(r$summary))
        output$downloadFile <- renderInfoBox(
          infoBox(
            "Download",
            color="green",
            a(
              href="https://drive.google.com/file/d/1NcWhy1Lm0IzFu7lBBeidpa3pPsS_1XjT/view?usp=drive_link",
              target="_blank",
              "Report")
          )
        )
        output$entityPurpose <- renderUI(HTML(r$purpose))
        output$entityCitation <- renderUI(HTML(r$citation))

        output$locMap <- renderLeaflet({
          # s <- r$spatial
          # print (s)

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