#
# This is a Shiny web application to access the diet database from Oshima and Leaf 2018. It allows the user to select a predator or prey of interest and specifiy the diet metric and taxonomic level that should be used to display the information. This will generate a network on the right side of the first page and a data table that can be downloaded on the second page.
#
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(igraph)
library(networkD3)

Diet <- read.csv("Diet.sub.csv",
                 stringsAsFactors = F,
                 header = T)

pred.name <- unique(Diet$Predator_Scientific_Name)
prey.tax <- unique(Diet$Lowest_Taxonomic_Identification_Prey)

#Progress bar function
compute_data <- function(updateProgress = NULL) {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:10) {
    Sys.sleep(0.25)
    
    # Compute new row of data
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <-
        paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
    }
    
    # Add the new row of data
    dat <- rbind(dat, new_row)
  }
  
  dat
}
####

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Diet Database"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Parameters",
        tabName = "paramaters",
        icon = shiny::icon("bar-chart")
      ),
      menuItem(
        "Data Table",
        icon = shiny::icon("table"),
        tabName = "datatable"
      )
    ),
    menuItem(
      "Help",
      tabName = "help",
      icon = shiny::icon("question")
    )
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "paramaters",
            fluidRow(
              shiny::column(
                width = 4,
                
                shinydashboard::box(
                  title = "Predator",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  shiny::helpText("Select a predator to view its connections and prey items:"),
                  shiny::selectInput(
                    "pred",
                    shiny::h5("Predator Scientific Name:"),
                    c(NA, pred.name)
                  )
                ),
                
                shinydashboard::box(
                  title = "Prey",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  shiny::helpText("Select a prey taxa to view its connections and predators:"),
                  shiny::selectInput("prey",
                                     shiny::h5("Prey Taxa:"),
                                     c(NA, prey.tax))
                ),
                
                shinydashboard::box(
                  title = "Diet Metric",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  shiny::helpText("Select a diet metric to use:"),
                  shiny::selectInput(
                    "dietmetric",
                    shiny::h5("Diet Metric:"),
                    c(
                      "Frequency of Occurrence" = "Frequency_of_Occurrence",
                      "Wet Weight" = "Weight",
                      "Dry Weight" = "Dry_Weight",
                      "Volume" = "Volume",
                      "Index of Relative Importance" = "IRI",
                      "Index of Caloric Importance" = "ICI",
                      "Number" = "Number"
                    )
                  )
                ),
                
                shinydashboard::box(
                  title = "Taxonomic Level",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  shiny::helpText("Select a taxonomic level of nodes:"),
                  shiny::selectInput(
                    "nodetax",
                    shiny::h5("Taxonomic Level:"),
                    c(
                      "Order" = "Order",
                      "Family" = "Family",
                      "Genus" = "Genus",
                      "Species" = "Species"
                    )
                  )
                ),
                shinydashboard::box(
                  title = "Generate Network",
                  status = "primary",
                  solidHeader = T,
                  collapsible = T,
                  width = NULL,
                  actionButton("makenet", "Generate")
                )
              ),
              
              #Area for network to be displayed
              shiny::column(
                width = 8,
                shinydashboard::box(
                  title = "Trophic Network",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = NULL,
                  forceNetworkOutput("netplot", height = "700px")
                )
              )
            )),
    tabItem(tabName = "datatable",
            fluidRow(
              column(
                width = 4,
                shinydashboard::box(
                  title = "Data Table",
                  status = "primary",
                  solidHeader = T,
                  collapsible = T,
                  width = NULL,
                  helpText("Select variables to be displayed in data table output:"),
                  checkboxGroupInput(
                    "vars",
                    h5("Data Table Variables"),
                    c(
                      "Title" = "Title",
                      "Author" = "Authors",
                      "Year" = "Year",
                      "Predator Scientific Name" = "Predator_Scientific_Name",
                      "Predator Common Name" = "Predator_Common_Name",
                      "Sample Size" = "n",
                      "Prey Taxa" = "Lowest_Taxonomic_Identification_Prey"
                    )
                  ),
                  actionButton("makedt", "Generate Table")
                ),
                shinydashboard::box(
                  title = "Download Data Table",
                  status = "primary",
                  solidHeader = T,
                  width = NULL,
                  downloadButton("Download", "Download")
                )
              ),
              column(
                width = 8,
                shinydashboard::box(
                  title = "Network Data Table",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  tableOutput("vardt")
                )
              )
            )),
    tabItem(tabName = "help",
            fluidRow(
              shinydashboard::box(
                title = "Help Page",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                h3("Tips for running the network app"),
                br(),
                h4(
                  "This app allows a user to select a predator or prey, the diet metric used to report the interactions and the taxonomic level the nodes are shown at. Note that prey were identified to varrying taxonomic levels depending on the studies. If you get an error that says arguments imply differing number of rows, try selecting a different diet metric. The diet metrics reported for a selected predator or prey can be seen in the data table. Please only select either a predator or prey to query."
                ),
                br(),
                h4("For questions please email megumi.oshima@usm.edu."),
                h4("2018 By Meg Oshima")
                
                
              )
            ))
  ))
)


#Server function
server <- function(input, output, session) {
  #Reactively filter and select the rows and columns specified by the inputs
  
  network.data <- eventReactive(input$makenet, {
    Diet %>% filter(
      Predator_Scientific_Name == input$pred |
        Lowest_Taxonomic_Identification_Prey == input$prey
    ) %>% select(
      paste(input$nodetax, "Predator", sep = "_"),
      Class_Predator,
      paste(input$nodetax, "Prey", sep = "_"),
      Class_Prey,
      input$dietmetric
    ) %>% rename(
      "SourceName" = paste(input$nodetax, "Predator", sep = "_"),
      "SourceClass" = Class_Predator,
      "TargetName" = paste(input$nodetax, "Prey", sep = "_"),
      "TargetClass" = Class_Prey,
      "Weight" = input$dietmetric
    ) %>% na.omit()
    
  })
  
  output$netplot <- renderForceNetwork({
    edgelist <- network.data()
    
    #create graph object with igraph package
    ig <-
      igraph::simplify(igraph::graph_from_data_frame(edgelist[, c(1, 3, 5)], directed =
                                                       TRUE))
    #Identify node IDs (in numbers) and make sure they start at 0 which is necessary for networkd3
    SourceID <- TargetID <- c()
    for (i in 1:nrow(edgelist)) {
      SourceID[i] <- which(edgelist[i, 1] == V(ig)$name) - 1
      TargetID[i] <- which(edgelist[i, 3] == V(ig)$name) - 1
    }
    
    #Create edgeList that contains source and target nodes and edge weights and nodeList that contains the nodeID and name
    edgeList <- cbind(edgelist, SourceID, TargetID)
    
    nodeList <- data.frame(ID = c(0:(igraph::vcount(ig) - 1)),
                           nName = igraph::V(ig)$name)
    
    #Determine and assign groups based on class
    preddf <-
      data.frame(SciName = edgelist[, 1], class = edgelist[, 2])
    preydf <-
      data.frame(SciName = edgelist[, 3], class = edgelist[, 4])
    groupsdf <- rbind(preddf, preydf)
    groupsdf <- groupsdf %>% mutate(SciName = as.character(SciName),
                                    class = as.character(class))
    nodeGroup <- c()
    for (i in 1:nrow(nodeList)) {
      index <- which(groupsdf[, 1] == nodeList$nName[i])
      nodeGroup[i] <- groupsdf[index[1], 2]
    }
    
    nodeList <-
      cbind(nodeList,
            nodeGroup)
    
    #Progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Generating your network...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute the new data, and pass in the updateProgress function so
    # that it can update the progress indicator.
    compute_data(updateProgress)
    #Create network object
    networkD3::forceNetwork(
      Links = edgeList,
      # data frame that contains info about edges
      Nodes = nodeList,
      # data frame that contains info about nodes
      Source = "SourceID",
      # ID of source node
      Target = "TargetID",
      # ID of target node
      Value = "Weight",
      # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
      NodeID = "nName",
      # value from the node list (data frame) that contains node
      Group = "nodeGroup",
      # value from the node list (data frame) that contains value we want to use for node color
      fontSize = 25,
      opacity = 0.85,
      zoom = TRUE,
      # ability to zoom when click on the node
      opacityNoHover = 0.4,
      # opacity of labels when static
      height = "1000px",
      width = "1000px",
      bounded = FALSE,
      charge = -4,
      linkDistance = 200
    )
    
  })
  
  #Make a data table with the variables selected in the check boxes for the predator or prey that is selected. This data table can be downloaded as a .csv file.
  
  observeEvent(input$makedt, {
    output$vardt <- renderTable({
      Diet %>% filter(
        Predator_Scientific_Name == input$pred |
          Lowest_Taxonomic_Identification_Prey == input$prey
      ) %>% select(
        input$vars,
        "Frequency_of_Occurrence",
        "Dry_Weight",
        "Weight",
        "IRI",
        "Number",
        "ICI",
        "Volume"
      )
    }, row.names = FALSE)
    
    output$Download <- downloadHandler(
      filename = function() {
        paste("trophicnetwork", ".csv", sep = "")
      },
      content = function(file) {
        sub.diet <-
          Diet %>% filter(
            Predator_Scientific_Name == input$pred |
              Lowest_Taxonomic_Identification_Prey == input$prey
          ) %>% select(
            input$vars,
            "Frequency_of_Occurrence",
            "Dry_Weight",
            "Weight",
            "IRI",
            "Number",
            "ICI",
            "Volume"
          )
        write.csv(sub.diet, file, row.names = F)
      }
    )
    outputOptions(output, 'Download', suspendWhenHidden = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
