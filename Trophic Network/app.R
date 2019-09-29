#
# This is a Shiny web application to access the diet database from Oshima and Leaf 2018. It allows the user to select a predator or prey of interest and specifiy the diet metric and taxonomic level that should be used to display the information. This will generate a network on the right side of the first page and a data table that can be downloaded on the second page.

###If you have trouble deploying the app, try running app in rstudio then using the publish button at top right of viewer screen to deploy.

#
#Currently hosted at: https://megumi-oshima.shinyapps.io/diet/

## To publish on shinyapps.io
#library(rsconnect)
#rsconnect::deployApp("C:/Users/w986430/Dropbox/Diet.Database")

library(shiny)
library(shinydashboard)
library(dplyr)
library(igraph)
library(networkD3)
library(shinyjs)
library(tidyr)
library(tidyverse)

Diet <- read.csv("diet.tidy.csv",
                 stringsAsFactors = F,
                 header = T)

pred.name <- unique(Diet$Predator_Scientific_Name)
prey.tax <- unique(Diet$Lowest_Taxonomic_Identification_Prey)
prey.tax <- sort(prey.tax)

#Get diet metrics for selected species
get_metric <- function(df,sp,pred){
  metrics <- df %>%
    filter(if (pred == TRUE) 
      str_detect(Predator_Scientific_Name, sp) else
        str_detect(Lowest_Taxonomic_Identification_Prey, sp)
    ) %>%
    select(Frequency_of_Occurrence,
           Dry_Weight,
           Weight,
           IRI,
           Number,
           ICI,
           Volume) %>%     
    rename("Frequency of Occurrence" = Frequency_of_Occurrence,
           "Dry Weight" = Dry_Weight,
           "Weight" = Weight,
           "IRI" = IRI,
           "Number" = Number,
           "ICI" = ICI,
           "Volume" = Volume
    ) %>%
    discard( ~ all(is.na(.x))) %>%
    names()
  return(metrics)
}

get_taxlvl <- function(df, sp, metric, pred) {
  taxa <-  df %>%
    filter(
      if (pred == TRUE)
        str_detect(Predator_Scientific_Name, sp)
      else
        str_detect(Lowest_Taxonomic_Identification_Prey, sp)
    ) %>%
    select(metric, Order_Prey, Family_Prey, Genus_Prey, Species_Prey) %>%
    rename(
      "Order" = Order_Prey,
      "Family" = Family_Prey,
      "Genus" = Genus_Prey,
      "Species" = Species_Prey
    ) %>%
    select(-1) %>%
    discard(~ all(is.na(.x))) %>%
    names()
  
  return(taxa)
  
}

make_network <- function(df, sp, metric, pred) {
  
  temp <- df %>% filter(
    if (pred == TRUE)
      str_detect(Predator_Scientific_Name, sp)
    else
      str_detect(Lowest_Taxonomic_Identification_Prey, sp)
  ) %>% select(
    Predator_Scientific_Name,
    Family_Predator,
    Kingdom_Prey,
    Class_Prey,
    Order_Prey,
    Family_Prey,
    Genus_Prey,
    Species_Prey,
    metric,
    Lowest_Tax_Level
  ) %>% rename(
    "Predator_Group" = Family_Predator
  )
  
  TargetName <- c()
  for (i in 1:nrow(temp)) {
    col.ind <- which(str_detect(colnames(temp), temp$Lowest_Tax_Level[i]))
    if(col.ind == 8){
      TargetName[i] <- paste(temp$Genus_Prey[i], temp[i, col.ind])
    }else{
      TargetName[i] <- temp[i, col.ind]
    }
    
  }
  
  temp <-
    data.frame(
      SourceName = temp$Predator_Scientific_Name,
      TargetName = TargetName,
      Weight = temp[,9],
      Group = temp$Lowest_Tax_Level,
      Pred_fam = temp$Predator_Group
    )
  
  network <- temp %>% filter(complete.cases(.))
  return(network)
  
}



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
        "Welcome",
        tabName = "welcome",
        icon = shiny::icon("globe")
      ),
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
    )
  
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "welcome",
            fluidRow(
              shinydashboard::box(
                title = "Trophic Webs of the Gulf of Mexico",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                h3("How to run the network app"),
                br(),
                h4(
                  "This application was developed for the work of",
              shiny::a(href = 'http://www.ingentaconnect.com/content/umrsmas/bullmar/2018/00000094/00000001/art00003', "Oshima and Leaf 2018."),
              "We collected diet studies from the northern Gulf of Mexico and conducted network and simulation analyses to evaluate the structure and robustness of the trophic dynamics. This app allows the user to select either a predator or prey and the diet metric used to report the interactions the nodes are shown at. Note that prey were identified to varrying taxonomic levels depending on the studies, which is indicated by the color of the nodes. Since all predators were identified to species level, the node colors represent taxonomic families. If you do not see a network displayed or get an error, try selecting a different diet metric. The diet metrics reported for a selected predator or prey can be seen in the data table."
                ),
                br(),
                h4("For questions please email megumi.oshima@usm.edu."),
                h4("2018 By Meg Oshima")
                
                
              )
            )),
    tabItem(tabName = "paramaters",
            fluidRow(
              shiny::column(
                width = 4,
                
                shinydashboard::box(
                  title = "Predator or Prey",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  shiny::helpText("Select a predator or prey to view its connections and prey items:"),
                  shiny::selectInput(
                    "type",
                    shiny::h5("Predator or Prey Search"),
                    c("Predator", "Prey")
                  ),
                  uiOutput(outputId = "dyType")
                ),
                
                shinydashboard::box(
                  title = "Diet Metric",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  shiny::helpText("Select a diet metric to use:"),
                  uiOutput(outputId = "metric")
                ),
                
                # shinydashboard::box(
                #   title = "Taxonomic Level",
                #   status = "primary",
                #   solidHeader = TRUE,
                #   collapsible = TRUE,
                #   width = NULL,
                #   shiny::helpText("Select a taxonomic level of nodes:"),
                #   uiOutput(outputId = "taxa")
                # ),
                
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
            ))
   
  ))
)


#Server function
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  #Reactively filter and select the rows and columns specified by the inputs
  
 
  
  output$dyType <- renderUI({
  
    switch(input$type,
           "Predator" = selectInput(inputId = 'dyType',
                                    label = shiny::h5("Predator Species:"),
                                    choices = c("", pred.name),
                                    selectize = TRUE),
           "Prey" = selectInput(inputId = 'dyType',
                                label = shiny::h5("Prey Taxa:"),
                                choices = c("", prey.tax),
                                selectize = TRUE))
  })
  
    observeEvent(input$dyType, {
      if(input$dyType == ""){
        return()
      }
      
      sp_name <- input$dyType
      pred <- switch(input$type, 
                     "Predator" = TRUE,
                     "Prey" = FALSE)
    
      output$metric <- renderUI({
       
        metric_opts <- get_metric(Diet, sp_name, pred)
        selectInput(inputId = 'metric',
                    label = shiny::h5("Diet Metric:"),
                    choices = c("", metric_opts),
                    selectize = TRUE)
        })
      
    })

    
  network.data <- eventReactive(input$makenet, {
    sp_name <- input$dyType
    metric <- switch(input$metric, 
                     "Frequency of Occurrence" = "Frequency_of_Occurrence",
                     "Dry_Weight" = "Dry_Weight",
                     "Weight" = "Weight",
                     "IRI" = "IRI",
                     "Number" = "Number",
                     "ICI" = "ICI",
                     "Volume" = "Volume")
   
    pred <- switch(input$type, 
                   "Predator" = TRUE,
                   "Prey" = FALSE)
    make_network(Diet, sp_name, metric, pred)
  
    
  })
  
  output$netplot <- renderForceNetwork({
    edgelist <- network.data()
    
    #create graph object with igraph package
    ig <-
      igraph::simplify(igraph::graph_from_data_frame(edgelist[, c(1, 2, 3)], directed =
                                                       TRUE))
    #Identify node IDs (in numbers) and make sure they start at 0 which is necessary for networkd3
    SourceID <- TargetID <- c()
    for (i in 1:nrow(edgelist)) {
      SourceID[i] <- which(edgelist[i, 1] == V(ig)$name) - 1
      TargetID[i] <- which(edgelist[i, 2] == V(ig)$name) - 1
    }
    
    #Create edgeList that contains source and target nodes and edge weights and nodeList that contains the nodeID and name
    edgeList <- cbind(edgelist, SourceID, TargetID)
    
    nodeList <- data.frame(ID = c(0:(igraph::vcount(ig) - 1)),
                           nName = igraph::V(ig)$name)
    
    #Determine and assign groups based on class
     preddf <-
       data.frame(SciName = edgelist[,1], grp = edgelist[,5])
     preydf <-
       data.frame(SciName = edgelist[,2], grp = edgelist[,4])
     groupsdf <- rbind(preddf, preydf)
     groupsdf <- groupsdf %>% mutate(SciName = as.character(SciName),
                                     class = as.character(grp))
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
      opacity = 0.95,
      zoom = TRUE,
      # ability to zoom when click on the node
      opacityNoHover = 0.6,
      # opacity of labels when static
      height = "1000px",
      width = "100px",
      bounded = TRUE,
      charge = -200,
      linkDistance = 400, 
      #legend = TRUE,
      colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), 
      arrows = TRUE,
      linkColour = "#ADBFC9"
    )
    
  })
  
  #Make a data table with the variables selected in the check boxes for the predator or prey that is selected. This data table can be downloaded as a .csv file.
  
  observeEvent(input$makedt, {
    pred <- switch(input$type, 
                   "Predator" = TRUE,
                   "Prey" = FALSE)
    sp_name <- input$dyType
    metric <- switch(input$metric, 
                     "Frequency of Occurrence" = "Frequency_of_Occurrence",
                     "Dry_Weight" = "Dry_Weight",
                     "Weight" = "Weight",
                     "IRI" = "IRI",
                     "Number" = "Number",
                     "ICI" = "ICI",
                     "Volume" = "Volume")
    output$vardt <- renderTable({
      Diet %>% filter(
        if (pred == TRUE)
          str_detect(Predator_Scientific_Name, sp_name)
        else
          str_detect(Lowest_Taxonomic_Identification_Prey, sp_name)
      ) %>% select(input$vars, metric) %>% na.omit()
      
    }, row.names = FALSE)
    
    output$Download <- downloadHandler(
      filename = function() {
        paste("trophicnetwork", ".csv", sep = "")
      },
      content = function(file) {
        sub.diet <-
          Diet %>% filter(
            if (pred == TRUE)
              str_detect(Predator_Scientific_Name, sp_name)
            else
              str_detect(Lowest_Taxonomic_Identification_Prey, sp_name)
          ) %>% select(input$vars, metric) %>% na.omit()
        
        write.csv(sub.diet, file, row.names = F)
      }
    )
    outputOptions(output, 'Download', suspendWhenHidden = FALSE)
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
