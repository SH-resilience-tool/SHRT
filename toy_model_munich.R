

# rsconnect::deployApp(appFiles=c('toy_model_munich.R','output_for_accessibility2.R','assignment.R', 'shortest_main.R', 'shortest_paths_dijkstra_online.R', 'logo_sh.png', 'logo_unibo.png'), appPrimaryDoc='toy_model_munich.R')

    rm(list = ls())
    library(shiny)
    library(readr)
    library(shinydashboard)
    library(shinyWidgets)
    library(shinyjs)
    library(igraph)
    library(visNetwork)
    library(leaflet)
    library(rgdal)
    library(sf)
    library(terra)
    library(doParallel)
    library(foreach)
    library(iterators)
    library(dplyr) 
    library(leaflet.extras)
    library(htmlwidgets)
    library(igraph)
    library(rgeos)
    library(ggplot2)
    library(htmltools)
    library(tidyverse)
    library(parallel)
    library(doParallel)
    library(DT)
    library(rlang)
    Sys.setenv("SHAPE_RESTORE_SHX" = "YES")
    
    # env <- new.env()
    # parent.env(env) <- emptyenv()
    # assign(".GlobalEnv", env, envir = .GlobalEnv)
    
    # if (exists(".rlang_env_history", envir = .GlobalEnv)) {
    #   rm(list = ".rlang_env_history", envir = .GlobalEnv)
    # }
  
  unweighted_efficiency<-0
  eff_index<-0
  n1<-0
  added<-vector()
  
  
  ####GRAFICA##############
 
    ui <- fluidPage(
    useShinyjs(),

    tags$head(
      tags$style(
        HTML("
        
        .image-container {
          display: flex;
          justify-content: flex-end;
          align-items: center;
          position: absolute;
          top: -60px;
          right: 10px;
          height: 100%;
        }
        .image-container img {
          margin-left: 10px;
          max-height: 100%;
        }
      ")
      )
    ),
    
    tags$head(
      tags$style(HTML("
      .table-container {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        # position: absolute;
          # top: -50px;
         left: 50px;
        # padding-bottom: 50px; /* Aggiungi margine inferiore */

      }
      .table-container .table-wrapper {
        flex: 1;
        margin-right: 40px; /* Margine tra le tabelle */
      }
    "))
    ),
    
    titlePanel("Smart Hubs Resilience Tool: Connectivity Component"),
    
    sidebarLayout(
      sidebarPanel(
        # Selettore di sottopannelli
        radioButtons("tabselected", "Select a tab:",
                     choices = list("Load the city map and the PT nodes" = 1,
                                    "Load the PT network" = 2,
                                    "Load the bike-sharing network " = 3,
                                    "Insert OD demand" = 4,
                                    "Scenarios"= 5),
                     selected = 1),
        
        # Sottopannello 1
        conditionalPanel(
          condition = "input.tabselected == 1",
          # Tendina per la selezione del testo
          
          fileInput('shp', 'Load a Map', accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj')),
          # selectInput("delim", "Node File Delimiter:", choices = c(",", ";", "/", "-")),
          
          textInput('delim', 'Node File Delimiter', value=';'),
          textInput('label', 'Label', value='labels'),
          textInput('x', 'Longitude', value='x'),
          textInput('y', 'Latitude', value='y'),
          fileInput('file0', 'Load Nodes', accept = '.txt'),
          actionButton("resetButton1", "Reset")
        ),
        
        # Sottopannello 2
        conditionalPanel(
          condition = "input.tabselected == 2",
          textInput('delim1', 'Edges File Delimiter', value=';'),
          textInput('from', 'From', value='from'),
          textInput('to', 'To', value='to'),
          textInput('weight', 'Cost', value='weight'),
          textInput('freq', 'Frequency', value='freq'),
          textInput('mode', 'Mode', value='mode'),
          fileInput("file1", "Load Edges"),
          actionButton("compute_efficiency", "Compute efficiency"),
          actionButton("delete_link", "Delete Selected Link"),
          actionButton("reset_last_link", "Reset last deleted link"),
          actionButton("reset_map", "Reset map"),
          checkboxInput("show_labels", "Show labels", value = FALSE),
          checkboxInput("show_modes", "Show modes", value = FALSE),
          actionButton("resetButton2", "Reset")
          
          
        ),
        
        # Sottopannello 3
        conditionalPanel(
          condition = "input.tabselected == 3",
          textInput('delim2', 'Bikes Delimiter', value=';'),
          textInput('from', 'From', value='from'),
          textInput('to', 'To', value='to'),
          textInput('weight', 'Cost', value='weight'),
          textInput('freq', 'Frequency', value='freq'),
          textInput('mode', 'Mode', value='mode'),
          fileInput("file2", "Add Bike Stations"),
          actionButton("compute_efficiency_bikes", "Compute efficiency with bikes"),
          actionButton("delete_link2", "Delete Selected Link"),
          actionButton("reset_last_link2", "Reset last deleted link"),
          actionButton("reset_map2", "Reset map"),
          checkboxInput("show_labels2", "Show labels", value = FALSE),
          checkboxInput("show_modes2", "Show modes", value = FALSE),
          actionButton("resetButton3", "Reset")

        ),
        
        # Sottopannello 4
        conditionalPanel(
          condition = "input.tabselected == 4",
          textInput('delim3', 'OD Delimiter', value=';'),
          textInput('from', 'From', value='from'),
          textInput('to', 'To', value='to'),
          textInput('od_demand', 'Demand', value='demand'),
          fileInput("file3", "Add OD demand"),
          actionButton("compute_weighted_efficiency", "Compute weighted efficiency"),
          checkboxInput("show_flows", "Show flows", value = FALSE),
          # checkboxInput("save_results", "Save output for accessibility tool", value = FALSE),
          # textInput("directory", "Directory for accessibility output (ex. C:/Users/folder):", placeholder = "Inserisci la directory"),
          # actionButton("save_button", "Save"),
          downloadButton("downloadFile", "Download results for accessibility"),
          actionButton("resetButton4", "Reset"),
          # textOutput("save_message"),
          
          ),
        
        # Sottopannello 5
        conditionalPanel(
          condition = "input.tabselected == 5",
          radioButtons("tabselected_scenario", "Select a scenario:",
                       choices = list("Betweenness centrality rank removal" = 10,
                                      "Degree centrality rank removal" = 20,
                                      "Closeness centrality rank removal" = 30,
                                      "Random removal" = 40),
                       selected = 10), 
          numericInput('n_links', 'Number of links to remove', value=1),
          actionButton("show_results", "Show scenario results"),
          actionButton("compute_efficiency_scenario", "Compute disrupted efficiency"),
          actionButton("compute_efficiency_scenario_bike", "Compute disrupted efficiency with bikes"),
          actionButton("reset_map3", "Reset map"),
          actionButton("resetButton5", "Reset")
        ),
        
        fluid=T,),
      
      mainPanel(
        
        div(
          class = "image-container",
          imageOutput("logo1"),
          imageOutput("logo2")
        ),
        leafletOutput('mappa'),
        uiOutput('selected_link'),
        div(id = "progressBarWrapper", style = "display: none;", progressBar("progressBar", value = 0)),        
        # Display global efficiency
        uiOutput("unweighted_efficiency"),
        uiOutput("unweighted_efficiency_bike"),
        uiOutput("unweighted_efficiency2"), #@
        uiOutput("efficiency_change"),
        uiOutput("weighted_efficiency"),
        uiOutput("top_nodes"),
        # tableOutput("tabella"),
        div(class = "table-container",
              div(class = "table-wrapper",
                  tableOutput("tabella")
              ),
              div(class = "table-wrapper",
                  tableOutput("table2")
              )
        ),
        # textOutput('selected_link'),
        uiOutput("removed_links"),
        textOutput("progressText"),
        # progressBar("progressBar", value = 0),
        # div(id = "progressBarWrapper", style = "display: none;", progressBar("progressBar", value = 0)),        
        fluid=T,
      ),

  ))

  ######################  
  server <- function(input, output,session) {
    runjs("$('#delim').attr('maxlength', 1)")        # Set the maxImposta il limite di caratteri per ogni textInput
    data <- reactiveVal()
    edges_index<-reactiveVal()
    nodes_index<-reactiveVal()
    demand_index<-reactiveVal()
    district_index<-reactiveVal()
    shapefile_index<-reactiveVal()
    disrupted_index<-reactiveVal()
    bike_index<-reactiveVal()
    edges_bike_index<-reactiveVal()
    selected_link <- reactiveVal()
    unw_eff_index<-reactiveVal()
    eff_change_index<-reactiveVal()
    dist_index<-reactiveVal()
    results_index<-reactiveVal()
    top_nodes_index<-reactiveVal()
    unw_eff_index2<-reactiveVal()
    eff_bike_index<-reactiveVal()
    dist_bike_index<-reactiveVal()
    layerID_index<-reactiveVal()
    deleted_rows_index<-reactiveVal()
    added_index<-reactiveVal()
    graph_index<-reactiveVal()
    edges_base_index<-reactiveVal()
    edges_base_bike_index<-reactiveVal()
    top_nodes_base_index<-reactiveVal()
    unw_eff_bike_index<-reactiveVal()
    unw_eff_bike_index2<-reactiveVal()
    
    
    # setwd(dirname(rstudioapi::getSourceEditorContext()$path))
    # deleted_rows<-data.frame()
    
    output$logo1 <- renderImage({
      list(src = "logo_sh.png", width = "auto", height = 70, alt = "Logo")
    }, deleteFile = FALSE)
    
    output$logo2 <- renderImage({
      list(src = "logo_unibo.png", width = "auto", height = 70, alt = "Logo 2")
    }, deleteFile = FALSE)
    
    updateProgress <- function(value) {
      updateProgressBar(session, "progressBar", value = value)
      output$progressText <- renderText({
        paste("Progress: ", round(value, 1), "%")
      })
    }
    
    observe({
      req(input$shp)
      dati <- input$shp
      if (is.null(dati)) return(NULL)
      
      print('shp1')
      shapePath <- dati$datapath[grepl(".shp", dati$name)]
      shapefile1 <- sf::st_read(shapePath)
      
      # shapefile<<-shapefile
      # district_names<-rownames(shapefile)
      # n_district<-nrow(shapefile)
      
      output$mappa <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = shapefile1)
      })
      shapefile<-shapefile_index(shapefile1)
    })
    
    observeEvent(input$file0, {
      # if (!is.null(nodes)) {
        tryCatch({
      nodes1 <- read.delim(input$file0$datapath, header = TRUE, sep = input$delim)
      nodes1 <- nodes1[, c(input$label, input$x, input$y)]
      nodes_sf <- sf::st_as_sf(nodes1, coords = c(input$x, input$y))

      shapefile<-shapefile_index()
      observe({
        leafletProxy("mappa") %>%
          addCircleMarkers(data = nodes1, lat = ~nodes1[[input$y]], lng = ~nodes1[[input$x]], radius = 1, color= "black")
        })

      if (!is.null(shapefile)) {
        nodes_within_map <- as.integer(sf::st_within(nodes_sf, shapefile))
        nodes1$within_map <- nodes_within_map
        nodes<-nodes_index(nodes1)
      }

      nodes<-nodes_index(nodes1)

    }, error = function(e) {
      showNotification(paste("Si è verificato un errore:", e$message), type = "error")
    })
      # }
    })
    
    observeEvent(input$file1, {
      tryCatch({
      req(input$file0)
      req(input$file1)
      nodes<-nodes_index()
      edges <- read.delim(input$file1$datapath, header = TRUE, sep = input$delim1)
      edges <- edges[, c("from", "to", "weight", "freq", "mode")]

      edges$id <- seq_len(nrow(edges))
      # data(edges)
      bike_index(0)
      eff_bike_index(0)
      disrupted_index(0)
      unique_modes <- unique(edges$mode)
      
      getColor <- reactive({
        if (input$show_modes) {
          color_palette <- c("red", "orange","blue", "green", "purple", "pink", "cyan", "magenta", "brown")
        }  else {
          color_palette=colorRampPalette(c("blue", "red"))
          }
      })

      color_palette <- getColor()

      for(i in 1:nrow(edges)) {
        from_node <- nodes[nodes[[input$label]] == edges[i, "from"], ]
        to_node <- nodes[nodes[[input$label]] == edges[i, "to"], ]
        edges[i,"from_lng"]<-from_node$x
        edges[i,"from_lat"]<-from_node$y
        edges[i,"to_lng"]<-to_node$x
        edges[i,"to_lat"]<-to_node$y

        # edges[i,"color"]<-color_palette(max(edges$freq))[edges[i, "freq"]]
        getEdgeColor<-reactive({
          if (input$show_modes) {
            edge_color<-color_palette[which(unique_modes==edges[i, "mode"])]}
          else {
            edge_color<-color_palette(max(edges$freq))[edges[i, "freq"]]}
        })

        edges[i,"color"]<-getEdgeColor()

        leafletProxy("mappa") %>%
          addPolylines(lng = c(from_node[[input$x]], to_node[[input$x]]),
                       lat = c(from_node[[input$y]], to_node[[input$y]]),
                       color=edges[i,"color"],
                       group="links",
                       layerId = i,
                       )
      }

      #Add the legend
       leafletProxy("mappa") %>%
         addLegend(position = "bottomright",
                   colors = colorRampPalette(c("blue", "red"))(5),
                   labels = seq(from = min(edges$freq), to = max(edges$freq), length.out = 5),
                    title = "Hour Frequency")

       edges_index(edges)
       # edges_base<-edges_index(edges)
       edges_base_index(edges)

      observeEvent(input$show_labels, {

        leafletProxy("mappa") %>% clearPopups()

        if(input$show_labels) {
          for(i in 1:nrow(nodes)) {
            node <- nodes[i, ]
            leafletProxy("mappa") %>%
              addCircleMarkers(lng = node[[input$x]], lat = node[[input$y]], radius = 1) %>%
              addPopups(lng = node[[input$x]], lat = node[[input$y]], node[[input$label]])
          }
        }
      })

      observeEvent(input$show_modes, {
       color_palette <- getColor()
      leafletProxy("mappa") %>% clearPopups()

       for(i in 1:nrow(edges)) {
         from_node <- nodes[nodes[[input$label]] == edges[i, "from"], ]
         to_node <- nodes[nodes[[input$label]] == edges[i, "to"], ]


         getEdgeColor<-reactive({
           if (input$show_modes) {
             edge_color<-color_palette[which(unique_modes==edges[i, "mode"])]}
           else {
               edge_color<-color_palette(max(edges$freq))[edges[i, "freq"]]}
         })

         edges[i,"color"]<-getEdgeColor()
         leafletProxy("mappa") %>%
           addPolylines(lng = c(from_node[[input$x]], to_node[[input$x]]),
                        lat = c(from_node[[input$y]], to_node[[input$y]]),
                        color=edges[i,"color"],
                        # color = colorRampPalette(c("blue", "red"))(max(edges$freq))[edges[i, "freq"]]
           )
       }

      if (input$show_modes) {
        leafletProxy("mappa") %>%
          addLegend(position = "topright",
                    colors = color_palette[1:length(unique_modes)],
                    labels = unique_modes[1:length(unique_modes)],
                    title = "Transport Mode",
                    layerId = "legend_modes")
      } else {leafletProxy("mappa") %>% removeControl("legend_modes")}
      })
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    })
    
    observeEvent(input$file2, {
      tryCatch({
        req(input$file0)
        req(input$file2)
        nodes<-nodes_index()
        edges_bike <- read.delim(input$file2$datapath, header = TRUE, sep = input$delim2)
        edges_bike <- edges_bike[, c("from", "to", "weight", "freq", "mode")]
        edges_bike$id <- seq_len(nrow(edges_bike)) + nrow(edges_index())
        # bike<<-1
        bike<-bike_index(1)
        edges2<-edges_index()
        

        getColor <- reactive({
          if (input$show_modes2) {
            color_palette3 <- c("red", "orange", "blue", "green","purple", "pink", "cyan", "magenta", "brown")
          }  else {
            color_palette3<-colorRampPalette(c("blue", "red"))
          }
        })

        color_palette3 <- getColor()
        print("ok1")
        # Aggiungi le linee (edges) alla mappa
        print(nrow(edges_bike))
        for(i in 1:nrow(edges_bike)) {
          from_node <- nodes[nodes[[input$label]] == edges_bike[i, "from"], ]
          to_node <- nodes[nodes[[input$label]] == edges_bike[i, "to"], ]
          edges_bike[i,"from_lng"]<-from_node$x
          edges_bike[i,"from_lat"]<-from_node$y
          edges_bike[i,"to_lng"]<-to_node$x
          edges_bike[i,"to_lat"]<-to_node$y

          edges_bike[i,"color"]<-"yellow"

          leafletProxy("mappa") %>%
            addPolylines(lng = c(from_node[[input$x]], to_node[[input$x]]),
                         lat = c(from_node[[input$y]], to_node[[input$y]]),
                         color=edges_bike[i,"color"],
                         group="links",
                         layerId = i+nrow(edges2)
                         )
        }
        print("Okk")
        # data(rbind(edges2, edges_bike))
        edges_bike_index(edges_bike)
        edges_bike_base<-edges_bike_index(edges_bike)
        edges_base_bike_index(edges_bike_base)
        # edges_bike<<-edges_bike
        # edges_bike_base<<-edges_bike
        

        unique_modes2 <- unique(edges2$mode)
        print("ok3")

        observeEvent(input$show_labels2, {

          leafletProxy("mappa") %>% clearPopups()

          if(input$show_labels2) {
            for(i in 1:nrow(nodes)) {
              node <- nodes[i, ]
              leafletProxy("mappa") %>%
                addCircleMarkers(lng = node[[input$x]], lat = node[[input$y]], radius = 1) %>%
                addPopups(lng = node[[input$x]], lat = node[[input$y]], node[[input$label]])
            }
          }
        })

        observeEvent(input$show_modes2, {
          color_palette3 <- getColor()
          leafletProxy("mappa") %>% clearPopups()

          for(i in 1:nrow(edges2)) {
            from_node <- nodes[nodes[[input$label]] == edges2[i, "from"], ]
            to_node <- nodes[nodes[[input$label]] == edges2[i, "to"], ]

            getEdgeColor3<-reactive({
              if (input$show_modes2) {
                edge_color<-color_palette3[which(unique_modes2==edges2[i, "mode"])]}
              else {
                if (edges2[i,"freq"]==0){
                  edge_color<-"yellow"}else{
                edge_color<-color_palette3(max(edges2$freq))[edges2[i, "freq"]]}
              }
            })

            edges2[i,"color"]<-getEdgeColor3()
            leafletProxy("mappa") %>%
              addPolylines(lng = c(from_node[[input$x]], to_node[[input$x]]),
                           lat = c(from_node[[input$y]], to_node[[input$y]]),
                           color=edges2[i,"color"],
                           # color = colorRampPalette(c("blue", "red"))(max(edges$freq))[edges[i, "freq"]]
              )
          }

          if (input$show_modes2) {
            leafletProxy("mappa") %>%
              addLegend(position = "topright",
                        colors = c(color_palette3[1:length(unique_modes2)], "yellow"),
                        labels = c(unique_modes2[1:length(unique_modes2)], "bikes"),
                        title = "Transport Mode",
                        layerId = "legend_modes")
          } else {leafletProxy("mappa") %>% removeControl("legend_modes")}
        })
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
      })

    observeEvent(input$file3,{
      tryCatch({
      demand <- read.delim(input$file3$datapath, header = TRUE, sep = input$delim3)
      demand <- demand[, c("from", "to", "demand")]
      demand_index(demand)
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    })
      
    observeEvent(input$compute_efficiency|input$compute_efficiency_scenario, {
      req(input$compute_efficiency | input$compute_efficiency_scenario)
      updateProgress(0)
      tryCatch({
        shinyjs::show("progressBarWrapper")
        leafletProxy("mappa") %>% clearMarkers()
        edges2<-edges_index()
        # g <- graph.data.frame(data(), directed = T)
        g <- graph.data.frame(edges2, directed = T)
        print(nrow(edges2))
        print(ecount(g))
        g1<-g
        E(g1)$mode<-as.factor(E(g1)$mode)
        E(g1)$weights<-as.numeric(E(g1)$weight)/60
        E(g1)$freq<-as.numeric(E(g1)$freq)
        E(g1)$weights[is.na(E(g1)$weights)==TRUE]<-1
        E(g1)$mode1[E(g)$mode=="metro"]<-1
        E(g1)$mode1[E(g)$mode=="tram"]<-2
        E(g1)$mode1[E(g)$mode=="bus"]<-3
        E(g1)$mode1[E(g)$mode=="bike"]<-4
        E(g1)$mode1[E(g)$mode=="train"]<-5
        
        dist_mat2<-matrix(0,nrow=vcount(g), ncol=vcount(g))
        D2<-list(0)
        print(time1<-Sys.time())
     
        source("shortest_paths_dijkstra_online.R")
        results3<-dijkstra_gen(g1,dist_mat2,D2,updateProgress)
        dist_mat2<-results3[[1]]
        
        print(Sys.time()-time1)
        n<-vcount(g1)
        print(n)
        ones <- matrix(1, n, n)
        dist <- ones / (dist_mat2)
        dist[dist == Inf] <- 0
        dist[is.na(dist)==TRUE]<-0
        tot <- (1/(n*(n-1)))*sum(dist)

        disrupted<-disrupted_index()
        bike<-bike_index()
        
        # Render the global efficiency
        if (disrupted!=1){
          unweighted_efficiency<-tot
          output$unweighted_efficiency <- renderUI({
            tags$h1("Unweighted Efficiency: ", tags$span(sprintf("%.5f", unweighted_efficiency), style = " color: red;"))
            })
          
          unw_eff_index(unweighted_efficiency)
          unweighted_efficiency<-unw_eff_index()

          if (bike==1){
            efficiency_change<-(unweighted_efficiency_bike-unweighted_efficiency)/unweighted_efficiency
            output$efficiency_change <- renderUI({
              tags$h1("Efficiency Change: ", tags$span(sprintf("%.3f", efficiency_change),"%", style = "color: red;"))
            })
            eff_change_index(efficiency_change)
          }
        }
        
       
        # dist_mat2<<-dist_mat2
        # results3<<-results3
        dist_index(dist_mat2)
        results_index(results3)
        
        if (disrupted==1){
          unweighted_efficiency2<-tot
          unweighted_efficiency<-unw_eff_index()
          
          output$unweighted_efficiency2 <- renderUI({
            tags$h1("Disrupted Unweighted Efficiency: ", tags$span(sprintf("%.5f", unweighted_efficiency2), style = "color: red;"))
          })
          
          
          
          if (unweighted_efficiency!=0){
            efficiency_change<-(unweighted_efficiency2-unweighted_efficiency)/unweighted_efficiency
            output$efficiency_change <- renderUI({
              tags$h1("Efficiency Change: ", tags$span(sprintf("%.5f", efficiency_change),"%", style = "color: red;"))
            })
            eff_change_index(efficiency_change)
            unw_eff_index2(unweighted_efficiency2)
            unweighted_efficiency2<-unw_eff_index2()
            
          }
        }
        
        
        ## Calculate betweenness centrality ##
        g <- simplify(g)
        betweenness_centrality <- betweenness(g)
        top_nodes <- sort(betweenness_centrality, decreasing = TRUE)[1:10]
        output$top_nodes <- renderUI({
          tags$div(
            tags$h3("Top 10 Nodes for Betweenness Centrality:"),
            # tags$ul(
            #   lapply(names(top_nodes), function(node) {
            #     tags$li(node)
            #   })
            # )
          )
        })
        
        if (disrupted!=1){
          top_nodes_base<-top_nodes
          top_nodes_index(top_nodes_base)
          # top_nodes_base_index(top_nodes_base)
          # top_nodes_base<-top_nodes_base_index(top_nodes_base)
          
          }
        print(top_nodes)
        top_nodes_base<-top_nodes_index()
        
        
        if (disrupted!=1){
          output$tabella <- renderTable({
            dati <- data.frame(
              Rank = c(1:10),
              Name = names(top_nodes)
            )
            colnames(dati) <- c('Rank<br>(status quo)', "Stop name")
            dati
          }, sanitize.text.function = function(x) x) # Per evitare che Shiny effettui la sanitizzazione dei tag HTML
        }
        
        # top_nodes<-c("Sendlinger Tor","Hauptbahnhof", "Odeonsplatz",  "Goetheplatz", "Poccistraße",
        # "Implerstraße","Innsbrucker Ring",   "Max-Weber-Platz","Lehel", " Münchner Freiheit")
        index2<-0
        for (i in 1:10){
          if (length(which(names(top_nodes_base)[i]==names(top_nodes[]))) > 0) {
            index2[i]<-which(names(top_nodes_base)[i]==names(top_nodes[]))
          }else{index2[i]<-NA}}
        
        # index2<-c(1,2,4,7,6,5,NA,8,9,10)
        print(c(1:10)-index2)
        variation<-abs(c(1:10)-index2)
        variation[which(is.na(variation))]<-"OUT"
print(disrupted)
        if (disrupted==1){
          output$tabella <- renderTable({
            dati <- data.frame(
              Rank = c(1:10),
              Name = names(top_nodes_base),
              Variation = c(1:10)-index2
              # Name_disrupted = names(top_nodes)
            )
            dati$Name <- ifelse(is.na(dati$Variation), paste0("<span style='color: red;'>", dati$Name, "</span>"), dati$Name)
            # dati$Name_disrupted<-ifelse(dati$Name_disrupted %in% setdiff(dati$Name_disrupted, dati$Name), paste0("<span style='color: green;'>", dati$Name_disrupted, "</span>"), dati$Name_disrupted)
            dati$Variation <- paste0(
              '<svg xmlns="http://www.w3.org/2000/svg" width="15" height="15" viewBox="0 0 24 24">',
              ifelse(dati$Variation > 0, '<path fill="green" d="M12 2l-12 20h24z"></path><path fill="white" d="M12 6l8 8h-16z"></path>', ""),
              ifelse(dati$Variation < 0, '<path fill="red" d="M12 22l12-20h-24z"></path><path fill="white" d="M12 18l-8-8h16z"></path>', ""),
              ifelse(is.na(dati$Variation), '<path fill="red" d="M12 22l12-20h-24z"></path><path fill="white" d="M12 18l-8-8h16z"></path>', ""),
              '</svg>',
              # abs(dati$Variation)
              variation
            )
            colnames(dati) <- c('Rank<br>(status quo)', "Stop name", 'Rank variation<br>(after disruption)')
            
            dati
          }, sanitize.text.function = function(x) x) # Per evitare che Shiny effettui la sanitizzazione dei tag HTML
          
          output$table2<- renderTable({  
            dati2 <- data.frame(
              Rank = c(1:10),
              Name = names(top_nodes_base),
              Name_disrupted = names(top_nodes)
            )
            # dati$Name <- ifelse(is.na(dati$Variation), paste0("<span style='color: red;'>", dati$Name, "</span>"), dati$Name)
            dati2$Name_disrupted<-ifelse(dati2$Name_disrupted %in% setdiff(dati2$Name_disrupted, dati2$Name), paste0("<span style='color: green;font-weight: bold;'>", dati2$Name_disrupted, "</span>"), dati2$Name_disrupted)
            dati2 <- dati2[, c("Rank", "Name_disrupted")]  # Selezionare solo le colonne che si desidera mostrare
            colnames(dati2) <- c('Rank<br>(after disruption)', "Stop name")
            dati2
          }, sanitize.text.function = function(x) x)
          
        }
        nodes<-nodes_index()
        color_nodes<-nodes[which(nodes$label %in% names(top_nodes)),]
        
        edges_index(edges2)
        
        leafletProxy("mappa") %>%
          addCircleMarkers(data = color_nodes, lng = ~x, lat = ~y, radius = 2, color = "white",opacity=1)
        
        graph_index(g1)
        
        
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    })
    
    observeEvent(input$compute_weighted_efficiency,{
      tryCatch({
        
        nodes<-nodes_index()
        shapefile<-shapefile_index()
        eff_bike<-eff_bike_index()
        dist_mat2<-dist_index()
        dist_mat2_bike<-dist_bike_index()
        edges2<-edges_index()
        edges_bike<-edges_bike_index()
        demand<-demand_index()
        results3<-results_index()
        # g<-graph_index()
        
        if (eff_bike==1){
        dist_mat2<-dist_mat2_bike
        edges2<-rbind(edges2,edges_bike)
        }
        print(dist_mat2)
        g<-graph.data.frame(edges2, directed = T)
        print(g)
        g1<-g
        matrice_od<-matrix(0,nrow=vcount(g1), ncol=vcount(g1))
        n<-vcount(g1)
        for(o in 1:nrow(matrice_od)) {
          for(d in 1:ncol(matrice_od)) {
            if (nrow(demand[(demand$from ==attr(V(g1),"names")[o])&(demand$to ==attr(V(g1),"names")[d]),])==0){next}
            matrice_od[o,d]<-ceiling(demand[(demand$from ==attr(V(g1),"names")[o])&(demand$to ==attr(V(g1),"names")[d]),]$demand)
          }}
        
        dist <- matrice_od / dist_mat2
        dist[(dist == Inf)|(is.na(dist)==TRUE)] <- 0
        weighted_efficiency <- (1/(n*(n-1)))*sum(dist)
        
        output$weighted_efficiency <- renderUI({
          tags$h1("Weighted Efficiency: ", tags$span(sprintf("%.5f", weighted_efficiency), style = "color: red;"))
        })
        
        # source("C:/Users/Cate/Desktop/toy model marzo/assignment.R")
        source("assignment.R")
        results4<-assignment_gen(matrice_od,g1,results3)
        results4[(results4[,3] == Inf)|(is.na(results4[,3])==TRUE)|(results4[,3] == 0),3]<-1
        
        source("output_for_accessibility2.R")
        accessibility <- accessibility_output(nodes, shapefile, demand, dist_mat2)
        print("ok")
        
        output$downloadFile <- downloadHandler(
          filename = function() {
            "output_accessibility.txt"  
          },
          content = function(file) {
            # Scrivi i dati nel file di testo separato da tabulazioni
            write.table(accessibility, file = file, sep = "\t", row.names = FALSE, quote=F)
          }
        )
        
        # observeEvent(input$save_button, {
        #   dir <- input$directory
        #   if (dir != "") {
        #     write.table(accessibility, file = paste0(dir, "/output_accessibility.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
        #     output$save_message <- renderText({
        #       paste("File saved in:", dir)
        #     })
        #   } else {
        #     output$save_message <- renderText({
        #       "Insert a valid directory."
        #     })
        #   }
        # })
        
        # observeEvent(input$save_results, {
        #   if(input$save_results) {
        #     source("output_for_accessibility2.R")
        #     accessibility<-accessibility_output(nodes, shapefile, demand, dist_mat2)
        #     write.table(accessibility, file=paste0(getwd(), "/output_accessibility.txt"), sep="\t", row.names = F, quote=F)
        #     print("ok acc")
        #   }
        # })
        
        getColor2 <- reactive({
          if (input$show_flows) {
            color_palette2 <- colorRampPalette(c("yellow", "green"))
          }  else {
            color_palette2<-colorRampPalette(c("blue", "red")) }
        })
        color_palette2 <- getColor2()
        
        for(i in 1:nrow(edges2)) {
          getEdgeColor2<-reactive({
            if (input$show_flows) {
              edge_color<-color_palette2(max(results4[,3]))[results4[i, 3]]
            }
            else {
              if (edges2[i,"freq"]==0){
                edge_color<-"yellow"} else {
                  edge_color<-color_palette2(max(edges2$freq))[edges2[i, "freq"]]}
            }
          })
          
          if (edges2[i,"freq"]!=0){
            edges2[i,"color"]<-getEdgeColor2()}
          
          leafletProxy("mappa") %>% clearMarkers %>%
            addPolylines(lng = c(edges2[i,"from_lng"], edges2[i,"to_lng"]),
                         lat = c(edges2[i,"from_lat"], edges2[i,"to_lat"]),
                         color=edges2[i,"color"]
            )
        }
        
        
        observeEvent(input$show_flows, {
          color_palette2 <- getColor2()
          # edges2[edges2$freq==0,"freq"]<-1
          leafletProxy("mappa") %>% clearPopups()
          for(i in 1:nrow(edges2)) {
            getEdgeColor2<-reactive({
              if (input$show_flows) {
                edge_color<-color_palette2(max(results4[,3]))[results4[i, 3]]
              }
              else {
                if (edges2[i,"freq"]==0){
                  edge_color<-"yellow"} else {
                    edge_color<-color_palette2(max(edges2$freq))[edges2[i, "freq"]]}
              }
            })
            
            edges2[i,"color"]<-getEdgeColor2()
            leafletProxy("mappa") %>%
              addPolylines(lng = c(edges2[i,"from_lng"], edges2[i,"to_lng"]),
                           lat = c(edges2[i,"from_lat"], edges2[i,"to_lat"]),
                           color=edges2[i,"color"]
              )
          }
          
          if (input$show_flows) {
            leafletProxy("mappa") %>%
              addLegend(position = "topright",
                        colors=color_palette2(5),
                        labels = round(seq(from = floor(min(results4[,3]-1)), to = ceiling(max(results4[,3])), length.out = 5)),
                        title = "Transport flows",
                        layerId = "legend_flows")
          } else {leafletProxy("mappa") %>% removeControl("legend_flows")}
        })
        
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
      })
    
    find_nearest_line <- function(click_lat, click_lng) {
      bike<-bike_index()
      edges2<-edges_index()
      edges_bike<-edges_bike_index()
      
      if (bike==1){edges2<-rbind(edges2,edges_bike)}
      distances <- apply(edges2[, c("from_lat", "from_lng", "to_lat", "to_lng")], 1, function(row) {
        # Calcoliamo la distanza euclidea tra il punto cliccato e il centro della linea
        line_center_lat <- (row[1] + row[3]) / 2
        line_center_lng <- (row[2] + row[4]) / 2
        sqrt((line_center_lat - click_lat)^2 + (line_center_lng - click_lng)^2)
      })
      # Troviamo l'ID della polilinea con la minima distanza
      nearest_line_id <- which.min(distances)
      print(nearest_line_id)
      return(nearest_line_id)
    }
    
    observeEvent(input$mappa_click, {
      tryCatch({
      useShinyjs()
        bike<-bike_index()
        edges2<-edges_index()
        edges_bike<-edges_bike_index()
        
        if (bike==1){edges2<-rbind(edges2,edges_bike)}
      click <- input$mappa_click
      click_lat <- click$lat
      click_lng <- click$lng
      nearest_line <- find_nearest_line(click_lat, click_lng) # Funzione da definire
      selected_link(nearest_line)
      print(paste("Coordinates clicked: lat =", click_lat, ", lng =", click_lng))
      print(paste("Selected link:", nearest_line, 'from', edges2[nearest_line,"from"], 'to', edges2[nearest_line,"to"]))

      output$selected_link <- renderUI({
        tags$h4(paste("Selected link: from", edges2[nearest_line,"from"], 'to', edges2[nearest_line,"to"]), style = " font-style: italic; color: black;")
      })

      layerID_index(nearest_line)
      # layerID<<-nearest_line
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
        })
    
    observeEvent(input$delete_link|input$delete_link2, {
      req(input$delete_link | input$delete_link2)
      tryCatch({
        
      deleted_rows<-deleted_rows_index()
      added<-added_index()
      bike<-bike_index()
      edges2<-edges_index()
      edges_bike<-edges_bike_index()
      layerID<-layerID_index()
      
      if (bike==1){edges2<-rbind(edges2,edges_bike)}
      print(edges2[layerID,])
      print(layerID)
      print(length(added))
      print(nrow(deleted_rows))
      if (!is.null(deleted_rows)){
        n1<-nrow(deleted_rows)
        } else {n1<-0}
      
      layerID2<-which((edges2$to==edges2[layerID, "from"])&(edges2$from==edges2[layerID, "to"]))
      print('va bene')
      deleted_rows<-rbind(deleted_rows,edges2[layerID,], edges2[layerID2,])
      added1<-0+(nrow(deleted_rows)-n1)
      added<-append(added,added1)
      print(added)
      # print(deleted_rows[nrow(deleted_rows),])
      # print(nrow(edges2))
      # 
      edges2<-edges2[-c(layerID, layerID2),]
      print(nrow(edges2))
      print('va bene2')
      
      leafletProxy("mappa") %>% clearShapes()
      shapefile<-shapefile_index()
      output$mappa <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = shapefile)
      })
      
      for(i in 1:nrow(edges2)) {
        leafletProxy("mappa") %>%
          addPolylines(lng = c(edges2[i,"from_lng"],edges2[i,"to_lng"]),
                       lat = c(edges2[i,"from_lat"], edges2[i,"to_lat"]),
                       color=edges2[i,"color"],
          )
      }
      print(edges2[layerID,]$mode)
      
      deleted_rows_index(deleted_rows)
      added_index(added)
      edges_bike_index(edges2[which(edges2$mode=='bike'),])
      edges_index(edges2[which(edges2$mode!='bike'),])
      
      # edges_bike<<-edges2[which(edges2$mode=='bike'),]
      # edges2<<-edges2[which(edges2$mode!='bike'),]

      # disrupted<<-1
      disrupted_index(1)
      # print(disrupted)
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    })
    
    observeEvent(input$reset_last_link|input$reset_last_link2, {
      req(input$reset_last_link | input$reset_last_link2)
      tryCatch({
        
        deleted_rows<-deleted_rows_index()
        added<-added_index()
        bike<-bike_index()
        edges2<-edges_index()
        edges_bike<-edges_bike_index()
        layerID<-layerID_index()
        
      if (bike==1){edges2<-rbind(edges2,edges_bike)}
      print(edges2[layerID,])
      print('fin qui ok')

      print(nrow(edges2))
      print(deleted_rows[(nrow(deleted_rows)-1):nrow(deleted_rows),])
      print(length(added))

      edges2<-rbind(edges2,deleted_rows[(nrow(deleted_rows)-(added[length(added)]-1)):nrow(deleted_rows),])
      deleted_rows<-deleted_rows[1:(nrow(deleted_rows)-added[length(added)]),]
      added<-added[1:(length(added)-1)]
      print(added)
      print(nrow(edges2))
      print('fin qui ok')

      leafletProxy("mappa") %>% clearShapes()
      
      shapefile<-shapefile_index()
      output$mappa <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = shapefile)
      })
      
      for(i in 1:nrow(edges2)) {
        leafletProxy("mappa") %>%
          addPolylines(lng = c(edges2[i,"from_lng"],edges2[i,"to_lng"]),
                       lat = c(edges2[i,"from_lat"], edges2[i,"to_lat"]),
                       color=edges2[i,"color"],
          )
      }
      
      added_index(added)
      deleted_rows_index(deleted_rows)
      edges_bike_index(edges2[which(edges2$mode=='bike'),])
      edges_index(edges2[which(edges2$mode!='bike'),])
      
      # edges_bike<<-edges2[which(edges2$mode=='bike'),]
      # edges2<<-edges2[which(edges2$mode!='bike'),]
     
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    })
    
    observeEvent(input$reset_map|input$reset_map2|input$reset_map3, {
      req(input$reset_map|input$reset_map2|input$reset_map3)
      tryCatch({
        
        
        output$tabella <- renderTable({
          NULL})
        output$table2 <- renderTable({
          NULL})
        output$unweighted_efficiency2 <- renderUI({
          NULL})
        output$efficiency_change <- renderUI({
          NULL})
        output$weighted_efficiency <- renderUI({
          NULL})
        output$selected_links <- renderUI({
          NULL})
        
        deleted_rows<-deleted_rows_index()
        # added<-added_index()
        bike<-bike_index()
        # edges2<-edges_index()
        # edges_bike<-edges_bike_index()
        # layerID<-layerID_index()
        
      edges2<-edges_base_index()
      
      if (bike==1){
        edges_bike_base<-edges_base_bike_index()
        }
      # 
      # edges2<-rbind(edges2,edges_bike_base)
      # deleted_rows<<-data.frame()
      deleted_rows_index(0)
      print(nrow(edges2))

      shapefile<-shapefile_index()
      
      leafletProxy("mappa") %>% clearShapes()%>%addTiles() %>%
          addPolygons(data = shapefile)
    
      
      for(i in 1:nrow(edges2)) {
        leafletProxy("mappa") %>%
          addPolylines(lng = c(edges2[i,"from_lng"],edges2[i,"to_lng"]),
                       lat = c(edges2[i,"from_lat"], edges2[i,"to_lat"]),
                       color=edges2[i,"color"],
          )
      }
      # edges2<<-edges2
      edges_index(edges2)
      edges_base_index(edges2)
      
      if (bike==1){
      # leafletProxy("mappa") %>%
      #   addPolylines(lng = c(edges_bike[i,"from_lng"],edges_bike[i,"to_lng"]),
      #                lat = c(edges_bike[i,"from_lat"], edges_bike[i,"to_lat"]),
      #                color=edges_bike[i,"color"], )

        edges_bike_index(edges_bike_base)
        edges_base_bike_index(edges_bike_base)
      # edges_bike<<-edges_bike
      }
      disrupted_index(0)
      unw_eff_index(0)
      eff_bike_index(0)
      unw_eff_index2(0)
      bike_index(0)
      
      # disrupted<<-0
      # unweighted_efficiency<<-0
      # unweighted_efficiency_bike<<-0
      # unweighted_efficiency2<<-0
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    })
    
    observeEvent(input$compute_efficiency_bikes, {
      updateProgress(0)
      tryCatch({
        shinyjs::show("progressBarWrapper")
        edges2<-edges_index()
        edges_bike<-edges_bike_index()
        bike<-bike_index()
        disrupted<-disrupted_index()
        unweighted_efficiency<-unw_eff_index()
        
        edges3<-rbind(edges2,edges_bike)
        g <- graph.data.frame(edges3, directed = T)
        g1<-g
        E(g1)$mode<-as.factor(E(g1)$mode)
        E(g1)$weights<-as.numeric(E(g1)$weight)/60
        E(g1)$freq<-as.numeric(E(g1)$freq)
        E(g1)$weights[is.na(E(g1)$weights)==TRUE]<-1
        E(g1)$mode1[E(g)$mode=="metro"]<-1
        E(g1)$mode1[E(g)$mode=="tram"]<-2
        E(g1)$mode1[E(g)$mode=="bus"]<-3
        E(g1)$mode1[E(g)$mode=="bike"]<-4
        E(g1)$mode1[E(g)$mode=="train"]<-5
        dist_mat2<-matrix(0,nrow=vcount(g), ncol=vcount(g))
        D2<-list(0)
        print(time1<-Sys.time())
       
        source("shortest_paths_dijkstra_online.R")
        results3<-dijkstra_gen(g1,dist_mat2,D2,updateProgress)
        dist_mat2_bike<-results3[[1]]
        n<-vcount(g1)
        ones <- matrix(1, n, n)
        dist <- ones / (dist_mat2_bike)
        dist[dist == Inf] <- 0
        dist[is.na(dist)==TRUE]<-0
        tot <- (1/(n*(n-1)))*sum(dist)

         # Render the global efficiency
          if (disrupted!=1){
            if (bike==1){
                unweighted_efficiency_bike<-tot
                output$unweighted_efficiency_bike <- renderUI({
                  tags$h1("Unweighted Efficiency with bike: ", tags$span(sprintf("%.5f", unweighted_efficiency_bike), style = "color: red;"))
                })
              unw_eff_bike_index(unweighted_efficiency_bike)
          if (!is.null(unweighted_efficiency)){
            efficiency_change<-(unweighted_efficiency_bike-unweighted_efficiency)/unweighted_efficiency
            output$efficiency_change <- renderUI({
            tags$h1("Efficiency Change: ", tags$span(sprintf("%.3f", efficiency_change),"%", style = "color: red;"))
          })
            eff_change_index(efficiency_change)
            
          }
          }
          }
      
          if (disrupted==1){
            unweighted_efficiency_bike<-unw_eff_bike_index()
            if (bike==1){
            unweighted_efficiency2_bike<-tot
            output$unweighted_efficiency2 <- renderUI({
              tags$h1("Disrupted Unweighted Efficiency with bikes: ", tags$span(sprintf("%.5f", unweighted_efficiency2_bike), style = "color: red;"))
            })
            print(unweighted_efficiency2_bike)
            print(unweighted_efficiency_bike)
        efficiency_change<-(unweighted_efficiency2_bike-unweighted_efficiency_bike)/unweighted_efficiency_bike
        print(efficiency_change)    
        
        if (efficiency_change!=0){
        output$efficiency_change <- renderUI({
          tags$h1("Efficiency Change: ", tags$span(sprintf("%.5f", efficiency_change),"%", style = "color: red;"))
        })
        }
            unw_eff_bike_index2(unweighted_efficiency2_bike)
            eff_change_index(efficiency_change)
            }
          }

      dist_bike_index(dist_mat2_bike)  
      eff_bike_index(1)
      results_index(results3)
      
   #    dist_mat2_bike<<-dist_mat2_bike
   #    eff_bike<<-1
   #    results3<<-results3
   # 
   ######calculate betweenness centrality#######
      g <- simplify(g)
      betweenness_centrality <- betweenness(g)
      top_nodes <- sort(betweenness_centrality, decreasing = TRUE)[1:10]
      output$top_nodes <- renderUI({
        tags$div(
          tags$h3("Top 10 Nodes for Betweenness Centrality:"),
          )
      })

      if (disrupted!=1){
       top_nodes_base<-top_nodes
        top_nodes_index(top_nodes_base)
      
      }
   print(top_nodes)
   top_nodes_base<-top_nodes_index()
   

      if (disrupted!=1){
        output$tabella <- renderTable({
          dati <- data.frame(
            Rank = c(1:10),
            Name = names(top_nodes)
          )
          colnames(dati) <- c('Rank<br>(status quo)', "Stop name")
          dati
        }, sanitize.text.function = function(x) x) # Per evitare che Shiny effettui la sanitizzazione dei tag HTML
      }

      index2<-0
      for (i in 1:10){
        if (length(which(names(top_nodes_base)[i]==names(top_nodes[]))) > 0) {
          index2[i]<-which(names(top_nodes_base)[i]==names(top_nodes[]))
        }else{index2[i]<-NA}}

      variation<-abs(c(1:10)-index2)
      variation[which(is.na(variation))]<-"OUT"

      if (disrupted==1){
        output$tabella <- renderTable({
          dati <- data.frame(
            Rank = c(1:10),
            Name = names(top_nodes_base),
            Variation = c(1:10)-index2
            # Name_disrupted = names(top_nodes)
          )
          dati$Name <- ifelse(is.na(dati$Variation), paste0("<span style='color: red;'>", dati$Name, "</span>"), dati$Name)
          # dati$Name_disrupted<-ifelse(dati$Name_disrupted %in% setdiff(dati$Name_disrupted, dati$Name), paste0("<span style='color: green;'>", dati$Name_disrupted, "</span>"), dati$Name_disrupted)
          dati$Variation <- paste0(
            '<svg xmlns="http://www.w3.org/2000/svg" width="15" height="15" viewBox="0 0 24 24">',
            ifelse(dati$Variation > 0, '<path fill="green" d="M12 2l-12 20h24z"></path><path fill="white" d="M12 6l8 8h-16z"></path>', ""),
            ifelse(dati$Variation < 0, '<path fill="red" d="M12 22l12-20h-24z"></path><path fill="white" d="M12 18l-8-8h16z"></path>', ""),
            ifelse(is.na(dati$Variation), '<path fill="red" d="M12 22l12-20h-24z"></path><path fill="white" d="M12 18l-8-8h16z"></path>', ""),
            '</svg>',
            # abs(dati$Variation)
            variation
          )
          colnames(dati) <- c('Rank<br>(status quo)', "Stop name", 'Rank variation<br>(after disruption)')

          dati
        }, sanitize.text.function = function(x) x) # Per evitare che Shiny effettui la sanitizzazione dei tag HTML

        output$table2<- renderTable({
          dati2 <- data.frame(
            Rank = c(1:10),
            Name = names(top_nodes_base),
            Name_disrupted = names(top_nodes)
          )
          # dati$Name <- ifelse(is.na(dati$Variation), paste0("<span style='color: red;'>", dati$Name, "</span>"), dati$Name)
          dati2$Name_disrupted<-ifelse(dati2$Name_disrupted %in% setdiff(dati2$Name_disrupted, dati2$Name), paste0("<span style='color: green;font-weight: bold;'>", dati2$Name_disrupted, "</span>"), dati2$Name_disrupted)
          dati2 <- dati2[, c("Rank", "Name_disrupted")]  # Selezionare solo le colonne che si desidera mostrare
          colnames(dati2) <- c('Rank<br>(after disruption)', "Stop name")
          dati2
        }, sanitize.text.function = function(x) x)

      }
      nodes<-nodes_index()
      color_nodes<-nodes[which(nodes$label %in% names(top_nodes)),]

      leafletProxy("mappa") %>%
        addCircleMarkers(data = color_nodes, lng = ~x, lat = ~y, radius = 2, color = "white",opacity=1)

      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    }) 

    observeEvent(input$resetButton1|input$resetButton2|input$resetButton3|input$resetButton4|input$resetButton5, {
      req(input$resetButton1 | input$resetButton2 | input$resetButton3 | input$resetButton4 | input$resetButton5)
      tryCatch({
      session$reload()
        
        disrupted_index(0)
        unw_eff_index(0)
        eff_bike_index(0)
        unw_eff_index2(0)
        bike_index(0)
        
        # disrupted<<-0
        # unweighted_efficiency<<-0
        # unweighted_efficiency_bike<<-0
        # unweighted_efficiency2<<-0
        # bike<<-0
      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
    })
    
    observeEvent(input$show_results, {
      tryCatch({
      
      # nodes<-nodes_index()
      eff_bike<-eff_bike_index()
      # dist_mat2<-dist_index()
      # dist_mat2_bike<-dist_bike_index()
      edges2<-edges_index()
      edges_bike<-edges_bike_index()
      # demand<-demand_index()
      # results3<-results_index()
      # g<-graph_index()
      
      if (eff_bike==1){
        edges2<-rbind(edges2,edges_bike)
      }
      g1<-graph.data.frame(edges2, directed = T)
      
      n_links<-input$n_links

      # removeUI(selector = "#top_nodes")
      output$top_nodes <- renderUI({
        NULL})

      scenario <- input$tabselected_scenario
      if (scenario == 10) {

        output$tabella <- renderTable({
          NULL})
        output$table2 <- renderTable({
          NULL})
        output$unweighted_efficiency2 <- renderUI({
          NULL})
        output$efficiency_change <- renderUI({
          NULL})
        output$unweighted_efficiency_bike <- renderUI({
          NULL})
        output$weighted_efficiency <- renderUI({
          NULL})
        output$selected_links <- renderUI({
          NULL})


        # g <- simplify(g1)
        betweenness_centrality <- edge_betweenness(g1, directed=T)
        top_links <- sort(betweenness_centrality, decreasing = TRUE)[1:n_links]
        removed_links <- which(betweenness_centrality %in% top_links)
        top_links<-attr(E(g1), "vnames")[which(betweenness_centrality %in% top_links)]

        output$removed_links <- renderUI({
          tags$div(
            tags$h3("Top Links by Betweenness Centrality:"),
            tags$ul(
              lapply(top_links, function(link) {
                tags$li(link)
              })
            )
          )
        })

      }
      else if (scenario == 20) {

        output$tabella <- renderTable({
          NULL})
        output$unweighted_efficiency2 <- renderUI({
          NULL})
        output$efficiency_change <- renderUI({
          NULL})
        output$unweighted_efficiency_bike <- renderUI({
          NULL})
        output$weighted_efficiency <- renderUI({
          NULL})
        output$selected_links <- renderUI({
          NULL})

        degree_centrality <- degree(g1)
        top_nodes <- names(sort(degree_centrality, decreasing = TRUE)[1:n_links])
        removed_links<-which((edges2$from %in% top_nodes)|(edges2$to %in% top_nodes))
        top_links<-c(E(g1)[.from(top_nodes)|.to(top_nodes)])
        top_links<-attr(E(g1), "vnames")[top_links[]]

        output$removed_links <- renderUI({
          tags$div(
            tags$h3("Top Links by Node Degree Centrality:"),
            tags$ul(
              lapply(top_links, function(link) {
                tags$li(link)
              })
            )
          )
        })

      }
      else if (scenario == 40) {

        output$tabella <- renderTable({
          NULL})
        output$unweighted_efficiency2 <- renderUI({
          NULL})
        output$efficiency_change <- renderUI({
          NULL})
        output$unweighted_efficiency_bike <- renderUI({
          NULL})
        output$weighted_efficiency <- renderUI({
          NULL})
        output$selected_links <- renderUI({
          NULL})

        removed_links<-sample(ecount(g1),n_links)
        random_links<-attr(E(g1), "vnames")[removed_links[]]

        output$removed_links <- renderUI({
          tags$div(
            tags$h3("Links selected randomly"),
            tags$ul(
              lapply(random_links, function(link) {
                tags$li(link)
              })
            )
          )
        })


      }
      else if (scenario == 30) {

        output$tabella <- renderTable({
          NULL})
        output$unweighted_efficiency2 <- renderUI({
          NULL})
        output$efficiency_change <- renderUI({
          NULL})
        output$unweighted_efficiency_bike <- renderUI({
          NULL})
        output$weighted_efficiency <- renderUI({
          NULL})
        output$selected_links <- renderUI({
          NULL})

        closeness_centrality <- closeness(g1)
        top_nodes <- names(sort(closeness_centrality, decreasing = TRUE)[1:n_links])
        removed_links<-which((edges2$from %in% top_nodes)|(edges2$to %in% top_nodes))
        top_links<-c(E(g1)[.from(top_nodes)|.to(top_nodes)])
        top_links<-attr(E(g1), "vnames")[top_links[]]

        output$removed_links <- renderUI({
          tags$div(
            tags$h3("Top Links by Node Closeness Centrality:"),
            tags$ul(
              lapply(top_links, function(link) {
                tags$li(link)
              })
            )
          )
        })

        }

      for (i in 1:length(removed_links)){
        edges2[removed_links[i],"color"]<-'white'
        leafletProxy("mappa") %>%
          addPolylines(lng = c(edges2[removed_links[i],"from_lng"], edges2[removed_links[i],"to_lng"]),
                       lat = c(edges2[removed_links[i],"from_lat"], edges2[removed_links[i],"to_lat"]),
                       color=edges2[removed_links[i],"color"])

      }

      print(edges2[removed_links,])
      # print(deleted_rows)
      edges2<-edges2[-removed_links,]

      print(nrow(edges2))
      disrupted_index(1)
      edges_index(edges2)
      # edges2<<-edges2
      # disrupted<<-1

      }, error = function(e) {
        showNotification(paste("Si è verificato un errore:", e$message), type = "error")
      })
      }, ignoreInit = TRUE) # ignoreInit = TRUE per evitare l'evento iniziale

    
    
    
  }
  shinyApp(ui = ui, server = server, options = list(width = '100%', height = '100%'))


