## app.R ##
rm(list=ls())

library(shiny)
library(shinydashboard)
source("include.R")

#https://gist.git1hub.com/wch/5436415/


ui <- dashboardPage(
  dashboardHeader(title="MCN E2E Demonstration"),
  dashboardSidebar(
    dateRangeInput("dateRange", label="Interval dates"),
    textInput("user", label="user"),
    passwordInput("password", label="password"),
    actionButton("goButton", label="Go!", width="60%"),
    #submitButton(text="goButton", icon("refresh"), width="60%"),
    p(),
    tags$style(type='text/css', "button#goButton { margin-left: 15px; }"), #Do the magic to align button
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = shiny::icon("dashboard")),
      menuItem("Stats", tabName = "stats", icon = shiny::icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              #h2("Main Dashboard"),
              fluidRow(
                h3(textOutput("dates_info"))
              ),
              fluidRow(
                  infoBoxOutput("services", width=3),
                  infoBoxOutput("info_Deployment", width=3),
                  infoBoxOutput("info_Disposal", width=3)
              ),
              fluidRow(
                column(8, align="center",
                      h4(textOutput("deployinfo")),
                      plotlyOutput("plot1", width = "90%"),
                      p(),
                      h4(textOutput("deployinfo2")),
                      plotlyOutput("plot2", width = "90%"),
                      p(),
                      h4(textOutput("disposalinfo")),
                      plotlyOutput("plot3", width = "90%"),
                      p(),
                      h4(textOutput("disposalinfo2")),
                      plotlyOutput("plot4", width = "90%")
                )
              )
              #fluidRow(
              #  tableOutput("table_raw")
              #)
      ),
      
      # Second tab content
      tabItem(tabName = "stats",
              fluidRow(
                h3(textOutput("stats_dates_info"))
              ),
              fluidRow(
                column(8, align="center",
                  h4(textOutput("mcnstats")),
                  plotlyOutput("plot_stats_mcn", width = "90%"),
                  p(),
                  h4(textOutput("stats_all")),
                  plotlyOutput("plot_stats_all", width = "90%"),
                  p(),
                  h4(textOutput("stats_aaa")),
                  plotlyOutput("plot_stats_aaa", width = "90%")
                )
              )
      )
    )
  )
)




server <- function(input, output) {
  begin_date <- reactive({paste0(input$dateRange[1], " 00:00:00") })
  end_date <- reactive({paste0(input$dateRange[2], " 23:59:59")})
  
  #
  #control flow of app
  #
  observe({
    aux_ini <- ymd_hms(begin_date())
    aux_end <- ymd_hms(end_date())
    
    if (aux_ini > aux_end){ input$goButton = 0} # reset input button after everything has been drawn
      
  })
  
  myRaw_data <- eventReactive( input$goButton, {
    dd <- NULL
    if (input$goButton){
      withProgress(message="Please wait",detail="Loading data from Graylog", value=NULL,{
        dd<-get_data_from_graylog(start_period = begin_date(), end_period = end_date(), 
                                  user_=input$user, pass_ = input$password)
      })
      dd
    }
  })
  
  mydata <- reactive({
    dd <- NULL
    if (input$goButton){
      dd <- myRaw_data()
      if (!is.null(dd)){
        dd <- dd[dd$sm_name != '' & dd$so_phase != '' , ]
        
        dd <- dd[dd$sm_name %in% demo_e2e_services, ]
        #dd$sm_name <- factor(dd$sm_name, levels=levels_demo_e2e)
        
        dd$tim <- ymd_hms(dd$timestamp)
        dd$time_plot <- f_time_plot(dd)
        dd$time_plot2 <- f_time_plot(dd,4)
        dd$timestamp_unix <- f_to_timestamp(dd$timestamp)
        
        dd$response_time <- as.numeric(as.character(dd$response_time))
        #dd$so_phase <- factor(dd$so_phase, levels=levels_so_phase)
        dd$so_phase_mcn <- as.character(dd$so_phase)
        
        # order by time
        dd <- dd[with(dd, order(tim,timestamp)),]
        
        #Determine MCN phases
        mydbg("\n determining MCN phases")
        dd <- f_replace_so_phases(dd, demo_e2e_services, rm.retrieve = TRUE, rm.disposal=FALSE, all.sources=TRUE)
        
        #mydbg("\n determining duration")
        #dd <- calc_duration(dd, demo_e2e_services)
        #remove NAs at tim 
        dd <- dd[!is.na(dd$tim),]
      }
      dd
    }
  })
  
  mystats_data <- reactive({
    dd <- NULL
    if (input$goButton){
      dd <- myRaw_data()
      if (!is.null(dd)){
        dd <- dd[dd$sm_name != '' & dd$so_phase != '' , ]
        
        dd <- dd[dd$sm_name %in% demo_e2e_services, ]
        dd$tim <- ymd_hms(dd$timestamp)
        dd$response_time <- as.numeric(as.character(dd$response_time))

        # order by time
        dd <- dd[with(dd, order(tim,timestamp)),]
        
        #Determine MCN phases
        mydbg("\n determining MCN phases")
        dd <- f_stats_lifeCycle_mcn(dd, demo_e2e_services, rm.retrieve = FALSE, rm.disposal=FALSE, all.sources=TRUE)
      }
      dd
    }
  })
  
  
  
  number_of_deploys <- reactive({
    if (!is.null(mydata()) && input$goButton){
      aux <- mydata()
      #runs_init <- aux[aux$sm_name=="demo-e2e" & aux$so_phase=="init" & aux$phase_event=="start", "tim"]
      #aux_val <- aux[ aux$so_phase %in% c( "activate", "deploy") & aux$phase_event=="done", "response_time"]
      ab <- ddply(.data=aux[aux$phase_event=="done" & aux$so_phase %in% c( "activate", "deploy"), ] , .(sm_name, so_phase_mcn), .fun=summarize, mymean=mean(response_time)  )
      aux_val <- ddply(.data=ab, .(sm_name), .fun=summarize, mymean=sum(mymean ))
      if (length(aux_val) > 0){
        aa <- mean(aux_val$mymean, na.rm=TRUE)
        aa <- sprintf("%.3f", aa)
      }else{
        aa <- "N/A"
      }
      #aa <- length(runs_init)
      mydbg(paste0("Mean deploy (s) ", aa))
      aa
    }else{
      return(NA)
    }
  })
  
  
  number_of_disposal <- reactive({
    if (!is.null(mydata()) && input$goButton){
      aux <- mydata()
      #runs_init <- aux[aux$sm_name=="demo-e2e" & aux$so_phase=="destroy" & aux$phase_event=="start", "tim"]
      #aa <- length(runs_init)
      #aux_val <- aux[aux$so_phase %in% c("provision") & aux$phase_event=="done", "response_time"]
      ab <- ddply(.data=aux[aux$phase_event=="done" & aux$so_phase %in% c( "provision", "update"), ] , .(sm_name, so_phase_mcn), .fun=summarize, mymean=mean(response_time)  )
      aux_val <- ddply(.data=ab, .(sm_name), .fun=summarize, mymean=sum(mymean ))
      
      if (length(aux_val) > 0){
        aa <- mean(aux_val$mymean, na.rm=TRUE)
        aa <- sprintf("%.3f", aa)
      }else{
        aa <- "N/A"
      }
      #mydbg(paste0("Number of disposals", aa))
      mydbg(paste0("Mean provision (s) ", aa))
      aa
    }else{
      return(NA)
    }
  })
  
  services_count <- reactive({
    if (!is.null(mydata())){
      length(unique(mydata()$sm_name))
    }else{
      return(NA)
    }
  })
  
  
  #output$table_raw <- renderTable(mydata())
  
  
  output$services <- renderInfoBox({
    infoBox(
      "Services",
      services_count(),
      icon = icon("credit-card")
    )
  })
  
  output$info_Deployment <- renderInfoBox({
    infoBox(
      "Deployment (s)",
      number_of_deploys(),
      icon = icon("credit-card")
    )
  })
  
  output$info_Disposal <- renderInfoBox({
    infoBox(
      "Provision (s)",
      number_of_disposal(),
      icon = icon("hourglass")
    )
  })
  
  
  
  #
  #deployment
  #
  output$plot1 <- renderPlotly({
    if (!is.null(mydata())){
      
      #By default only draw the last deployment.
      aux <- mydata()
      max_ini_deploy <- max(aux[aux$sm_name=="demo-e2e" & aux$so_phase=="init" & aux$phase_event=="start", "tim"])
      mydbg(paste0("\n Last Deployment ", max_ini_deploy) )
      
      output$deployinfo <- renderText({
        paste0("Deployment ")
      })
      aux <- aux[aux$tim >= max_ini_deploy, ]
      
      mydbg("\n Drawing graphic")
      #dg_mcn2 <- f_grapg_plot_mcn(aux)
      
      #gla2 <- g2_graph_so_phase_mcn(dg_mcn2, scale_x=NULL, so_phases_mcn=c("Deploy", "Provision"), do.summary = TRUE)
      gla2 <- g2_graph_so_phase_mcn_bytim(aux, scale_x=NULL, so_phases_mcn=c("Deploy", "Provision"), do.summary = TRUE)
      gg <- ggplotly(gla2)
      gg
    }
  })
  
  #
  # Deployment details
  #
  output$plot2 <- renderPlotly({
    if (!is.null(mydata())){
      
      #By default only draw the last deployment.
      aux <- mydata()
      max_ini_deploy <- max(aux[aux$sm_name=="demo-e2e" & aux$so_phase=="init" & aux$phase_event=="start", "tim"])
      mydbg(paste0("\n Last Deployment ", max_ini_deploy) )
      
      output$deployinfo2 <- renderText({
        paste0("Details of deployment per service ")
      })
      aux <- aux[aux$tim >= max_ini_deploy, ]
      
      mydbg("\n Drawing Second graphic")
      
      #dg_mcn_plt <- f_calc_duration_phases_mcn(aux, demo_e2e_services, levels_demo_e2e)
      dg_mcn_plt <- aux
      
      dg_mcn_all_plt <- dg_mcn_plt[dg_mcn_plt$so_phase_mcn %in% c("Deploy", "Provision"), ]
      
      #gl_bw_ser <- g2_graph_response_time_per_so_phase_mcn(dg_mcn_all_plt, scale_y=NULL, rm.operation = TRUE)
      gl_bw_ser <- g2_graph_response_time_per_so_phase_mcn(dg_mcn_all_plt, scale_y=NULL, rm.operation = TRUE, type="response_time")
      gg <- ggplotly(gl_bw_ser)
      gg
    }
  })
  
  #
  # disposal
  #
  output$plot3 <- renderPlotly({
    if (!is.null(mydata())){
      
      #By default only draw the last deployment.
      aux <- mydata()
      max_ini_deploy <- max(aux[aux$sm_name=="demo-e2e" & aux$so_phase=="init" & aux$phase_event=="start", "tim"])
      mydbg(paste0("\n Last Deployment for disposal ", max_ini_deploy) )
      
      aux <- aux[aux$tim >= max_ini_deploy, ]
      
      mydbg("\n Drawing graphic of disposal")
      
      aux <- aux[aux$so_phase_mcn=="Disposal" ,]
      
      if (dim(aux)[1]>0){
        output$disposalinfo <- renderText({
          paste0("Disposal ")
        })
        
        #dg_mcn2 <- f_grapg_plot_mcn(aux)
        
        gla2 <- g2_graph_so_phase_mcn_bytim(aux, scale_x=NULL)
        gg <- ggplotly(gla2)
        gg
      }else{
        output$disposalinfo <- renderText({
          paste0("No Disposal for deployment ")
        })
        return(NULL)
      }
    }
  })
  
  
  #
  # disposal 2
  #
  output$plot4 <- renderPlotly({
    if (!is.null(mydata())){
      
      #By default only draw the last deployment.
      aux <- mydata()
      max_ini_deploy <- max(aux[aux$sm_name=="demo-e2e" & aux$so_phase=="init" & aux$phase_event=="start", "tim"])
      mydbg(paste0("\n Last Deployment for disposal ", max_ini_deploy) )
      
      aux <- aux[aux$tim >= max_ini_deploy, ]
      
      mydbg("\n Drawing graphic of disposal")
      
      aux <- aux[aux$so_phase_mcn=="Disposal" ,]
      
      if (dim(aux)[1]>0){
        output$disposalinfo2 <- renderText({
          paste0("Details of disposal ")
        })
        
        #dg_mcn2 <- f_grapg_plot_mcn(aux)
        
        #gla2 <- g2_graph_response_time_per_so_phase_mcn(dg_mcn2, scale_y=NULL , rm.operation = FALSE, is.update=FALSE, type="duration")
        gla2 <- g2_graph_response_time_per_so_phase_mcn(aux, scale_y=NULL , rm.operation = FALSE, is.update=FALSE, type="response_time")
        gg <- ggplotly(gla2)
        gg
      }else{
        output$disposalinfo2 <- renderText({
          paste0("No Disposal details for deployment ")
        })
        return(NULL)
      }
    }
  })
  
  #
  # Mcn Stats
  #
  output$plot_stats_mcn <- renderPlotly({
    aux <- mystats_data()
    if (!is.null(aux)){
      
      output$mcnstats <- renderText({
        paste0("Stats for MCN Life Cicle")
      })
      
      gla2 <- g2_graph_stats_lifecycle_mcn(aux,rm.start=TRUE, rm.responstime0=TRUE )
      gg <- ggplotly(gla2)
      gg
    }
  })
  
  #
  # Raw Stats coming from SM
  #
  output$plot_stats_all <- renderPlotly({
    aux <- mystats_data()
    if (!is.null(aux)){
      
      output$stats_all <- renderText({
        paste0("Stats for ALL Services Life Cicle (raw) ")
      })
      gla2 <- g2_graph_stats_lifecycle(aux,rm.start=TRUE, rm.responstime0=TRUE,service=NA )
      gg <- ggplotly(gla2)
      gg
    }
  })
  
}

shinyApp(ui, server)