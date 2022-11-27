
  library(shiny)
  library(shinydashboard)


  ui<-  dashboardPage( #creating a dashboard menu#
    dashboardHeader(title = "Dynamic sidebar"),#dashboard header#
    dashboardSidebar(sidebarMenu(id = "menu", sidebarMenuOutput("menu"))), #commanding the the inputs of the dashboard#
    dashboardBody( #contents that will appear on the body of the dashboard#
      fluidPage(
        tabItems(
          tabItem
          (tabName = "Data",
            fileInput(inputId = "file",
                      label = "Choose a CSV file",#creating a tab on the menu for file uploading#
                      accept = ".csv" #accepts csv files only#
            ),
            tableOutput(outputId = "Contents"), #the table output of the data#
            verbatimTextOutput(outputId = "DaTa")),
          tabItem
          (tabName = "means", h2("STATISTICS"),#creating a tab for means under statistics#
            fluidRow(column(width = 12, class = "well",
                            h4("Mean"),#heading of the output panel#
                            tableOutput(outputId="means")))#creating the output id to call the mean output#
          ),
          tabItem
          (tabName = "variances", h2("STATISTICS"),# creating a tab for variances under statistics menu#
            fluidRow(column(width = 12, class = "well",
                            h4("Covariance"),#heading for the output panel#
                            tableOutput(outputId = "variances")))#output id#
          ),

          tabItem
          (tabName = "histogram", h2("Histogram"), #tab for histograms under graphs menu#
            fluidRow(column(width = 12, class = "well",
                            h4("GRAPHS"),#heading for the output panel#
                            plotOutput("histogram")))#plotting the histogram#
          ),

          tabItem
          (tabName = "boxplots", h2("Boxplots"), #boxplots tab under graphs menu#
            fluidRow(column(width = 12, class = "well",
                            h4("GRAPHS"),#header for the output#
                            plotOutput("boxplots")))#plotting outputs#
          ),

          tabItem
          (tabName = "scatterplots", h2("Scatterplots"),#scatterplots tab#
            fluidRow(column(width = 12, class = "well",
                            h4("GRAPHS"),
                            plotOutput("Scatterplots")))


          )
        )
      )))

  server <- function(input, output, session) {
    #communication server#
    DATa <-reactive({       #reactive data importing#
      req(req(input$file))
      if(input$HEadings=="true"){
        j<-TRUE
      } else {
        j<-FALSE
      }

      if (is.null(req(req(input$file)))){
        cat("please upload csv file")}


      read.table(input$file$datapath,header=j,sep = input$separator)

    })
    output$Contents <- renderTable({

      DATa()
    })
    output$menu <- renderMenu({
      sidebarMenu( #reactive menu output from the menu tabs#
        menuItem("Data Input", tabName = "Data", icon = icon("Data"),
                 menuSubItem('Data', tabName = 'Data'),#creating a scroll down menu and reactive dashboard#
                 selectInput("separator", "Data Separator:",
                             c(Comma = ",",BlankSpace = "\t",Semicolon = ";")),
                 menuSubItem('Header', tabName = 'dataset'),
                 selectInput("HEadings", "Headings",
                             c("true","false"))),
        menuItem("statistics", tabName = "statistics", icon = icon("calender"),#creating a scroll down for the statistics to find variance and mean tabs#
                 menuSubItem('Mean', tabName = 'means'),
                 menuSubItem('variances', tabName = 'variances')),#submenu for variances#
        menuItem("graphs", tabName = "graphs", icon = icon("line_chart"),
                 menuSubItem('histogram', tabName = 'histogram'),
                 textInput(
                   inputId = "vinput", label = "variable",
                   value=1),
                 #input variable of the histogram ploting on the menu bar#
                 menuSubItem('boxplots', tabName = 'boxplots'),
                 menuSubItem('scatterplots', tabName = 'scatterplots')#panels for the scatterplot#
        )


      )})

    #instructions to the r console to plotting reactive to the user input on the dashboard#
    output$Scatterplots <- renderPlot({
      plot(DATa(),main="Scatterplots")
    })
    #plotting the scatterplots#
    output$variances <- renderTable({
      cov(DATa())
    })
    #variance computations#
    output$means<- renderTable({
      t(apply(DATa(),2,mean))
    })
    output$histogram <- renderPlot({
      k<-1:length(DATa())
      MAIN<-paste("Histogram of ",colnames(DATa())[k[input$vinput==k]])
      hist(DATa()[,k[input$vinput==k]],main=MAIN,xlab = "")
    }
    )
    #histogram plotting with option of chossing variable number#
    output$boxplots <- renderPlot({
      boxplot(DATa(),main="Boxplots")
    })
  }
  shinyApp(ui=ui, server=server)
