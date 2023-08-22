shinyUI(
  navbarPage(
    title = img(src="algoritma logo.png", height = "40px"), id = "navBar",
    theme = "paper.css",
    collapsible = TRUE,
    position = "fixed-top",
    header = tags$style(
      ".navbar-right {
                       float: right !important;
                       }",
      "body {padding-top: 75px;}"),
    inverse = T,
    
    # Panel 1 [Home]  ----------------------------------------------------------
    tabPanel(
      title = "HOME",
        imageOutput("banner"),
        h4(strong("Project Description")),
        p(
          style = "text-align: justify; font-size = 20px",
          "Request Dashboard is a Shiny application made with
        the purpose of",
        strong(
          em(
            "assist the operations team to recap and analyze item requests made by the Algoritma team,"
          )
        ),
        "Request Dashboard are necessary because
        previously to request, recap and analyze the data using several different media,
        which has an impact on the difficulty of recapitulating properly.
        With Request Dashboard hopefully will ease Algoritma team to do request item,
        and ease Operation team to recap, manage and analyze the request data",
        
        tags$blockquote(
          strong(
            "Request Dashboard is still under continuous development.
           Please look forward to future updates!"
          ))
        )
      ), # close of PANEL 1
 
    # Panel 2 [SLA] ------------------------------------------------------------
    tabPanel(
      title = "SLA",
      h2(strong("Service Level Agreement"), style = "text-align: center"),
      p(
        style = "text-align: center; font-size = 20px",
        "For the entire Algoritma team please pay attention to the estimated processing time listed in the following SLA, when making a submission. "
      ),
      tableOutput("tabel_sla")
      
    ), # close of PANEL 2
    
    # Panel 3 [Request form] ---------------------------------------------------
    tabPanel(
      # br(),
      # br(),
      # br(),
      title = "REQUEST FORM",
      surveyOutput(df = rbind(req_date,due_dates,df2),
                   survey_title = h2("Hello, Algoritma Team!",style="text-align: center"),
                   survey_description = h4(style="text-align: center","What request application do you want to make today?"),
                   theme = "white"
      )
      
    ), # close of PANEL 3
    
    
    # Panel 4 [Request Status]  ------------------------------------------------
    tabPanel(
      title = "REQUEST STATUS",
      fluidPage(
        tabsetPanel(
          tabPanel(
            "In Progress",
            h2(strong("Request in Progress"),style = "text-align: center"),
            dataTableOutput(outputId = "table_rip")
          ),
          tabPanel(
            "Done",
            h2(strong("Request Done"),style = "text-align: center"),
            dataTableOutput(outputId = "table_rd")
          ),
          tabPanel(
            "Cancel/Blocked",
            h2(strong("Request Canceled or Blocked"),style = "text-align: center"),
            dataTableOutput(outputId = "table_rcb")
          )
        ))
    ), # close of PANEL 4
  
    
    # Panel 5 [Request Overview]  ----------------------------------------------
    tabPanel(
      title = "REQUEST OVERVIEW",
      useShinydashboard(),
      tabsetPanel(
        tabPanel(title = "OVERVIEW",
                 fluidRow(
                   column(3),
                   column(6,
                          box(title = h6(strong("CHOOSE THE DATE RANGE"), style = "text-align: center"),
                            dateRangeInput("tgl",
                                           label = NULL,
                                           start = ymd("2023-08-08")),
                            width = 12,
                            height = 100
                          )
                          ),
                   column(3)
                 ),
                 
                 fluidRow(
                   box(title = h5(strong("NUMBER OF REQUEST"), style = "text-align: center"),
                       width = 12,
                       infoBoxOutput("box_tr", width = 3),
                       infoBoxOutput("box_rip", width = 3),
                       infoBoxOutput("box_rd", width = 3),
                       infoBoxOutput("box_rcb", width = 3)
                   )
                 ),
                 
                 fluidRow(
                   box(width = 12,
                       plotOutput("plot1",height = "250px"))
                 ),
                 
                 fluidRow(
                   box(width = 6,
                       plotOutput("plot2",height = "250px")),
                   box(width = 6,
                       plotOutput("plot3",height = "250px"))
                 ),
                 fluidRow(
                   box(width = 12,
                       plotOutput("plot4",height = "300px"))
                 )
                 ), # close of Overview Panel
        
        tabPanel(title = "DIVISION",
                 fluidRow(
                   
                   box(
                     dateRangeInput("tgl1",
                                    label = "Choose the Date Range",
                                    start = ymd("2023-08-08")), width = 8,height = 100),
                   
                   box(selectInput("team1", 
                                   label = "Choose the Division",
                                   choices = list("Business Development", 
                                                  "Marketing",
                                                  "Product"),
                                   selected = "Business Development"), width = 4, height = 100)
                   
                   
                 ),
                 fluidRow(
                   box(width = 8,
                       plotOutput(outputId = "plot5", height = 250)
                   ),
                   box(width = 4,
                       plotOutput(outputId = "plot6", height = 250)
                   )
                 ),
                 fluidRow(
                   box(width = 6,
                       plotOutput(outputId = "plot7", height = 250)
                   ),
                   box(width = 6,
                       plotOutput(outputId = "plot8", height = 250)
                   )
                 )
                 ) # close of Division Panel

      ) #close of tabset panel
    ) # close of PANEL 5
  )
)  
