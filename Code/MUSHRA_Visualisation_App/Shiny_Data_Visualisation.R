##### Shiny Visualisations
##### Daniel Dempsey

### Load in packages
library( readr ) # For reading in csv files
library( dplyr ) # Useful data frame 
library( shiny ) # For building the application
library( plotly ) # For creating interactive graphs

# Read in data
xp_dat <- read_csv( 'new_dat.csv', col_types = 'cdcdd' )
xp_dat$stimulus <- factor( xp_dat$stimulus, levels = c('Passive', 'Hybrid',
                                                       'Active', 'Anchor',
                                                       'Reference') )
subs <- sort( unique( xp_dat$subject ) )
vars <- as.list( colnames(xp_dat)[5:7] )
names(vars) <- c('Section', 'Signal Type', 'Source')

# Define UI ----
ui <- fluidPage(
  titlePanel("MUSHRA Data Visualisation"), tabsetPanel(
    tabPanel("Overall Data",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(inputId = "var", label = "Select type to group by:",
                             choices = vars),
                 
                 uiOutput(outputId = "case_control"),
                 
                 uiOutput(outputId = "test_control"),
                 
                 checkboxGroupInput(inputId = "sub", label = "Select participants to be included:",
                                    choices = subs, selected = subs)
               ),
               
               # Plot Output ----
               mainPanel(tabsetPanel(
                 tabPanel("Boxplots", plotlyOutput(outputId = "test_boxplots")),
                 tabPanel("Histograms", plotlyOutput(outputId = "test_hist"))
               )))),
    
    tabPanel("Individual Breakdown",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("test_sel", label = "Select test:",
                             choices = as.list(unique(xp_dat$test))),
                 
                 checkboxInput(inputId = "Ref_Anc2", label = "Include Anchor and Reference")),
                 
               mainPanel( plotlyOutput(outputId = "ind_plot") ))
    )
  ))

# Define server ----
server <- function(input, output, session) {
  xp_sub <- reactive({
    dat <- filter(xp_dat, subject %in% input$sub)
    dat
  })
  
  output$case_control <- renderUI({
    available_cases <- unique(xp_sub()[[input$var]])
    selectInput(inputId = "case_content", label = "Select case to display:",
                choices = as.list(available_cases))
  })
  
  observeEvent( input$case_content, {
    
    xp <- reactive({
      keep <- xp_sub()[[input$var]] == input$case_content
      dat <- xp_sub()[keep, ] 
      dat
    })
    
    output$test_control <- renderUI({
      available_tests <- unique(xp()$test)
      checkboxGroupInput("test_choose", "Select tests to be included:", 
                         available_tests, available_tests)
    })
    
    output$test_boxplots <- renderPlotly({
      dat <- filter(xp(), test %in% input$test_choose)
      fig <- plot_ly(x = ~stimulus, y = ~result, data = dat, 
                     type = "box", split = ~stimulus) %>% 
        layout(xaxis = list(title = "Decoder"), 
               yaxis = list(title = "Score", range = c(-1, 101)),
               legend = list(title = list(text = '<b> Decoder </b>')))
      fig
    })
    
    output$test_hist <- renderPlotly({
      dat <- filter(xp(), test %in% input$test_choose)
      dat_split <- split(dat, dat$stimulus)
      fig <- plot_ly(alpha = 0.6)
      nam <- names(dat_split)
      for (i in seq_along(dat_split)) {
        fig <- fig %>% add_trace(x = dat_split[[i]]$result, 
                                 type = "histogram", name = nam[i])
      }
      fig %>% layout(barmode = "overlay", xaxis = list(title = "Score"),
                     legend = list(title = list(text = '<b> Decoder </b>')))
    })
    
  })
  
  ind_set <- reactive({
    dat <- filter(xp_dat, test == input$test_sel)
    if (!input$Ref_Anc2) {
      dat <- filter(dat, !(stimulus %in% c("Reference", "Anchor")))
    }
    dat
  })
  
  output$ind_plot <- renderPlotly({
    
    p <- plot_ly()%>%
      layout(xaxis = list(title = "Decoder"),
             yaxis = list(title = "Score", range = c(-1, 101)),
             legend = list(title = list(text = '<b> Participant </b>')))
    
    for(i in subs){
      df <- filter(ind_set(), subject == i)
      ord <- match(levels(df$stimulus), df$stimulus)
      ord <- ord[!is.na(ord)]
      p <- p %>% add_trace(x = df$stimulus[ord], y = df$result[ord], name = i,
                           type = 'scatter',
                           mode = 'line')
    }
    
    p
    
  })
  
}

# Run App ----
shinyApp(ui, server)
