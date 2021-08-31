#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(DT)
library(sjPlot)
library(pcalg)
library(networkD3)
library(igraph)

source("analysis.R")
source("plot_util.R")
on_server <- grepl("shiny-server", getwd())
if(on_server){
    result_dir <- "../dgm_demo/output/results"
} else{
    result_dir <- "data/from_server"
}

setup_workspace(result_dir)

var_choices <- setdiff(names(master), c("p_id",
                                       "time_started", 
                                       "time_ended", 
                                       "pilot", 
                                       "complete", 
                                       "num_restarts", 
                                       "language", "DEG.gender", "DEG.age", 
                                       "RAT.num_items", "SRS.num_items", "ART.num_items",
                                       "SRS.num_correct", "ART.num_correct"))
var_types <- c("categorial", "numeric")[1 + map_lgl(var_choices, ~{(master[[.x]] %>% class())[1] == "numeric"})]
var_data <- tibble(variable = var_choices, type = var_types)



theme_set(get_default_theme())

get_intro_text <- function(){
  div(h3("Welcome to the DGM DOTS DEMO  Analysis App"), 
         p("This app allows you visualize and inspect the data from a DGM DOTS DEMO study"),
      p("Have fun!"),
      style = "width:50%;text-align:justify")
}

impressum <- function(){
    p(
        "DGM DOTS DEMO  Monitor  v0.1", 
        shiny::tags$br(), 
        shiny::tags$br(), 
        "Author: Klaus Frieler", 
        shiny::tags$br(), 
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(), 
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        shiny::tags$br(), 
        shiny::tags$br(),
        shiny::a(href = "https://github.com/klausfrieler/dmg_monitor", "On Github", target = "_blank"), 
        style = "font-size: 10pt; display: block"
    )
    
}

input_width <- 300


ui_new <-   
    shiny::shinyUI(
        navbarPage(
            title = "DGM DOTS DEMO Memory", 
            theme = shinytheme("spacelab"),
            id = "tabs",
            tabPanel(
                "Home",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        htmlOutput("introduction"),
                        h4("Summary"),
                        tableOutput("overall_stats"),
                        htmlOutput("model_tab")
                    )
                    
                )
            ),
            tabPanel(
                "Univariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("uv_variable", "Variable:", var_choices, multiple = F), 
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("univariate_plot", width = "800px")
                        )
                    
                )
            ),            
            tabPanel(
                "Bivariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("bv_variable1", "Variable X:", var_choices, selected = "RAT.ability", multiple = F), 
                        selectizeInput("bv_variable2", "Variable y:", var_choices, selected = "SRS.perc_correct", multiple = F), 
                        actionButton("switch_axes", 
                                     label = "Switch axes", style = "margin-bottom: 10px"),
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("bivariate_plot", width = "800px"),
                        tableOutput("corr_tab")
                    )
                    
                )
            ),            
            
            tabPanel(
                "Data",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        DT::DTOutput("raw_data")
                    )
                    
                )
            ),
            tabPanel(
                "Causal Network",
                sidebarLayout(
                    sidebarPanel(
                        # Input: Select information ----
                        selectizeInput("pc_variable", "Variables:", num_predictors, selected = c("RAT.ability", "GMS.general", "ARTS.points"),
                                       multiple = T), 
                        selectInput(inputId = "alpha", 
                                    label = "Alpha Level",
                                    choices = c(.05, .01, .001), selected = ".05",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "charge", 
                                    label = "Node Charge",
                                    choices = seq(1, 5)*(-30), selected = "-60",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "link_distance", 
                                    label = "Link Distance",
                                    choices = seq(1, 5)*20 + 20, selected = "100",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "font_size", 
                                    label = "Font Size",
                                    choices = seq(1, 10)*2 + 12, selected = "16",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "opacity", 
                                    label = "Opacity",
                                    choices = seq(0, 1, .1), selected = "0.8",
                                    multiple = F, selectize = F),
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        forceNetworkOutput("pc_plot", width = "800px"),
                        htmlOutput("num_model_tab")
                        )
                )
            )))

# Define server logic required to draw a plot
server <- function(input, output, session) {
   message("*** STARTING APP***")
   check_data <- reactiveFileReader(1000, session, result_dir, setup_workspace)
   shiny::observeEvent(input$switch_axes, {
       x <- input$bv_variable1
       y <- input$bv_variable2
       updateSelectizeInput(session, inputId = "bv_variable1",
                            selected = y)
       updateSelectizeInput(session, inputId = "bv_variable2",
                            selected = x)
       
   })
   output$introduction <- renderUI({
     get_intro_text()
   })
   output$overall_stats <- renderTable({
      check_data()
     browser()
      p_id_stats <- master %>% 
         distinct(p_id, gender, age, GMS.general, complete) %>% 
         summarise(n_female = sum(gender == "female", na.rm = T), 
                   n_male = sum(gender == "male", na.rm = T), 
                   n_other = sum(gender == "other", na.rm = T), 
                   n_not_say = sum(gender == "not_say", na.rm = T), 
                   mean_age = mean(age, na.rm = T), 
                   mean_GMS = mean(GMS.general, na.rm = T), 
                   n_unique = n(),
                   n_complete = sum(complete, na.rm = T),
                   .groups = "drop")
      p_id_stats %>% 
         select(n_unique, n_complete, starts_with("n"), mean_age, mean_GMS, everything()) %>% 
          set_names("Total N", "Completed", "Females", "Males", "Other", "Rather not say",  "Mean Age", "Mean GMS General") 
      })
   
   output$raw_data <- renderDataTable({
       check_data()
       master %>% 
           select(-p_id, -num_restarts, -pilot,  -DEG.age, -DEG.gender) %>% 
           mutate_if(is.numeric, round, 2) %>% 
           select(time_started, time_ended, language, complete, age, gender, everything())
   }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
   
   output$univariate_plot <- renderPlot({
       check_data()
       var_info <- var_data %>% filter(variable == input$uv_variable)
       if(var_info$type == "numeric"){
           q <- univariate_plot_numeric(master, input$uv_variable, remove_na = T)
        } 
        else if (var_info$type == "categorial"){
            data <- master 
            coord_flip <- n_distinct(data[[input$uv_variable]]) > 3
            q <- univariate_plot_categorial(data, input$uv_variable,  remove_na = T, coord_flip = coord_flip)
        }
        else {
            return()
        }
        q
   })

   output$bivariate_plot <- renderPlot({
       check_data()
       #browser()
       if(input$bv_variable1 == input$bv_variable2){
           return()
       }
       bivariate_plot_auto(master, input, var_data, remove_na = T)   
    })
   
   output$pc_plot <- renderForceNetwork({
       check_data()
       if(length(input$pc_variable) < 2){
           return()
       }
       get_pc_graph(master %>% select(input$pc_variable), 
                    alpha = as.numeric(input$alpha),
                    charge = as.numeric(input$charge),
                    linkDistance = as.numeric(input$link_distance),
                    fontSize = as.numeric(input$font_size),
                    opacityNoHover = as.numeric(input$opacity)
                    )
   })
   
   output$corr_tab <- renderTable({
       check_data()
       vars <- get_parameters(data, input, var_data = var_data)
       if(vars$sub_type == "num-num" && input$bv_variable1 != input$bv_variable2) {
           get_correlations(master, input$bv_variable1, input$bv_variable2)   
       }
   },  caption = "Correlations", caption.placement = getOption("xtable.caption.placement", "top"))
   
   output$model_tab <- renderUI({
       check_data()
       lm_tab <- lm(scale(SRS.perc_correct) ~ 
                        scale(age) + 
                        scale(RAT.ability) + 
                        scale(GMS.general) + 
                        scale(ART.points) 
    , 
                    data = master ) %>% 
           sjPlot::tab_model()
       shiny::div(shiny::h4("Main Model"), shiny::HTML(lm_tab$knitr))
   })
   output$model_tab_stats <- renderTable({
       check_data()
       lm(SRS.perc_correct ~ age + RAT.ability + ART.points + GMS.general, data = master) %>% 
           broom::glance()
   },  caption = "Main Model Performance", caption.placement = getOption("xtable.caption.placement", "top"))
   output$num_model_tab <- renderUI({
       check_data()
       lm_tab <- get_model(master, predictors = input$pc_variable, output_format = "sj")
       shiny::div(shiny::h4("Causal Network Model Regression"), shiny::HTML(lm_tab$knitr))
   })
}

# Run the application 
shinyApp(ui = ui_new, server = server)

