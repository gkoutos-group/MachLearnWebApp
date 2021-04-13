#Remember requires 3.6 R version

#source("PipelineShiny11.R", chdir = T)
#library("jsmodule")
#source("Module1.R")


#rm(list = ls(all = TRUE))
#devtools::session_info()

#clean(destroy = TRUE)
#cached()

#some issues with rlang?

require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(DT)
require(plotly)
require(tidyverse)
require(shinyBS)
require(shinyr)
require(shinycssloaders)
require(corrr)
require(recipes)
require(tidymodels)
require(vip)
require(furrr)
require(future)
require(promises)
require(plotly)
require(patchwork)
require(doFuture)
require(gridExtra)

#########
require(magrittr)
require(data.table)
require(glmnet)
require(ranger)

#update.packages(ask = FALSE, checkBuilt = TRUE)
########did somethinng with factors - if continous but levels not above x it is a factor##########

#setwd("/rds/homes/l/lxb732/Pipeline2")

# Functions ---------------------------------------------------------------

source("FunsPipeline14MachLearn.R")
source("TrialFS.R")
.seed <- 42L 
set.seed(.seed)


# Extras ------------------------------------------------------------------

require(parallel)
require(vip)
require(future.apply)
require(patchwork)
require(sjPlot) 
require(readr)
require(knitr)
require(kableExtra)


registerDoFuture()
plan(multiprocess, workers = availableCores())


############

#
#data <- read.csv("Gastro.csv")[,-1] %>%
#  rename("Label" = "Sarcopenia")
#selectedFeats <- FeatureSelection3(data) 
#KeyFeats1 <- unique(str_extract(as.character(selectedFeats[[1]]$`Variable Name`), "[^_]+"))
#if (length(KeyFeats1) <= 4){
#  KeyFeats <- KeyFeats1[1:length(KeyFeats1)]
#}else{
#  KeyFeats <- KeyFeats1[1:4]
#}
#
#Start <- choose(length(KeyFeats), 1:length(KeyFeats))
#lst2 <- sapply(seq_along(KeyFeats), function(j) combn(KeyFeats, j, simplify= FALSE))
#hh <- unlist(lst2, recursive=FALSE)
#hh <- hh[(Start[1]+1):length(hh)] 
#
#FeaturesS <- hh
#Boosted <- 2
#Alg <- c(1,2)
#print("runningHey2")
##c(1,2)
#FurrDat2 <- furrr::future_map(.x= c(1:length(FeaturesS)), ~iter_parallelFinal(.x, data, FeaturesS,1,Boosted,Alg), future.seed = TRUE)
##c(1,2)
#print("Models run!")
#
#g <-FurrDat2
#
#print("Models run 2!")
#
#if (length(which(lapply(g, is.null) == T)) != 0) {
#  hh1 <- KeyFeats[-which(sapply(g, is.null))]
#  
#  g1 <- g[-which(lapply(g, is.null) == T)]
#  
#} else{
#  hh1 <- KeyFeats[1:length(g)]
#  g1 <- g
#  
#}
#
#MergeOut <- MergeOutFuns(g1, SelectedModels)
#
#q3 <- MergeOut %>%
#  filter(Type == "Test", Metrics == "AUROC") %>%
#  select(-c(.metric, Type, ModelFinal)) %>%
#  arrange(desc(Mean)) %>%
#  mutate_if(is.numeric, ~ round(., 3))
#
#hh1 <- hh # **** THis is not KeyFeats!!!
#count <- list()
#for (i in 1:length(hh1)) {
#  count[[i]] <- length(hh1[[i]])
#}
#
#Variables <-
#  data.frame(
#    Names = (
#      gsub("^c\\(|\\)$", "", unlist(
#        as.character(
#          hh1
#        )))),
#    NumVars = t(data.frame(count)),
#    Run = c(1:length(hh1))
#  ) %>%
#  remove_rownames()
#
#q3A <- inner_join(q3, Variables) %>%
#  arrange(desc(Mean)) %>%
#  mutate_if(is.numeric, ~ round(., 3)) %>%
#  group_by(Model) %>%
#  top_n(n = dim(q3)[1], wt = Mean)
#
#q3A
#
#pp <- list()
#
#for (i in KeyFeats){
#  print(i)
#  pp[[i]] <- q3A %>% 
#    filter(grepl(i,Names)) %>%
#    mutate(Names2 = gsub(as.character(i), "", Names)) %>%
#    add_column(b = i) 
##  
#}
#
#pp <- pp %>%
#  bind_rows() 
#
#pp1 <- pp %>%
#  mutate_if(is.character, as.factor) %>% 
#  unite(New, c( Run, Names), sep = ":  ", remove = FALSE) %>%
#  filter(.,Model == "logit")
#
#ggplot(pp1,  aes(reorder(New, Mean), b)) + 
#  geom_tile(aes(fill = Mean),colour = "black",  width = 1, height = 0.00001) +   
#  scale_fill_stepsn(colours = topo.colors(20)[c( 2,8, 16)], name = "Mean AUC", n.breaks = 8) + 
#  facet_grid(NumVars~b, scales="free", space="free") +
#  theme_linedraw() + 
#  theme(axis.text = element_text(size = 10), 
#        panel.grid.major = element_blank(),
#        #panel.grid.minor = element_blank(),
#        strip.background =element_rect(fill="grey"),
#        strip.text = element_text(size = 10, colour = 'black'), 
#        legend.text=element_text(size=10)) +
#  scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0), labels = c())  + 
#  coord_flip() +
#  xlab("Model Runs") + ylab("Variables")  
##
#Size <- 14
#MergeOut %>%
#  #filter(.metric == "roc_auc") %>%
#  ggplot(aes(
#    x = Run,
#    y = Mean,
#    col = Model,
#    shape = Type
#  )) +
#  geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd),
#                  position = position_dodge(width = 0.8),
#                  size = 0.6) +
#  facet_grid(Metrics ~ ., scales = "free") +
#  scale_y_continuous(
#    labels = scales::percent,
#    breaks = seq(0, 1, 0.1),
#    expand = c(0, 0)
#  ) +
#  scale_x_continuous(breaks = seq(0, max(MergeOut$Run) + 2, 1), expand = c(0, 0)) +
#  labs(title = paste0("Bootstraps n=",  2)) +
#  theme(text = element_text(size = 12)) + theme(
#    plot.title = element_text(size = Size, face = "bold"),
#    legend.title = element_text(Size),
#    legend.text = element_text(size = Size),
#    strip.text = element_text(size = Size),
#    panel.spacing = unit(0.5, "lines")
#  )
#

############

#fluidRow(
#  column(width = 12, echarts4rOutput(ns("plot_monthly_blogs"), height = "200px")),

# UI ----------------------------------------------------------------------


ui = shinyUI(
  #  ui <- 
  dashboardPage( skin ="red", 
                 dashboardHeader(title = "Machine Learning for Biomarker Discovery"),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem("How it works", tabName = "works"),
                     menuItem("0. Setup", tabName = "setup"),
                     menuItem("6. Visualization", tabName = "vis"), #icon("chart-pie")),
                     menuItem("7. Feature Selection", tabName = "fs"), #icon("filter")),
                     menuItem("8. Model Training", tabName = "MT")) ##icon("brain"))) 
                 ),
                 dashboardBody(
                   uiOutput("AllFs"),
                   tabItems(
                     tabItem("works",
                             
                             fluidPage(
                               titlePanel("Objective"),
                               tags$h4("This app allows for the analysis of binary classification, through a three step approach. First it allows the user to explore the dataset and
                                          choose the outcome label, followed by simple visualizations. Then feature selection allows to create a better understanding of the most highly associated variables with the outcome of choice. Finally
                                          the top 4 features are then modeled through algorithms of choice. A report with all results can then be downloaded at the end."),
                               hr(), 
                               titlePanel("Instructions"),
                               tags$h4("Follow the numbers and instructions in the boxes and watch video below for a general example."),
                               
                               
                               HTML('<iframe width="1260" height="715" src=" https://www.youtube.com/embed/WsOwZ7kIFEQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                               #https://github.com/InFlamUOB/MyFiles/blob/master/ApplicationNoteBioinformatics2.pdf
                               
                               #htmlOutput('pdfviewer')
                               
                             )
                     ),
                     tabItem("setup",
                             
                             box(width = 4, title = '1. Input Dataset',solidHeader = T,status = 'primary',
                                 helpText("Choose already loaded datasets shown in paper, or upload your own dataset"),
                                 hr(),
                                 uiOutput("data_to_use"),
                                 uiOutput("input_data"),
                                 actionButton('btn_viewData',label = 'View Data',icon=icon('table', lib = "glyphicon")),
                                 hr(),
                                 bsModal('id',title = 'Dataset',trigger = 'btn_viewData',size = 'large',
                                         DTOutput("tbl")) 
                                 
                             ), 
                             
                             box(width =4, title = '2. Explore variables/columns', solidHeader = T,status = 'primary',
                                 uiOutput("columns_to_analyze"),
                                 helpText("Choose the column that is going to be the outcome variable. It has to be a binary variable to continue the analysis"),
                                 hr(),
                                 plotlyOutput("data_tab2_summary_plot",height = 200) %>% 
                                   withSpinner(color="#0dc5c1"),
                                 tableOutput("data_tab2_var_summary")
                             ), 
                             
                             box(width = 4, title = "3. Select/change the outcome variable", "Please select your dependent variable in the dropdown list of box 2, its name will change to 'Label'.", 
                                 " \n WARNING: It has to be a binary variable (factor) as we are performing binary classification",solidHeader = T,status = 'primary', hr(), 
                                 actionButton("load", "4. Select as outcome"),
                                 hr(), 
                                 DTOutput("tbl2")),
                             
                             infoBoxOutput("missingBox"), 
                             box(width = 5, title ="Preprocessing", solidHeader = F,status = 'info',   background = "purple",
                                 htmlOutput("ibox") %>% 
                                   withSpinner(color="#0dc5c1")) 
                             #downloadButton('downloadData')
                     ),
                     tabItem(tabName = "vis",
                             uiOutput("about_data"),
                             
                             box( width = 7, title = "Variables stratified by outcome (i.e Label)", solidHeader = T,status = 'primary', 
                                  #withSpinner(plotOutput("plot1"),type = 1),
                                  #withSpinner(plotOutput("plot1"),type = 1)
                                  plotOutput("plot1", height = 600) %>% 
                                    withSpinner(color="#0dc5c1")),
                             
                             box( width = 5, title = "Correlation Structure", solidHeader = T,status = 'primary', 
                                  plotOutput("plot2", height = 600) %>% 
                                    withSpinner(color="#0dc5c1")
                             )
                             #downloadButton('downloadVis')
                             
                     ),
                     tabItem(tabName = "fs",   
                             
                             box(title = "Feature Selection", solidHeader = T,status = 'primary', 
                                 
                                 plotOutput("FeatSel2", height = 680, width = 600) %>% 
                                   withSpinner(color="#0dc5c1"), 
                                 hr(),
                                 helpText("Feature selection performed by applying both random forest and elastic net and assesing feature importance through ranger and vip packages"),
                                 sliderInput("ThreshGrid", "Select Threshold: ",min=1, max=15, value=10, step=2),
                             ),
                             verbatimTextOutput("text"),
                             #verbatimTextOutput("What"),
                             
                             box( title = "Feature Ranking (top 5) ", solidHeader = T,status = 'primary', 
                                  
                                  helpText("Ranking created by combining both raking in model and if appeared in both or with different interactions (if used)"),
                                  hr(),
                                  DTOutput("tbl3")), 
                             box(title = 'Selected Features',  background = "light-blue",
                                 uiOutput("SelF"))
                             #downloadButton('downloadFs')
                     ),
                     
                     tabItem(tabName = "MT",
                             fluidRow(
                               box( title= "Machine Learning Models and Bootstraps", solidHeader = T,status = 'primary',
                                    
                                    helpText("Choose modeling approaches and amount of bootstraps. All models have hyperparamters that are optimized through 10 fold crossvalidation. Generalized Linear Model is 
                                             the quickest with the rest of models taking an exponentially higher time to process.Also, be mindful that a higher amount of bootstraps will also take more time, be patient. "),
                                    hr(),
                                    checkboxGroupInput("checkGroup", 
                                                       "Checkbox group", 
                                                       choices = list("Generalized Linear Models" = 1, 
                                                                      "Random Forest" = 2, 
                                                                      "Decision Trees" = 3, 
                                                                      "Boosted Tress" = 4, 
                                                                      "Regularized Boosted Trees" = 5),
                                                       selected = c(1,2)), 
                                    sliderInput("Boost", "Amount of Bootraps: ",min=2, max=8, value=2, step=1 ),
                                    
                                    hr(),
                                    
                                    actionButton('btn_train',label = 'Train Models',
                                                 icon = icon('cogs', lib = "glyphicon"), #'bullseye','rocket'
                                                 width='50%')
                               ), 
                               
                               verbatimTextOutput("Boost"),
                               
                               
                               box(
                                 title = 'Results Model Training',solidHeader = T,status = 'primary',
                                 helpText("Runs equate to a different combination of parameters. Each of these runs are modelled through the algorithms of choice."),
                                 hr(),
                                 DTOutput("FinalFinal"), 
                                 DTOutput("Variables"))
                             ),
                             fluidRow(
                               box(title = 'AUROC values by model and feature',width =12, status = 'warning',
                                   plotOutput("FinalFinalPlot2") %>% 
                                     withSpinner(color="#0dc5c1"),
                                   plotOutput("FinalFinalPlot") %>% 
                                     withSpinner(color="#0dc5c1"))
                             ),
                             #downloadButton('downloadMT')
                             #downloadButton('downloadReport')
                             radioButtons('format', 'Obtain report with results', c( 'HTML'),
                                          inline = TRUE),
                             downloadButton('downloadReport')
                             
                     )
                     
                   ) 
                 )      
  )
)



# Server ------------------------------------------------------------------

server = shinyServer(function( input, output,session) {
  
  #  output$pdfviewer <- renderText({
  #    return(paste('<iframe style="height:600px; width:100%" src="', "/rds/homes/l/lxb732/Pipeline2/ApplicationNoteBioinformatics2.pdf", '"></iframe>', sep = ""))
  # })
  
  #output$pdfviewer <- renderUI({
  #   tags$iframe(src="/rds/homes/l/lxb732/Pipeline2/ApplicationNoteBioinformatics2.pdf")
  # })
  
  set.seed(122)
  
  # Upload Data -------------------------------------------------------------
  
  output$data_to_use <- renderUI({
    radioButtons(inputId = "data_to_use_id", 
                 label = "Input Data", 
                 choices = c("Inflammation", "Upload"))
  })
  
  output$input_data <- renderUI({
    
    if(input$data_to_use_id == "Upload") {
      box(
        fileInput(inputId = "file1",
                  label = "Upload a csv file", 
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain", 
                             ".csv")),
        
        checkboxInput('headerUI','Header',TRUE),
        radioButtons('sepUI','Seperator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
        radioButtons('quoteUI','Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')
      )
      
    } else {
      
      selectInput("data", label= "Pick an Inflammatory Dataset", choices = names(datasets), 
                  selected = "GastroIntestinal",
                  multiple = FALSE)
      
      
    }
    
  })
  
  
  datasets <- list(
    GastroIntestinal = read.csv("GastroMachLearn.csv")[,-1],
    ICU = read.csv("ICUMachLearn.csv")[,-1],
    ChronicKidneyDisease = read.csv("CKDMachLearn.csv")[,-1]
  )
  
  
  readFile <- reactive({
    headerTag = input$headerUI;
    sepTag = input$sepUI;
    quoteTag = input$quoteUI;
    
    if(input$data_to_use_id == "Upload"){
      
      input_file <- input$file1
      
      input_file_path <- input_file$datapath
      
      x <- read.csv(input_file_path, header=headerTag,sep=sepTag,quote=quoteTag)
      
      y <- as.data.frame(x)
      
    } else {
      
      y <- datasets[[input$data]]
      
    }
    
    return(y)
    
  })
  
  
  # Upload Data -------------------------------------------------------------
  
  output$missingBox <- renderValueBox({
    data = readFile()
    missing = sapply(data, function(x) sum(is.na(x)))
    df.missing = data.frame(missing)
    total = sum(df.missing$missing)
    
    valueBox(
      paste0(total), "Missing Value(s)", icon = icon("search"),
      width = 3,
      color = "purple"
    )
  })
  
  sumdiplay = reactive({
    
    data <- Data2()
    p <- Conditional(data)
    
    Text <- list(
      ClassImbalance = "- 1. No  2. Downsample 3. Upsample",
      UseOfDummy = "- 1. No (no factors)  2. Yes",
      Normalization = "-  1. No (no numeric vars)  2. Yes",
      IncludeInteractions = "- 1. No (high feature count)  2. Yes")
    
    Text2 <- HTML(paste("For " , "<b>",  names(Text) , "</b>",  " the options are: " , "<b>",  Text , "</b>", ". Our choice is: ", "<b>",  p , "</b>", "<br />"))
    
    
    
    info <- lapply(seq_along(1:4),function(coln){
      infoBox(
        paste0(names(p)[coln],Text[coln]),
        paste0("Value: ", p[1, coln]),
        #icon = icon("search"),
        width = 4,  color = "yellow", fill = TRUE
      )
    })
    return(Text2)
  })
  
  output$ibox <- renderText({
    print(sumdiplay())
  })
  
  
  # Visualize Data ----------------------------------------------------------
  
  output$tbl <- DT::renderDataTable(server = FALSE,{
    DT::datatable(
      readFile(),class = 'cell-border stripe',
      selection=list(mode="multiple", target="row"),
      rownames=TRUE,
      extensions=c("Buttons",'Scroller'),
      options = list(scrollX = TRUE,pageLength = 5,
                     autoWidth = FALSE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 
                                 'excel'))
    )
  }
  )
  
  
  initial_analysis <- reactive({
    temp <- readFile()
    getDataInsight(temp)
  })
  
  
  Names <- reactive({
    
    if (!is.null(input$data)){
      print("Inflammation")
      print(input$data)
      Names <- names(datasets[input$data])
    } else if (!is.null(input$example_data)){
      
      print("NO Inflammation")
      Names <- input$example_data
      
      
    } else {
      Names <- "User data"
    }
    
  })
  
  
  output$about_data <- renderUI({
    
    x <- initial_analysis()
    
    box(
      title = "Dataset", 
      width = 12,
      infoBox(title = "Name", 
              value =  Names(), 
      ),
      infoBox(title = "Dimension", 
              value =  paste0(x$Rows ,"x" ,x$Columns),
              subtitle = "Rows x Columns"),
      infoBox(title = "Label",
              value = input$cols_to_analyze,
      )
    )
    
  })
  
  
  output$columns_to_analyze <- renderUI({
    # req(input$file1 | input$example_data)
    my_data <- readFile()
    # if(is.null(input$file1) & is.null(input$example_data)) {
    #   return()
    # } else{
    pickerInput(inputId = "cols_to_analyze",
                label = "Columns in dataset",
                choices = names(my_data),
                multiple = FALSE,
                selected = names(my_data))
    # }
    
  })
  
  #######
  observeEvent({input$cols_to_analyze},{
    r1 <- input$cols_to_analyze
    if(is.numeric(readFile()[, r1]) | is.integer(readFile()[, r1])){
      var.mean <- mean(readFile()[, r1], na.rm = TRUE)
      var.min  <- min(readFile()[, r1], na.rm = TRUE)
      var.max  <- max(readFile()[, r1], na.rm = TRUE)
      var.median <- median(readFile()[, r1], na.rm = TRUE)
      var.sd <- sd(readFile()[, r1])
      summary.vec <- c(var.mean, var.min, var.max, var.median, var.sd)
      var_s <- data.frame(summary.vec)
      names(var_s) <- r1
      row.names(var_s) <- c("Mean", "Min", "Max", "Median", "Standard Deviation")
      p <- plot_ly(y = ~ readFile()[, r1], type = "box", name = r1,
                   boxpoints = "all", jitter = 0.3,
                   pointpos = -1.8)%>%
        layout(yaxis = list(title = "Range"))
      
      
      
    } else if(is.factor(readFile()[, r1])){
      #| is.character(readFile()[, r1])
      #readFile()[,r1] <- as.factor(readFile()[,r1]) ##### added character and this line?
      var.n.levels <- length(levels(readFile()[, r1]))
      var.levels <- NULL
      for(i in 1:var.n.levels){var.levels <- c(var.levels,levels(readFile()[, r1])[i])}
      var_s <- c(var.n.levels)
      var_s <- data.frame(var_s)
      row.names(var_s) <- c("Number of Levels")
      names(var_s) <- r1
      factor.df <- group_by(readFile(), get(r1)) %>%
        summarise(count = n())
      names(factor.df) <- c(names(r1), "Count")
      p <- plot_ly(data = factor.df, name = "Levels",
                   x =  ~ get(names(factor.df)[1]),
                   y =  ~ get(names(factor.df)[2]), 
                   type = "bar") %>%
        layout(yaxis = list(title = "Count"),
               xaxis = list(title = "Levels"))
      
    } 
    
    # render the data summary into table
    output$data_tab2_var_summary <- renderTable(var_s, rownames = TRUE)
    output$data_tab2_summary_plot <- renderPlotly(p)
  })
  
  
  # Feature Selection -------------------------------------------------------
  
  
  
  Data2 <- eventReactive({input$load},{
    # Data2 <- eventReactive({input$cols_to_analyze},{
    r1 <- input$cols_to_analyze
    if ((is.factor(readFile()[, r1]))) {
      Data2 <- readFile() %>% 
        rename(Label = input$cols_to_analyze)  %>%
        drop_na() #added this!
      
      
    }else{
      Data2 <- readFile()
      showNotification("Need to pick a factor column: Do not proceed until preprocessing steps in preprocessing box are disclosed", type= "error", closeButton = TRUE, duration = NULL)
    }
    
    return(Data2)
  })
  
  
  
  output$tbl2 <- DT::renderDataTable(server = FALSE,{
    DT::datatable(
      
      Data2(),class = 'cell-border stripe',
      selection=list(mode="multiple", target="row"),
      rownames=TRUE,
      extensions=c("Buttons",'Scroller'),
      options = list(scrollX = TRUE,pageLength = 5,
                     autoWidth = FALSE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 
                                 'excel'))
      
    )
  }
  )
  
  
  
  plot1 <- function(){
    Plot2A(Data2())
  } 
  
  
  selectedFeaturesA <- reactiveValues(
    data = NULL
  )  
  
  
  observeEvent(input$load, {
    insertUI("#load", "afterEnd",
             actionButton("dynamic", "5. Click to proceed with visualization and feature selection"))
    observeEvent(input$dynamic, {
      #showModal(modalDialog("Doing a function WHAAAATTT", footer=NULL))
      output$plot1 <- renderPlot({ 
        Plot2A(Data2())
      })
      output$plot2 <- renderPlot({ 
        res <- getDataInsight((Data2()))
        corrplot::corrplot(as.matrix(res$cor_matrix),method = "color", tl.col="black")
      })
      
      observe({
        #selectedFeaturesA$data <- FeatureSelection3a(Data2()) ##
        selectedFeaturesA$data <- FeatureSelection3a(Data2(), "Label","C", input$ThreshGrid) #####
        #selectedFeaturesA$data <- readRDS("FeatureSelection.rds")
        #return(FSFinal)
        
      })
      #removeModal()
    })
  })
  
  

  FinalFS2 <-  reactive({
    
    #here prblem with variable ?¿
    FinalFS2 <- selectedFeaturesA$data[[1]]  
    FinalFS2
    
    
  }) 
  
  
  
  output$tbl3 <- DT::renderDataTable(server = FALSE,{
    DT::datatable(
      
      FinalFS2(),class = 'cell-border stripe',
      selection=list(mode="multiple", target="row"),
      rownames=TRUE,
      extensions=c("Buttons",'Scroller'),
      options = list(scrollX = TRUE,pageLength = 5,
                     autoWidth = FALSE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 
                                 'excel'))
    )
  }
  )
  
  
  FeatSel2 <- function(){
    
    
  #   grid.arrange(
  #  selectedFeaturesA$data[[2]] + 
  #    theme(text = element_text(size=12)), 
  #  #selectedFeaturesA$data[[3]] + 
  #  # theme(text = element_text(size=12)),
  #  selectedFeaturesA$data[[4]] + 
  #    theme(text = element_text(size=12)),
  #  nrow = 2, 
  #  #ncol = 3,
  #  top = "Variable Importance", 
  #  #widths = c(0.7, 1,0.8)
  #  heights = c(1,1)
    
    
    selectedFeaturesA$data[[2]]
    
    

   
  }
  
  
  
  
  output$FeatSel2 <- renderPlot(
    
    FeatSel2()
    
  )
  
  
  KeyFeats <- reactive({
    
    KeyFeats1 <- unique(str_extract(as.character(FinalFS2()$`Variable Name`), "[^_]+"))
    
    if (length(KeyFeats1) <= 4){
      KeyFeats <- KeyFeats1[1:length(KeyFeats1)]
    }else{
      KeyFeats <- KeyFeats1[1:4]
    }
    
  })
  
  
  FSel <-  reactive({
    
    Text <- KeyFeats()
    
    info2 <- lapply(seq_along(1:length(Text)),
                    function(coln){
                      infoBox(
                        paste0(Text[coln]),
                        #icon = icon("tick"),
                        width = 5,  color = "blue", fill = TRUE
                      )
                    })
    return(info2)
  })
  
  output$SelF <- renderUI({
    FSel()
  })
  
  
  
  KeyFeatsComb <- reactive({
    
    print("Feats")
    print(KeyFeats())
    Start <- choose(length(KeyFeats()), 1:length(KeyFeats()))
    lst2 <- sapply(seq_along(KeyFeats()), function(j) combn(KeyFeats(), j, simplify= FALSE))
    hh <- unlist(lst2, recursive=FALSE)
    hh <- hh[(Start[1]+1):length(hh)] 
    return(hh)
    
  })
  
  
  ##################################################################################################
  
  
  
  FinalFinal3 <- eventReactive(input$btn_train, {
    
    print("runningHey")
    
    FeaturesS <- KeyFeatsComb()
    Boosted <- input$Boost
    Alg <- as.numeric(input$checkGroup)
    data <- Data2()
    print("runningHey2")
    #c(1,2)
    FurrDat2 <- furrr::future_map(.x= c(1:length(FeaturesS)), ~iter_parallelFinal(.x, data, FeaturesS,1,Boosted,Alg))
    
    #FurrDat2 <- readRDS("ModelTest.RDS")
  })
  
  
  
  MergeOut <- reactive({
    
    SelectedModels <- as.numeric(input$checkGroup)
    #c(1,2)
    print("Models run!")
    g <- FinalFinal3()
    #g1 <-FurrDat2
    
    print("Models run 2!")
    
    if (length(which(lapply(g, is.null) == T)) != 0) {
      hh1 <- KeyFeatsComb()[-which(sapply(g, is.null))]
      
      g1 <- g[-which(lapply(g, is.null) == T)]
      
    } else{
      hh1 <- KeyFeatsComb()[1:length(g)]
      g1 <- g
      
    }
    
    MergeOut <- MergeOutFuns(g1, SelectedModels)
    
  }
  )
  
  Variables <- reactive({
    
    hh1 <- KeyFeatsComb()
    count <- list()
    for (i in 1:length(hh1)) {
      count[[i]] <- length(hh1[[i]])
    }
    
    Variables <-
      data.frame(
        Names = (
          gsub("^c\\(|\\)$", "", unlist(
            as.character(
              hh1
            )))),
        NumVars = t(data.frame(count)),
        Run = c(1:length(hh1))
      ) %>%
      remove_rownames()
    
  })
  
  q3A <- reactive({
    
    q3 <- MergeOut() %>%
      filter(Type == "Test", Metrics == "AUROC") %>%
      select(-c(.metric, Type, ModelFinal)) %>%
      arrange(desc(Mean)) %>%
      mutate_if(is.numeric, ~ round(., 3))
    
    q3A <- inner_join(q3, Variables()) %>%
      arrange(desc(Mean)) %>%
      mutate_if(is.numeric, ~ round(., 3)) %>%
      group_by(Model) %>%
      top_n(n = dim(q3)[1], wt = Mean)
    
    q3A
    
  })
  
  output$FinalFinal <- DT::renderDataTable(server = FALSE,{
    DT::datatable(
      
      q3A(),
      class = 'cell-border stripe',
      selection=list(mode="multiple", target="row"),
      rownames=TRUE,
      extensions=c("Buttons",'Scroller'),
      options = list(scrollX = TRUE,pageLength = 5,
                     autoWidth = FALSE,
                     dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 
                                 'excel'))
    )
  }
  )
  
  
  output$Variables <- renderDT(
    
    Variables(),
    class = 'cell-border stripe',
    selection=list(mode="multiple", target="row"),
    rownames=TRUE,
    options = list(scrollX = TRUE,pageLength = 5,
                   autoWidth = FALSE)
    
  )
  
  
  pp <- reactive({
    
    pp <- list()
    
    for (i in KeyFeats()){
      print(i)
      pp[[i]] <- q3A() %>% 
        filter(grepl(i,Names)) %>%
        mutate(Names2 = gsub(as.character(i), "", Names)) %>%
        add_column(b = i) 
      
    }
    
    pp <-  pp %>%
      bind_rows() 
    
  })
  
  
  SelMods <- function(){
    
    namesModels <- c("logit", "rf", "dt", "bt", "dtR")
    
    SelectedModels <- as.numeric(input$checkGroup)
    
    SelMods <- namesModels[c(SelectedModels)]
  }
  
  
  FinalFinalPlot2 <- function(){
    
    namesModels <- c("logit", "rf", "dt", "bt", "dtR")
    
    SelectedModels <- as.numeric(input$checkGroup)
    
    SelMods <- namesModels[c(SelectedModels)]
    
    pp1 <- list()
    count <- 0
    
    for (i in (SelMods)){
      
      count <- count + 1 
      pp1[[count]] <- pp() %>%
        mutate_if(is.character, as.factor) %>% 
        unite(New, c( Run, Names), sep = ":  ", remove = FALSE) %>%
        filter(.,Model == i) %>%
        ggplot( aes(reorder(New, Mean), b)) + 
        geom_tile(aes(fill = Mean),colour = "black",  width = 1, height = 0.00001) +   
        scale_fill_stepsn(colours = topo.colors(20)[c( 2,8, 16)], name = "Mean AUC", n.breaks = 8) + 
        facet_grid(NumVars~b, scales="free", space="free") +
        theme_linedraw() + 
        theme(axis.text = element_text(size = 10), 
              panel.grid.major = element_blank(),
              #panel.grid.minor = element_blank(),
              strip.background =element_rect(fill="grey"),
              strip.text = element_text(size = 10, colour = 'black'), 
              legend.text=element_text(size=10)) +
        scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0), labels = c())  + 
        coord_flip() +
        xlab("Model Runs") + ylab("Variables")  +   labs(title = paste0("Model: ",i))
      
      
    }
    
    
    pp1
    
    
  }
  
  
  output$FinalFinalPlot2 <- renderPlot({
    
    namesModels <- c("logit", "rf", "dt", "bt", "dtR")
    
    SelectedModels <- as.numeric(input$checkGroup)
    
    SelMods <- namesModels[c(SelectedModels)]
    
    
    if (length(SelMods) >= 2) {
      
      grid.arrange(
        FinalFinalPlot2()[[1]], 
        FinalFinalPlot2()[[2]],
        #nrow = 2, 
        ncol = 2,
        top = "Variable Importance", 
        widths = c(1, 1)
        #heights = c(1,1,1)
      )
    }else{
      
      FinalFinalPlot2()[[1]]
      
    }
    
    
    
    #FinalFinalPlot2()
    
  })
  
  FinalFinalPlot <- function(){
    
    
    theme_set(theme_bw())
    
    Size <- 14
    
    q4 <-  MergeOut() %>%
      #filter(.metric == "roc_auc") %>%
      ggplot(aes(
        x = Run,
        y = Mean,
        col = Model,
        shape = Type
      )) +
      geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd),
                      position = position_dodge(width = 0.8),
                      size = 0.6) +
      facet_grid(Metrics ~ ., scales = "free") +
      scale_y_continuous(
        labels = scales::percent,
        breaks = seq(0, 1, 0.1),
        expand = c(0, 0)
      ) +
      scale_x_continuous(breaks = seq(0, max(MergeOut()$Run) + 2, 1), expand = c(0, 0)) +
      labs(title = paste0("Bootstraps n=",  input$Boost)) +
      theme(text = element_text(size = 12)) + theme(
        plot.title = element_text(size = Size, face = "bold"),
        legend.title = element_text(Size),
        legend.text = element_text(size = Size),
        strip.text = element_text(size = Size),
        panel.spacing = unit(0.5, "lines")
      )
    
    
    q4
    
    
  }
  
  Boost <- function(){
    input$Boost
  }
  
  output$FinalFinalPlot <- renderPlot({
    FinalFinalPlot()
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format,  HTML = 'html'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('reportMachLearn.Rmd')
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'reportMachLearn.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('reportMachLearn.Rmd', switch(
        input$format,
        HTML = html_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  
  ##########################################################################################################
  
  output$AllFs <- renderUI({
    
    tags$head(tags$style(HTML("
    
                                .shiny-notification {
                                  position:fixed;
                                  top: calc(75%);
                                  left: calc(75%);
                                  width: 15em;
                                  opacity: 1;
                                }
                                
                                .btn {
                                color:rgb(255,255,255);
                                text-align: left;
                                #border-color:rgb(0,144,197);
                                background-color:rgb(0,144,197);}

                                # #load:active {
                                # background: green;
                                # }

                                .btn:hover{
                                #border-color:rgb(232,245,251);
                                background-color: rgb(232,245,251);color:   rgb(0,144,197);font-weight: bold;
                                }
                                .btn:focus{
                                background-color:green;
                                }

                                ")))
  })
  
  
  
  
})

shinyApp(ui, server)