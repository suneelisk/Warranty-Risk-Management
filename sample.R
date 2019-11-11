library(shiny)
library(esquisse)
library(tidyverse)
library(itunesr)
library(plyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)

ui <- fluidPage(
  navbarPage(title = "Acer", selected = "Upload File",
             tabPanel(title = "Upload File",icon = icon("upload"),
                      
                      fluidPage(
                        
                        tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
                        
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                            overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                        tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                            overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
                        
                        
                        br(),
                        br(),
                        br(),
                        
                        column(7,
                               
                               # tags$h3(strong(em("Aim of this Analysi(s):")),style="text-align:center;color:#004264;font-size:180%"),br(),
                               # tags$div(h4("The identification of rare items, events or observations which raise suspicions",style="text-align:center;color:dimgrey"),align="center"),
                               # tags$div(h4("by differing significantly from the majority of the data.",style="text-align:center;color:dimgrey"),align="center"),
                               br(),br(),
                               tags$div(id = 'logo1',img(src="logo4.png",height='80%',width='80%'),align="center")
                        ),
                        
                        br(),
                        br(),
                        
                        column(5,
                               
                               
                               bootstrapPage(useShinyjs(),
                                             br(),
                                             
                                             tags$h3(strong(em(HTML("Acer Laptop"))),style="text-align:center;color:#024b74;font-size:180%"),
                                             br(),
                                             
                                             tags$div(id = 'logo2',img(src="logo1.jpg",height='35%',width='35%'),align="center"),br(),
                                             
                                             
                                             

                                             uiOutput('file'),
                                             # uiOutput('fileupload1'),
                                             uiOutput('checkbox'),
                                             uiOutput("button"),
                                             uiOutput("helptext"),
                                             br(),
                                             br(),
                                             bsPopover(id="check",title = "",content = "Note: I accept the SeaportAI Terms & Conditions.. Show the Analyse button",placement = "right"),
                                             tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                             
                                             
                                             #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                             br(),
                                             
                                             tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                                      tags$tbody("Need Help ?"),
                                                      tags$a(href = "http://seaportai.com/contact-us/", "Contact Us...", target="_blank")
                                             )
                               )
                        )
                        
                        
                        
                      )),
             # tabPanel("Tabple Output",
#          dataTableOutput("table")),
tabPanel("Branch",
         tags$div(bsButton("button1", label = "data", icon = icon("refresh")), align = "center"),

         tags$div(
           style = "height: 700px;", # needs to be in fixed height container
           esquisserUI(
             id = "esquisse",
             header = FALSE, # dont display gadget title
             choose_data = FALSE # dont display button to change data
           )
         )
),

tabPanel("City",
         tags$div(bsButton("button2", label = "data", icon = icon("refresh")), align = "center"),
         
         tags$div(
           style = "height: 700px;", # needs to be in fixed height container
           esquisserUI(
             id = "esquisse1",
             header = FALSE, # dont display gadget title
             choose_data = FALSE # dont display button to change data
           )
         )
)

# tabPanel("Branch_MV",
#          tags$div(bsButton("button3", label = "data", icon = icon("refresh")), align = "center"),
#          
#          tags$div(
#            style = "height: 700px;", # needs to be in fixed height container
#            esquisserUI(
#              id = "esquisse2",
#              header = FALSE, # dont display gadget title
#              choose_data = FALSE # dont display button to change data
#            )
#          )
# ),
# 
# tabPanel("City_MV",
#          tags$div(bsButton("button4", label = "data", icon = icon("refresh")), align = "center"),
#          
#          tags$div(
#            style = "height: 700px;", # needs to be in fixed height container
#            esquisserUI(
#              id = "esquisse3",
#              header = FALSE, # dont display gadget title
#              choose_data = FALSE # dont display button to change data
#            )
#          )
# )




)
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=100*1024^2)

  output[["file"]] = renderUI({
    tags$div(fileInput("file1", "Upload File", accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")), align = "center")
  })
  
  output[["checkbox"]] <- renderUI({
    input$reset
    tags$div(checkboxInput("check",tags$a(href = "http://seaportai.com/privacy-policy/", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
    
  })
  
  output[["button"]] <- renderUI({
    if(input$check==TRUE){
      tags$div(bsButton("analyse",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
               style="color:white;font-weight:100%;",align="center")
    }
  })
  
  output[["helptext"]] <- renderUI({
    if(input$check==TRUE){
      tags$div(helpText("To get results, click the 'Lets go!' button...",style="text-align:center"),align="center")
    }
  })
  
  Full_data <-reactive({
    file1 <- input$file1
    if(is.null(file1)) {return(NULL)}
    data <- read.csv(file1$datapath)
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data=data.frame(data)
    
    data
    
  })
  
  out = function(){
    sub_data = Full_data()[,c("Branch","New.Part.Description")]
    sub_data = na.omit(sub_data)
    
    # staten = unique(sub_data$CRM_STATE_PROVINCE)
    
    state = list()
    
    for(i in unique(sub_data$Branch)){
      
      print(paste0("state_",i))
      
      df = ddply(sub_data, .(sub_data$Branch, sub_data$`New.Part.Description`), nrow)
      Subset_data <- df[grep(i, df$`sub_data$Branch`), ]
      names(Subset_data) = c("Branch", "Part", "V1")
      subset_data1 = Subset_data[grep("Mainboard", Subset_data$Part), ]
      subset_data1$percent = round((subset_data1$V1/sum(Subset_data$V1))*100,2)
      if(dim(subset_data1)[1]==0){
        dsds = data.frame(Branch = unique(Subset_data$Branch), Part = c("Mainboard"), Sum = c(0), percent = c(0))
      }else{
        dsds = data.frame(Branch = unique(subset_data1$Branch), Part = c("Mainboard"), Sum = sum(subset_data1$V1), percent = sum(subset_data1$percent))
      }
      
      state[[i]] = dsds
    }
    
    final = do.call(rbind, state)
  }
  
  observeEvent(input$button, {
  output$table = renderDataTable({
    out()
  })
  })
  
  data_r <- reactiveValues(data = mtcars, name = "mtcars")
  
  observeEvent(input$button1, {
    
    data_r$data = out()
    data_r$name <- "Branch"
    
  })


  callModule(module = esquisserServer, id = "esquisse", data = data_r)
  
  
  pl_mb <- reactive({
    Subset_data <- Full_data()[grep("Mainboard", Full_data()$`New.Part.Description`), ]
    
    branch = ddply(Subset_data, .(Subset_data$`Product.Line`, Subset_data$`New.Part.Description`), nrow)
    names(branch) = c("Product_Line", "New Part Description", "Sum")
    
    branch1 = ddply(branch, .(branch$Product_Line), summarise, Sum = sum(Sum))
    
    branch1$percentage = round((branch1$Sum/sum(branch1$Sum))*100,1)
    branch1 = data.frame(branch1)
    percentage = branch1$percentage
    branch2 = data.frame(branch1[,c(1,2)], percentage)
    branch2
  })
  
  data_rr <- reactiveValues(data = mtcars, name = "mtcars")
  
  observeEvent(input$button2, {
    
    data_rr$data = pl_mb()
    data_rr$name <- "Branch"
    
  })
  
  
  callModule(module = esquisserServer, id = "esquisse1", data = data_rr)
  
  
  # chart_data1 = reactive({
  #   Subset_data <- Full_data()[grep("Mainboard", Full_data()$`New.Part.Description`), ]
  #   chart_data = Subset_data[,c("Case.Created.Date", "New.Part.Description")]
  #   chart_data1 <- ddply(chart_data, .(chart_data$`Case.Created.Date`), nrow)
  #   names(chart_data1)=c("Date", "Frequency")
  #   chart_data1$Date = dmy(as.character(chart_data1$Date))
  #   chart_data1 = data.frame(chart_data1)
  #   chart_data1
  # })    
  # 
  # 
  # data_rrr <- reactiveValues(data = mtcars, name = "mtcars")
  # 
  # observeEvent(input$button3, {
  #   
  #   data_rrr$data = chart_data1()
  #   data_rrr$name <- "Branch"
  #   
  # })
  # 
  # 
  # callModule(module = esquisserServer, id = "esquisse2", data = data_rrr)
  # 
  # 
  # pl_mb1 = reactive({
  #   Subset_data <- Full_data()[grep("Mainboard", Full_data()$`New.Part.Description`), ]
  #   chart_data = Subset_data[,c("Product.Line","Case.Created.Date", "New.Part.Description")]
  #   chart_data1 <- ddply(chart_data, .( chart_data$`Product.Line`, chart_data$`Case.Created.Date`), nrow)
  #   names(chart_data1)=c("ProductLine", "Date", "Frequency")
  #   chart_data1$Date = dmy(as.character(chart_data1$Date))
  #   chart_data1 = data.frame(chart_data1)
  #   chart_data1
  #   chart_data1$Year = substring(chart_data1$Date, 1,7)
  #   chart_data1$Day = substring(chart_data1$Date, 9,10)
  #   chart_data1
  # })
  # 
  # data_rrrr <- reactiveValues(data = mtcars, name = "mtcars")
  # 
  # observeEvent(input$button4, {
  #   
  #   data_rrrr$data = pl_mb1()
  #   data_rrrr$name <- "Branch"
  #   
  # })
  # 
  # 
  # callModule(module = esquisserServer, id = "esquisse3", data = data_rrrr)
  
}

shinyApp(ui, server)