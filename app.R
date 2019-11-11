library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)
library(DT)
library(rworldmap)
library(ggmap)
library(leaflet)
library(googleway)

gm= tags$h3(strong("Good Morning",style="color:#ff5c33"))
ga= tags$h3(strong("Good Afternoon",style="color:#ff5c33"))
ge= tags$h3(strong("Good Evening",style="color:#ff5c33"))

#===========
## FUNCTIONS
#===========
## SIMPLE GREETING
good_time <- function(){
  
  ## LOAD PACKAGE
  require(lubridate, quietly = T)
  
  ## ISOLATE currHour
  currhour = hour(now())
  
  ## RUN LOGIC
  if(currhour < 12){
    return(gm)
  } 
  else if(currhour >= 12 & currhour < 17){
    return(ga)
  }
  else if( currhour >= 17){
    return(ge)  
  }
}

register_google(key = "AIzaSyDUcfjDRiqexZjk3He8mHkBCFgllTu9Dis")


## STARTING LOGGED VALUE; LET'S CHANGE THAT!
Logged = FALSE;


#====
# UI
#====
## make login screen
ui1 <- function(){
  
  tagList(tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:#00CED1}')),
          div(id="container",align="center",
              div(id = "login",
                  # make login panel
                  wellPanel(id="well",style = "overflow-y: ;width:100%;height:100%",
                            br(),
                            HTML(paste0('
                                        <h2>
                                        Hello, ', good_time(),
                                        '</h2>',
                                        '<h3>
                                        <br>You are in Admin page.</br>
                                        </h3>')
                            ),
                            br(),
                            br(),
                            tags$div(textInput("userName", "Username",width = "100%"),align="left"),
                            br(),
                            tags$div(passwordInput("passwd", "Password",width = "100%"),align="left"),
                            br(),
                            # button
                            tags$div(actionButton("Login", "Log in"),align="center"),
                            # login error message
                            tags$div(uiOutput("message"),align="center")
                            )
                  
                  )
          ),
          # css for container
          tags$style(type = "text/css", 
                     "#container{
                     display: flex;
                     justify-content: center;
                     margin-top: 150px;
}"),
        # css for login well panel
        tags$style(type="text/css", "
                   #login,{
                   font-size:14px; 
                   width: 360px;}"),
        # well panel
        tags$style(type="text/css",
                   "#well{
                   padding: 50px;
                   background: white;
                   border: 1px;
                   box-shadow: ;}"),
        # welcome text css
        tags$style(type = 'text/css',
                   "h2, h3{
                   color: #525252;}"),
        # input fields
        tags$style(type="text/css",
                   "#userName, #passwd{
                   box-shadow: none;
                   outline:none;
                   border: none;
                   padding-left: 0;
                   border-bottom: 2px solid #ff5c33;
                   border-radius: 0;
                   }
                   #userName:focus, #passwd:focus{
                   box-shadow: 0px 10px 10px -5px lightgray;
                   }"),
        # button css
        tags$style(type='text/css',
                   "#Login{
                   outline: none;
                   margin-left: 0px;
                   width: 100px;
                   font-size: 12pt;
                   background: transparent;
                   border: 2px solid #ff5c33;
                   color: #ff5c33;
                   border-radius: 10px;
                   transition: 0.8s ease-in-out;
                   }
                   #Login:hover{
                   background: rgb(255, 92, 51);
                   color: white;}"),
        # error box - fadeOut animation
        tags$style(type="text/css",
                   "@-webkit-keyframes fadeOut {
                   from {
                   opacity: 1;
                   }
                   to {
                   opacity: 0;
                   }
                   }
                   @keyframes fadeOut {
                   from {
                   opacity: 1;
                   }
                   to {
                   opacity: 0;
                   }
                   }"),
        tags$style(type="text/css",
                   "#error-box{
                   margin-top: 20px;
                   margin-left: 0px;
                   padding: 5px 10px 5px 10px;
                   text-align: center;
                   width: 325px;
                   color: white;
                   background: #ef3b2c;
                   border: 1px solid #ef3b2c;
                   border-radius: 5px;
                   -webkit-animation: fadeOut;
                   animation: fadeOut;
                   opacity: 0;
                   animation-duration: 15s;}")
        )
  }

#=========
# PRINT UI
#=========
ui = (uiOutput("page"))

#========
# SERVER
#========

server = shinyServer(function(input, output,session){
  options(shiny.maxRequestSize=100*1024^2)
  users <- data.frame(User="risk",Password="acer")
  ## BEGIN BUILD LOG IN SCREEN
  USER <- reactiveValues(Logged = Logged)
  
  ## ERROR CHECKING
  observeEvent(input$Login,{
    
    ## output error message
    output$message <- renderUI({
      if(!is.null(input$Login)){
        my_username <- length(users$User[grep(pattern = input$userName, x = users$User)])
        my_password <- length(users$User[grep(pattern = input$passwd, x = users$Password)])
        if(input$Login > 0){
          if(my_username < 1 ||  my_password < 1){
            HTML("<div id='error-box'>
                 Sorry, that's not the right username or password. Please, 
                 try again. If you continue to have problems,
                 <a href='http://seaportai.com/contact-us/'>
                 <u>Contact Us..</u></a>
                 </div>")
          }
          }
          }
          })
    
    ## CHECK INPUT
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(users$User == Username)
          Id.password <- which(users$Password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username %in% Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
          })
  
  ## Make UI
  observe({
    # What to do when logged = F
    if (USER$Logged == FALSE) {
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    
    # Render UI when logged = T
    if (USER$Logged == TRUE) 
    {
      ## get the current user's authorization level 
      user_log <- toupper(input$userName)
      
      # if admin ("input.SELECT == 1 || input.FED == 2" )
      if(user_log == "RISK" ){
        output$page <- renderUI({
          ###################################################### ADMIN UI PAGE ###################################################################################################################
          fluidPage(
            tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
            theme = shinytheme("united"),
            withAnim(),
            #setBackgroundImage(src = "w.jpg"),
            tags$head(
              tags$style(type = 'text/css', 
                         HTML('
                              .navbar-default .navbar-brand{color: ;}
                              .tab-panel{ background-color: #; color: #}
                              .navbar-default .navbar-nav > .active > a, 
                              .navbar-default .navbar-nav > .active > a:focus, 
                              .navbar-default .navbar-nav > .active > a:hover {
                              color: #e6e6e6;
                              background-color: #;
                              
                              }')
                                                )
                         ),
            
            tags$style(HTML(".navbar  {
                            background-color:#005380; }
                            
                            .navbar .navbar-nav {float: right; margin-right: 35px;
                            margin-top: 26px;
                            color: #; 
                            font-size: 18px; 
                            background-color: #; }
                            
                            .navbar.navbar-default.navbar-static-top{ 
                            color: #; 
                            font-size: 23px; 
                            background-color: # ;}
                            
                            .navbar .navbar-header {
                            float: left;
                            background-color: # ;}
                            
                            .navbar-default .navbar-brand { color: #e6e6e6; 
                            margin-top: 10px;
                            font-size: 24px; 
                            background-color: # ;} 
                            
                            ")),
            tags$style(type="text/css",
                       "#well0{
                       padding: 100px;
                       background: white;
                       border: 1px;
                       box-shadow:2px 2px;}"),
            tags$style(type="text/css",
                       "#well2{
                       padding: 100px;
                       background: #;
                       border: 1px;
                       box-shadow:2px 2px;}"),
            tags$style(type="text/css",
                       "#well8{
                       padding: 100px;
                       background: #;
                       border: 1px;
                       box-shadow: 2px 2px;}"),
            tags$style(type="text/css",
                       "#rrr{
                       padding: 100px;
                       background: #;
                       border: 0px;
                       box-shadow: 0px 0px;}"),
            tags$head(
              tags$style(HTML("
                              input[type=\"number\"] {
                              font-size: 20px;height:50px;
                              }
                              
                              "))
              ),
            #tags$style(type='text/css','#qq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
            #tags$style(type='text/css','#qqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
            #tags$style(type='text/css','#qqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
            #tags$style(type='text/css','#qqqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
            
            tags$head(HTML("<title>SeaportAI Analytics</title> <link rel='icon' type='image/gif/png' href='t.png'>")),
            navbarPage(id="tabset",tags$li(class = "dropdown",
                                           tags$style(".navbar {min-height:100px }")
            ),
            #title = ,position = "fixed-top",selected = "Upload",inverse = TRUE,
            title = tags$div(img(src="log.png","SeaportAI(Analytics|Robotics)", style="margin-top: -4px;margin-left: 30px;", height = 60)),position = "fixed-top",selected = "Upload",inverse = F,
            
            tabPanel(title = "Upload",icon = icon("upload"),
                     
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
                              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
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
                                            
                                            
                                            
                                            withAnim(),
                                            
                                            uiOutput('fileupload'),
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
            
            
            tabPanel(title = strong("|")),
            
            tabPanel(
              title = "Summary", icon = icon("table"),
              br(),br(),br(),br(),br(),br(),br(),br(),
              fluidPage(
                fluidRow(
                  
                  
                  wellPanel(tags$div(h3(strong("Compliance Score"),style="color:#024b74;font-weight:100%;"),align="center")),
                  wellPanel(
                    downloadButton('downloadData7', 'Download data...'),br(),br(),br(),
                    addSpinner(DT::dataTableOutput("summary"),spin = "bounce", color = "#E41A1C"))
                  
                  
                ))),
            
            
            tabPanel(title = strong("|")),
            
            tabPanel(
              title = "Warranty", icon = icon("table"),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidPage(
                fluidRow(
                  
                  
                  wellPanel(tags$div(h3(strong("Warranty Date - Case Create Date!"),style="color:#024b74;font-weight:100%;"),align="center")),
                  wellPanel(
                    downloadButton('downloadData5', 'Download data...'),br(),br(),br(),
                    addSpinner(DT::dataTableOutput("Warranty"),spin = "bounce", color = "#E41A1C"))
                  
                  
                ))),
            
            tabPanel(title = strong("|")),
            
            tabPanel(
              title = "Multiple Repair", icon = icon("table"),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidPage(
                fluidRow(
                  
                  
                  wellPanel(tags$div(h3(strong("Branch & Serial Number"),style="color:#024b74;font-weight:100%;"),align="center")),
                  wellPanel(
                    downloadButton('downloadData3', 'Download data...'),br(),br(),br(),
                    addSpinner(DT::dataTableOutput("sums"),spin = "bounce", color = "#E41A1C")),
                  
                  wellPanel(tags$div(h3(strong("Customer Bill To City & Serial Number"),style="color:#024b74;font-weight:100%;"),align="center")),
                  wellPanel(
                    downloadButton('downloadData4', 'Download data...'),br(),br(),br(),
                    addSpinner(DT::dataTableOutput("sums2"),spin = "bounce", color = "#E41A1C"))
                  
                  
                ))),
            
            
            tabPanel(title = strong("|")),
            
            
            tabPanel(title = "Mainboard", icon = icon("table"),br(),br(),br(),br(),br(),br(),br(),br(),
                     fluidPage(
                       
                       
                       fluidRow(
                         
                         wellPanel(tags$div(h3(strong("Mianboard Damage vs Branch"),style="color:#024b74;font-weight:100%;"),align="center")),
                         wellPanel(
                           downloadButton('downloadData6', 'Download data...'),br(),br(),br(),
                           addSpinner(plotlyOutput("mainboard", height = "800px" ),spin = "bounce", color = "#E41A1C")),
                         wellPanel(tags$div(h3(strong("Mianboard Damage vs Customer Bill To City"),style="color:#024b74;font-weight:100%;"),align="center")),
                         wellPanel(
                           downloadButton('downloadData2', 'Download data...'),br(),br(),br(),
                           addSpinner(DT::dataTableOutput("mainboard1"),spin = "bounce", color = "#E41A1C")),
                         
                         tags$div(helpText(tags$a(href='http://www.seaportai.com',"- By SeaportAI",style="color:black", target="_blank"),style="text-align:center"),align="center")
                       )
                     )
                     
            ),
            
            
            
            
            
            
            
            tabPanel(title = strong("|")),
            
            
            tabPanel(
              title = "Serial Number", icon = icon("table"),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidPage(
                fluidRow(
                  
                  
                  wellPanel(tags$div(h3(strong("Serial Number Frequency!"),style="color:#024b74;font-weight:100%;"),align="center")),
                  wellPanel(
                    downloadButton('downloadData1', 'Download data...'),br(),br(),br(),
                    addSpinner(DT::dataTableOutput("Serial"),spin = "bounce", color = "#E41A1C")
                  )
                  
                  
                ))),
            
            
            tabPanel(title = strong("|")),
            
            
            
            
            
            tabPanel(title = "Monthly View", icon = icon("table"),br(),br(),br(),br(),br(),br(),br(),br(),
                     
                     fluidPage(
                       
                       fluidRow(
                         
                         # wellPanel(tags$div(h3(strong("Scatter Plot"),style="color:#024b74;font-weight:100%;"),align="center")),
                         # 
                         # wellPanel(
                         #   addSpinner(plotlyOutput('freqplot',height = "800px"),spin = "bounce", color = "#E41A1C")
                         # ),br(),
                         wellPanel(tags$div(h3(strong("Main Bord Issues Comparing by Month"),style="color:#024b74;font-weight:100%;"),align="center")),
                         wellPanel(
                           downloadButton('downloadData9', 'Download data...'),br(),
                           sliderInput("slider1", label = h3("Increase Value for good visualization"), min = 0, 
                                       max = 20000, value = 1000),br(),br(),
                           uiOutput("monthplotfd")
                         )
                         

                       )
                     )
                     
                     
                     
                     
                     
            ),
            
            tabPanel(title = strong("|")),
            
            tabPanel(
              title = "Map View", icon = icon("table"),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidPage(
                fluidRow(
                  
                  
                  wellPanel(tags$div(h3(strong("Maps"),style="color:#024b74;font-weight:100%;"),align="center")),
                  wellPanel(
                    downloadButton('downloadData8', 'Download data...'),br(),br(),br(),
                    
                    # fluidRow(
                    #   
                    #   column(2,
                    #          tags$div(numericInput("zoom",label= "Zoom",  value = 5, min = 1, max = 6), style="color:green;font-size:150%;", align="left")
                    #   ),
                    #   
                    #   column(2,
                    #          tags$div(selectInput("maptype" , label = "Map Type", choices = c( "satellite", "terrain", "hybrid", "roadmap", "hybrid"), 
                    #                               selected = "satellite"),style="color:green;font-size:150%;", align="left")
                    #   ),
                    #   column(2,
                    #          tags$div(selectInput("values" , label = "Select..", choices = c("Branch","Compliance_Score")),style="color:green;font-size:150%;", align="left")
                    #   )
                    # ),
                    
                    addSpinner(plotOutput('googleplot', height = "1500px", width = "100%"),spin = "bounce", color = "#E41A1C"))
                  
                  
                  
                ))),
            
            tabPanel(title = strong("|")),
            
            # tabPanel(title = "Help", icon = icon("table"),
            #          
            #          br(),br(),br(),br(),
            #          br(),br(),br(),br(),
            #          
            #          fluidPage(
            #            
            #            fluidRow(
            #              
            #              
            #              # wellPanel(tags$div(h3(strong(htmlOutput("cqqpast")),style="color:#004264;"),align="center")),
            #              wellPanel(
            #                # tags$div(h4(strong(htmlOutput("cont6")),style="color:#004264;font-weight:120%;"),align="center"),br(),
            #                # tags$div(h5("The main objective of the application is to estimate, 
            #                #             an appropriate annual premium for the health insurance 
            #                #             policies in the future using frequency-severity, 
            #                #             burning cost and exponential curve fitting. ")),
            #                # tags$div(h5("In order to use the application, the user 
            #                #             should follow the instructions given below.")),
            #                # br(),
            #                # tags$ol(
            #                #   tags$li("The file to be uploaded should be in .CSV format. "),
            #                #   br(),
            #                #   tags$li("The file uploaded should contain unique policies."),
            #                #   br(),
            #                #   tags$li("Before uploading the file, user should make few changes to their dataset. 
            #                #           The data should consist of the following variables :"),
            #                #   br(),
            #                #   tags$ul(
            #                #     tags$li("Age and Age Band (", span("Age Band", style = "color:red;"), "should be representated as A_BAND) of the policyholder."),
            #                #     br(),
            #                #     tags$li(strong("The variables like number of claims, exposure (Unit representing the measure of insured 
            #                #                    object to risk with the respect to risk start date (RSD), risk end date(RED) and valuation date of the policy), 
            #                #                    gross incurred cost (claim amount) and gross written premium (Premium paid by the policyholder) should be represented as NOC, 
            #                #                    EXP, GIC and GWP respectively.  
            #                #                    
            #                #                    "))
            #                #     )
            #                #     ),br(),
            #                # tags$div(h5("If these instructions are followed by the users, 
            #                #             the premium table based on age band will be displayed."))
            #              )
            #              
            #              
            #            )
            #          )     
            #          
            # ),
            tabPanel(
              title = "Product Line",
              br(),br(),br(),
              br(),br(),br(),
              br(),br(),br(),
            
            tabsetPanel(id = "tabselected", selected = "Summary",
                        
                        
                        tabPanel(
                          title = "Summary", icon = icon("table"),
                          br(),
                          fluidPage(
                            fluidRow(
                              
                              
                              wellPanel(tags$div(h3(strong("Compilance Score"),style="color:#024b74;font-weight:100%;"),align="center")),
                              wellPanel(
                                downloadButton('downloadData14', 'Download data...'),br(),br(),br(),
                                addSpinner(dataTableOutput("Productline5"),spin = "bounce", color = "#E41A1C"))
                              
                              
                            ))),
                        
                        tabPanel(
                          title = "Warranty", icon = icon("table"),
                          br(),
                          fluidPage(
                            fluidRow(
                              
                              
                              wellPanel(tags$div(h3(strong("Warranty"),style="color:#024b74;font-weight:100%;"),align="center")),
                              wellPanel(
                                downloadButton('downloadData13', 'Download data...'),br(),br(),br(),
                                
                                addSpinner(dataTableOutput("Productline4"),spin = "bounce", color = "#E41A1C")
                                
                                
                              )
                            ))),
                        
                        tabPanel(
                          title = "Multiple Repair", icon = icon("table"),
                          br(),
                          fluidPage(
                            fluidRow(
                              
                              
                              wellPanel(tags$div(h3(strong("Multiple Repair"),style="color:#024b74;font-weight:100%;"),align="center")),
                              wellPanel(
                                downloadButton('downloadData12', 'Download data...'),br(),br(),br(),
                                
                                addSpinner(dataTableOutput("Productline3"),spin = "bounce", color = "#E41A1C")
                                
                              )
                              
                            ))),
                        
                        
                        
              
              tabPanel(
                title = "Mainboard", icon = icon("table"),
                br(),
                fluidPage(
                  fluidRow(
                    
                    
                    wellPanel(tags$div(h3(strong("Main Bpoard"),style="color:#024b74;font-weight:100%;"),align="center")),
                    wellPanel(
                      downloadButton('downloadData10', 'Download data...'),br(),br(),br(),
                      
                      addSpinner(plotlyOutput('Productline1', height = "1500px", width = "100%"),spin = "bounce", color = "#E41A1C"))
                    
                    
                    
                  ))),
              
              tabPanel(
                title = "Monthly View", icon = icon("table"),
                br(),
                fluidPage(
                  fluidRow(
                    
                    
                    wellPanel(tags$div(h3(strong("Monthly View"),style="color:#024b74;font-weight:100%;"),align="center")),
                    wellPanel(
                      downloadButton('downloadData11', 'Download data...'),br(),
                      sliderInput("slider2", label = h3("Increase Value for good visualization"), min = 0, 
                                  max = 20000, value = 1000),br(),br(),
                      uiOutput("monthplotfdd")
                      )
                    
                    
                    
                  )))
              
            )
              
            ),
            
            
            tabPanel(title = strong("|")),
            
            
            
            navbarMenu("More",icon = icon("plus-square"),
                       tabPanel(
                         tags$div(tags$a(href="javascript:history.go(0)",bsButton("logoutadmin", label = "Logout", icon =   icon("repeat",lib = "glyphicon"),block = F, style="success"),style="text-align:center"),align="center"),
                         br()
                       )
            ))
            )
          
          #########################################################################################################################################################################
          
          
          
                              })
      }
      
      # if standard user
      else{
        output$page <- renderUI({
          
          
        })
      }
  }
        })
  
  # set.seed(644)
  # load("Full_data().rda")
  # load('Warrent_Date.rda')
  ####################################################### server #############################################################################################
  
  
  
  observeEvent(input$reset,{
    reset(id = "file")
  })
  
  output[["fileupload"]] <- renderUI({
    input$reset
    tags$div(fileInput("file",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept=c("text/csv","text/comma-separated-values,text/plain", ".csv")),align="center")
    
  })
  output[["fileupload1"]] <- renderUI({
    input$reset
    tags$div(fileInput("file1",label = tags$h4(strong(em("Upload data..")),style="color:#004264;font-size:160%"),accept=c("text/csv","text/comma-separated-values,text/plain", ".csv")),align="center")
    
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
  
  output[["monthplotfd"]] <- renderUI({
    if(input$check==TRUE){
      addSpinner(plotOutput('monthplot',height = input$slider1),spin = "bounce", color = "#E41A1C")
    }
  })
  
  output[["monthplotfdd"]] <- renderUI({
    if(input$check==TRUE){
      addSpinner(plotOutput('Productline2',height = input$slider2),spin = "bounce", color = "#E41A1C")
    }
  })
  
  # 
  # observe(addHoverAnim(session, 'logo1', 'pulse'))
  # observe(addHoverAnim(session, 'logo2', 'pulse'))
  # observe(addHoverAnim(session, 'analyse', 'shake'))
  # observe(addHoverAnim(session, 'reset', 'shake'))
  
  
  observeEvent(input$analyse, {
    confirmSweetAlert(
      session = session,
      inputId = "confirmation",
      type = "warning",
      title = "Are you sure the data was uploaded ?",
      btn_labels = c("Nope", "Yep"),
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$cont1<- renderText({
        paste0(div("Frequency before Smoothening",style="color:#004264;font-size:100%;"))
        
      })
    }
  })
  
  ############################################# Data ###############################################################################  
  
  Full_data <-reactive({
    file1 <- input$file
    if(is.null(file1)) {return(NULL)}
    data <- read.csv(file1$datapath)
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data=data.frame(data)
    
    data
    
  })
  Warran <-reactive({
    file2 <- input$file1
    if(is.null(file2)) {return(NULL)}
    data <- read.csv(file1$datapath)
    #data=data.frame(readxl::read_excel("ega.xlsx"))
    data=data.frame(data)
    
    data
    
  })
  
  ##############################################  Serial Number Frequency   ###########################
  
  bsm<-reactive({
    counts <- ddply(Full_data(), .(Full_data()$`Serial.Number`), nrow)
    names(counts) = c('Serialno', 'Freq')
    ascen = counts[order(as.integer(counts$Freq),decreasing = TRUE), ]
    ascen
    colors = unique(ascen$Freq)
    more3 = which(colors>3)
    more33 = colors[more3]
    more33
    tt = datatable(tibble(
      Serialno = ascen$Serialno,
      Freq = ascen$Freq
    )) %>% formatStyle("Freq",
                       background = styleEqual(c(more33), c(rep('red',length(more33)))))
    list(ascen = ascen, tt = tt)
  })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$Serial<-renderDataTable(
    bsm()$tt
  )
    }
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(bsm()$ascen, file)
    })
  
  # output$Serial <- renderDataTable({
  #   datatable(bsm(),filter = "top",
  #             extensions = c('Buttons', 'Scroller'),
  #             options = list(
  #               dom = 'Bfrtip',
  #               deferRender = TRUE,
  #               scrollY = 500,
  #               scroller = TRUE,
  #               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  #             ))
  # })
  
  ##################################################  Mainboard Issues Percentage Branch wise  #############################
  values <- reactive({
    
    sub_data = Full_data()[,c("Branch","New.Part.Description")]
    sub_data = na.omit(sub_data)
    
    # staten = unique(sub_data$CRM_STATE_PROVINCE)
    
    state = list()
    
    df = ddply(sub_data, .(sub_data$Branch, sub_data$`New.Part.Description`), nrow)
    
    for(i in levels(sub_data$Branch)){
      
      print(paste0("state_",i))
      
      Subset_data <- df[grep(i, df$`sub_data$Branch`), ]
      names(Subset_data) = c("Branch", "Part", "V1")
      subset_data1 = Subset_data[grep("Mainboard", Subset_data$Part), ]
      if(dim(Subset_data)[1]==0){
        dsds = data.frame(Branch = i, Part = c("Mainboard"), Sum = c(0), percent = c(0))
      }else{
        subset_data1$percent = round((subset_data1$V1/sum(Subset_data$V1))*100,2)
        if(dim(subset_data1)[1]==0){
          dsds = data.frame(Branch = unique(Subset_data$Branch), Part = c("Mainboard"), Sum = c(0), percent = c(0))
        }else{
          dsds = data.frame(Branch = unique(subset_data1$Branch), Part = c("Mainboard"), Sum = sum(subset_data1$V1), percent = sum(subset_data1$percent))
        }
      }
      
      state[[i]] = dsds
    }
    
    final = do.call(rbind, state)
    
    age <- function(percen){
      sapply(percen, function(x) if(x < 10) 'rgb(158,202,225)' else if (x > 10) 'rgba(255, 0, 0, 0.7)')
    }
    sdsd = age(final$percent)
    
    text=paste0(final$Branch," count: ",final$Sum," (",final$percent,"%)")
    plot = plot_ly(final, x = ~Branch, y = ~percent, type = 'bar', text = text,
                   marker = list(color = dput(sdsd),
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5))) %>%
      # add_trace(branch1, x = ~branch.Branch, y = ~percentage, type = "scatter", text = text, mode = "lines",
      #           line = list(color = "green"))%>%
      layout(title = "Mainboard Issues Vs Branch",
             xaxis = list(title = "Branch"),
             yaxis = list(title = "Percentage"))%>%
      layout(height = 800, width = 1800)
    plot
    
    list(final = final, plot = plot)
  })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$mainboard = renderPlotly({
    values()$plot
  })
    }
  })
  
  output$downloadData6 <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(values()$final, file)
    })
  
  
  
  
  
  ##################################################  Mainboard Issues Percentage Branch City wise  #############################
  values1 <- reactive({
    
    Subset_data <- Full_data()[grep("Mainboard", Full_data()$`New.Part.Description`), ]
    
    branch = ddply(Subset_data, .(Subset_data$`Customer.Bill.To.City`, Subset_data$`New.Part.Description`), nrow)
    names(branch) = c("City_Branch", "New Part Description", "Sum")
    
    branch1 = ddply(branch, .(branch$City_Branch), summarise, Sum = sum(Sum))
    
    branch1$percentage = paste0(round((branch1$Sum/sum(branch1$Sum))*100,3), " %")
    names(branch1)= c('City_Branch','Sum', 'percentage')
    branch1 = data.frame(branch1)
    
  })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$mainboard1 = renderDataTable({
    values1()
  })
    }
  })
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(values1(), file)
    })
  
  
  # output$mainboard1 <- renderDataTable({
  #   datatable(values1(),filter = "top",
  #             extensions = c('Buttons', 'Scroller'),
  #             options = list(
  #               dom = 'Bfrtip',
  #               deferRender = TRUE,
  #               scrollY = 500,
  #               scroller = TRUE,
  #               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  #             ))
  # })
  
  ###################################  Plot Mainboard Issue vs Case created date  ##################################
  
  chart_data1 = reactive({
    Subset_data <- Full_data()[grep("Mainboard", Full_data()$`New.Part.Description`), ]
    chart_data = Subset_data[,c("Case.Created.Date", "New.Part.Description")]
    chart_data1 <- ddply(chart_data, .(chart_data$`Case.Created.Date`), nrow)
    names(chart_data1)=c("Date", "Frequency")
    chart_data1$Date = dmy(as.character(chart_data1$Date))
    chart_data1 = data.frame(chart_data1)
    chart_data1
  })    
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$freqplot<-renderPlotly({
    text=paste0(chart_data1()$Date," Mainboard Damage: ",chart_data1()$Frequency)
    plot_ly(chart_data1(), x = ~Date, y = ~Frequency, type = 'scatter', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = "Mainbord Issues",
             xaxis = list(title = "Branch"),
             yaxis = list(title = "Percentage"))%>%
      layout(height = 800, width = 1800)
  })
    }
  })
  
  chart_data2 = reactive({
    Subset_data <- Full_data()[grep("Mainboard", Full_data()$`New.Part.Description`), ]
    chart_data = Subset_data[,c("Branch","Case.Created.Date", "New.Part.Description")]
    chart_data1 <- ddply(chart_data, .( chart_data$Branch, chart_data$`Case.Created.Date`), nrow)
    names(chart_data1)=c("Branch", "Date", "Frequency")
    chart_data1$Date = dmy(as.character(chart_data1$Date))
    chart_data1 = data.frame(chart_data1)
    chart_data1
    chart_data1$Year = substring(chart_data1$Date, 1,7)
    chart_data1$Day = substring(chart_data1$Date, 9,10)
    chart_data1
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  
  output$monthplot = renderPlot({
    
    chart_data2() %>%
      ggplot(aes(x = Day, y = Frequency)) +
      geom_point(color = "darkorchid4") +
      facet_wrap( Branch~ Year, ncol = 3) +
      labs(
        # subtitle = "Data plotted by year",
        y = "Mainboard Damage",
        x = "Day") + theme_bw(base_size = 15)+
      theme(panel.background = element_rect(fill = "lightblue",
                                            colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white"))
  })
  
    }
  })
  
  output$downloadData9 <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(chart_data2()[,c("Branch", "Date", "Frequency")], file)
    })
  
  ###################################################  Data summarise Branch VS Serial Number  ##########################
  Branch_serialno = reactive({
    sum1 = Full_data()[,c("Branch", "Serial.Number")]
    
    library(plyr)
    ds <- ddply(sum1, .(sum1$Branch,sum1$`Serial.Number`), nrow)
    
    
    ds1 = table(ds$`sum1$Branch`, ds$V1)
    ds1 = data.frame(ds1)
    
    ds1$Var2 = as.numeric(ds1$Var2)
    ones = ds1%>%filter(Var2 == 1)
    two_three = ds1%>%filter(Var2 >=2 & Var2<4)
    four_five = ds1%>%filter(Var2>=4 & Var2<6)
    six_ten = ds1%>%filter(Var2>=6 & Var2<11)
    morethanten = ds1%>%filter(Var2>=11)
    
    two_three1 = ddply(two_three,.(two_three$Var1), summarise, sum = sum(Freq))
    four_five1 = ddply(four_five,.(four_five$Var1), summarise, sum = sum(Freq))
    six_ten1 = ddply(six_ten,.(six_ten$Var1), summarise, sum = sum(Freq))
    morethanten1 = ddply(morethanten,.(morethanten$Var1), summarise, sum = sum(Freq))
    
    
    
    final = cbind(ones[,c(1,3)],two_three1[,2],four_five1[,2],six_ten1[,2],morethanten1[,2])
    names(final)= c("Branch", "Percen_Ones", "Percen_Two_Three", "Percen_Four_Five","Percen_Six_Ten", "Percen_Morethanten")
    final
    final$Sums = rowSums(final[,2:6, drop = FALSE])
    
    final1 = round((final[,2:6]/final[,7])*100,2)
    # final1$Ones = paste0(final1$Ones, " %")
    # final1$Two_Three = paste0(final1$Two_Three, " %")
    # final1$Four_Five = paste0(final1$Four_Five, " %")
    # final1$Six_Ten = paste0(final1$Six_Ten, " %")
    # final1$Morethanten = paste0(final1$Morethanten, " %")
    final1 = data.frame(final$Branch,final1,final$Sums)
    names(final1)[1] = c('Branch')
    names(final1)[7] = c('Sums')
    final1
    
    
  })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$sums = renderDataTable({
    Branch_serialno()
  })
    }
  })
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv( Branch_serialno(), file)
    })
  
  
  
  
  #######################    Data Summarise City_Wise_Branch Vs Serial Number  ###################################
  Customer_Bill_To_Cityserialno = reactive({
    
    
    sum2 = Full_data()[,c("Customer.Bill.To.City", "Serial.Number")]
    
    library(plyr)
    ds <- ddply(sum2, .(sum2$`Customer.Bill.To.City`,sum2$`Serial.Number`), nrow)
    names(ds)= c("Bill_City", "Serial_Number", "V1")
    
    ds1 = table(ds$Bill_City, ds$V1)
    ds1 = data.frame(ds1)
    ds1$Var2 = as.numeric(ds1$Var2)
    ones = ds1%>%filter(Var2 == 1)
    two_three = ds1%>%filter(Var2 >=2 & Var2<4)
    four_five = ds1%>%filter(Var2>=4 & Var2<6)
    six_ten = ds1%>%filter(Var2>=6 & Var2<11)
    morethanten = ds1%>%filter(Var2>=11)
    
    two_three1 = ddply(two_three,.(two_three$Var1), summarise, sum = sum(Freq))
    four_five1 = ddply(four_five,.(four_five$Var1), summarise, sum = sum(Freq))
    six_ten1 = ddply(six_ten,.(six_ten$Var1), summarise, sum = sum(Freq))
    morethanten1 = ddply(morethanten,.(morethanten$Var1), summarise, sum = sum(Freq))
    
    
    
    final = cbind(ones[,c(1,3)],two_three1[,2],four_five1[,2],six_ten1[,2],morethanten1[,2])
    names(final)= c("Branch", "Ones", "Two_Three", "Four_Five","Six_Ten", "Morethanten")
    final
    final$Sums = rowSums(final[,2:6, drop = FALSE])
    
    final1 = round((final[,2:6]/final[,7])*100,2)
    final1$Ones = paste0(final1$Ones, " %")
    final1$Two_Three = paste0(final1$Two_Three, " %")
    final1$Four_Five = paste0(final1$Four_Five, " %")
    final1$Six_Ten = paste0(final1$Six_Ten, " %")
    final1$Morethanten = paste0(final1$Morethanten, " %")
    final1 = data.frame(final$Branch,final1,final$Sums)
    names(final1)[1] = c('Branch')
    names(final1)[7] = c('Sums')
    final1
    
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$sums2 = renderDataTable({
    Customer_Bill_To_Cityserialno()
  })
    }
  })
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Customer_Bill_To_Cityserialno(), file)
    })
  
  
  
  ##############################################################  Warranty Date - Case Create Date  #############################
  
  Case_Created = reactive({
    # names(Warranty)[1] =c("Serial Number")
    # 
    # merged = left_join(Full_data,Warranty, by = "Serial Number")
    
    dates = data.frame(Full_data()$Branch,Full_data()$`Case.Created.Date`, Full_data()$WarrantyExpiryDate)
    names(dates) = c('Branch', 'Case Created Date', 'WarrantyExpiryDate')
    
    dates$sum = as.Date(as.character(dates$WarrantyExpiryDate), format = '%d-%m-%Y')-
      dmy(as.character(dates$`Case Created Date`))
    
    zero = dates%>%filter(sum<0)
    thirty = dates%>%filter(sum>=0 & sum<30)
    thirty_sixty = dates%>%filter(sum>=30 & sum<60)
    sixty_ninety = dates%>%filter(sum>=60 & sum<90)
    morethan_ninety = dates%>%filter(sum>=90)
    
    zero1 = ddply(zero,.(zero$Branch), nrow)
    thirty1 = ddply(thirty,.(thirty$Branch), nrow)
    thirty_sixty1 = ddply(thirty_sixty,.(thirty_sixty$Branch),nrow)
    sixty_ninety1 = ddply(sixty_ninety,.(sixty_ninety$Branch),nrow)
    morethan_ninety1 = ddply(morethan_ninety,.(morethan_ninety$Branch),nrow)
    names(zero1) = c("Branch", "Values")
    names(thirty1) = c("Branch", "Values")
    names(thirty_sixty1) = c("Branch", "Values")
    names(sixty_ninety1) = c("Branch", "Values")
    names(morethan_ninety1) = c("Branch", "Values")
    
    merged_final = merge(morethan_ninety1,thirty1, by = "Branch",all = TRUE)
    merged_final2 = merge(merged_final,thirty_sixty1, by = "Branch",all = TRUE)
    merged_final3 = merge(merged_final2,sixty_ninety1, by = "Branch",all = TRUE)
    merged_final4 = merge(merged_final3, zero1, by = "Branch",all = TRUE)
    
    names(merged_final4) = c("Branch", "Percen_Morethan_ninety", "Percen_Zero_Thirty", "Percen_Thirty_Sixty","Percen_Sixty_Ninety", "Percen_Beyond_Warranty")
    
    # merged_final4 = datatable(
    #   merged_final4, colnames = c("Branch", "% of Morethan_ninety", "% of Zero_Thirty", "% of Thirty_Sixty","% of Sixty_Ninety", "% of Beyond_Warranty")
    # )
    
    
    final_merged = merged_final4[,c(1,6,3,4,5,2)]
    final_merged[is.na(final_merged)] <- 0
    
    final_merged$sums = rowSums(final_merged[,2:6, drop = FALSE])
    
    final1 = round((final_merged[,2:6]/final_merged[,7])*100,2)
    # final1$Below_Thirty = paste0(final1$Below_Thirty, " %")
    # final1$Thirty_Sixty = paste0(final1$Thirty_Sixty, " %")
    # final1$Sixty_Ninety = paste0(final1$Sixty_Ninety, " %")
    # final1$Morethan_ninety = paste0(final1$Morethan_ninety, " %")
    
    final2 = data.frame(final_merged$Branch, final1, final_merged$sums)
    names(final2)[1] = c("Branch")
    names(final2)[7] = c("Sums")
    final2
  })
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$Warranty = renderDataTable({
    Case_Created ()
  })
    }
  })
  
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Case_Created(), file)
    })
  
  
  
  
  ######################################################  Summarise  ###########################################
  
  finalout = reactive({
    
    Branch = values()$final[,c(1,4)]
    Branch_serialno = Branch_serialno()[,6]
    Warranty = Case_Created()[,2]
    gfgf = data.frame(Branch, Branch_serialno, Warranty)
    names(gfgf) = c('Branch', "MB", "Morethan_10Times", "Beyond_Warranty")
    
    MB1 = mutate(gfgf, MB1 = case_when(MB<=3 ~ "Good",
                                       MB>3 & MB<=7  ~ 'Average',
                                       MB>7  ~ 'Bad')
    )
    
    Morethan_10Times1 = mutate(gfgf, Morethan_10Times1 = case_when(Morethan_10Times<=0.1 ~ "Good",
                                                                   Morethan_10Times>0.1 & Morethan_10Times<=0.2  ~ 'Average',
                                                                   Morethan_10Times>0.2  ~ 'Bad')
    )
    
    Below_Thirty1 = mutate(gfgf, Below_Thirty1 = case_when(Beyond_Warranty<=1 ~ "Good",
                                                           Beyond_Warranty>1 & Beyond_Warranty<=2  ~ 'Average',
                                                           Beyond_Warranty>2  ~ 'Bad')
    )
    
    
    
    gfgf1 = data.frame(gfgf, MB1[,5], Morethan_10Times1[,5], Below_Thirty1[,5])
    names(gfgf1)[5] = c("MB1")
    names(gfgf1)[6] = c("Morethan_10Times1")
    names(gfgf1)[7] = c('Below_Thirty1')
    gfgf1
    
    gfgf2 = gfgf1
    gfgf2$MB1 = ifelse(gfgf2$MB1 == 'Good',9,ifelse(gfgf2$MB1=='Average',5,0))
    gfgf2$Morethan_10Times1 = ifelse(gfgf2$Morethan_10Times1 == 'Good',9,ifelse(gfgf2$Morethan_10Times1=='Average',5,0))
    gfgf2$Below_Thirty1 = ifelse(gfgf2$Below_Thirty1 == 'Good',9,ifelse(gfgf2$Below_Thirty1=='Average',5,0))
    
    
    
    gfgf2$Compliance_Score = paste0(round((rowSums(gfgf2[,5:7])/27)*100,1)," %")
    
    final1 = data.frame(gfgf1, gfgf2[,8])
    names(final1)[8] = c("Compliance_Score")
    final1
    
    final2 = datatable(tibble(
      Branch = final1$Branch,
      MB =final1$MB,
      Morethan_10Times = final1$Morethan_10Times,
      Beyond_Warranty = final1$Beyond_Warranty,
      MB1 =final1$MB1,
      Morethan_10Times1 = final1$Morethan_10Times1,
      Below_Thirty1 = final1$Below_Thirty1,
      Compliance_Score = final1$Compliance_Score
    ), colnames = c("Branch", "% of MB Consumption", "% of Multiple Repair > 10 times", "% of repairs after warranty",
                    "% of MB Consumption", "% of Multiple Repair > 10 times", "% of repairs after warranty", "Compliance Score")) %>%
      formatStyle(c("MB1", "Morethan_10Times1", "Below_Thirty1"),
                  background = styleEqual(c("Bad","Average","Good"), c('red','yellow','green')))
    
    # names(final2) = c("Branch", "% of MB Consumption", "Multiple Repair > 10 times", "% of repairs after warranty",
    #                     "% of MB Consumption", "Multiple Repair > 10 times", "% of repairs after warranty", "Compliance Score")
    final2
    
    list(final1 = final1, final2 = final2)
    
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$summary = renderDataTable({
    finalout()$final2
  })
    }
  })
  
  output$downloadData7 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(finalout()$final1, file)
    })
  
  #########################################################  Google Mpas  ######################
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output$googleplot = renderPlot({
    
    
    data = finalout()$final1
    lat1 = geocode(as.character(finalout()$final1[,1]))
    data1 = data.frame(data, lat1)
    
    
    msm=input$values
    map=get_map(location= "india", zoom= 5, maptype = "satellite", source = "google" )
    mapPoints <- ggmap(map) +
      geom_point(data = data1, alpha = .5, color = "red", size=3)+
      geom_text(data = data1, aes(label=paste(data1$Branch,"-", data1$Compliance_Score)),hjust=0, vjust=0, color= "DarkOrange", size= 6)
    mapPoints
    
  })
    }
  })
  
  output$downloadData8 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(finalout()$final1, file)
    })
  
  
  ###############################################################  Product Line  ######################################
  
  ###############################################################  Main Board Damage  ########################################
  
  pl_mb <- reactive({
    sub_data = Full_data()[,c("Product.Line","New.Part.Description")]
    sub_data = na.omit(sub_data)
    
    # staten = unique(sub_data$CRM_STATE_PROVINCE)
    
    state1 = list()
    df = ddply(sub_data, .(sub_data$Product.Line, sub_data$`New.Part.Description`), nrow)
    
    for(i in unique(sub_data$Product.Line)){
      
      print(paste0("state_",i))
      
      
      #df[is.na(df$`sub_data$New.Part.Description`)] <- "ISK_PARt"
      Subset_data <- df[grep(i, df$`sub_data$Product.Line`), ]
      names(Subset_data) = c("Product.Line", "Part", "V1")
      subset_data1 = Subset_data[grep("Mainboard", Subset_data$Part), ]
      
      if(dim(Subset_data)[1]==0){
        dsds = data.frame(Product.Line = i, Part = c("Mainboard"), Sum = c(0), percent = c(0))
      }else{
        subset_data1$percent = round((subset_data1$V1/sum(Subset_data$V1))*100,2)
        if(dim(subset_data1)[1]==0){
          dsds = data.frame(Product.Line = unique(Subset_data$Product.Line), Part = c("Mainboard"), Sum = c(0), percent = c(0))
        }else{
          dsds = data.frame(Product.Line = unique(subset_data1$Product.Line), Part = c("Mainboard"), Sum = sum(subset_data1$V1), percent = sum(subset_data1$percent))
        }
      }
      
      state1[[i]] = dsds
    }
    
    final = do.call(rbind, state1)
    
    age <- function(percen){
      sapply(percen, function(x) if(x < 10) 'rgb(158,202,225)' else if (x > 10) 'rgba(255, 0, 0, 0.7)')
    }
    sdsd = age(final$percent)
    
    text=paste0(final$Product.Line," count: ",final$Sum," (",final$percent,"%)")
    plot = plot_ly(final, x = ~Product.Line, y = ~percent, type = 'bar', text = text,
                   marker = list(color = dput(sdsd),
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5))) %>%
      # add_trace(branch1, x = ~branch.Branch, y = ~percentage, type = "scatter", text = text, mode = "lines",
      #           line = list(color = "green"))%>%
      layout(title = "Mainboard Issues Vs Product.Line",
             xaxis = list(title = "Product.Line"),
             yaxis = list(title = "Percentage"))%>%
      layout(height = 800, width = 1800)
    plot
    list(final = final, plot = plot)
  })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$Productline1 = renderPlotly({
        pl_mb()$plot
      })
    }
  })
  
  output$downloadData10 <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(pl_mb()$branch2, file)
    })
  
  
  ###################################################   Plot Maind board issues date wise Monthly View  #######################################
  pl_mb1 = reactive({
    Subset_data <- Full_data()[grep("Mainboard", Full_data()$`New.Part.Description`), ]
    chart_data = Subset_data[,c("Product.Line","Case.Created.Date", "New.Part.Description")]
    chart_data1 <- ddply(chart_data, .( chart_data$`Product.Line`, chart_data$`Case.Created.Date`), nrow)
    names(chart_data1)=c("ProductLine", "Date", "Frequency")
    chart_data1$Date = dmy(as.character(chart_data1$Date))
    chart_data1 = data.frame(chart_data1)
    chart_data1
    chart_data1$Year = substring(chart_data1$Date, 1,7)
    chart_data1$Day = substring(chart_data1$Date, 9,10)
    chart_data1
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      
      output$Productline2 = renderPlot({
        
        pl_mb1() %>%
          ggplot(aes(x = Day, y = Frequency)) +
          geom_point(color = "darkorchid4") +
          facet_wrap( ProductLine~ Year, ncol = 3) +
          labs(
            # subtitle = "Data plotted by year",
            y = "Mainboard Damage",
            x = "Day") + theme_bw(base_size = 15)+
          theme(panel.background = element_rect(fill = "lightblue",
                                                colour = "lightblue",
                                                size = 0.5, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                colour = "white"), 
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                colour = "white"))
      })
      
    }
  })
  
  output$downloadData11 <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(pl_mb1()[,c("ProductLine", "Date", "Frequency")], file)
    })
  
  #####################################################  Multiple Repair  #####################################
  
  pl_mb2 = reactive({
    sum1 = Full_data()[,c("Product.Line", "Serial.Number")]
    
    library(plyr)
    ds <- ddply(sum1, .(sum1$`Product.Line`,sum1$`Serial.Number`), nrow)
    names(ds) = c("Product_Line", "Serial_number", "V1")
    
    
    ds1 = table(ds$Product_Line, ds$V1)
    ds1 = data.frame(ds1)
    
    ds1$Var2 = as.numeric(ds1$Var2)
    ones = ds1%>%filter(Var2 == 1)
    two_three = ds1%>%filter(Var2 >=2 & Var2<4)
    four_five = ds1%>%filter(Var2>=4 & Var2<6)
    six_ten = ds1%>%filter(Var2>=6 & Var2<11)
    morethanten = ds1%>%filter(Var2>=11)
    
    two_three1 = ddply(two_three,.(two_three$Var1), summarise, sum = sum(Freq))
    four_five1 = ddply(four_five,.(four_five$Var1), summarise, sum = sum(Freq))
    six_ten1 = ddply(six_ten,.(six_ten$Var1), summarise, sum = sum(Freq))
    morethanten1 = ddply(morethanten,.(morethanten$Var1), summarise, sum = sum(Freq))
    
    
    
    final = cbind(ones[,c(1,3)],two_three1[,2],four_five1[,2],six_ten1[,2],morethanten1[,2])
    names(final)= c("ProductLine", "Percen_Ones", "Percen_Two_Three", "Percen_Four_Five","Percen_Six_Ten", "Percen_Morethanten")
    final
    final$Sums = rowSums(final[,2:6, drop = FALSE])
    
    final1 = round((final[,2:6]/final[,7])*100,2)
    
    # final1$Percen_Ones = paste0(final1$Percen_Ones, " %")
    # final1$Percen_Two_Three = paste0(final1$Percen_Two_Three, " %")
    # final1$Percen_Four_Five = paste0(final1$Percen_Four_Five, " %")
    # final1$Percen_Six_Ten = paste0(final1$Percen_Six_Ten, " %")
    # final1$Percen_Morethanten = paste0(final1$Percen_Morethanten, " %")
    
    final1 = data.frame(final$ProductLine,final1,final$Sums)
    names(final1)[1] = c('Product Line')
    names(final1)[7] = c('Sums')
    final1  
  })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$Productline3 = renderDataTable({
        pl_mb2()
      })
    }
  })  
  
  output$downloadData12 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv( pl_mb2(), file)
    })
  
  
  ##########################################################  Warranty  #############################################
  
  pl_mb3 = reactive({
    dates = data.frame(Full_data()$`Product.Line`,Full_data()$`Case.Created.Date`, Full_data()$WarrantyExpiryDate)
    names(dates) = c('Branch', 'Case Created Date', 'WarrantyExpiryDate')
    
    dates$sum = as.Date(as.character(dates$WarrantyExpiryDate), format = '%d-%m-%Y')-
      dmy(as.character(dates$`Case Created Date`))
    
    zero = dates%>%filter(sum<0)
    thirty = dates%>%filter(sum>=0 & sum<30)
    thirty_sixty = dates%>%filter(sum>=30 & sum<60)
    sixty_ninety = dates%>%filter(sum>=60 & sum<90)
    morethan_ninety = dates%>%filter(sum>=90)
    
    zero1 = ddply(zero,.(zero$Branch), nrow)
    thirty1 = ddply(thirty,.(thirty$Branch), nrow)
    thirty_sixty1 = ddply(thirty_sixty,.(thirty_sixty$Branch),nrow)
    sixty_ninety1 = ddply(sixty_ninety,.(sixty_ninety$Branch),nrow)
    morethan_ninety1 = ddply(morethan_ninety,.(morethan_ninety$Branch),nrow)
    names(zero1) = c("Branch", "Values")
    names(thirty1) = c("Branch", "Values")
    names(thirty_sixty1) = c("Branch", "Values")
    names(sixty_ninety1) = c("Branch", "Values")
    names(morethan_ninety1) = c("Branch", "Values")
    
    merged_final = merge(morethan_ninety1,thirty1, by = "Branch",all = TRUE)
    merged_final2 = merge(merged_final,thirty_sixty1, by = "Branch",all = TRUE)
    merged_final3 = merge(merged_final2,sixty_ninety1, by = "Branch",all = TRUE)
    merged_final4 = merge(merged_final3, zero1, by = "Branch",all = TRUE)
    
    names(merged_final4) = c("Branch", "Percen_Morethan_ninety", "Percen_Zero_Thirty", "Percen_Thirty_Sixty","Percen_Sixty_Ninety", "Percen_Beyond_Warranty")
    
    # merged_final4 = datatable(
    #   merged_final4, colnames = c("Branch", "% of Morethan_ninety", "% of Zero_Thirty", "% of Thirty_Sixty","% of Sixty_Ninety", "% of Beyond_Warranty")
    # )
    
    
    final_merged = merged_final4[,c(1,6,3,4,5,2)]
    final_merged[is.na(final_merged)] <- 0
    
    final_merged$sums = rowSums(final_merged[,2:6, drop = FALSE])
    
    final1 = round((final_merged[,2:6]/final_merged[,7])*100,2)
    
    # final1$Percen_Beyond_Warranty = paste0(final1$Percen_Beyond_Warranty, " %")
    # final1$Percen_Zero_Thirty = paste0(final1$Percen_Zero_Thirty, " %")
    # final1$Percen_Thirty_Sixty = paste0(final1$Percen_Thirty_Sixty, " %")
    # final1$Percen_Sixty_Ninety = paste0(final1$Percen_Sixty_Ninety, " %")
    # final1$Percen_Morethan_ninety = paste0(final1$Percen_Morethan_ninety, " %")
    
    final2 = data.frame(final_merged$Branch, final1, final_merged$sums)
    names(final2)[1] = c("Product Line")
    names(final2)[7] = c("Sums")
    final2
  })
 
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$Productline4 = renderDataTable({
        pl_mb3()
      })
    }
  })  
  
  
  output$downloadData13 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(pl_mb3(), file)
    })
  
  ###################################################  Summary Product Line  #################################
  
  pl_mb4 = reactive({
    
    Branch = pl_mb()$final[,c(1,4)]
    Branch_serialno = pl_mb2()[,c(6)]
    Warranty = pl_mb3()[,2]
    gfgf = data.frame(Branch, Branch_serialno, Warranty)
    names(gfgf) = c('Branch', "MB", "Morethan_10Times", "Beyond_Warranty")
    
    MB1 = mutate(gfgf, MB1 = case_when(MB<=0 ~ "Good",
                                       MB>0 & MB<=0.5  ~ 'Average',
                                       MB>0.5  ~ 'Bad')
    )
    
    Morethan_10Times1 = mutate(gfgf, Morethan_10Times1 = case_when(Morethan_10Times<=0 ~ "Good",
                                                                   Morethan_10Times>0 & Morethan_10Times<=0.5  ~ 'Average',
                                                                   Morethan_10Times>0.5  ~ 'Bad')
    )
    
    Below_Thirty1 = mutate(gfgf, Below_Thirty1 = case_when(Beyond_Warranty<=2 ~ "Bad",
                                                           Beyond_Warranty>2 & Beyond_Warranty<=4  ~ 'Average',
                                                           Beyond_Warranty>4  ~ 'Good')
    )
    
    
    
    gfgf1 = data.frame(gfgf, MB1[,5], Morethan_10Times1[,5], Below_Thirty1[,5])
    names(gfgf1)[5] = c("MB1")
    names(gfgf1)[6] = c("Morethan_10Times1")
    names(gfgf1)[7] = c('Below_Thirty1')
    gfgf1
    
    gfgf2 = gfgf1
    gfgf2$MB1 = ifelse(gfgf2$MB1 == 'Good',9,ifelse(gfgf2$MB1=='Average',5,0))
    gfgf2$Morethan_10Times1 = ifelse(gfgf2$Morethan_10Times1 == 'Good',9,ifelse(gfgf2$Morethan_10Times1=='Average',5,0))
    gfgf2$Below_Thirty1 = ifelse(gfgf2$Below_Thirty1 == 'Good',9,ifelse(gfgf2$Below_Thirty1=='Average',5,0))
    
    
    
    gfgf2$Compliance_Score = paste0(round((rowSums(gfgf2[,5:7])/27)*100,1)," %")
    
    final1 = data.frame(gfgf1, gfgf2[,8])
    names(final1)[8] = c("Compliance_Score")
    final1
    
    final2 = datatable(tibble(
      Branch = final1$Branch,
      MB =final1$MB,
      Morethan_10Times = final1$Morethan_10Times,
      Beyond_Warranty = final1$Beyond_Warranty,
      MB1 =final1$MB1,
      Morethan_10Times1 = final1$Morethan_10Times1,
      Below_Thirty1 = final1$Below_Thirty1,
      Compliance_Score = final1$Compliance_Score
    ), colnames = c("Product Line", "% of MB Consumption", "% of Multiple Repair > 10 times", "% of repairs after warranty",
                    "% of MB Consumption", "% of Multiple Repair > 10 times", "% of repairs after warranty", "Compliance Score")) %>%
      formatStyle(c("MB1", "Morethan_10Times1", "Below_Thirty1"),
                  background = styleEqual(c("Bad","Average","Good"), c('red','yellow','green')))
    
    # names(final2) = c("Branch", "% of MB Consumption", "Multiple Repair > 10 times", "% of repairs after warranty",
    #                     "% of MB Consumption", "Multiple Repair > 10 times", "% of repairs after warranty", "Compliance Score")
    final2
    
    list(final1 = final1, final2 = final2)
    
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$Productline5 = renderDataTable({
        pl_mb4()$final2
      })
    }
  })  
  
  output$downloadData14 <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(pl_mb4()$final1, file)
    })
  
  

  
      }) # END SHINYAPP

#======
# RUN
#======
shinyApp(ui = ui, server = server)