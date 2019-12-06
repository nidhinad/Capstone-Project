# clear the list
# rm(list = ls())

# libraries required

library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(sp)
library(dplyr)
library(RSQLite)
library(rsconnect)
library(here)
library(DT)
library(DBI)
library(ggplot2)
library(shinyjs)
#library(rJava)
library(gridExtra)

Logged = FALSE

#To get the main directory
top_dir <- here()
#To get the database location
sqlitePath <- paste0(top_dir,'/',"labdata.db")
#Db connection
db <- dbConnect(SQLite(), sqlitePath)
#Query to pick the Lab locations in USA 
query <- sprintf("select * from Test_Location_Dim")
Loc_df<- dbGetQuery(db, query)
Loc_df <- Loc_df %>%
  mutate(concat_addr=paste(Street_Addr,  City, Province,Zip,Phone_Num, sep="\n"))
dbDisconnect(db)


dbHeader <- dashboardHeader(title = "My Dashboard",
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://127.0.0.1:7120/',
                                      img(src = 'logo.png',
                                          title = "Company Home", height = "50px",allign="left"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

tags$li(a(href = 'http://127.0.0.1:7120/',
          img(src = 'logo.png',
              title = "Company Home", height = "100px",allign="left"),
          style = "padding-top:10px; padding-bottom:10px;"),
        class = "dropdown")


ui <- fluidPage(theme = shinytheme("simplex"),
                includeCSS("styles.css"),
                navbarPage(title = tags$li(a(href = 'http://127.0.0.1:7120/',
                                             img(src = 'logo.png',
                                                 title = "Company", height = "200px",allign="left"),
                                             style = "padding-top:10px; padding-bottom:10px;"),
                                           class = "dropdown"), tags$li(img(src = 'lab_picture.png', allign = "right", height ="200px", width = "850px", style = "padding-left:250px; padding-bottom:10px;")),
                           
                           
                           id = "mainpage1",
                           
                           h4(strong("Welcome to MU Diagnostics")),
                           
                           ##### actionButton("Logout", "Logout"),
                           #shinyjs::useShinyjs(),
                           
                           
                           navlistPanel( id = "Navp", widths =c(2, 10),
                                         
                                         tabPanel(
                                           title = "Home",
                                           id = "Home",
                                           verbatimTextOutput("HomeInfo"),
                                           wellPanel(
                                             fixedRow(
                                               column(7,h3("Sign UP"),
                                                      
                                                      fixedRow(
                                                        column(6,
                                                               textInput('UserId', h5('Full Name:'), width = '100%', placeholder = "Enter your name")
                                                        ),
                                                        column(6,
                                                               textInput ('Role', h5('Role:'), "customer", width = '100%')
                                                        )
                                                      )
                                               )
                                             ),
                                             fixedRow(
                                               column(7,
                                                      
                                                      fixedRow(
                                                        column(6,
                                                               textInput('Email_ID', h5('Email ID:'), width = '100%', placeholder = "Enter your Email ID")
                                                        ),
                                                        column(6,
                                                               textInput('Contact_Nbr', h5('Contact Number:'), width = '100%', placeholder = "Enter your Contact Number"))
                                                      )
                                               )
                                             ),
                                             
                                             fixedRow(
                                               column(7,
                                                      
                                                      fixedRow(
                                                        column(6,
                                                               dateInput('Birthday', h5('Birthday'),
                                                                         value = as.Date(Sys.Date()),
                                                                         format = "yyyy-mm-dd",
                                                                         startview = 'year'
                                                               )) ,
                                                        column(6,
                                                               selectInput("Gender",h5('Gender'),
                                                                           choices = list("","Male","Female"), width = '200px'))
                                                      )
                                               )
                                             ),
                                             fixedRow(
                                               column(7,
                                                      
                                                      textInput('Password', h5('Password:'), width = '100%', placeholder = "Enter your Password"))),
                                             actionButton("submit", "Submit"),
                                             actionButton("cancel", "Cancel")
                                           ),
                                           
                                           br(),br(),br(),
                                           strong("Welcome to Maryville University Diagnostics , this website is developed using R Shiny to manage Lab Data of all the customers in USA.
                                                 New Customers can signup  their own user account and order the test results easily, Existing customers can  log in and order their tests and see the trends of their past test results. Analysts can update test results of customer after their analysis, view how many open/completed orders are belonging to his name
                                   and inturn helps them to manage and track their work efficiently.Depending on the user types and their roles, only relavent tabs will be displayed")
                                           
                                           ,br(), br(),br(), br(), br(),br()
                                           ,br(), br(),
                                           tags$div(class = "footer", checked = NA,
                                                    tags$p("Developed by Nidhin Anedath Dharman
                                                           as part of Capstone Project"))
                                           
                                           ),
                                         tabPanel(
                                           title = "Login",
                                           
                                           
                                           tagList(
                                             div(id = "login",
                                                 wellPanel(textInput("userName", h5("Username")),
                                                           passwordInput("passwd", h5("Password")),
                                                           br(),actionButton("Login", "Log in"),
                                                           verbatimTextOutput("dataInfo")
                                                           
                                                           
                                                 )),
                                             tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
                                           )
                                           
                                           
                                         ),
                                         tabPanel(
                                           
                                           title = "Location",
                                           id = "Location",
                                           
                                           verbatimTextOutput("LocationInfo"),
                                           
                                           leafletOutput("mymap")
                                           
                                         ),
                                         tabPanel(
                                           
                                           title = "Contact Us",
                                           id = "Contact",
                                           
                                           verbatimTextOutput("ContactInfo"),
                                           h2("Contact Us"),
                                           br(),br(),
                                           
                                           div(id='contact', fluidRow(column(1,
                                                  h4("Ready for your questions"),
                                                  h4("Phone: 314-610-6000"),
                                                  h4("Email: nanedathdharman1@live.maryville.edu")
                                         )), align = 'left')),
                                         
                                         tabPanel(title = "Customer",
                                                  h1(strong("Order your Test")),
                                                  
                                                  tagList(
                                                    div(id = "Customer",
                                                        wellPanel(
                                                          
                                                          verbatimTextOutput("CustInfo"),
                                                          
                                                          htmlOutput("Cust_Name"),
                                                          selectInput("Gender", h5("Gender:"),c("Male", "Female")),
                                                          dateInput("Request_Date", h5("Request Date")),
                                                          selectInput("Test_Category", h5("Test Category:"),c("Fasting", "Non Fasting")),
                                                          selectInput("Test_Type", h5("Test Types:"),c("Lipid Panel", "Comprehensive Metabolic Panel")),
                                                          selectInput("Test_Name", h5("Test Names:"),c("HDL", "LDL","Triglycerides","Glucose")),
                                                          selectInput("LabLoc1", h5("Lab Location:"), choices = c("MIAMI-MU LABS","MH- LABS","WALTHAM-MU LABS","RALEIGH-MU LABS","BRYAN-MU LABS","CHICO-MU LABS","PHOENIX-MU LABS","WHEATRIDGE-MU LABS","BEAVERTON-MU LABS","STPAUL-MU LABS"
                                                          ),selected ='MH- LABS'),
                                                          #htmlOutput("TestCategory"),
                                                          #htmlOutput("TestTypes1"),
                                                          #htmlOutput("TestName"),
                                                          #htmlOutput("LabLocation1"),
                                                          br(),actionButton("order", "Order"))),
                                                    tags$style(type="text/css", "login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;width: 200px}")
                                                  )
                                         ),
                                         ############ Analyst page page UI
                                         
                                         tabPanel(title = "Analyst",
                                                  h1(strong("Analyst Page")),
                                                  
                                                  navbarPage("", id = "analystpage",
                                                             
                                                             #verbatimTextOutput("AnaInfo"),
                                                             
                                                             
                                                             frow1 <- fluidRow(
                                                               
                                                               actionButton("displayResults", label = "Display Records")
                                                               ,actionButton("action", label = "Update Records")
                                                               ,br(),br()
                                                               , width = "1100px"
                                                               ,status = "primary"
                                                               ,solidHeader = TRUE
                                                               ,collapsible = TRUE
                                                               ,label = "View Results"   ### )
                                                               ,DTOutput("Results", height = "300px", width = "1100px")
                                                               ###           ,actionButton("action", label = "Write to DB")
                                                             )
                                                  )
                                                  
                                         ),    #Analyst tab end
                                         
                                         ###################################### Analyst Dashboard Page UI Starts here ######################################
                                         
                                         
                                         tabPanel(title = "Analyst Dashboard",titleWidth = 200,
                                                  
                                                  dashboardPage(
                                                    
                                                    dashboardHeader(title = "Analyst Dashboard"),
                                                    dashboardSidebar(
                                                      
                                                      tags$style(type='text/css', "label {font-size: 10px; }
                                                                 
                                                                 .nav-tabs {font-family:'arial';font-size:10px}
                                                                  sidebar {background-color: #5C97BF;}
                                                                 mainbar {background-color: #5C97BF;}
                                                                 body { background-color: #002B55;}
                                                                 input[type=checkbox] {transform: scale(2);margin-top:10px;}
                                                                 # .multicol {height: 200px; -webkit-column-count: 4;
                                                                 #-moz-column-count: 4;    /* Firefox */
                                                                 # column-count: 4; -moz-column-fill: auto;-column-fill: auto;}
                                                                 .checkbox {margin-top:-5px;}
                                                                 
                                                                 #modules .checkbox label span {font-weight:bold;}
                                                                 label {color:#fff;}
                                                                 "),
                                                      
                                                      selectInput("LabLoc", h4("Lab Location:"), choices = c("MIAMI-MU LABS","MH- LABS","WALTHAM-MU LABS","RALEIGH-MU LABS","BRYAN-MU LABS","CHICO-MU LABS","PHOENIX-MU LABS","WHEATRIDGE-MU LABS","BEAVERTON-MU LABS","STPAUL-MU LABS"
                                                      ),selected ='MH- LABS'),
                                                      tags$style(".list {width: 400px;}"),
                                                      fluidRow(
                                                        column(1,
                                                               checkboxGroupInput("Tcat", label = h4("Test Category"),
                                                                                  choices = list("Fasting" , "Non Fasting"),selected = c("Fasting" , "Non Fasting"))
                                                               
                                                               
                                                        )),
                                                      fluidRow(
                                                        column(3, verbatimTextOutput("Tcat"))
                                                      ),
                                                      
                                                      
                                                      checkboxGroupInput("Ttype", label = h4("Test Type"),
                                                                         choices = list("Lipid Panel" , "Comprehensive Metabolic Panel"),selected = c("Lipid Panel" , "Comprehensive Metabolic Panel")),
                                                      
                                                      fluidRow(column(2, verbatimTextOutput("Ttype"))),
                                                      
                                                      fluidRow(
                                                        column(6,style=list("padding-right: 20px;"),tags$style(".shiny-input-container  {width: 200px;}")
                                                               ,tags$style(".form-control  {width: 200px;}"),
                                                               dateInput("analystStartDate",h4("From Date"),value=format(Sys.time(),"%Y-%m-%d"))
                                                        )),
                                                      fluidRow(
                                                        column(4, verbatimTextOutput("analystStartDate"))
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(6,style=list("padding-right: 20px;"),tags$style(".shiny-input-container  {width: 200px;}")
                                                               ,tags$style(".form-control  {width: 200px;}"),
                                                               dateInput("analystEndDate",h4("To Date"),value=format(Sys.time(),"%Y-%m-%d"))
                                                        )),
                                                      fluidRow(
                                                        column(4, verbatimTextOutput("analystEndDate"))
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(3,
                                                               checkboxGroupInput("Sex", label = h4("Gender"),
                                                                                  choices = list("Male" , "Female"),selected = c("Male","Female"))
                                                               
                                                               
                                                        )),
                                                      fluidRow(
                                                        column(3, verbatimTextOutput("Sex"))
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(4,
                                                               checkboxGroupInput("Status", label = h4("Status"),
                                                                                  choices = list("Completed" , "Open"),selected = c("Completed" , "Open"))
                                                               
                                                               
                                                        )),
                                                      fluidRow(
                                                        column(4, verbatimTextOutput("Status"))
                                                      ),
                                                      fluidRow(
                                                        br(),br(),br(),
                                                        
                                                        downloadButton('AnalystdownloadReport')
                                                        
                                                      )
                                                      
                                                      
                                                      ), ####dashboardSidebar end
                                                    
                                                    dashboardBody(
                                                      
        
                                                      
                                                      fluidRow(
                                                        box(
                                                          title = "Result Status by Gender"
                                                          ,status = "primary"
                                                          ,solidHeader = TRUE
                                                          ,collapsible = TRUE
                                                          #,DTOutput("graph1", height = "300px", width = "1100px")
                                                          ,plotOutput("MaxTestResultsbyType", height = "300px")
                                                        )
                                                        
                                                        ,box(
                                                          title = "Result Status by Test Name"
                                                          ,status = "primary"
                                                          ,solidHeader = TRUE
                                                          ,collapsible = TRUE
                                                          ,plotOutput("TestNamevsStatus", height = "300px")
                                                        )
                                                      )
                                                    )   # Analyst Dashboard body end here
                                                    
                                                    )  # Analyst dashboardPage end here
                                           ) ,
                                         ###################################### Analyst Dashboard Page UI TAB Panel End here ######################################
                                         
                                         ###################################### customer Dashboard Page UI Starts here ######################################
                                         
                                         
                                         tabPanel(title = "Customer Dashboard",titleWidth = 200,
                                                  
                                                  dashboardPage(
                                                    dashboardHeader(title = "Customer Dashboard"),
                                                    dashboardSidebar(
                                                      selectInput("CustLabLoc", h4("Lab Location:"), choices = c("MIAMI-MU LABS","MH- LABS","WALTHAM-MU LABS","RALEIGH-MU LABS","BRYAN-MU LABS","CHICO-MU LABS","PHOENIX-MU LABS","WHEATRIDGE-MU LABS","BEAVERTON-MU LABS","STPAUL-MU LABS"
                                                      ),selected ='MH- LABS'),
                                                      tags$style(".list {width: 400px;}"),
                                                      fluidRow(
                                                        column(1, verbatimTextOutput("CustLabLoc"))
                                                      ),
                                                      fluidRow(
                                                        column(1,
                                                               checkboxGroupInput("CustTcat", label = h4("Test Category"),
                                                                                  choices = list("Fasting" , "Non Fasting"),selected = c("Fasting" , "Non Fasting"))
                                                               
                                                               
                                                        )),
                                                      fluidRow(
                                                        column(3, verbatimTextOutput("CustTcat"))
                                                      ),
                                                      checkboxGroupInput("CustTtype", label = h4("Test Type"),
                                                                         choices = list("Lipid Panel" , "Comprehensive Metabolic Panel"),selected = c("Lipid Panel" , "Comprehensive Metabolic Panel")),
                                                      
                                                      fluidRow(column(2, verbatimTextOutput("CustTtype"))),
                                                      checkboxGroupInput("CustTname", label = h4("Test Name"),
                                                                         choices = list("HDL","LDL","TRIGLYCERIDES", "GLUCOSE")),
                                                      
                                                      fluidRow(column(2, verbatimTextOutput("CustTname"))),
                                                      
                                                      
                                                      
                                                      
                                                      fluidRow(
                                                        column(6,style=list("padding-right: 20px;"),tags$style(".shiny-input-container  {width: 200px;}")
                                                               ,tags$style(".form-control  {width: 200px;}"),
                                                               dateInput("startDate",h4("From Date"),value=format(Sys.time(),"%Y-%m-%d"))
                                                        )),
                                                      fluidRow(
                                                        column(4, verbatimTextOutput("startDate"))
                                                      ),
                                                      
                                                      fluidRow(
                                                        column(6,style=list("padding-right: 20px;"),tags$style(".shiny-input-container  {width: 200px;}")
                                                               ,tags$style(".form-control  {width: 200px;}"),
                                                               dateInput("endDate",h4("To Date"),value=format(Sys.time(),"%Y-%m-%d"))
                                                        )),
                                                      fluidRow(
                                                        column(4, verbatimTextOutput("endDate"))
                                                      ),
                                                      fluidRow(
                                                        br(),br(),br(),
                                                        
                                                        downloadButton('downloadReport')
                                                        
                                                      )
                                                      
                                                    ) , ####dashboardSidebar end
                                                    
                                                    dashboardBody(
                                                      fluidRow(
                                                        box(
                                                          title = "Result by Different Tests"
                                                          ,status = "primary"
                                                          ,solidHeader = TRUE
                                                          ,collapsible = TRUE
                                                          #,DTOutput("graph1", height = "300px", width = "1100px")
                                                          ,plotOutput("CustResults", height = "300px")
                                                        )
                                                      )
                                                      
                                                    )   # customer Dashboard body end here
                                                    
                                                  )  # customer dashboardPage end here
                                         )
                                         ###################################### Customer Dashboard Page UI TAB Panel End here ######################################
                                         
                                         
                                         
                                         ##############   Logout button in main page  ###############
                                         
                                         , tabPanel(actionButton("Logout", "Logout") )
                                         )
                                         ),
                uiOutput("page")
)

top_dir <- here()
sqlitePath <- paste0(top_dir,'/',"labdata.db")

#Fields required for new user registration

NewUserRegistration <- c("UserId","Role","Email_ID","Contact_Nbr","Birthday","Gender","Password")
NewTestTypes <- c("TestName")
NewTestOrder <- c("Cust_Name","Gender", "Request_Date","Test_Category","Test_Type", "Test_Name")



############################################################################
############################################################################
################################SERVER LOGIC################################
############################################################################
############################################################################

server <- function(input, output,session) {
  
  output$mymap <- renderLeaflet({
    leaflet()  %>% addTiles() %>%
      addMarkers(data=Loc_df,lng = ~Long_id,lat = ~Lat_id,popup = ~ concat_addr) })
  
  #Hiding the Analyst and Customer pages before customer login
  
  hideTab(inputId = "Navp", target = "Analyst Dashboard")
  hideTab(inputId = "Navp", target = "Analyst")
  hideTab(inputId = "Navp", target = "Customer")
  hideTab(inputId = "Navp", target = "Customer Dashboard")
  
  #shinyjs::hide("Logout")
  Logged = FALSE
  
  ############### Logic for New user signup  ################
  
  formData <- reactive({
    
    data <- sapply(NewUserRegistration, function(x) input[[x]])
    data
  })
  
  # Observe event to track the submit button click by customer to initiate the authentications
  table <- "Users"
  
  observeEvent(input$submit, {
    
    saveData(formData())
    updateNavlistPanel(session, "Navp", selected = "Login")
  })
  
  ####### Save query for New user
  
  saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table,
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
  ############################# Log in validation ################
  
  USER <- reactiveValues(Logged = Logged)
  
  inputdata <- reactive({
    
    validate(need(isolate(input$userName) == "", "Please Enter your User Name"))
    
  })
  
  
  
  ############# Logic of User Login #####################
  
  observeEvent(input$Login, {
    
    output$dataInfo <- renderText({""})
    
    ### Check if user already logged in
    
    if (USER$Logged) {
      
      output$dataInfo <- renderText(stop({"You have already logged in!!!!!!"}))
      
      return()
      
    }
    
    #  Check if User Name & Password entered or not
    
    if(input$userName == "" & input$passwd == "") {
      
      output$dataInfo <- renderText({"Please check your credentials"})
      
      return()
    }
    
    if(input$userName == "" ) {
      
      output$dataInfo <- renderText({"Please check your User"})
      return()
    }
    
    if(input$passwd == "") {
      
      output$dataInfo <- renderText({"Please check your password"})
      return()
    }
    
    
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          db <- dbConnect(SQLite(), sqlitePath)
          
          query <- sprintf({"
            SELECT UserId, Role
            FROM USERS
            WHERE UserId ='%s' and Password ='%s'"},
                           Username, Password, serialize=F)
          
          query_delete <- sprintf({"Delete FROM User_Trans"})
          dbExecute(db,query_delete)
          
          query_trans <- sprintf({"
            Insert into User_Trans VALUES('%s','%s','%s')"},
                                 Username, Password,Sys.time(), serialize=F)
          
          #db <- dbConnect(SQLite(), sqlitePath)
          
          userrec <- dbGetQuery(db, query)
          
          dbExecute(db,query_trans)
          userid_fetch <<- userrec$UserId
          #  print(userrec)
          
          dbDisconnect(db)
          
          
          if (length(userrec$UserId) == 0 ) {
            
            # print error/ warning message
            
            
            output$dataInfo <- renderText({"If you are a new user please register before login OR Check your credentials"})
            return()
            
          } else {
            
            if ( userrec$UserId == Username ) {
              
              USER$Logged <- TRUE
              
            }
            
            
            successInfo <- cbind ("You Have Successfully logged in as", Username)
            
            output$HomeInfo <- renderText({successInfo})
            output$CustInfo <- renderText({successInfo})
            ###     output$AnaInfo <- renderText({successInfo})
            
            #output$dataInfo <- renderText({""})   ##### Clear previous message
            
          }
          }
        }
      }
    
    
    if (USER$Logged == TRUE)
    {
      ######################### LOAD User Name in Customer tab ###############
      ###################################################
      
      output$Cust_Name <- renderUI({
        selectInput("Cust_Name", h5("Customer ID:"), userrec$UserId) })
      
      
      ########### Hide some Tabs when Login #######################
      
      updateTextInput(session, "userName", value = '')
      updateTextInput(session, "passwd", value = '')
      
      if ( userrec$Role == "analyst" ) {
        
        showTab(inputId = "Navp", target = "Analyst Dashboard")
        showTab(inputId = "Navp", target = "Analyst")
        showTab(inputId = "Navp", target = "New User")
        
        hideTab(inputId = "Navp", target = "Login")
        #   hideTab(inputId = "Navp", target = "NewUser")
        hideTab(inputId = "Navp", target = "Customer")
        
        updateNavlistPanel(session, "Navp", selected = "Analyst")
        
        
      }
      if ( userrec$Role == "customer" ) {
        
        showTab(inputId = "Navp", target = "Customer")
        
        showTab(inputId = "Navp", target = "Customer Dashboard")
        hideTab(inputId = "Navp", target = "Analyst")
        hideTab(inputId = "Navp", target = "Login")
        hideTab(inputId = "Navp", target = "New User")
        
        updateNavlistPanel(session, "Navp", selected = "Customer")}
      
    }
    })
  
  ################ Data Table start###############################
  ###########################################################
  ###########################################################
  
  observeEvent(input$displayResults, {
    db <- dbConnect(SQLite(), sqlitePath)
    datatb <- tbl(db, "User_Submit_Data")
    datatb <- datatb %>% as.data.frame()
    query_fetch1 <- sprintf({"SELECT UserId FROM User_Trans"})
    userid_fetch1<- dbGetQuery(db,query_fetch1)
    dbDisconnect(db)
    userid_fetch1<-userid_fetch1 %>% as.data.frame()
    
    datatb<-datatb %>% inner_join(userid_fetch1, by = c("Analyst_Asgnd" = "UserID"))
    
    
    TestResults <- datatb
    output$Results <- renderDT(TestResults, options =
                                 list(scrollX = TRUE), editable = TRUE)
    #necessary code to replace data once edited
    proxy1 = dataTableProxy('Results')
    
    #print(proxy1)
    
    ####TestResults_rows <- which(datatb$ TestName1 != "")
    TestResults_rows <- which(TestResults$Test_Results != "" | is.na(TestResults$Test_Results) )
    
    # print(TestResuts_rows)
    
    
    observeEvent(input$Results_cell_edit, {
      
      info = input$Results_cell_edit
      str(info)
      
      
      i = info$row
      j = info$col
      v = info$value
      
      ############ get new value
      new_value <- DT::coerceValue(v, TestResults[i, j])
      
      ############# update local copy of TestResuts
      TestResults[i, j] <<- new_value
      
      ############# update local copy of data
      
      datatb[TestResults_rows[i], j] <<- new_value
      
      ############# update browser
      replaceData(proxy1, TestResults, resetPaging = TRUE)  # important
      
       
      
    })
    
    
    observeEvent(input$action, {
      db <- dbConnect(SQLite(), sqlitePath)
      dbWriteTable(db, "User_Submit_Data", data.frame(datatb), overwrite = TRUE)
      
    })
    
     dbDisconnect(db)
    
  })   ########### end of display results
  
  ################ end of Data Table ###############################
  ###########################################################
  ###########################################################
  
  ################### Logout logic#####################
  
  observeEvent(input$Logout, {
    print("inside observe event")
    
    USER$Logged <- FALSE 
    
    hideTab(inputId = "Navp", target = "Customer")
    hideTab(inputId = "Navp", target = "Analyst")
    showTab(inputId = "Navp", target = "Login")
    hideTab(inputId = "Navp", target = "Analyst Dashboard")
    hideTab(inputId = "Navp", target = "Customer Dashboard")
    showTab(inputId = "Navp", target = "Home")
    
    updateTextInput(session, "userName", value = '')
    updateTextInput(session, "passwd", value = '')
    
    output$dataInfo <- renderText({""})
    output$HomeInfo <- renderText({"You Have successfully Logged out"})
    output$CustInfo <- renderText({""})
    
    output$Cust_Name <- renderUI({
      selectInput("Cust_Name", "Customer ID", "") })
    
    updateNavlistPanel(session, "Navp", selected = "Login")
    
    dbDisconnect(db)
  })
  ############   Customer & Analyst DashBoard Dataframes and logic ##############################
  #dbDisconnect(db)
  db <- dbConnect(SQLite(), sqlitePath)
  query_fetch <- sprintf({"SELECT UserId FROM User_Trans"})
  userid_fetch<- dbGetQuery(db,query_fetch)
  
  userid_fetch<-userid_fetch %>% as.data.frame()
  testresultstabel <- tbl(db, "User_Submit_Data")
  
  testresultstabel <- testresultstabel %>% as.data.frame()
  custresultslabel <- testresultstabel
  testresultstabel<- testresultstabel %>%
    select(Cust_Name,Loc_Name,Request_Date,Test_Name,Gender,Test_Type,Test_Category,Status,Analyst_Asgnd) %>% distinct()
  
  
  testresultstabel<-testresultstabel %>% inner_join(userid_fetch, by = c("Analyst_Asgnd" = "UserID"))
  
  
  custresultslabel <- custresultslabel  %>%
    select(Cust_Name,Loc_Name,Request_Date,Test_Name,Gender,Test_Type,Test_Category,Status,Test_Results,Analyst_Asgnd) %>% distinct() %>%
    filter( Status=="Completed")
  custresultslabel<-custresultslabel %>% inner_join(userid_fetch, by = c("Cust_Name" = "UserID"))
  print(custresultslabel)
  
  output$graph1 <- renderDT(custresultslabel, options =
                              list(scrollX = TRUE), editable = TRUE)
  
  ################ initialize the variables for reports in dashboard body ##################
  
  vals <- reactiveValues(MaxTestResultsbyType=NULL, TestResultsPerCustomer = NULL)
  cust_val<-reactiveValues(CustResults=NULL)
  
  
  #######################customer graphs##################
  Dashboarddata3 <- reactive({
    Dashboarddata3 <- custresultslabel %>%
      filter(Loc_Name %in% input$CustLabLoc) %>%
      filter(Test_Type %in% input$CustTtype) %>%
      filter(Test_Category %in% input$CustTcat) %>%
      filter(Request_Date >= input$startDate & Request_Date <= input$endDate) %>%
      filter(Test_Name %in% input$CustTname) 
   
    if(is.null(input$CustTtype))
      return()
    Dashboarddata3
    
  })
  
  ################### Customer dashboard graph ########################
  #creating the plotOutput content
  output$CustResults <- renderPlot({
    
    cust_val$CustResults <-    ggplot(data=Dashboarddata3(), aes(fill=Test_Name, y=Test_Results, x=Request_Date)) +
      geom_bar(position="dodge", stat="identity")+ ylab("Result") +
      xlab("Request Date") + theme(legend.position="right"
                                   ,plot.title = element_text(size=15, face="bold"))
    
    cust_val$CustResults
    
    
  })
  
  
  ################ Analyst Dashboard Filtering starts ################################
  
  Dashboarddata2 <- reactive({
    Dashboarddata2 <- testresultstabel %>%
      filter(Loc_Name %in% input$LabLoc) %>%
      filter(Test_Type %in% input$Ttype) %>%
      filter(Request_Date >= input$analystStartDate & Request_Date <= input$analystEndDate) %>%
      filter(Test_Category %in% input$Tcat) %>%
      filter(Gender %in% input$Sex) %>%
      filter(Status %in% input$Status)
    if(is.null(input$Ttype))
      return()
    Dashboarddata2
    
  })
  
  
  
  
  ################### Analyst Dashboard Filtering End #######################
  
  ################### Analyst Dashboard status reports########################
  
  
  #creating the plotOutput content
  output$MaxTestResultsbyType <- renderPlot({
    
    vals$MaxTestResultsbyType <-    ggplot(data =Dashboarddata2(),
                                           aes(Gender)) + geom_bar(aes(fill=Status) )+
      #geom_bar(position = "dodge", stat = "identity") + ylab("Count") +
      xlab("Gender") + theme(legend.position="bottom"
                             ,plot.title = element_text(size=15, face="bold"))
    
    vals$MaxTestResultsbyType
    
  })
  
  ################### Analyst Dashboard Testname vs status reports########################
  #creating the plotOutput TestName vs status
  output$TestNamevsStatus <- renderPlot({
    
    vals$TestNamevsStatus <-      ggplot(data =Dashboarddata2(), aes(Test_Name)) + geom_bar(aes(fill=Status), width = 0.5) +
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
      xlab("Test Name") + ylab("Count")
    
    vals$TestNamevsStatus
    
    
  })
  
  ################### Analyst Dashboard - Test Results by Customer reports########################
  output$TestResultsPerCustomer <- renderPlot({
    
    vals$TestResultsPerCustomer <- ggplot(Dashboarddata4(),
                                          aes(x = Cust_Name, y = TestResults,fill=factor(Test_Name))) +
      ###  ggplot(Dashboarddata4(),
      ###        aes(x = Cust_Name, y = Test1Results,fill=factor(TestName1))) +
      
      geom_point(size = 5, stat = "identity") + ylab("Test Results") +
      xlab("Customer") + theme(legend.position="bottom"
                               ,plot.title = element_text(size=15, face="bold")) +
      ggtitle("Test Results by Customer") + labs(fill = "Test Name")
    
    vals$TestResultsPerCustomer
    #dbDisconnect(db)
  })
 
  
  ############   End of Customer Dashboard - Test Results by Customer reports### ##############################
  
  ##################### Load data function #####################
  
  loadData <- function(fields, table, sortCol= '' , whereCls = ''){
    if (whereCls == "")
      query <- sprintf("SELECT %s FROM %s", fields, table)
    else
      query <- sprintf("SELECT %s FROM %s WHERE %s", fields, table, whereCls)
    db <- dbConnect(SQLite(), sqlitePath)
    dataDB <- dbGetQuery(db, query)
    
    if(sortCol != "") dataDB[order(dataDB[sortCol]),]
    else dataDB
    dbDisconnect(db)
    
    print(dataDB)
  }
  
  ################# load Test Categories in Customer tab
  
  Listdata0 <- loadData("TestCategory", "TestCategory","TestCategory","")
  TestCategorylist <- setNames(Listdata0$TestCategory, Listdata0$TestCategory)
  output$TestCategory <- renderUI({
    selectInput("TestCategory", "Test Category: ", TestCategorylist)
  })
  
  ################# load Test Types in Customer tab
  
  Listdata <- loadData("TestTypes", "TestTypes","TestTypes","")
  TestTypeslist <- setNames(Listdata$TestTypes, Listdata$TestTypes)
  output$TestTypes <- renderUI({
    selectInput("TestTypes", "Test Types: ", TestTypeslist)
  })
  
  ####### Lab Locations load data from database
  
  Listdata1 <- loadData("Loc_name", "Test_Location_Dim","Loc_name","")
  # print(Listdata1)
  
  LabLoclist <- setNames(Listdata1$Loc_name, Listdata1$Loc_name)
  
  
  output$LabLocation1 <- renderUI({
    selectInput("LabLocation1", "Lab: ", LabLoclist)
  })
  
  ################# load Test Names in Customer tab
  
  Listdata2 <- loadData("TestName", "TestName","TestName","")
  TestNamelist <- setNames(Listdata2$TestName, Listdata2$TestName)
  output$TestName <- renderUI({
    selectInput("TestName", "Test Name: ", TestNamelist)
  })
  
  
  ####### Save query for New tests
  
  saveTestData <- function(newtestdata) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table1,
      paste(names(newtestdata), collapse = ", "),
      paste(newtestdata, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }
  
  
  ################ New Test Orders    ######################
  
  
  
  # When the form data for test order
  
  TestOrderFormData <- reactive({
    
    orderdata <- sapply(NewTestOrder, function(x) input[[x]])
    
    orderdata[3] <-  as.Date(as.numeric(orderdata[3]), origin = "1970-01-01")
     # format(as.Date(orderdata[3],origin="1970-01-01"),"%Y-%m-%d")

    
    orderdata
  })
  
  
  # When the order button is clicked, save the test order form data to TestResults table
  
  ordertable <- "User_Submit_Data"
  
  observeEvent(input$order, {
    
    saveOrderData(TestOrderFormData())
    # updateNavlistPanel(session, "Navp", selected = "Customer")
  })
  
  ####### Save query for test order
  
  saveOrderData <- function(orderdata) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
    query11 <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      ordertable,
      paste(names(orderdata), collapse = ", "),
      paste(orderdata, collapse = "', '")
    )
    ############# Submit the update query and disconnect
    dbGetQuery(db, query11)
    dbDisconnect(db)
    
    ############# Successfully order test message #######################
    ####     updateTextInput(session, "Cust_Name", value = '')
    output$CustInfo <- renderText({"You have successfully placed your tests. Thank you!!!!"})
    return()
    
    ################## Cancel or clear ########################
    
    observeEvent(input$cancel, {
      
      updateTextInput(session, "Name", value = '')
      updateTextInput(session, "UserId", value = '')
      updateTextInput(session, "Password", value = '')
    })
  }
  
  ############## END Save Test Orders#######################
  
  ##############################################################
  ###############  Download Dashboard ##########################
  ##############################################################
  
  
  output$downloadReport <- downloadHandler(
    
    
    filename = function() {
      paste("downloadReport.pdf",sep="")},
    
    content = function(file) {
      pdf(file)
      grid.arrange(cust_val$CustResults) 
      
      dev.off()
    }
  )
  
  
  output$AnalystdownloadReport <- downloadHandler(
    filename = function() {
      paste("AnalystdownloadReport.pdf",sep="")},
    
    content = function(file) {
      pdf(file)
      grid.arrange(vals$MaxTestResultsbyType) 

      dev.off()
    }
  )
  
  
  ############   End of download file ##############################
  
  ############### Home page Logos #####################
  
  top_dir <- here()
  Logofilename <- paste0(top_dir,'/',"logo.png")
  output$logo <- renderImage({
    # Return a list containing the filename
    list(src = Logofilename)
  },deleteFile = FALSE
  )
  
  Labfilename <- paste0(top_dir,'/',"lab_picture.png")
  output$lablogo <- renderImage({
    # Return a list containing the filename
    list(src = Labfilename)
  },deleteFile = FALSE
  )
  
  rsconnect::setAccountInfo(name='nidhinanedathdharman', token='1C9E71C2901C9D6F999D4D56B42B8D5C', secret='QXuKuTF2DOSe8+XXHCQ6MnTLh2ph13VrGZ1SM/sp')
}
shinyApp(ui = ui, server = server)
