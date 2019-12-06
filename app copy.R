# clear the list
# rm(list = ls())

# libraries required

library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(sp)
library(dplyr)
library(RPostgreSQL)
library(RSQLite)
library(rsconnect)
library(here)


Logged = FALSE
#Enter the values for you database connection
dsn_database = "dvdrental"            # e.g. "compose"
dsn_hostname = "localhost" # e.g.: "aws-us-east-1-portal.4.dblayer.com"
dsn_port = "5432"                 # e.g. 11101 
dsn_uid = "postgres"        # e.g. "admin"
dsn_pwd = "password"      # e.g. "xxx"

tryCatch({
  drv <- dbDriver("PostgreSQL")
  print("Connecting to database")
  conn <- dbConnect(drv, 
                    dbname = dsn_database,
                    host = dsn_hostname, 
                    port = dsn_port,
                    user = dsn_uid, 
                    password = dsn_pwd)
  print("Connected!")
},
error=function(cond) {
  print("Unable to connect to database.")
})

Loc_df <- dbGetQuery(conn, "SELECT * FROM Test_Loc_Dim")
Loc_df <- Loc_df %>% 
  mutate(concat_addr=paste(street_addr,  city, province,zip,phone_num, sep="\n"))

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
                
                navbarPage(title = tags$li(a(href = 'http://127.0.0.1:7120/',
                                             img(src = 'logo.png',
                                                 title = "Company", height = "200px",allign="left"),
                                             style = "padding-top:10px; padding-bottom:10px;"),
                                           class = "dropdown"), tags$li(img(src = 'lab_picture.png', allign = "right", height ="200px", width = "850px", style = "padding-left:250px; padding-bottom:10px;")),
                  
                  
                  id = "mainpage1",
                           
                           strong("Welcome"),
                           
                           ##### actionButton("Logout", "Logout"),
                           
                           
                           navlistPanel( id = "Navp", widths =c(2, 10),
                                         
                                         tabPanel(
                                           
                                           title = "Home", 
                                           id = "Home",
                                           
                                           verbatimTextOutput("HomeInfo"),
wellPanel(
                                           fixedRow(
                                             column(7,h2("Sign UP"),
                                                    
                                                    fixedRow(
                                                      column(6,
                                                             textInput("firstname",label="", 
                                                                       value = "First Name")
                                                      ),
                                                      column(6,
                                                             textInput("lastname", label="", 
                                                                       value = "Last Name")
                                                      )
                                                    )
                                             )
                                           ),
                                           fixedRow(
                                             column(7,
                                                    
                                                    fixedRow(
                                                      column(6,
                                                             textInput("Email",label="", 
                                                                       value = "Email ID")
                                                      ),
                                                      column(6,
                                                             textInput("MobileNo",label="", 
                                                                       value = "Mobile Number"))
                                                      )
                                                    )
                                             ),
                                           
                                           fixedRow(
                                             column(7,
                                                    
                                                    fixedRow(
                                                      column(6,"Birthday",
                                                             dateInput("DOB",label="", 
                                                                       value = Sys.Date()- as.difftime(936, unit="weeks"))) ,
                                                      column(6,"Gender",
                                                             selectInput("Gender",label="", 
                                                                         choices = list("","Male","Female")))
                                                    )
                                             )
                                           ),
                                           fixedRow(
                                             column(7,
                                           
                                                  textInput("NewPassword",label="", 
                                                            value = "New Password"))),
                                           actionButton("submit", "Submit"),
                                           actionButton("cancel", "Cancel")
                                           ),

                                           br(),br(),br(),
                                           strong("Welcome to your Lab Data, this website is developed using R Shiny to manage customer's Lab Data.
                                                  Customers can create their own user account, log in and order their tests and logout. Analysts can create new users, update test results, view reports and download 
                                                  reports. Depending on the user login and role relavent tabs will be displayed")
                                           
                                           ,br(), br(),br(), br(), br(),br()
                                           ,br(), br(),
tags$div(class = "footer", checked = NA,
         tags$p("Developed by Nidhin Anedath Dharman 
                as part of Capstone Project"))
                                           ,h5("Developed by Nidhin Anedath Dharman 
                                                as part of Capstone Project")
                                           
                                           ),
                                         tabPanel(
                                           title = "Login", 
                                           
                                           
                                           tagList(
                                             div(id = "login",
                                                 wellPanel(textInput("userName", "Username"),
                                                           passwordInput("passwd", "Password"),
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
                                           h1("Contact Us"),
                                           h5("Please select from the options below to contact the right team for assistance. "),
                                           br(),br(),
                                           column(3, offset = 1,
                                                  img(src = 'call_logo.png',
                                                       height = "30px",allign="left")
                                                
                                           ) ,column(2,offset=1, h5("314-610-6000"))
                                          
                                           
                                         )
                                         
)
)
)

top_dir <- here()
sqlitePath <- paste0(top_dir,'/',"labdata.db")

#Defining the fields from new user registration

NewUserRegistration <- c("First_Name","Last_Name","Email_ID","Contact_Nbr","Birthday","Password")
NewTestTypes <- c("TestName")
NewTestOrder <- c("CustId","gender", "RequestDate","TestName1","LabLocation1", "Test1Std")

                           
                           
server <- function(input, output) {
  output$mymap <- renderLeaflet({ 
    leaflet()  %>% addTiles() %>%
      addMarkers(data=Loc_df,lng = ~long_id,lat = ~lat_id,popup = ~ concat_addr) })
  
  
  Logged = FALSE
  
  ############### New user  ################
  
  # When the form data for new USERS 
  
  formData <- reactive({
    
    data <- sapply(NewUserRegistration, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the New User form data to USERS table
  table <- "User_DIM"
  
  observeEvent(input$submit, {
    
    saveData(formData())
    updateNavlistPanel(session, "Navp", selected = "Login")
  })
  
  ####### Save quesry for New user
  
  saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    
    # Construct the update query by looping over the data fields
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
  
}



shinyApp(ui = ui, server = server)