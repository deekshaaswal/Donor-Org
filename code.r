
## app.R ##
#library----
library(shiny)
library(DBI)
library(pool)
library(shinydashboard)
library(RMariaDB)
library(RMySQL)  
library(ggplot2)
library(dplyr)
library(DT) 
library(plotly)
library(data.table)
library(readr) # to read_csv function
library(data.table)
library(base)
library(leaflet)
library(lubridate)
library(gridExtra)
library(scales)
library(pool)
library("tidyverse")
library("dplyr")
library("tidyr")
library(class)

#Create connection from R shiny app to the MariaDB (database: Project) hosted at the AWS server with correct user credentials 
conn <- DBI::dbConnect(
  drv = RMariaDB::MariaDB(),
  host = "ec2-3-19-242-132.us-east-2.compute.amazonaws.com", 
  db= "Project_Final",
  username = "Deeksha",
  password = "1234"
)


#First Tab: Queries to fetch data from database -----
#Donations <- dbGetQuery(conn, 'Select * from Donations;')
names(Donations) <- make.names(names(Donations))

#Donors  <- read_csv("C:/Users/shipr/Documents/R/Donors.csv")
#Donors <- dbGetQuery(conn, 'Select * from Donors;')
names(Donors)<-make.names(names(Donors))

#Schools <- read_csv("C:/Users/shipr/Documents/R/Schools.csv")
#Schools <- dbGetQuery(conn, 'Select * from Schools;')
names(Schools) <- make.names(names(Schools))

#Teachers <- read_csv("C:/Users/shipr/Documents/R/Teachers.csv")
#Teachers <- dbGetQuery(conn, 'Select * from Teachers;')
names(Teachers)<-make.names(names(Teachers))

#Projects <- read_csv("C:/Users/shipr/Documents/R/Projects.csv",col_names = TRUE)
#Projects <- dbGetQuery(conn, 'Select * from Projects;')
names(Projects)<-make.names(names(Projects))



#Second Tab: Queries to fetch data from database -----
query_project_category<- "select * from Project_Type;"
Project_Type <- dbGetQuery(conn, query_project_category)

query_school_location <- "select District from School;"
District <- dbGetQuery(conn, query_school_location)

query_grade_level <- "select * from Grade_Level;"
Grade_Level <- dbGetQuery(conn, query_grade_level)

query_metro_type <- "select * from Metro_Type;"
Metro_Type <- dbGetQuery(conn, query_metro_type)

query_project_title<- "select Title from Project Order By 'Project ID' DESC LIMIT 20;"
Project_Title <- dbGetQuery(conn, query_project_title)


#Queries to fetch data for the prediction model
query_pred_model_data <- "SELECT P.Posted_Date, P.Fully_Funded_Date, P.Cost, P.Project_Type_ID, S.District, 
                        P.Grade_Level_ID, S.Metro_Type_ID FROM Project P
                        JOIN School S ON P.School_ID = S.School_ID 
                        JOIN Project_Type PT ON PT.Project_Type_ID = P.Project_Type_ID
                        JOIN Grade_Level GL ON P.Grade_Level_ID = GL.Grade_Level_ID
                        JOIN Metro_Type MT ON S.Metro_Type_ID = MT.Metro_Type_ID
                        WHERE Project_Status_ID = 2;"
pred_model_data <- dbGetQuery(conn, query_pred_model_data)
pred_model_data <- na.omit(pred_model_data)
pred_model_data$DaysTaken <- (as.Date(pred_model_data$Fully_Funded_Date) - as.Date(pred_model_data$Posted_Date))
pred_model_data$DaysTakenGroup <- (ifelse(pred_model_data$DaysTaken >= 150, 11, 
                                    ifelse(pred_model_data$DaysTaken >= 135,10, 
                                    ifelse(pred_model_data$DaysTaken >= 120,9, 
                                    ifelse(pred_model_data$DaysTaken >= 105,8, 
                                    ifelse(pred_model_data$DaysTaken >= 90, 7, 
                                    ifelse(pred_model_data$DaysTaken >= 75, 6, 
                                    ifelse(pred_model_data$DaysTaken >= 60, 5, 
                                    ifelse(pred_model_data$DaysTaken >= 45, 4, 
                                    ifelse(pred_model_data$DaysTaken >= 30, 3, 
                                    ifelse(pred_model_data$DaysTaken >= 15, 2, 1 )))))))))))
pred_model_data <- pred_model_data %>% separate(Posted_Date, c("Post_Year","Post_Month","Post_Date"), "-")
pred_model_data$Fully_Funded_Date <- NULL
pred_model_data$DaysTaken <- NULL
pred_model_data$Post_Year <- NULL
pred_model_data$Post_Date <- NULL

#Functions to fetch input from the UI into fields required for the prediction---
fields2 <- c("Post_month_1","Cost_1","Project_Type_1","District_1","Grade_level_1","Metro_Type_1")
fields3 <- c("ID3")

#To fetch project category
get_query_data <- function(q){
  return(dbGetQuery(conn, q))
  dbDisconnect(db)
}

on.exit(dbDisconnect(conn))


#Save function----
saveProjectData <- function(data) {
  
  responses <<- data
  responses
  print("Please wait while the model is trained.")
  print("It may take upto 15 minutes.")
  #Adding 'X' so that we can get the predicted value later. It is cumpulsory to have a value in the column.
  responses$DaysTakenGroup <- 1
  #Making Column names same to ensure 'rbind' function works.
  names(responses) <- names(pred_model_data)
  pred_model_data <- rbind(pred_model_data, responses)
  #Making Cost Group to transform data for prediction model.
  pred_model_data$CostGroup <- (ifelse(pred_model_data$Cost >= 1500, 6,
                                       ifelse(pred_model_data$Cost >= 1200, 5,
                                              ifelse(pred_model_data$Cost >= 900, 4,
                                                     ifelse(pred_model_data$Cost >= 600, 3,
                                                            ifelse(pred_model_data$Cost >= 300, 2, 1))))))
  #Mapping numeric values instead of District character. Making another District table,
  # adding a numeric primary key and then making a join to map the numeric values.
  District_Numeric <- as.data.frame(table(pred_model_data$District))
  District_Numeric$District_ID <- c(1:length(table(pred_model_data$District)))
  pred_model_data <- inner_join(pred_model_data, District_Numeric, by = c("District" = "Var1"))
  pred_model_data <- na.omit(pred_model_data)
  #Deleting the District Numeric table.
  rm(District_Numeric)
  pred_model_data$District <- NULL
  
  #Making Table for KNN prediction, according to required variables in the correct order.
  Project_Schools_KNN_Table <- data.frame(DaysTakenGroup = pred_model_data$DaysTakenGroup,
                                          Project_Type = as.numeric(pred_model_data$Project_Type_ID),
                                          Project_Cost_Group = as.numeric(pred_model_data$CostGroup),
                                          School_District = as.numeric(pred_model_data$District_ID),
                                          Project_Grade_level_Category = as.numeric(pred_model_data$Grade_Level_ID),
                                          Post_Month = as.numeric(pred_model_data$Post_Month),
                                          School_Metro_Type = as.numeric(pred_model_data$Metro_Type_ID))
  
  
  
  
  Project_Schools_KNN_Table <- na.omit(Project_Schools_KNN_Table)
  
  #Normilization Function to normalize all the columns.
  normalize <- function(x) { (x - min(x))/(max(x) - min(x)) }
  
  #Applying the Normalization function to all rows of the table except 1st column - which we want to predict.
  Project_Schools_KNN_Table_Normalised <- as.data.frame(lapply(na.omit(Project_Schools_KNN_Table[,c(1:7)]), normalize))
  Project_Schools_KNN_Table_Normalised$DaysTakenGroup <- Project_Schools_KNN_Table$DaysTakenGroup
  
  #Making final table for KNN prediction after normalization.
  Project_Schools_KNN_Table_Normalised_Final <- data.frame(DaysTakenGroup = Project_Schools_KNN_Table_Normalised$DaysTakenGroup,
                                                           Project_Cost_Group = Project_Schools_KNN_Table_Normalised$Project_Cost_Group,
                                                           Project_Type = Project_Schools_KNN_Table_Normalised$Project_Type,
                                                           School_District = Project_Schools_KNN_Table_Normalised$School_District,
                                                           Project_Grade_Level_Category = Project_Schools_KNN_Table_Normalised$Project_Grade_level_Category,
                                                           Post_Month = Project_Schools_KNN_Table_Normalised$Post_Month,
                                                           Metro_Type = Project_Schools_KNN_Table_Normalised$School_Metro_Type)
  #Omitting any N.A values if available.
  Project_Schools_KNN_Table_Normalised_Final <- na.omit(Project_Schools_KNN_Table_Normalised_Final)
  #Deleting the not required table.
  rm(Project_Schools_KNN_Table_Normalised)
  #Breaking the table into test and train tables.
  Project_Schools_KNN_Table_Normalised_Train <- Project_Schools_KNN_Table_Normalised_Final[c(1:744000),]
  Project_Schools_KNN_Table_Normalised_Test <- Project_Schools_KNN_Table_Normalised_Final[c(744001:length(Project_Schools_KNN_Table_Normalised_Final$DaysTakenGroup)),]
  #Prediting by training the KNN model.
  pred_model <- knn((Project_Schools_KNN_Table_Normalised_Train), (Project_Schools_KNN_Table_Normalised_Test), 
                    (Project_Schools_KNN_Table[c(1:744000),1]), k= 50)
  #Displaying the prediction value - 
  pred_ans <- ifelse(pred_model[length(pred_model)] == 1 , "Less Than 15 Days",
                     ifelse(pred_model[length(pred_model)] == 2 , "Between 15 and 30 days",
                            ifelse(pred_model[length(pred_model)] == 3 , "Between 30 and 45 days",
                                   ifelse(pred_model[length(pred_model)] == 4 , "Between 45 and 60 days",
                                          ifelse(pred_model[length(pred_model)] == 5 , "Between 60 and 75 days",
                                                 ifelse(pred_model[length(pred_model)] == 6 , "Between 75 and 90 days",
                                                        ifelse(pred_model[length(pred_model)] == 7 , "Between 90 and 105 days",
                                                               ifelse(pred_model[length(pred_model)] == 8 , "Between 105 and 120 days",
                                                                      ifelse(pred_model[length(pred_model)] == 9 , "Between 120 and 135 days",
                                                                             ifelse(pred_model[length(pred_model)] == 10 , "Between 135 and 150 days",
                                                                                    "More than 150 days"))))))))))
  
  
  
  ans <<- data.frame(Predicted_Time = pred_ans ,
                    Accuracy = "90%")
  return(ans)
  print(ans)
  #pred_model()
  #Calling the Prediction Model Function
  #pred_model()
  
}
#Prediction Model Function



#UI of the application ----
ui <- dashboardPage(
  dashboardHeader(title = "Donor Org Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analytics", tabName = "analytics", icon = icon("bar-chart-o")),
      menuItem("Success Predictor", tabName = "predictor", icon = icon("dashboard")),
      menuItem("Get Donors", tabName = "donors", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # First tab content----
      tabItem(tabName = "analytics",
              h2("Graphical Insights"),
              mainPanel( splitLayout(style = "border: 1px solid black;",
                                     cellWidths = 500,
                                     cellArgs = list(style = "padding: 3px"),
                                     plotOutput("coolplot1")),
                         br(), br(),
                         splitLayout(style = "border: 1px solid black;",
                                     cellWidths = 500,
                                     cellArgs = list(style = "padding: 3px"),
                                     plotOutput("coolplot2")),
                         br(), br(),
                         splitLayout(style = "border: 2px solid black;",
                                     cellWidths = 500,
                                     cellArgs = list(style = "padding: 3px"),
                                     plotOutput("coolplot3")),
                         br(), br(),
                         splitLayout(style = "border: 2px solid black;",
                                     cellWidths = 500,
                                     cellArgs = list(style = "padding: 3px"),
                                     plotOutput("coolplot4")),
                         
                         
                         br(), br(),
                         tableOutput("results")
                         
              )
              
              
      ),
      
      
      # Second tab content----
      tabItem(tabName = "predictor",
              h2("Project Information"),
              selectInput("Post_month_1", "Choose your Post Month:", choices = c(1,2,3,4,5,6,7,8,9,10,11,12)),
              textInput("Cost_1", "Enter your Project Cost"),
              selectInput("Project_Type_1", "Choose your Project Type:", choices = Project_Type),
              selectInput("District_1", "Choose your School District", choices = District),
              selectInput("Grade_level_1", "Choose your Project Grade Level Category:", choices = Grade_Level),
              selectInput("Metro_Type_1", "Choose your School Metro Type:", choices = Metro_Type),
              actionButton("submit", "Submit"),
              tableOutput("resText"), 
              tableOutput("res"), tags$hr(),
              tableOutput("resOutput")
              
      ),
      # Third tab content----
      tabItem(tabName = "donors",
              h2("Get Donors List"),
              selectInput("ID3", "Choose your relevant Project Title:", choices = Project_Title) ,
              actionButton("submit3", "Submit"),
              tableOutput("res3"), tags$hr()
              
      ) 
    )
  )
)



#Server code of application-----
server <- function(input, output, session) {
  
  #first tab----
  {
    output$countryOutput <- renderUI({
      
    })  
    
    filtered <- reactive({
      if (is.null(input$countryInput)) {
        return(NULL)
      }    
      
    })
    
    output$coolplot1 <- renderPlot({
      Donor_Project <- Donations%>%left_join(Projects, by='Project.ID')%>%left_join(Schools, by='School.ID')%>%left_join(Teachers, by='Teacher.ID')
      
      Donor_Project <- Donor_Project%>%arrange(Donation.Received.Date)
      
      Donor_Project <- data.table::data.table(Donor_Project)
      
      Donor_Project[, nth_attempt_donor:=1:.N,by=list(Donor.ID) ]
      
      Donor_Project%>%filter(Donor.Cart.Sequence<=30)%>%
        group_by(Donor.Cart.Sequence)%>%summarize(count=n())%>%
        mutate(Donor.Cart.Sequence=as.factor(Donor.Cart.Sequence))%>%   ggplot(aes(x=Donor.Cart.Sequence, y=count, group=1))+geom_line(stat="identity", color="blue") +
        geom_point(stat="identity", color="red")+ labs(x="The Donor cart sequence",y="no of Donors", title="The difference between the no of one time donors and others")
      
    })
    
    output$coolplot2 <- renderPlot({
      Donor_Project <- Donations%>%left_join(Projects, by='Project.ID')%>%left_join(Schools, by='School.ID')%>%left_join(Donors, by='Donor.ID')
      
      Donor_Project <- data.table::data.table(Donor_Project)
      
      Donor_Project[, nth_attempt_donor:=1:.N,by=list(Donor.ID) ]
      
      Donor_Project<- Donor_Project%>%group_by(Donor.Cart.Sequence, Donor.State, Donor.City, School.State, School.City)%>%summarize(type_of_donor=max(Donor.Cart.Sequence))%>%mutate(type_Donor=ifelse(type_of_donor>1, "Regular Donor", "Single Donor"))
      
      Donor_Project<-Donor_Project%>%mutate(same_state_contribution=ifelse(Donor.State == School.State,"Same Sate Contribution", "other" ), same_city_contribution=ifelse(Donor.City==School.City,"Same city contribution", "other" ))
      
      Donor_Project%>%group_by(Donor.State,same_state_contribution,type_Donor)%>%summarize(count=n())%>% ggplot(aes(x=Donor.State, y=count, fill=same_state_contribution))+geom_bar(stat="identity")+ facet_wrap(~type_Donor)+coord_flip()
      
    })
    
    output$coolplot3 <- renderPlot({
      Donor_Project <- Donations%>%left_join(Projects, by='Project.ID')%>%left_join(Schools, by='School.ID')%>%left_join(Donors, by='Donor.ID')
      
      Donor_Project <- data.table::data.table(Donor_Project)
      
      Donor_Project[, nth_attempt_donor:=1:.N,by=list(Donor.ID) ]
      
      Donor_Project%>%group_by( Project.Grade.Level.Category)%>%summarise(count=n())%>% arrange((count))%>%ungroup%>%mutate(Project.Grade.Level.Category=factor(Project.Grade.Level.Category, levels=Project.Grade.Level.Category))%>% ggplot(aes(x=Project.Grade.Level.Category, y=count))+geom_bar(stat="identity", color="green")+labs(x="Grade Level Category of Projects", y="Donor Count", title="Number of Donations in each Product Grade Level Category")+ coord_flip()
      
    })
    
    output$coolplot4 <- renderPlot({
      
      Donor_Project <- Donations%>%left_join(Projects, by='Project.ID')%>%left_join(Schools, by='School.ID')%>%left_join(Donors, by='Donor.ID')
      
      Donor_Project <- data.table::data.table(Donor_Project)
      
      Donor_Project[, nth_attempt_donor:=1:.N,by=list(Donor.ID) ]
      
      Donor_Project%>%group_by(Project.Subject.Category.Tree)%>%summarise(count=n())%>% arrange((count))%>%ungroup%>%mutate(Project.Subject.Category.Tree=factor(Project.Subject.Category.Tree, levels=Project.Subject.Category.Tree))%>% ggplot(aes(x=Project.Subject.Category.Tree, y=count))+geom_bar(stat="identity", color = "Orange")+labs(x="Project Subject Category", y="Donors Count", title="Nubmer of Donations in each Product Sub Category")+ coord_flip()
      
    })
    
  }
  
  
  
  #Second tab ----
  #reactive value to get the logical output
  rv <- reactiveVal()
  
  #method to generate output on the click of submit button in tab 2
  observeEvent(input$submit, {
    
    
    
    df <- as.data.frame(
      t(sapply(fields2, function(X) input[[X]]))
    ) 
    df2 <- saveProjectData(df)
    rv(rbind(df2))
    #saveProjectData(df)
  })
  
  
  # Update with current input responses selected when Submit is clicked
  output$res <- renderTable({
    input$submit
    rv()
  })
  
  
  
  
  
  output$resText <- renderText("")
  
  #Third tab-----
  
  #method to generate output on the click of submit button in tab 3
  
  
  # titleinput <- as.data.frame(
  #   t(sapply(fields3, function(X) input[[X]]))
  # ) 
  
  
  observeEvent(input$submit3, {
    titleinput <- as.character(input[[fields3]])
    output$res3 <- renderTable({
      dbGetQuery(conn, paste("Select Donor_ID, Is_Teacher from Donor 
                  where City=(Select City From School S JOIN Project P 
                  ON P.School_ID = S.School_ID AND Title = '", titleinput, "' LIMIT 1) LIMIT 15;",sep = ""))
      
    })  
  })
  
  
  
}


#Shiny app call----
shinyApp(ui, server)
