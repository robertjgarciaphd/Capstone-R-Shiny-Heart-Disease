library(ggplot2)


# Define UI for application that estimates probability of heart disease
shinyUI(fluidPage(
  tags$style('.container-fluid {
                             background-color: #00fffc;
              }'),
  # Application title
  titlePanel("Heart Disease Estimator"),
  #Originally, the dataset come from the CDC and is a major part of the Behavioral Risk Factor Surveillance System (BRFSS), 
  #which conducts annual telephone surveys to gather data on the health status of U.S. residents. 
  #As the CDC describes: "Established in 1984 with 15 states, BRFSS now collects data in all 50 states as well as the District of Columbia and three U.S. territories. 
  #BRFSS completes more than 400,000 adult interviews each year, making it the largest continuously conducted health survey system in the world.". 
  #The most recent dataset (as of February 15, 2022) includes data from 2020. 
  #It consists of 401,958 rows and 279 columns. 
  #The vast majority of columns are questions asked to respondents about their health status, such as "Do you have serious difficulty walking or climbing stairs?" or "Have you smoked at least 100 cigarettes in your entire life? [Note: 5 packs = 100 cigarettes]".
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "sex",
                     label = "What is your sex?",
                     choices = c('Male','Female')),
      selectizeInput(inputId = "race",
                     label = "What is your race?  (If Other choose White)",
                     choices = c('White','Hispanic', 'Asian', 'Black','American Indian/Alaskan Native')),
      numericInput(inputId ="bmi", label = "What is your BMI?", value = 24, min = 12, max = 60),
      selectizeInput(inputId = "smoking",
                     label = "Have you smoked at least 100 cigarettes in your entire life (approx. 5 packs)?)",
                     choices = c("No","Yes")),
      selectizeInput(inputId = "alcoholdrinking",
                     label ="Do you have more than 14 drinks of alcohol (if you are male) or more than 7 (if you are female) in a week?",
                     choices = c("No","Yes")),
      selectizeInput(inputId = "stroke",
                     label = "Have you ever had a stroke?",
                     choices = c("No","Yes")),
      selectizeInput(inputId = "diffwalking",
                     label = "Do you have serious difficulty walking or climbing stairs?",
                     choices = c("No","Yes")),
      selectizeInput(inputId = "agecategory",
                     label = "Which age category describes you?",
                     choices = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older")),
      selectizeInput(inputId = "diabetic",
                   label = "Do you have diabetes (excluding pregnancy)?",
                   choices = c("No", "No, borderline diabetes", "Yes")),
      selectizeInput(inputId = "genhealth",
                     label = "How would you rate your general health?",
                     choices = c("Good", "Poor", "Fair","Very good", "Excellent")),
      numericInput(inputId = "physicalhealth",
                 label = "For how many days during the past 30 days was your physical health not good?",
                 value = 0, min = 0, max = 30),
      numericInput(inputId = "mentalhealth",
                 label = "For how many days during the past 30 days was your mental health not good?",
                 value = 0, min = 0, max = 30),
     numericInput(inputId = "sleeptime",
               label = "How many hours on average do you sleep?",
               value = 8, min = 0, max = 24),
      selectizeInput(inputId = "asthma",
                     label = "Do you have asthma?",
                     choices = c("No","Yes")),
      selectizeInput(inputId = "kidneydisease",
                     label = "Do you have kidney disease?",
                     choices = c("No","Yes")),
      selectizeInput(inputId = "skincancer",
                     label = "Do you have skin cancer?",
                     choices = c("No","Yes"))
    ),
    
    # Display the probability of heart disease
    mainPanel(
      HTML('<center><img src="heart_image.jpg" width="400"></center>'),
      tabPanel("Description", 
              textOutput("description")
              ),
      actionButton("calculate", "Calculate"),
      tabPanel("probability_hd",
              textOutput("prob_hd")
              )
      
     
    )
  )
))



#################
