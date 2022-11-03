library(ggvis)
library(dplyr)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}

# Define server logic required to draw a graph
shinyServer(function(input, output) {
  hd <- eventReactive(
    input$calculate,
    {
    #Create a row of data for the user 
      user_data = data.frame(BMI=as.double(input$bmi),
                             Smoking=as.factor(input$smoking),
                             AlcoholDrinking=as.factor(input$alcoholdrinking),
                             Stroke=as.factor(input$stroke),
                             PhysicalHealth=as.numeric(input$physicalhealth),
                             MentalHealth=as.numeric(input$mentalhealth),
                             DiffWalking=as.factor(input$diffwalking),
                             Sex=as.factor(input$sex),
                             AgeCategory=as.factor(input$agecategory),
                             Race=as.factor(input$race),
                             Diabetic=as.factor(input$diabetic),
                             GenHealth=as.factor(input$genhealth),
                             SleepTime=as.integer(input$sleeptime),
                             Asthma=as.factor(input$asthma),
                             KidneyDisease=as.factor(input$kidneydisease),
                             SkinCancer=as.factor(input$skincancer),
                             stringsAsFactors=TRUE)
    
      
      #Run user-generated values in the logistic regression model for probability calculation
      #data_balanced_over <- ovun.sample(HeartDisease ~ ., data = train, method = "over",N = 2*233377)$data
      #model = glm(HeartDisease ~ . -PhysicalActivity, family = "binomial", data = data_balanced_over)
      
      user_prediction = predict(model_old, type="response", newdata = user_data)
  
      round(user_prediction*100,2)
    })   
  
  
  output$description <-renderText({"This app uses logistic regression to calculate your probability of heart disease based on your demographic and health data. 
               Note that this statistical approach is not to be mistaken for a true diagnosis and that the error rate is non-trivial. 
               For example, though the accuracy - or percentage of total correct predictions over total predictions - is 75%, 
               the precision - or percentage of positive identifications the model got correct - is 22%, so false positives are common."
  })
    
  output$prob_hd <- renderText({ paste0("Probability of heart disease: ", hd(), "%")
  })

  
  
}  

)  
