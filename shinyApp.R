library(shiny)
library(ggplot2)
library(mc2d)
library(sqldf)

ui <- fluidPage(
  headerPanel("Bed pooling demonstration"), 
  sidebarPanel(
    selectInput("graphType","Choose graph",c("Overspill","Occupied Beds")),
    numericInput("modelLen","Model length (days)",365),
    numericInput("meanInputMales","Mean admissions per day - male",.75),
    numericInput("meanInputFemales","Mean admissions per day - female",.75),
    "Length of Stay Distribution",
    numericInput("losLow","min",1),
    numericInput("losMid","median",6),
    numericInput("losHigh","max",122),
    "Capacity",
    numericInput("capacityMale","capacity - male",8),
    numericInput("capacityFemale","capacity - female",8)
  ),
  mainPanel(
    plotOutput('Plot1'),
    plotOutput('Plot2')
  )
)

server <- function(input, output){
  
  output$Plot2 <- renderPlot({
    vals <- rtriang(10000, input$losLow,input$losMid,input$losHigh)
    hist(vals, main = "Graph of LOS distribution")
  })
  output$Plot1 <- renderPlot({
    rLen <- input$modelLen
    rDays <- rep(c(1:rLen),3)
    
    rAdmissionsMale <- rpois(rLen,input$meanInputMales)
    rAdmissionsFemale <- rpois(rLen,input$meanInputFemales)
    rAdmissionsNull <- rep(0,rLen)
    rAdmissions <- c(rAdmissionsMale,rAdmissionsFemale,rAdmissionsNull)  
    
    rCapacityMale <- rep(input$capacityMale,rLen)
    rCapacityFemale <- rep(input$capacityFemale,rLen)
    rCapacityCombined <- rep(input$capacityMale+input$capacityFemale,rLen)
    rCapacity <- c(rCapacityMale,rCapacityFemale,rCapacityCombined)

    rGender = c(rep("Gendered scenario - Male",rLen),rep("Gendered scenario - Female",rLen), rep("Combined wards scenario",rLen))
    
    rOccupiedBedsBase <- rep(0,(3*rLen))
    
    rExcessDemand <- rep(0,(3*rLen))
    
    xD <- data.frame(day = rDays,gender = rGender,admissions = rAdmissions,capacity = rCapacity, 
                     occupiedbeds = rOccupiedBedsBase,excessdemand = rExcessDemand,
                     occupiedinternalbeds = rOccupiedBedsBase,occupiedoverspillbeds = rOccupiedBedsBase)
    

    for(i in 1:rLen){
      if(xD$admissions[i]>0){
          patients <- rtriang(xD$admissions[i],input$losLow,input$losMid,input$losHigh)
          for(j in 1:length(patients)){
            for(k in 0:(patients[j]-1)){
              if(i+k <= rLen){
                xD$occupiedbeds[i+k] <- xD$occupiedbeds[i+k] + 1
              }
            }
          }
      }
    }

    for(i in (rLen+1):(2*rLen)){
      if(xD$admissions[i]>0){
        patients <- rtriang(xD$admissions[i],input$losLow,input$losMid,input$losHigh)
        for(j in 1:length(patients)){
          for(k in 0:(patients[j]-1)){
            if(i+k <= (2*rLen)){
              xD$occupiedbeds[i+k] <- xD$occupiedbeds[i+k] + 1
            }
          }
        }
      }
    }
    
    for(i in (2*rLen+1):(3*rLen)){
      xD$occupiedbeds[i] <- xD$occupiedbeds[i-rLen] + xD$occupiedbeds[i-2*rLen]
    }
    
    for(i in 1:(3*rLen)){
      xD$excessdemand[i] = max(0,xD$occupiedbeds[i]-xD$capacity[i])
    }
    
    #write.csv(xD,"TestData.csv")
    
    smry <- sqldf("SELECT gender, sum(excessdemand) as overspill FROM xD GROUP BY gender")
    addtosmry <- data.frame("Gendered wards scenario",sum(smry$overspill[which(smry$gender != 'Combined')]))
    names(addtosmry) <- names(smry)
    smry <- rbind(smry,addtosmry)
    smry$overspill <- smry$overspill/rLen
    smry <- smry[which(!(smry$gender %in% c("Gendered scenario - Male","Gendered scenario - Female"))),]
    
    if(input$graphType == "Overspill"){
          ggplot(smry, aes(x=gender, y = overspill, color = gender)) + geom_point(size  = 4) + 
      ggtitle("use of overspill") + expand_limits(y=0)
      
    } else {
      ggplot(xD, aes(x = day, y = occupiedbeds, color = gender)) + 
       geom_line() + ggtitle("occupied beds over modelling period")
    }
  })
}

shinyApp(ui = ui, server = server)