#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

datamerged_coordinate <- read.csv("merged_parcel.csv")

library(shiny)

EnsurePackage <- function(x) {
  x <- as.character(x)
  
  if(!require(x,character.only = T))
     {
    install.packages(pkgs = x,repos = "https://cran.rstudio.org")
    require(x,character.only = T)}
  }
EnsurePackage("ggplot2")
EnsurePackage("e1071")
EnsurePackage("RCurl")
EnsurePackage("jsonlite")
EnsurePackage("geosphere")
EnsurePackage("ggmap")
EnsurePackage("shiny")
EnsurePackage("rgdal")
EnsurePackage("googleway")
EnsurePackage("leaflet")
library(leaflet)


# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Prediction Models"),
   
   # Sidebar with a select input for adding categories 
      tabsetPanel(type = "tabs",
                    tabPanel("NaivesBayes",sidebarPanel(selectInput(inputId = "naivebayes",label = "Select Variables",multiple = TRUE,choices = colnames(datamerged_coordinate)
                    ),
                    actionButton("button", "Predict")), mainPanel(tableOutput("table"))),
                    tabPanel("SVM", mainPanel(tableOutput("SVMtable"))),
                    tabPanel("Map",sidebarPanel(checkboxGroupInput(inputId = "choice",label = "choice",c("vacant" = "vacant","not vacant" = "not vacant")),actionButton("check", "find")), mainPanel(leafletOutput("Maps")))
                    )
# Show a table of predicted vs observed
)

# Define server logic
server <- function(input, output) {

#Naives Bayes Tab
model <- eventReactive(input$button, {
  VacList<- c("VacantBuil",unlist(input$naivebayes))
  VacSubset <- datamerged_coordinate[,VacList]
  set.seed(101) # Set Seed so that same sample can be reproduced in future also
  # Now Selecting 75% of data as sample from total 'n' rows of the data  
  sample <- sample.int(n = nrow(VacSubset), size = floor(.9*nrow(VacSubset)), replace = F)
  train <<- VacSubset[sample, ]
  test  <<- VacSubset[-sample, ]
  naiveBayesModel <- naiveBayes(VacantBuil ~.,data = train)
  
  prediction <- predict(naiveBayesModel,test)
  
  return(table(prediction,test$VacantBuil))
  
  })

#SVM Tab
svmmodel <- eventReactive(input$button,{
  Svm_model <- svm(formula = VacantBuil ~ .,data = train) 
  Svmprediction <- predict(Svm_model,test)
  
  return(table(Svmprediction,test$VacantBuil))
})

# Get coords from Google
# for (i in 1:length(parcelchunk1$Hospital.Name))
# {lonlat <- geocode(parcelchunk1$Hospital.Name[i],sensor = F,override_limit = T)
# Sys.sleep(0.5)
# parcelchunk1$lon[i] <- lonlat$lon
# parcelchunk1$lat[i] <- lonlat$lat
# print(i)
# }
# 
# write.csv(parcelchunk1, file ="parcelchunk16f.csv")


#Map Tab
map <- eventReactive(input$check,{
                    key <- "AIzaSyBG0Qze9xGJ8vouUfvBWUm-RJp7mRECWho"
                     
                    m <- leaflet() %>% addTiles() %>% setView(lng = -76.154480, lat = 43.088947, zoom = 10)
                    return(m)                   
                    
                    # url = paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=43.088947,-76.154480&radius=50000&types=police","&key=",key)
                     # print(url)
                     # doc <- getURL(url)
                     # print(doc)
                     # x <- jsonlite::fromJSON(doc)
                     # Police <- cbind(Name = x$results$name, Address = x$results$vicinity,x$results$geometry$location)
                     
                     })
  

register_google(key = "AIzaSyBG0Qze9xGJ8vouUfvBWUm-RJp7mRECWho")

# parcelchunk1 <- read.csv("SyracuseHospitals.csv",stringsAsFactors = F)
# resultHosp <- do.call(rbind,
#         lapply(1:nrow(parcelchunk1),
#                function(i)revgeocode(as.numeric(parcelchunk1[i,7:8]))))
# parcelchunk1 <- cbind(parcelchunk1,resultHosp)
# write.csv(parcelchunk1,file = "syracusehospital.csv")
# write.csv(Police,file = "SyracusePoliceAddress.csv")

output$table <- renderTable(model())
output$SVMtable <- renderTable(svmmodel())
output$Maps <- renderLeaflet(map())
   
}
  
# Run the application 
shinyApp(ui = ui, server = server)

