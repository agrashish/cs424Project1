#Project 1
#Aashish Agrawal - aagraw10
#CS 424

#load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)

#read in data from csv
rawdata <- read.csv("litterati challenge-65.csv")

#convert data to proper formats

#fix date format to posix ct
rawdata$litterTimestamp <- as.POSIXct(rawdata$litterTimestamp, format = "%Y-%m-%d %H:%M:%S")

#fix tags data into characters
rawdata$tags <- as.character(rawdata$tags)
#turn tags into a list of character vectors
rawdata$tags  <- strsplit(rawdata$tags, ",")
#also make tags that were 'untagged' marked as such
rawdata$tags <- lapply(rawdata$tags, function(x) {
  if(length(x) == 0) {
    x <- c("untagged")
  }
  else {
    x
  }
})

#get rid of variables that were factor types
#converting them to character vectors instead
rawdata$url <- as.character(rawdata$url)
rawdata$username <- as.character(rawdata$username)

#when lat and long are 0, that marks the middle of the ocean
#i am assuming this is the filler variable for when the user may not have geotagged their litter
#so i will replace those 0's with NA's, so that it does not appear on the map
rawdata$lat[(rawdata$lat == 0) & (rawdata$lon == 0)] <- NA
rawdata$lon[which(is.na(rawdata$lat))] <- NA

#there are two unique outliers, that appear to be in antartica
#upon looking at the data, it seems to be a simple error
#the users seem to have marked the latitude as longitude, and vice versa
#so we'll swap those values for those 2 outliers, fixing the data
swapindexes <- which(rawdata$lat < 0)
temp <- rawdata$lat[swapindexes]
temp2 <- rawdata$lon[swapindexes]
rawdata$lat[swapindexes] <- temp2
rawdata$lon[swapindexes] <- temp

#now we'll setup top 10 table data
#the process below generates frequencies of data using a table
#then cleans and formats data to be ideal for graphing
litterRank <- as.data.frame(table(rawdata$username))
colnames(litterRank) <- c("Username", "Total")
litterRank <- litterRank[rev(order(litterRank$Total)),]
litter10 <- data.frame("Username" = 1:length(litterRank$Username), "Total" = 1:length(litterRank$Total))
litter10$Username <- litterRank$Username
litter10$Total <- litterRank$Total
litter10 <- head(litter10, 10)

#for the litter picked each day
#the process below generates frequencies of data using a table
#then cleans and formats data to be ideal for graphing
allDays <- as.data.frame(table(as.Date(rawdata$litterTimestamp)))
colnames(allDays) <- c("Day", "LitterPickedUp")
allDays$Day <- as.Date(allDays$Day)
allDays$set <- "all"

#for the litter picked up by weekday
#the process below generates frequencies of data using a table
#then cleans and formats data to be ideal for graphing
byWeekday <- as.data.frame(table(weekdays(as.Date(rawdata$litterTimestamp))))
colnames(byWeekday) <- c("DayOfWeek", "LitterPickedUp")
weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
byWeekday$DayOfWeek <- as.character(byWeekday$DayOfWeek)
byWeekday <- byWeekday[match(weekdays, byWeekday$DayOfWeek),]
byWeekday$DayOfWeek <- weekdays
byWeekday$LitterPickedUp[is.na(byWeekday$LitterPickedUp)] <- 0
oldWeekday <- byWeekday
byWeekday <- data.frame("DayOfWeek" = 1:length(oldWeekday$DayOfWeek), "LitterPickedUp" = 1:length(oldWeekday$LitterPickedUp))
byWeekday$DayOfWeek <- oldWeekday$DayOfWeek
byWeekday$LitterPickedUp <- oldWeekday$LitterPickedUp
byWeekday$set <- "all"

#for the litter picked up by hour
#the process below generates frequencies of data using a table
#then cleans and formats data to be ideal for graphing
byHour <- as.data.frame(table(factor(as.POSIXlt(rawdata$litterTimestamp)$hour, levels = 0:23)))
colnames(byHour) <- c("Hour", "LitterCount")
byHour$Hour <- as.numeric(byHour$Hour)
byHour$set <- "all"

#for the litter by tag
#the process below generates frequencies of data using a table
#then cleans and formats data to be ideal for graphing
byTags <- as.data.frame(table(unlist(rawdata$tags)))
colnames(byTags) <- c("Tag", "LitterCount")
byTags <- byTags[rev(order(byTags$LitterCount)),]
tag10 <- data.frame("Tag" = 1:length(byTags$Tag), "LitterCount" = 1:length(byTags$LitterCount))
tag10$Tag <- byTags$Tag
tag10$Tag <- as.character(tag10$Tag)
tag10$LitterCount <- byTags$LitterCount
tag10$LitterCount <- as.numeric(tag10$LitterCount)
tag10$set <- "all"
tag10 <- head(tag10, 10)

#used for time ranges calculations
morning <- c(5,12)
afternoon <- c(12,17)
evening <- c(17,20)
night <- c(20,24)
night2 <- c(0,5)

#used for picking month calculations
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  dashboardSidebar(
      sidebarMenu(id = "tabs",
        #for our sidebar, we'll add a bunch of blanks
        #in order to generate space from the top of the big screen
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        menuItem("", tabName = "blanks"),
        #after that, we'll have a tab that can be clicked to move the main display down
        menuItem("Move Visuals Down", tabName = "filler"),
        #and one more for an "About" page
        menuItem("About", tabName = "About"),
        #we'll make a selection input for picking what we're comparing the summary data to
        selectInput("pickFilter", "Select Type to Compare to", choices = c("None", "User", "Tag", "Time of Day", "Month")),
        #then we'll dynamically generate the corresponding selection based on what kind of data the user is comparing with
        uiOutput("picker")
      )
  ),
  dashboardBody(
    #display about or filler here if needed
    uiOutput("about"),
    #first row will have all of our bar charts
    fluidRow(
      #we'll make a nested tabbed box
      #the outer nest lets you pick if you want the data by Day, Weekday, Hour, or Tag
      #the inner nest lets you pick if you want a bar chart or a table
      tabBox(width = 12, 
        tabPanel(
          title = "By Day",
          tabsetPanel(
            tabPanel(title = "Bar Chart", plotOutput("barAllDays")),
            tabPanel(title = "Table", DT::dataTableOutput('tableAllDays'))
          )
        ),
        tabPanel(
          title = "By Weekday",
          tabsetPanel(
            tabPanel(title = "Bar Chart", plotOutput("barWeekdays")),
            tabPanel(title = "Table", DT::dataTableOutput('tableWeekdays'))
          )
        ),
        tabPanel(
          title = "By Hour",
          tabsetPanel(
            tabPanel(title = "Bar Chart", plotOutput("barHours")),
            tabPanel(title = "Table", DT::dataTableOutput('tableHours'))
          )
        ),
        tabPanel(
          title = "By Tag",
          tabsetPanel(
            tabPanel(title = "Bar Chart", plotOutput("barTags")),
            tabPanel(title = "Table", DT::dataTableOutput('tableTags'))
          )
        )
      )
    ),
    #the second row has the map, and the top 10 pickers by username table
    #below the map the total amount of litter picked up is also listed
    fluidRow(
      box(width = 6, title = "Litter Map", leafletOutput("leaf"), paste("The total count of litter picked up was ", length(rawdata$litterId))),
      box(width = 6, title = "Top 10 Pickers by Username", DT::dataTableOutput('top10'))
    )
  )
)

server <- function(input, output) {
  #this reactive variable is where we'll filter the raw data, to be used for the rest of the dataframes
  rawdataFiltered <- reactive({
    if(input$pickFilter == "User") {
      rawdataFiltered <- rawdata[rawdata$username == input$userFilter,]
    }
    else if(input$pickFilter == "Tag") {
      rawdataFiltered <- rawdata
      rawdataFiltered$tags <- lapply(rawdataFiltered$tags, function(x){
        if(input$userFilter %in% unlist(x)) {
          x
        }
        else {
          NA
        }
      })
      rawdataFiltered <- rawdataFiltered[!is.na(rawdataFiltered$tags),]
    }
    else if(input$pickFilter == "Time of Day") {
      rawdataFiltered <- rawdata
      if(input$userFilter == "Morning") {
        hours <- morning
        rawdataFiltered <- rawdataFiltered[
          as.POSIXlt(rawdataFiltered$litterTimestamp)$hour >= hours[1] 
          & as.POSIXlt(rawdataFiltered$litterTimestamp)$hour <= hours[2]
          ,]
      }
      else if(input$userFilter == "Afternoon") {
        hours <- afternoon
        rawdataFiltered <- rawdataFiltered[
          as.POSIXlt(rawdataFiltered$litterTimestamp)$hour >= hours[1] 
          & as.POSIXlt(rawdataFiltered$litterTimestamp)$hour <= hours[2]
          ,]
      }
      else if(input$userFilter == "Evening") {
        hours <- evening
        rawdataFiltered <- rawdataFiltered[
          as.POSIXlt(rawdataFiltered$litterTimestamp)$hour >= hours[1] 
          & as.POSIXlt(rawdataFiltered$litterTimestamp)$hour <= hours[2]
          ,]
      }
      else {
        hours <- night
        hours2 <- night2
        rawdataFiltered <- rawdataFiltered[
          (as.POSIXlt(rawdataFiltered$litterTimestamp)$hour >= hours[1] 
          & as.POSIXlt(rawdataFiltered$litterTimestamp)$hour <= hours[2])
          ||(as.POSIXlt(rawdataFiltered$litterTimestamp)$hour >= hours2[1] 
             & as.POSIXlt(rawdataFiltered$litterTimestamp)$hour <= hours2[2])
          ,]
      }
    }
    else if(input$pickFilter == "Month") {
      rawdataFiltered <- rawdata
      rawdataFiltered <- rawdataFiltered[months(as.POSIXlt(rawdataFiltered$litterTimestamp)) == input$userFilter,]
    }
    else {
      rawdataFiltered <- rawdata
    }
    rawdataFiltered
  })
  
  #the reactive dataframe used for the By Day data
  allDaysFiltered <- reactive({
    allDaysFiltered <- as.data.frame(table(as.Date(rawdataFiltered()$litterTimestamp)))
    colnames(allDaysFiltered) <- c("Day", "LitterPickedUp")
    allDaysFiltered$Day <- as.Date(allDaysFiltered$Day)
    allDaysFiltered$set <- input$userFilter
    allDaysFiltered
  })
  
  #the reactive dataframe used for the By Weekday data
  byWeekdayFiltered <- reactive({
    byWeekdayFiltered <- as.data.frame(table(weekdays(as.Date(rawdataFiltered()$litterTimestamp))))
    colnames(byWeekdayFiltered) <- c("DayOfWeek", "LitterPickedUp")
    weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    byWeekdayFiltered$DayOfWeek <- as.character(byWeekdayFiltered$DayOfWeek)
    byWeekdayFiltered <- byWeekdayFiltered[match(weekdays, byWeekdayFiltered$DayOfWeek),]
    byWeekdayFiltered$DayOfWeek <- weekdays
    byWeekdayFiltered$LitterPickedUp[is.na(byWeekdayFiltered$LitterPickedUp)] <- 0
    oldWeekday <- byWeekdayFiltered
    byWeekdayFiltered <- data.frame("DayOfWeek" = 1:length(oldWeekday$DayOfWeek), "LitterPickedUp" = 1:length(oldWeekday$LitterPickedUp))
    byWeekdayFiltered$DayOfWeek <- oldWeekday$DayOfWeek
    byWeekdayFiltered$LitterPickedUp <- oldWeekday$LitterPickedUp
    byWeekdayFiltered$set <- input$userFilter
    byWeekdayFiltered
  })
  
  #the reactive dataframe used for the By Hour data
  byHourFiltered <- reactive({
    byHourFiltered <- as.data.frame(table(factor(as.POSIXlt(rawdataFiltered()$litterTimestamp)$hour, levels = 0:23)))
    colnames(byHourFiltered) <- c("Hour", "LitterCount")
    byHourFiltered$Hour <- as.numeric(byHourFiltered$Hour)
    byHourFiltered$set <- input$userFilter
    byHourFiltered
  })
  
  #the reactive dataframe used for the Tag data
  tag10Filtered <- reactive({
    byTagsFiltered <- as.data.frame(table(unlist(rawdataFiltered()$tags)))
    colnames(byTagsFiltered) <- c("Tag", "LitterCount")
    byTagsFiltered <- byTagsFiltered[rev(order(byTagsFiltered$LitterCount)),]
    tag10Filtered <- data.frame("Tag" = 1:length(byTagsFiltered$Tag), "LitterCount" = 1:length(byTagsFiltered$LitterCount))
    tag10Filtered$Tag <- byTagsFiltered$Tag
    tag10Filtered$LitterCount <- byTagsFiltered$LitterCount
    oldtag <- tag10Filtered[tag10Filtered$Tag %in% tag10$Tag,]
    tag10Filtered <- data.frame("Tag" = 1:length(oldtag$Tag), "LitterCount" = 1:length(oldtag$LitterCount))
    tag10Filtered$Tag <- oldtag$Tag
    tag10Filtered$Tag <- as.character(tag10Filtered$Tag)
    tag10Filtered$LitterCount <- oldtag$LitterCount
    tag10Filtered$LitterCount <- as.numeric(tag10Filtered$LitterCount)
    tagsFill <- tag10$Tag
    if(length(tag10Filtered$Tag) != 10) {
      for(x in (length(tag10Filtered$Tag)+1):10) {
        for(y in tagsFill) {
          if(!(y %in% tag10Filtered[,1])) {
            tag10Filtered <- rbind(tag10Filtered, c(y, 0))
            break
          }
        }        
      }
    }
    tag10Filtered$set <- input$userFilter
    tag10Filtered <- head(tag10Filtered, 10)
  })
  
  #generate our map here
  output$leaf <- renderLeaflet({
    #use custom icon colors based on if it's the summary data, or filtered data
    greenLeafIcon <- makeIcon(
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
      iconWidth = 38, iconHeight = 95,
      iconAnchorX = 22, iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    redLeafIcon <- makeIcon(
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-red.png",
      iconWidth = 38, iconHeight = 95,
      iconAnchorX = 22, iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    if(input$pickFilter != "None") {
      map <- leaflet()
      map <- addTiles(map)
      temp <- input$pickFilter
      map <- addMarkers(map = map, data = rawdata, lat = ~lat, lng = ~lon, group = "all", clusterOptions = markerClusterOptions(), icon = greenLeafIcon)
      map <- addMarkers(map = map, data = rawdataFiltered(), lat = ~lat, lng = ~lon, group = input$userFilter, clusterOptions = markerClusterOptions(), icon = redLeafIcon)
      map <- addLayersControl(map = map, overlayGroups = c("all", input$userFilter))
      map <- setView(map = map, lat = 41.869, lng = -87.81, zoom = 13)
      map
    }
    else {
      map <- leaflet()
      map <- addTiles(map)
      map <- addMarkers(map = map, data = rawdata, lat = ~lat, lng = ~lon, group = "all", clusterOptions = markerClusterOptions(), icon = greenLeafIcon)
      map <- setView(map = map, lat = 41.869, lng = -87.81, zoom = 13)
      map
    }
  })
  
  #generate the top 10 pickers table here
  output$top10 <- DT::renderDataTable(litter10)
  
  #generate the By Day graph  here
  output$barAllDays <- renderPlot({
    if(input$pickFilter != "None") {
      g1 <- ggplot(rbind(allDays, allDaysFiltered()), aes(x = Day, y = LitterPickedUp, fill = set)) 
      g1 <- g1 + geom_col(position = "dodge")
      g1 <- g1 + labs(title = "Amount of Litter Picked Up Each Day", subtitle = "From Apr 4, 2018 to Jan 7, 2020")
      g1 <- g1 + labs(x = "Day", y = "Litter Count")
      g1
    }
    else {
      g1 <- ggplot(allDays, aes(x = Day, y = LitterPickedUp)) + geom_col(col = "blue")
      g1 <- g1 + labs(title = "Amount of Litter Picked Up Each Day", subtitle = "From Apr 4, 2018 to Jan 7, 2020")
      g1 <- g1 + labs(x = "Day", y = "Litter Count")
      g1
    }
  })
  
  #generate the By Day table  here
  output$tableAllDays <- DT::renderDataTable({
    if(input$pickFilter != "None") {
      rbind(allDays, allDaysFiltered())
    }
    else {
      allDays
    }
  })
  
  #generate the By Weekday graph  here
  output$barWeekdays <- renderPlot({
    if(input$pickFilter != "None") {
      g2 <- ggplot(rbind(byWeekday,byWeekdayFiltered()), aes(x = factor(DayOfWeek, weekdays), y = LitterPickedUp, fill = set))
      g2 <- g2 + geom_col(position = "dodge")
      g2 <- g2 + labs(title = "Amount of Litter Picked Up by Day of Week")
      g2 <- g2 + labs(x = "Weekday", y = "Litter Count")
      g2
    }
    else {
      g2 <- ggplot(byWeekday, aes(x = factor(DayOfWeek, weekdays), y = LitterPickedUp)) + geom_col(color = "black", fill = "#FF99CC")
      g2 <- g2 + labs(title = "Amount of Litter Picked Up by Day of Week")
      g2 <- g2 + labs(x = "Weekday", y = "Litter Count")
      g2
    }
  })
  
  #generate the By Weekday table here
  output$tableWeekdays <- DT::renderDataTable({
    if(input$pickFilter != "None") {
      rbind(byWeekday,byWeekdayFiltered())
    }
    else {
      byWeekday
    }
  })
  
  #generate the By Hour graph here
  output$barHours <- renderPlot({
    if(input$pickFilter != "None") {
      g3 <- ggplot(rbind(byHour, byHourFiltered()), aes(x = Hour, y = LitterCount, fill = set)) 
      g3 <- g3 + geom_col(position = "dodge")
      g3 <- g3 + labs(title = "Amount of Litter Picked Up by Hour of Day")
      g3 <- g3 + labs(x = "Hour", y = "Litter Count")
      g3
    }
    else {
      g3 <- ggplot(byHour, aes(x = Hour, y = LitterCount)) + geom_col(color = "black", fill = "#33FFFF")
      g3 <- g3 + labs(title = "Amount of Litter Picked Up by Hour of Day")
      g3 <- g3 + labs(x = "Hour", y = "Litter Count")
      g3
    }
  })
  
  #generate the By Hour table here
  output$tableHours <- DT::renderDataTable({
    if(input$pickFilter != "None") {
      rbind(byHour, byHourFiltered())
    }
    else {
      byHour
    }
  })
  
  #generate the By Tag graph here
  output$barTags <- renderPlot({
    if(input$pickFilter != "None") {
      g4 <- ggplot(rbind(tag10, tag10Filtered()), aes(x = Tag, y = as.numeric(LitterCount), fill = set))
      g4 <- g4 + geom_col(position = "dodge")
      g4 <- g4 + labs(title = "Amount of Litter Picked Up by Top 10 Tags")
      g4 <- g4 + labs(x = "Tag", y = "Litter Count")
      g4
    }
    else {
      g4 <- ggplot(tag10, aes(x = Tag, y = LitterCount)) + geom_col(color = "black", fill = "#66FF00")
      g4 <- g4 + labs(title = "Amount of Litter Picked Up by Top 10 Tags")
      g4 <- g4 + labs(x = "Tag", y = "Litter Count")
      g4
    }
  })
  
  #generate the By Tag table here
  output$tableTags <- DT::renderDataTable({
    if(input$pickFilter != "None") {
      rbind(tag10, tag10Filtered())
    }
    else {
      tag10
    }
  })
  
  #generate the selector based on the category of data we want to filter by
  #categories include User, Tag, Time of Day, and Month
  output$picker <- renderUI({
    if(input$pickFilter == "User") {
      selectInput("userFilter", "Select User", choices = litter10$Username)
    }
    else if(input$pickFilter == "Tag") {
      selectInput("userFilter", "Select Tag", choices = tag10$Tag)
    }
    else if(input$pickFilter == "Time of Day") {
      selectInput("userFilter", "Select Time of Day (Note: Morning is from 5am to 12pm. Afternoon is from 12pm to 5pm. Evening is from 5pm to 8 pm. Night is from 8pm to 5am)", choices = c("Morning", "Afternoon", "Evening", "Night"))
    }
    else if(input$pickFilter == "Month") {
      selectInput("userFilter", "Select Month", choices = months)
    }
    else {
      
    }
  })
  
  #generate about or filler ui elements here
  output$about <- renderUI({
    if(input$tabs == "About") {
      box(
        title = "About", 
        height = 400,
        p("This project was written by Aashish Agrawal."), 
        p("The libraries used are shiny, shiny dashboard, leaflet, and ggplot2."),
        p("The data came from Literatti, from a 2019 challenge from the Go Green Forest Park group.")
      )
    }
    else if(input$tabs == "filler") {
      box(
       title = "Litterati Visualization, for CS 424",
       width = 12,
       height = 400
      )
    }
    else {
      
    }
  })
}

shinyApp(ui = ui, server = server)