library(shiny)
library(stringr)
#load("class100.Rdata")
load("classes.Rdata")

shinyServer(function(input, output) {
  
  values <- reactiveValues()
  updateData <- function() {
    #save(class_keys, file = ".original.class100.Rdata", compress = "bzip2")
    vars <- load(file = "class100.Rdata", envir = .GlobalEnv)
    for (var in vars)
      values[[var]] <- get(var, .GlobalEnv)
  }
  updateData()  # also call updateData() whenever you want to reload the data
  
  output$class1 <- renderText({classes[1,]})
  output$class2 <- renderText({classes[2,]})
  output$class3 <- renderText({classes[3,]})
  output$class4 <- renderText({classes[4,]})
  output$class5 <- renderText({classes[5,]})
  output$class6 <- renderText({classes[6,]})
  output$class7 <- renderText({classes[7,]})
  output$class8 <- renderText({classes[8,]})
  output$class9 <- renderText({classes[9,]})
  output$class10 <- renderText({classes[10,]})
  output$class11 <- renderText({classes[11,]})
  output$class12 <- renderText({classes[12,]})
  output$class13 <- renderText({classes[13,]})
  output$class14 <- renderText({classes[14,]})
  output$class15 <- renderText({classes[15,]})
  output$class16 <- renderText({classes[16,]})
  output$class17 <- renderText({classes[17,]})
  output$class18 <- renderText({classes[18,]})
  output$class19 <- renderText({classes[19,]})
  
  
  output$classh1 <- renderText({paste("## ", classes[1,])})
  output$classh2 <- renderText({paste("## ", classes[2,])})
  output$classh3 <- renderText({paste("## ", classes[3,])})
  output$classh4 <- renderText({paste("## ", classes[4,])})
  output$classh5 <- renderText({paste("## ", classes[5,])})
  output$classh6 <- renderText({paste("## ", classes[6,])})
  output$classh7 <- renderText({paste("## ", classes[7,])})
  output$classh8 <- renderText({paste("## ", classes[8,])})
  output$classh9 <- renderText({paste("## ", classes[9,])})
  output$classh10 <- renderText({paste("## ", classes[10,])})
  output$classh11 <- renderText({paste("## ", classes[11,])})
  output$classh12 <- renderText({paste("## ", classes[12,])})
  output$classh13 <- renderText({paste("## ", classes[13,])})
  output$classh14 <- renderText({paste("## ", classes[14,])})
  output$classh15 <- renderText({paste("## ", classes[15,])})
  output$classh16 <- renderText({paste("## ", classes[16,])})
  output$classh17 <- renderText({paste("## ", classes[17,])})
  output$classh18 <- renderText({paste("## ", classes[18,])})
  output$classh19 <- renderText({paste("## ", classes[19,])})

  
  output$keywordr <- reactive({
    if(dim(class_keys[class_keys$keyword==input$keyword,])[1] != 0) {
      updateData()
      output$download <- downloadHandler(
        filename = paste("keyword_list",Sys.time(),".csv"),
        content = function(file) {
          write.csv(class_keys, file)
        })
      
      "Keyword found in Database"
    }
    else {
      updateData()
      output$download <- downloadHandler(
        filename = paste("keyword_list",Sys.time(),".csv"),
        content = function(file) {
          write.csv(class_keys, file)
        })
      
      "Keyword not found in Database"
    }
  })

  output$classr <- reactive({
    paste("Stored Value: ", class_keys[class_keys$keyword==input$keyword,][2])
    })
  
  output$toast <- reactive({
    
    if(dim(class_keys[class_keys$keyword==input$keyword,])[1] != 0 && nchar(input$class)>0) {
      class_keys[class_keys$keyword==input$keyword,][2]<-input$class
      save(class_keys, file = "class100.Rdata", compress = "bzip2")
      updateData()
      output$download <- downloadHandler(
        filename = paste("keyword_list",Sys.time(),".csv"),
        content = function(file) {
          write.csv(class_keys, file)
        })
      
      output$classn <- renderText({paste("New Value: ", class_keys[class_keys$keyword==input$keyword,][2])})
      "Status: Successfully Updated Record"
    }
    
    else if(dim(class_keys[class_keys$keyword==input$keyword,])[1] == 0 && nchar(input$class)>0) {
      keyword_list <- tolower(str_trim(unlist(str_split(string = input$keyword,pattern = ';'))))
      for(keyword in keyword_list) {
        if(dim(class_keys[class_keys$keyword==keyword,])[1] != 0) {
          class_keys[class_keys$keyword==keyword,][2]<-input$class
          output$keywordr <- renderText({"Keyword(s) found in data"})
        }
        else {
          class_keys[dim(class_keys)[1]+1, ] <- c(keyword, input$class)
          output$keywordr <- renderText({"Keyword not present in data"})
        }
      }
      save(class_keys, file = "class100.Rdata", compress = "bzip2")
      updateData()
      output$download <- downloadHandler(
        filename = paste("keyword_list",Sys.time(),".csv"),
        content = function(file) {
          write.csv(class_keys, file)
        })
      
      output$classn <- renderText({paste("New Value: ", class_keys[class_keys$keyword==input$keyword,][2])})
      "Status: New Keyword(s) added to data"
    }
    
    else {
      updateData()
      output$download <- downloadHandler(
        filename = paste("keyword_list",Sys.time(),".csv"),
        content = function(file) {
          write.csv(class_keys, file)
        })
      
      output$classn <- renderText({paste("New Value: ")})
      "Status: No Change"
    }
    
  })
  
  output$download <- downloadHandler(
    filename = paste("keyword_list",Sys.time(),".csv"),
    content = function(file) {
      write.csv(class_keys, file)
    })
  
  output$keywords1 <- reactive({
    if(input$class == '1') {
      paste(class_keys[class_keys$classes == "1",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "1",]$keyword, collapse = " ; ")
    }
  })
  
  output$keywords2 <- reactive({
    if(input$class == '2') {
      paste(class_keys[class_keys$classes == "2",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "2",]$keyword, collapse = " ; ")
    }
  })
  output$keywords3 <- reactive({
    if(input$class == '3') {
      paste(class_keys[class_keys$classes == "3",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "3",]$keyword, collapse = " ; ")
    }
  })
  output$keywords4 <- reactive({
    if(input$class == '4') {
      paste(class_keys[class_keys$classes == "4",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "4",]$keyword, collapse = " ; ")
    }
  })
  output$keywords5 <- reactive({
    if(input$class == '5') {
      paste(class_keys[class_keys$classes == "5",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "5",]$keyword, collapse = " ; ")
    }
  })
  output$keywords6 <- reactive({
    if(input$class == '6') {
      paste(class_keys[class_keys$classes == "6",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "6",]$keyword, collapse = " ; ")
    }
  })
  output$keywords7 <- reactive({
    if(input$class == '7') {
      paste(class_keys[class_keys$classes == "7",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "7",]$keyword, collapse = " ; ")
    }
  })
  output$keywords8 <- reactive({
    if(input$class == '8') {
      paste(class_keys[class_keys$classes == "8",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "8",]$keyword, collapse = " ; ")
    }
  })
  output$keywords9 <- reactive({
    if(input$class == '9') {
      paste(class_keys[class_keys$classes == "9",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "9",]$keyword, collapse = " ; ")
    }
  })
  output$keywords10 <- reactive({
    if(input$class == '10') {
      paste(class_keys[class_keys$classes == "10",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "10",]$keyword, collapse = " ; ")
    }
  })
  output$keywords11 <- reactive({
    if(input$class == '11') {
      paste(class_keys[class_keys$classes == "11",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "11",]$keyword, collapse = " ; ")
    }
  })
  output$keywords12 <- reactive({
    if(input$class == '12') {
      paste(class_keys[class_keys$classes == "12",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "12",]$keyword, collapse = " ; ")
    }
  })
  output$keywords13 <- reactive({
    if(input$class == '13') {
      paste(class_keys[class_keys$classes == "13",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "13",]$keyword, collapse = " ; ")
    }
  })
  output$keywords14 <- reactive({
    if(input$class == '14') {
      paste(class_keys[class_keys$classes == "14",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "14",]$keyword, collapse = " ; ")
    }
  })
  output$keywords15 <- reactive({
    if(input$class == '15') {
      paste(class_keys[class_keys$classes == "15",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "15",]$keyword, collapse = " ; ")
    }
  })
  output$keywords16 <- reactive({
    if(input$class == '16') {
      paste(class_keys[class_keys$classes == "16",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "16",]$keyword, collapse = " ; ")
    }
  })
  output$keywords17 <- reactive({
    if(input$class == '17') {
      paste(class_keys[class_keys$classes == "17",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "17",]$keyword, collapse = " ; ")
    }
  })
  output$keywords18 <- reactive({
    if(input$class == '18') {
      paste(class_keys[class_keys$classes == "18",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "18",]$keyword, collapse = " ; ")
    }
  })
  output$keywords19 <- reactive({
    if(input$class == 'NA') {
      paste(class_keys[class_keys$classes == "NA",]$keyword, collapse = " ; ")
    }
    else {
      paste(class_keys[class_keys$classes == "NA",]$keyword, collapse = " ; ")
    }
  })
  
})
