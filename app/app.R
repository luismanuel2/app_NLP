#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(keras)
library(tensorflow)
library(tm)
library(ramify)

pred<-function(seed){
  print(seed)
  xh<-unlist(texts_to_sequences(tokenizer, seed))
  print(xh)
  n<-20 - length(xh)
  if(n>0){
    xh<-c( as.integer(runif(n,1,8515)),xh)
  }else if (n<0){
    xh<-tail(xh,20)
  }
  xh<-matrix(xh,1,length(xh))
  yclass<-model %>% predict(xh) %>% argmax()
  outpud<-tokenizer$index_word[yclass]
  
  return(unlist(outpud))
}
model<-load_model_hdf5("model.h5")
tokenizer<-load_text_tokenizer("tokenizer")
ant<-' '
cadena<-NULL
antcad<-' '

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$script('$(document).on("keydown",
                 function (e) {
                 if(e.which == 9) {
                   Shiny.onInputChange("go", new Date());
                 } 
                 });
                '),
  fluidRow(column(width = 4,
                  textOutput("b")), 
           column(width = 2,
                  actionButton("go","Apply"))),
  textAreaInput("a","","",width="100%",
                height ="800px", resize = "none" ))
  
server <-
  function(input,output){
    re <- reactive({
        cad<-input$a
        cad<-tolower(cad)
        cad<-gsub(","," , ",cad)
        cad<-stripWhitespace(cad)
        cadena <<-cad
        pal<-NULL
        if(substr(cad,nchar(cad),nchar(cad))!=' '){
            pal<-cad %>% 
            str_extract_all('[:alpha:]+') %>% 
            unlist()
            pal<-pal[1:length(pal)-1]
        }else {
            pal<-cad %>% 
            str_extract_all('[:alpha:]+') %>% 
            unlist()
        }
        return(pal)
      })
    
    output$b <- renderText({ 
      cad <-paste(re(),collapse = ' ')
      resp<-ant
      if(nchar(cad)>0 & substr(cadena,nchar(cadena),nchar(cadena))==' ' & cad!=antcad){
        resp<-pred(cad)
        ant<<-resp
        antcad<<-cad
      } 
      return(resp)
    })
    
    observeEvent(input$go,{
      updateTextAreaInput(inputId = "a",
                          value = paste0(input$a,paste0(ant,' ')))
    })
    
  }
# Run the application 
shinyApp(ui = ui, server = server)
