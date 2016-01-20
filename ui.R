library(shiny)
library(ggplot2)

# ui.R

shinyUI(
  fluidPage(
      titlePanel("NLP Word Prediction"),
      
      div("This is a simple UI for accepting a phrase from the user and returning the phrase with the next predicted word in the phrase. Please enter your phrase in the box below, and when you are ready, press the Submit button to initiate the prediction. The Output text box displays the predicted word appended to the original phrase. You can edit text or erase text in the box any time to enter a new phrase for prediction. The UI selects an example phrase for prediction during the initial launch for demonstrating the basic input/output mechanism. The UI takes 10 to 15 seconds during initialization to load the training data sets for n-Grams. Please be patient to allow the loading to complete. Please check the Progress Bar on the upper right for initialization progress. The UI is ready to use once the progress bar closes and the output box shows the completion of the sample phrase. At that point, the user can erase the existing text in the input text box to enter new phrase or edit the text before a new submit. Repeat the previous steps for the next prediction. Have fun.",
          style="font-sil6pt; font-weight:bold"),
  
      # helpText(h4("This is a simple UI for accepting a phrase from the user and returning the phrase with the next predicted word in the sentence. Please enter your phrase in the box below, and when you are ready, press the Submit button for prediction. You can edit text or erase text in the box any time to enter a new phrase for prediction. The UI selects an example phrase for prediction during the initial launch for demonstrating the basic input/output mechanism")),
      fluidRow(
        br()
      ),
      
      textInput("var", label = NULL, value = "You're the reason why I smile everyday. Can you follow me please? It would mean the", width = '100%'),
      
      submitButton("Submit"),
      
      fluidRow(
          br(),
          br()
      ),
      
      h4("Output text with prediction"),
      verbatimTextOutput("text1"),
      #textOutput("text1"),
      tags$head(tags$style("#text1{color: white; 
                                   font-size: 12px; 
                                   /* font-weight: bold; */
                                   background-color: darkblue }"))
#       ,
#       tabsetPanel(
#         tabPanel("Rank Predictions", dataTableOutput("rankPreds")),
#         tabPanel("")
#         class = "span2"
#       )
      
  )
)

