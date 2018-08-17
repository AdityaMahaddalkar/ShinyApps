library(shiny)
library(ggplot2)
dataset = read.csv('NSE-ICICI500.csv')
dataset$Date = as.Date(dataset$Date)

## Create a model

library(caTools)

split = sample.split(dataset$Open, SplitRatio = 0.75)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)


rg_model = lm(formula = High ~ Date, data = training_set )

## Plot
train_plot <- ggplot()+
  geom_point(aes(x = training_set$Date, y = training_set$Open),
             colour = 'red') +
  geom_line(aes(x = training_set$Date, training_set$Open),
            colour = 'blue')+
  ggtitle('ICICI 500 NSE') +
  xlab('Date') + 
  ylab('Rupees')

test_plot <- ggplot()+
  geom_point(aes(x = test_set$Date, y = test_set$Open),
            colour = 'red') +
  geom_line(aes(x = test_set$Date, y = predict(rg_model, newdata = test_set)),
            colour = 'blue') +
  ggtitle('ICICI 500 NSE') +
  xlab('Date') + 
  ylab('Rupees')


ui <- fluidPage(
  actionButton(inputId = 'training_set' ,
               label = 'Training Set') ,
  actionButton(inputId = 'test_set',
               label = 'Test Set') ,
  plotOutput(outputId = 'plot',
             width = '800px',
             height = '600px')
)

server <- function(input, output) {

  train_event <- observeEvent(input$training_set, {output$plot <- renderPlot(expr = train_plot)})
  test_event <- observeEvent(input$test_set, {output$plot <- renderPlot(expr = test_plot)})
}

# Run the application 
shinyApp(ui = ui, server = server)

