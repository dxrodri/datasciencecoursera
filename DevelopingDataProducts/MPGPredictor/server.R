require(shiny)
require(ggvis)
require(magrittr)
options("scipen"=100, "digits"=4)

data(mtcars)
#Convert factor variables into factors
mtcars$cyl <- as.factor(mtcars$cyl) #4, 6 or 8
mtcars$vs <- as.factor(mtcars$vs) #0- V engine or 1= straight engine. 
mtcars$am <- as.factor(mtcars$am) #0 - automatic and 1 - gear
mtcars$gear <- as.factor(mtcars$gear) #3,4,5
mtcars$carb <- as.factor(mtcars$carb) #1,2,3,4
mymtcars <- data.frame(car=rownames(mtcars), mtcars[,c("mpg","wt", "cyl","am")], row.names=NULL)
mymtcars$car <- as.character(mymtcars$car)
mymtcars$mpg <- as.numeric(mymtcars$mpg)

bestFitWithCylWtAm <- lm(mpg~cyl + wt+am, data=mtcars)

getMpg <- function(wt,am,cyl) {
  newdata <- data.frame(wt=wt,am=as.factor(am),cyl=as.factor(cyl))
  predict(bestFitWithCylWtAm, newdata)[1]
}

shinyServer(function(input, output, session) {
  dataInput <- reactive({
       newmpg <- signif(getMpg(input$wt,input$am,input$cyl),digits=4)
       mymtcars1 <- rbind(mymtcars,c("**** New Car ****",newmpg,input$wt,input$cyl,input$am))
       mymtcars1 <- mymtcars1[order(as.numeric(mymtcars1$mpg)),]
       mymtcars1$desc <- as.character(paste0(ifelse(mymtcars1$am == 0, 'Automatic', 'Manual'), "<br>",
			"Wt: ", data$wt, "<br>", "Cyl: ", data$cyl))
       mymtcars1
 })
  
  dataInput %>% 
  	ggvis(~mpg, ~car, stroke := "black",key := ~desc) %>% 
  	layer_points(shape = ~factor(cyl),fill = ~factor(cyl)) %>% 
	add_tooltip(function(data){
           paste0(data$car,"<br>","Mpg: ",as.character(data$mpg), "<br>", as.character(data$desc))
    }, "hover") %>%
	add_legend(c("shape", "fill")) %>% 
	add_axis("y", title = "") %>% 
	bind_shiny("plot", "plot_ui")
  output$predictedMpg <- renderText(dataInput()[dataInput()$car == "**** New Car ****",]$mpg)
  output$carTable <- renderTable({
    dataInput()[, c("car","wt", "cyl", "am", "mpg")]
  })

 })

# runApp("DP_Assignment1")
