library(shiny)
library(ggtern)
library(gtools)

ui <- shinyUI(fluidPage(
	titlePanel("Dirichlet Density"),
	sidebarLayout(
		sidebarPanel(
			sliderInput("a1", "a1:", min = 0.01, max = 50, value = 1),
			sliderInput("a2", "a2:", min = 0.01, max = 50, value = 1),
			sliderInput("a3", "a3:", min = 0.01, max = 50, value = 1)
		),
		mainPanel(
			plotOutput("densityPlot", height = 300)
		)
	)
))

x <- seq(0, 1, 0.01)
y <- seq(0, 1, 0.01)
grid <- expand.grid(x = x, y = y)
grid <- grid[grid$x + grid$y < 1,]
grid$z <- 1 - grid$x - grid$y

server <- shinyServer(function(input, output) {
	output$densityPlot <- renderPlot({
		a <- c(input$a1, input$a2, input$a3)
		dat <- grid
		dat$density <- ddirichlet(as.matrix(grid), a)

		g <- ggtern(dat, aes(x,y,z)) +
			geom_point(aes(colour=density)) +
			scale_colour_gradient(low='yellow', high='red') +
			theme_hidetitles() +
			theme_showarrows()
		print(g)
	})
})

shinyApp(ui = ui, server = server)
