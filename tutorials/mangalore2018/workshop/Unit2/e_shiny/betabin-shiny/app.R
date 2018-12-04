library(shiny)

source("betabin.R")

ui <- shinyUI(fluidPage(
	titlePanel("Beta-Binomial vs. Binomial"),
	sidebarLayout(
		sidebarPanel(
			sliderInput("m", "m:", min = 1, max = 50, value = 20),
			sliderInput("Pi", "Pi:", min = 0.01, max = 0.99, value = 0.5),
			sliderInput("rho", "rho:", min = 0.01, max = 0.99, value = 0.01)
		),
		mainPanel(
			plotOutput("bbPlot", height = 300),
			plotOutput("binPlot", height = 300)
		)
	)
))

server <- shinyServer(function(input, output) {
	output$bbPlot <- renderPlot({
		m <- input$m
		ff <- d.beta.binom(0:m, Pi = input$Pi, rho = input$rho, m = m)
		barplot(ff, names.arg = 0:m, col = "blue")
		title("BB(m, Pi, rho) Density")
		box()
	})
	output$binPlot <- renderPlot({
		m <- input$m
		ff <- dbinom(0:m, prob = input$Pi, size = m)
		barplot(ff, names.arg = 0:m, , col = "green")
		title("Binomial(m, Pi) Density")
		box()
	})
})

shinyApp(ui = ui, server = server)
