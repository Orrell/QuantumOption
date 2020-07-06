shinyUI(navbarPage("QuantumOption",
                   tabPanel("Stock",
                            # a(img(src = "SystemsForecastingBanner.png", height = '10%', width = '100%'),
                            #   target="_blank",href="http://www.systemsforecasting.com"),
                            sidebarLayout(
                              sidebarPanel(
                                p('For use with the paper A Quantum Walk Model of Financial Options'),
                                fluidRow(
                                  column(6,
                                         h5('Time points (max 100)')
                                  ),
                                  column(6,
                                         numericInput("nmax", label = NULL, value = 40, step = 10, min = 10, max = 100)
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         h5('Duration in weeks')
                                  ),
                                  column(6,
                                         numericInput("nweeks", label = NULL, value = 26, step = 1) #22
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         h5('Decoherence (0 to 1)')
                                  ),
                                  column(6,
                                         numericInput("decoh", label = NULL, value = 0*0.15, step = 0.05, min=0, max=1) #0.15
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         h5('Ensemble size for decoherence')
                                  ),
                                  column(6,
                                         numericInput("nens", label = NULL, value = 80, step=10, min=0, max=100)  #80
                                  )
                                ),
                                hr(),
                                p('Plot compares stock price density for the quantum and classical models, 
                                  initial price is 1. Volatility determines the horizontal scale, 
                                  risk-free interest rate introduces a horizontal shift. Start angle
                                  controls initial state for quantum coin - set to 0 for down, 90 for
                                  up, and 45 for balanced. Check box to compare benchmark 
                                  JR density (see Notes).'),
                                checkboxInput("showJR", label = "show JR density", value = FALSE),                                p('Check box to switch to unscaled random and 
                                  quantum walks.'),
                                checkboxInput("showwalk", label = "show unscaled walk", value = FALSE),
                                p('The step sizes for the classical and quantum models are scaled to give the correct volatility:'),
                                fluidRow(
                                  column(12,
                                         h5(textOutput('stepsize'))
                                  )
                                ),
                                p('Expected final price:'),
                                fluidRow(
                                  column(12,
                                         h5(textOutput('stockmn'))
                                  )
                                ),
                                fluidRow(
                                  column(6,downloadButton('saveimage1', label='Stock plot'))
                                )
                              ),
                              mainPanel(
                                fluidRow(
                                  plotOutput("stockPlot"),
                                  sliderInput("volyr", "annual volatility",
                                              min = 0, max = 40, value = 20, step=0.1, width='100%'), #19
                                  sliderInput("rfyr", "interest rate",
                                              min = 0, max = 10, value = 2, step=0.1, width='100%'),
                                  sliderInput("startangle", "start angle",
                                              min = 0, max = 90, value = 45, step=1, width='100%'),
                                  sliderInput("rangeAxisStock", "log price range",
                                              min = -1, max = 1, value = c(-1,1), step=0.1, width='100%')
                                  
                                )
                                
                              )
                            )
                   ),
                   tabPanel("Options",
                            shinyUI(fluidPage(
                              
                              sidebarLayout(
                                sidebarPanel(
                                  p('Plot shows option price for classical binomial model (red) and quantum (blue).
                                  Decoherence brings 
                                  quantum results closer to the classical case.'),
                                  p('Check box to show Black-Scholes prices:'),
                                  checkboxInput("blschplot", label = "Black-Scholes", value = FALSE),
                                  # p('Check box to overlay normalised
                                  #   sample data for GOOG 22-week option chain.'),
                                  checkboxInput("optiondata", label = "sample option data", value = FALSE),
                                  hr(),
                                  p('Range of strike prices where quantum price exceeds classical:'),
                                  fluidRow(
                                    column(12,
                                           h5(tableOutput('posrange'))
                                    )
                                  ),
                                  fluidRow(
                                    column(6,downloadButton('saveimage2', label='Option plot'))
                                  )
                                ),
                                mainPanel(
                                  plotOutput("optionPlot"),
                                  #plotOutput("compPlot"),
                                  sliderInput("rangeAxisOption", "strike range",
                                              min = 0, max = 2, value = c(0.6,1.4), step=0.01, width='100%')
                                )
                                
                              )
                            )
                            )
                   ),
                   
                   tabPanel("Implied volatility",
                            shinyUI(fluidPage(
                              
                              sidebarLayout(
                                sidebarPanel(
                                  p('Plot shows implied volatility of the quantum results. The strike price
                                    range shown is one where the quantum and classical prices tend to diverge. 
                                    Most transactions also tend to take place in this region. The curve can be
                                    flattened by adding a degree of decoherence to the quantum model. Note
                                    that the implied volatility is not stable for very high or low strike
                                    prices, where the quantum and classical models tend to be in very close
                                    agreement and are not sensitive to small changes in volatility.'),
                                  p('Check box to overlay normalised
                                    sample data for GOOG 22-week option chain.'),
                                  checkboxInput("vimpdata", label = "sample option data", value = FALSE),
                                  fluidRow(
                                    column(6,downloadButton('saveimage3', label='Vimp plot'))
                                  )
                                ),
                                mainPanel(
                                  #plotOutput("compPlot"),
                                  plotOutput("vimpPlot"),
                                  sliderInput("rangeAxisVimp", "strike range",
                                              min = 0, max = 2, value = c(0.9,1.3), step=0.01, width='100%')
                                )
                                
                              )
                            )
                            )
                   ),
                   tabPanel("Table",
                            shinyUI(fluidPage(
                              fluidRow(
                                column(12,
                                       DT::dataTableOutput('resultsTable')
                                )
                              )
                            )
                            )
                   ),
                   tabPanel("Notes",
                            # a(img(src = "SystemsForecastingBanner.png", height = '10%', width = '100%'),
                            #   target="_blank",href="http://www.systemsforecasting.com"),
                            shinyUI(fluidPage(
                              h4('Description'),
                              p('This program is for use with the paper A Quantum Walk Model of Financial Options,
                                by David Orrell.'),
                              # a("A Quantum Walk Model of Financial Options",target="_blank",
                              #   href="https://papers.ssrn.com/sol3/Papers.cfm?abstract_id=3512481"),
                              p('It can be used to compare call option prices calculated using the classical binomial model
                                and the quantum walk model.'),
                              p('The Stock tab shows the probability density for the stock price. The classical model used 
                              is equivalent to the Jarrow-Rudd (JR) equal probability model.
                              The unscaled random and quantum walks can also be plotted. Decoherence is
                                set by adjusting a parameter as discussed in the paper.'),
                              p('The Options tab shows the option prices for a range of strike prices.
                              Results can be compared with those from the
                                Black-Scholes (BS) model as calculated using the derivmkts R package, 
                                and are shown listed in the Table tab.'), 
                              p('The implied volatility tab shows the implied volatility of the quantum model,
                                defined as the volatility which gives the same price in the classical model.'), 
                              # p('A sample data set for a 22-week GOOG option chain can also
                                # be plotted.'),
                               p('Note that the stock price in the quantum model is not the actual price, but rather
                                the future possible price, as imagined by market participants. Decoherence reflects
                                the partial collapse of this model, due for example to empirical observations of 
                                option prices. Adding decoherence therefore brings the quantum model closer to the 
                              classical model. '),
                              hr(),
                              a("Contact Systems Forecasting",target="_blank",href="http://systemsforecasting.com/contact/"),
                              hr()
                            )
                            )
                   )
                   )
)
