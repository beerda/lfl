.lcutServer <- function(input, output, session) {
    observeEvent(input$contextType, {
        updateTabsetPanel(session, 'contextValues', selected=input$contextType)
    })

    output$plot <- renderPlot({
        ctx <- NULL
        if (input$contextType == 'ctx3') {
            ctx <- ctx3(input$context3Low, input$context3Center, input$context3High)
        } else if (input$contextType == 'ctx5') {
            ctx <- ctx5(input$context5Low, input$context5LowerCenter, input$context5Center, input$context5UpperCenter, input$context5High)
        } else if (input$contextType == 'ctx3bilat') {
            ctx <- ctx3bilat(input$context3bNegMax, input$context3bNegCenter, input$context3bOrigin, input$context3bCenter, input$context3bMax)
        } else if (input$contextType == 'ctx5bilat') {
            ctx <- ctx5bilat(input$context5bNegMax, input$context5bNegUpperCenter, input$context5bNegCenter, input$context5bNegLowerCenter,
                input$context5bOrigin, input$context5bLowerCenter, input$context5bCenter, input$context5bUpperCenter, input$context5bMax)
        }

        v <- seq(from=head(ctx, 1), to=tail(ctx, 1), length.out=1000)
        d <- lcut(v, context=ctx, name='', atomic=input$atomic, hedges=input$hedges)
        colnames(d) <- substr(colnames(d), 1, nchar(colnames(d)) - 1)
        d <- as.data.frame(d)
        d$x <- v
        d <- pivot_longer(d, -x, names_to='expression', values_to='y')
        ggplot(d) +
            geom_line(aes(x=x, y=y, color=expression)) +
            xlab('') + ylab('') +
            theme(legend.position='bottom')
    })
}


#' @export
#' @importFrom tidyr pivot_longer
.demoLcut <- function() {
    shinyApp(
        ui=fluidPage(
            titlePanel('lcut demo'),
            sidebarLayout(
                div(class='col-sm-4',
                    tabsetPanel(
                        tabPanel('context', wellPanel(
                            selectInput('contextType', 'type', c('ctx3', 'ctx3bilat', 'ctx5', 'ctx5bilat')),
                            tabsetPanel(id='contextValues', type='hidden',
                                tabPanel('ctx3',
                                    numericInput('context3Low', 'low', value=0.0),
                                    numericInput('context3Center', 'center', value=0.5),
                                    numericInput('context3High', 'high', value=1.0)
                                ),
                                tabPanel('ctx3bilat',
                                    numericInput('context3bNegMax', 'negMax', value=-1.0),
                                    numericInput('context3bNegCenter', 'negCenter', value=-0.5),
                                    numericInput('context3bOrigin', 'origin', value=0.0),
                                    numericInput('context3bCenter', 'center', value=0.5),
                                    numericInput('context3bMax', 'max', value=1.0)
                                ),
                                tabPanel('ctx5',
                                    numericInput('context5Low', 'low', value=0.0),
                                    numericInput('context5LowerCenter', 'lowerCenter', value=0.25),
                                    numericInput('context5Center', 'center', value=0.5),
                                    numericInput('context5UpperCenter', 'upperCenter', value=0.75),
                                    numericInput('context5High', 'high', value=1.0)
                                ),
                                tabPanel('ctx5bilat',
                                    numericInput('context5bNegMax', 'negMax', value=-1.0),
                                    numericInput('context5bNegUpperCenter', 'negUpperCenter', value=-0.75),
                                    numericInput('context5bNegCenter', 'negCenter', value=-0.5),
                                    numericInput('context5bNegLowerCenter', 'negLowerCenter', value=-0.25),
                                    numericInput('context5bOrigin', 'origin', value=0.0),
                                    numericInput('context5bLowerCenter', 'lowerCenter', value=0.25),
                                    numericInput('context5bCenter', 'center', value=0.5),
                                    numericInput('context5bUpperCenter', 'upperCenter', value=0.75),
                                    numericInput('context5bMax', 'max', value=1.0)
                                )
                            )
                        )),
                        tabPanel('atomic', wellPanel(
                            checkboxGroupInput('atomic', 'atomic expressions',
                                choices= c('sm', 'me', 'bi', 'lm', 'um', 'ze', 'neg.sm', 'neg.me', 'neg.bi', 'neg.lm', 'neg.um'))
                        )),
                        tabPanel('hedges', wellPanel(
                            checkboxGroupInput('hedges', 'hedges',
                                choices=c('ex', 'si', 've', 'ty', '-', 'ml', 'ro', 'qr', 'vr'))
                        ))
                    )
                ),
                mainPanel(plotOutput('plot'))
            )
        ),
        server=.lcutServer)
}
