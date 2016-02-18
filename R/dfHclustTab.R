
dfHclust = function(df) {
# validate input
   stopifnot(inherits(df, "data.frame"))
   stopifnot(ncol(df)>1)
# obtain software
   require(shiny)
   require(cluster)
# global variables ... 
   nms = names(df)
   cmeths = c("ward.D", "ward.D2",
             "single", "complete", "average", "mcquitty",
             "median", "centroid")
   dmeths = c("euclidean", "maximum", "manhattan", "canberra",
             "binary")
#
# main shiny components: ui and server
#   ui: defines page layout and components
#   server: defines operations
#
   ui <- fluidPage(
#
# we will have four components on sidebar: selectors for 
# distance, agglomeration method, height for tree cut, and variables to use
#
     titlePanel(paste(substitute(df), "hclust")),
     sidebarPanel(
          helpText(paste("Select distance:" )),
          fluidRow(
             selectInput("dmeth", NULL, choices=dmeths,
               selected=dmeths[1])),
          helpText(paste("Select clustering method:" )),
          fluidRow(
             selectInput("meth", NULL, choices=cmeths,
               selected=cmeths[1])),
          helpText(paste("Select height for cut:" )),
          fluidRow(
             numericInput("cutval", NULL, value=40, min=0, max=Inf, step=1)),
          helpText(paste("Select variables for clustering from", substitute(df), ":" )),
          fluidRow(
             checkboxGroupInput("vars", NULL, choices=nms,
               selected=nms[1:2]))
            ),
#
# main panel is a simple plot
#
     mainPanel(
       tabsetPanel(
        tabPanel("tree", 
         plotOutput("plot1")),
        tabPanel("pairs", 
         plotOutput("pairsplot")),
        tabPanel("silh", 
         plotOutput("silplot"))
         )
       )
  )  # end fluidPage
   
#
# server computes distance, then hclust and then plots dendrogram
# renderPlot makes it reactive, so when input components are altered,
# data frame in use and plot are updated
#
   server <- function(input, output) {
     data(mtcars)
     output$plot1 <- renderPlot({
       xv = df[,input$vars]
       plot(hclust(dist(data.matrix(xv),method=input$dmeth), method=input$meth),
         xlab=paste(input$dmeth, "distance;", input$meth, "clustering"))
       abline(h=input$cutval, lty=2, col="gray")
     })
     output$pairsplot <- renderPlot({
       xv = df[,input$vars]
       pairs(data.matrix(xv))
     })
     output$silplot <- renderPlot({
       xv = df[,input$vars]
       dm = dist(data.matrix(xv),method=input$dmeth)
       hc = hclust(dist(data.matrix(xv),method=input$dmeth), method=input$meth)
       ct = cutree(hc, h=input$cutval)
       plot(silhouette(ct, dm))
     })
   }
   
   shinyApp(ui, server)
}
