library(ggvis)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
### library(RSQLite)
### library(RSQLite.extfuns)
#library(data.table)

# Set up handles to database tables on app start
### db <- src_sqlite("movies.db")
### omdb <- tbl(db, "omdb")
### tomatoes <- tbl(db, "tomatoes")



#header = FALSE, skip = 1,


columns <- c('Country' = "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG")
all_movies <- read.csv("GovBal.csv", header = FALSE, skip = 2, col.names = columns, stringsAsFactors = FALSE) %>%
  select(A,B,C,D,E,F,G,H,K,L,M,N,O,P,Q,S,U,V,W,Z,AF,AG)

HM <- read.csv("HeatMap.csv" ,sep=",")

#HM <- HM[,2:5]

#row.names(HM) <- HM$Name



# HM_heatmap <- heatmap(HM_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column",margins=c(2,2))



shinyServer(function(input, output, session) {
  
  
  
  
  
  # Filter the movies, returning a data frame
  movies <- reactive({
    minrank <- input$rank[1]
    maxrank <- input$rank[2]
    mtable <- input$mtable
    #     minranks <- input$ranks[1]
    #     maxranks <- input$ranks[2]
    year <- input$year
    
    spread5 <- input$spread5
#    spread10 <- input$spread10
    minspread5 <- input$spread5[1]
    maxspread5 <- input$spread5[2]
#    minspread10 <- input$spread10[1]
 #   maxspread10 <- input$spread10[2]
    
    #         minspread <- input$spread[1]
    #         maxspread <- input$spread[2]
    
    
    
    me <- all_movies  %>%
      filter(
        N >= minrank,
        N <= maxrank,
        G == year,
        E >= minspread5,
        E <= maxspread5
 #       F >= minspread10,
  #      F <= maxspread10
        #         S >= minranks,
        #         S <= maxranks
      )%>%
      arrange(E)
    
    
    
    if (input$rate != "All") {
      rate <- paste0("%", input$rate, "%")
      rate <- rate
      me <- me %>% filter(M == input$rate)
    }    
    
    #     year <- paste0("%", input$year, "%")
    #     year <- year
    #     me <- me %>% filter(D == input$year)
    
    
    if (input$genre != "All") {
      genre <- paste0("%", input$genre, "%")
      genre <- genre
      me <- me %>% filter(B == input$genre)
    }
    
    if (input$region != "All") {
      region <- paste0("%", input$region, "%")
      region <- region
      me <- me %>% filter(C == input$region)
    }
    
    # 
    # if (input$subregion != "All") {
    #   subregion <- paste0("%", input$subregion, "%")
    #   subregion <- subregion
    #   me <- me %>% filter(D == input$subregion & C == input$region )
    # }
    
    #updateSelectInput(subregion, "inSelect", choices = s_options)
    # 
    #   if (input$year != "All") {
    #     year <- paste0("%", input$year, "%")
    #     year <- year
    #     me <- me %>% filter(G == 2015)
    #   }
    # 
    #     movies2 <- reactive(
    #       year <- input$year
    #     )
    #     
    
    me <- as.data.frame(me)
    
  })
  
  
  # Function for generating tooltip text
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$A)) return(NULL)
    
    
    # Pick out the movie with this ID
    all_movies <- isolate(movies())
    
    movie <- all_movies[all_movies$A == x$A, ]
    #  movies2 <- all_movies[all_movies$A == x$A, ]
    paste0("<b>", movie$A, "</b><br>",
           movie$M, "<br>", "<b>"
    )
    
    
  }           
  
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    if (is.null(input$xvar)) return(NULL)
    if (is.null(input$yvar)) return(NULL)
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    movies %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~B, key := ~A) %>%
      add_tooltip(movie_tooltip, "hover") %>%
      add_axis("x", title = paste(xvar_name, "% of GDP"), subdivide = 0) %>%
      add_axis("y", title = yvar_name, subdivide = 0) %>%
      add_legend( "stroke", title = "Index", values = c("Advanced Economies", "Emerging Market","Developing Countries"))%>%
      #      #  add_legend("stroke", title = "Index", values = c("CDS", "LCDS")) %>%
      # add_legend("stroke", title = "Index" ) %>%
      scale_nominal("stroke", range = c( "blue" ,"green", "brown")) %>%
      set_options(width = 600, height = 600)

  })
  
  
  vis %>% bind_shiny("plot1")

HM  %>% ggvis(~Spread5) %>% layer_histograms(width=150, center = 0)%>%
  add_axis("x", title = "Spread 5yr Summary") %>%
  set_options(width = 450, height = 300) %>%  bind_shiny("histoy", title = "Index")  

HM  %>% ggvis(~Debt) %>% layer_histograms(width=150, center = 0)%>%
  add_axis("x", title = "Spread 10yr Summary") %>%
  set_options(width = 300, height = 300) %>%  bind_shiny("histox")  

{
    #  output$m_variables = renderText ({"E = Spread, K = Gross Debt, Q = Current Account Balance, H = Revenue, U = Fiscal Balance, L = Net Debt,
    #                                   P = Inflation Annual CPI, W = Gross National Savings, N = Moody's Rating"})
    #  output$m_movies = renderTable({ (movies()[c("A",input$yvar,input$xvar)])}, options = list(orderClasses = TRUE))
    #    output$m_movies = renderTable({ (movies())})
    #output$m_movies = renderTable({ (movies()[c(1,input$yvar,input$xvar)])})
  }

output$m_movies = renderDataTable({
  library(ggplot2)
  movies()[, input$showcols, drop = FALSE]},options=list(
    paging = FALSE))



output$n_movies <- renderText({ nrow(movies()) })


HM.m <- melt(HM)
output$HM_text = renderText({"          Spread5          Spread10              Revenue                Debt                Balance"})

output$HMPlot <- renderPlot({
  row.names(HM) <- HM$Name
  HM_matrix <- data.matrix(HM)
  base_size <- 6
  HM_matrix <- HM_matrix[,2:6]

  #  p <- heatmap(HM_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column")
  
#output$HM_text = renderText({colnames(HM) ,

# output$HM_text = renderText({col_heads})

p <- heatmap(HM_matrix, Rowv=NA, Colv=NA, col=heat.colors(64), scale='column', margins=c(5,10),labCol="", add.expr = text(x = seq_along(colnames(HM[,2:6])), y=-0.2, srt=45,labels=colnames(HM[,2:6]), xpd=TRUE))

#grid.arrange(p)

})

#HM_heatmap <- heatmap(HM_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column",margins=c(2,2))


})
