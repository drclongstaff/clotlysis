options(shiny.maxRequestSize = 30 * 1024^2)
# √ersion
Thisvers <- "version 1.2.8" # this line is also in server
Thisapp <- "ClotLysisCL_2019"

function(input, output) {
  # Load data using load_file function
  readData <- reactive({
    inputFile <- input$data
    if (is.null(inputFile)) {
      read.csv("./Data/ClotLysisDoses.csv")
    } else {
      (
        load_file(input$data$name, input$data$datapath, input$sheet) %>%
          as.data.frame()
      )
    }
  })



  # displays the Excel sheet in the settings table
  whichsheet <- reactive({
    input$sheet
  })

  output$whichsheet <- renderUI({
    whichsheet()
  })

  # processed data dealing with background subtraction
  procdat0 <- reactive({
    switch(input$abini,
      "global zero" = readData(),
      # "global zero"=as_tibble(cbind("Time"=readData()[[1]], readData()[,-1]-input$back)),
      "nth absorbance" = map_df(readData()[, -1], ~ BaselineNP(.x, input$arow)) %>% # BaselineNP is a function defined above
        add_column("Time" = readData()[[1]], .before = TRUE),
      "min+offset" = map_df(readData()[, -1], ~ BaselineOff(.x, input$off)) %>%
        add_column("Time" = readData()[[1]], .before = TRUE) # ,
      # "raw data"=readData()
    )
  })

  # For spline fitting and adjusting number of points
  procdat <- reactive({
    switch(input$spln,
      "raw" = procdat0(),
      # "spline"=procdat0()
      "spline" = allSpline(NULL, input$npoints, input$zero, input$trunc, procdat0())
    )
    # use clipr for pasting to clipboard locally only
    # clipr::write_clip(procdat0)
  })

  output$text3 <- renderText({
    # text3 is a header for the results table to show what's been displayed
    paste(names(TabRes()[as.numeric(input$tabRes)]), "for ", input$ini, "%", "of maximum")
  })

  # Whichfile is for identifying the file and size loaded to include in the settings table
  whichfile <- reactive({
    if (is.null(input$data)) {
      return("Data/ClotLysisDoses.csv")
    } else {
      (input$data[, 1:2])
    }
  })

  # whichfile 2 is only the filename when needed
  whichfile2 <- reactive({
    inputFile <- input$data
    if (is.null(inputFile)) {
      return(file.path("Data/ClotLysisDoses.csv"))
    } else {
      (input$data$name)
    }
  })

  # The following operations return file names to go into the ui
  output$which <- renderUI({
    whichfile2()
  })


  output$which1 <- renderUI({
    whichfile()
  })


  output$which2 <- renderUI({
    whichfile2()
  })

  output$whichraw <- renderUI({
    whichfile2()
  })

  output$setfile <- renderUI({
    whichfile()
  })

  # This is to collect the column names without the first column of Time
  var <- reactive({
    colnames(readData()[, -1])
  })

  # This is for tab 2 to select the individual curve
  output$what <- renderUI({
    selectInput("colmnames",
      label = h5("Select a column of absorbance data"),
      choices = var()
    )
  })

  # generates the table of all results with some rounding and re-ordering
  # output$contents<-renderDataTable({
  output$contents <- DT::renderDT({
    TabRes() %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 3))) %>%
      select(c(1:10, 15:16, 11:14))
  })


  ### Table of results
  # There are some functions to do the calculations
  # Then purrr makes the final table of results

  # first min and max for the basics of the curve analysis
  MaxandMin <- function(m, aplate, ini, thresh) {
    minAbs <- switch(input$abini,
      "global zero" = input$back,
      "nth absorbance" = m[input$arow],
      "min+offset" = min(m, na.rm = TRUE) #+input$off
    )
    # This min abs is presented without the offset because it's not used for clot or lys calculations
    # It is used for plotting
    Time <- aplate[[1]]
    firstA <- m[1]
    pointmax <- which.max(m)

    maxA <- max(m, na.rm = TRUE)

    maxT <- Time[which.max(m)] # ,
    changeA <- maxA - minAbs

    # Here list the variables you want to collect
    Aminmax <- c(firstA, minAbs, maxA, changeA, maxT, pointmax)
  }

  # uppity analysis the clotting part
  uppity <- function(u, aplate, ini, thresh) {
    minAbs <- switch(input$abini,
      "global zero" = input$back,
      "nth absorbance" = u[input$arow],
      "min+offset" = min(u, na.rm = TRUE) + input$off
    )
    Time <- aplate[[1]]
    maxAbs <- max(u, na.rm = TRUE)

    pointmax <- which.max(u)
    upTime <- Time[c(1:pointmax)] # vector of time to max
    upAbs <- u[c(1:pointmax)] # vector of absorbances to max
    pcChange <- ini * (maxAbs - minAbs) + minAbs # this minAbs is determined in shiny, may be set or calculated
    startPoint <- which(abs(upAbs - pcChange) == min(abs(upAbs - pcChange)))[1]
    # StartAbs is fitted if abs > threshold, otherwise is closest point
    # This prevents crashing if there are blank wells
    ifelse(max(u) - min(u) < thresh,
      startAbs <- upAbs[startPoint],
      startAbs <- round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x, 3)
    )

    # StartTime is fitted if abs > threshold, otherwise is closest point
    # This prevents crashing if there are blank wells
    ifelse(max(u) - min(u) < thresh,
      startTime <- upTime[startPoint],
      startTime <- round(approx(upAbs, upTime, xout = startAbs, ties = mean)$y, 3)
    )

    # Collect the variables
    upcurve <- c(minAbs, startTime, startAbs, startAbs, startPoint, pointmax)
  }

  # and here we analyse the downside of the curve
  downy <- function(d, aplate, ini, thresh) {
    minAbs <- switch(input$abini,
      "global zero" = input$back,
      "nth absorbance" = d[input$arow],
      "min+offset" = min(d, na.rm = TRUE) + input$off
    )
    
    Time <- aplate[[1]]
    # Need to define how to get the max abs
    maxAbs <- max(d, na.rm = TRUE)

    pointmax <- which.max(d)
    pcChange <- ini * (maxAbs - minAbs) + minAbs
    # ifelse statements are used to differentiate between clotting or clotlysis curves
    # ifelse(d[length(d)]>=pcChange, downTime <- Time, downTime<-Time[-c(1:pointmax)])
    # ifelse(d[length(d)]>=pcChange, downAbs <- d,  downAbs<-d[-c(1:pointmax)] )
    downTime <- Time[-c(1:pointmax)]
    downAbs <- d[-c(1:pointmax)]
    # decaypoint is where set% lysis occurs
    if_else(d[length(d)] >= pcChange, decayPoint <- length(d), decayPoint <- which(abs(downAbs - pcChange) == min(abs(downAbs - pcChange)))[1])
    # end point is where 100% lysis occurs
    if_else(d[length(d)] >= pcChange, endPoint <- length(d), endPoint <- which(downAbs <= minAbs)[1])

    if_else(d[length(d)] >= pcChange, endTime <- Time[length(d)], endTime <- downTime[endPoint])

    if_else(d[length(d)] >= pcChange, lastPoint <- endPoint, lastPoint <- endPoint + pointmax)
    # will crash if lastPoint is NA
    ifelse(is.na(lastPoint), lastPoint <- length(d), lastPoint <- endPoint + pointmax)

    # will this avoid crashes due to endtime = NA
    ifelse(is.na(endTime), endTime <- Time[length(d)], endTime <- endTime)

    # In the simple clotlysis app don't use AUC it can be crashy
    # Was requested to add back Oct30 2024
    AUC <- sum(diff(Time[1:lastPoint]) * (head(d[1:lastPoint], -1) + tail(d[1:lastPoint], -1))) / 2

    if_else(max(d) - min(d) < thresh,
      decayAbs <- downAbs[decayPoint],
      decayAbs <- round(approx(downTime, downAbs, xout = pcChange, ties = mean)$x, 3)
    )

    # StartTime is fitted if abs > threshold, otherwise is closest point
    # This prevents crashing if there are blank wells
    ifelse(max(d) - min(d) < thresh,
      decayTime <- downTime[decayPoint],
      decayTime <- round(approx(downAbs, downTime, xout = decayAbs, ties = mean)$y, 3)
    )

    downcurve <- c(decayAbs, decayTime, decayPoint + pointmax, lastPoint, endTime, AUC)
  }
  
  #Use purrr and imap to complete calculations using functions
  TabRes <- reactive({
    whichPlate <- procdat()
    ini <- input$ini * .01
    thresh <- input$thresh
    args_list <- list(whichPlate, ini = 0.5, thresh = 0.05)
    
    moreRes <- whichPlate[ -1] |>
      imap(~ {
        resultsm <- do.call(MaxandMin,  c(list(.x), args_list))
        resultsu <- do.call(uppity, c(list(.x), args_list))
        resultsd <- do.call(downy, c(list(.x), args_list))
        data.frame(
          Wells = .y,
          min.abs = resultsm[2],
          max.abs = resultsm[3],
          delta.abs = resultsm[4],
          max.time = resultsm[5],
          pointmax = resultsm[6],
          clot.time = resultsu[2],
          clot.abs = resultsu[3],
          startPoint = resultsu[5],
          lys.abs = resultsd[1],
          lys.time = resultsd[2],
          decayPoint = resultsd[3],
          endPoint = resultsd[4],
          end.time = resultsd[5],
          clotTolys.time = resultsd[2] - resultsu[2],
          AUC = resultsd[5]
        )
      }) |>
      list_rbind() |> 
      select(
        Wells, min.abs, clot.time, clot.abs, max.abs, delta.abs, max.time, lys.time, lys.abs,
        clotTolys.time, startPoint, pointmax, decayPoint, endPoint, end.time, AUC
      ) %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 4)))
  })
  
 
  # for multiple and single plots
  plot <- reactive({
    multi_plotFun(procdat(), input$numrows, TabRes())
  })
  output$plot <- renderPlot({
    plot()
  })

  myplot <- reactive({
    one_plotFun(procdat(), input$colmnames, TabRes())
  })
  output$myplot <- renderPlot({
    myplot()
  })

  # table of results
  output$head <- renderTable({
    TabRes()
  })

  # raw or processed data
  output$raw <- renderTable({
    procdat()
  })

  # table of results to match the multiple plots
  plotTabl <- reactive({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded

    RowNum <- input$numrows

    data <- switch(input$tabRes,
      "1" = matrix(TabRes()[, 1], byrow = TRUE, nrow = RowNum),
      "2" = matrix(TabRes()[, 2], byrow = TRUE, nrow = RowNum),
      "3" = matrix(TabRes()[, 3], byrow = TRUE, nrow = RowNum),
      "4" = matrix(TabRes()[, 4], byrow = TRUE, nrow = RowNum),
      "5" = matrix(TabRes()[, 5], byrow = TRUE, nrow = RowNum),
      "6" = matrix(TabRes()[, 6], byrow = TRUE, nrow = RowNum),
      "7" = matrix(TabRes()[, 7], byrow = TRUE, nrow = RowNum),
      "8" = matrix(TabRes()[, 8], byrow = TRUE, nrow = RowNum),
      "9" = matrix(TabRes()[, 9], byrow = TRUE, nrow = RowNum),
      "10" = matrix(TabRes()[, 10], byrow = TRUE, nrow = RowNum),
      "11" = matrix(TabRes()[, 11], byrow = TRUE, nrow = RowNum),
      "12" = matrix(TabRes()[, 12], byrow = TRUE, nrow = RowNum),
      "13" = matrix(TabRes()[, 13], byrow = TRUE, nrow = RowNum),
      "14" = matrix(TabRes()[, 14], byrow = TRUE, nrow = RowNum),
      "15" = matrix(TabRes()[, 15], byrow = TRUE, nrow = RowNum),
      "16" = matrix(TabRes()[, 16], byrow = TRUE, nrow = RowNum),
      # "17"=matrix(TabRes()[,17], byrow=TRUE, nrow=RowNum),
      # "18"=matrix(TabRes()[,18], byrow=TRUE, nrow=RowNum)
    )

    colnames(data) <- as.character(1:(length(data) / RowNum))
    # clipr::write_clip(data)
    data
  })

  output$resultsTable <- renderTable({
    plotTabl()
  })

  # results to go with single plot
  output$curveTable <- renderTable({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded
    TabNames <- colnames(TabRes())
    ColNam <- c("Parameter", "Result")

    All.Res <- TabRes() %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 3))) %>%
      filter(Wells == input$colmnames) %>%
      select(c(1:10, 15:16, 11:14))
    All.Res.Tab <- cbind(TabNames[c(1:10, 15:16, 11:14)], t(All.Res))
    colnames(All.Res.Tab) <- ColNam

    Generation.Res <- TabRes() %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 3))) %>%
      filter(Wells == input$colmnames) %>%
      select(c(1:7, 10))
    Generation.Res.Tab <- cbind(TabNames[c(1:7, 10)], t(Generation.Res))
    colnames(Generation.Res.Tab) <- ColNam

    Decay.Res <- TabRes() %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 3))) %>%
      filter(Wells == input$colmnames) %>%
      select(1, 5:7, 8:10, 15:16)
    Decay.Res.Tab <- cbind(TabNames[c(1, 5:7, 8:10, 15:16)], t(Decay.Res))
    colnames(Decay.Res.Tab) <- ColNam

    curveDat <- switch(input$curveRes,
      "All" = All.Res.Tab,
      "Clotting" = Generation.Res.Tab,
      "Lysis" = Decay.Res.Tab
    )
  })
  # Get the names from results table
  varnames <- reactive({
    mynames <- colnames(TabRes()[c(1:10, 15:16, 11:14)])
  })

  # Find data for graphs in Explore tab
  output$whatx <- renderUI({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded
    selectInput("mynamesx",
      label = h5("On X axis"),
      choices = varnames(), selected = colnames(TabRes()[1])
    )
  })
  # Find data for graphs in Explore tab
  output$whaty <- renderUI({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded
    selectInput("mynamesy",
      label = h5("On Y axis"),
      choices = varnames(), selected = colnames(TabRes()[8])
    )
  })

  # select data for heatmap
  output$whatheat <- renderUI({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded
    selectInput("mynamesheat",
      label = h5("Choose data for heatmap"),
      choices = varnames(), selected = colnames(TabRes()[8])
    )
  })


  # generate the heatmap or scatterplot in the Explore tab
  output$exploreplot <- renderPlotly({
    if (is.null(input$colmnames)) {
      return(NULL)
    } # To stop this section running and producing an error before the data has uploaded
    TabRes_r <- TabRes() %>% mutate(across(where(is.numeric), \(x) round(x, digits = 2)))
    N <- TabRes()[[1]]
    # X<-TabRes()[,input$mynamesx]
    # Y<-TabRes()[,input$mynamesy]
    X <- TabRes_r[, input$mynamesx]
    Y <- TabRes_r[, input$mynamesy]
    Z <- matrix(TabRes()[, input$mynamesheat], byrow = TRUE, nrow = input$numrows)
    switch(input$heat,
      "heat" = plot_ly(z = Z, colors = colorRamp(c("grey", "pink", "red")), type = "heatmap") %>% layout(yaxis = list(autorange = "reversed")),
      "scatter" = plot_ly(type = "scatter", mode = "markers") %>%
        add_trace(x = X, y = Y, text = N, marker = list(size = 10, color = "pink", line = list(color = "red", width = 2)), hoverinfo = N)
    )
  })

  # Collection of setting for records in the settings tab
  setsTab <- reactive({
    # make a matrix 4 columns, 10 rows to show as a table
    setTable <- matrix(
      c(
        Thisapp, Thisvers, "", "",
        "Date", format(Sys.Date(), "%d %b %Y"), "Time", format(Sys.time(), "%X"),
        "Read interval s", readData()[2, 1] - readData()[1, 1], "Chosen % clotting or lysis", input$ini,
        "Threshold value", input$thresh, "", "",
        "Raw or spline fitted", input$spln, "", "",
        "Fit start", input$zero, "Truncate for fit", input$trunc,
        "Read interval with fit", round(procdat()[2, 1] - procdat()[1, 1], 4), "Number of points in fit", input$npoints,
        "Zero method", input$abini, "", "",
        "", "", "Global zero", input$back,
        "", "", "nth point", input$arow,
        "", "", "offset with min abs", input$off
      ),
      byrow = TRUE, nrow = 11
    )

    colnames(setTable) <- c("Parameter", "Value", "Parameter", "Value")
    setTable
  })


  # generate the table
  output$settings <- renderTable({
    if (is.null(input$colmnames)) {
      return(NULL)
    }
    setTable <- setsTab()
    setTable
  })
  
  output$text4 <- renderText({
    This_session[[1]]
  })
  
  output$text5 <- renderText({
    This_session[[2]]
  })
  
  output$session <- renderTable({
    
    # Extract other packages with versions
    other_packages <- session$otherPkgs
    if (!is.null(other_packages)) {
      other_pkg_info <- data.frame(
        Package = names(other_packages),
        Version = sapply(other_packages, function(x) x$Version),
        stringsAsFactors = FALSE
      )
    } else {
      other_pkg_info <- data.frame(Package = character(0), Version = character(0))
    }
  })
  
}
