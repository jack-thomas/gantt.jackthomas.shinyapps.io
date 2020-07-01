##############################################################################80
library(ggplot2)
library(magrittr)
library(shiny)

################################################################################
# Rounded Rectangles
## https://github.com/brry/berryFunctions/blob/master/R/roundedRect.R
roundedRectangle <- function(xleft, xright, ybottom, ytop, rounding = 0.25, npoints=200, ...) {
  inset <- rounding * min(xright - xleft, ytop - ybottom)
  
  x_ellipse <- function(from, to) inset * cos(seq(from, to, length.out = npoints / 4))
  y_ellipse <- function(from, to) inset * sin(seq(from, to, length.out = npoints / 4))
  
  xcoord <- c(xright  - inset + x_ellipse(0,      pi/2  ), # Corner 3 (Top Right)
              xleft   - inset + x_ellipse(pi/2,   pi    ), # Corner 2 (Top Left)
              xleft   - inset + x_ellipse(pi,     3*pi/2), # Corner 1 (Bottom Left)
              xright  - inset + x_ellipse(3*pi/2, 2*pi  )) # Corner 4 (Bottom Right)
  
  ycoord <- c(ytop    - inset + y_ellipse(0,      pi/2  ), # Corner 3 (Top Right)
              ytop    - inset + y_ellipse(pi/2,   pi    ), # Corner 2 (Top Left)
              ybottom - inset + y_ellipse(pi,     3*pi/2), # Corner 1 (Bottom Left)
              ybottom - inset + y_ellipse(3*pi/2, 2*pi  )) # Corner 4 (Bottomr Right)
  
  polygon(x = xcoord, y = ycoord, ...)
}

################################################################################
# Shiny UI
ui <- fluidPage(
  titlePanel("Gantt Charts"),
  sidebarPanel(
    width = 2,
    fileInput(
      inputId = "timelineFile",
      label = "Upload XLSX file",
      multiple = FALSE, accept = c(".xlsx")
    ),
    textInput(
      inputId = "textChartName",
      label = "Gantt Chart Name (Optional)",
      value = ""
    ),
    selectInput(
      inputId = "selectTodayLineColor",
      label = "Today Color",
      choices = colors(),
      selected = "red"
    ),
    selectInput(
      inputId = "selectTextLabelLocation",
      label = "Label Text Location",
      selected = "left",
      choices = list(
        "Far Left" = "far_left",
        "Left" = "left",
        "Center" = "center",
        "Right" = "right"
      )
    ),
    sliderInput(
      inputId = "sliderMarginBottom",
      label = "Bottom Margin",
      min = 0, max = 20, value = 2
    ),
    sliderInput(
      inputId = "sliderMarginLeft",
      label = "Left Margin",
      min = 0, max = 20, value = 0
    ),
    sliderInput(
      inputId = "sliderMarginTop",
      label = "Top Margin",
      min = 0, max = 20, value = 3
    ),
    sliderInput(
      inputId = "sliderMarginRight",
      label = "Right Margin",
      min = 0, max = 20, value = 0
    ),
    sliderInput(
      inputId = "sliderTaskHeight",
      label = "Bar Height",
      min = 0, max = 0.7, step = 0.1, value = 0.7
    ),
    sliderInput(
      inputId = "sliderMilestoneWidth",
      label = "Milestone Width",
      min = 0, max = 14, value = 2
    )
  ),
  mainPanel(
    width = 10,
    plotOutput("ganttPlot", width = "100%", height = "800px")
  )
)

################################################################################
# Shiny Server
server <- function(input, output) {
  output$ganttPlot <- renderPlot({
    req(input$timelineFile)
    
    ################################################################################
    # Prepare Data
    # parse uploaded data
    dfTimeline <- input$timelineFile$datapath %>%
      readxl::read_xlsx() %>%
      dplyr::arrange(dplyr::desc(Order)) %>%
      dplyr::mutate(
        Start = as.Date(Start),
        End = as.Date(End)
      )
    
    # filter data, separating tasks from milestones
    dfMilestone <- dfTimeline %>% dplyr::filter(Type == "Milestone")
    dfTimeline %<>% dplyr::filter(Type == "Task")
    
    # generate swimlane data
    dfSwimlane <- dfTimeline %>%
      dplyr::filter(Type == "Task") %>%
      dplyr::mutate(ID = dplyr::row_number()) %>%
      dplyr::filter(Swimlane != "") %>%
      dplyr::group_by(Swimlane)
    dfSwimlaneMin <- dfSwimlane %>%
      dplyr::slice(which.max(Order)) %>%
      dplyr::select(Swimlane, ID) %>%
      dplyr::arrange(ID) %>%
      dplyr::select(MinID = ID)
    dfSwimlaneMax <- dfSwimlane %>%
      dplyr::slice(which.min(Order)) %>%
      dplyr::select(Swimlane, ID) %>%
      dplyr::arrange(ID) %>%
      dplyr::select(MaxID = ID)
    dfSwimlane <- dplyr::full_join(
      dfSwimlaneMin, dfSwimlaneMax
    )
    
    # round to the nearest start/end of the month for the x bounds
    startDate <- dfTimeline$Start %>%
      min() %>%
      lubridate::floor_date(unit = "months")
    endDate <- dfTimeline$End %>%
      max() %>%
      lubridate::ceiling_date(unit = "months")
    
    ################################################################################
    # Draw Plot
    # set up the margins
    par(mar = c(
      input$sliderMarginBottom,
      input$sliderMarginLeft,
      input$sliderMarginTop,
      input$sliderMarginRight
    ))
    
    # build basic plot, mostly focused on arranging the scales
    plot(
      c(1:nrow(dfTimeline)) ~ dfTimeline$Start,
      main = input$textChartName,
      axes = FALSE, xlab = "", ylab = "", pch = NA,
      xlim = c(startDate, endDate),
      ylim = c(1, nrow(dfTimeline) + 2)
    )
    
    # add line for today
    # abline(v = Sys.Date(), lty = 2, col = "red")
    segments(
      x0 = Sys.Date(),
      y0 = 0.5, y1 = nrow(dfTimeline) + 1,
      lty = 2, col = input$selectTodayLineColor
    )
    mtext(
      side = 1, line = -1, col = input$selectTodayLineColor,
      at = Sys.Date(),
      text = format(Sys.Date(), "Today (%b %d)")
    )
    
    # draw swimlane rectangles
    for (i in c(1:nrow(dfSwimlane))) {
      sw_ybottom = dfSwimlane$MinID[i] - 0.1
      sw_ytop    = dfSwimlane$MaxID[i] + input$sliderTaskHeight + 0.1
      
      rect(
        xleft = startDate, xright = endDate,
        ybottom = sw_ybottom, ytop = sw_ytop
      )
      text(
        dfSwimlane$Swimlane[i], adj = 1.5,
        x = endDate, y = sw_ytop - 0.5,
      )
    }
    
    # for each task, add rectangle, segment (if applicable), and label
    # the placement/method of placement of the label is dependent on
    # the "Label Text Location" [selectTextLabelLocation] dropdown
    for (i in c(1:nrow(dfTimeline))) {
      rect(
        xleft = dfTimeline$Start[i],
        xright = dfTimeline$End[i],
        ybottom = i, ytop = i + input$sliderTaskHeight,
        col = dfTimeline$Color[i], border = NA
      )
      if (input$selectTextLabelLocation == "far_left") {
        segments(
          x0 = as.Date("1999-01-01"), x1 = dfTimeline$Start[i],
          y0 = i + (input$sliderTaskHeight / 2),
          lty = 2
        )
      } else if (input$selectTextLabelLocation == "left") {
        text(
          x = dfTimeline$Start[i],
          y = i + (input$sliderTaskHeight / 2),
          labels = dfTimeline$Name[i], adj = 1.05
        )
      } else if (input$selectTextLabelLocation == "center") {
        text(
          x = as.POSIXct((as.numeric(dfTimeline$End[i]) + as.numeric(dfTimeline$Start[i])) / 2, origin = '1970-01-01'),
          y = i + (input$sliderTaskHeight / 2),
          labels = dfTimeline$Name[i], adj = 0.5
        )
      } else if (input$selectTextLabelLocation == "right") {
        text(
          x = dfTimeline$End[i],
          y = i + (input$sliderTaskHeight / 2),
          labels = dfTimeline$Name[i], adj = -0.05
        )
      }
    }
    
    # these labels are drawn on the axis instead of just as text
    if (input$selectTextLabelLocation == "far_left") {
      axis(
        2,        # draw axis at left
        at = c(1:nrow(dfTimeline)) + (input$sliderTaskHeight / 2),
        labels = dfTimeline$Name,
        col = NA, # don't draw axis line or ticks
        las = 1   # label text perpendicular to axis
      )
    }
    
    # add milestones to top axis
    for (i in c(1:nrow(dfMilestone))) {
      mtext(
        text = paste(
          dfMilestone$Name[i],
          format(dfMilestone$Start[i], "(%b %d)"),
          sep = "\n"
        ),
        at = dfMilestone$Start[i],
        side = 3, line = -1.5, las = 1
      )
      polygon(
        y = nrow(dfTimeline) + c(2, 2, 1),
        x = input$sliderMilestoneWidth %>%
          as.integer() %>%
          {c(
            dfMilestone$Start[i] - .,
            dfMilestone$Start[i] + .,
            dfMilestone$Start[i]
          )},
        col = dfMilestone$Color[i],
        border = NA
      )
    }
    
    # add dates to bottom axis
    axis.Date(
      side = 1,
      at = seq(startDate, endDate, "months"),
      labels = format(seq(startDate, endDate, "months"), "%b")
    )
  })
}

################################################################################
# Shiny App
shinyApp(ui, server)
