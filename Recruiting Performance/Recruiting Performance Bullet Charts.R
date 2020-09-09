rm(list = ls(all.names = TRUE))
library(plotly)
Sys.setenv("plotly_username"="Calvinschoice")
Sys.setenv("plotly_api_key"="nlzCIoVJl6H4uWAeCiL9")

bbna_design_last <- 86
bbna_detail_last <- 242
west_design_last <- 37
west_detail_last <- 90
east_design_last <- 34
east_detail_last <- 64
intl_design_last <- 15
intl_detail_last <- 88

bbna_design_thresh <- 91
bbna_detail_thresh <- 259
west_design_thresh <- 48
west_detail_thresh <- 102
east_design_thresh <- 29
east_detail_thresh <- 62
intl_design_thresh <- 14
intl_detail_thresh <- 95

bbna_design_step <- c(0,45.5)
bbna_detail_step <- c(0,129.5)
west_design_step <- c(0,24)
west_detail_step <- c(0,51)
east_design_step <- c(0,14.5)
east_detail_step <- c(0,31)
intl_design_step <- c(0,15)
intl_detail_step <- c(0,47.5)

bbna_design_axis <- list(NULL,101)
bbna_detail_axis <- list(NULL,269)
west_design_axis <- list(NULL,58)
west_detail_axis <- list(NULL,112)
east_design_axis <- list(NULL,39)
east_detail_axis <- list(NULL,72)
intl_design_axis <- list(NULL,24)
intl_detail_axis <- list(NULL,105)

bbna_design_current <- 88
bbna_detail_current <- 238
west_design_current <- 38
west_detail_current <- 87
east_design_current <- 35
east_detail_current <- 66
intl_design_current <- 15
intl_detail_current <- 85

#-----------------
## creating design charts first
p <- plot_ly() %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 15,
    delta = list(reference = 14),
    domain = list(x = c(0.25, 1), y = c(0.08, 0.25)),
    title =list(text = "Intl"),
    gauge = list(
      shape = "bullet",
      axis = list(range = intl_design_axis),
      threshold = list(
        line= list(color = "red", width = 2),
        thickness = 0.75,
        value = intl_design_thresh),
      steps = list(
        list(range = c(0, 5), color = "gray"),
        list(range = c(5, 15), color = "lightgray")),
      bar = list(color = "#ffffff"))) %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 35,
    delta = list(reference = 200),
    domain = list(x = c(0.25, 1), y = c(0.4, 0.6)),
    title = list(text = "Profit"),
    gauge = list(
      shape = "bullet",
      axis = list(range = list(NULL, 100)),
      threshold = list(
        line = list(color = "black", width= 2),
        thickness = 0.75,
        value = 50),
      steps = list(
        list(range = c(0, 25), color = "gray"),
        list(range = c(25, 75), color = "lightgray")),
      bar = list(color = "black"))) %>%
  add_trace(
    type =  "indicator",
    mode = "number+gauge+delta",
    value = 220,
    delta = list(reference = 300 ),
    domain = list(x = c(0.25, 1), y = c(0.7, 0.9)),
    title = list(text = "Satisfaction"),
    gauge = list(
      shape = "bullet",
      axis = list(range = list(NULL, 300)),
      threshold = list(
        line = list(color = "black", width = 2),
        thickness = 0.75,
        value = 210),
      steps = list(
        list(range = c(0, 100), color = "gray"),
        list(range = c(100, 250), color = "lightgray")),
      bar = list(color = "black")))

p

#-----------------

bullet_chrt <- plot_ly(
  type = "indicator",
  mode = "number+gauge+delta",
  value = 88,
  title = list(
    text = "<b>BBNA</b><br><span style='color: gray; font-size:0.8em'>Design</span>",
    font = list(size = 14)),
  domain = list(x = c(0,1), y = c(0,1)),
  delta = list(reference = 86, position = "top"),
  gauge = list(
    shape = "bullet",
    axis = list(range = c(0,101)),
    threshold = list(
      line = list(color = "#fc0000"),
      value = 91)))
bullet_chrt

#------------------

bullet_chrt_design <- plot_ly() %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 88,
    title = list(
      text = "<b>BBNA</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.75,0.85)),
    delta = list(reference = 87, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,101)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 91))) %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 38,
    title = list(
      text = "<b>West</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.55,0.65)),
    delta = list(reference = 37, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,58)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 48))) %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 35,
    title = list(
      text = "<b>East</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.35,0.45)),
    delta = list(reference = 35, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,39)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 29))) %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 15,
    title = list(
      text = "<b>Intl</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.15,0.25)),
    delta = list(reference = 15, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,24)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 14))) %>%
  layout(title = "<br><b>Design</b>")

bullet_chrt_design

plotly_IMAGE(bullet_chrt_design, width = 700, height = 700, format = "png",out_file = "~/R/R Data/Engineering/MBR Charts/Charts/headcount_design.png")

#----------------------

bullet_chrt_detail <- plot_ly() %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 238,
    title = list(
      text = "<b>BBNA</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.75,0.85)),
    delta = list(reference = 243, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,269)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 259))) %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 90,
    title = list(
      text = "<b>West</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.55,0.65)),
    delta = list(reference = 87, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,112)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 102))) %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 63,
    title = list(
      text = "<b>East</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.35,0.45)),
    delta = list(reference = 66, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,72)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 62))) %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 85,
    title = list(
      text = "<b>Intl</b>",
      font = list(size = 14)),
    domain = list(x = c(0,1), y = c(0.15,0.25)),
    delta = list(reference = 90, position = "top"),
    gauge = list(
      bar = list(color = "darkblue"),
      shape = "bullet",
      axis = list(range = c(0,105)),
      threshold = list(
        line = list(color = "#fc0000"),
        value = 95))) %>%
  layout(title = "<br><b>Detailing</b>")

bullet_chrt_detail
plotly_IMAGE(bullet_chrt_detail, width = 700, height = 700, format = "png",out_file = "~/R/R Data/Engineering/MBR Charts/Charts/headcount_detail.png")

# x = c(0,1), y = c(0,1)
# started with doing one basic bullet chart in plotly. All values are input explicitly before figuring out how to create from a tibble or df.
# when I created a combined visual for all regional design, I didn't update the domain, so all were layered on top of each other. 
# I somewhat expected this result but wanted to verify before changing. Trying to use the row call within domain