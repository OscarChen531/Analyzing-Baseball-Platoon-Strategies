library(readr)
library('summarytools')

df <- read_csv("Final Project/data/statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv")
str(df)


st_options(
  dfSummary.custom.1 = 
    expression(
      paste(
        "Q1 - Q3 :",
        round(
          quantile(column_data, probs = .25, type = 2, 
                   names = FALSE, na.rm = TRUE), digits = 1
        ), " - ",
        round(
          quantile(column_data, probs = .75, type = 2, 
                   names = FALSE, na.rm = TRUE), digits = 1
        )
      )
    )
)

print(
  dfSummary(df, 
            varnumbers   = FALSE,
            na.col       = FALSE,
            style        = "multiline",
            plain.ascii  = FALSE,
            headings     = FALSE,
            graph.magnif = .8),
  method = "render"
)