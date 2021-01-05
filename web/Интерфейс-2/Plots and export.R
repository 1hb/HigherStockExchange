# Function

Plot_and_export <- function(data, line_chart, export_csv, export_xlsx) {
  
  # Line chart
  
  if (line_chart == T) {
    print(data %>%
            ggplot(aes(x = HM, y = CLOSE)) +
            geom_line(colour="lightblue", size = 1) +
            geom_point(colour="blue", size = 2) +
            labs(title = data$SECCODE[1], y = "Closing Price", x = "") +
            theme_bw())
  }
  
  # Export csv
  
  if (export_csv == T) {
    write.csv(data, 'Quotes.csv', row.names = FALSE)
  }
  
  # Export xlsx
  
  if (export_xlsx == T) {
    write.csv(data, 'Quotes.xlsx', row.names = FALSE)
  }
}

# Results: plot will be shown, exported data will be saved in working directory

Plot_and_export(data = Quotes_result, line_chart = T, export_csv = F, export_xlsx = F)
