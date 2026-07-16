library(readr)

generate_schema <- function(data_path) {
  data <- read_csv(data_path)
  saveRDS(spec(data), file = "schema.rds")
}
