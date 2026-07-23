library(readr)

# Identifier columns are kept as character so downstream code treats them as
# join keys, not survey items. census_id (a 7-digit CSD code) would otherwise be
# guessed as a double, land in the app's numeric set, and surface as a bogus
# ordinal variable -- besides losing its meaning as a code.
id_cols <- c("census_id")

# Rebuild schema.rds from the cleaned data that data/clean.R publishes. The app
# forces every read_sheet() column to character and then type_convert()s it with
# this spec, so the spec is what fixes each column's type. Regenerate whenever
# the cleaned data's columns change.
generate_schema <- function(
  data_path = "../../data/clean/cao-2026-clean.csv",
  out_path = "schema.rds"
) {
  sp <- spec(read_csv(data_path, show_col_types = FALSE))
  for (v in intersect(id_cols, names(sp$cols))) {
    sp$cols[[v]] <- col_character()
  }
  saveRDS(sp, file = out_path)
  sp
}

# Rscript scripts/generate-schema.R (from the app dir) regenerates in place.
if (!interactive()) {
  sp <- generate_schema()
  message(sprintf(
    "Wrote schema for %d columns to %s",
    length(sp$cols),
    normalizePath("schema.rds")
  ))
}
