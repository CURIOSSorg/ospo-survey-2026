write_df_to_file <- function(
  df,
  filename,
  path_key = "data_path",
  cfg_obj = cfg
) {
  #' Write data frame to a tsv file
  #'
  #' @description
  #' Exports a data frame as a plain, tab-delimited text file.
  #' By default, it is saved in the directory specified by the `data_path`
  #' variable in config.yml. Specify a different config variable
  #' to change the output path.
  #' Include .tsv extension when specifying filename.
  #'
  #' @param df A data frame to be written to file.
  #' @param filename A character string specifying the destination file name.
  #'
  #' @return `NULL` (called for its side effect of writing a file).
  # Validate that the specified key exists in the configuration

  if (!path_key %in% names(cfg_obj)) {
    stop(sprintf("Path key '%s' not found in configuration.", path_key))
  }

  write.table(
    df,
    file.path(cfg[[path_key]], filename),
    quote = FALSE,
    row.names = FALSE,
    sep = "\t"
  )
}

remove_uninformative_rows <- function(df) {
  #' Remove Uninformative Rows
  #'
  #' Removes rows that contain either an NA object, an empty string, or a
  #' whitespace string ("  ") in every column.
  #'
  #' @param df Data frame to process.
  #' @return A copy of the data frame with uninformative rows removed.
  df |>
    filter(if_any(everything(), function(x) {
      val <- trimws(x) # trim whitespace
      !is.na(val) & val != ""
    }))
}
