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

consolidate_org_responses_yn <- function(
  data,
  group_col = "organization",
  yes_val = "Yes",
  no_val = "No"
) {
  #' Consolidate Organizational Responses - SATA questions (yes/no)
  #'
  #' Aggregates a data frame by a grouping column (e.g., organization) and creates
  #' a single composite row per group. A column receives an affirmative value ("Yes")
  #' if at least one entry within the group contains that value (case-insensitive);
  #' otherwise, it is assigned the negative value ("No").
  #'
  #' @param data A data frame or tibble containing response columns to aggregate.
  #' @param group_col Character string. The name of the column to group by.
  #'   Default is `"organization"`.
  #' @param yes_val Character string. The affirmative value to search for.
  #'   Default is `"Yes"`.
  #' @param no_val Character string. The fallback value if no affirmative entry exists.
  #'   Default is `"No"`.
  #'
  #' @return A consolidated data frame or tibble with one row per unique group.

  data |>
    group_by(across(all_of(group_col))) %>%
    summarise(
      across(
        everything(),
        ~ if (any(tolower(.x) == tolower(yes_val), na.rm = TRUE)) {
          yes_val
        } else {
          no_val
        }
      ),
      .groups = "drop"
    )
}


consolidate_org_responses_num <- function(
  df,
  org_col = organization,
  na.rm = TRUE
) {
  #' Consolidate Organizational Responses - matrix questions (numeric)
  #'
  #' Groups a data frame or tibble by organization and replaces multiple responses
  #' for the same organization with a single composite row calculated from the average
  #' of each numeric column.
  #'
  #' @param df A data frame or tibble containing survey responses.
  #' @param org_col Column representing the organization grouping variable (quoted or unquoted). Defaults to `organization`.
  #' @param na.rm Logical. Indicates whether missing values (`NA`) should be stripped when computing column means. Defaults to `TRUE`.
  #'
  #' @return A consolidated tibble containing exactly one row per unique organization with averaged numeric values.

  df |>
    dplyr::group_by({{ org_col }}) |>
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = na.rm)),
      .groups = "drop"
    )
}

count_yes_responses <- function(data, yes_val = "Yes", no_val = "No") {
  #' Count Affirmative Responses
  #'
  #' Filters a data frame to retain only columns containing strictly binary yes/no
  #' values, counts occurrences of the affirmative value per column in descending
  #' order, and calculates the total count across all valid columns. Columns containing
  #' non-yes/no entries are automatically ignored.
  #'
  #' @param data A data frame or tibble containing response columns.
  #' @param yes_val Character string. The affirmative response value to count.
  #'   Default is `"Yes"`.
  #' @param no_val Character string. The negative response value used alongside
  #'   `yes_val` to identify valid binary columns. Default is `"No"`.
  #'
  #' @return A tibble with three columns: count, tip, nickname

  # Keep only columns where every entry is either yes_val or no_val
  is_yn_col <- sapply(data, function(col) all(col %in% c(yes_val, no_val)))
  yn_data <- data[, is_yn_col, drop = FALSE]

  # Count affirmative responses and sort descending
  yes_counts <- colSums(yn_data == yes_val)
  yes_counts <- sort(yes_counts, decreasing = TRUE)

  # Convert to tibble, join with codebook, sort, and select columns
  enframe(yes_counts, name = "nickname", value = "count") |>
    left_join(codebook, by = "nickname") |>
    arrange(desc(count)) |>
    select(count, tip, nickname)
}

tally_matrix_vals_by_org <- function(
  df,
  ref_df = codebook,
  count_fn = function(x) sum(x > 0, na.rm = TRUE)
) {
  #' Summarize Matrix Counts and Map Reference Information
  #'
  #' Aggregates numeric responses across organizations for each column,
  #' matches the aggregated counts with a reference table using the column `nickname`,
  #' and returns a tibble with the count, tip description, and nickname.
  #'
  #' @param df A data frame or tibble containing an `organization` column and numeric response columns.
  #' @param ref_df A reference data frame containing at least `nickname` and `tip` columns.
  #' @param count_fn A function defining how counts are calculated per column (e.g., sum of positive values vs. sum of scores).
  #'                 Defaults to counting positive responses (`sum(x > 0, na.rm = TRUE)`).
  #'
  #' @return A tibble with three columns: `count`, `tip`, and `nickname`.

  counts <- df |>
    pivot_longer(
      cols = where(is.numeric),
      names_to = "nickname",
      values_to = "val"
    ) |>
    group_by(nickname) |>
    summarise(
      count = count_fn(val),
      .groups = "drop"
    )

  ref_df %>%
    inner_join(counts, by = "nickname") |>
    arrange(desc(count)) |>
    select(count, tip, nickname)
}
