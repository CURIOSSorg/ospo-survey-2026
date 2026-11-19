#' write_df_to_file: Write data frame to a tsv file
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
write_df_to_file <- function(
  df,
  filename,
  path_key = "data_path",
  cfg_obj = cfg
) {
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

#' remove_uninformative_rows: Remove Uninformative Rows
#'
#' Removes rows that contain either an NA object, an empty string, or a
#' whitespace string ("  ") in every column.
#'
#' @param df Data frame to process.
#' @return A copy of the data frame with uninformative rows removed.
remove_uninformative_rows <- function(df) {
  df |>
    filter(if_any(everything(), function(x) {
      val <- trimws(x) # trim whitespace
      !is.na(val) & val != ""
    }))
}


#' count_yes_responses: Count Affirmative Responses
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
count_yes_responses <- function(data, yes_val = "Yes", no_val = "No") {
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

#' consolidate_org_responses_yn: Consolidate Responses from SATA questions by Organization
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
consolidate_org_responses_yn <- function(
  data,
  group_col = "organization",
  yes_val = "Yes",
  no_val = "No"
) {
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

#' consolidate_org_responses_num: Consolidate Responses from Matrix Questions (numeric) by Organization
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
consolidate_org_responses_num <- function(
  df,
  org_col = organization,
  na.rm = TRUE
) {
  df |>
    dplyr::group_by({{ org_col }}) |>
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = na.rm)),
      .groups = "drop"
    )
}

#' tally_matrix_vals_by_org: Summarize Matrix Counts and Map Reference Information
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
tally_matrix_vals_by_org <- function(
  df,
  ref_df = codebook,
  count_fn = function(x) sum(x > 0, na.rm = TRUE)
) {
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


################ Utils for plotting ################

COLORS <- c(
  "#00817f",
  "#42F2FF",
  "#060606",
  "#F7D00B",
  "#6A1B70",
  "#F27405",
  "#D9A00E",
  "#124B5C"
)

# Re-order factor levels in one column by values in another.
# Returns the original data frame with factor_col re-ordered based on value_col.
# Leave column names unquoted.
reorder_factor_by_column <- function(
  df,
  factor_col,
  value_col,
  descending = TRUE
) {
  df <- df %>%
    mutate(
      {{ factor_col }} := fct_reorder(
        # double-curlys and := assign a col by name using tidy evaluation
        .f = {{ factor_col }},
        .x = {{ value_col }},
        .fun = if (descending) `-` else identity # If descending = TRUE, add a negative sign (-) to reverse the sort
      )
    )
  return(df)
}


basic_bar_chart <- function(
  df,
  x_var,
  y_var,
  title,
  ylabel = "Number of respondents",
  show_axis_title_x = FALSE,
  show_axis_title_y = TRUE,
  axis_title_size_x = 11,
  axis_title_size_y = 11,
  axis_text_size_x = 10,
  axis_text_size_y = 10,
  title_size = 14,
  axis_text_angle_x = 45,
  color_index = 1,
  horizontal = FALSE,
  show_ticks_x = FALSE,
  show_ticks_y = TRUE,
  show_bar_labels = FALSE,
  label_position = c("inside", "above"),
  label_color = "white",
  label_size = 8,
  show_grid = TRUE,
  percent = FALSE,
  plot_title_margin_b = 15,
  x_axis_title_margin_t = 15,
  margin_vals = c(0.3, 0.3, 0.3, 0.3)
) {
  label_position <- match.arg(label_position)

  # Axis title settings
  axis_title_x <- if (show_axis_title_x) {
    element_text(
      size = axis_title_size_x,
      margin = margin(t = x_axis_title_margin_t)
    )
  } else {
    element_blank()
  }
  axis_title_y <- if (show_axis_title_y) {
    element_text(
      size = axis_title_size_y,
      margin = margin(r = 15)
    )
  } else {
    element_blank()
  }

  # Axis tick settings
  axis_ticks_x <- if (show_ticks_x) element_line() else element_blank()
  axis_ticks_y <- if (show_ticks_y) element_line() else element_blank()

  # Build the plot
  p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_bar(stat = "identity", fill = COLORS[[color_index]]) +
    ggtitle(title) +
    labs(y = ifelse(is.null(ylabel), "Number of Respondents", ylabel)) +
    theme(
      axis.title.x = axis_title_x,
      axis.title.y = axis_title_y,
      axis.text.x = element_text(
        angle = axis_text_angle_x,
        hjust = 1,
        vjust = 1,
        size = axis_text_size_x
      ),
      axis.text.y = element_text(size = axis_text_size_y),
      axis.ticks.x = axis_ticks_x,
      axis.ticks.y = axis_ticks_y,
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid = if (show_grid) {
        element_line(linetype = "solid", color = "gray90")
      } else {
        element_blank()
      },
      plot.title = element_text(
        hjust = 0,
        face = "bold",
        size = title_size,
        margin = margin(b = plot_title_margin_b)
      ),
      plot.margin = unit(margin_vals, "cm")
    )

  # Convert y-axis values from proportion to percent (optional)
  if (percent) {
    p <- p +
      scale_y_continuous(labels = scales::percent)
  }

  # Add text labels (optional)
  if (show_bar_labels) {
    if (horizontal) {
      p <- p +
        geom_text(
          aes(
            # if percent=TRUE, format the y‐var as a percent; otherwise just show the raw value
            label = if (percent) {
              scales::percent(.data[[y_var]], accuracy = 1)
              # accuracy says use whole-number percentage, no decimal
            } else {
              .data[[y_var]]
            }
          ),
          color = label_color,
          size = label_size,
          hjust = if (label_position == "inside") 1.2 else -0.1, # shift left of bar end
          vjust = 0.5
        )
    } else {
      # vertical bars
      p <- p +
        geom_text(
          aes(
            # if percent=TRUE, format the y‐var as a percent; otherwise just show the raw value
            label = if (percent) {
              scales::percent(.data[[y_var]], accuracy = 1)
              # accuracy says use whole-number percentage, no decimal
            } else {
              .data[[y_var]]
            }
          ),
          color = label_color,
          size = label_size,
          vjust = if (label_position == "inside") 1.2 else -0.3, # above bar = negative vjust
          hjust = 0.5
        )
    }
  }

  # Flip coordinates if horizontal
  if (horizontal) {
    p <- p +
      coord_flip() +
      theme(
        axis.text.y = element_text(
          margin = margin(r = 8),
          vjust = 0.5
        )
      )
  }

  return(p)
}
