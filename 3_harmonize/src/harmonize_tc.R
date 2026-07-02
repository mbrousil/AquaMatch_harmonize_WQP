
harmonize_tc <- function(raw_tc, p_codes){
  
  # Starting values for dataset
  starting_data <- tibble(
    step = "true_color harmonization",
    reason = "Starting dataset",
    short_reason = "Start",
    number_dropped = 0,
    n_rows = nrow(raw_tc),
    order = 0
  )
  
  # Minor data prep ---------------------------------------------------------
  
  # Produce a bar chart of the current CharacteristicName counts
  stack_chart <- raw_tc %>%
    count(CharacteristicName) %>% 
    # Alpha order
    arrange(desc(CharacteristicName)) %>%
    # Cumulative (i.e., stacked) position of the labels, using midpoint of each
    # bar
    mutate(cume_pos = cumsum(n) - n / 2) %>%
    ggplot(aes(x = 1, y = n)) +
    geom_bar(aes(fill = CharacteristicName),
             color = "black", position = "stack", stat = "identity",
             alpha = 0.85) +
    # Align right
    geom_text_repel(
      aes(
        # Include row counts
        label = paste0(CharacteristicName, ": n = ", comma(n)),
        y = cume_pos),
      hjust = 0,
      force_pull = 0,
      nudge_x = 1,
      min.segment.length = 0.75,
      direction = "y",
      show.legend = FALSE
    ) +
    xlim(c(0, 10)) +
    scale_fill_viridis_d() +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    xlab(NULL) +
    ylab("Cumulative record count") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank())
  
  ggsave(filename = "3_harmonize/out/tc_stacked_char_names.png",
         plot = stack_chart, width = 6.5, height = 7.5, units = "in", device = "png")
  
  # Grab the column names of the dataset coming in
  raw_names <- names(raw_tc)
  
  # First step is to read in the data and do basic formatting and filtering
  tc_narrowed <- raw_tc %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    filter(
      # Filter out non-target media types
      ActivityMediaSubdivisionName %in% c("Surface Water", "Water", "Estuary") |
        is.na(ActivityMediaSubdivisionName)
    ) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index") %>%
    # Remove unrelated pcode (and keep NAs)
    filter(USGSPCode != "70516" | is.na(USGSPCode)) %>%
    # Use the parameter column to reassign based on p-code where necessary.
    mutate(
      parameter = case_when(
        # USGS P Code for True Color
        USGSPCode == "00080" ~ "True color",
        USGSPCode == "00081" ~ "Apparent color",
        .default = CharacteristicName
      )
    )
  
  tc_param_changes <- tc_narrowed %>%
    filter(tc_narrowed$parameter != tc_narrowed$CharacteristicName) %>%
    count(CharacteristicName, parameter) %>%
    rename(CharacteristicName_old = CharacteristicName,
           parameter_new = parameter)
  
  param_change_table_out_path <- "3_harmonize/out/tc_param_changes_table.csv"
  
  write_csv(x = tc_param_changes,
            file = param_change_table_out_path)  
  
  if(any(is.na(tc_narrowed$parameter))){
    cli_abort("Unexpected values generated when classifying parameters by CharacteristicName.")
  }
  
  # Catch up to naming that follows
  tc <- tc_narrowed
  
  # Produce a bar chart of the current parameter counts
  stack_param_chart <- tc %>%
    count(parameter) %>% 
    # Alpha order
    arrange(desc(parameter)) %>%
    # Cumulative (i.e., stacked) position of the labels, using midpoint of each
    # bar
    mutate(cume_pos = cumsum(n) - n / 2) %>%
    ggplot(aes(x = 1, y = n)) +
    geom_bar(aes(fill = parameter),
             color = "black", position = "stack", stat = "identity",
             alpha = 0.85) +
    # Align right
    geom_text_repel(
      aes(
        # Include row counts
        label = paste0(parameter, ": n = ", comma(n)),
        y = cume_pos),
      hjust = 0,
      force_pull = 0,
      nudge_x = 1,
      min.segment.length = 0.75,
      direction = "y",
      show.legend = FALSE
    ) +
    xlim(c(0, 10)) +
    scale_fill_viridis_d() +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    xlab(NULL) +
    ylab("Cumulative record count") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank())
  
  ggsave(filename = "3_harmonize/out/tc_stacked_param_names.png",
         plot = stack_param_chart, width = 6.5, height = 7.5, units = "in", device = "png")
  
  # Record info on any dropped rows  
  dropped_media <- tibble(
    step = "true_color harmonization",
    reason = "Filtered for only specific water media & relevant parameters",
    short_reason = "Target water media & parameters",
    number_dropped = nrow(raw_tc) - nrow(tc),
    n_rows = nrow(tc),
    order = 1
  )
  
  rm(raw_tc, tc_narrowed)
  gc()
  
  
  # Document and remove fail language ---------------------------------------
  
  # The values that will be considered fails for each column:
  fail_text <- c(
    "beyond accept", "cancelled", "contaminat", "error", "fail", "improper",
    "interference", "invalid", "no result", "no test", "not accept",
    "outside of accept", "problem", "questionable", "suspect", "unable",
    "violation", "reject", "no data", "value extrapolated",
    "exceed", "biased", "parameter not required", "not visited", "warm",
    "broken"
  )
  
  # Now get counts of fail-related string detections for each column: 
  fail_counts <- list("ActivityCommentText", "ResultLaboratoryCommentText",
                      "ResultCommentText", "ResultMeasureValue_original",
                      "ResultDetectionConditionText") %>%
    # Set list item names equal to each item in the list so that map will return
    # a named list
    set_names() %>%
    map(
      .x = .,
      .f = ~ {
        # Pass column name into the next map()
        col_name <- .x
        
        # Check each string pattern separately and count instances
        map_df(.x = fail_text,
               .f = ~{
                 hit_count <- tc %>%
                   filter(grepl(pattern = .x,
                                x = !!sym(col_name),
                                ignore.case = TRUE)) %>%
                   nrow()
                 
                 # Return two-col df
                 tibble(
                   word = .x,
                   record_count = hit_count
                 )
               }) %>%
          # Ignore patterns that weren't detected
          filter(record_count > 0)
      }) %>%
    # If there's any data frames with 0 rows (i.e., no fails detected) then
    # drop them to avoid errors in the next step. This has happened with
    # ResultMeasureValue in the past
    keep(~nrow(.) > 0)
  
  
  # Plot and export the plots as png files
  walk2(.x = fail_counts,
        .y = names(fail_counts),
        .f = ~ ggsave(filename = paste0("3_harmonize/out/tc_",
                                        .y,
                                        "_fail_pie.png"),
                      plot = plot_fail_pie(dataset = .x, col_name = .y),
                      width = 6, height = 6, units = "in", device = "png"))
  
  
  # Now that the fails have been documented, remove them:
  tc_fails_removed <- tc %>%
    filter(
      if_all(.cols = c(ActivityCommentText, ResultLaboratoryCommentText,
                       ResultCommentText, ResultMeasureValue_original,
                       ResultDetectionConditionText),
             .fns = ~
               !grepl(
                 pattern = paste0(fail_text, collapse = "|"),
                 x = .x,
                 ignore.case = T
               )))
  
  # How many records removed due to fails language?
  print(
    paste0(
      "Rows removed due to fail-related language: ",
      nrow(tc) - nrow(tc_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows containing fail-related language",
    short_reason = "Fails, etc.",
    number_dropped = nrow(tc) - nrow(tc_fails_removed),
    n_rows = nrow(tc_fails_removed),
    order = 2)
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  non_detect_text <- "non-detect|not detect|non detect|undetect|below|Present <QL"
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- tc_fails_removed %>%
    # only want NAs and character value data:
    filter(is.na(ResultMeasureValue)) %>%
    # if the value is NA BUT there is non detect language in the comments...  
    mutate(
      mdl_vals = ifelse(
        test = (is.na(ResultMeasureValue_original) &
                  (grepl(non_detect_text, ResultLaboratoryCommentText, ignore.case = TRUE) | 
                     grepl(non_detect_text, ResultCommentText, ignore.case = TRUE) |
                     grepl(non_detect_text, ResultDetectionConditionText, ignore.case = TRUE))) |
          #.... OR, there is non-detect language in the value column itself /
          # it's labeled as ND
          (
            grepl(non_detect_text, ResultMeasureValue_original, ignore.case = TRUE) |
              ResultMeasureValue_original == "ND" |
              ResultMeasureValue_original == "nd"
          ),
        #... use the DetectionQuantitationLimitMeasure.MeasureValue value.
        yes = DetectionQuantitationLimitMeasure.MeasureValue,
        # if there is a `<` and a number in the values column...
        no = ifelse(test = grepl("[0-9]", ResultMeasureValue_original) &
                      grepl("<", ResultMeasureValue_original),
                    # ... use that number as the MDL
                    yes = str_replace_all(string = ResultMeasureValue_original,
                                          pattern = c("\\<"="", "\\*" = "", "\\=" = "" )),
                    no = NA)
      ),
      # preserve the units if they are provided:
      mdl_units = ifelse(!is.na(mdl_vals), 
                         DetectionQuantitationLimitMeasure.MeasureUnitCode, 
                         ResultMeasure.MeasureUnitCode),
      half = as.numeric(mdl_vals) / 2)
  
  # Using the EPA standard for non-detects, select a random number between zero and HALF the MDL:
  mdl_updates$std_value <- with(mdl_updates, runif(nrow(mdl_updates), 0, half))
  mdl_updates$std_value[is.nan(mdl_updates$std_value)] <- NA
  
  # Keep important data
  mdl_updates <- mdl_updates %>%
    select(index, std_value, mdl_vals, mdl_units) %>%
    filter(!is.na(std_value))
  
  
  print(
    paste(
      round((nrow(mdl_updates)) / nrow(tc_fails_removed) * 100, 1),
      "% of samples had values listed as being below a detection limit"
    )
  )
  
  # Replace "harmonized_value" field with these new values
  tc_mdls_added <- tc_fails_removed %>%
    left_join(x = ., y = mdl_updates, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% mdl_updates$index, std_value, ResultMeasureValue),
           harmonized_units = ifelse(index %in% mdl_updates$index, mdl_units, ResultMeasure.MeasureUnitCode),
           # Flag: 0 = value not adjusted and MDL not a concern
           #       1 = original NA value adjusted using MDL method
           #       2 = provided value below provided MDL; not adjusted
           mdl_flag = case_when(
             index %in% mdl_updates$index ~ 1,
             (!(index %in% mdl_updates$index) & DetectionQuantitationLimitMeasure.MeasureValue <= ResultMeasureValue) |
               (!(index %in% mdl_updates$index) & is.na(DetectionQuantitationLimitMeasure.MeasureValue)) ~ 0,
             DetectionQuantitationLimitMeasure.MeasureValue > ResultMeasureValue ~ 2,
             .default = 0
           ))
  
  dropped_mdls <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while cleaning MDLs",
    short_reason = "Clean MDLs",
    number_dropped = nrow(tc_fails_removed) - nrow(tc_mdls_added),
    n_rows = nrow(tc_mdls_added),
    order = 3
  )
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  approx_text <- "result approx|RESULT IS APPROX|value approx"
  
  tc_approx <- tc_mdls_added %>%
    # First, remove the samples that we've already approximated using the EPA method:
    filter(!index %in% mdl_updates$index,
           # Then select fields where the numeric value column is NA....
           is.na(ResultMeasureValue) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", ResultMeasureValue_original) &
             # ...AND any of the comment fields have approximation language...
             (grepl(approx_text, ResultLaboratoryCommentText, ignore.case = T)|
                grepl(approx_text, ResultCommentText, ignore.case = T )|
                grepl(approx_text, ResultDetectionConditionText, ignore.case = T)))
  
  tc_approx$approx_value <- as.numeric(str_replace_all(tc_approx$ResultMeasureValue_original, c("\\*" = "")))
  tc_approx$approx_value[is.nan(tc_approx$approx_value)] <- NA
  
  # Keep important data
  tc_approx <- tc_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(tc_approx)) / nrow(tc_mdls_added) * 100, 3),
      "% of samples had values listed as approximated"
    )
  )
  
  # Replace harmonized_value field with these new values
  tc_approx_added <- tc_mdls_added %>%
    left_join(x = ., y = tc_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% tc_approx$index,
                                     approx_value,
                                     harmonized_value),
           # Flag: 1 = used approximate adjustment, 0 = value not adjusted
           approx_flag = ifelse(index %in% tc_approx$index, 1, 0))
  
  dropped_approximates <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while cleaning approximate values",
    short_reason = "Clean approximates",
    number_dropped = nrow(tc_mdls_added) - nrow(tc_approx_added),
    n_rows = nrow(tc_approx_added),
    order = 4
  )
  
  
  # Clean up "greater than" values ------------------------------------------
  
  # Next step, incorporating and flagging "greater than" values. Using a similar
  # approach to the previous two flags, we can identify results that 
  # contain values greater than some amount
  
  greater_vals <- tc_approx_added %>%
    # First, remove the samples that we've already approximated:
    filter((!index %in% mdl_updates$index) & (!index %in% tc_approx$index)) %>%
    # Then select fields where the NUMERIC value column is NA....
    filter(is.na(ResultMeasureValue) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", ResultMeasureValue_original) &
             #... AND a `>` symbol
             grepl(">", ResultMeasureValue_original))
  
  greater_vals$greater_value <- as.numeric(
    str_replace_all(
      greater_vals$ResultMeasureValue_original,
      c("\\>" = "", "\\*" = "", "\\=" = "" )))
  greater_vals$greater_value[is.nan(greater_vals$greater_value)] <- NA
  
  # Keep important data
  greater_vals <- greater_vals %>%
    select(greater_value, index)
  
  print(
    paste(
      round((nrow(greater_vals)) / nrow(tc_approx_added) * 100, 9),
      "% of samples had values listed as being above a detection limit//greater than"
    )
  )
  
  # Replace harmonized_value field with these new values
  tc_harmonized_values <- tc_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           # Flag: 1 = used greater than adjustment, 0 = value not adjusted
           greater_flag = ifelse(index %in% greater_vals$index, 1, 0))
  
  dropped_greater_than <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(tc_approx_added) - nrow(tc_harmonized_values),
    n_rows = nrow(tc_harmonized_values),
    order = 5
  )
  
  # Free up memory
  rm(tc)
  gc()
  
  
  # Remove remaining NAs ----------------------------------------------------
  
  # At this point we've processed MDLs, approximate values, and values containing
  # symbols like ">". If there are still remaining NAs in the numeric measurement
  # column then it's time to drop them, unless they are slopes.
  
  tc_no_na <- tc_harmonized_values %>%
    filter(
      !is.na(harmonized_value),
      # Some negative values can be introduced by the previous NA parsing steps:
      harmonized_value >= 0
    )
  
  dropped_na <- tibble(
    step = "true_color harmonization",
    reason = "Dropped unresolved NAs",
    short_reason = "Unresolved NAs",
    number_dropped = nrow(tc_harmonized_values) - nrow(tc_no_na),
    n_rows = nrow(tc_no_na),
    order = 6
  )
  
  # Free up memory
  rm(tc_harmonized_values, tc_approx_added, tc_mdls_added,
     tc_fails_removed)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Matchup table for expected True Color units in the dataset
  unit_conversion_table <- tribble(
    ~ResultMeasure.MeasureUnitCode, ~conversion,
    # Assume these are the same: Platinum-Cobalt Units (PCU)
    "PCU",                                1,
    "CU",                                 1, 
    "pt",                                 1,
    # Others to keep
    "ADMI value",                         1,
    "None",                               1
  )
  
  # Export a record of unit conversions
  unit_table_out_path <- "3_harmonize/out/tc_unit_table.csv"
  
  write_csv(x = unit_conversion_table,
            file = unit_table_out_path)
  
  # Do the conversion
  converted_units_tc <- tc_no_na %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "ResultMeasure.MeasureUnitCode") %>%
    mutate(
      harmonized_value = harmonized_value * conversion,
      harmonized_units = case_when(
        # Assume these are the same: Platinum-Cobalt Units (PCU)
        ResultMeasure.MeasureUnitCode %in% c("PCU", "CU", "pt") ~ "PCU",
        # ADMI
        ResultMeasure.MeasureUnitCode == "ADMI value" ~ "ADMI value",
        # Other
        ResultMeasure.MeasureUnitCode == "None" ~ "None",
        .default = "Unexpected")
    )
  
  # Check for unexpected units in the "harmonized_units" column
  if ("Unexpected" %in% converted_units_tc$harmonized_units) {
    cli_abort("True color unit harmonization has encountered an {cli::col_red('unexpected unit')}.")
  }
  
  
  # Plot and export unit codes that didn't make it through joining
  tryCatch({
    tc_no_na %>%
      anti_join(x = .,
                y = unit_conversion_table,
                by = "ResultMeasure.MeasureUnitCode")  %>%
      count(ResultMeasure.MeasureUnitCode, name = "record_count") %>%
      plot_unit_pie() %>%
      ggsave(filename = "3_harmonize/out/tc_unit_drop_pie.png",
             plot = .,
             width = 6, height = 6, units = "in", device = "png")
  }, error = function(e) NULL
  )
  
  # How many records removed due to limits on values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(tc_no_na) - nrow(converted_units_tc)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(tc_no_na) - nrow(converted_units_tc),
    n_rows = nrow(converted_units_tc),
    order = 7
  )
  
  
  # Clean and flag depth data -----------------------------------------------
  
  # Recode any error-related character values to NAs
  recode_depth_na_tc <- converted_units_tc %>%
    mutate(
      across(.cols = c(ActivityDepthHeightMeasure.MeasureValue,
                       ResultDepthHeightMeasure.MeasureValue,
                       ActivityTopDepthHeightMeasure.MeasureValue,
                       ActivityBottomDepthHeightMeasure.MeasureValue),
             .fns = ~if_else(condition = .x %in% c("NA", "999", "-999",
                                                   "9999", "-9999", "-99",
                                                   "99", "NaN"),
                             true = NA_character_,
                             false = .x))
    )
  
  # Reference table for unit conversion
  depth_unit_conversion_table <- tibble(
    depth_units = c("in", "ft", "feet", "cm", "m", "meters"),
    depth_conversion = c(0.0254, 0.3048, 0.3048, 0.01, 1, 1)
  )
  
  # There are four columns with potential depth data that we need to convert
  # into meters:
  converted_depth_units_tc <- recode_depth_na_tc %>%
    # 1. Activity depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ActivityDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_activity_depth_value = as.numeric(ActivityDepthHeightMeasure.MeasureValue) * depth_conversion
    ) %>%
    # Drop conversion col to avoid interfering with next join
    select(-depth_conversion) %>%
    # 2. Result depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ResultDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_result_depth_value = as.numeric(ResultDepthHeightMeasure.MeasureValue) * depth_conversion
    ) %>%
    select(-depth_conversion) %>%
    # 3. Activity top depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ActivityTopDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_top_depth_value = as.numeric(ActivityTopDepthHeightMeasure.MeasureValue) * depth_conversion,
      harmonized_top_depth_unit = "m"
    ) %>%
    select(-depth_conversion) %>%
    # 4. Activity bottom depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ActivityBottomDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_bottom_depth_value = as.numeric(ActivityBottomDepthHeightMeasure.MeasureValue) * depth_conversion,
      harmonized_bottom_depth_unit = "m"
    )
  
  # Now combine the two columns with single point depth data into one and clean
  # up values generally:
  harmonized_depth_tc <- converted_depth_units_tc %>%
    rowwise() %>%
    mutate(
      # New harmonized discrete column:
      harmonized_discrete_depth_value = case_when(
        # Use activity depth mainly
        !is.na(harmonized_activity_depth_value) &
          is.na(harmonized_result_depth_value) ~ harmonized_activity_depth_value,
        # Missing activity depth but not result depth
        is.na(harmonized_activity_depth_value) &
          !is.na(harmonized_result_depth_value) ~ harmonized_result_depth_value,
        # Disagreeing activity and result depths
        (!is.na(harmonized_activity_depth_value) &
           !is.na(harmonized_result_depth_value)) &
          harmonized_activity_depth_value != harmonized_result_depth_value ~ mean(
            c(harmonized_activity_depth_value, harmonized_result_depth_value)),
        # Both agree
        harmonized_activity_depth_value == harmonized_result_depth_value ~ harmonized_activity_depth_value,
        # Defaults to NA otherwise
        .default = NA_real_
      ),
      # Indicate depth unit going along with this column
      harmonized_discrete_depth_unit = "m"
    ) %>%
    ungroup()
  
  # Create a flag system based on depth data presence/completion
  flagged_depth_tc <- harmonized_depth_tc %>%
    mutate(
      depth_flag = case_when(
        # No depths (including because of recoding above)
        is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value) ~ 0,
        # All columns present
        !is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value) ~ 3,
        # Integrated depths
        (!is.na(harmonized_top_depth_value) |
           !is.na(harmonized_bottom_depth_value)) &
          is.na(harmonized_discrete_depth_value) ~ 2,
        # Discrete depths
        !is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value) ~ 1,
        # Discrete and integrated present
        # Note that here using the non-combined discrete col since part of
        # the combination process above was to create NAs when the discrete
        # values disagree
        ((!is.na(harmonized_activity_depth_value) | !is.na(harmonized_result_depth_value)) &
           !is.na(harmonized_top_depth_value)) |
          ((!is.na(harmonized_activity_depth_value) | !is.na(harmonized_result_depth_value)) &
             !is.na(harmonized_bottom_depth_value)) ~ 3,
        .default = NA_real_
      )) %>%
    # These columns are no longer necessary since the harmonization is done
    select(-c(harmonized_activity_depth_value, harmonized_result_depth_value,
              depth_conversion))
  
  # Sanity check that flags are matching up with their intended qualities:
  depth_check_table <- flagged_depth_tc %>%
    mutate(
      # Everything present
      three_cols_present = if_else(
        !is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Only discrete present
      only_discrete = if_else(
        !is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Only top present
      only_top = if_else(
        is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Only bottom present
      only_bottom = if_else(
        is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Full integrated present
      fully_integrated = if_else(
        is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # No depths present
      no_depths = if_else(
        is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Discrete and one of the integrated
      discrete_partial_integ = if_else(
        (!is.na(harmonized_discrete_depth_value) &
           !is.na(harmonized_top_depth_value) &
           is.na(harmonized_bottom_depth_value)) |
          (!is.na(harmonized_discrete_depth_value) &
             is.na(harmonized_top_depth_value) &
             !is.na(harmonized_bottom_depth_value)),
        true = 1, false = 0)
    ) %>%
    bind_rows() %>%
    count(three_cols_present, only_discrete, discrete_partial_integ,
          only_top, only_bottom, fully_integrated, no_depths, depth_flag,
          harmonized_units) %>%
    arrange(depth_flag, harmonized_units)
  
  
  depth_check_out_path <- "3_harmonize/out/tc_depth_check_table.csv"
  
  write_csv(x = depth_check_table,
            file = depth_check_out_path)
  
  # Depth category counts:
  depth_counts <- flagged_depth_tc %>%
    # Using a temporary flag to aggregate depth values for count output
    mutate(depth_agg_flag = case_when(
      depth_flag == 1 &
        harmonized_discrete_depth_value <= 1 ~ "<=1m",
      depth_flag == 1 &
        harmonized_discrete_depth_value <= 5 ~ "<=5m",
      depth_flag == 1 &
        harmonized_discrete_depth_value > 5 ~ ">5m",
      depth_flag == 2 &
        harmonized_bottom_depth_value <= 1 ~ "<=1m",
      depth_flag == 2 &
        harmonized_bottom_depth_value <= 5 ~ "<=5m",
      depth_flag == 2 &
        harmonized_bottom_depth_value > 5 ~ ">5m",
      .default = "No or inconsistent depth"
    )) %>%
    bind_rows() %>%
    count(depth_agg_flag, harmonized_units)
  
  depth_counts_out_path <- "3_harmonize/out/tc_depth_counts.csv"
  
  write_csv(x = depth_counts, file = depth_counts_out_path)
  
  # Have any records been removed while processing depths?
  print(
    paste0(
      "Rows removed due to non-target depths: ",
      nrow(converted_units_tc) - nrow(flagged_depth_tc)
    )
  )
  
  dropped_depths <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while cleaning depths",
    short_reason = "Clean depths",
    number_dropped = nrow(converted_units_tc) - nrow(flagged_depth_tc),
    n_rows = nrow(flagged_depth_tc),
    order = 8
  )
  
  
  # Aggregate and tier analytical methods -----------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of true color analytical methods present: ",
      flagged_depth_tc %>%
        bind_rows() %>%
        pull(ResultAnalyticalMethod.MethodName) %>%
        unique() %>%
        length()
    )
  )
  
  # Before creating tiers remove records that have clearly unrelated or unreliable
  # data based on their method.
  unrelated_text <- paste0(c("2320", "alkalin"),
                           collapse = "|")
  
  tc_relevant <- flagged_depth_tc %>%
    filter(
      !grepl(pattern = unrelated_text,
             x = ResultAnalyticalMethod.MethodName,
             ignore.case = TRUE)
    )
  
  # How many records removed due to irrelevant analytical methods?
  print(
    paste0(
      "Rows removed due to unrelated analytical methods: ",
      nrow(flagged_depth_tc) - nrow(tc_relevant)
    )
  )
  
  # NOTE: The options below are based on what's actually present in the dataset,
  # so all conceivable options are NOT included
  tiered_methods_tc <- tc_relevant %>%
    mutate(
      tier = case_when(
        # Tier 0:      
        # Apparent color: Total fraction, units not "none", 2120 C methods or
        # EPA 110.3, HACH 8025
        parameter == "Apparent color" & 
          ResultSampleFractionText %in% c("Total", "Non-Filterable", 
                                          "Non-Filterable (Particle)", 
                                          "None", "Suspended", "Unfiltered")  &
          ResultMeasure.MeasureUnitCode != "None" &
          ( 
            grepl(
              x = ResultAnalyticalMethod.MethodName,
              pattern = "2120C|2120-C|110.3|8025"
            ) |
              grepl(
                x = ResultAnalyticalMethod.MethodIdentifier,
                pattern = "2120C|2120-C|110.3|8025"
              ) 
          ) ~ 0,
        
        # True Color : Same as Apparent, but "Dissolved" fraction
        parameter == "True color" & 
          ResultSampleFractionText %in% c("Dissolved", "Acid Soluble", "Filterable",
                                          "Filtered, field", "Filtered, lab") &
          ResultMeasure.MeasureUnitCode != "None" &
          ( 
            grepl(
              x = ResultAnalyticalMethod.MethodName,
              pattern = "2120C|2120-C|110.3|8025"
            ) |
              grepl(
                x = ResultAnalyticalMethod.MethodIdentifier,
                pattern = "2120C|2120-C|110.3|8025"
              ) 
          ) ~ 0,
        
        # Tier 1: Same as above with 2120B and 110.2 methods
        # Apparent color
        parameter == "Apparent color" & 
          ResultSampleFractionText %in% c("Total", "Non-Filterable", 
                                          "Non-Filterable (Particle)", 
                                          "None", "Suspended", "Unfiltered")  &
          ResultMeasure.MeasureUnitCode != "None" &
          ( 
            grepl(
              x = ResultAnalyticalMethod.MethodName,
              pattern = "2120B|2120-B|110.2|visual|CC003",
              ignore.case = TRUE
            ) |
              grepl(
                x = ResultAnalyticalMethod.MethodIdentifier,
                pattern = "2120B|2120-B|110.2|visual|CC003",
                ignore.case = TRUE
              ) 
          ) ~ 1,
        
        # True Color
        parameter == "True color" & 
          ResultSampleFractionText %in% c("Dissolved", "Acid Soluble", "Filterable",
                                          "Filtered, field", "Filtered, lab") &
          ResultMeasure.MeasureUnitCode != "None" &
          ( 
            grepl(
              x = ResultAnalyticalMethod.MethodName,
              pattern = "2120B|2120-B|110.2|visual|CC003",
              ignore.case = TRUE
            ) |
              grepl(
                x = ResultAnalyticalMethod.MethodIdentifier,
                pattern = "2120B|2120-B|110.2|visual|CC003",
                ignore.case = TRUE
              ) 
          ) ~ 1,
        
        # Some additional True Color samples with a non-standard wavelength
        parameter == "True color" & 
          ResultSampleFractionText =="Dissolved" &
          ResultMeasure.MeasureUnitCode == "PCU" &
          ( 
            grepl(
              x = ResultAnalyticalMethod.MethodName,
              pattern = "345|440"
            ) |
              grepl(
                x = ResultAnalyticalMethod.MethodIdentifier,
                pattern = "345|440"
              ) 
          ) ~ 1,
        
        # Default to inclusive tier (2)
        .default = 2
      )
    )
  
  # Export a record of how methods were tiered and their respective row counts
  tier_group_cols <- c(
    "parameter",
    # Charname
    "CharacteristicName",
    # Methods cols: 
    "ResultAnalyticalMethod.MethodName",
    "USGSPCode",
    "ResultAnalyticalMethod.MethodIdentifier",
    "ResultAnalyticalMethod.MethodIdentifierContext",
    # Units
    "ResultMeasure.MeasureUnitCode",
    # Fraction: should generally be filtered 
    "ResultSampleFractionText",
    "tier"
  )
  
  tiering_record <- tiered_methods_tc %>%
    group_by(across(all_of(tier_group_cols))) %>%
    add_count() %>%
    mutate(min_value = min(harmonized_value),
           max_value = max(harmonized_value)) %>%
    ungroup() %>%
    select(all_of(tier_group_cols), min_value, max_value, n) %>%
    distinct() %>%
    arrange(desc(n)) 
  
  tiering_record_out_path <- "3_harmonize/out/tc_tiering_record.csv"
  
  write_csv(x = tiering_record, file = tiering_record_out_path)
  
  # Confirm that no rows were lost during tiering
  if(nrow(tc_relevant) != nrow(tiered_methods_tc)){
    cli_abort("Rows were lost during analytical method tiering. This is not expected.")
  }  
  
  dropped_methods <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(flagged_depth_tc) - nrow(tiered_methods_tc),
    n_rows = nrow(tiered_methods_tc),
    order = 9
  )
  
  
  # Flag field methods ------------------------------------------------------
  
  field_flagged_tc <- tiered_methods_tc %>%
    mutate(
      field_flag = case_when(
        # Discrete sampling methods are given a field_flag of 0
        grepl(
          pattern = paste0(
            c("grab", "bucket", "point", "kemmerer", "van dorn", "bailer", "bottle"),
            collapse = "|"),
          x = SampleCollectionEquipmentName,
          ignore.case = TRUE
        ) ~ 0,
        
        # Integrated sample are given a field_flag of 1
        grepl(
          pattern = paste0(
            c("integrated", "multiple", "pump"),
            collapse = "|"),
          x = SampleCollectionEquipmentName,
          ignore.case = TRUE
        ) ~ 1,
        
        # Everything else, 2
        .default = 2
      )
    )
  
  # How many records removed while field flagging?
  print(
    paste0(
      "Rows removed while assigning field flags: ",
      nrow(tiered_methods_tc) - nrow(field_flagged_tc)
    )
  )
  
  dropped_field <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while assigning field flags",
    short_reason = "Field flagging",
    number_dropped = nrow(tiered_methods_tc) - nrow(field_flagged_tc),
    n_rows = nrow(field_flagged_tc),
    order = 10
  )
  
  
  # Miscellaneous flag ------------------------------------------------------
  
  # Flag values over 1500 as potentially unrealistic (instead of removing them
  # in the next section)
  
  misc_flagged_tc <- field_flagged_tc %>%
    mutate(
      misc_flag = if_else(
        condition = harmonized_value >= 1500,
        true = 1, 
        false = 0
      )
    )
  
  # Export a record of flag counts
  misc_flag_table_out_path <- "3_harmonize/out/tc_misc_flag_table.csv"
  
  write_csv(x = misc_flagged_tc %>%
              count(parameter, harmonized_units, misc_flag),
            file = misc_flag_table_out_path)
  
  dropped_misc <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while assigning misc flags",
    short_reason = "Misc flagging",
    number_dropped = nrow(field_flagged_tc) - nrow(misc_flagged_tc),
    n_rows = nrow(misc_flagged_tc),
    order = 11
  )
  
  
  # Unrealistic values ------------------------------------------------------
  
  # We remove unrealistically high values prior to the final data export.
  
  # We remove any depths > 592m, the deepest point in a lake in the U.S.
  
  realistic_tc <- misc_flagged_tc %>%
    filter(
      harmonized_top_depth_value <= 592 | is.na(harmonized_top_depth_value),
      harmonized_bottom_depth_value <= 592 | is.na(harmonized_bottom_depth_value),
      harmonized_discrete_depth_value <= 592 | is.na(harmonized_discrete_depth_value)
    )
  
  dropped_unreal <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows with unrealistic values",
    short_reason = "Unrealistic values",
    number_dropped = nrow(misc_flagged_tc) - nrow(realistic_tc),
    n_rows = nrow(realistic_tc),
    order = 12
  )
  
  
  # Aggregate simultaneous records ------------------------------------------
  
  # There are true duplicate entries in the WQP or records with non-identical
  # values recorded at the same time and place and by the same organization
  # (field and/or lab replicates/duplicates). We take the mean of those values here
  
  # First tag aggregate subgroups with group IDs
  grouped_tc <- realistic_tc %>%
    group_by(parameter, OrganizationIdentifier, MonitoringLocationIdentifier,
             MonitoringLocationTypeName, ResolvedMonitoringLocationTypeName,
             ActivityStartDate, ActivityStartTime.Time,
             ActivityStartTime.TimeZoneCode, harmonized_tz,
             harmonized_local_time, harmonized_utc, ActivityStartDateTime,
             harmonized_top_depth_value, harmonized_top_depth_unit,
             harmonized_bottom_depth_value, harmonized_bottom_depth_unit,
             harmonized_discrete_depth_value, harmonized_discrete_depth_unit,
             depth_flag, mdl_flag, approx_flag, greater_flag, tier, field_flag,
             misc_flag, harmonized_units) %>%
    mutate(subgroup_id = cur_group_id())
  
  # Export the dataset with subgroup IDs for joining future aggregated product
  # back to original raw data (Excludes data with flagged high values)
  grouped_tc_out_path <- "3_harmonize/out/tc_harmonized_grouped.feather"
  
  grouped_tc %>%
    select(
      all_of(c(raw_names,
               "parameter_code", "group_name", "parameter_name_description",
               "subgroup_id")),
      group_cols(),
      harmonized_value
    ) %>%
    write_feather(path = grouped_tc_out_path)
  
  # Now aggregate at the subgroup level to take care of simultaneous observations
  no_simul_tc <- grouped_tc %>%
    # Make sure we don't drop subgroup ID
    group_by(subgroup_id, .add = TRUE) %>%
    summarize(
      harmonized_row_count = n(),
      harmonized_value_sd = sd(harmonized_value),
      harmonized_value = mean(harmonized_value),
      lon = unique(lon),
      lat = unique(lat),
      datum = unique(datum)
    ) %>%
    # Calculate coefficient of variation as the standard deviation divided by
    # the mean value (harmonized_value in this case)
    mutate(
      harmonized_value_cv = harmonized_value_sd / harmonized_value
    ) %>%
    ungroup() %>%
    select(
      # No longer needed
      -harmonized_value_sd) %>%
    relocate(
      c(subgroup_id, harmonized_row_count, harmonized_units,
        harmonized_value, harmonized_value_cv, lat, lon, datum),
      .after = misc_flag
    ) 
  
  rm(grouped_tc)
  gc()
  
  # Plot harmonized measurements by Tier:
  
  # 1. Harmonized values
  no_simul_tc_tier_label <- no_simul_tc %>%
    mutate(
      tier_label = case_when(
        tier == 0 ~ "Restrictive (Tier 0)",
        tier == 1 ~ "Narrowed (Tier 1)",
        tier == 2 ~ "Inclusive (Tier 2)"
      ),
      tier_label = factor(
        x = tier_label,
        levels = c("Restrictive (Tier 0)", "Narrowed (Tier 1)", "Inclusive (Tier 2)"),
        ordered = TRUE
      )
    )
  
  tier_dists <- no_simul_tc_tier_label %>%
    select(parameter, tier_label, harmonized_value, harmonized_units) %>%
    mutate(plot_value = harmonized_value + 0.001) %>%
    ggplot() +
    geom_histogram(aes(plot_value, fill = parameter),
                   color = "black", alpha = 0.85) +
    # facet_wrap allows each facet to have its own axes; grid doesn't
    facet_wrap(harmonized_units ~ tier_label, ncol = 3, scales = "free") +
    xlab(expression("Harmonized values")) +
    ylab("Record count") +
    ggtitle(label = "Distribution of harmonized values by parameter, tier, and unit",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(
      strip.text = element_text(size = 7),
      legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 5))
  
  ggsave(filename = "3_harmonize/out/tc_tier_dists_postagg.png",
         plot = tier_dists,
         width = 8, height = 10, units = "in", device = "png")
  
  
  # 1(b): Harmonized values by location, param, unit (not tier, but uses the 
  # dataset made for tiers)
  
  location_tier_dists <- no_simul_tc_tier_label %>%
    select(parameter, ResolvedMonitoringLocationTypeName, harmonized_value, harmonized_units) %>%
    mutate(plot_value = harmonized_value + 0.001) %>%
    ggplot() +
    geom_histogram(aes(plot_value, fill = parameter),
                   color = "black", alpha = 0.85) +
    # facet_wrap allows each facet to have its own axes; grid doesn't
    facet_wrap(harmonized_units ~ ResolvedMonitoringLocationTypeName, ncol = 3, scales = "free") +
    xlab(expression("Harmonized values")) +
    ylab("Record count") +
    ggtitle(label = "Distribution of harmonized values by location type, tier, and unit",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(
      strip.text = element_text(size = 7),
      legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 5))
  
  ggsave(filename = "3_harmonize/out/tc_tier_dists_location_postagg.png",
         plot = location_tier_dists,
         width = 8, height = 10, units = "in", device = "png")
  
  
  # 2: Harmonized CVs
  
  # There are very few non-NA rows
  non_na_rows <- no_simul_tc_tier_label %>%
    select(parameter, tier_label, harmonized_value_cv) %>%
    mutate(plot_value = harmonized_value_cv + 0.001) %>%
    na.omit() %>%
    nrow()
  
  tier_cv_dist <- no_simul_tc_tier_label %>%
    select(parameter, tier_label, harmonized_value_cv) %>%
    mutate(plot_value = harmonized_value_cv + 0.001) %>%
    na.omit() %>%
    ggplot() +
    geom_histogram(aes(plot_value), color = "black", fill = "white") +
    # facet_grid(cols = vars(tier_label), rows = vars(parameter), scales = "free_x") +
    facet_wrap(parameter ~ tier_label, scales = "free_x", ncol = 3) +
    xlab(expression("Harmonized coefficient of variation, " ~ log[10] ~ " transformed)")) +
    ylab("Record count") +
    ggtitle(
      label = paste0(
        "Distribution of harmonized CVs (Tier 0 only; n = ",
        non_na_rows,
        ")"
      ),
      subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/tc_tier_cv_dists_postagg.png",
         plot = tier_cv_dist,
         width = 8.5, height = 10, units = "in", device = "png")
  
  # 3. Maps
  # Similarly, create maps of records counts by tier
  plot_tier_maps(dataset = no_simul_tc, custom_width = 8, custom_height = 6,
                 n_bins = 15, param_name = "tc", flip_facets = TRUE,
                 legend_position = "bottom")
  
  # 4. Time
  # Year, month, day of week
  plot_time_charts(dataset = no_simul_tc, custom_width = 7, custom_height = 8,
                   year_seq = 5, param_name = "tc", legend_position = "bottom",
                   scale_type = "free_y")
  
  # 5. Depths
  # And the three depth cols
  
  top_depth_dist <- no_simul_tc_tier_label %>%
    ggplot() +
    geom_histogram(
      aes(harmonized_top_depth_value, fill = tier_label),
      color = "black") +
    facet_grid(
      cols = vars(ResolvedMonitoringLocationTypeName), rows = vars(parameter),
      scales = "free_y"
    ) +
    scale_fill_viridis_d("Tier", direction = -1) +
    xlab("harmonized_top_depth_value, m") +
    ylab("Record count") +
    ggtitle("harmonized_top_depth_value distribution by parameter and location type") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(filename = "3_harmonize/out/tc_tier_top_depth_dist_postagg.png",
         plot = top_depth_dist,
         width = 8, height = 6, units = "in", device = "png")
  
  bottom_depth_dist <- no_simul_tc_tier_label %>%
    ggplot() +
    geom_histogram(
      aes(harmonized_bottom_depth_value, fill = tier_label),
      color = "black") +
    facet_grid(
      cols = vars(ResolvedMonitoringLocationTypeName), rows = vars(parameter),
      scales = "free_y"
    ) +
    scale_fill_viridis_d("Tier", direction = -1) +
    xlab("harmonized_bottom_depth_value, m") +
    ylab("Record count") +
    ggtitle("harmonized_bottom_depth_value distribution by parameter and location type") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(filename = "3_harmonize/out/tc_tier_bottom_depth_dist_postagg.png",
         plot = bottom_depth_dist,
         width = 8, height = 6, units = "in", device = "png")
  
  discrete_depth_dist <- no_simul_tc_tier_label %>%
    ggplot() +
    geom_histogram(
      aes(harmonized_discrete_depth_value, fill = tier_label),
      color = "black") +
    facet_grid(
      cols = vars(ResolvedMonitoringLocationTypeName), rows = vars(parameter),
      scales = "free_y"
    ) +
    scale_fill_viridis_d("Tier", direction = -1) +
    xlab("harmonized_discrete_depth_value, m") +
    ylab("Record count") +
    ggtitle("harmonized_discrete_depth_value distribution by parameter and location type") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggsave(filename = "3_harmonize/out/tc_tier_discrete_depth_dist_postagg.png",
         plot = discrete_depth_dist,
         width = 8, height = 6, units = "in", device = "png")
  
  # Clean up
  rm(no_simul_tc_tier_label)
  gc()
  
  
  # How many records removed in aggregating simultaneous records?
  print(
    paste0(
      "Rows removed while aggregating simultaneous records: ",
      nrow(realistic_tc) - nrow(no_simul_tc)
    )
  )
  
  dropped_simul <- tibble(
    step = "true_color harmonization",
    reason = "Dropped rows while aggregating simultaneous records",
    short_reason = "Simultaneous records",
    number_dropped = nrow(realistic_tc) - nrow(no_simul_tc),
    n_rows = nrow(no_simul_tc),
    order = 13
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_media,
                                dropped_fails, dropped_mdls,
                                dropped_approximates, dropped_greater_than,
                                dropped_na, dropped_harmonization,
                                dropped_depths, dropped_methods,
                                dropped_field, dropped_misc, dropped_unreal,
                                dropped_simul)
  
  documented_drops_out_path <- "3_harmonize/out/tc_harmonize_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/tc_harmonized_final.csv"
  
  write_csv(no_simul_tc,
            data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(no_simul_tc)
    )
  )
  
  return(list(
    tc_param_change_table_path = param_change_table_out_path,
    tc_tiering_record_path = tiering_record_out_path,
    tc_grouped_preagg_path = grouped_tc_out_path,
    tc_harmonized_path = data_out_path,
    compiled_drops_path = documented_drops_out_path
  ))  
}
