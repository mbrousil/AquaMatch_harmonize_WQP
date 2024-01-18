# Source the functions that will be used to build the targets in p3_targets_list
tar_source(files = "3_harmonize/src/")

p3_targets_list <- list(
  
  # Pre-harmonization data prep ---------------------------------------------
  
  # All columns in p3_wqp_data_aoi_* are of class character. Coerce select 
  # columns back to numeric, but first retain original entries in new columns
  # ending in "_original". The default option is to format "ResultMeasureValue"
  # and "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but 
  # additional variables can be added using the `vars_to_numeric` argument in 
  # format_columns(). By default, format_columns() will retain all columns, but
  # undesired variables can also be dropped from the WQP dataset using the 
  # optional `drop_vars` argument. 
  tar_target(
    p3_wqp_data_aoi_formatted_chl,
    format_columns(p2_wqp_data_aoi_chl),
    format = "feather"
  ),
  
  # Cleaning steps: 
  # Remove duplicates, ensure meaningful results present, check data status,
  # check media, remove white spaces
  tar_target(
    p3_wqp_data_aoi_ready_chl,
    clean_wqp_data(wqp_data = p3_wqp_data_aoi_formatted_chl,
                   char_names_crosswalk = p1_char_names_crosswalk,
                   # Convert list of sites by param to single df
                   site_data = bind_rows(p2_site_counts),
                   wqp_metadata = p1_wqp_inventory_aoi),
    packages = c("tidyverse", "feather")),
  
  # Connect cleaned data output to the pipeline
  tar_target(p3_cleaned_wqp_data_chl,
             read_feather(p3_wqp_data_aoi_ready_chl$wqp_data_clean_path),
             packages = "feather",
             format = "feather",
             cue = tar_cue("always")),
  
  # Get parameter codes for use in cleaning processes
  tar_target(
    name = p3_p_codes,
    command = get_p_codes(),
    packages = c("tidyverse", "rvest", "janitor")
  ),
  
  
  # Harmonization process ---------------------------------------------------
  
  tar_target(p3_chla_harmonized,
             harmonize_chla(raw_chla = p3_cleaned_wqp_data_chl,
                            p_codes = p3_p_codes),
             packages = c("tidyverse", "feather", "ggrepel", "scales")),
  
  tar_file_read(name = p3_chla_tiering_record,
                command = p3_chla_harmonized$chla_tiering_record_path,
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  # Harmonized chlorophyll data containing grouping IDs for simultaneous
  # records, but not aggregated
  tar_file_read(name = p3_chla_preagg_grouped,
                command = p3_chla_harmonized$chla_grouped_preagg_path,
                read = read_feather(path = !!.x),
                cue = tar_cue("always"),
                packages = "feather"),
  
  # Harmonized chlorophyll data after simultaneous record aggregation (i.e.,
  # final product)
  tar_file_read(name = p3_chla_agg_harmonized,
                command = p3_chla_harmonized$chla_harmonized_path,
                read = read_csv(path = !!.x),
                cue = tar_cue("always")),
  
  tar_target(p3_documented_drops,
             map_df(.x = c(p3_wqp_data_aoi_ready_chl$compiled_drops_path,
                           p3_chla_harmonized$compiled_drops_path),
                    .f = read_csv),
             cue = tar_cue("always"))
)

