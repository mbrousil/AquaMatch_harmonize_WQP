#' @title Create hex maps of record counts by data tier
#' 
#' @description
#' A function that creates hex maps of record counts for each tier of a selected
#' parameter's harmonized dataset. The maps are created as a single paneled ggplot
#' object and then exported as a single PNG file.
#' 
#' @param dataset A data frame of the harmonized parameter's dataset containing
#' the columns `parameter`, `tier`, `MonitoringLocationIdentifier`, `lat`, `lon`,
#' and `datum`. Intended to be the version of the dataset with simultaneous observations removed. 
#' @param param_name String that should be used to refer to the name of the overall
#' dataset when naming the output file. e.g., "cdom". Avoids accidentally pasting
#' together several parameter names into along filename.
#' @param map_crs The epsg code that should be used when creating the maps.
#' @param flip_facets Logical, defaults to FALSE. Should the facet_grid axes be
#' flipped?
#' @param custom_width The desired output PNG width in inches.
#' @param custom_height The desired output PNG height in inches.
#' @param n_bins From ?geom_hex: "numeric vector giving number of bins in both vertical and horizontal directions." 
#' @param legend_position Custom input string to provide to ggplot2::theme(legend.position).
#' Options are "none", "left", "right", "bottom", "top", "inside". Defaults to "right".
plot_tier_maps <- function(dataset, param_name, map_crs = 9311, flip_facets = FALSE,
                           custom_width = 6.5, custom_height = 8, n_bins = 60,
                           legend_position = "right"){
  
  # Check for multiple parameter entries. Will facet if so
  if(length(unique(dataset$parameter)) > 1) {
    multiple <- TRUE
  } else {
    multiple <- FALSE
  }
  
  # Conterminous US sf object
  conterminous_us <- tigris::states(progress_bar = FALSE) %>%
    st_transform(crs = 9311) %>%
    filter(!(NAME %in% c("Alaska", "Hawaii", "American Samoa",
                         "Guam", "Puerto Rico",
                         "United States Virgin Islands",
                         "Commonwealth of the Northern Mariana Islands")))
  
  # Other US territories sf object
  non_conterminous_us <- tigris::states(progress_bar = FALSE) %>%
    st_transform(crs = 9311) %>%
    filter((NAME %in% c("Alaska", "Hawaii", "American Samoa",
                        "Guam", "Puerto Rico",
                        "United States Virgin Islands",
                        "Commonwealth of the Northern Mariana Islands")))
  
  
  # Datum varies throughout the dataset; build a conversion table.
  epsg_codes <- tribble(
    ~datum, ~epsg,
    # American Samoa Datum
    "AMSMA", 4169,
    # Midway Astro 1961
    "ASTRO", 37224,
    # Guam 1963
    "GUAM", 4675,
    # High Accuracy Reference Network for NAD83
    "HARN", 4957,
    # Johnston Island 1961 (Spelled Johnson in WQX)
    "JHNSN", 6725,
    # North American Datum 1927
    "NAD27", 4267,
    # North American Datum 1983
    "NAD83", 4269,
    # Old Hawaiian Datum
    "OLDHI", 4135,
    # Assume WGS84
    "OTHER", 4326,
    # Puerto Rico Datum
    "PR", 4139,
    # St. George Island Datum
    "SGEOR", 4138,
    # St. Lawrence Island Datum
    "SLAWR", 4136,
    # St. Paul Island Datum
    "SPAUL", 4137,
    # Assume WGS84
    "UNKWN", 4326,
    # Wake-Eniwetok 1960
    "WAKE", 37229,
    # World Geodetic System 1972
    "WGS72", 4322,
    # World Geodetic System 1984
    "WGS84", 4326
  )
  
  # Create a simplified sf object from the provided dataset
  recs_sf <- dataset %>%
    select(parameter, tier, MonitoringLocationIdentifier, lat, lon, datum) %>%
    left_join(
      x = .,
      y = epsg_codes,
      by = "datum"
    ) %>%
    # Group by CRS 
    split(f = .$epsg) %>%
    # Transform and re-stack
    map_df(.x = .,
           .f = ~ .x %>%
             st_as_sf(coords = c("lon", "lat"),
                      crs = unique(.x$epsg)) %>%
             st_make_valid() %>%
             st_transform(crs = map_crs)) %>%
    # More informative facet panel labels
    mutate(
      tier_label = case_when(
        tier == 0 ~ "Tier 0: Restrictive",
        tier == 1 ~ "Tier 1: Narrowed",
        tier == 2 ~ "Tier 2: Inclusive",
        # SDD only
        tier == 3 ~ "Tier 3: Inclusive"
      ),
      param_label = case_when(
        parameter == "Absorbance at 254 nm" ~ "Abs 254 nm",
        parameter == "Absorbance at 280 nm" ~ "Abs 280 nm",
        parameter == "Absorbance at 370 nm" ~ "Abs 370 nm", 
        parameter == "Absorbance at 412 nm" ~ "Abs 412 nm",
        parameter == "Absorbance at 440 nm" ~ "Abs 440 nm",
        parameter == "Absorption spectral slope, 275 to 295 nm" ~ "Slope 275-295 nm", 
        parameter == "Absorption spectral slope, 290 to 350 nm" ~ "Slope 290-350 nm",
        parameter == "Absorption spectral slope, 350 to 400 nm" ~ "Slope 350-400 nm", 
        parameter == "Absorption spectral slope, 400 to 500 nm" ~ "Slope 400-500 nm",
        parameter == "Absorption spectral slope, 412 to 600 nm" ~ "Slope 412-600 nm", 
        parameter == "Absorption spectral slope, 412 to 676 nm" ~ "Slope 412-676 nm",
        .default = parameter
      )
    )
  
  # Focal records for the map
  trim_recs_sf <- recs_sf[conterminous_us, ]
  
  if(multiple){
    
    # Make the map
    map_plot <- sf_to_df(trim_recs_sf, fill = TRUE) %>%
      ggplot() +
      geom_hex(aes(x = x, y = y),
               bins = n_bins) +
      geom_sf(data = conterminous_us,
              color = "black",
              fill = NA) +
      scale_fill_viridis_c("Record count",
                           trans = "log",
                           breaks = breaks_log(n = 6),
                           labels = label_number(big.mark = ",")) +
      xlab(NULL) +
      ylab(NULL) +
      # If TRUE swap rows and cols
      if(flip_facets){
        facet_grid(rows = vars(param_label), cols = vars(tier_label)) 
      } else{
        facet_grid(rows = vars(tier_label), cols = vars(param_label)) 
      } 
    
    # Continue adding after if/else
    map_plot <- map_plot +
      guides(x = guide_axis(check.overlap = TRUE),
             y = guide_axis(check.overlap = TRUE)) +
      ggtitle(
        label = "Record counts across the US by tier and parameter",
        subtitle = paste0(
          "Not shown: ",
          comma(nrow(recs_sf[non_conterminous_us,])),
          " records from outside the conterminous US"
        )) +
      theme_bw() +
      theme(legend.position = legend_position)
    
  } else {
    
    # Make the map
    map_plot <- sf_to_df(trim_recs_sf, fill = TRUE) %>%
      ggplot() +
      geom_hex(aes(x = x, y = y),
               bins = n_bins) +
      geom_sf(data = conterminous_us,
              color = "black",
              fill = NA) +
      scale_fill_viridis_c("Record count",
                           trans = "log",
                           breaks = breaks_log(n = 6),
                           labels = label_number(big.mark = ",")) +
      xlab(NULL) +
      ylab(NULL) +
      facet_wrap(vars(tier_label), ncol = 1) +
      guides(x = guide_axis(check.overlap = TRUE),
             y = guide_axis(check.overlap = TRUE)) +
      ggtitle(
        label = paste0(unique(dataset$param_label),
                       " record counts across the US by tier"),
        subtitle = paste0(
          "Not shown: ",
          comma(nrow(recs_sf[non_conterminous_us,])),
          " records from outside the conterminous US"
        )) +
      theme_bw() +
      theme(legend.position = legend_position)
    
  }
  
  
  # Export with autogenerated filename
  ggsave(filename = paste0("3_harmonize/out/",
                           param_name,
                           "_tier_hex_map.png"),
         plot = map_plot, units = "in", device = "png",
         width = custom_width, height = custom_height)
}
