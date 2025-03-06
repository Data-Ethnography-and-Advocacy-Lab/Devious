library(dplyr)
library(ggplot2)
#' Phantom Flags Function
#'
#' This function flags rows with drop offs or pick ups from the previous time
#' @param aggregated_data Dataset
#' @param time Time that each row represents
#' @param ID Identifying key
#' @param category Categorization of value
#' @param value Numeric value of interest
#' @keywords phantom reductions
#' @export
#' @examples
#' OP_flags <- phantom_flags(open_payments, year, company, funding_category, n)
phantom_flags <- function(aggregated_data, time, ID, category, value) {
  time <- enquo(time)
  ID <- enquo(ID)
  category <- enquo(category)
  value <- enquo(value)

  phantom_emissions_flags <- aggregated_data |>
    group_by(!!ID, !!category) |>
    mutate(last_time = lag(!!value),
           this_time_minus_last = !!value - last_time,
           # Dropoff is when value drops to 0
           dropoff_flag = case_when(last_time != 0 & !!value == 0 ~ "dropoff",
                                    TRUE ~ NA_character_),
           # Dropoff total is difference in value between times where a dropoff occurs
           dropoff_total = case_when(dropoff_flag == "dropoff" ~ this_time_minus_last,
                                     TRUE ~ NA_real_))  |>
    ungroup() |>
    group_by(!!ID, !!time) |>
    # Calculate a dropoff time total per ID
    mutate(dropoff_time_total = sum(dropoff_total, na.rm = TRUE)) |>
    ungroup() |>
    # Pickup is identified in two different cases:
    # 1. When the difference between times is greater than 0 and greater than 80 percent of the total dropoff time total
    # 2. When the value is greater than double last times' value
    mutate(pickup_flag = case_when(this_time_minus_last > 0 &
                                     (this_time_minus_last > abs(dropoff_time_total) * 0.8 | !!value > last_time * 2) ~ "pickup",
                                   TRUE ~ NA_character_)) |>
    group_by(!!ID, !!time) |>
    # Identify cases where a given ID has both a dropoff and pickup in one time
    mutate(change_cat = case_when(any(dropoff_flag == "dropoff") & any(pickup_flag == "pickup") ~ "phantom_reduction_flag",
                                  TRUE ~ NA_character_)) |>
    ungroup()
  return(phantom_emissions_flags)
}

#' Dropoff and Pickup Filter Function
#'
#' This function filters the dataset to just rows with phantom reductions
#' @param phantom_emissions_flags Dataset with dropoff?pickup flags, resulting dataset from phantom_flags function
#' @keywords dropoff pickup
#' @export
#' @examples
#' dropoff_and_pickup(OP_flags)
dropoff_and_pickup <- function(phantom_emissions_flags){
  # Filter rows with a phantom reduction
  IDs_with_dropoff_and_pickup <-
    phantom_emissions_flags |>
    filter(change_cat == "phantom_reduction_flag" & (dropoff_flag == "dropoff" | pickup_flag == "pickup")) |>
    arrange(dropoff_time_total)
  return(IDs_with_dropoff_and_pickup)
}


c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow3",
  "darkorange4", "brown"
)


#' Create Sankey Bump Function
#'
#' This function creates sankey bump graphs to visualize phantom reductions
#' @param phantom_emissions_flags Dataset with dropoff?pickup flags, resulting dataset from phantom_flags function
#' @param time Time that each row represents
#' @param ID Identifying key
#' @param category Categorization of value
#' @param value Numeric value of interest
#' @param id_instance Specific ID that is visualized
#' @export
#' @examples
#' create_sankey_bump(OP_flags, year, company, funding_category, n, 3)
create_sankey_bump <- function(phantom_emissions_flags, time, ID, category, value, id_instance) {
  # , title, xaxis, yaxis, labels
  # string, df
  # Filter dataset to only ID of interest
  time <- enquo(time)
  ID <- enquo(ID)
  print(ID)
  category <- enquo(category)
  value <- enquo(value)

  ID_phantom_emissions <- phantom_emissions_flags |>
    filter(!!ID == id_instance)
  #mutate(category = str_wrap(category, width = 40)) Delete this???

  # Change later
  file_name <- paste0("analysis_data/sankey_op_", id_instance,".png" )

  # Filter out to only include categories seen in plot
  ID_phantom_emissions <- ID_phantom_emissions |>
    group_by(!!category) |>
    filter(any(!!value > 0))

  # Create sankey
  ID_phantom_emissions |>
    ggplot(aes(
      x = !!time,
      node = !!category,
      fill = factor(!!category),
      value = !!value
    )) +
    geom_sankey_bump(space = 0, type = "alluvial", color = "transparent", smooth = 6, alpha = .8) +
    theme_sankey_bump(base_size = 16) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          subtitle.text = element_text(size = 10)) +
    guides(fill = guide_legend(nrow = 2)) +
    ggtitle("Add in Later") +
    scale_fill_manual(values = c25) +
    labs(x = "",
         y = "Number of Payments"
    )
}

