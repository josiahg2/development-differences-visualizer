theme_development <- function(){
  theme_bw() %+replace%
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
}

plot_tab2 <- function(.data, .countries, .indicators, .view, .normalize, .year) {
  # error checking: some countries don't have data for some columns
  missing_data_cases <- .data %>%
    rename(country = "Country name") %>%
    pivot_longer(-c(country:Year), names_to = "indicator", values_to = "value") %>%
    drop_na(value) %>%
    select(country, indicator) %>%
    filter(country %in% .countries, indicator %in% .indicators) %>%
    distinct %>%
    mutate(has_values = 1) %>%
    right_join(data.frame(country = rep(.countries, each = length(.indicators)),
                          indicator = rep(.indicators, length(.countries)))) %>%
    filter(is.na(has_values))
  # validate(need(nrow(missing_data_cases) > 0,
  #               message = str_c("No values of ", missing_data_cases$indicator, " for ", missing_data_cases$country, ".")))
  
  plot_data <- .data %>%
    pivot_longer(-c(`Country name`:Year), names_to = "indicator", values_to = "value") %>%
    select(`Country name`, Year, indicator, value) %>%
    filter(`Country name` %in% .countries, indicator %in% .indicators)
  
  if (.normalize) {
    baseline_data <- plot_data %>%
      group_by(`Country name`, indicator) %>%
      filter(Year == .year) %>%
      rename("baseline" = "value") %>%
      select(-Year)
    
    plot_data <- plot_data %>%
      left_join(baseline_data, by = c("Country name", "indicator")) %>%
      mutate(value = value / baseline * 100) %>%
      select(-baseline)
  }
  
  facet_labeller <- function(x) {
    if (.view == "Difference") {
      x <- str_c(x, ", ", .countries[2], " - ", .countries[1])
    }
    if (.normalize) {
      x <- str_c(x, ", normalized at ", if_else(.view == "Difference", 0, 100), 
                 " in ", .year)
    }
    x
  }
  
  if (.view == "Difference") {
    plot_data %>%
      pivot_wider(names_from = `Country name`, values_from = value) %>%
      mutate(difference = .[[.countries[2]]] - .[[.countries[1]]]) %>%
      drop_na(difference) %>%
      ggplot(aes(x = Year, y = difference)) +
      geom_line() +
      facet_wrap(~indicator, scales = "free", ncol = 1, strip.position = "top", 
                 labeller = as_labeller(facet_labeller)) +
      theme_development() %+replace%
      theme(axis.title.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            strip.placement = "outside",
            strip.text = element_text(size = 14, hjust = 0),
            strip.background = element_rect(fill = NA, color = NA))
  }
  
  else {
    plot_data %>%
      drop_na(value) %>%
      ggplot(aes(x = Year, y = value, color = `Country name`)) +
      geom_line() +
      facet_wrap(~indicator, scales = "free", ncol = 1, strip.position = "top", 
                 labeller = as_labeller(facet_labeller)) +
      theme_development() %+replace%
      theme(axis.title.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            strip.placement = "outside",
            strip.text = element_text(size = 14, hjust = 0),
            strip.background = element_rect(fill = NA, color = NA))
  }
}

# suggestions for country 2
get_divergence_examples <- function(.data, .country1, .indicator1){
  # some countries don't have data for some columns
  # if(all(is.na(.data[[.indicator1]][.data[["Country name"]] == .country1]))){
  #   stop(str_c("No values of ", .indicator1, " for ", .country1, ". Please choose another variable or another country."))
  # }
  
  # get similar countries within 10 percentile-ranks of country 1 in the beginning
  similar_countries <- .data %>%
    mutate(indicator1 = .[[.indicator1]]) %>%
    select(`Country name`, Year, indicator1) %>%
    drop_na(indicator1) %>%
    filter(Year == min(Year[.[["Country name"]] == .country1])) %>%
    mutate(percentile = ntile(indicator1, 100)) %>%
    filter(percentile < percentile[.[["Country name"]] == .country1] + 10, 
           percentile > percentile[.[["Country name"]] == .country1] - 10) %>% 
    pull(`Country name`)
  
  # within those similar countries, get top 3 most different countries today
  divergent_countries <- .data %>%
    mutate(indicator1 = .[[.indicator1]]) %>%
    select(`Country name`, Year, indicator1) %>%
    drop_na(indicator1) %>%
    filter(Year == max(Year[.[["Country name"]] == .country1])) %>%
    mutate(percentile = ntile(indicator1, 100),
           difference = abs(percentile - percentile[.[["Country name"]] == .country1])) %>%
    filter(`Country name` %in% similar_countries) %>%
    arrange(desc(difference)) %>%
    slice_head(n = 3) %>%
    pull(`Country name`)
  
  str_c("Suggested: ", str_c(divergent_countries, collapse = ", "))
}

# Development Visualizer

Dev_visualizer <- function(data, x_var, y_var, color_var, size_var, year_range, region, log_x = FALSE, log_y = FALSE, add_reg_line = FALSE) {
  
  # Filter the data based on the year range
  data <- data %>% filter(Year == year_range)
  
  # Filter the data based on region if applicable
  if (region != "All") {
    data <- data %>% filter(Region == region)
  }
  
  # Prepare variables for ggplot (handling spaces in variable names)
  x_var_plot <- ifelse(grepl(" ", x_var), paste0("`", x_var, "`"), x_var)
  y_var_plot <- ifelse(grepl(" ", y_var), paste0("`", y_var, "`"), y_var)
  color_var_plot <- ifelse(grepl(" ", color_var), paste0("`", color_var, "`"), color_var)
  size_var_plot <- ifelse(grepl(" ", size_var), paste0("`", size_var, "`"), size_var)
  
  if (color_var == "None") {
    color_var_plot <- NULL
  }
  if (size_var == "None") {
    size_var_plot <- NULL
  }
  
  # Create scatter plot with size coding
  p <- ggplot(data, aes_string(x = x_var_plot, y = y_var_plot)) +
    geom_point(aes_string(color = color_var_plot, size = size_var_plot)) +
    theme_minimal() +
    labs(x = x_var, y = y_var)
  
  # Apply log transformation if selected
  if (log_x) {
    p <- p + scale_x_log10()
  }
  if (log_y) {
    p <- p + scale_y_log10()
  }
  
  # Add a single regression line for the whole plot if selected
  if (add_reg_line) {
    p <- p + geom_smooth(method = "lm", se = FALSE)
  }
  
  # Remove legend for size if not a chosen variable
  if (size_var == "None") {
    p <- p + guides(size = FALSE)
  }
  
  return(p)
}

#regression summary output
regressionSummary <- function(input_data, x_var, y_var, year, region, log_x, log_y) {
  
  require(broom)
  
  formula <- as.formula(paste("`", y_var, "` ~ `", x_var, "`", sep = ""))
  
  # Apply log transformation if selected
  if (log_x) {
    input_data[[x_var]] <- log(input_data[[x_var]])
  }
  if (log_y) {
    input_data[[y_var]] <- log(input_data[[y_var]])
  }
  
  # Filter the data based on the year range
  input_data <- input_data %>% filter(Year == year)
  
  # Filter the data based on region if applicable
  if (region != "All") {
    input_data <- input_data %>% filter(Region == region)
  }
  
  model <- lm(formula, data = input_data)
  
  summary_table <- tidy(model) %>%
    filter(term != "(Intercept)")
  
  slope <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]

  # Determining if the slope is positive or negative
  positive <- slope > 0

  # Determining if the result is significant
  significant <- p_value < 0.05

  # Construct the summary text
  if (log_x) {
    summary_text <- str_c("For every 1 unit increase in ", x_var, " (logged), there is an estimated ")
  } else {
    summary_text <- str_c("For every 1 unit increase in ", x_var, ", there is an estimated ")
  }
  
  if (positive) {
    summary_text <- str_c(summary_text, round(slope, 2), " unit increase in ", y_var)
  } else {
    summary_text <- str_c(summary_text, abs(round(slope, 2)), " unit decrease in ", y_var)
  }
  
  if (log_y) {
    summary_text <- str_c(summary_text, " (logged).")
  } else {
    summary_text <- str_c(summary_text, ".")
  }
  
  if (significant) {
    summary_text <- str_c(summary_text, " If the <a href = https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/assumptions-of-linear-regression/>assumptions of linear regression</a> are met, we can be 95% confident that there is a statistical relationship between the two variables.")
  } else {
    summary_text <- str_c(summary_text, " Due to randomness, we cannot be confident that there is a statistical relationship between the two variables.")
  }
  
  return(list(summary_table, summary_text))
}
