### PLOTTING SETUPS 

library(ggplot2)

# General Plot Theme ----

plot_theme <- theme(axis.title=element_text(size=20,color='gray30'),
                    axis.line=element_line(color='gray30', linewidth = 1),
                    axis.ticks=element_line(color='gray30', linewidth = 1),
                    axis.text.y=element_text(size=20,color='gray30'),
                    axis.text.x=element_text(size=20,color='gray30'),
                    legend.text=element_text(size=20),
                    legend.key.size=unit(2,'mm'),
                    panel.background=element_blank(),
                    panel.grid.major.y = element_blank(),
                    legend.position = 'none',
                    plot.margin = margin(0.5,0.5,0.5,0.5,'cm'),
                    strip.background = element_blank(),
                    strip.text = element_text(size=20,color='gray30'))

# Color palettes ----

col_cs = c('#21908C','#482076FF','#5DC863')


# Export functions ----
export_lmer_summary <- function(model, path) {
  # Load necessary packages
  if (!requireNamespace("broom.mixed", quietly = TRUE)) stop("Please install 'broom.mixed'")
  if (!requireNamespace("writexl", quietly = TRUE)) stop("Please install 'writexl'")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install 'dplyr'")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Please install 'tidyr'")
  
  library(broom.mixed)
  library(writexl)
  library(dplyr)
  library(tidyr)

  # Extract fixed effects
  fixed <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    select(term, estimate, std.error, statistic, conf.low, conf.high) %>%
    mutate(group = NA, level = NA, section = "Fixed Effects")
  
  # Extract random effects
  random <- tidy(model, effects = "ran_vals") %>%
    select(term, estimate, std.error, group, level) %>%
    mutate(section = "Random Effects")
  
  # Extract model summary
  summary_df <- glance(model) %>%
    mutate(section = "Model Summary") %>%
    pivot_longer(cols = -section, names_to = "term", values_to = "estimate") %>%
    mutate(
      std.error = NA, statistic = NA,
      conf.low = NA, conf.high = NA, group = NA, level = NA
    ) %>%
    select(term, estimate, std.error, statistic, conf.low, conf.high, group, level, section)
  
  # Create blank row for spacing
  space_row <- tibble(
    term = "", estimate = NA, std.error = NA, statistic = NA,
    conf.low = NA, conf.high = NA, group = NA, level = NA, section = ""
  )
  
  # Combine all into one data frame
  final_df <- bind_rows(
    fixed,
    space_row,
    random,
    space_row,
    summary_df
  )
  
  # Write to Excel
  write_xlsx(list("LME4 Results" = final_df), path)
  message("Export complete: ", path)
}

export_emmeans_contrasts <- function(contrast_list, path = "emmeans_contrasts.xlsx", one_sheet = FALSE) {
  if (!requireNamespace("writexl", quietly = TRUE)) stop("Please install 'writexl'")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install 'dplyr'")
  
  library(writexl)
  library(dplyr)
  
  if (one_sheet) {
    # Combine all into one sheet with spacing and section names
    combined <- bind_rows(
      lapply(names(contrast_list), function(name) {
        df <- as.data.frame(contrast_list[[name]])
        df$contrast_name <- name
        bind_rows(
          tibble(contrast_name = name),  # Section header row
          df,
          tibble()  # Blank row
        )
      })
    )
    
    write_xlsx(list("All Contrasts" = combined), path)
  } else {
    # Each contrast gets its own sheet
    contrast_tables <- lapply(contrast_list, as.data.frame)
    write_xlsx(contrast_tables, path)
  }
  
  message("Exported emmeans contrasts to: ", path)
}
