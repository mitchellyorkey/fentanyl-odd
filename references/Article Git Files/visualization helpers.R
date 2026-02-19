# Read me ----------------------------------------------------------------
# November 14
# This script is used to help with visualizations. It mostly contains some functions for plotting and labeling. It is called in the "visualize compiled data.r" script.

plot_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_line(color = "grey90", size = 0.2),
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      plot.caption = ggtext::element_textbox(
        width = unit(0.95, "npc"), # wrap caption at 95% of plot width
        hjust = 0, # left align
        halign = 0, # left align text within textbox
        size = 10
      )
    )
}

# Functions for Canadian plots -------------------------------------------
# Custom x-axis labels with newlines
quarter_labels <- function(x) {
  q <- quarters(x)
  yr <- format(x, "%Y")
  paste0(q, "\n", yr)
}

# Plotting function with dynamic lag
build_source_plot <- function(df) {
  plot_data <- df
  # Apply Q4 2024 filter when q_lag = 0
  if (q_lag == 0) {
    plot_data <- plot_data |>
      filter(Year_Quarter <= as.yearqtr("2025 Q4"))
  }

  # Custom x-axis labels with newlines
  quarter_labels <- function(x) {
    q <- quarters(x)
    yr <- format(x, "%Y")
    paste0(q, "\n", yr)
  }

  ggplot(plot_data, aes(x = Year_Quarter)) +
    # Secondary series (Purity)
    # geom_line(aes(y = purity_scaled), color = "red", linetype = "solid", linewidth = 0.6) +
    # geom_smooth(aes(y = purity_scaled), color = "red", se = FALSE, linewidth = 0.8) +

    # Primary series (Value)
    geom_line(aes(y = Value), color = "black", linewidth = .8) +
    # geom_smooth(aes(y = Value), color = "black", se = FALSE) +

    # Index year marker
    geom_vline(
      xintercept = as.numeric(zoo::as.yearqtr(index_year)),
      linetype = "dashed",
      color = "gray40"
    ) +

    # Scales and labels
    scale_y_continuous(
      name = NULL,
      labels = scales::comma
    ) +
    scale_x_yearqtr(
      format = "%Y Q%q",
      labels = quarter_labels,
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = unique(df$Source)
    ) +
    plot_theme()
}
