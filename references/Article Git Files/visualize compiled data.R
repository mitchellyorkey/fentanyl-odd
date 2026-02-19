# Read me ----------------------------------------------------------------
# November 14
# To help reduce confusion in coding and constant back and forth this code takes compiled data and visualizes it

# Libraries --------------------------------------------------------------
# Vector of required packages
packages <- c(
  "tidyverse",
  "patchwork",
  "zoo",
  "ggrepel",
  "GGally",
  "ggcorrplot",
  "ggtext"
)

# Install any packages that are not yet installed
install_if_missing <- packages[
  !(packages %in% installed.packages()[, "Package"])
]
if (length(install_if_missing)) {
  install.packages(install_if_missing)
}

# Load all packages
lapply(packages, library, character.only = TRUE)

# Directories and dependencies -------------------------------------------
dir <- "Data/Compiled"
file <- "compiled_data.rds"
source("./Scripts/visualization helpers.r")

# Load data --------------------------------------------------------------
data <- read_rds(file.path(dir, file))

##### USA plot #####
index_year <- "May 2023"
##### Correlation coefficients between purity and ODD #####
data_cor <- data$data_us |>
  select(period, category, value) |>
  filter(!str_detect(category, "seizures")) |>
  pivot_wider(names_from = category, values_from = value)

cor_powder <- cor.test(
  data_cor$`Synthetic opioid ODD`,
  data_cor$`Fentanyl purity (powder)`,
  method = "pearson"
)

cor_pill <- cor.test(
  data_cor$`Synthetic opioid ODD`,
  data_cor$`Fentanyl purity (pill)`,
  method = "pearson"
)

r_powder <- signif(cor_powder$estimate, 2)
r_pill <- signif(cor_pill$estimate, 2)

caption <- paste0("Dashed vertical line is ", index_year)
caption <- "<b>Source:</b> Data regarding fentanyl purity come from DEA National Drug Threat Assessment, data on synthetic opioid overdose deaths come from CDC WONDER and CDC Provisional drug overdose death counts. National Center for Health Statistics. Crude death rates involving ODDs are calculated using monthly population counts."

##### Plot US visualization #####
plot_us <- data$data_us |>
  ggplot(aes(x = period, y = index_value, linetype = category)) +
  geom_hline(yintercept = 100, color = "darkgrey") +
  geom_line() +

  # --- Rich label annotation ---
  ggtext::geom_richtext(
    aes(
      x = max(data$data_us$period),
      y = 35,
      label = paste0(
        "<b>Correlation with ODD</b><br>",
        "Powder = ",
        r_powder,
        "<br>",
        "Pill = ",
        r_pill
      )
    ),
    hjust = 1,
    vjust = 1,
    fill = "white",
    label.size = 0.3,
    size = 3,
    label.padding = unit(0.15, "lines")
  ) +

  # --- Dashed vertical line at May 2023 ---
  geom_vline(
    xintercept = as.numeric(zoo::as.yearmon(index_year)),
    linetype = "dashed",
    color = "gray40"
  ) +

  # --- Label for vertical line ---
  ggtext::geom_richtext(
    data = tibble(
      x = as.numeric(zoo::as.yearmon(index_year)),
      y = 115,
      label = paste0("<b>", index_year, "</b>")
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0,
    fill = "white",
    label.size = 0.3,
    size = 3
  ) +

  # --- Line styles ---
  scale_linetype_manual(
    values = c(
      "Synthetic opioid ODD" = "solid",
      "Fentanyl purity (pill)" = "dashed",
      "Fentanyl purity (powder)" = "dotted"
    )
  ) +

  # --- Force Y-axis from 0 to 120 ---
  scale_y_continuous(
    breaks = seq(0, 120, 20),
    limits = c(0, 120)
  ) +

  scale_x_yearmon(format = "%Y", expand = expansion(mult = c(0.01, 0.01))) +

  labs(
    title = str_wrap(
      "Monthly Synthetic Opioid Overdoses and Fentanyl Purity in the United States",
      55
    ),
    x = NULL,
    y = paste0("Indexed to ", index_year),
    caption = caption
  ) +
  plot_theme()

# Display US plot
plot_us

# remove title for manuscript
plot_us_nt <- plot_us +
  labs(
    title = NULL,
    caption = NULL
  )

##### Reddit Plot #####
index_year <- "July 2023"

ban1 <- "January 2024"
ban2 <- "October 2024"

plot_reddit <- data$data_reddit |>
  ggplot(aes(period)) +
  geom_line(aes(y = KeywordRatio_index)) +
  geom_point(aes(y = KeywordRatio_index)) +
  geom_line(
    aes(y = UpperControlLimit_index),
    linetype = "dashed",
    color = "steelblue"
  ) +
  geom_line(
    aes(y = LowerControlLimit_index),
    linetype = "dashed",
    color = "steelblue"
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "black"
  ) +
  # geom_vline(
  #   xintercept = as.numeric(zoo::as.yearmon(index_year)),
  #   linetype = "dashed",
  #   color = "gray40"
  # ) +
  geom_vline(
    xintercept = as.numeric(zoo::as.yearmon(ban1)),
    linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = as.numeric(zoo::as.yearmon(ban2)),
    linetype = "dashed",
    color = "red"
  ) +
  annotate(
    "label",
    x = as.numeric(zoo::as.yearmon(ban1)),
    y = max(data$data_reddit$KeywordRatio_index, na.rm = TRUE) * 0.80,
    label = str_wrap("First Reddit ban on drought discussion", 16),
    size = 3.5,
    angle = 0,
    vjust = -0.5,
    color = "red"
  ) +
  annotate(
    "label",
    x = as.numeric(zoo::as.yearmon(ban2)) - .12,
    y = max(data$data_reddit$KeywordRatio_index, na.rm = TRUE) * 1,
    label = str_wrap("Second Reddit ban on drought discussion", 16),
    size = 3.5,
    angle = 0,
    vjust = -0.3,
    color = "red"
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 2000, 500)),
    expand = expansion(mult = c(0.01, 0.2))
  ) +
  # coord_cartesian(clip = "off") +
  scale_x_yearmon(
    breaks = zoo::as.yearmon(seq.Date(
      from = as.Date(min(data$data_reddit$period)),
      to = as.Date(max(data$data_reddit$period)),
      by = "6 months"
    )),
    labels = function(x) {
      ifelse(
        format(x, "%m") == "07",
        paste("July", format(x, "\n%Y")),
        ifelse(format(x, "%m") == "01", paste("Jan", format(x, "\n%Y")), "")
      )
    },
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = str_wrap("Monthly Proportion of Posts Mentioning Drought", 55),
    x = NULL,
    y = paste0("Indexed to pretrend mean"),
    caption = paste0(
      # "Dashed horizontal black line is pre-trend mean index = 100",
      "\nDashed vertical red lines inidicate timing of Reddit bans on discussing \"drought\"",
      "\nDashed horizontal blue lines indicate three standard deviations from pre-trend mean"
    )
  ) +
  plot_theme()

# Display reddit plot
plot_reddit

plot_reddit_nt <- plot_reddit +
  labs(
    title = NULL
  )

##### Canada Plot #####
index_year <- "2023 Q3"

# Create and combine plots
q_lag <- 0 # Number of quarters to lead purity data
source_plots <- data$canada |>
  filter(Year_Quarter >= "2019 Q1", Substance == "Opioids") |>
  split(~Source) |>
  map(build_source_plot)

# Dynamic caption based on q_lag
plot_caption <- if (q_lag > 0) {
  paste(
    "Dashed vertical bar is",
    index_year,
    "\nPurity data are shifted by",
    q_lag,
    "quarter(s) to account for possible lags in reporting."
  )
} else {
  paste("Dashed vertical bar is", index_year)
}

das_plot1 <- data$das_purity |>
  filter(Year_Quarter <= "2025 Q4") |>
  ggplot(aes(Year_Quarter, purity / 100)) +
  geom_line(linewidth = .8) +
  # Index year marker
  geom_vline(
    xintercept = as.numeric(zoo::as.yearqtr(index_year)),
    linetype = "dashed",
    color = "gray40"
  ) +

  # Scales and labels
  scale_y_continuous(
    name = NULL,
    labels = scales::percent
  ) +
  scale_x_yearqtr(
    format = "%Y Q%q",
    labels = quarter_labels,
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Fentanyl powder purity (%)"
  ) +
  plot_theme()

das_plot2 <- data$das_count |>
  filter(Year_Quarter <= "2025 Q4") |>
  ggplot(aes(Year_Quarter, value)) +
  geom_col(aes(fill = drug), position = "dodge", alpha = 0.8) +
  # geom_line(aes(color = drug), linewidth = .8) +
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
    color = NULL,
    fill = NULL,
    title = "Fentanyl counts reported to DAS"
  ) +
  theme(legend.title = element_blank()) +
  plot_theme()


plot_canada <- wrap_plots(
  source_plots$Deaths,
  source_plots$`Emergency Department (ED) Visits`,
  source_plots$`Emergency Medical Services (EMS)`,
  source_plots$Hospitalizations,
  das_plot1,
  das_plot2,
  ncol = 2
) +
  plot_annotation(
    title = "Quarterly Opioid-Related Events and Fentanyl Supply Measures in Canada",
    caption = plot_caption,
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  ) +
  plot_layout(guides = "collect") & # <--- collect the guides here
  theme(
    legend.position = "bottom", # put legend at bottom of whole figure
    legend.direction = "horizontal",
    legend.justification = "center", # center the legend
    legend.box = "horizontal" # arrange items horizontally
  )

# Display Canada plots
plot_canada

plot_canada_nt <- plot_canada
plot_canada_nt$patches$annotation$title <- NULL


# ======================================================================
# Save all plots that start with "plot_"
# ======================================================================
plot_dir <- "Plots"
png_dir <- file.path(plot_dir, "PNG")
pdf_dir <- file.path(plot_dir, "PDF")

dir.create(plot_dir, showWarnings = FALSE)
dir.create(png_dir, showWarnings = FALSE)
dir.create(pdf_dir, showWarnings = FALSE)

# get only ggplot objects starting with "plot_"
plot_names <- ls(pattern = "^plot_")
plot_names <- plot_names[
  sapply(plot_names, function(x) inherits(get(x), "ggplot"))
]

message("Saving plots:\n", paste(plot_names, collapse = ", "))

for (nm in plot_names) {
  plot_obj <- get(nm)

  # choose size
  if (grepl("canada", nm, ignore.case = TRUE)) {
    w <- 9
    h <- 8
  } else {
    w <- 8
    h <- 6
  }

  ggsave(
    file.path(png_dir, paste0(nm, ".png")),
    plot = plot_obj,
    width = w,
    height = h,
    dpi = 300
  )

  ggsave(
    file.path(pdf_dir, paste0(nm, ".pdf")),
    plot = plot_obj,
    width = w,
    height = h
  )
}
