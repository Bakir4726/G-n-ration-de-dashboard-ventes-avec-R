options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
  library(scales)
})

input_file <- "sales.csv"
output_file <- "sales_dashboard.png"

if (!file.exists(input_file)) {
  stop(sprintf("Fichier introuvable: %s", input_file))
}

sales <- read.csv(
  input_file,
  check.names = FALSE,
  na.strings = c("", "NA")
)

required_columns <- c(
  "Date",
  "Country",
  "Product Category",
  "Sub Category",
  "Quantity",
  "Cost",
  "Revenue"
)

missing_columns <- setdiff(required_columns, names(sales))
if (length(missing_columns) > 0) {
  stop(sprintf("Colonnes manquantes: %s", paste(missing_columns, collapse = ", ")))
}

sales <- sales[
  !is.na(sales$Date) &
    !is.na(sales$Country) &
    !is.na(sales[["Product Category"]]) &
    !is.na(sales[["Sub Category"]]) &
    !is.na(sales$Revenue),
]

sales$Date <- as.Date(sales$Date, format = "%m/%d/%Y")
sales$Quantity <- as.numeric(sales$Quantity)
sales$Cost <- as.numeric(sales$Cost)
sales$Revenue <- as.numeric(sales$Revenue)

sales <- sales[
  !is.na(sales$Date) &
    !is.na(sales$Revenue) &
    sales$Revenue >= 0,
]

if (nrow(sales) == 0) {
  stop("Aucune ligne exploitable apres nettoyage du dataset.")
}

sales$Cost[is.na(sales$Cost)] <- 0
sales$Quantity[is.na(sales$Quantity)] <- 0
sales$Profit <- sales$Revenue - sales$Cost
sales$MonthStart <- as.Date(format(sales$Date, "%Y-%m-01"))

monthly_metrics <- aggregate(cbind(Revenue, Profit) ~ MonthStart, data = sales, sum)
monthly_metrics <- monthly_metrics[order(monthly_metrics$MonthStart), ]

country_revenue <- aggregate(Revenue ~ Country, data = sales, sum)
country_revenue <- country_revenue[order(country_revenue$Revenue, decreasing = TRUE), ]
top_countries <- head(country_revenue, 6)
top_countries$Country <- factor(
  top_countries$Country,
  levels = top_countries$Country[order(top_countries$Revenue)]
)

category_metrics <- aggregate(
  cbind(Revenue, Profit) ~ `Product Category`,
  data = sales,
  sum
)
category_metrics <- category_metrics[
  order(category_metrics$Revenue, decreasing = TRUE),
]
category_metrics$`Product Category` <- factor(
  category_metrics$`Product Category`,
  levels = category_metrics$`Product Category`
)

category_long <- rbind(
  data.frame(
    Category = category_metrics$`Product Category`,
    Metric = "Revenue",
    Value = category_metrics$Revenue
  ),
  data.frame(
    Category = category_metrics$`Product Category`,
    Metric = "Profit",
    Value = category_metrics$Profit
  )
)

subcategory_revenue <- aggregate(Revenue ~ `Sub Category`, data = sales, sum)
subcategory_revenue <- subcategory_revenue[
  order(subcategory_revenue$Revenue, decreasing = TRUE),
]
top_subcategories <- head(subcategory_revenue, 8)
top_subcategories$`Sub Category` <- factor(
  top_subcategories$`Sub Category`,
  levels = top_subcategories$`Sub Category`[order(top_subcategories$Revenue)]
)

total_revenue <- sum(sales$Revenue, na.rm = TRUE)
total_profit <- sum(sales$Profit, na.rm = TRUE)
average_order_value <- mean(sales$Revenue, na.rm = TRUE)
units_sold <- sum(sales$Quantity, na.rm = TRUE)
best_country <- as.character(country_revenue$Country[1])
best_category <- as.character(category_metrics$`Product Category`[1])

metric_formatter <- label_number(
  accuracy = 0.1,
  scale_cut = cut_short_scale(),
  big.mark = " "
)
integer_formatter <- label_number(accuracy = 1, big.mark = " ")
percent_formatter <- label_percent(accuracy = 0.1)

format_metric <- function(value) {
  metric_formatter(value)
}

dashboard_background <- "#F6F1E8"
panel_background <- "#FFFDF9"
panel_border <- "#E4DBCF"
title_color <- "#21324E"
muted_text <- "#6A7382"
accent_blue <- "#1F6E8C"
accent_coral <- "#E07A5F"
accent_green <- "#3D9970"
accent_plum <- "#7B4F9E"
accent_gold <- "#D9A441"

dashboard_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = panel_background, colour = NA),
      panel.background = element_rect(fill = panel_background, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E9E1D7", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#EFE7DD", linewidth = 0.35),
      axis.title = element_text(colour = title_color, face = "bold"),
      axis.text = element_text(colour = muted_text),
      plot.title = element_text(colour = title_color, face = "bold", size = 14),
      plot.subtitle = element_text(colour = muted_text, size = 10),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(colour = muted_text),
      plot.margin = margin(14, 16, 14, 16)
    )
}

plot_monthly <- ggplot(monthly_metrics, aes(x = MonthStart)) +
  geom_area(aes(y = Revenue), fill = accent_blue, alpha = 0.16) +
  geom_line(aes(y = Revenue, colour = "Revenue"), linewidth = 1.1) +
  geom_point(aes(y = Revenue, colour = "Revenue"), size = 1.8) +
  geom_line(aes(y = Profit, colour = "Profit"), linewidth = 1) +
  scale_colour_manual(
    values = c("Revenue" = accent_blue, "Profit" = accent_gold)
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = metric_formatter, expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Evolution mensuelle",
    subtitle = "Suivi du chiffre d'affaires et du profit",
    x = NULL,
    y = "Montant"
  ) +
  dashboard_theme()

country_limit <- max(top_countries$Revenue) * 1.14
plot_countries <- ggplot(top_countries, aes(x = Revenue, y = Country)) +
  geom_col(fill = accent_coral, width = 0.68) +
  geom_text(
    aes(label = format_metric(Revenue)),
    hjust = -0.05,
    colour = title_color,
    size = 3.6
  ) +
  scale_x_continuous(
    labels = metric_formatter,
    limits = c(0, country_limit),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Pays leaders",
    subtitle = "Top 6 par chiffre d'affaires",
    x = "Chiffre d'affaires",
    y = NULL
  ) +
  dashboard_theme()

plot_categories <- ggplot(category_long, aes(x = Category, y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.66), width = 0.56) +
  scale_fill_manual(values = c("Revenue" = accent_green, "Profit" = accent_plum)) +
  scale_y_continuous(labels = metric_formatter, expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Categories",
    subtitle = "Comparaison revenu vs profit",
    x = NULL,
    y = "Montant"
  ) +
  dashboard_theme()

subcategory_limit <- max(top_subcategories$Revenue) * 1.15
plot_subcategories <- ggplot(
  top_subcategories,
  aes(x = Revenue, y = `Sub Category`)
) +
  geom_col(fill = accent_plum, width = 0.68) +
  geom_text(
    aes(label = format_metric(Revenue)),
    hjust = -0.05,
    colour = title_color,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = metric_formatter,
    limits = c(0, subcategory_limit),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Sous-categories",
    subtitle = "Top 8 par chiffre d'affaires",
    x = "Chiffre d'affaires",
    y = NULL
  ) +
  dashboard_theme()

card_specs <- list(
  list(title = "Chiffre d'affaires", value = format_metric(total_revenue), note = paste("Categorie leader:", best_category), color = accent_blue),
  list(title = "Profit total", value = format_metric(total_profit), note = paste("Marge:", percent_formatter(total_profit / total_revenue)), color = accent_gold),
  list(title = "Panier moyen", value = format_metric(average_order_value), note = paste(integer_formatter(nrow(sales)), "commandes"), color = accent_coral),
  list(title = "Volumes vendus", value = integer_formatter(units_sold), note = paste("Pays leader:", best_country), color = accent_green)
)

draw_card <- function(spec) {
  grid.roundrect(
    x = 0.5,
    y = 0.5,
    width = 0.96,
    height = 0.88,
    r = unit(0.04, "snpc"),
    gp = gpar(fill = panel_background, col = panel_border, lwd = 1.2)
  )
  grid.roundrect(
    x = 0.08,
    y = 0.72,
    width = 0.03,
    height = 0.42,
    r = unit(0.015, "snpc"),
    gp = gpar(fill = spec$color, col = spec$color)
  )
  grid.text(
    spec$title,
    x = 0.14,
    y = 0.76,
    just = c("left", "center"),
    gp = gpar(col = muted_text, fontsize = 11)
  )
  grid.text(
    spec$value,
    x = 0.08,
    y = 0.48,
    just = c("left", "center"),
    gp = gpar(col = title_color, fontsize = 22, fontface = "bold")
  )
  grid.text(
    spec$note,
    x = 0.08,
    y = 0.2,
    just = c("left", "center"),
    gp = gpar(col = muted_text, fontsize = 10)
  )
}

draw_plot <- function(plot_object, row, col) {
  print(plot_object, vp = viewport(layout.pos.row = row, layout.pos.col = col))
}

png(output_file, width = 2000, height = 1300, res = 160, bg = dashboard_background)

grid.newpage()
pushViewport(
  viewport(
    layout = grid.layout(
      nrow = 4,
      ncol = 12,
      heights = unit(c(1.0, 1.4, 4.3, 4.3), "null"),
      widths = unit(rep(1, 12), "null")
    )
  )
)

pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:12))
grid.text(
  "Analyse des ventes",
  x = 0.02,
  y = 0.72,
  just = c("left", "center"),
  gp = gpar(col = title_color, fontsize = 26, fontface = "bold")
)
grid.text(
  paste(
    "Periode couverte:",
    format(min(sales$Date), "%Y-%m-%d"),
    "au",
    format(max(sales$Date), "%Y-%m-%d"),
    "| Dashboard ggplot2"
  ),
  x = 0.02,
  y = 0.28,
  just = c("left", "center"),
  gp = gpar(col = muted_text, fontsize = 12)
)
popViewport()

for (index in seq_along(card_specs)) {
  left_col <- ((index - 1) * 3) + 1
  right_col <- left_col + 2
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = left_col:right_col))
  draw_card(card_specs[[index]])
  popViewport()
}

draw_plot(plot_monthly, 3, 1:7)
draw_plot(plot_countries, 3, 8:12)
draw_plot(plot_categories, 4, 1:6)
draw_plot(plot_subcategories, 4, 7:12)

dev.off()

cat(sprintf("Lignes analysees : %d\n", nrow(sales)))
cat(sprintf("Chiffre d'affaires total : %s\n", format_metric(total_revenue)))
cat(sprintf("Profit total : %s\n", format_metric(total_profit)))
cat(sprintf("Visualisation ggplot2 generee : %s\n", output_file))
