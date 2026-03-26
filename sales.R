options(stringsAsFactors = FALSE)

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

# f -----------------------------------------------------------------------


required_columns <- c(
  "Date",
  "Country",
  "Product Category",
  "Sub Category",
  "Revenue",
  "Quantity"
)

missing_columns <- setdiff(required_columns, names(sales))
if (length(missing_columns) > 0) {
  stop(
    sprintf(
      "Colonnes manquantes: %s",
      paste(missing_columns, collapse = ", ")
    )
  )
}

sales <- sales[
  !is.na(sales$Date) &
    !is.na(sales$Country) &
    !is.na(sales[["Product Category"]]) &
    !is.na(sales[["Sub Category"]]) &
    !is.na(sales$Revenue),
]

sales$Date <- as.Date(sales$Date, format = "%m/%d/%Y")
sales$Revenue <- as.numeric(sales$Revenue)
sales$Quantity <- as.numeric(sales$Quantity)

sales <- sales[
  !is.na(sales$Date) &
    !is.na(sales$Revenue) &
    sales$Revenue >= 0,
]

if (nrow(sales) == 0) {
  stop("Aucune ligne exploitable apres nettoyage du dataset.")
}

sales$MonthStart <- as.Date(format(sales$Date, "%Y-%m-01"))

monthly_revenue <- aggregate(Revenue ~ MonthStart, data = sales, sum)
monthly_revenue <- monthly_revenue[order(monthly_revenue$MonthStart), ]

country_revenue <- aggregate(Revenue ~ Country, data = sales, sum)
country_revenue <- country_revenue[order(country_revenue$Revenue, decreasing = TRUE), ]
top_countries <- head(country_revenue, 6)

category_revenue <- aggregate(Revenue ~ `Product Category`, data = sales, sum)
category_revenue <- category_revenue[
  order(category_revenue$Revenue, decreasing = TRUE),
]

subcategory_revenue <- aggregate(Revenue ~ `Sub Category`, data = sales, sum)
subcategory_revenue <- subcategory_revenue[
  order(subcategory_revenue$Revenue, decreasing = TRUE),
]
top_subcategories <- head(subcategory_revenue, 8)

format_millions <- function(values) {
  paste0(format(round(values / 1e6, 1), nsmall = 1, trim = TRUE), " M")
}

background <- "#F7F2EA"
ink <- "#24324A"
grid <- "#D7CFC2"
accent_line <- "#19647E"
accent_bar <- "#E07A5F"
accent_bar_2 <- "#3D9970"
accent_bar_3 <- "#7B4F9E"

png(output_file, width = 1800, height = 1200, res = 150, bg = background)

par(
  mfrow = c(2, 2),
  mar = c(5.5, 5.5, 4, 1.5),
  oma = c(0, 0, 3, 0),
  bg = background,
  col.axis = ink,
  col.lab = ink,
  col.main = ink
)

plot(
  monthly_revenue$MonthStart,
  monthly_revenue$Revenue,
  type = "o",
  pch = 16,
  lwd = 2.5,
  col = accent_line,
  xlab = "Mois",
  ylab = "Chiffre d'affaires",
  main = "Evolution mensuelle du CA",
  xaxt = "n"
)
grid(nx = NA, ny = NULL, col = grid, lty = 1)
axis.Date(
  1,
  at = seq(
    from = min(monthly_revenue$MonthStart),
    to = max(monthly_revenue$MonthStart),
    by = "3 months"
  ),
  format = "%b\n%Y"
)

country_values <- rev(top_countries$Revenue / 1e6)
country_names <- rev(top_countries$Country)
barplot(
  country_values,
  names.arg = country_names,
  horiz = TRUE,
  las = 1,
  col = accent_bar,
  border = NA,
  xlab = "Chiffre d'affaires (millions)",
  main = "Top pays par chiffre d'affaires"
)
grid(nx = NULL, ny = NA, col = grid, lty = 1)

category_values <- category_revenue$Revenue / 1e6
bar_positions <- barplot(
  category_values,
  names.arg = category_revenue$`Product Category`,
  col = accent_bar_2,
  border = NA,
  ylab = "Chiffre d'affaires (millions)",
  main = "Poids des categories"
)
text(
  x = bar_positions,
  y = category_values,
  labels = format_millions(category_revenue$Revenue),
  pos = 3,
  cex = 0.9,
  col = ink
)
grid(nx = NA, ny = NULL, col = grid, lty = 1)

subcategory_values <- rev(top_subcategories$Revenue / 1e6)
subcategory_names <- rev(top_subcategories$`Sub Category`)
barplot(
  subcategory_values,
  names.arg = subcategory_names,
  horiz = TRUE,
  las = 1,
  col = accent_bar_3,
  border = NA,
  xlab = "Chiffre d'affaires (millions)",
  main = "Top sous-categories"
)
grid(nx = NULL, ny = NA, col = grid, lty = 1)

mtext(
  "Analyse des ventes - vue d'ensemble",
  outer = TRUE,
  line = 1,
  cex = 1.5,
  font = 2,
  col = ink
)

dev.off()

cat(sprintf("Lignes analysees : %d\n", nrow(sales)))
cat(
  sprintf(
    "Periode couverte : %s a %s\n",
    format(min(sales$Date), "%Y-%m-%d"),
    format(max(sales$Date), "%Y-%m-%d")
  )
)
cat(
  sprintf(
    "Pays leader : %s (%s)\n",
    top_countries$Country[1],
    format_millions(top_countries$Revenue[1])
  )
)
cat(
  sprintf(
    "Categorie leader : %s (%s)\n",
    category_revenue$`Product Category`[1],
    format_millions(category_revenue$Revenue[1])
  )
)
cat(sprintf("Visualisation generee : %s\n", output_file))

# Run ---------------------------------------------------------------------


