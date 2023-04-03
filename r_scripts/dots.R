
# DOTS 
# n <- 1000
# max <- 200
# 
# mtrx <- matrix(data = 1:n, ncol = max)
# nrow_mtrx <- nrow(mtrx)
# 
# df <- data.frame(x = rep(1:max, each = nrow_mtrx),
#                  y = rep(1:nrow_mtrx, max))
# 
# sz <- 0.05
# ggplot()+
#   geom_point(data = df, aes(x = x, y = y), size = sz) +
#   theme_classic()+
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank())+
#   ggtitle(paste(as.character(n), "dots"))


# SOURCES:
# facet_grid() code: https://community.rstudio.com/t/normalising-column-width-whilst-using-facet-wrap-and-coord-flip-in-ggplot2/70617/2
# geom_vline() code: https://stackoverflow.com/questions/54655751/ggplot-add-grid-lines-between-bars-groups
# Replace all NA with 0
# https://www.r-bloggers.com/2022/06/replace-na-with-zero-in-r/#:~:text=T2%20R2%20139-,Using%20the%20dplyr%20package%20in%20R%2C%20you%20can%20use%20the,zero%20for%20any%20NA%20values.&text=0)-,To%20replace%20NA%20values%20in%20a%20particular%20column%20of%20a,replace%20NA%20values%20with%20zero.
# Add image to xaxis https://wilkelab.org/ggtext/
# Add image to title: https://takehomessage.com/2019/12/18/r-package-ggtext/

setwd("~/Documents/ww2_casualties/")

library(ggplot2)
library(ggtext)
library(XML)
library(readr)
library(stringr)
library(reshape2)
library(rvest)
library(dplyr)
library(png)
library(jpeg)
library(sigmoid)

options(scipen = 999)

# Paths
img_dir <- "www/"
if (!dir.exists(img_dir)) {dir.create(img_dir)}
data_path <- "data/"

# Files ----

url <- "https://en.wikipedia.org/wiki/World_War_II_casualties"
country_manual_lookup_file <- paste0(data_path, "country_manual_lookup.csv")
allied_axis_lookup_file <- paste0(data_path, "allied_axis_lookup.csv")

# Load files/data ----

country_manual_lookup <- read.csv(country_manual_lookup_file)
allied_axis_lookup <- read.csv(allied_axis_lookup_file)

lines <- rvest::read_html(url)
tab <- data.frame(lines %>% html_node("table") %>% html_table())

# tab <- cbind(readHTMLTable(lines, header=T, which=1,stringsAsFactors=F))

# Clean ----

# Save names
orig_names <- names(tab)

# Change names
names(tab) <- c("country", 
                "pop_39", 
                "mil_all_causes", 
                "civ_military", 
                "civ_other", 
                "total", 
                "pc_of_pop", 
                "av", 
                "mil_wounded")

# Drop Approx. totals row
tab <- subset(tab, country != "Approx. totals")

# Drop average, % and total cols - can work out later
tab <- dplyr::select(tab, -(total), -(pc_of_pop), -(av))

# x <- "Nauru (Australian)AK"
# Remove everything after brackets
tab$country <- gsub("\\(..*", "", tab$country)
# Trim white space
tab$country <- trimws(tab$country)
# Remove capitals at end
tab$country <- sub("[A-Z/]+$", "", tab$country)
# Trim white space
tab$country <- trimws(tab$country)
# Clean other
tab$country <- ifelse(tab$country == "United KingdomBE including Crown Colonies", 
                      "United Kingdom", tab$country)

# Clean footnotes ("[x]")
tab <- data.frame(apply(tab, 2, function(x){ gsub("\\[[0-9/]+\\]", "", x) } ))

# Remove commas
tab <- data.frame(apply(tab, 2, function(x){ gsub(",", "", x) } ) )

# Save version with notes
tab_notes <- tab

# Population
tab$pop_39 <- as.numeric(tab$pop_39)

# Clean military all causes col
# Split by "to"
tab <- tidyr::separate(tab, mil_all_causes, c("min_mil_all_causes", "max_mil_all_causes"), sep = "to")
# Clean - pull numbers
tab$min_mil_all_causes <- as.vector(parse_number(tab$min_mil_all_causes))
tab$max_mil_all_causes <- as.vector(parse_number(tab$max_mil_all_causes))
# Mean of the two cols
tab$mean_mil_all_causes <- rowMeans(tab[c("min_mil_all_causes", "max_mil_all_causes")], na.rm = T)


# Clean civilian deaths due to military
tab <- tidyr::separate(tab, civ_military, c("min_civ_mil", "max_civ_mil"), sep = "to")
# Clean - pull numbers
tab$min_civ_mil <- as.vector(parse_number(tab$min_civ_mil))
tab$max_civ_mil <- as.vector(parse_number(tab$max_civ_mil))
# Mean of the two cols
tab$mean_civ_mil <- rowMeans(tab[c("min_civ_mil", "max_civ_mil")], na.rm = T)

# Clean civilain other causes
tab <- tidyr::separate(tab, civ_other, c("min_civ_other", "max_civ_other"), sep = "to|and")
# Clean - pull numbers
tab$min_civ_other <- as.vector(parse_number(tab$min_civ_other))
tab$max_civ_other <- as.vector(parse_number(tab$max_civ_other))
# Mean of the two cols
tab$mean_civ_other <- rowMeans(tab[c("min_civ_other", "max_civ_other")], na.rm = T)

# Clean military wounded
tab$mil_wounded <- as.vector(parse_number(tab$mil_wounded))

# Calc total, % of pop based on means
tab$total <- rowSums(tab[c("mean_mil_all_causes", "mean_civ_mil", "mean_civ_other")], na.rm = T)
tab$pc_pop <- round((tab$total / tab$pop_39) * 100, 2)

# Arrange cols
tab <- dplyr::select(tab, 
                     country, 
                     pop_39, 
                     min_mil_all_causes, 
                     max_mil_all_causes, 
                     mean_mil_all_causes, 
                     min_civ_mil, 
                     max_civ_mil, 
                     mean_civ_mil, 
                     min_civ_other, 
                     max_civ_other, 
                     mean_civ_other, 
                     total, 
                     pc_pop)

# Clean up rows (countries)
# Remove ones included with others 
included_countries <- tab[grepl("Included", tab_notes$mil_all_causes), "country"]
tab <- subset(tab, !(country %in% included_countries) )

# Remove small population
tab <- subset(tab, pop_39 > 1000000)

# Remove small total deaths
tab <- subset(tab, total > 1000)

# Save and clean countries; make df for lookup later
countries <- tab$country
# Remove space for file names
countries_for_grep <- gsub(" ", "_", countries)

# Combine "civilian deaths due to military activity" and "civilian deaths due to war-related famine and disease"
tab$mean_all_civ <- rowSums( cbind (tab$mean_civ_mil, tab$mean_civ_other), na.rm=TRUE)

# Divide cols with millions by 1m i.e. 25,000,000 -> 25
tab$mean_mil_all_causes_rnd <- tab$mean_mil_all_causes/1000000
tab$mean_civ_mil_rnd <- tab$mean_civ_mil/1000000
tab$total_rnd <- tab$total/1000000
tab$mean_all_civ_rnd <- tab$mean_all_civ/1000000

# Clean
country_df <- data.frame(countries = countries, 
                         search_term = countries_for_grep)

country_df <- subset(country_df, !(countries == "Other nations"))

# Have to manually add search terms for some countries
# Some flags use another country's flag e.g. Ruanda-Urundi is the Belgium flag
# Use this table later for selecting the right flag file
# "left join" but replace values
country_df <- country_df %>% 
  dplyr::rows_update(country_manual_lookup, by = "countries")

# Add axis/allied column 
tab <- merge(tab, allied_axis_lookup, by = "country", all.x = T, sort = F)

# Replace all NA with 0
# https://www.r-bloggers.com/2022/06/replace-na-with-zero-in-r/#:~:text=T2%20R2%20139-,Using%20the%20dplyr%20package%20in%20R%2C%20you%20can%20use%20the,zero%20for%20any%20NA%20values.&text=0)-,To%20replace%20NA%20values%20in%20a%20particular%20column%20of%20a,replace%20NA%20values%20with%20zero.
tab <- tab %>% replace(is.na(.), 0)

# Remove neutral

tab <- subset(tab, !(aa == "Neutral"))

# Flags ----

# Download flags
flag_urls <- lines %>% html_elements("img") %>% html_attr("src")
flag_urls <- grep("Flag|Ensign", flag_urls, value = T)
flag_urls <- gsub("^//", "", flag_urls)
flag_urls <- gsub("\\d+px", "1200px", flag_urls)

flag_not_found <- c()
flag_files <- vector()
label_country_vect <- vector()
for(country_grep in unique(country_df$search_term)){
  flag <- unique(grep(country_grep, flag_urls, value = T))[1]
  file <- paste0(img_dir, country_grep, ".png")
  tryCatch({
    if(!(file.exists(file))){
      download.file(flag, file)
      }
    flag_files <- append(flag_files, file)
  }, error = function(e){
    flag_not_found <<- c(flag_not_found, country_grep)
    cat("ERROR :",conditionMessage(e), "\n")
  })
}

for (file in flag_files){
  img <- readPNG(file)
  file <- gsub(".png", ".jpg", file)
  writeJPEG(img, target = file, quality = 0.10)
}

# Add flag files and html code (for ggplot) to country df
country_df$flag_file <- paste0(img_dir, country_df$search_term, ".jpg")
country_df$flag_file_html <- paste0(country_df$countries, 
                                    " <img src='", 
                                    country_df$flag_file, 
                                    "' width='15' />")

# Set up html labels for plots as named vector SOURCE: https://wilkelab.org/ggtext/
labels <- country_df$flag_file_html
labels <- setNames(labels, country_df$countries)

# Plots ----

# Total
total_plot <- ggplot()+
  theme_classic()+
  geom_bar(data = tab,
           aes(x = reorder(country, total), 
               y = total),
           stat = "identity",
           position = "dodge", 
           fill = "grey")+
  scale_y_continuous(breaks = seq(0, max(tab$total)+1000000, by = 1000000), 
                     labels = scales::label_number_si())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  facet_grid(rows = vars(aa),
             scales = "free_y",
             space = "free_y", 
             switch = "y")+
  geom_vline(xintercept = seq(0.5, length(tab$country), by = 1), 
             color="gray", 
             size=.5, 
             alpha=.5)+
  # Add flags
  scale_x_discrete(name = NULL, 
                   labels = labels)+
  theme(axis.text.y = element_markdown(color = "black", size = 11))+
  xlab("")+
  ylab("Millions")+
  ggtitle("Total deaths, all causes")

ggsave(total_plot, 
       filename = paste0(img_dir, "total_plot.png"))

max_pc_pop <- ceiling(max(tab$pc_pop))

mil_plot <- ggplot()+
  theme_classic()+
  geom_bar(data = tab,
           aes(x = reorder(country, mean_mil_all_causes_rnd), 
               y = mean_mil_all_causes_rnd),
           stat = "identity",
           position = "dodge", 
           fill = "darkgreen")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  facet_grid(rows = vars(aa),
             scales = "free_y",
             space = "free_y", 
             switch = "y")+
  geom_vline(xintercept = seq(0.5, length(tab$country), by = 1), 
             color="gray", 
             size=.5, 
             alpha=.5)+
  # Add flags
  scale_x_discrete(name = NULL, 
                   labels = labels)+
  theme(axis.text.y = element_markdown(color = "black", size = 11))+
  scale_y_continuous(breaks = seq(0, max_pc_pop, by = 1), 
                     limits = c(0, max_pc_pop))+
  xlab("")+
  ylab("Millions")+
  ggtitle("Mean est. military deaths, all causes")

ggsave(mil_plot, 
       filename = paste0(img_dir, "mil_plot.png"))

civ_plot <- ggplot()+
  theme_classic()+
  geom_bar(data = tab,
           aes(x = reorder(country, mean_all_civ_rnd), 
               y = mean_all_civ_rnd),
           stat = "identity",
           position = "dodge", 
           fill = "darkred")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  facet_grid(rows = vars(aa),
             scales = "free_y",
             space = "free_y", 
             switch = "y")+
  geom_vline(xintercept = seq(0.5, length(tab$country), by = 1), 
             color="gray", 
             size=.5, 
             alpha=.5)+
  # Add flags
  scale_x_discrete(name = NULL, 
                   labels = labels)+
  theme(axis.text.y = element_markdown(color = "black", size = 11))+
  scale_y_continuous(breaks = seq(0, max_pc_pop, by = 1), 
                     limits = c(0, max_pc_pop))+
  xlab("")+
  ylab("Millions")+
  ggtitle("Mean est. cililian deaths, all causes, including war-related famine and disease")

ggsave(civ_plot, 
       filename = paste0(img_dir, "civ_plot.png"))

pc_plot <- ggplot()+
  theme_classic()+
  geom_bar(data = tab,
           aes(x = reorder(country, pc_pop), 
               y = pc_pop),
           stat = "identity",
           position = "dodge", 
           fill = "darkblue")+
  facet_grid(rows = vars(aa),
             scales = "free_y",
             space = "free_y", 
             switch = "y")+
  geom_vline(xintercept = seq(0.5, length(tab$country), by = 1), 
             color="gray", 
             size=.5, 
             alpha=.5)+
  # Add flags
  scale_x_discrete(name = NULL, 
                   labels = labels)+
  theme(axis.text.y = element_markdown(color = "black", size = 11))+
  scale_y_continuous(breaks = seq(0, max_pc_pop, by = 1), 
                     limits = c(0, max_pc_pop))+
  coord_flip()+
  xlab("")+
  ylab("Percentage")+
  ggtitle("Percentage deaths of 1939 population")
  
ggsave(pc_plot, 
       filename = paste0(img_dir, "pc_plot.png"))





# DOTS 

# Plot dots "as square as possible"
# For example if plotting 100 dots, dots will be 10 x 10.
# If 1000, will be the floor of the square root (31 x 31), plus the remainder (39) 
# added to the bottom row as a 31 x 8 matrix.

n <- 50000
sqrt_n <- floor(sqrt(n))
remainder <- n - (sqrt_n^2)

# Point size as linear proportion of 1:max n (100k), betweeen 1 and 0.25
# Plot max 100k dots, otherwise will break down - divide by 1000 if over 100k
max_n <- 100000
min_pt_sz <- 0.25

# Linear function
# y = mx + c
x <- 1:max_n
y <- -((1/max_n)*x) + 1
lin_df <- data.frame(x = x, y = y)
lin <- lm(y ~ x, data = lin_df)
# Get size value
sz <- predict(lin, newdata = data.frame(x = n))

# Min point size selection
sz <- max(c(min_pt_sz, sz))

# Code to show flag in plot title
# https://takehomessage.com/2019/12/18/r-package-ggtext/
flag_code <- "Soviet Union <img src='www/Soviet_Union.jpg' width='100' />"

# So for large numbers, divide by 1000 and use 1 dot for 1000

if(n > max_n){
  n <- n/1000
  plot_title <- paste0(flag_code, " ", fmt(n), " dots. 1 dot = 1000 people")
}else{
  plot_title <- paste0(flag_code, " ", fmt(n), " dots.")
}

# Wrangle data for plot using matrices for 'main' square and 'remainder' square, and converting to data frame:

# Main square
mat <- matrix(1, nrow = sqrt_n, 
              ncol = sqrt_n)

# Remainder square
mat_rem <- matrix(rep(NA, remainder),
                  ncol = sqrt_n)

# Fill with 1 up to value of remainder
i <- 0
for(row in 1:nrow(mat_rem)){
  for(col in 1:sqrt_n){
    i <- i + 1
    if(i <= remainder){
      mat_rem[row, col] <- 1
    }
  }
}

# Reverse remainder matrix rows (does not work if just one row)
if(nrow(mat_rem) > 1){
  mat_rem <- mat_rem[nrow(mat_rem):1, ]
}

# Combine main square matrix with remainder matrix
mat <- rbind(mat_rem, mat)

# Put matrix into dataframe
# df <- data.frame(melt(mat_rem, varnames = c("x", "y"), value.name = "z"))
df <- data.frame(melt(mat, varnames = c("x", "y"), value.name = "z"))

# Remove NA values
df <- df[!is.na(df[, "z"]), ]

ggplot()+
  geom_point(data = df, aes(y, x), size = sz, stroke = 0)+
  # ggtitle(flag_code)+
  labs(title = plot_title)+
  theme(axis.line = element_blank(),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_markdown(color = "black", size = 24),
        legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

  
  # ggtitle(paste(as.character(n), "dots", "Soviet Union <img src='www/Soviet_Union.jpg' width='15' />"))
















