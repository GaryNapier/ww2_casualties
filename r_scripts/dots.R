

library(ggplot2)

n <- 1000
max <- 200

mtrx <- matrix(data = 1:n, ncol = max)
nrow_mtrx <- nrow(mtrx)

df <- data.frame(x = rep(1:max, each = nrow_mtrx), 
                 y = rep(1:nrow_mtrx, max))

sz <- 0.05
ggplot()+
  geom_point(data = df, aes(x = x, y = y), size = sz) + 
  theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle(paste(as.character(n), "dots"))




library(XML)
library(readr)

url <- "https://en.wikipedia.org/wiki/World_War_II_casualties"

lines <- readLines(url)

lines <- gsub("<span></span>", 1, lines)

tab <- cbind(readHTMLTable(lines, header=T, which=1,stringsAsFactors=F))

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






# Remove all notes

























