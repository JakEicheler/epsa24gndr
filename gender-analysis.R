library(tidyverse)

# install.packages("gender)
library(gender)

#Set Working Directory
# In this directory, you should put Ben's data: 
# https://github.com/benjaminguinaudeau/epsa2024/blob/main/epsa_program.xlsx
setwd("")

#Read in Ben's data
df <- readxl::read_excel("epsa_program.xlsx")

# Read in my handcoded data
manfn <- readxl::read_excel("manual.xlsx") %>% 
  select(-Column1)

# Create the dataframe columns with firstnames
dat <- df %>% mutate(
  years = 2012,
  first_name_p_chair = map_chr(strsplit(p_chair, " "), 1),
  first_name_p_discussant = map_chr(strsplit(p_discussant, " "), 1)
) %>%
  separate(authors, into = c("author1", "author2", "author3", "author4", "author5", "author6", "author7"), sep = ", ", fill = "right") %>%
  mutate(
    first_name_author1 = map_chr(strsplit(author1, " "), 1),
    first_name_author2 = map_chr(strsplit(author2, " "), 1),
    first_name_author3 = map_chr(strsplit(author3, " "), 1),
    first_name_author4 = map_chr(strsplit(author4, " "), 1),
    first_name_author5 = map_chr(strsplit(author5, " "), 1),
    first_name_author6 = map_chr(strsplit(author6, " "), 1),
    first_name_author7 = map_chr(strsplit(author7, " "), 1),
  )

# Create a list of all first names
firstnames <- unique(c(dat$first_name_p_chair, 
                dat$first_name_p_discussant, 
                dat$first_name_author1,
                dat$first_name_author2,
                dat$first_name_author3,
                dat$first_name_author4,
                dat$first_name_author5,
                dat$first_name_author6,
                dat$first_name_author7))

# Use the gender library to classify first names as male/female
# I use the most recent data abailable from social security records in the US
# This is the most closely fitting data in the gender package
fngender <- gender(firstnames, years = 2012, method = "ssa")

#Create a csv file of unclassified names
firstnames %>%
  as.data.frame() %>%
  filter(! firstnames %in% fngender$name) %>%
  write.csv("notclassified.csv")

# Bind rows of manually classified firstnames to the automatically 
# encoded firstnames
missing_cols <- setdiff(names(fngender), names(manfn))
manfn[missing_cols] <- NA
manfn <- manfn[, names(fngender)]
combined_fng <- rbind(fngender, manfn)

# Join the (automatically and manually) classified names to the main dataframe
dat <- dat %>% left_join(combined_fng,
                   by = join_by(first_name_p_chair == name))

# Create a new dataframe with each panel only once, 
# and drop panels without gender data (one Roundtable which had no chair)
datp <- dat %>% 
  distinct(p_nr, .keep_all = TRUE) %>%
  filter(! is.na(gender))

# Table general gender distribution as percentage
table(datp$gender, useNA = "ifany") %>%
  prop.table() %>% 
  round(2)

# Table general gender distribution in absolute terms
table(datp$gender)

# Table absolute number of male/female chairs by section
table(datp$section, datp$gender, useNA = "ifany")

# Create a table as a dataframe for the barplot
contingency_table <- table(datp$section, datp$gender, useNA = "ifany") %>%
  as.data.frame()

# Adjust counts for males to be negative
datp2 <- contingency_table %>%
  mutate(count = ifelse(Var2 == "male", -Freq, Freq)) %>%
  rename(section = Var1,
         gender = Var2)

# Create the diverging bar chart with counts
plot_chgndr <- ggplot(datp2, aes(x = section, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = abs(count)), 
            position = position_stack(vjust = 0.5), 
            size = 3.5) +
  labs(
    title = "Counts of Panel Chair's Gender by Section",
    x = "Section",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(labels = abs) +  # Display absolute values on the y-axis
  theme(
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +   scale_fill_manual(values = c("female" = "#66C2A5", "male" = "#FC8D62"))  # Neutral colors

# Export the barplot
ggsave("plot-chair-gndr.jpg", plot = plot_chgndr, width = 10, height = 6)
