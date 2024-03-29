#== Tallying "Checkbox" Survey Responses: R Walkthrough ==#

# Loading the appropriate libraries in====
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Loading the data into the session====
Colors <- read_csv("Data/Colors.csv", col_types = cols(`What colors do you like?` = col_character()))

#Adding a Tidyr solution with seperate rows====
# The function "Separate_rows" can be used to tally checkbox results===
TidyrColors <- Colors %>%
  separate_rows(`What colors do you like?`, sep = ", ") %>%
  count(`What colors do you like?`, sort = TRUE, name = "Tally")

#=====More Involved Ways for String/Regex/Loop Practice=====#

# Creating a reference vector that contains all possible color choices/responses====
Colorref <- c("Red","Blue","Green","Yellow","Black","Orange","Brown","Pink")

# Creating an empty dataframe with the number of columns matching the number of options, and the number of rows that match the number of observations we have, afterwards, we can rename the columns to be our color choices====
Colorsnew <- as.data.frame(matrix(ncol = length(Colorref), nrow = nrow(Colors)))
names(Colorsnew) <- Colorref

# Creating a for-loop to use grepl and ifelse function to scan our original strings and populate our new data set with a raw tally of our observations. Each string that is found produces a "1", each non-match produces a "0"====
for (i in seq_along(Colorref)){
  Colorsnew[i] = ifelse(grepl(Colorref[i],Colors$`What colors do you like?`),1,0)
}

# Creating a for-loop to use stringr and ifelse functions to scan our original strings and populate our new data set with a raw tally of our observations. Each string that is found produces a "1", each non-match produces a "0" This is functionally duplicative of the above code. You can choose which method you like best.====
for (i in seq_along(Colorref)){
  Colorsnew[i] = ifelse(str_detect(Colors$`What colors do you like?`,Colorref[i]),1,0)
}

# Transposing the data into a "long format", grouping the data set by "color", then collapsing the sum of these tallies by color with the summarise function====
Colorsnew <- Colorsnew %>% 
  pivot_longer(everything(),names_to = "Color", values_to = "Tally") %>%
  group_by(Color) %>%
  summarise(Tally = sum(Tally))
