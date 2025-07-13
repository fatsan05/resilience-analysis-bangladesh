# Install required packages (only run this block once)
packages <- c("tidyverse", "tidytext", "tm", "SnowballC", "pdftools", "stringr", "RColorBrewer")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all libraries necessary for text analysis and visualization
library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(pdftools)
library(stringr)
library(RColorBrewer)

# Specify folder containing PDF files for analysis
path <- "C:/Users/User/Desktop/WAF/Articles"
pdf_files <- list.files(path, pattern = "\\.pdf$", full.names = TRUE)

# Define a list of farming systems and their associated keywords
tag_keywords <- list(
  Agroforestry = c("agroforestry", "treebased", "multistrata", "silvopasture", "homegarden", "intercropping", "alleycropping", "treecrops"),
  Rice_Based = c("rice", "paddy", "bororice", "amanrice", "ausrice", "ricewheat", "irrigatedrice"),
  Maize_Based = c("maize", "maizebased"),
  Livestock_Based = c("livestock", "cattle", "goat", "poultry", "dairy", "grazing", "fodder", "animalhusbandry"),
  Mixed_Farming = c("mixedfarming", "croplivestock", "croptreelivestock", "integratedfarming", "multienterprise"),
  Horticulture = c("horticulture", "fruit", "vegetable", "orchard", "banana", "mango", "spice", "flower"),
  Aquaculture = c("aquaculture", "fish", "fishery", "fisheries", "pisciculture", "shrimp", "pondaquaculture", "ricefish"),
  Climate_Smart = c("climatesmart", "resilientagriculture", "sustainablefarming", "adaptivefarming", "climateadaptation", "lowemissionagriculture"),
  Organic = c("organic", "naturalfarming", "pesticidefree", "compost", "greenmanure", "biologicalpest"),
  Conservation_Ag = c("conservationagriculture", "minimumtillage", "zerotillage", "covercrops", "mulching", "soilconservation", "residueretention"),
  Urban_Ag = c("urbanfarming", "rooftop", "periurban", "kitchengarden", "verticalfarming", "backyard"),
  Irrigated = c("irrigation", "canal", "dripirrigation", "sprinkler", "tubewell", "surfaceirrigation"),
  Other_Crop_Farming = c(
    "wheat", "barley", "sorghum", "millet", "oat", "rye", "triticale",
    "potato", "sweetpotato", "cassava", "yam", "taro", "colocasia", "elephantfootyam",
    "sugarcane", "sugarbeet",
    "mustard", "sunflower", "sesame", "groundnut", "soybean", "linseed", "castor", "safflower", "rapeseed",
    "lentil", "pea", "chickpea", "bean", "mungbean", "blackgram", "pigeonpea", "cowpea", "lablab", "broadbean", "greengram", "splitpea",
    "pumpkin", "amaranth", "spinach", "okra", "bittergourd", "bottlegourd", "ridgegourd", "snakegourd", "cucumber", "carrot", "radish", "turnip", "onion", "garlic", "ginger", "turmeric"
  )
)

# Set up a data frame to store results from all documents
all_words <- tibble()

# Loop through each PDF, extract text, and process content
for (file in pdf_files) {
  pdf_text_raw <- pdf_text(file)
  text_combined <- paste(pdf_text_raw, collapse = " ")
  
  # Remove reference section to focus on main content
  text_before_references <- str_split(text_combined, regex("\\bReferences\\b|\\bREFERENCE\\b|\\bBibliography\\b|\\bWorks Cited\\b|\\bLiterature Cited\\b", ignore_case = TRUE))[[1]][1]
  
  # Create a basic data frame with document ID and extracted text
  docs <- tibble(doc_id = basename(file), text = text_before_references)
  
  # Clean and tokenize the text into individual words
  tidy_doc <- docs %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_replace_all(word, "[^[:alnum:]]", "")) %>%
    filter(str_length(word) > 2) %>%
    mutate(word = tolower(word))
  
  # Append processed words to the main dataset
  all_words <- bind_rows(all_words, tidy_doc)
}

# Function to classify each word into a farming system based on keyword match
theme_assignment <- function(word) {
  for (theme in names(tag_keywords)) {
    if (any(str_detect(word, fixed(tag_keywords[[theme]], ignore_case = TRUE)))) {
      return(theme)
    }
  }
  return(NA)
}

# Assign a farming system label to each word
all_words <- all_words %>%
  mutate(farming_system = sapply(word, theme_assignment))

# Summarize word counts by farming system and keyword
theme_summary <- all_words %>%
  filter(!is.na(farming_system)) %>%
  group_by(farming_system, word) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(farming_system, desc(count))

# Generate a grouped summary paragraph with counts per farming system
label_output <- theme_summary %>%
  group_by(farming_system) %>%
  summarise(labels = paste0(word, "(", count, ")", collapse = ", "),
            total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

# Display the results in a readable format
cat("\n========== FARMING SYSTEM MENTION SUMMARY ==========\n\n")
for (i in 1:nrow(label_output)) {
  cat(paste0(label_output$farming_system[i], " (", label_output$total[i], "): ", label_output$labels[i], "\n\n"))
}

# Define colors to be used in the bar plot
n_systems <- length(unique(label_output$farming_system))
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(n_systems)

# Create a horizontal bar plot of farming system mentions
ggplot(label_output, aes(x = reorder(farming_system, total),
                         y = total,
                         fill = farming_system)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = total),
            hjust = -0.2,
            size = 4) +
  coord_flip() +
  scale_fill_manual(values = mycolors) +
  labs(title = "Farming System Mentions Across Reviewed Articles",
       x = "Farming System",
       y = "Number of Mentions") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 14),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# Export the final plot as an image file
ggsave("Farming_System_Mentions_Corrected.png",
       dpi = 300,
       width = 10,
       height = 8)