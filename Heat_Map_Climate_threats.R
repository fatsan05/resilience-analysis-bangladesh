# Load all necessary packages, installing any that are missing
packages <- c("tidyverse", "tidytext", "tm", "SnowballC", "pdftools", "stringr", "RColorBrewer", "reshape2", "stringdist")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
install.packages(c("dplyr", "tidyr", "knitr", "kableExtra"))

# Load the libraries
library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(pdftools)
library(stringr)
library(RColorBrewer)
library(reshape2)
library(stringdist)

# Set the folder location containing the PDF articles
path <- "C:/Users/User/Desktop/WAF/Articles"
pdf_files <- list.files(path, pattern = "\\.pdf$", full.names = TRUE)

# List of article titles and their corresponding citations
citation_db <- tribble(
  ~title_key, ~citation,
  "A comprehensive review on sustainable coastal zone management in Bangladesh Present status and the way forward", "Shampa et al. (2023)",
  "A review of watershed management in Bangladesh Options, challenges and legal framework", "Hassan et al. (2024)",
  "AGROFORESTRY PRACTICES FOR SUSTAINABLE PRODUCTION IN BANGLADESH A REVIEW", "Saha et al. (2022)",
  "Building Coastal Agricultural Resilience in Bangladesh A Systematic Review of Progress, Gaps and Implications", "Kundu et al. (2020)",
  "Challenges and Adaptations for Resilient Rice Production under Changing Environments in Bangladesh", "Jamal et al. (2023)",
  "Climate Change Trends and Vulnerabilities in Bangladesh’s Crop Sector A Review of Crop Production Challenges and Resilience Strategies", "Uddin & Hoque (2025)",
  "Farmers’ adaptation practices in climate-stressed coastal Bangladesh a systematic review", "Nandi et al. (2024)",
  "Food and health security impact of climate change in Bangladesh a review", "Rahman et al. (2025)",
  "Impact of climate change on livestock production in Bangladesh - A review", "Moon (2023)",
  "Impacts of climate change on food system security and sustainability in Bangladesh", "Naim et al. (2023)",
  "Literature review of climate resilient agri-food systems in the coastal zone of Bangladesh", "Rahman et al. (2024)",
  "Potentiality of homestead agroforestry for achieving sustainable development goals Bangladesh perspectives", "Ruba & Talucder (2023)",
  "THE ROLE OF AGROFORESTRY APPROACH AS A POTENTIAL TOOL FOR ATTAINING CLIMATE SMART AGRICULTURE FRAMEWORK BANGLADESH PERSPECTIVES", "Talucder & Ruba (2023)",
  "The state of climate change adaptation research in Bangladesh a systematic literature review", "Morshed et al. (2025)"
)

# Helper function to standardize and clean text for title comparison
normalize_title <- function(text) {
  text %>%
    tolower() %>%
    str_replace_all("[:punct:]", " ") %>%
    str_squish()
}

# Define the different climate threats and associated keywords
climate_threats <- list(
  Salinity_Intrusion = c(
    "salinity", "salinization", "soil salinization", "saltwater intrusion", "saline intrusion",
    "saline encroachment", "salt accumulation", "salt-affected", "salinised", "saline waterlogging"
  ),
  Flooding = c(
    "flood", "floods", "flooding", "flash flood", "flash floods", "tidal surge",
    "tidal flooding", "storm flooding", "coastal inundation", "inundation", "riverine flooding",
    "urban flooding", "pluvial flooding", "floodplain"
  ),
  Cyclones = c(
    "cyclone", "cyclones", "tropical cyclone", "tropical cyclones", "storm surge",
    "storm surges", "hurricane", "hurricanes", "typhoon", "typhoons", "windstorm", "gale force winds",
    "severe storm", "low pressure system"
  ),
  Sea_Level_Rise = c(
    "sea level rise", "rising sea levels", "coastal erosion", "shoreline erosion",
    "land loss", "increased sea level", "coastal retreat", "relative sea level rise",
    "ocean encroachment", "encroaching sea", "submergence"
  ),
  Drought = c(
    "drought", "droughts", "dry spell", "dry spells", "water scarcity", "moisture stress",
    "agricultural drought", "meteorological drought", "prolonged dryness", "hydrological drought",
    "rainfall deficit", "crop failure due to drought"
  ),
  Temperature_Rise = c(
    "temperature rise", "rising temperatures", "heatwave", "heatwaves", "extreme heat",
    "warming trend", "global warming", "hot weather", "temperature anomaly", "thermal stress",
    "warming climate", "elevated temperature", "high temperature stress"
  ),
  Rainfall_Variability = c(
    "rainfall variability", "variable rainfall", "irregular rainfall", "erratic rainfall",
    "monsoon delay", "delayed rainfall", "shifting monsoon", "monsoon failure",
    "unpredictable rainfall", "changing precipitation", "inconsistent rainfall",
    "precipitation irregularity"
  ),
  River_Erosion = c(
    "river erosion", "bank erosion", "channel shifting", "riverbank failure",
    "fluvial erosion", "river migration", "channel migration", "cutbank erosion"
  ),
  Waterlogging = c(
    "waterlogging", "water logged", "logged soil", "standing water", "prolonged inundation",
    "excess soil moisture", "drainage congestion", "soaked field", "saturated soil"
  ),
  Saline_Water = c(
    "saline water", "salty water", "brackish water", "salinized water",
    "salt-contaminated water", "high salinity water","saltwater intrusion", "sea water intrusion", "seawater contamination"
  )
)

# Identify which threat category a word belongs to
assign_threat <- function(word) {
  for (threat in names(climate_threats)) {
    if (any(str_detect(word, fixed(climate_threats[[threat]], ignore_case = TRUE)))) {
      return(threat)
    }
  }
  return(NA)
}

# Prepare an empty data structure to store all extracted words
all_docs <- tibble()

# Loop through each PDF file and extract, clean, and match content
for (file in pdf_files) {
  tryCatch({
    pdf_text_raw <- pdf_text(file)
    text_combined <- paste(pdf_text_raw, collapse = " ")
    text_main <- str_split(text_combined, regex("\\bReferences\\b|\\bBibliography\\b", ignore_case = TRUE))[[1]][1]
    
    meta_info <- pdf_info(file)
    doc_title <- if ("Title" %in% names(meta_info$keys)) meta_info$keys$Title else basename(file)
    doc_title_norm <- normalize_title(doc_title)
    
    citation_key <- citation_db %>%
      mutate(norm_title = normalize_title(title_key),
             dist = stringdist(doc_title_norm, norm_title, method = "jw")) %>%
      arrange(dist) %>%
      slice(1) %>%
      pull(citation)
    
    if (is.na(citation_key) || citation_key == "") {
      citation_key <- tools::file_path_sans_ext(basename(file))
      message(sprintf("⚠️ No good match found for: %s", citation_key))
    }
    
    tidy_doc <- tibble(text = text_main) %>%
      unnest_tokens(word, text) %>%
      mutate(
        word = str_remove_all(word, "[^[:alnum:]]"),
        word = tolower(word),
        doc_id = citation_key
      ) %>%
      filter(str_length(word) > 2)
    
    all_docs <- bind_rows(all_docs, tidy_doc)
  }, error = function(e) {
    message(sprintf("❌ Error processing %s: %s", basename(file), e$message))
  })
}

# Classify and count threat terms in each document
threat_data <- all_docs %>%
  mutate(threat_type = map_chr(word, assign_threat)) %>%
  filter(!is.na(threat_type)) %>%
  group_by(doc_id, threat_type) %>%
  summarise(count = n(), .groups = "drop")

# Convert the data into a matrix format
threat_matrix <- threat_data %>%
  pivot_wider(names_from = threat_type, values_from = count, values_fill = 0) %>%
  column_to_rownames("doc_id")

# Reshape data for plotting
heat_data <- melt(as.matrix(threat_matrix), varnames = c("Article", "Threat"))

# Load table and plotting packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Create a formatted table from the threat counts
table_data <- heat_data %>%
  pivot_wider(names_from = Threat, values_from = value, values_fill = 0)

table_data %>%
  kbl(caption = "Table 1: Number of Climate Threat Mentions per Article", align = "c") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Generate the heatmap showing threat frequencies per article
ggplot(heat_data, aes(x = Threat, y = Article, fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(value > 0, value, "")),
            color = "black", size = 2.5, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
    name = "Mentions"
  ) +
  labs(
    title = "Climate Threat Mentions in Coastal Agriculture Literature",
    x = "Climate Threats",
    y = "Research Articles"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    axis.text.y = element_text(size = 8, face = "italic"),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10),
    panel.border = element_rect(color = "gray90", fill = NA)
  ) +
  coord_equal(ratio = 0.6)

# Save the final plot as an image
ggsave("Climate_Threats_Heatmap_Red.png", dpi = 400, width = 14, height = 10, bg = "white")
