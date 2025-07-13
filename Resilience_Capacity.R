### Step 1:  Required Libraries
packages <- c("tidyverse", "tidytext", "tm", "SnowballC", "pdftools", "stringr", "stringdist",
              "kableExtra", "ggplot2", "reshape2", "readr", "plotly", "rgl")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(pdftools)
library(stringr)
library(stringdist)
library(kableExtra)
library(ggplot2)
library(reshape2)
library(plotly)
library(rgl)

### Step 2: Intervention to Resilience Mapping
intervention_resilience_mapping <- read_csv("C:/Users/User/Desktop/WAF/intervention.csv")

### Step 3: Define PDF Directory (put yours)
pdf_dir <- "C:/Users/User/Desktop/WAF/Articles"

article_citations <- tribble(
  ~title_key, ~citation,
  "A comprehensive review on sustainable coastal zone management in Bangladesh Present status and the way forward", "Shampa et al. (2023)",
  "A review of watershed management in Bangladesh Options, challenges and legal framework", "Hassan et al. (2024)",
  "AGROFORESTRY PRACTICES FOR SUSTAINABLE PRODUCTION IN BANGLADESH A REVIEW", "Saha et al. (2022)",
  "Building Coastal Agricultural Resilience in Bangladesh A Systematic Review of Progress, Gaps and Implications", "Kundu et al. (2020)",
  "Challenges and Adaptations for Resilient Rice Production under Changing Environments in Bangladesh", "Jamal et al. (2023)",
  "Climate Change Trends and Vulnerabilities in Bangladesh's Crop Sector A Review of Crop Production Challenges and Resilience Strategies", "Uddin & Hoque (2025)",
  "Crop diversification in Bangladesh Public policy provisions, practices, and insights to future initiative", "Nandi et al. (2024)",
  "Farmers' adaptation practices in climate-stressed coastal Bangladesh a systematic review", "Ashik Ur Rahman et al. (2025)",
  "Food and health security impact of climate change in Bangladesh a review", "Moon (2023)",
  "Impact of climate change on livestock production in Bangladesh - A review", "Naim et al. (2023)",
  "Impacts of climate change on food system security and sustainability in Bangladesh", "Rahman et al. (2024)",
  "Literature review of climate resilient agri-food systems in the coastal zone of Bangladesh", "Ali et al. (2024)",
  "Potentiality of homestead agroforestry for achieving sustainable development goals Bangladesh perspectives", "Ruba & Talucder (2023)",
  "THE ROLE OF AGROFORESTRY APPROACH AS A POTENTIAL TOOL FOR ATTAINING CLIMATE SMART AGRICULTURE FRAMEWORK BANGLADESH PERSPECTIVES", "Talucder & Ruba (2023)",
  "The state of climate change adaptation research in Bangladesh a systematic literature review", "Morshed et al. (2025)"
)

### Step 4: Normalizer
normalize_text <- function(text) {
  text %>%
    tolower() %>%
    str_replace_all("[:punct:]", " ") %>%
    str_squish()
}

### Step 5: Process PDFs
all_docs <- tibble()
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

for (file in pdf_files) {
  tryCatch({
    raw_text <- pdf_text(file)
    combined_text <- paste(raw_text, collapse = " ")
    main_text <- str_split(combined_text, regex("\\bReferences\\b|\\bBibliography\\b", ignore_case = TRUE))[[1]][1]
    
    meta_info <- pdf_info(file)
    doc_title <- if ("Title" %in% names(meta_info$keys)) meta_info$keys$Title else basename(file)
    doc_title_norm <- normalize_text(doc_title)
    
    citation_key <- article_citations %>%
      mutate(norm_title = normalize_text(title_key),
             dist = stringdist(doc_title_norm, norm_title, method = "jw")) %>%
      arrange(dist) %>%
      slice(1) %>%
      pull(citation)
    
    tidy_doc <- tibble(text = main_text) %>%
      unnest_tokens(word, text) %>%
      mutate(word = str_remove_all(word, "[^[:alnum:]]"),
             word = tolower(word),
             doc_id = citation_key) %>%
      filter(str_length(word) > 3)
    
    all_docs <- bind_rows(all_docs, tidy_doc)
  }, error = function(e) {
    message(sprintf("❌ Error processing %s: %s", basename(file), e$message))
  })
}

### Step 6: Match Interventions
matched_data <- all_docs %>%
  inner_join(intervention_resilience_mapping, by = c("word" = "intervention")) %>%
  group_by(doc_id, typology, resilience_capacity) %>%
  summarise(count = n(), .groups = "drop")

### Step 7: Creating Table
intervention_table <- matched_data %>%
  pivot_wider(names_from = typology, values_from = count, values_fill = 0)

intervention_table %>%
  kbl(caption = "Table 1: Categorization of Climate Resilience Interventions by Typology and Article", align = "c") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

### Step 8: 2D Plotting
stacked_summary <- matched_data %>%
  group_by(resilience_capacity, typology) %>%
  summarise(Total_Mentions = sum(count), .groups = "drop")

# 2D Bar Plot
ggplot(stacked_summary, aes(x = resilience_capacity, y = Total_Mentions, fill = typology)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Typology Composition within Resilience Capacities",
    x = "Resilience Capacity",
    y = "Total Mentions",
    fill = "Intervention Typology"
  ) +
  theme_minimal()

ggsave("Stacked_Resilience_Typology_BarPlot.png", dpi = 400, width = 10, height = 6, bg = "white")
