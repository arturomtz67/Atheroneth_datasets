
# Script clean excel dataset

# - Read ALL sheets
# - Treat EVERY cell as STRING
# - Keep blank cells BLANK ("")
# - Keep literal "NA" as "NA" (string)
# - Produce:
#     data_clean/cleaned_long.csv   (dataset, section, variable, visit, value)
#     data_clean/cleaned_wide.xlsx  (one sheet per dataset; visits as columns)


# ---------------- Load and Install packages ----------------
pacman::p_load(readxl, dplyr, tidyr, tidyverse)



# ---------------- 1. Read Excel file and paths----------------
excel_path <- "input/data_sets_wp3.xlsx"
sheets <- excel_sheets(excel_path)
sheets %>% print()

# store the output in the input folder
out_dir    <- "input"                     


out_wide_rds <- file.path(out_dir, "dataset_wide.rds")
out_long_rds <- file.path(out_dir, "dataset_long.rds")
out_list_rds <- file.path(out_dir, "dataset_by_sheet.rds")


# ---------------- 2. Section titles ----------------
sections_raw <- c(
  "General",
  "Cardiovascular risk factors, athropometrics, glucose and lipid",
  "Lifestyle",
  "SES",
  "Inflammation biomarkers",
  "(Gen)Omics & Epigenomics data",
  "Imaging modalities / Surrogate cardiovascular endpoints",
  "Diseases (questionnaire/diagnosis/medication)",
  "Other variables"
)

squish_ <- function(x) x %>% as.character() %>% str_replace_all("\\s+", " ") %>% str_trim()
sections_norm <- sections_raw %>% squish_()


# ---------------- 3. Cleaning function ----------------
clean_sheet <- function(sheet_name, path) {
  
  df0 <- read_excel(path, sheet = sheet_name, col_types = "text", col_names = FALSE) %>%
    mutate(across(everything(), ~ replace_na(.x, "")))   # keep blanks blank
  
  if (nrow(df0) < 2 || ncol(df0) < 2) return(tibble())
  
  # dataset name from A1
  dataset_name <- df0 %>% slice(1) %>% pull(1) %>% as.character() %>% first()
  if (is.na(dataset_name) || dataset_name == "") dataset_name <- sheet_name
  
  # expected column names
  expected <- c("Variable", "Visit_A_Baseline", "Visit_B", "Visit_C", "Visit_D", "Conditions_Availability")
  names(df0) <- expected[seq_len(ncol(df0))]
  
  # remove first row, drop Conditions_Availability
  df <- df0 %>%
    slice(-1) %>%
    select(any_of(c("Variable", "Visit_A_Baseline", "Visit_B", "Visit_C", "Visit_D"))) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(Variable = squish_(Variable))
  
  visit_cols <- intersect(c("Visit_A_Baseline", "Visit_B", "Visit_C", "Visit_D"), names(df))
  
  # Identify section rows
  is_section_by_name <- df %>%
    transmute(is_section = squish_(Variable) %in% sections_norm) %>%
    pull(is_section)
  
  is_section_by_blank <- df %>%
    transmute(variable_not_blank = Variable != "",
              all_visits_blank = if_all(all_of(visit_cols), ~ .x == "")) %>%
    transmute(is_section = variable_not_blank & all_visits_blank) %>%
    pull(is_section)
  
  is_section <- is_section_by_name | is_section_by_blank
  
  df2 <- df %>%
    mutate(Section = if_else(is_section, Variable, NA_character_)) %>%
    tidyr::fill(Section, .direction = "down") %>%
    filter(!is_section) %>%
    mutate(Dataset = dataset_name, .before = 1)
  
  # wide version
  wide <- df2 %>%
    select(Dataset, Section, Variable, all_of(visit_cols))
  
  # long version
  long <- df2 %>%
    pivot_longer(cols = all_of(visit_cols),
                 names_to = "Visit", values_to = "Value") %>%
    mutate(
      Visit = recode(Visit,
                     Visit_A_Baseline = "Visit A (Baseline)",
                     Visit_B = "Visit B",
                     Visit_C = "Visit C",
                     Visit_D = "Visit D")
    ) %>%
    select(Dataset, Section, Variable, Visit, Value)
  
  list(wide = wide, long = long)
}


# ---------------- 4. Process all sheets ----------------
cat("\nProcessing all sheets...\n")
by_sheet <- map(sheets, ~ clean_sheet(.x, excel_path))
names(by_sheet) <- sheets

wide_all <- map(by_sheet, "wide") %>% list_rbind()
long_all <- map(by_sheet, "long") %>% list_rbind()


# ---------------- 5. Summary ----------------
cat("\n=== SUMMARY ===\n")
cat("Datasets: ", n_distinct(wide_all$Dataset), "\n")
cat("Variables (rows, wide): ", nrow(wide_all), "\n")
cat("Rows (long): ", nrow(long_all), "\n")
cat("\nCounts per dataset:\n")
print(wide_all %>% count(Dataset))


# ---------------- 6. Save outputs ----------------
saveRDS(wide_all, out_wide_rds)
cat("\n✓ Saved (wide): ", out_wide_rds, "\n", sep = "")

saveRDS(long_all, out_long_rds)
cat("✓ Saved (long): ", out_long_rds, "\n", sep = "")

saveRDS(by_sheet, out_list_rds)
cat("✓ Saved (list by dataset): ", out_list_rds, "\n", sep = "")

cat("\n✓ Cleaning completed successfully!\n")
