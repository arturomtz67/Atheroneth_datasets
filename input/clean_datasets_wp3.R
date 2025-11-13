

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
out_dir <- "input"


out_wide_rds <- file.path(out_dir, "dataset_wide.rds")
out_long_rds <- file.path(out_dir, "dataset_long.rds")
out_list_rds <- file.path(out_dir, "dataset_by_sheet.rds")


# -------------------- 2. Sections --------------------
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

squish_ <- function(x)
  x %>% as.character() %>% str_replace_all("\\s+", " ") %>% str_trim()
sections_norm <- sections_raw %>% squish_()

# -------------------- 3. Cleaning function --------------------
clean_sheet <- function(sheet_name, path) {
  # Read everything as TEXT; blanks -> "" (NA from readxl becomes "")
  df0 <- read_excel(path,
                    sheet = sheet_name,
                    col_types = "text",
                    col_names = FALSE) %>%
    mutate(across(everything(), ~ replace_na(.x, "")))
  
  if (nrow(df0) < 2 || ncol(df0) < 2)
    return(tibble())
  
  # Dataset name from A1 (fallback to sheet name)
  dataset_name <- df0 %>% slice(1) %>% pull(1) %>% as.character() %>% first()
  if (is.na(dataset_name) ||
      dataset_name == "")
    dataset_name <- sheet_name
  
  # Expected columns (A..F)
  expected <- c(
    "Variable",
    "Visit_A_Baseline",
    "Visit_B",
    "Visit_C",
    "Visit_D",
    "Conditions_Availability"
  )
  names(df0) <- expected[seq_len(ncol(df0))]
  
  # Remove first row; drop Conditions_Availability
  df <- df0 %>%
    slice(-1) %>%
    select(any_of(
      c(
        "Variable",
        "Visit_A_Baseline",
        "Visit_B",
        "Visit_C",
        "Visit_D"
      )
    )) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(
      Variable_raw   = Variable,
      Variable_clean = squish_(Variable)       # cleaned text
    )
  
  visit_cols <- intersect(c("Visit_A_Baseline", "Visit_B", "Visit_C", "Visit_D"),
                          names(df))
  
  # ---------- A. True section rows ONLY when the name matches known sections ----------
  df <- df %>%
    mutate(
      is_section = Variable_clean %in% sections_norm,
      Section    = if_else(is_section, Variable_clean, NA_character_)
    ) %>%
    tidyr::fill(Section, .direction = "down") %>%
    filter(!is_section)   # drop the actual section-title rows themselves
  
  # Now Section is ALWAYS one of the canonical section names (or NA),
  # never a variable like "• Age range men" or "o Obesity".
  
  # ---------- B. Handle bullet structure ("•" = main, "o" = sub) ----------
  df_bullets <- df %>%
    mutate(
      text = squish_(Variable_raw),
      bullet_type = case_when(
        str_detect(text, "^•") ~ "main",
        str_detect(text, "^o") ~ "sub",
        TRUE                   ~ "main"
      ),
      var_label = text %>%
        str_remove("^•\\s*") %>%
        str_remove("^o\\s*")
    ) %>%
    mutate(
      Main_variable = if_else(bullet_type == "main", var_label, NA_character_),
      Sub_variable  = if_else(bullet_type == "sub", var_label, NA_character_)
    ) %>%
    tidyr::fill(Main_variable, .direction = "down") %>%
    mutate(Section = squish_(Section), Dataset = dataset_name)
  
  # ---------- C. Build wide and long tables ----------
  wide <- df_bullets %>%
    select(Dataset,
           Section,
           Main_variable,
           Sub_variable,
           all_of(visit_cols))
  
  long <- df_bullets %>%
    pivot_longer(
      cols = all_of(visit_cols),
      names_to = "Visit",
      values_to = "Value"
    ) %>%
    mutate(
      Visit = recode(
        Visit,
        Visit_A_Baseline = "Visit A (Baseline)",
        Visit_B = "Visit B",
        Visit_C = "Visit C",
        Visit_D = "Visit D"
      )
    ) %>%
    select(Dataset, Section, Main_variable, Sub_variable, Visit, Value)
  
  list(wide = wide, long = long)
}
# -------------------- 4. Process all sheets --------------------
cat("\nProcessing all sheets...\n")
by_sheet <- map(sheets, ~ clean_sheet(.x, excel_path))
names(by_sheet) <- sheets

wide_all <- map(by_sheet, "wide") %>% list_rbind()
long_all <- map(by_sheet, "long") %>% list_rbind()

# -------------------- 5. Summary --------------------
cat("\n=== SUMMARY ===\n")
cat("Datasets: ", n_distinct(wide_all$Dataset), "\n")
cat("Main variables (rows, wide): ", nrow(wide_all), "\n")
cat("Rows (long): ", nrow(long_all), "\n")
cat("\nCounts per dataset:\n")
print(wide_all %>% count(Dataset))

# -------------------- 6. Save outputs --------------------
saveRDS(wide_all, out_wide_rds)
cat("\n✓ Saved (wide): ", out_wide_rds, "\n", sep = "")

saveRDS(long_all, out_long_rds)
cat("✓ Saved (long): ", out_long_rds, "\n", sep = "")

saveRDS(by_sheet, out_list_rds)
cat("✓ Saved (list by dataset): ", out_list_rds, "\n", sep = "")

cat("\n✓ Cleaning completed successfully!\n")
