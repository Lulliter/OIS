library(dplyr)
library(skimr)
library(tidyr)
library(scales)  # per la formattazione numerica
library(glue)    # per stringhe dinamiche

# OKKIO --- aggiunto Step 6) con formattazione in stile italiano

# ------ Funzione per calcolare: total_rows | n_distinct | n_missing -----
f_recap_values <- function(data, columns) {
  # Step 0: Controllo che tutte le colonne esistano
  if (!all(columns %in% names(data))) {
    missing_cols <- setdiff(columns, names(data))
    stop(glue("Le seguenti colonne non sono presenti nel dataset: {paste(missing_cols, collapse = ', ')}"))
  }
  
  # Step 1: Seleziona le colonne di interesse
  df_subset <- data %>% select(all_of(columns))
  
  # Step 2: Conta le righe totali
  total_rows <- nrow(df_subset)
  
  # Step 3: Usa skimr per estrarre n_missing
  skimmed <- skim(df_subset)
  
  # Step 4: Calcola n_distinct per ciascuna colonna
  distinct_counts <- df_subset %>%
    summarise(across(everything(), n_distinct)) %>%
    pivot_longer(everything(), names_to = "skim_variable", values_to = "n_distinct")
  
  # Step 5: Costruisce tabella con n_missing + n_distinct + n_dup + total_rows
  summary_tbl <- skimmed %>%
    select(skim_variable, n_missing) %>%
    mutate(total_rows = total_rows) %>%
    left_join(distinct_counts, by = "skim_variable") %>%
    mutate(
      n_dup = total_rows - n_distinct
    ) %>%
    relocate(n_distinct, n_dup, n_missing, .after = total_rows) %>%
    mutate(
      missing_perc = round((n_missing / total_rows) * 100, 1),
      missing_perc = glue("{missing_perc}%")
    )
  
  # Step 6: Applica formattazione numerica in stile italiano
  summary_tbl <- summary_tbl %>%
    mutate(across(
      c(total_rows, n_missing, n_distinct),
      ~ number(.x, big.mark = ".", decimal.mark = ",")
    ))
  
  # Ritorna la tabella
  return(summary_tbl)
}

# ------ Funzione per calcolare con grouping (long-form data) -----
# Applies f_recap_values() iteratively to each group
f_recap_values_grouped <- function(data, columns, group_by) {
  # Step 0: Controllo che tutte le colonne esistano
  all_cols_needed <- c(columns, group_by)
  if (!all(all_cols_needed %in% names(data))) {
    missing_cols <- setdiff(all_cols_needed, names(data))
    stop(glue("Le seguenti colonne non sono presenti nel dataset: {paste(missing_cols, collapse = ', ')}"))
  }

  # Step 1: Ottieni i valori unici dei gruppi
  group_values <- data %>%
    distinct(across(all_of(group_by))) %>%
    arrange(across(all_of(group_by)))

  # Step 2: Applica f_recap_values() per ogni gruppo
  summary_list <- purrr::map_df(
    1:nrow(group_values),
    function(i) {
      # Filtra i dati per il gruppo corrente
      group_data <- data

      for (g in group_by) {
        filter_value <- group_values[[g]][i]
        group_data <- group_data %>% filter(.data[[g]] == filter_value)
      }

      # Applica f_recap_values al sottogruppo
      result <- f_recap_values(group_data, columns)

      # Aggiungi le colonne di gruppo
      for (g in group_by) {
        result[[g]] <- group_values[[g]][i]
      }

      # Aggiungi colonna n_dup (numero di duplicati)
      result <- result %>%
        mutate(
          n_dup = as.numeric(gsub("\\.", "", total_rows)) - as.numeric(gsub("\\.", "", n_distinct)),
          n_dup = number(n_dup, big.mark = ".", decimal.mark = ",")
        )

      return(result)
    }
  )

  # Step 3: Riordina le colonne (gruppo, variabile, total_rows, n_distinct, n_dup, n_missing, missing_perc)
  summary_list <- summary_list %>%
    relocate(all_of(group_by), skim_variable, total_rows, n_distinct, n_dup, n_missing, missing_perc)

  # Ritorna la tabella
  return(summary_list)
}

# ------ Esempio d'uso -----
# f_recap_values(df, c("col1", "col2"))
# f_recap_values_grouped(df, c("col1", "col2"), group_by = "month")

