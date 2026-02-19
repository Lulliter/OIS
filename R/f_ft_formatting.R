# ------- Flextable settings ----
# FT cheatsheet https://ardata-fr.github.io/flextable-book/static/pdf/cheat_sheet_flextable.pdf
library(dplyr)
library(stringr)
library(flextable)
library(systemfonts)

# --- Inspect available system fonts (optional) -----------------------------
# ---- VEDERE `_fonts.qmd` ------
# fonts <-  systemfonts::system_fonts() |>
#   # filter if family name contains "condensed"
#   filter(str_detect(family,  "Condensed"))  |>
#   select(name, family, style)

# 1) set target font with safe fallbacks -----------------------------
# verify a font exists on *your* system
f_has_font <- function(family) {
  suppressWarnings({
    fi <- systemfonts::system_fonts()
    any(tolower(fi$family) == tolower(family))
  })
}

# EXE
f_has_font("Roboto Condensed") # TRUE
f_has_font("Segoe UI") # FALSE
f_has_font("Libre Franklin") # TRUE

# Pick preferred or fallback font
f_pick_font <- function(
  candidates = c(
    "Roboto Condensed",
    "Libre Franklin",
    "Open Sans",
    "Segoe UI",
    "Tahoma",
    "Arial"
  )
) {
  for (fam in candidates) {
    if (f_has_font(fam)) return(fam)
  }
  "Arial" # absolute last-resort
}

# Define target font
target_font <- f_pick_font()
message("Using font: ", target_font)

# 2) set flextable defaults -----------------------------------------
set_flextable_defaults(
  line_spacing = 1,
  #scroll =           # NULL or a list if you want to add a scroll-box
  cs.family = target_font, # as decided above
  font.family = target_font, # as decided above
  font.size = 12,
  theme_fun = theme_box, #theme_vanilla,
  #padding = 1,
  padding.bottom = 2,
  padding.top = 2,
  padding.left = 4,
  padding.right = 4,
  background.color = "#F2F2F2",
  border.color = "#A6A6A6",
  border.width = .5,
  table.layout = "autofit" # adapts to content width (works for HTML & Word)
)

# ___ Default numbers format like in Italy ----
set_flextable_defaults(
  digits = 2,
  decimal.mark = ",",
  big.mark = "."
)

# ___ Border style ----
brdr_in <- fp_border_default(color = "#4c4c4c", width = 0.25)

# 3) Funzione CUSTOM per tabelle in Word ----
# f_ft_word <- function(ft) {
#   ft |>
#     set_table_properties(
#       width = 1,
#       layout = "autofit",
#       align = "left") |>
#     fit_to_width(max_width = 6.7) |>
#     autofit(add_w = 0.1, add_h = 0)
# }

f_ft_word <- function(ft) {
  ft |>
    set_table_properties(
      width = 1,
      layout = "autofit", # Word layout mode (diff flextable::autofit)
      align = "left",
      opts_word = list(
        split = FALSE, # <- DO NOT let rows break across pages
        repeat_headers = TRUE # <- repeat header row(s) on subsequent pages
      )
    ) |>
    fit_to_width(max_width = 6.7) |>
    autofit(add_w = 0.1, add_h = 0) # compute widths from content (once)
}

# --- OKKIO: QUESTA E' da aggiungere con -----
# ft |> f_ft_word()  # <-- applica le impostazioni per Word

# ---- OKKIO: non usare doppio caption con tbl-cap e set_caption----
