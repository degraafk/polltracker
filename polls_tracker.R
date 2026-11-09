# =============================================================================
# Opinion polling tracker – 2026 Spanish general election
# Versión con Frente Amplio = FA + Front València (siempre conjunto)
# Gráfico: línea simple que conecta los puntos (sin loess)
# =============================================================================

# install.packages(c("rvest", "xml2", "dplyr", "tidyr", "stringr",
#                    "lubridate", "ggplot2", "purrr"))

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(purrr)

url      <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2026_Spanish_general_election"
election <- as.Date("2026-11-29")

# ---- 1. Read page -----------------------------------------------------------
page  <- read_html(url)
nodes <- html_elements(page, "table.wikitable")
cat("Found", length(nodes), "wikitable(s)\n")

# ---- Helpers ----------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# Extract header names from the first row (prefer image alt)
get_headers <- function(node) {
  rows <- xml_find_all(node, ".//tr")
  if (length(rows) == 0) return(character(0))
  cells <- xml_find_all(rows[[1]], "./th|./td")
  map_chr(cells, function(cell) {
    img <- xml_find_first(cell, ".//img")
    if (!inherits(img, "xml_missing")) {
      alt <- xml_attr(img, "alt")
      if (!is.null(alt) && nzchar(alt)) return(str_squish(alt))
      title <- xml_attr(img, "title")
      if (!is.null(title) && nzchar(title)) return(str_squish(title))
    }
    str_squish(html_text2(cell))
  })
}

get_year <- function(node) {
  h <- xml_find_first(node, "preceding::*[self::h2 or self::h3 or self::h4 or self::h5][1]")
  if (inherits(h, "xml_missing")) return(NA_integer_)
  as.integer(str_extract(html_text2(h), "20\\d{2}"))
}

# Turn a table node into a character matrix.
# Cells with colspan > 1 keep the text in the first column and the remaining
# columns are marked "SPAN", so columns stay aligned with the headers.
node_to_matrix <- function(node) {
  rows <- xml_find_all(node, ".//tr")
  if (length(rows) < 2) return(NULL)
  
  row_list <- map(rows, function(r) {
    cells <- xml_find_all(r, "./th|./td")
    out <- character(0)
    for (cell in cells) {
      txt <- str_squish(html_text2(cell))
      cs  <- suppressWarnings(as.integer(xml_attr(cell, "colspan")))
      if (is.na(cs)) cs <- 1L
      out <- c(out, txt, rep("SPAN", cs - 1L))
    }
    out
  })
  
  max_cols <- max(map_int(row_list, length))
  if (max_cols < 4) return(NULL)
  
  row_list <- map(row_list, function(x) {
    if (length(x) < max_cols) c(x, rep("", max_cols - length(x))) else x
  })
  
  do.call(rbind, row_list)
}

# ---- 2. Identify useful tables ----------------------------------------------
headers_list <- map(nodes, get_headers)
years        <- map_int(nodes, get_year)

is_main <- map_lgl(seq_along(nodes), function(i) {
  hdrs <- headers_list[[i]]
  length(hdrs) >= 6 &&
    (str_detect(tolower(hdrs[1] %||% ""), "polling firm|commissioner") ||
       any(str_detect(toupper(hdrs), "^(PP|PSOE|VOX)$")))
})

use <- which(is_main & !is.na(years) & years >= 2023)
if (length(use) == 0) use <- which(is_main)[1:min(5, sum(is_main))]

cat("Using tables:", paste(use, collapse = ", "),
    "(years:", paste(years[use], collapse = ", "), ")\n\n")

# Para comprobar cómo se llaman las cabeceras (por ejemplo Front València):
# print(headers_list[[use[1]]])

# ---- 3. Parsing helpers -----------------------------------------------------
share <- function(x) {
  x <- as.character(x)
  num <- str_extract(x, "^\\d{1,2}(?:\\.\\d)?")
  suppressWarnings(as.numeric(num))
}

parse_mid <- function(txt, yr) {
  if (is.na(txt) || !nzchar(as.character(txt))) return(as.Date(NA))
  txt <- str_remove_all(as.character(txt), "\\[[^]]*\\]")
  txt <- str_squish(str_replace_all(txt, "[–—−]", "-"))
  parts <- str_split(txt, "-")[[1]] |> str_squish()
  
  end_txt   <- parts[length(parts)]
  start_txt <- parts[1]
  
  ed <- as.integer(str_extract(end_txt, "\\d{1,2}"))
  em <- match(str_to_title(str_sub(str_extract(end_txt, "[A-Za-z]+"), 1, 3)), month.abb)
  if (is.na(ed) || is.na(em)) return(as.Date(NA))
  
  sd <- as.integer(str_extract(start_txt, "\\d{1,2}"))
  sm <- match(str_to_title(str_sub(str_extract(start_txt, "[A-Za-z]+"), 1, 3)), month.abb)
  if (is.na(sm)) sm <- em
  if (is.na(sd)) sd <- ed
  
  end_date   <- make_date(yr, em, ed)
  start_year <- if (sm > em) yr - 1L else yr
  start_date <- make_date(start_year, sm, sd)
  
  start_date + floor(as.numeric(end_date - start_date) / 2)
}

# ---- 4. Parse one table -----------------------------------------------------
parse_table <- function(node, hdrs, yr) {
  mat <- node_to_matrix(node)
  if (is.null(mat) || nrow(mat) < 2) return(NULL)
  
  # Drop header-looking rows
  firm <- mat[, 1]
  keep <- !str_detect(firm, regex("^Polling firm|^Fieldwork|^Sample size|^Turnout|^$|election result",
                                  ignore_case = TRUE)) &
    nzchar(firm)
  
  mat <- mat[keep, , drop = FALSE]
  if (nrow(mat) == 0) return(NULL)
  
  # Normalise headers
  norm <- function(h) {
    h <- str_to_upper(str_squish(h))
    case_when(
      h %in% c("PP")                           ~ "PP",
      h %in% c("PSOE")                         ~ "PSOE",
      h %in% c("VOX")                          ~ "Vox",
      h %in% c("FA", "FRENTE AMPLIO", "SUMAR") ~ "Frente Amplio",
      str_detect(h, "^FRONT")                  ~ "Front Valencia",
      h %in% c("ERC")                          ~ "ERC",
      h %in% c("JUNTS")                        ~ "Junts",
      h %in% c("EH BILDU", "BILDU")            ~ "EH Bildu",
      h %in% c("PNV", "EAJ-PNV")               ~ "PNV",
      h %in% c("PODEMOS")                      ~ "Podemos",
      h %in% c("SALF")                         ~ "SALF",
      TRUE ~ h
    )
  }
  hdrs_norm <- map_chr(hdrs, norm)
  
  # Make sure header vector matches matrix columns
  n_cols <- ncol(mat)
  if (length(hdrs_norm) > n_cols) hdrs_norm <- hdrs_norm[1:n_cols]
  if (length(hdrs_norm) < n_cols) hdrs_norm <- c(hdrs_norm, rep("EXTRA", n_cols - length(hdrs_norm)))
  
  get_party <- function(name) {
    j <- which(hdrs_norm == name)[1]
    if (is.na(j) || j > n_cols) return(rep(NA_real_, nrow(mat)))
    share(mat[, j])
  }
  
  # --- Frente Amplio = FA + Front València ---
  # Si FV está fusionada con FA (celda con colspan), el dato ya es conjunto.
  # Si van separadas, se suman. Si FV no existe o es "—", cuenta como 0.
  # Si FV es "?", el dato es nulo: esa fila se ignora para Frente Amplio.
  fa      <- get_party("Frente Amplio")
  fv      <- get_party("Front Valencia")
  j_fv    <- which(hdrs_norm == "Front Valencia")[1]
  fv_span <- if (is.na(j_fv)) rep(FALSE, nrow(mat)) else mat[, j_fv] == "SPAN"
  fv_q    <- if (is.na(j_fv)) rep(FALSE, nrow(mat)) else str_detect(mat[, j_fv], "^\\?")
  
  fa_total <- ifelse(is.na(fa) | fv_q, NA_real_,
                     fa + ifelse(fv_span, 0, coalesce(fv, 0)))
  
  tibble(
    date            = as.Date(map_dbl(mat[, 2], ~ as.numeric(parse_mid(.x, yr))),
                              origin = "1970-01-01"),
    pollster        = str_squish(str_remove_all(mat[, 1], "\\[[^]]*\\]")),
    n               = suppressWarnings(as.numeric(str_replace_all(mat[, 3], "[^0-9]", ""))),
    PP              = get_party("PP"),
    PSOE            = get_party("PSOE"),
    Vox             = get_party("Vox"),
    `Frente Amplio` = fa_total,
    fa_format       = case_when(            # diagnostic: how FA was parsed
      fv_q    ~ "FV unknown",
      fv_span ~ "joint",
      TRUE    ~ "separate"),
    ERC             = get_party("ERC"),
    Junts           = get_party("Junts"),
    `EH Bildu`      = get_party("EH Bildu"),
    PNV             = get_party("PNV"),
    Podemos         = get_party("Podemos"),
    SALF            = get_party("SALF")
  ) |>
    filter(!is.na(date))
}

# ---- 5. Build the dataset ---------------------------------------------------
polls_list <- map(use, function(i) {
  cat("Parsing table", i, "(year", years[i], ") ... ")
  res <- tryCatch(
    parse_table(nodes[[i]], headers_list[[i]], years[i] %||% 2026),
    error = function(e) {
      cat("FAILED:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (is.null(res)) cat("0 rows\n") else cat(nrow(res), "rows\n")
  res
})

polls_list <- polls_list[!map_lgl(polls_list, is.null)]

if (length(polls_list) == 0) stop("All tables failed to parse.")

polls <- bind_rows(polls_list) |>
  arrange(desc(date)) |>
  distinct(date, pollster, .keep_all = TRUE)

# Encuestadoras excluidas (peso 0). Coincidencia EXACTA (sin distinguir
# mayúsculas), así que variantes como "CIS/Opina 360" se mantienen en el modelo.
# "\u00e9" = é (evita problemas de codificación del archivo).
excluded_pollsters <- c("PSOE", "The Objective", "CIS",
                        "SW Demoscopia/La S\u00e9ptima", "VOX/The Objective", "SocioMétrica/PP")

n_before <- nrow(polls)
polls <- polls |>
  filter(!str_to_lower(pollster) %in% str_to_lower(excluded_pollsters))
cat("Excluded", n_before - nrow(polls), "poll(s) from:",
    paste(excluded_pollsters, collapse = ", "), "\n")

cat("\nTotal polls:", nrow(polls),
    " from", format(min(polls$date, na.rm = TRUE)),
    "to",   format(max(polls$date, na.rm = TRUE)), "\n\n")

print(head(polls, 10))

# Diagnostic: check FA = FA + FV parsing (comment out once verified)
polls |>
  select(date, pollster, `Frente Amplio`, fa_format) |>
  print(n = 30)

# ---- 6. Long format + plot --------------------------------------------------
start_date <- as.Date("2026-01-01")   # only polls from this date onwards are plotted

parties <- c("PP", "PSOE", "Vox", "Frente Amplio", "ERC", "Junts",
             "EH Bildu", "PNV", "Podemos", "SALF")

long <- polls |>
  pivot_longer(all_of(parties), names_to = "party", values_to = "share") |>
  filter(!is.na(share))

main <- c("PP", "PSOE", "Vox", "Frente Amplio", "Podemos")
cols <- c(
  PP              = "#1F78C8",
  PSOE            = "#E8222B",
  Vox             = "#5FB82C",
  `Frente Amplio` = "#ff430a",
  Podemos         = "#8A5CF6"
)

plot_data <- long |>
  filter(party %in% main, date >= start_date)

month_starts <- seq(
  floor_date(start_date, "month"),
  max(election, max(polls$date, na.rm = TRUE)),
  by = "month"
)

p <- ggplot(plot_data, aes(date, share, colour = party)) +
  geom_vline(xintercept = month_starts, colour = "grey93", linewidth = 0.2) +
  geom_point(alpha = 0.3, size = 0.6) +
  geom_line(linewidth = 0.55) +
  geom_vline(xintercept = election, linetype = "solid", colour = "grey50", linewidth = 0.4) +
  annotate("text", x = election, y = Inf, label = "Elecciones\n29-N 2026",
           hjust = 1.1, vjust = 1.4, size = 3.2, colour = "grey30") +
  scale_colour_manual(values = cols) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%")) +
  scale_x_date(
    breaks = month_starts,
    limits = c(start_date, election),
    date_labels = "%b"
  ) +
  labs(x = NULL, y = NULL, colour = NULL,
       title = "Intención de Voto – Elecciones Generales",
       subtitle = "Todas las encuestas publicadas, 2026") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )

print(p)

# ---- 7. Export --------------------------------------------------------------
write.csv(polls, "polls_wide.csv", row.names = FALSE)
cat("\nSaved polls_wide.csv to", getwd(), "\n")

# ---- 8. Export for the website ----------------------------------------------
# install.packages("jsonlite")
# Writes polls.js, which index.html loads. Drops the "2024 EP election" row
# (it is a result, not a poll).
site <- polls |>
  filter(!str_detect(pollster, regex("election", ignore_case = TRUE))) |>
  select(date, pollster, n, all_of(parties)) |>
  arrange(date)

con <- file("polls.js", open = "w", encoding = "UTF-8")
writeLines(
  paste0("window.POLLS = ",
         jsonlite::toJSON(site, dataframe = "rows", na = "null", digits = NA),
         ";"),
  con
)
close(con)
cat("Saved polls.js to", getwd(), "\n")
