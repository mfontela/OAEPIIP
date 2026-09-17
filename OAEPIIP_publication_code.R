###############################################################################
##                                                                           ##
##  OAEPIIP Vigo (May 2025) -- reproducible analysis code                    ##
##                                                                           ##
##  Ocean Alkalinity Enhancement Pelagic Impact Intercomparison Project      ##
##  Microcosm experiment, Ria de Vigo (42.239 N, 8.763 W), 9-28 May 2025     ##
##                                                                           ##
##  This single script reproduces, starting ONLY from the published data     ##
##  file `OAEPIIP_Vigo_data_report.xlsx`:                                    ##
##                                                                           ##
##    Figure 2 -- temperature and inorganic carbon system                    ##
##    Figure 3 -- dissolved inorganic nutrients and stoichiometric ratios    ##
##    Figure 4 -- Chla, POC, BSi and PON                                     ##
##    Figure 5 -- microplankton abundance (a) and biomass composition (b)    ##
##    Figure 6 -- plankton groups resolved by flow cytometry                 ##
##    Table  2 -- GAMM model selection (best model, R2, AIC)                 ##
##                                                                           ##
##  Contact: Marcos Fontela <mfontela@iim.csic.es>                           ##
##  Licence: CC-BY 4.0 (code released together with the dataset)             ##
##                                                                           ##
##  ------------------------------------------------------------------------ ##
##                                                                           ##
##  Tested with R 4.5.2. Package versions used at the time of writing are    ##
##  printed by sessionInfo() at the end of the run (see `OAEPIIP_sessionInfo`##
##  in the output folder).                                                   ##
##                                                                           ##
###############################################################################


## ===========================================================================
## 0. USER SETTINGS
## ===========================================================================

## Path to the published Excel data file. By default the script looks for it
## in the working directory; edit this line if it lives somewhere else.
DATA_FILE <- "OAEPIIP_Vigo_data_report.xlsx"

## Where figures and tables are written.
OUT_DIR <- "OAEPIIP_output"

## Write figures / tables to disk (TRUE) or only build the objects (FALSE).
SAVE_OUTPUT <- TRUE

## Run the GAMM section (section 7). Fitting the full model set takes a few
## minutes; set to FALSE if you only want the figures.
RUN_GAMM <- TRUE

## Jitter of the individual replicate points is random; a fixed seed makes
## the figures bit-for-bit reproducible between runs.
set.seed(77)


## ===========================================================================
## 1. PACKAGES
## ===========================================================================

.pkgs <- c(
  "readxl",     # read the Excel workbook
  "dplyr", "tidyr", "stringr", "purrr", "tibble", "forcats",  # data handling
  "ggplot2", "patchwork", "ggtext", "ggh4x", "scales",        # figures
  "mgcv",       # GAM / GAMM fitting
  "emmeans", "multcomp"                                        # post-hoc EMMs
)

.missing <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  stop("Missing packages: ", paste(.missing, collapse = ", "),
       "\nInstall them with: install.packages(c(",
       paste0('"', .missing, '"', collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(purrr);  library(tibble); library(forcats)
  library(ggplot2); library(patchwork); library(ggtext); library(ggh4x)
  library(mgcv)
})

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

if (!file.exists(DATA_FILE)) {
  stop("Data file not found: ", normalizePath(DATA_FILE, mustWork = FALSE),
       "\nEdit DATA_FILE at the top of the script.")
}


## ===========================================================================
## 2. HELPERS: TREATMENT PALETTE, LABELS AND SMALL UTILITIES
## ===========================================================================

## Treatment colour palette used throughout the manuscript.
oae_pal <- c(
  "control"        = "#969696",   # grey
  "equilibrated"   = "#65cdab",   # green
  "unequilibrated" = "#ce6f53"    # brown / orange
)

## Legend entries are coloured HTML (rendered by ggtext::element_markdown).
oae_labels <- c(
  control        = "<span style='color:#969696;'>Control</span>",
  equilibrated   = "<span style='color:#65cdab;'>Equilibrated</span>",
  unequilibrated = "<span style='color:#ce6f53;'>Unequilibrated</span>"
)

trt_levels <- c("control", "equilibrated", "unequilibrated")

## ggplot2 >= 3.5 deprecated `legend.position = c(x, y)` in favour of
## `legend.position = "inside"` + `legend.position.inside = c(x, y)`.
## This helper keeps the script working on both old and new versions.
legend_inside <- function(x, y) {
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    theme(legend.position = "inside", legend.position.inside = c(x, y))
  } else {
    theme(legend.position = c(x, y))
  }
}

## Boxed legend shared by several panels.
legend_box <- function(text_size = 14) {
  theme(
    legend.key.size      = unit(1.2, "lines"),
    legend.title         = element_blank(),
    legend.text          = ggtext::element_markdown(size = text_size),
    legend.box.background = element_rect(colour = "black", linewidth = 0.7,
                                         fill = "white", linetype = "solid")
  )
}

## Common x axis of every time-series panel (day 0 to day 19).
scale_x_day <- function(...) {
  scale_x_continuous(breaks = c(seq(0, 15, 5), 19), ...)
}

## Treatment colour + fill scales with the coloured HTML legend labels.
scale_trt <- function(labels = TRUE) {
  list(
    scale_colour_manual(values = oae_pal,
                        labels = if (labels) oae_labels else waiver()),
    scale_fill_manual(values = oae_pal,
                      labels = if (labels) oae_labels else waiver())
  )
}

## Mean +- SD across the three replicate microcosms, for one variable.
##   df  : data frame with columns day, treatment and `var`
##   var  : name of the response column
## Returns day / treatment / mean / sd, "before" rows excluded.
summarise_trt <- function(df, var) {
  df %>%
    filter(treatment != "before", !is.na(.data[[var]])) %>%
    group_by(treatment, day) %>%
    summarise(mean = mean(.data[[var]], na.rm = TRUE),
              sd   = sd(.data[[var]],   na.rm = TRUE),
              n    = dplyr::n(),
              .groups = "drop")
}

## Standard mean +- SD time-series panel used in Figures 2, 3, 4 and 6.
##   points_df : individual microcosm observations (jittered, small points)
##   before_df : optional in-situ value at the time of filling ("before"),
##               drawn as a navy triangle
panel_timeseries <- function(df, var, title,
                             points_df   = NULL,
                             before_df   = NULL,
                             before_x    = 0,
                             ribbon_alpha = 0.2,
                             point_size   = 2,
                             legend       = NULL,     # NULL or c(x, y)
                             legend_size  = 14,
                             xlab         = "",
                             extra        = NULL) {

  sm <- summarise_trt(df, var)
  if (is.null(points_df)) points_df <- df %>% filter(treatment != "before")
  points_df <- points_df %>% filter(!is.na(.data[[var]]))

  p <- ggplot(sm, aes(x = day, y = mean, group = treatment)) +
    ## individual replicate observations
    geom_jitter(data = points_df, aes(x = day, y = .data[[var]], colour = treatment),
                size = 1, alpha = 0.7, inherit.aes = FALSE) +
    ## +- 1 SD across replicates
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = treatment),
                alpha = ribbon_alpha, colour = NA) +
    geom_path(aes(colour = treatment), linewidth = 1) +
    geom_point(aes(colour = treatment), size = point_size)

  ## in-situ ("before") value at the time of microcosm filling
  if (!is.null(before_df) && nrow(before_df)) {
    bdf <- before_df %>%
      filter(!is.na(.data[[var]])) %>%
      mutate(.before_x = before_x)
    p <- p + geom_point(data = bdf,
                        aes(x = .before_x, y = .data[[var]]),
                        colour = "navyblue", shape = 17, size = 3,
                        inherit.aes = FALSE)
  }

  p <- p +
    scale_x_day() +
    expand_limits(x = c(0, 19)) +
    scale_trt() +
    theme_bw() +
    labs(x = xlab, y = "", title = title) +
    theme(axis.text = element_text(size = 15), legend.position = "none")

  if (!is.null(legend)) p <- p + legend_inside(legend[1], legend[2]) + legend_box(legend_size)
  if (!is.null(extra))  p <- p + extra
  p
}

## Attribution printed in the bottom-right corner of every figure.
FIG_CAPTION <- "Marcos Fontela - OAEPIIP Vigo experiment results"

tag_panels <- function(x = 0.094, y = 0.99) {
  list(
    plot_annotation(
      tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
      caption = FIG_CAPTION,
      theme = theme(plot.caption = element_text(hjust = 1, size = 8,
                                                colour = "grey35"))
    ),
    theme(plot.tag = element_text(size = 12, face = "bold"),
          plot.tag.position = c(x, y))
  )
}

save_fig <- function(plot, file, width, height) {
  if (!SAVE_OUTPUT) return(invisible(NULL))
  f <- file.path(OUT_DIR, file)
  ggsave(f, plot = plot, device = cairo_pdf, dpi = 300,
         width = width, height = height, units = "cm")
  message("  written: ", f)
  invisible(f)
}


## ===========================================================================
## 3. DATA: READ THE PUBLISHED WORKBOOK
## ===========================================================================
##
## The workbook holds one sheet per measurement block. All sheets share the
## keys `day` (0-19), `microcosm` (1-9) and `treatment`
## (before | control | equilibrated | unequilibrated). Day 0 carries both the
## "before" rows (the water as it entered the microcosms, prior to the
## alkalinity perturbation) and the first post-perturbation sampling.
##
## `oae` below is the single wide table used by Figures 2, 3, 4 and 6 and by
## the GAMM section; `fito` is the long (one row per taxon) microscopy table
## used by Figure 5.
## ===========================================================================

message("Reading ", DATA_FILE, " ...")

key <- c("day", "microcosm", "treatment")

sheet_ta   <- read_excel(DATA_FILE, sheet = "TA")
sheet_carb <- read_excel(DATA_FILE, sheet = "Carbchem")
sheet_tsl  <- read_excel(DATA_FILE, sheet = "T_S_L")
sheet_nut  <- read_excel(DATA_FILE, sheet = "Nutrients")
sheet_chla <- read_excel(DATA_FILE, sheet = "Chla")
sheet_poc  <- read_excel(DATA_FILE, sheet = "POCPON")
sheet_bsi  <- read_excel(DATA_FILE, sheet = "BSi")
sheet_fcm  <- read_excel(DATA_FILE, sheet = "FlowCyto")
sheet_mic  <- read_excel(DATA_FILE, sheet = "Microscopy")

## --- flow cytometry: heterotrophic bacteria -------------------------------

HAS_BACTERIA <- "Bacteria" %in% names(sheet_fcm) &&
  any(!is.na(sheet_fcm[["Bacteria"]]))
if (!HAS_BACTERIA) {
  message("  NOTE: no `Bacteria` column with data in sheet FlowCyto -- ",
          "Figure 6d and the heterotrophic bacteria row of Table 2 are skipped.")
}

fcm_vars <- c("Picoeuk", "Nanoeuk", "Synecho", if (HAS_BACTERIA) "Bacteria")

## --- wide table -----------------------------------------------------------
oae <- sheet_ta %>%
  select(all_of(key), date, Alk_data, Alk_fill = Alkalinity_interpolated) %>%
  full_join(sheet_carb %>% select(all_of(key), pHT25, pHisT, DIC, pCO2, OmegaAragonite),
            by = key) %>%
  full_join(sheet_tsl  %>% select(all_of(key), T, Sal_data, light), by = key) %>%
  full_join(sheet_nut  %>% select(all_of(key), NO3, NO2, NH4, PO4, SiO4, NO3_NO2) %>%
              mutate(Nutrients = 1L),                      # flag: nutrient sampling day
            by = key) %>%
  full_join(sheet_chla %>% select(all_of(key), Chla_data), by = key) %>%
  full_join(sheet_poc  %>% select(all_of(key), POC, PON),  by = key) %>%
  full_join(sheet_bsi  %>% select(all_of(key), BSiO2),     by = key) %>%
  full_join(sheet_fcm  %>% select(all_of(key), all_of(fcm_vars)), by = key) %>%
  ## derived nutrient quantities (same definitions as in the manuscript)
  mutate(
    DIN = if_else(is.na(NH4), NO3 + NO2, NO3 + NO2 + NH4),
    NPratio = if_else(is.na(NH4),
                      (NO3 + NO2)       / (16 * PO4),
                      (NO3 + NO2 + NH4) / (16 * PO4)),
    percentage_NH4 = if_else(is.na(NH4), NA_real_,
                             (NH4 / (NO3 + NO2 + NH4)) * 100),
    treatment = factor(treatment, levels = c("before", trt_levels))
  ) %>%
  arrange(day, microcosm)

## Rows sampled before the perturbation (in-situ reference of Figures 3 and 4).
oae_before <- oae %>% filter(treatment == "before")
## Everything used for the treatment comparison.
oae_trt    <- oae %>% filter(treatment != "before") %>% droplevels()

## --- microscopy: long table with functional group and genus ---------------
##
## The Microscopy sheet is wide: two columns per taxon, "<taxon> (cells/mL)"
## and "<taxon> (um3/mL)". A blank cell means the taxon was not observed in
## that sample (i.e. zero), not "not analysed".
##
## The functional-group assignment of every taxon is given in
## `taxon_groups` below so that the script is self-contained; the genus is
## the leading run of letters of the taxon name, as in the original analysis.

taxon_groups <- tibble::tribble(
  ~taxon,                                          ~group,
  "Actinomonas",                                   "FLAGELLATES",
  "Aloricate ciliate (20 - 50 µm)",           "CILIATES",
  "Aloricate ciliate (< 20 µm)",              "CILIATES",
  "Aloricate ciliate (> 50µm)",               "CILIATES",
  "Amphidinium flagellans (< 20 µm)",         "DINOFLAGELLATES",
  "Amphidinium flagellans (> 20 µm)",         "DINOFLAGELLATES",
  "Appendicularia",                                "ZOOPLANKTON",
  "Asterionella glacialis",                        "DIATOMS",
  "Asteromphalus sarcophagus",                     "DIATOMS",
  "Athecate dinoflagellate (20 - 50 µm)",     "DINOFLAGELLATES",
  "Athecate dinoflagellate (< 20 µm)",        "DINOFLAGELLATES",
  "Athecate dinoflagellate (> 50 µm)",        "DINOFLAGELLATES",
  "Bacteriastrum sp.",                             "DIATOMS",
  "Centric diatom (20 - 50 µm)",              "DIATOMS",
  "Centric diatom (< 20 µm)",                 "DIATOMS",
  "Cerataulina pelagica",                          "DIATOMS",
  "Chaetoceros (< 20 µm)",                    "DIATOMS",
  "Chaetoceros affinis",                           "DIATOMS",
  "Chaetoceros curvisetus",                        "DIATOMS",
  "Chaetoceros danicus",                           "DIATOMS",
  "Chaetoceros debilis",                           "DIATOMS",
  "Chaetoceros decipiens",                         "DIATOMS",
  "Chaetoceros didymus",                           "DIATOMS",
  "Chaetoceros peruvianus",                        "DIATOMS",
  "Chaetoceros spores",                            "DIATOMS",
  "Ciliate cf Cyclidium",                          "CILIATES",
  "Cochlodinium sp (< 20 µm)",                "DINOFLAGELLATES",
  "Cochlodinium sp (> 20 µm)",                "DINOFLAGELLATES",
  "Coconeis",                                      "DIATOMS",
  "Copepods",                                      "ZOOPLANKTON",
  "Cryptophyceae",                                 "FLAGELLATES",
  "Cylindrotheca closterium",                      "DIATOMS",
  "Cyst dinoflagellate",                           "DINOFLAGELLATES",
  "Dactyliosolen mediterraneus (Leptocylindrus)",  "DIATOMS",
  "Dinobryon",                                     "FLAGELLATES",
  "Dinophysis acuminata",                          "DINOFLAGELLATES",
  "Dinophysis acuta",                              "DINOFLAGELLATES",
  "Dinophysis caudata",                            "DINOFLAGELLATES",
  "Dinophysis sp",                                 "DINOFLAGELLATES",
  "Diplopsalis sp",                                "DINOFLAGELLATES",
  "Ditylum brightwellii",                          "DIATOMS",
  "Ebria",                                         "FLAGELLATES",
  "Eucampia striata (Guinardia)",                  "DIATOMS",
  "Eucampia zoodiacus",                            "DIATOMS",
  "Eutreptiella sp",                               "FLAGELLATES",
  "Gonyaulax polyedra (Lingulodinium)",            "DINOFLAGELLATES",
  "Gonyaulax sp",                                  "DINOFLAGELLATES",
  "Gonyaulax spinifera",                           "DINOFLAGELLATES",
  "Gymnodinium catenatum",                         "DINOFLAGELLATES",
  "Gymnodinium sp (< 20 µm)",                 "DINOFLAGELLATES",
  "Gymnodinium sp (> 20 µm)",                 "DINOFLAGELLATES",
  "Gyrodinium (> 50 µm)",                     "DINOFLAGELLATES",
  "Gyrodinium sp (< 20 µm)",                  "DINOFLAGELLATES",
  "Gyrodinium sp (> 20 µm)",                  "DINOFLAGELLATES",
  "Haptophyta - Coccolithophores",                 "FLAGELLATES",
  "Hemiaulus sp",                                  "DIATOMS",
  "Heterocapsa niei",                              "DINOFLAGELLATES",
  "Heterosigma akashiwo",                          "FLAGELLATES",
  "Laboea strobila",                               "CILIATES",
  "Lauderia pumila (Detonula)",                    "DIATOMS",
  "Lebouridinium (< 20 µm)",                  "DINOFLAGELLATES",
  "Lebouridinium glaucum",                         "DINOFLAGELLATES",
  "Leptocylindrus danicus",                        "DIATOMS",
  "Leptocylindrus minimus",                        "DIATOMS",
  "Leptocylindrus spores",                         "DIATOMS",
  "Licmophora",                                    "DIATOMS",
  "Mesodinium rubrum (< 20 µm)",              "CILIATES",
  "Mesodinium rubrum (> 20 µm)",              "CILIATES",
  "Navicula (> 20 µm)",                       "DIATOMS",
  "Nitzchia longissima (< 20 µm)",            "DIATOMS",
  "Nitzchia longissima (> 20 µm)",            "DIATOMS",
  "Octactis octonaria",                            "FLAGELLATES",
  "Oxytoxum  sp",                                  "DINOFLAGELLATES",
  "Oxytoxum laticeps",                             "DINOFLAGELLATES",
  "Parafavella denticulata",                       "CILIATES",
  "Pennate diatom (20 - 50 µm)",              "DIATOMS",
  "Pennate diatom (< 20 µm)",                 "DIATOMS",
  "Pennate diatom (> 50 µm)",                 "DIATOMS",
  "Phalacroma rotundatum",                         "DINOFLAGELLATES",
  "Pleurosigma (> 50 µm)",                    "DIATOMS",
  "Proboscia alata",                               "DIATOMS",
  "Pronoctiluca pelagica",                         "DINOFLAGELLATES",
  "Prorocentrum cordatum (minimum)",               "DINOFLAGELLATES",
  "Prorocentrum gracile",                          "DINOFLAGELLATES",
  "Prorocentrum micans",                           "DINOFLAGELLATES",
  "Prorocentrum sp",                               "DINOFLAGELLATES",
  "Protoperidinium  sp (20 - 50 µm)",         "DINOFLAGELLATES",
  "Protoperidinium  sp (> 50 µm)",            "DINOFLAGELLATES",
  "Protoperidinium bipes",                         "DINOFLAGELLATES",
  "Protoperidinium conicum",                       "DINOFLAGELLATES",
  "Protoperidinium diabolus",                      "DINOFLAGELLATES",
  "Protoperidinium divergens",                     "DINOFLAGELLATES",
  "Protoperidinium steinii",                       "DINOFLAGELLATES",
  "Pseudo-nitzschia (< 20 µm)",               "DIATOMS",
  "Pseudo-nitzschia (> 20 µm)",               "DIATOMS",
  "Pseudo-nitzschia seriata",                      "DIATOMS",
  "Pyrophacus horologium",                         "DINOFLAGELLATES",
  "Rhizosolenia alata",                            "DIATOMS",
  "Rhizosolenia delicatula (Guinardia)",           "DIATOMS",
  "Rhizosolenia flaccida (Guinardia)",             "DIATOMS",
  "Rhizosolenia fragilissima (Dactyliosolen)",     "DIATOMS",
  "Rhizosolenia imbricata",                        "DIATOMS",
  "Scrippsiella",                                  "DINOFLAGELLATES",
  "Skeletonema costatum",                          "DIATOMS",
  "Strombidium cornutum",                          "CILIATES",
  "Strombidium sp (< 20 µm)",                 "CILIATES",
  "Strombidium sp (> 20 µm)",                 "CILIATES",
  "Thalassionema nitzschioides",                   "DIATOMS",
  "Thalassiosira (20 - 50 µm)",               "DIATOMS",
  "Thalassiosira (< 20 µm)",                  "DIATOMS",
  "Thalassiosira gravida (rotula)",                "DIATOMS",
  "Thecate dinoflagellate (20 - 50 µm)",      "DINOFLAGELLATES",
  "Thecate dinoflagellate (< 20 µm)",         "DINOFLAGELLATES",
  "Thecate dinoflagellate (> 50 µm)",         "DINOFLAGELLATES",
  "Tintinnopsys",                                  "CILIATES",
  "Torodinium",                                    "DINOFLAGELLATES",
  "Tripos candelabrum",                            "DINOFLAGELLATES",
  "Tripos furca (Ceratium)",                       "DINOFLAGELLATES",
  "Tripos fusus (Ceratium)",                       "DINOFLAGELLATES",
  "Tripos horridus (Ceratium)",                    "DINOFLAGELLATES",
  "Tripos macrocerus",                             "DINOFLAGELLATES",
  "Tripos minutus",                                "DINOFLAGELLATES"
)

## helper: wide -> long for one of the two measurement suffixes
microscopy_long <- function(sheet, suffix, value_name) {
  sheet %>%
    select(all_of(key), ends_with(suffix)) %>%
    pivot_longer(-all_of(key), names_to = "taxon", values_to = value_name) %>%
    mutate(taxon = str_trim(str_remove(taxon, stringr::fixed(suffix))))
}

fito <- microscopy_long(sheet_mic, " (cells/mL)",      "abundance") %>%
  left_join(microscopy_long(sheet_mic, " (µm3/mL)", "biomass"),
            by = c(key, "taxon")) %>%
  left_join(taxon_groups, by = "taxon") %>%
  ## genus = leading run of letters of the taxon name
  mutate(genera = str_extract(taxon, "^[A-Za-z]+"))

if (anyNA(fito$group)) {
  stop("Taxa without a functional group assignment: ",
       paste(unique(fito$taxon[is.na(fito$group)]), collapse = ", "))
}


## ===========================================================================
## 4. FIGURE 2 -- TEMPERATURE AND THE INORGANIC CARBON SYSTEM
## ===========================================================================
## Six panels: temperature, total alkalinity, pH_T25, DIC, pCO2 and the
## aragonite saturation state. Large points and lines are the mean of the
## three replicate microcosms, ribbons are +- 1 SD, small points are the
## individual microcosms.
## ===========================================================================

message("Building Figure 2 ...")

Fig2_T <- panel_timeseries(
  oae, "T", " Temperature (ºC)",
  ribbon_alpha = 0.4, point_size = 3,
  extra = list(
    ## sea surface temperature at the time of microcosm filling
    geom_hline(yintercept = 17, colour = "navyblue", linetype = 2, linewidth = 1.7),
    scale_y_continuous(limits = c(15, 19))
  )
)
## the reference line must sit under the data
Fig2_T$layers <- Fig2_T$layers[c(length(Fig2_T$layers), seq_len(length(Fig2_T$layers) - 1L))]

Fig2_Alk <- panel_timeseries(
  oae, "Alk_fill", expression(" Total alkalinity (" * mu * "mol kg"^{-1} * ")"),
  ## small points are the directly measured TA (the line uses the
  ## gap-filled series `Alkalinity_interpolated`)
  points_df = oae_trt %>% filter(!is.na(Alk_data)) %>% mutate(Alk_fill = Alk_data),
  point_size = 3, legend = c(0.5, 0.5), legend_size = 17,
  extra = scale_y_continuous(breaks = c(2300, 2550, 2800))
) + theme(legend.key.size = unit(1.5, "lines"))

Fig2_pH <- panel_timeseries(
  oae, "pHT25", expression(pH[T] * 25), point_size = 3,
  extra = scale_y_continuous(breaks = seq(7.9, 8.5, 0.2))
)

Fig2_DIC <- panel_timeseries(
  oae, "DIC", expression(" Dissolved inorganic carbon (" * mu * "mol kg"^{-1} * ")"),
  ribbon_alpha = 0.3, point_size = 3, xlab = "Day",
  extra = scale_y_continuous(breaks = c(2000, 2200, 2400))
)

Fig2_pCO2 <- panel_timeseries(
  oae, "pCO2", expression(pCO[2] ~ "(" * mu * "atm)"),
  ribbon_alpha = 0.3, point_size = 3, xlab = "Day",
  extra = scale_y_continuous(breaks = c(100, 200, 300, 400))
)

Fig2_omega <- panel_timeseries(
  oae, "OmegaAragonite",
  expression(" Aragonite saturation state (" * Omega[aragonite] * ")"),
  ribbon_alpha = 0.3, point_size = 3, xlab = "Day",
  extra = scale_y_continuous(breaks = c(3, 5, 7))
)

Figure_2 <- Fig2_T + Fig2_Alk + Fig2_pH + Fig2_DIC + Fig2_pCO2 + Fig2_omega +
  plot_layout(ncol = 3) + tag_panels()[[1]] & tag_panels()[[2]]

save_fig(Figure_2, "Figure_2.pdf", width = 29, height = 22)


## ===========================================================================
## 5. FIGURE 3 -- DISSOLVED INORGANIC NUTRIENTS AND RATIOS
## ===========================================================================
## Nine panels. The navy triangle at day 0 is the in-situ value of the water
## used to fill the microcosms ("before" rows).
## ===========================================================================

message("Building Figure 3 ...")

## Only the days on which nutrients were sampled.
nut      <- oae %>% filter(!is.na(Nutrients))
nut_trt  <- nut %>% filter(treatment != "before")
nut_bef  <- nut %>% filter(treatment == "before")

panel_nutrient <- function(var, title, xlab = "", legend = NULL, extra = NULL) {
  panel_timeseries(nut, var, title,
                   points_df = nut_trt, before_df = nut_bef,
                   legend = legend, legend_size = 10, xlab = xlab, extra = extra)
}

Fig3_NO3  <- panel_nutrient("NO3", expression("NO"[3] ~ "(" * mu * "mol L"^{-1} * ")"))
Fig3_NO2  <- panel_nutrient("NO2", expression("NO"[2] ~ "(" * mu * "mol L"^{-1} * ")"))
Fig3_NH4  <- panel_nutrient("NH4", expression("NH"[4] ~ "(" * mu * "mol L"^{-1} * ")"))
Fig3_DIN  <- panel_nutrient("DIN", expression("DIN" ~ "(" * mu * "mol L"^{-1} * ")"))
Fig3_NOx  <- panel_nutrient("NO3_NO2",
                            expression("NO"[3] - " + NO"[2] ~ "(" * mu * "mol L"^{-1} * ")"),
                            legend = c(0.5, 0.67))
Fig3_pNH4 <- panel_nutrient("percentage_NH4", expression("% NH"[4] ~ "in DIN"))
Fig3_PO4  <- panel_nutrient("PO4", expression("PO"[4] ~ "(" * mu * "mol L"^{-1} * ")"),
                            xlab = "Day")
Fig3_SiO4 <- panel_nutrient("SiO4", expression("SiO"[4] ~ "(" * mu * "mol L"^{-1} * ")"),
                            xlab = "Day")


np <- nut %>%
  mutate(NP16 = NPratio * 16) %>%
  filter(is.na(PO4) | PO4 > 0.0097)

Fig3_NP <- panel_timeseries(
  np, "NP16", "N:P",
  points_df = nut_trt %>% mutate(NP16 = NPratio * 16),
  before_df = nut_bef %>% mutate(NP16 = NPratio * 16),
  xlab = "Day", legend_size = 10,
  extra = list(
    geom_hline(yintercept = 16, linetype = 2, linewidth = 0.7, colour = "gray77"),
    scale_y_continuous(breaks = c(1, 8, 16, 24), limits = c(0, 30))
  )
)
Fig3_NP$layers <- Fig3_NP$layers[c(length(Fig3_NP$layers),
                                   seq_len(length(Fig3_NP$layers) - 1L))]

Figure_3 <- Fig3_NO3 + Fig3_NO2 + Fig3_NH4 +
  Fig3_DIN + Fig3_NOx + Fig3_pNH4 +
  Fig3_PO4 + Fig3_NP + Fig3_SiO4 +
  plot_layout(ncol = 3) + tag_panels()[[1]] & tag_panels()[[2]]

save_fig(Figure_3, "Figure_3.pdf", width = 29, height = 22)


## ===========================================================================
## 6. FIGURE 4 -- CHLOROPHYLL A, POC, BSi AND PON
## ===========================================================================

message("Building Figure 4 ...")

margin0 <- theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

Fig4_Chla <- panel_timeseries(
  oae, "Chla_data", expression(Chla ~ (mg ~ m^{-3})),
  before_df = oae_before %>% filter(!is.na(Chla_data)), before_x = -0.5
) + margin0

Fig4_POC <- panel_timeseries(
  oae %>% filter(is.na(POC) | POC > 0), "POC",
  expression("POC" ~ "(" * mu * mol ~ L^{-1} * ")"),
  points_df = oae_trt %>% filter(POC > 0),
  before_df = oae_before %>% filter(!is.na(POC)), before_x = -0.5,
  legend = c(0.7, 0.7)
) + margin0

Fig4_BSi <- panel_timeseries(
  oae, "BSiO2", expression("BSi" ~ "(" * mu * mol ~ L^{-1} * ")"),
  xlab = "Day"
) + margin0

Fig4_PON <- panel_timeseries(
  oae %>% filter(is.na(PON) | PON > 0), "PON",
  expression("PON" ~ "(" * mu * mol ~ L^{-1} * ")"),
  points_df = oae_trt %>% filter(PON > 0),
  xlab = "Day",
  extra = geom_point(data = tibble(day = -0.5, y = 0, treatment = "control"),
                     aes(x = day, y = y), shape = NA, inherit.aes = FALSE)
) + margin0

Figure_4 <- Fig4_Chla + Fig4_POC + Fig4_BSi + Fig4_PON +
  plot_layout(ncol = 2) + tag_panels(0.02, 0.95)[[1]] & tag_panels(0.02, 0.95)[[2]]

save_fig(Figure_4, "Figure_4.pdf", width = 21, height = 18)


## ===========================================================================
## 7. FIGURE 5 -- MICROPLANKTON ABUNDANCE AND BIOMASS COMPOSITION
## ===========================================================================
## (a) total abundance of each functional group (mean +- SD of the three
##     replicate microcosms), one facet per treatment, sqrt y axis.
## (b) relative biomass of the five dominant genera of each functional group.
## ===========================================================================

message("Building Figure 5 ...")

lighten_hex <- function(col, amount = 0.72) {
  m <- col2rgb(col) / 255; m2 <- m + (1 - m) * amount
  rgb(m2[1L, ], m2[2L, ], m2[3L, ])
}
darken_hex <- function(col, amount = 0.35) {
  m <- col2rgb(col) / 255; m2 <- m * (1 - amount)
  rgb(m2[1L, ], m2[2L, ], m2[3L, ])
}

grp_pal <- c("DIATOMS"         = "#2ca25f",
             "DINOFLAGELLATES" = "#e6550d",
             "CILIATES"        = "#6a51a3",
             "FLAGELLATES"     = "#737373",
             "ZOOPLANKTON"     = "pink")

grp_pal_dark <- c(
  purrr::map_chr(grp_pal[c("DIATOMS", "DINOFLAGELLATES", "CILIATES", "FLAGELLATES")],
                 darken_hex),
  ZOOPLANKTON = grp_pal[["ZOOPLANKTON"]]
)

grp_levels  <- c("DIATOMS", "DINOFLAGELLATES", "CILIATES", "FLAGELLATES", "ZOOPLANKTON")
grp_display <- c(DIATOMS = "Diatoms", DINOFLAGELLATES = "Dinoflagellates",
                 CILIATES = "Ciliates", FLAGELLATES = "Flagellates")
legend_labels_colored <- purrr::imap_chr(
  grp_display, ~ sprintf("<span style='color:%s;'>%s</span>", grp_pal_dark[[.y]], .x))

tr_labs <- c(control = "Control", equilibrated = "Equilibrated",
             unequilibrated = "Unequilibrated")

## coloured facet strips, one per treatment (alphabetical panel order)
strip_trt <- ggh4x::strip_themed(
  background_x = list(element_rect(fill = oae_pal[["control"]]),
                      element_rect(fill = oae_pal[["equilibrated"]]),
                      element_rect(fill = oae_pal[["unequilibrated"]])),
  text_x       = rep(list(element_text(colour = "white", face = "bold")), 3)
)

## --- (a) abundance by functional group ------------------------------------
## step 1: sum all taxa of a group within each microcosm
## step 2: mean +- SD of those totals across the three replicate microcosms
abund_lines_df <- fito %>%
  filter(!is.na(abundance), treatment != "before") %>%
  mutate(group = factor(group, levels = grp_levels)) %>%
  group_by(day, treatment, microcosm, group) %>%
  summarise(total = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(day, treatment, group) %>%
  summarise(total_adj = mean(total, na.rm = TRUE),
            sd_total  = sd(total,  na.rm = TRUE),
            n         = dplyr::n(), .groups = "drop")

## day-0 diatom abundance exceeds the y-axis limit and is annotated instead
day0_diatoms <- abund_lines_df %>%
  filter(group == "DIATOMS", day == 0) %>%
  select(treatment, total_adj) %>%
  mutate(x_marker = 2.5, y_marker = 645, x_label = 3.6, y_label = 655)

Fig5_abundance <- ggplot(
  abund_lines_df %>% filter(group != "ZOOPLANKTON"),
  aes(x = day, y = total_adj, colour = group, fill = group)
) +
  geom_ribbon(aes(ymin = pmax(total_adj - sd_total, 0), ymax = total_adj + sd_total),
              alpha = 0.18, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_point(data = day0_diatoms, aes(x = x_marker, y = y_marker),
             shape = 17, size = 6, colour = grp_pal_dark[["DIATOMS"]],
             inherit.aes = FALSE) +
  geom_text(data = day0_diatoms,
            aes(x = x_label, y = y_label,
                label = paste0(round(total_adj), " cells·mL⁻¹ at day 0")),
            colour = grp_pal_dark[["DIATOMS"]], size = 3.6, hjust = 0,
            inherit.aes = FALSE) +
  scale_colour_manual(values = grp_pal_dark, breaks = names(legend_labels_colored),
                      labels = unname(legend_labels_colored), name = "Group") +
  scale_fill_manual(values = grp_pal_dark, breaks = names(legend_labels_colored),
                    labels = unname(legend_labels_colored), name = "Group") +
  ggh4x::facet_grid2(cols = vars(treatment),
                     labeller = labeller(treatment = tr_labs), strip = strip_trt) +
  scale_y_sqrt(breaks = c(0, 1, 10, 25, 50, 100, 250, 500, 1000, 1500, 2000)) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 19), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 650), clip = "on") +
  labs(title = "Daily plankton community abundance by functional group",
       x = "Day", y = expression("Abundance (cells·mL"^{-1} * ")")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor  = element_blank(),
        plot.title        = element_text(face = "bold"),
        legend.title      = element_blank(),
        legend.justification = c(0.5, 0.5),
        legend.background = element_rect(fill = scales::alpha("white", 0.75), colour = NA),
        legend.key.size   = unit(6, "pt"),
        legend.text       = ggtext::element_markdown(size = 9.5, lineheight = 1)) +
  legend_inside(0.535, 0.73)

## --- (b) relative biomass of the dominant genera --------------------------
top_n     <- 5L
other_lbl <- function(g) paste("Other", stringr::str_to_sentence(g))

comp <- fito %>%
  filter(!is.na(biomass), group != "ZOOPLANKTON", treatment != "before") %>%
  mutate(treatment = factor(treatment, levels = trt_levels)) %>%
  group_by(treatment, day, microcosm, group, genera) %>%
  summarise(bio_micro = sum(biomass, na.rm = TRUE), .groups = "drop") %>%
  group_by(treatment, day, group, genera) %>%
  summarise(bio = mean(bio_micro, na.rm = TRUE), .groups = "drop")

top_genera <- comp %>%
  group_by(group, genera) %>%
  summarise(total = sum(bio, na.rm = TRUE), .groups = "drop") %>%
  group_by(group) %>%
  slice_max(total, n = top_n, with_ties = FALSE) %>%
  arrange(group, desc(total)) %>%
  ungroup()

comp_lab <- comp %>%
  mutate(label = if_else(paste(group, genera) %in% paste(top_genera$group, top_genera$genera),
                         genera, other_lbl(group))) %>%
  group_by(treatment, day, group, label) %>%
  summarise(bio = sum(bio, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(treatment, day, label, fill = list(bio = 0)) %>%
  ungroup()

grp_order <- top_genera %>%
  group_by(group) %>% summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  filter(group %in% names(grp_pal)) %>%
  arrange(desc(total)) %>% pull(group)

label_levels <- unlist(lapply(grp_order, function(g) {
  gen <- top_genera %>% filter(group == g) %>% arrange(desc(total)) %>% pull(genera)
  c(gen, other_lbl(g))
}), use.names = FALSE)

## sequential shades within each functional-group colour family
fill_pal <- unlist(lapply(grp_order, function(g) {
  gen  <- top_genera %>% filter(group == g) %>% arrange(desc(total)) %>% pull(genera)
  n    <- length(gen) + 1L
  cols <- colorRampPalette(c(darken_hex(grp_pal[[g]]), lighten_hex(grp_pal[[g]])))(n)
  setNames(cols, c(gen, other_lbl(g)))
}), use.names = TRUE)


legend_labels_bio <- names(fill_pal)

sample_days <- sort(unique(comp_lab$day))

Fig5_biomass <- comp_lab %>%
  mutate(label = factor(label, levels = label_levels)) %>%
  ggplot(aes(x = day, y = bio, fill = label)) +
  geom_area(position = "fill", alpha = 0.9, colour = "white", linewidth = 0.15) +
  ## dashed lines mark the days with light-microscopy counts
  geom_vline(xintercept = sample_days, colour = "grey70",
             linewidth = 0.85, linetype = "dashed") +
  scale_fill_manual(values = fill_pal, labels = legend_labels_bio, name = NULL,
                    guide = guide_legend(ncol = 4, reverse = FALSE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0), name = "Relative biomass (%)") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 19), expand = c(0, 0), name = "Day") +
  labs(title = "% Biomass composition by top genera per functional group") +
  ggh4x::facet_grid2(cols = vars(treatment),
                     labeller = labeller(treatment = tr_labs), strip = strip_trt) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor  = element_blank(),
        plot.title        = element_text(face = "bold"),
        legend.title      = element_blank(),
        legend.position   = "bottom",
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key.size   = unit(8, "pt"),
        legend.text       = ggtext::element_markdown(size = 8.5, lineheight = 1))

Figure_5 <- Fig5_abundance / Fig5_biomass +
  tag_panels(0.02, 0.95)[[1]] & tag_panels(0.02, 0.95)[[2]]

save_fig(Figure_5, "Figure_5.pdf", width = 30, height = 26)


## ===========================================================================
## 8. FIGURE 6 -- PLANKTON GROUPS RESOLVED BY FLOW CYTOMETRY
## ===========================================================================

message("Building Figure 6 ...")

Fig6_Nano <- panel_timeseries(
  oae, "Nanoeuk", expression("Nanoeukaryotes abundance (cells·mL"^-1 * ")")
) + margin0

Fig6_Pico <- panel_timeseries(
  oae, "Picoeuk", expression("Picoeukaryotes abundance (cells·mL"^-1 * ")"),
  legend = c(0.3, 0.7), legend_size = 13
) + margin0 + theme(legend.key.size = unit(1, "lines"))

Fig6_Syn <- panel_timeseries(
  oae, "Synecho",
  expression(italic("Synechococcus") ~ "abundance (cells·mL"^-1 * ")"),
  xlab = "Day"
) + margin0

Fig6_panels <- list(Fig6_Nano, Fig6_Pico, Fig6_Syn)

if (HAS_BACTERIA) {
  Fig6_Bact <- panel_timeseries(
    oae, "Bacteria", expression("Bacteria abundance (cells·mL"^-1 * ")"),
    xlab = "Day"
  ) + margin0
  Fig6_panels <- c(Fig6_panels, list(Fig6_Bact))
} else {
  ## keep panel (c) on the bottom row when the fourth panel is unavailable
  Fig6_Syn <- Fig6_Syn + labs(x = "Day")
  Fig6_panels[[3]] <- Fig6_Syn
}

Figure_6 <- Reduce(`+`, Fig6_panels) +
  plot_layout(ncol = 2) + tag_panels(0.02, 0.95)[[1]] & tag_panels(0.02, 0.95)[[2]]

save_fig(Figure_6, "Figure_6.pdf", width = 24, height = 19)


## ===========================================================================
## 9. TABLE 2 -- GAMM ANALYSIS
## ===========================================================================
##
## Four candidate models are fitted to every response variable:
##
##   GAMM_1  Y ~ s(Day) + s(Day, Microcosm, bs = "fs")
##           temporal trend and absolute values independent of treatment
##   GAMM_2  Y ~ s(Day, by = Treatment) + s(Day, Microcosm, bs = "fs")
##           treatment alters the temporal trend but not the absolute values
##   GAMM_3  Y ~ s(Day) + s(Day, Microcosm, bs = "fs") + Treatment
##           treatment alters the absolute values but not the temporal trend
##   GAMM_4  Y ~ s(Day, by = Treatment) + s(Day, Microcosm, bs = "fs") + Treatment
##           treatment alters both
##
## The microcosm enters as a factor-smooth random effect, so each bottle gets
## its own smooth temporal deviation. Basis dimensions follow Wood (2017,
## sec. 5.5.1): k = number of sampled days, k for the microcosm smooth is
## min(9, number of sampled days).
##
## Model choice: lowest AIC; among models within delta-AIC < 2 the simplest
## one is kept (parsimony order GAMM_1 < GAMM_3 < GAMM_2 < GAMM_4).
##
## The selected model, its adjusted R2 and its AIC are reported, except for
## the variables listed in `vars_not_reported` below. Note that AIC is
## comparable only among the four candidates of a given variable, never
## across variables, because responses are fitted on different scales (see
## the `transform` column of `gamm_vars`).
##
## For the variables whose best model carries a parametric treatment term
## (GAMM_3 / GAMM_4), time-averaged estimated marginal means are compared
## pairwise and summarised as a compact letter display.
## ===========================================================================

## Variables for which the model assumptions are not met on visual
## inspection of the residual diagnostics (residuals vs fitted,
## scale-location and the residual ACF of the selected model). They are
## still fitted, but no model statistics and no EMMs are reported for them
## in Table 2, which shows a dash instead.
vars_not_reported <- c(
  "PO4",
  "PON",
  "group_abundance_DIATOMS",
  "group_abundance_ZOOPLANKTON"
)

## --- model fitting --------------------------------------------------------

extract_model_info <- function(model) {
  c(AIC = round(AIC(model), 3), R_squared = round(summary(model)$r.sq, 3))
}

select_model <- function(data, variable, k_value = NULL, k_micro_value = NULL) {

  d <- data %>%
    rename(Day = day, Microcosm = microcosm, Treatment = treatment,
           Y = all_of(variable)) %>%
    filter(Treatment != "before") %>%
    select(Day, Microcosm, Treatment, Y) %>%
    mutate(Microcosm = factor(Microcosm), Treatment = factor(Treatment)) %>%
    filter(complete.cases(.), is.finite(Y))

  n_days <- length(unique(d$Day))
  if (is.null(k_value))       k_value       <- n_days
  if (is.null(k_micro_value)) k_micro_value <- min(9, n_days)

  gamm_1 <- gam(Y ~ s(Day, k = k_value) +
                  s(Day, Microcosm, bs = "fs", k = k_micro_value),
                family = gaussian(), method = "REML", data = d)
  gamm_2 <- gam(Y ~ s(Day, by = Treatment, k = k_value) +
                  s(Day, Microcosm, bs = "fs", k = k_micro_value),
                family = gaussian(), method = "REML", data = d)
  gamm_3 <- gam(Y ~ s(Day, k = k_value) +
                  s(Day, Microcosm, bs = "fs", k = k_micro_value) + Treatment,
                family = gaussian(), method = "REML", data = d)
  gamm_4 <- gam(Y ~ s(Day, by = Treatment, k = k_value) +
                  s(Day, Microcosm, bs = "fs", k = k_micro_value) + Treatment,
                family = gaussian(), method = "REML", data = d)

  models <- list(gamm_1 = gamm_1, gamm_2 = gamm_2, gamm_3 = gamm_3, gamm_4 = gamm_4)

  ## AIC / R2 of the four candidates. Used here to pick the best model and
  ## then discarded: only the winner is carried out of this function.
  model_results <- data.frame(
    model = names(models),
    do.call(rbind, lapply(models, extract_model_info)),
    row.names = NULL
  )

  best_model <- model_results %>%
    arrange(AIC) %>%
    mutate(delta_AIC = AIC - min(AIC)) %>%
    filter(delta_AIC < 2) %>%
    mutate(complexity = case_when(model == "gamm_1" ~ 1, model == "gamm_3" ~ 2,
                                  model == "gamm_2" ~ 3, model == "gamm_4" ~ 4)) %>%
    arrange(complexity) %>%
    slice(1) %>%
    mutate(variable = variable) %>%
    select(variable, everything())

  list(variable = variable, k_value = k_value, k_micro_value = k_micro_value,
       data_used = d, models = models, best_model = best_model)
}

## --- estimated marginal means for GAMM_3 / GAMM_4 -------------------------

emm_summary <- function(fit, p_limit = 0.05) {

  mod <- fit$models[[fit$best_model$model]]
  ## EMMs are computed on the scale the model was fitted on (log10 or sqrt
  ## of the response, or the response itself). Those transformations are
  ## monotone, so the compact letter display is the same as it would be on
  ## the measurement scale; only the `emmean` column would need
  ## back-transforming for interpretation.
  emm <- emmeans::emmeans(mod, ~ Treatment)
  ## Note: emmeans converts adjust = "tukey" to Sidak inside cld(), because
  ## the Tukey adjustment is not valid once the comparisons are re-expressed.
  cld <- multcomp::cld(emm, alpha = p_limit, Letters = letters, adjust = "tukey")
  cld$.group <- trimws(cld$.group)

  list(
    cld   = as_tibble(cld),
    ## a single shared letter means no pair differs significantly
    text  = if (length(unique(cld$.group)) == 1L) {
      "Not significant difference"
    } else {
      paste(paste0(as.character(cld$Treatment), ": ", cld$.group), collapse = "; ")
    }
  )
}

if (RUN_GAMM) {

  message("Fitting GAMMs (this takes a few minutes) ...")

  ## --- response variables, display names and transformations --------------
  ## Every model is a Gaussian GAMM; the response is first put on the scale
  ## that stabilises its variance:
  ##   "log10" -- dissolved inorganic nutrients, particulate matter and the
  ##              flow-cytometry counts
  ##   "sqrt"  -- microscopy counts, which include exact zeros
  ##   "none"  -- chlorophyll a, modelled on its measurement scale
  gamm_vars <- tibble::tribble(
    ~variable,                         ~label,                                  ~transform,
    "NO3",                             "NO3",                                   "log10",
    "NO2",                             "NO2",                                   "log10",
    "NH4",                             "NH4",                                   "log10",
    "PO4",                             "PO4",                                   "log10",
    "SiO4",                            "H4SiO4",                                "log10",
    "Chla_data",                       "Chla",                                  "log10",
    "POC",                             "POC",                                   "log10",
    "PON",                             "PON",                                   "log10",
    "BSiO2",                           "BSi",                                   "log10",
    "group_abundance_DIATOMS",         "Diatoms abundance",                     "sqrt",
    "group_abundance_DINOFLAGELLATES", "Dinoflagellates abundance",             "sqrt",
    "group_abundance_CILIATES",        "Ciliates abundance",                    "sqrt",
    "group_abundance_FLAGELLATES",     "Flagellates abundance",                 "sqrt",
    "group_abundance_ZOOPLANKTON",     "Zooplankton abundance",                 "sqrt",
    "Nanoeuk",                         "Nanoeukaryotes abundance",              "log10",
    "Picoeuk",                         "Autotrophic picoeukaryotes abundance",  "log10",
    "Synecho",                         "Synechococcus abundance",               "log10",
    "Bacteria",                        "Heterotrophic bacteria abundance",      "log10"
  )
  if (!HAS_BACTERIA) {
    gamm_vars <- gamm_vars %>% filter(variable != "Bacteria")
  }

  ## --- modelling table ----------------------------------------------------
  ## Total abundance per functional group, per microcosm and day.
  group_abundance <- fito %>%
    filter(treatment != "before") %>%
    group_by(day, microcosm, treatment, group) %>%
    summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = group, values_from = abundance,
                names_prefix = "group_abundance_")

  oae_model <- oae_trt %>%
    mutate(treatment = as.character(treatment)) %>%
    left_join(group_abundance %>% mutate(treatment = as.character(treatment)),
              by = key)

  ## apply the transformations
  log10_vars <- gamm_vars$variable[gamm_vars$transform == "log10"]
  sqrt_vars  <- gamm_vars$variable[gamm_vars$transform == "sqrt"]

  oae_tr <- oae_model %>%
    mutate(across(all_of(log10_vars), log10)) %>%
    mutate(across(all_of(sqrt_vars),  sqrt))

  ## --- fit ---------------------------------------------------------------
  model_fits <- purrr::map(
    gamm_vars$variable,
    function(v) {
      message("  ", v)
      tryCatch(select_model(oae_tr, v),
               error = function(e) {
                 warning("select_model failed for ", v, ": ", conditionMessage(e))
                 NULL
               })
    }
  )
  names(model_fits) <- gamm_vars$variable
  model_fits <- purrr::compact(model_fits)

  ## --- post-hoc EMMs for the models carrying a treatment term -------------
  emm_results <- purrr::imap(model_fits, function(fit, v) {
    if (v %in% vars_not_reported) return(NULL)
    if (!fit$best_model$model %in% c("gamm_3", "gamm_4")) return(NULL)
    tryCatch(emm_summary(fit), error = function(e) NULL)
  })
  emm_results <- purrr::compact(emm_results)

  ## --- assemble Table 2 ---------------------------------------------------
  best_tbl <- purrr::map_dfr(model_fits, ~ .x$best_model)

  table2 <- gamm_vars %>%
    left_join(best_tbl %>% select(variable, model, R_squared, AIC), by = "variable") %>%
    mutate(
      ## dash out the variables whose residual diagnostics fail on inspection
      reported     = !variable %in% vars_not_reported,
      `Best model` = if_else(reported, toupper(model),               NA_character_),
      `R2`         = if_else(reported, sprintf("%.3f", R_squared),   NA_character_),
      `AIC`        = if_else(reported, sprintf("%.3f", AIC),         NA_character_),
      `EMMs`       = purrr::map_chr(variable, function(v) {
        if (is.null(emm_results[[v]])) NA_character_ else emm_results[[v]]$text
      })
    ) %>%
    ## NB: AIC is only comparable among the four candidates of a given row,
    ## never between rows, because responses are fitted on different scales
    ## (see the `transform` column of `gamm_vars`).
    transmute(Parameter = label,
              `Best model` = tidyr::replace_na(`Best model`, "–"),
              `R2`         = tidyr::replace_na(`R2`,         "–"),
              `AIC`        = tidyr::replace_na(`AIC`,        "–"),
              `EMMs`       = tidyr::replace_na(`EMMs`,       ""))

  ## reorder the rows as they appear in the manuscript
  table2 <- table2 %>%
    mutate(Parameter = factor(Parameter, levels = c(
      "NO3", "NO2", "NH4", "PO4", "H4SiO4", "Chla", "POC", "PON", "BSi",
      "Diatoms abundance", "Dinoflagellates abundance", "Ciliates abundance",
      "Flagellates abundance", "Zooplankton abundance",
      "Nanoeukaryotes abundance", "Autotrophic picoeukaryotes abundance",
      "Synechococcus abundance", "Heterotrophic bacteria abundance"))) %>%
    arrange(Parameter) %>%
    mutate(Parameter = as.character(Parameter))

  if (!HAS_BACTERIA) {
    table2 <- bind_rows(table2, tibble(
      Parameter = "Heterotrophic bacteria abundance",
      `Best model` = "not available in this data release",
      `R2` = "–", `AIC` = "–", `EMMs` = ""))
  }

  cat("\n================ TABLE 2 ================\n")
  print(as.data.frame(table2), row.names = FALSE)
  cat("\n")

  if (SAVE_OUTPUT) {
    write.csv(table2, file.path(OUT_DIR, "Table_2_GAMM_results.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    message("  written: ", file.path(OUT_DIR, "Table_2_GAMM_results.csv"))
  }
}

message("Done. Outputs in: ", normalizePath(OUT_DIR))
