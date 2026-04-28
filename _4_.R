# ============================================================
# KI og produktivitet i Europa — Eurostat-panel
# Torkild, mars 2026
# ============================================================
#
# Struktur:
#   0. Pakker og oppsett
#   1. Hent KI-adopsjon (isoc_eb_ain2)
#   2. Hent produktivitet (nama_10_a64 + nama_10_a64_e)
#   3. NACE-mapping og aggregering
#   4. Bygg panelet
#   5. Deskriptiv statistikk
#   6. Norsk kontekst: K vs J i Norden
#   7. Exposure × Post estimering
#   8. Event-study
# ============================================================


# --- 0. Pakker ---------------------------------------------------

pkgs <- c("eurostat", "dplyr", "tidyr", "stringr", "ggplot2",
          "fixest", "modelsummary", "rjstat")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(eurostat)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(fixest)

# Mappe for mellomlagring
dir.create("data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)


# === 1. KI-ADOPSJON (EUROSTAT) ===================================

cat("\n========== 1. Henter KI-adopsjon (isoc_eb_ain2) ==========\n")

# Denne tabellen har KI-adopsjon per NACE-sektor, land, år
# Variabler matcher SSB 13265 nøyaktig
ai_raw <- get_eurostat("isoc_eb_ain2", time_format = "num")

saveRDS(ai_raw, "data/eurostat_isoc_eb_ain2_raw.rds")

cat("Dimensjoner:", dim(ai_raw), "\n")
cat("Kolonner:", names(ai_raw), "\n")
cat("År tilgjengelig:", sort(unique(ai_raw$time)), "\n")

# Se på strukturen
cat("\nNACE-sektorer:\n")
print(sort(unique(ai_raw$nace_r2)))

cat("\nKI-indikatorer (indic_is):\n")
print(sort(unique(ai_raw$indic_is)))

cat("\nStørrelsesgrupper:\n")
print(sort(unique(ai_raw$sizen_r2)))

cat("\nEnheter:\n")
print(sort(unique(ai_raw$unit)))

# --------------------------------------------------
# Filtrer og rydd
# --------------------------------------------------
# Vi vil ha:
#   - Alle bedrifter 10+ ansatte (uten K = eks. finans)
#   - OG inkludert K (sjekk om det finnes separat)
#   - Prosent av foretak
#   - Alle KI-teknologivariabler

# Først: finn riktig størrelsesfilterkode
# Typisk: "10_C10_S951_XK" (10+, NACE C-N+95.1, ekskl. finans)
# eller noe lignende. La oss se:
ai_raw %>%
  filter(str_detect(sizen_r2, "10")) %>%
  distinct(sizen_r2) %>%
  print()

# Filtrer: alle bedrifter, prosent
ai_eu <- ai_raw %>%
  filter(
    unit == "PC_ENT"  # prosent av foretak
  ) %>%
  select(geo, nace_r2, sizen_r2, indic_is, time, values) %>%
  rename(
    land     = geo,
    nace     = nace_r2,
    size     = sizen_r2,
    ai_var   = indic_is,
    year     = time,
    ai_pct   = values
  )

# --------------------------------------------------
# Mapping av KI-variabelnavn (Eurostat -> norsk)
# --------------------------------------------------
# Eurostat-koder for KI-teknologier:
# Sjekk hva som finnes:
cat("\nAlle indic_is verdier:\n")
ai_eu %>% distinct(ai_var) %>% arrange(ai_var) %>% print(n = 50)

# Typisk mapping (verifiser mot faktiske koder etter nedlasting):
ai_labels <- tribble(
  ~ai_var,     ~ai_label_no,
  "E_AI",      "Bruker minst én KI-teknologi",
  "E_AITM",    "Tekstanalyse (text mining)",
  "E_AISR",    "Talegjenkjenning",
  "E_AINLG",   "Generering av naturlig språk",
  "E_AIMG",    "Generering av bilder/video/lyd",
  "E_AIIR",    "Bildegjenkjenning",
  "E_AIML",    "Maskinlæring for dataanalyse",
  "E_AIAW",    "Automatisering av arbeidsflyter",
  "E_AIPH",    "Autonome roboter/droner"
)
# NB: Eksakte koder kan variere — sjekk output over og juster


# === 2. PRODUKTIVITET (NASJONALREGNSKAP) =========================

cat("\n========== 2. Henter produktivitet ==========\n")

# A) Brutto verdiskaping (GVA) i faste priser
cat("Henter nama_10_a64 (GVA)...\n")
gva_raw <- get_eurostat("nama_10_a64", time_format = "num")
saveRDS(gva_raw, "data/eurostat_nama_10_a64_raw.rds")

cat("GVA dimensjoner:", dim(gva_raw), "\n")
cat("na_item verdier:", unique(gva_raw$na_item), "\n")
cat("unit verdier:", unique(gva_raw$unit), "\n")

# Filtrer: GVA, kjedede volum (2010-priser)
gva <- gva_raw %>%
  filter(
    na_item == "B1G",         # Gross value added
    unit == "CLV10_MEUR"      # kjedede volum, mill. EUR, 2010-priser
  ) %>%
  select(geo, nace_r2, time, values) %>%
  rename(land = geo, nace = nace_r2, year = time, gva = values)

cat("GVA etter filtrering:", nrow(gva), "obs\n")
cat("NACE-sektorer i GVA:\n")
print(sort(unique(gva$nace)))

# B) Sysselsetting / timeverk
cat("\nHenter nama_10_a64_e (sysselsetting)...\n")
emp_raw <- get_eurostat("nama_10_a64_e", time_format = "num")
saveRDS(emp_raw, "data/eurostat_nama_10_a64_e_raw.rds")

cat("Employment dimensjoner:", dim(emp_raw), "\n")
cat("na_item verdier:", unique(emp_raw$na_item), "\n")
cat("unit verdier:", unique(emp_raw$unit), "\n")

# Timeverk (foretrukket mål for produktivitet)
hw <- emp_raw %>%
  filter(
    na_item == "EMP_DC",   # total employment domestic concept
    unit == "THS_HW"       # tusen timeverk
  ) %>%
  select(geo, nace_r2, time, values) %>%
  rename(land = geo, nace = nace_r2, year = time, hours = values)

# Hvis THS_HW ikke finnes, bruk THS_PER (tusen personer)
if (nrow(hw) == 0) {
  cat("MERK: THS_HW ikke tilgjengelig, bruker THS_PER (personer)\n")
  hw <- emp_raw %>%
    filter(na_item == "EMP_DC", unit == "THS_PER") %>%
    select(geo, nace_r2, time, values) %>%
    rename(land = geo, nace = nace_r2, year = time, hours = values)
}

cat("Timeverk etter filtrering:", nrow(hw), "obs\n")


# === 3. NACE-MAPPING =============================================

cat("\n========== 3. NACE-mapping ==========\n")

# Eurostat AI-tabellen bruker brede NACE-grupper.
# Nasjonalregnskapet har finere inndeling.
# Vi må aggregere nasjonalregnskapet OPP til AI-tabellens nivå.

# Først: se hvilke NACE-koder som finnes i AI-tabellen
ai_nace <- sort(unique(ai_eu$nace))
cat("NACE i KI-tabellen:\n")
print(ai_nace)

# Og i nasjonalregnskapet
gva_nace <- sort(unique(gva$nace))
cat("\nNACE i GVA-tabellen (utvalg):\n")
print(head(gva_nace, 30))

# Mapping fra nasjonalregnskap (A64) til KI-tabellens NACE-grupper
# VIKTIG: Tilpass denne etter hva du faktisk ser i ai_nace!
# Typiske koder i isoc_eb_ain2: C, D-E, F, G, H, I, J, K, L, M_N
# eller: C, C10-C12, ..., J, J58-J60, J61, J62-J63, K, L, M, N

nace_map <- tribble(
  ~nace_a64,  ~nace_ai,     ~nace_label,
  # Manufacturing
  "C",        "C",          "Industri",
  # Utilities (D+E)
  "D",        "D-E",        "Kraft, vann, avfall",
  "E",        "D-E",        "Kraft, vann, avfall",
  # Construction
  "F",        "F",          "Bygg og anlegg",
  # Trade (G samlet, eller G45/G46/G47 separat)
  "G",        "G",          "Varehandel",
  # Transport
  "H",        "H",          "Transport og lagring",
  # Accommodation
  "I",        "I",          "Overnatting og servering",
  # ICT
  "J",        "J",          "Informasjon og kommunikasjon",
  # Finance
  "K",        "K",          "Finans og forsikring",
  # Real estate
  "L",        "L68",        "Eiendom",
  # Professional + admin services
  "M",        "M_N",        "Faglig/admin tjenesteyting",
  "N",        "M_N",        "Faglig/admin tjenesteyting"
)

# NB: Eksakte koder i isoc_eb_ain2 kan avvike (f.eks. "C-E", "D-E35" etc.)
# Du MÅ sjekke output fra ai_nace over og justere nace_map tilsvarende!
# Print en advarsel:
cat("\n⚠️  VIKTIG: Sjekk at nace_ai-kodene i nace_map matcher")
cat(" det du ser i ai_nace over. Juster manuelt hvis nødvendig.\n\n")

# Aggreger GVA
gva_agg <- gva %>%
  inner_join(nace_map, by = c("nace" = "nace_a64")) %>%
  group_by(land, nace_ai, year) %>%
  summarise(gva = sum(gva, na.rm = TRUE), .groups = "drop") %>%
  rename(nace = nace_ai)

# Aggreger timeverk
hw_agg <- hw %>%
  inner_join(nace_map, by = c("nace" = "nace_a64")) %>%
  group_by(land, nace_ai, year) %>%
  summarise(hours = sum(hours, na.rm = TRUE), .groups = "drop") %>%
  rename(nace = nace_ai)

# Konstruer arbeidsproduktivitet
prod <- gva_agg %>%
  inner_join(hw_agg, by = c("land", "nace", "year")) %>%
  filter(hours > 0) %>%
  mutate(
    lp    = gva / hours,
    ln_lp = log(lp)
  )

cat("Produktivitetspanel:", nrow(prod), "obs\n")


# === 4. BYGG HOVEDPANELET ========================================

cat("\n========== 4. Bygger panelet ==========\n")

# Hovedvariabel: "bruker minst én KI-teknologi"
# Filtrer til alle bedrifter 10+ (sjekk size-koden!)
ai_main <- ai_eu %>%
  filter(
    ai_var == "E_AI",  # bruker minst én KI — SJEKK KODEN
    str_detect(size, "10")  # alle 10+ ansatte — SJEKK KODEN
  ) %>%
  select(land, nace, year, ai_pct)

# Merge
panel <- ai_main %>%
  inner_join(prod, by = c("land", "nace", "year"))

cat("Panel dimensjoner:\n")
cat("  Observasjoner:", nrow(panel), "\n")
cat("  Land:", length(unique(panel$land)), "\n")
cat("  Sektorer:", length(unique(panel$nace)), "\n")
cat("  År:", sort(unique(panel$year)), "\n")

saveRDS(panel, "data/panel_main.rds")


# === 5. DESKRIPTIV STATISTIKK ====================================

cat("\n========== 5. Deskriptiv statistikk ==========\n")

# A) KI-adopsjon per sektor (EU-snitt)
ai_sector_mean <- ai_main %>%
  group_by(nace, year) %>%
  summarise(
    mean_ai = mean(ai_pct, na.rm = TRUE),
    sd_ai   = sd(ai_pct, na.rm = TRUE),
    n_land  = sum(!is.na(ai_pct)),
    .groups = "drop"
  )

cat("\nKI-adopsjon per sektor (EU-snitt):\n")
ai_sector_mean %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  arrange(desc(mean_ai)) %>%
  print(n = 15)

# B) Plot: KI-adopsjon over tid per sektor
p1 <- ai_sector_mean %>%
  ggplot(aes(x = year, y = mean_ai, color = nace)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "KI-adopsjon per NACE-sektor, EU-gjennomsnitt",
    x = "År", y = "Andel foretak med KI (%)",
    color = "NACE-sektor"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/fig1_ai_adoption_by_sector.png", p1, width = 10, height = 6)

# C) Plot: Norge vs EU-snitt
nordic <- c("NO", "SE", "DK", "FI")

ai_nordic <- ai_main %>%
  filter(land %in% nordic) %>%
  group_by(land, year) %>%
  summarise(mean_ai = mean(ai_pct, na.rm = TRUE), .groups = "drop")

ai_eu_avg <- ai_main %>%
  group_by(year) %>%
  summarise(mean_ai = mean(ai_pct, na.rm = TRUE), .groups = "drop") %>%
  mutate(land = "EU-snitt")

p2 <- bind_rows(ai_nordic, ai_eu_avg) %>%
  ggplot(aes(x = year, y = mean_ai, color = land)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "KI-adopsjon: Norden vs EU-snitt",
    x = "År", y = "Andel foretak med KI (%)",
    color = "Land"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/fig2_nordic_vs_eu.png", p2, width = 8, height = 5)


# === 6. NORSK KONTEKST: K vs J I NORDEN ==========================

cat("\n========== 6. Finans (K) vs IKT (J) i Norden ==========\n")

# Hent K og J for nordiske land
kj_nordic <- ai_eu %>%
  filter(
    land %in% nordic,
    nace %in% c("J", "K"),        # IKT og Finans — SJEKK KODER
    ai_var == "E_AI",             # bruker minst én KI
    str_detect(size, "10")
  ) %>%
  select(land, nace, year, ai_pct) %>%
  pivot_wider(names_from = nace, values_from = ai_pct, names_prefix = "ai_")

cat("\nK vs J i Norden:\n")
print(kj_nordic, n = 20)

# Beregn ratio gamma = K/J
kj_nordic <- kj_nordic %>%
  mutate(gamma = ai_K / ai_J)

cat("\nRatio gamma (K/J):\n")
kj_nordic %>%
  filter(!is.na(gamma)) %>%
  group_by(land) %>%
  summarise(
    mean_gamma = mean(gamma, na.rm = TRUE),
    sd_gamma   = sd(gamma, na.rm = TRUE),
    n          = n()
  ) %>%
  print()

# Nordisk gjennomsnitt av gamma
gamma_nordic <- kj_nordic %>%
  filter(!is.na(gamma)) %>%
  summarise(
    gamma_mean = mean(gamma),
    gamma_sd   = sd(gamma),
    gamma_min  = min(gamma),
    gamma_max  = max(gamma),
    n          = n()
  )

cat("\nNordisk gamma (K/J):\n")
print(gamma_nordic)

# Hvis du vil imputere norsk K (for deskriptive figurer, IKKE regresjon):
# norsk_J <- dine SSB-data for seksjon J
# norsk_K_hat <- gamma_nordic$gamma_mean * norsk_J


# === 7. EXPOSURE × POST ESTIMERING ===============================

cat("\n========== 7. Exposure × Post ==========\n")

# Bygg eksponeringsindeks: 2021 EU-gjennomsnitt per sektor
# (forhåndsbestemt, pre-ChatGPT)
exposure_2021 <- ai_main %>%
  filter(year == 2021) %>%
  group_by(nace) %>%
  summarise(exposure = mean(ai_pct, na.rm = TRUE), .groups = "drop")

cat("\nEksponeringsindeks (2021 EU-snitt):\n")
exposure_2021 %>% arrange(desc(exposure)) %>% print()

# Legg til panel
panel <- panel %>%
  left_join(exposure_2021, by = "nace", suffix = c("", "_base")) %>%
  mutate(
    post       = if_else(year >= 2023, 1L, 0L),
    exp_x_post = exposure * post
  )

# --- Modell 1: Naiv OLS (betinget korrelasjon) ---
m1 <- feols(ln_lp ~ ai_pct | land + year, data = panel,
            cluster = ~land)

# --- Modell 2: Exposure × Post (hoveddspesifikasjon) ---
m2 <- feols(ln_lp ~ exp_x_post | land + nace + year, data = panel,
            cluster = ~land)

# --- Modell 3: Exposure × Post med kontroller ---
# (legg til kontroller etter behov: FoU, kapitalintensitet, etc.)
m3 <- feols(ln_lp ~ exp_x_post + ai_pct | land + nace + year, data = panel,
            cluster = ~land)

# Vis resultater
cat("\n--- Regresjonsresultater ---\n")
etable(m1, m2, m3,
       headers = c("OLS", "Exp×Post", "Exp×Post + AI"),
       fitstat = c("n", "r2", "ar2"))

# Lagre tabell
sink("output/regression_table.txt")
etable(m1, m2, m3,
       headers = c("OLS", "Exp×Post", "Exp×Post + AI"),
       fitstat = c("n", "r2", "ar2"))
sink()


# === 8. EVENT-STUDY ==============================================

cat("\n========== 8. Event-study ==========\n")

# Interager eksponering med årsdummier (referanseår = 2021)
panel <- panel %>%
  mutate(year_f = factor(year))

m_event <- feols(
  ln_lp ~ i(year_f, exposure, ref = "2021") | land + nace,
  data = panel,
  cluster = ~land
)

# Event-study plot
png("output/fig3_event_study.png", width = 800, height = 500)
iplot(m_event,
      main = "Event-study: KI-eksponering og arbeidsproduktivitet",
      xlab = "År",
      ylab = "Koeffisient (log produktivitet)")
abline(v = 2022.5, lty = 2, col = "red")
text(2022.5, par("usr")[4] * 0.9, "ChatGPT\nlansert", pos = 4, col = "red", cex = 0.8)
dev.off()

cat("\nEvent-study koeffisienter:\n")
summary(m_event)


# === 9. LAGRE ALT ================================================

cat("\n========== 9. Lagrer ==========\n")

saveRDS(panel, "data/panel_final.rds")
saveRDS(ai_eu, "data/ai_eurostat_clean.rds")
saveRDS(prod, "data/productivity_eurostat.rds")

cat("\n✓ Ferdig! Sjekk output/-mappen for figurer og tabeller.\n")
cat("✓ Sjekk data/-mappen for mellomlagrede datasett.\n")

# ============================================================
# NESTE STEG:
# - Verifiser NACE-kodene (se ⚠️-merknadene over)
# - Legg til Eloundou et al. (2024) eksponeringsmål som alternativ
# - Robusthetstester: ekskluder J (IKT), vekt med sysselsetting
# - Heterogenitet: GenAI (E_AINLG) vs tradisjonell KI (E_AIML)
# - IV: instrumenter med cross-country adopsjon