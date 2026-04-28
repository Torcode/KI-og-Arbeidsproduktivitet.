library(eurostat)
library(dplyr)




#Datahent
# store datafiler, tar kanskje ett minutt eller to.


ai_raw <- get_eurostat("isoc_eb_ain2", time_format = "num")
saveRDS(ai_raw, "data/ai_raw.rds")

cat("Kolonner:", paste(names(ai_raw), collapse=", "), "\n")
cat("\nNACE:\n"); print(sort(unique(ai_raw$nace_r2)))
cat("\nIndikator:\n"); print(sort(unique(ai_raw$indic_is)))

cat("\nLand:\n"); print(sort(unique(ai_raw$geo)))
cat("\nUnit:\n"); print(sort(unique(ai_raw$unit)))

cat("\nStørrelse:\n"); print(sort(unique(ai_raw$size_emp)))

cat("\nÅr:\n"); print(sort(unique(ai_raw$TIME_PERIOD)))







####################

gva_raw <- get_eurostat("nama_10_a64", time_format = "num")
saveRDS(gva_raw, "data/gva_raw.rds")

cat("Dim:", dim(gva_raw), "\n")
cat("Kolonner:", paste(names(gva_raw), collapse=", "), "\n")
cat("\nNACE:\n"); print(sort(unique(gva_raw$nace_r2)))
cat("\nna_item:\n"); print(sort(unique(gva_raw$na_item)))
cat("\nunit:\n"); print(sort(unique(gva_raw$unit)))
cat("\nÅr (range):", range(gva_raw$TIME_PERIOD), "\n")



cat("\nunit:\n"); print(sort(unique(gva_raw$unit)))
cat("\nNACE:\n"); print(sort(unique(gva_raw$nace_r2)))





###### Sysselsetting

emp_raw <- get_eurostat("nama_10_a64_e", time_format = "num")
saveRDS(emp_raw, "data/emp_raw.rds")

cat("Dim:", dim(emp_raw), "\n")
cat("Kolonner:", paste(names(emp_raw), collapse=", "), "\n")
cat("\nna_item:\n"); print(sort(unique(emp_raw$na_item)))
cat("\nunit:\n"); print(sort(unique(emp_raw$unit)))
cat("\nNACE (20 første):\n"); print(head(sort(unique(emp_raw$nace_r2)), 20))
cat("\nÅr (range):", range(emp_raw$TIME_PERIOD), "\n")







#### 

cap_raw <- get_eurostat("nama_10_nfa_st", time_format = "num")
saveRDS(cap_raw, "data/cap_raw.rds")

cat("Dim:", dim(cap_raw), "\n")
cat("Kolonner:", paste(names(cap_raw), collapse=", "), "\n")
cat("\nNACE:\n"); print(sort(unique(cap_raw$nace_r2)))
cat("\nasset10:\n"); print(sort(unique(cap_raw$asset10)))
cat("\nunit:\n"); print(sort(unique(cap_raw$unit)))
cat("\nÅr (range):", range(cap_raw$TIME_PERIOD), "\n")





#### Bygger panel


nace_ai  <- sort(unique(ai_raw$nace_r2))
nace_gva <- sort(unique(gva_raw$nace_r2))
nace_emp <- sort(unique(emp_raw$nace_r2))
nace_cap <- sort(unique(cap_raw$nace_r2))

overlap <- Reduce(intersect, list(nace_ai, nace_gva, nace_emp, nace_cap))
cat("Direkte overlapp (", length(overlap), "koder):\n")
print(overlap)

cat("\nKun i KI-tabell (ikke i alle andre):\n")
print(setdiff(nace_ai, overlap))




#### Sjekker overlapp

# Ikke-overlappende NACE-koder (detaljert der mulig)
nace_use <- c(
  # Manufacturing (13 delsektorer, IKKE "C")
  "C10-C12", "C13-C15", "C16-C18", "C19", "C20", "C21",
  "C22_C23", "C24_C25", "C26", "C27", "C28", "C29_C30", "C31-C33",
  # Utilities
  "D", "E",
  # Construction
  "F",
  # Trade (3 delsektorer, IKKE "G")
  "G45", "G46", "G47",
  # Transport, overnatting (aggregat)
  "H", "I",
  # IKT (3 delsektorer, IKKE "J")
  "J58-J60", "J61", "J62_J63",
  # Eiendom
  "L",
  # Faglig tjenesteyting (3 delsektorer, IKKE "M")
  "M69-M71", "M72", "M73-M75",
  # Admin tjenesteyting (aggregat)
  "N"
)

cat(length(nace_use), "ikke-overlappende sektorer\n")

# Filtrer alle fire tabeller
ai <- ai_raw %>%
  filter(nace_r2 %in% nace_use,
         indic_is == "E_AI_TANY",
         unit == "PC_ENT") %>%
  select(geo, nace_r2, TIME_PERIOD, values) %>%
  rename(land=geo, nace=nace_r2, year=TIME_PERIOD, ai_pct=values)

gva <- gva_raw %>%
  filter(nace_r2 %in% nace_use,
         na_item == "B1G",
         unit == "CLV15_MEUR") %>%
  select(geo, nace_r2, TIME_PERIOD, values) %>%
  rename(land=geo, nace=nace_r2, year=TIME_PERIOD, gva=values)

emp <- emp_raw %>%
  filter(nace_r2 %in% nace_use,
         na_item == "EMP_DC",
         unit == "THS_HW") %>%
  select(geo, nace_r2, TIME_PERIOD, values) %>%
  rename(land=geo, nace=nace_r2, year=TIME_PERIOD, hours=values)

cap <- cap_raw %>%
  filter(nace_r2 %in% nace_use,
         asset10 == "N11N",
         unit == "CLV15_MEUR") %>%
  select(geo, nace_r2, TIME_PERIOD, values) %>%
  rename(land=geo, nace=nace_r2, year=TIME_PERIOD, capital=values)

# Sjekk størrelse
cat("\nEtter filtrering:\n")
cat("AI: ", nrow(ai), "\n")
cat("GVA:", nrow(gva), "\n")
cat("EMP:", nrow(emp), "\n")
cat("CAP:", nrow(cap), "\n")

# Merge — bare år som finnes i KI-tabellen
panel <- ai %>%
  inner_join(gva, by = c("land","nace","year")) %>%
  inner_join(emp, by = c("land","nace","year")) %>%
  inner_join(cap, by = c("land","nace","year")) %>%
  filter(gva > 0, hours > 0, capital > 0)

cat("\n--- PANEL ---\n")
cat("Obs:     ", nrow(panel), "\n")
cat("Land:    ", length(unique(panel$land)), "\n")
cat("Sektorer:", length(unique(panel$nace)), "\n")
cat("År:      ", sort(unique(panel$year)), "\n")

saveRDS(panel, "data/panel.rds")




#################################




library(ggplot2)

# Rask sjekk
cat("Manglende:\n")
colSums(is.na(panel))

# Log-variabler
panel <- panel %>%
  mutate(ln_y = log(gva),
         ln_l = log(hours),
         ln_k = log(capital))

# Rask figur: KI vs produktivitet
panel %>%
  mutate(lp = gva/hours) %>%
  ggplot(aes(x = ai_pct, y = log(lp))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~year) +
  labs(title = "KI-adopsjon vs arbeidsproduktivitet",
       x = "KI-adopsjon (%)", y = "ln(GVA/timeverk)") +
  theme_minimal()

# Første SFA
library(frontier)

sfa1 <- sfa(ln_y ~ ln_l + ln_k | ai_pct, data = panel)
summary(sfa1)






# 1. Sjekk: fjern EU/EA-aggregater som dobbelteller
panel2 <- panel %>%
  filter(!land %in% c("EU27_2020", "EA"))

sfa2 <- sfa(ln_y ~ ln_l + ln_k | ai_pct, data = panel2)
summary(sfa2)

# 2. Translog-spesifikasjon
panel2 <- panel2 %>%
  mutate(ln_l2 = ln_l^2,
         ln_k2 = ln_k^2,
         ln_lk = ln_l * ln_k)

sfa3 <- sfa(ln_y ~ ln_l + ln_k + ln_l2 + ln_k2 + ln_lk | ai_pct,
            data = panel2)
summary(sfa3)







library(plm)

# Lag en unik ID per land-sektor
panel2$id <- paste(panel2$land, panel2$nace, sep = "_")

pdata <- pdata.frame(panel2, index = c("id", "year"))

sfa4 <- sfa(ln_y ~ ln_l + ln_k | ai_pct,
            data = pdata,
            timeEffect = TRUE)
summary(sfa4)




# SFA med år i ineffektivitetsligningen
panel2$y2023 <- as.integer(panel2$year == 2023)
panel2$y2024 <- as.integer(panel2$year == 2024)

sfa5 <- sfa(ln_y ~ ln_l + ln_k | ai_pct + y2023 + y2024,
            data = panel2)
summary(sfa5)

# Sammenlign med vanlig panel FE (for Andrea)
library(fixest)

fe1 <- feols(ln_y ~ ln_l + ln_k + ai_pct | land + nace + year,
             data = panel2, cluster = ~land)
summary(fe1)



##############



# 1. Har vi riktig unit overalt?
panel2 %>% head(3)

# 2. Er verdiene rimelige?
panel2 %>%
  filter(land == "NO", nace == "J62_J63", year == 2023) %>%
  select(land, nace, year, gva, hours, capital, ai_pct)

# 3. Mistet vi mye data i merge?
cat("AI obs før merge:", nrow(ai), "\n")
cat("Panel obs etter merge:", nrow(panel2), "\n")
cat("Tap:", round(100*(1 - nrow(panel2)/nrow(ai)), 1), "%\n")

# 4. Hvilke land mangler kapitaldata?
ai_land <- unique(ai$land)
panel_land <- unique(panel2$land)
cat("\nLand i KI men ikke i panel:\n")
print(setdiff(ai_land, panel_land))







panel_lp <- ai %>%
  filter(!land %in% c("EU27_2020", "EA")) %>%
  inner_join(gva, by = c("land","nace","year")) %>%
  inner_join(emp, by = c("land","nace","year")) %>%
  filter(gva > 0, hours > 0) %>%
  mutate(lp = gva / hours,
         ln_lp = log(lp))

cat("Obs:     ", nrow(panel_lp), "\n")
cat("Land:    ", length(unique(panel_lp$land)), "\n")
cat("Sektorer:", length(unique(panel_lp$nace)), "\n")
cat("År:      ", sort(unique(panel_lp$year)), "\n")






library(fixest)

fe_lp <- feols(ln_lp ~ ai_pct | land + nace + year,
               data = panel_lp, cluster = ~land)
summary(fe_lp)







# 1. Legg til kapital for de som HAR det (subset)
panel_full <- panel_lp %>%
  inner_join(cap %>% select(land, nace, year, capital),
             by = c("land","nace","year")) %>%
  filter(capital > 0) %>%
  mutate(ln_k = log(capital),
         kl_ratio = log(capital / hours))  # kapitalintensitet

# 2. Tre spesifikasjoner
fe_a <- feols(ln_lp ~ ai_pct | land + nace + year,
              data = panel_lp, cluster = ~land)

fe_b <- feols(ln_lp ~ ai_pct + kl_ratio | land + nace + year,
              data = panel_full, cluster = ~land)

fe_c <- feols(ln_lp ~ ai_pct | land + nace + year,
              data = panel_full, cluster = ~land)

etable(fe_a, fe_b, fe_c,
       headers = c("Alle (uten K)", "Med K/L-ratio", "Subset uten K"),
       fitstat = c("n", "r2", "wr2"))





######## Difference Hill og SFA


# Sjekk problemet
panel_full %>%
  filter(!is.na(d_ln_lp)) %>%
  nrow()

# Fiks: bruk eksplisitt year-matching i stedet for lag()
diff_data <- panel_full %>%
  select(land, nace, year, ln_lp, ai_pct, kl_ratio) %>%
  filter(!is.na(ai_pct)) %>%
  arrange(land, nace, year) %>%
  group_by(land, nace) %>%
  mutate(d_ln_lp = ln_lp - dplyr::lag(ln_lp, order_by = year),
         d_ai    = ai_pct - dplyr::lag(ai_pct, order_by = year),
         d_kl    = kl_ratio - dplyr::lag(kl_ratio, order_by = year)) %>%
  ungroup() %>%
  filter(!is.na(d_ln_lp), !is.na(d_ai))

cat("Diff obs:", nrow(diff_data), "\n")
cat("Diff år:", sort(unique(diff_data$year)), "\n")

fe_diff <- feols(d_ln_lp ~ d_ai + d_kl, data = diff_data, cluster = ~land)
summary(fe_diff)




# Trenger ln_y, ln_l, ln_k i panel_full
panel_full <- panel_full %>%
  mutate(ln_y = log(gva),
         ln_l = log(hours),
         ln_k = log(capital))

sfa_final <- sfa(ln_y ~ ln_l + ln_k | ai_pct, data = panel_full)
summary(sfa_final)



