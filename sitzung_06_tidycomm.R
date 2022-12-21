# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022
# Julia Niemann-Lenz
# Skript zur sechsten Sitzung, 2022-12-14

# #########################################################################

# Inhalte: 
# Vorstellung tidycomm-Paket

# #########################################################################

pacman::p_load(tidycomm)

?WoJ
names(WoJ)

# deskriptive Statistiken --------------------------------------------------

# describe, um einen Überblick zu bekommen
WoJ %>% 
  describe()

# desscribe für kategoriale Variablen
WoJ %>% 
  describe_cat()

# Häufigkeitsauszählungen
WoJ %>% 
  tab_frequencies(employment)

WoJ %>% 
  tab_frequencies(country)


# bivariate Verfahren -----------------------------------------------------

# Kreuztabelle
WoJ %>%
  crosstab(employment, country)

# mit Formatierungsanweisungen
df <- WoJ %>%
  crosstab(country, employment, 
           add_total = TRUE, 
           percentages = TRUE,
           chi_square = TRUE)

# was tidycomm zurückgibt ist auch ein Datensatz
names(df)
df

# T-Test
my_test <- WoJ %>% 
  t_test(group_var = temp_contract, 
         autonomy_selection, autonomy_emphasis)


WoJ %>% 
  t_test(group_var = employment, 
               autonomy_selection, autonomy_emphasis,
               levels = c("Full-time", "Freelancer"))

# ANOVA
WoJ %>% 
  unianova(group_var = employment, autonomy_selection, autonomy_emphasis)

# Mit Mittelwerten & Post-hoc-Text (Tukey)
a <- WoJ %>% 
  unianova(group_var = employment, autonomy_selection, autonomy_emphasis,
         descriptives = TRUE,
         post_hoc = TRUE)

# Anzeige des Post-hoc-Tests
a$post_hoc


# Korrelation
WoJ %>% 
  correlate(work_experience, autonomy_selection, autonomy_emphasis)


# Als Matrix
WoJ %>% 
  correlate(work_experience, autonomy_selection, autonomy_emphasis) %>% 
  to_correlation_matrix()


# Reliabilität ------------------------------------------------------------

# Index berechnen
df <- WoJ %>% 
  add_index(name = ethics_mx, 
            ethics_1, ethics_2, ethics_3, ethics_4)

df %>% 
  get_reliability()

# möglich wäre auch ein Summenindex
WoJ %>% 
  add_index(name = ethics_sx, 
            ethics_1, ethics_2, ethics_3, ethics_4,
            type = "sum")

# Reliabilität berechnen
WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
  add_index(trust_in_politics, trust_parliament, trust_government, trust_parties, trust_politicians) %>% 
  get_reliability()


# get_reliability ist eine Wrapper-Funktion für die Funktion ci.reliability aus 
# dem Paket MBESS. Deshalb kann man hier auch alle Arguemnte angeben, die diese 
# Funktion unterstützt, z.B. type = "omega", interval.type = "mlr" für Konfidenz-
# intervalle (mit MLR geschätzt)

WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
  add_index(trust_in_politics, trust_parliament, trust_government, trust_parties, trust_politicians) %>% 
  get_reliability(type = "omega", interval.type = "mlr")



# inter coder reliability -------------------------------------------------

# ein Inhaltsanalyse-Datensatz:
fbposts

# Standard-Koeffizienten
fbposts %>% 
  test_icr(unit_var = post_id, 
           coder_var = coder_id, 
           pop_elite, pop_people, pop_othering)

# Weitere Koeffizienten möglich (siehe Hilfe)
fbposts %>% 
  test_icr(unit_var = post_id, 
           coder_var = coder_id, 
           pop_elite, pop_people, pop_othering,
           kripp_alpha = TRUE,
           fleiss_kappa = TRUE,‚
           s_lotus = TRUE)
