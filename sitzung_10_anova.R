# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022/23
# Julia Niemann-Lenz
# Skript zur zehnten Sitzung, 2023-01-25

# #########################################################################

# Inhalte: 
# - T-Tests
# - ANOVA

# #########################################################################

pacman::p_load(tidyverse,      # für die Pipe
               rstatix,        # für pipe-friendly Syntax
               palmerpenguins, # Daten
               psych,           # deskriptive Statistiken
               flextable
) 


df_prosocial <- read_csv2("data/gaming_data.csv")

# Gruppe 1 wurde gebeten, zwanzig Minuten lang ein Superhelden-Spiel zu spielen.
# Gruppe 2 spielte hingegen genauso lange ein Rennspiel.

# aV: Wiviel von ihrer 10 Euro-Aufwandsentschädigung spenden die Proband:innen?


head(df_prosocial)

df_prosocial %>% 
  ggplot(aes(Spende_t0)) +
  geom_bar() 

psych::describe(df_prosocial$Spende_t0)


# T-Tests bei einer Stichprobe -----------------------------------------------------

df_prosocial %>% 
  t_test(Spende_t0 ~ 1, mu = 1.5, alternative = "greater") 

df_prosocial %>% 
  cohens_d(Spende_t0 ~ 1, mu = 1.5)


# alternativ mit base R
t.test(x = df_prosocial$Spende_t0, mu = 1.5, alternative = "greater")   



# T-Test für unabhängige Stichproben --------------------------------------

df_prosocial %>% 
  group_by(Gruppe) %>% 
  summarise(M = mean(Spende_t1, na.rm = TRUE), 
            SD = sd(Spende_t1, na.rm = TRUE), 
            n = n())

df_prosocial %>% 
  filter(!is.na(Spende_t1)) %>% 
  ggplot(aes(Gruppe, Spende_t1, color = Gruppe)) +
    geom_boxplot() +
    theme(legend.position = "none")

df_prosocial %>% 
  filter(!is.na(Spende_t1)) %>% 
  ggplot(aes(Spende_t1, fill = Gruppe)) + 
  geom_density(alpha = 0.2)


# Levene-Test
df_prosocial %>% 
  levene_test(Spende_t1 ~ Gruppe)

# T-Test
tabelle1 <- df_prosocial %>% 
  t_test(Spende_t1 ~ Gruppe, var.equal = TRUE, alternative = "less") 

df_prosocial %>% 
  cohens_d(Spende_t1 ~ Gruppe, var.equal = TRUE, ci = TRUE) 

write_csv2(tabelle1, "mein_t-test.csv")


# T-Test für abhängige Stichproben ----------------------------------------
# Deskription als Grafik
df_prosocial %>% 
  ggplot() +
  geom_density(aes(Spende_t1), alpha = 0.2, fill = "blue") +
  geom_density(aes(Spende_t2), alpha = 0.2, fill = "orange") +
  facet_wrap(~Gruppe)

# Deskription als Tabelle
df_prosocial %>% 
  select(Gruppe, Spende_t1, Spende_t2) %>% 
  group_by(Gruppe) %>% 
  summarise(t1_M = mean(Spende_t1, na.rm= TRUE),
            t1_SD = SD(Spende_t1, na.rm= TRUE),
            t2_M = mean(Spende_t2, na.rm= TRUE),
            t2_SD = SD(Spende_t2, na.rm= TRUE))

# Levene-Test
df_prosocial_long <- df_prosocial %>%
  pivot_longer(cols = c(Spende_t1, Spende_t2), 
               names_to = "Zeitpunkt", 
               values_to = "Betrag")

df_prosocial_long %>% 
  group_by(Gruppe) %>% 
  levene_test(Betrag ~ Zeitpunkt)

# T-Test
df_prosocial_long %>% 
  group_by(Gruppe) %>% 
  t_test(Betrag ~ Zeitpunkt, 
         var.equal = FALSE, 
         paired = TRUE,
         alternative = "two.sided")



# ANOVA -------------------------------------------------------------------
# Zurück zu Pinguinen...
"
      -=(o '.
         '.-.\
         /|  \\
         '|  ||
         _\_):,_
"


# Normalverteilung?
penguins %>% 
  group_by(island) %>% 
  shapiro_test(bill_length_mm)

penguins %>% 
  ggplot(aes(bill_length_mm)) +
  geom_histogram() +
  facet_wrap(~island)

# Varianzhomogenität?
penguins %>% 
  levene_test(bill_length_mm ~ island)

# Welch Anova (mit Korrektur, wegen heterogener Varianzen)
penguins %>% 
  welch_anova_test(bill_length_mm ~ island)

# falls Varianzhomogenität gegeben wäre:
penguins %>% 
  anova_test(bill_length_mm ~ island, 
             type = 3, 
             effect.size = "ges") # Eta-Quadrat

penguins %>% 
  pairwise_t_test(bill_length_mm ~ island,
                  p.adjust.method	= "bonferroni")

penguins %>% 
  tukey_hsd(bill_length_mm ~ island)



# Mehrfaktorielle ANOVA
penguins %>% 
  drop_na(bill_length_mm, island, sex) %>% 
  anova_test(bill_length_mm ~ island * sex)


# ANCOVA
# Covariate zuerst in die Formel!
penguins %>% 
  anova_test(bill_length_mm ~ body_mass_g + island * sex) 


penguins %>% 
  emmeans_test(
    bill_length_mm ~ island, covariate = body_mass_g,
    p.adjust.method = "bonferroni"
  )


# Tabellen-Output --------------------------------------------------------

# Hilfe für das Paket flextable: https://ardata-fr.github.io/flextable-book/

penguins %>% 
  anova_test(bill_length_mm ~ body_mass_g + island * sex) %>% 
  as_tibble() %>% 
  mutate(p = round(p, 3)) %>% 
  flextable() %>% 
  add_footer_lines("Some ANCOVA I just calculated...") %>% 
  hline_bottom(part = "footer") %>% 
  save_as_docx(path = "some_table.docx")
