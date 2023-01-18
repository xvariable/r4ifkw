# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022/23
# Julia Niemann-Lenz
# Skript zur achten Sitzung, 2023-01-11

# #########################################################################

# Inhalte: 
# - Korrelation
# - Regression
# - Moderation Mediation

# #########################################################################

pacman::p_load(psych,          # für Korrelationen
               tidycomm,       # für Korrelationen
               tidyverse,      # Für die Datenauswahl und die Pipe
               corrr,          # für Korrelationsmatrizen
               palmerpenguins, # für die Daten
               lm.beta         # für standardisierte Regressionskoeffizienten
               ) 

?penguins
names(penguins)


# Streudiagramm -----------------------------------------------------------
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = flipper_length_mm)) +
  geom_point()


# Korrelationen -----------------------------------------------------------

# Vier von wahnsinnig vielen Möglichkeiten, mit R Korrelationen zu berechnen.
# Auswahl je nach Geschmack, ohnehin geladenen Paketen und Zielen.


# Base-R Kovarianz & Korrelation
cov(penguins$bill_length_mm, penguins$flipper_length_mm, 
    use = "complete.obs")

cor(penguins$bill_length_mm, penguins$flipper_length_mm, 
    use = "complete.obs",
    method = "pearson") # Pearson ist Standard

cor.test(penguins$bill_length_mm, penguins$flipper_length_mm, 
         use = "complete.obs",
         alternative = "two.sided") # Zweiseitig ist Standard


# Tidycomm
penguins %>% 
  tidycomm::correlate(bill_length_mm:body_mass_g) 

penguins %>% 
  tidycomm::correlate(bill_length_mm:body_mass_g) %>% 
  tidycomm::to_correlation_matrix()


# Psych Korrelation & Partialkorrelation
penguins %>%
  select(bill_length_mm, flipper_length_mm) %>% 
  psych::corr.test(use = "pairwise", method = "pearson")


penguins %>%
  nrow()

penguins %>%
  select(bill_length_mm, flipper_length_mm, body_mass_g) %>% 
  psych::partial.r(use="pairwise", method="pearson") %>% 
  psych::corr.p(n = 344)


# Korrelationsmatrizen und Korrelationsplots mit corrr
mtcars %>%
  corrr::correlate()

# mit Formatierung
mtcars %>% 
  corrr::correlate() %>% 
  corrr::shave() 

# Plot
mtcars %>% 
  corrr::correlate() %>% 
  corrr::rplot()

# schönerer Plot 
mtcars %>% 
  corrr::correlate() %>% 
  corrr::rearrange() %>% # Der Größe nach sortieren
  corrr::shave() %>%     # Dopplungen löschen
  corrr::rplot()


# Regression --------------------------------------------------------------
# Im folgenden Inetressiert uns der Zusammenhang zwischen 
# politischem Interesse under politischen Partizipation bei der Gen Z.

# Hypothese: 
# Je höher das politische Interesse, desto mehr patizipieren die 
# Jugendlichen und jungen Erwachsenen

df <- haven::read_sav("data/gen_z_recoded.sav") %>% 
  # Bilden eines Summenindices für politische Partizipation:
  add_index(name = pol_part_sx,
            pol_part_wahl:pol_part_anderes_engagement,
            type = "sum")

# Univariate Analyse und visuelle Datenexploration
df %>% 
  select(politisches_interesse, pol_part_sx) %>% 
  describe()
  
df %>% 
  tab_frequencies(politisches_interesse)

df %>% 
  tab_frequencies(pol_part_sx)

df %>% 
  ggplot(aes(x = politisches_interesse, y = pol_part_sx)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  xlab("politisches Interesse") +
  ylab("politische Partizipation")

# Regression berechnen und ausgeben / Die Formel-Schreibweise von R
rm <- lm(pol_part_sx ~ politisches_interesse, data = df)

rm

summary(rm)

lm.beta::lm.beta(rm) 
#

# Multiple Regression mit "+"
mrm <- lm(pol_part_sx ~ politisches_interesse + alter, data = df)
summary(mrm)


# Interaktionseffekte mit "*" 
# (beide Haupteffekte bleiben im Modell und müssen nicht in die Formel geschreiben werden)
irm <- lm(pol_part_sx ~ politisches_interesse * alter, data = df)
summary(irm)



# Prüfung der Vorrausstzungen der Regression ----------------

# Plots mit Residuen
par(mfrow = c(2,2)) # Ändert die Parameter so, dass im folgenden alle 4 Grafiken gleichzeitig dargestellt.
plot(mrm)

# oben links: Residuals vs. Fitted
# Die rote Linie (Loess-Kurve) soll paralel zur X-Achse verlaufen und die Verteilung 
# soll möglichst unsystematisch sein.

# oben rechts: QQ-Plot
# Wenn die Residuen normalverteilt sind, liegen sie auf der diagonalen Linie.

# unten links: Scale-Location
# Homoskedastizität besteht, wenn die Werte möglichst unsystematisch im Diagramm
# verteilt sind

# unten rechts: Residuals vs. Leverage
# markiert Fälle, die einen besonders hohen Einfluss auf die Regressionsgleichung haben.
# Insbesondere auch Werte jenseits der Cook´s distance.


# Standardisierte Residuen aller Fälle (zur Identifikation von Ausreißern)
rstandard(mrm)

# Multikollinearität
# Toleranz (sollte möglichst nah 1 sein, Kehrwert des Variance Inflation Factors)
1/car::vif(mrm)


# Mediation/Moderation mit processR ---------------------------------------

# Hayes, A. F. (2022). Introduction to Mediation, Moderation, and Conditional 
#    Process Analysis. A Regression-Based Approach (3rd Ed.). Guilford Press

# Mögliche Modelle: siehe Appendix A in dem Buch

# Tutorials für die verschiedenen Modelle (erklät Optionen und Output): 
# https://www.youtube.com/@RegorzStatistik/videos


# PROCESS für R ist KEIN Paket!

# "Installationsanaleitung"

# Download des R-Codes unter: 
# https://www.processmacro.org/ 
# bzw:
# https://haskayne.ucalgary.ca/CCRAM/resource-hub

# Zip entpacken und an einem zentralen Ort ablegen.

# Ausführen des "externen" R-Skripts um die Funktionen zu laden
# (Pfad muss angepasst werden, Achtung es dauert ein bisschen):
source("/Users/julia/Nextcloud/tools/processv42/PROCESS v4.2 for R/process.R")

# Es kann losgehen!
# Wichtig ist nur die Funktion process()


# Mediation mit "Model 4"
df %>% 
  process(x = "alter", 
          y = "pol_part_sx",
          m = "politisches_interesse", 
          model = 4
          ) 

# Bei mir ist das Bootstrapping kaputt. Leider nicht nachvollziehbar, woran das liegt.


# mehr Optionen 
# (leider nicht gerade selbsterklärend und keine Hilfe-Option, weil kein Paket)
df %>% 
  process(x = "alter", 
          y = "pol_part_sx",
          m = "politisches_interesse", 
          model = 4,
          stand = 1,    # für standardisierte Koeffizienten (beta)
          normal = 1,   # Sobel-Test
          total = 1,    # Total Effect ausgeben
          boot = 1000,  # Nummer der Bootstrap-Runden
          seed = 823134    # Seed für reproduzierbare Ergebnisse beim Bootstrapping
  ) 


# Moderation mit "Model 1"
df %>% 
  filter(geschlecht != 3) %>% 
  process(y = "politisches_interesse", 
          x = "pol_part_sx",
          w = "geschlecht", 
          model = 1)
