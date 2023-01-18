# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022/23
# Julia Niemann-Lenz
# Skript zur neunten Sitzung, 2023-01-18

# #########################################################################

# Inhalte: 
# Strukturgleichungsmodelle mit lavaan

# #########################################################################

pacman::p_load(lavaan,    # für die Strukturgleichungsanalyse
               tidyverse, # für dplyr & die Pipe
               haven      # zum Import von SPSS-Daten
) 

# Einstellung von Studierenden zur Nutzung von Wikipedia (Theory of Planned Behavior)
# Quelle:
# Kummer, C. (2013): Die Einstellung von Studenten zur Nutzung von Wikis im Studium. 
# GESIS Datenarchiv, Köln. ZA5683 Datenfile Version 1.0.0, doi:10.4232/1.11514 

df <- read_spss("data/ZA5683_v1_jn.sav")
names(df)

# Wir berechnen nur ein Teilmodell mit einem Mediationseffekt:
# Einfluss der Compatibility (COM = inwiefern passt Wikipedia zu meinem Arbeitsstil)
# auf Perceived Usefulness (PU) und der Einfluss dieser beiden Variablen auf
# Die Intention zur Nutzung von Wikipedia (Behavioral Intention BI).



# Pfadmodell --------------------------------------------------------------
# Zunächst als Pfadmodell mit Mittelwertindices

# Spezifikation des Modells in einem String
# "~" bedeutet "is regressed on"
pmod <-  "ix_PU ~ ix_COM
          ix_BI ~ ix_PU + ix_COM"

pmod_fit <- sem(model = pmod, data = df)

summary(pmod_fit, rsquare = TRUE, standardized = TRUE)



# Indirekte Effekte -------------------------------------------------------
# Indirekten und Gesamt-Effekt spezifizieren
# eigene PArameter spezifizieren mit ":="
pmod2 <-  "ix_PU ~ a*ix_COM
           ix_BI ~ b*ix_PU + c*ix_COM
           ab := a*b
           abc := a*b+c"

pmod2_fit <- sem(model = pmod2, data = df)

summary(pmod2_fit, rsquare = TRUE, standardized = TRUE)



# Messmodelle prüfen ------------------------------------------------------

# Test auf multivariate Normalverteilung (base R)
df %>% 
  select(PU1:PU4) %>% 
  mardia()    

# "=~" bedeutet "is manifested by" (bei latenten Variablen)
mmpu <-  "PU =~ PU1 + PU2 + PU3 + PU4"
mmpu_fit <- cfa(model = mmpu, data = df)
summary(mmpu_fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)

mmcom <-  "COM =~ COM1 + COM2 + COM3 + COM4"
mmcom_fit <- cfa(model = mmcom, data = df)
summary(mmcom_fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)

mmbi <-  "BI =~ BI1 + BI2 + BI3"
mmbi_fit <- cfa(model = mmbi, data = df)
summary(mmbi_fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)


# Mehr Fit-Kriterien ausgeben:
fitMeasures(mmpu_fit, "NFI")
fitMeasures(mmpu_fit)


# Messmodelle gemeinsam prüfen
mm <-  "COM =~ COM1 + COM2 + COM3 + COM4
        METH =~ COM1 + COM2
        BI =~ BI1 + BI2 + BI3
        COM ~~ BI"
mm_fit <- cfa(model = mm, data = df)
summary(mm_fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)



# Messmodelle optimieren --------------------------------------------------

# Modification Indices
inspect(mm_fit, "mi") %>% 
  filter(mi >= 10)

# standardisierte Residuen
residuals(mm_fit, type = "standardized")



# Optimierte Messmodelle
mmpu2 <-  "PU =~ PU1 + PU2 + PU3 + PU4
           PU1 ~~ PU4"
mmpu2_fit <- cfa(model = mmpu2, data = df)
summary(mmpu2_fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)


mmcom2 <-  "COM =~ COM1 + COM2 + COM3 + COM4
            COM1 ~~ COM4"
mmcom2_fit <- cfa(model = mmcom2, data = df)
summary(mmcom2_fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)



# Strukturmodell ----------------------------------------------------------
mysem <- "PU =~ PU1 + PU2 + PU3 + PU4
            PU1 ~~ PU4
          COM =~ COM1 + COM2 + COM3 + COM4
            COM1 ~~ COM4
          BI =~ BI1 + d*BI2 + d*BI3

          PU ~ a*COM
          BI ~ b*PU + c*COM

          ab := a*b
          abc := (a*b)+c"
mysem_fit <- sem(model = mysem, data = df, estimator = "MLM")
summary(mysem_fit, rsquare = TRUE, standardized = TRUE, fit.measures = TRUE)


