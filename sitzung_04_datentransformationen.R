# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022/23
# Julia Niemann-Lenz
# Skript zur 4. Sitzung, 2022-11-23

# #########################################################################

# Inhalte: 
# - Daten labeln im sjlabelled
# - Fälle filtern mit filter()
# - Fälle sortieren mit arrange()
# - Spalten auswählen mit select()


# Nützliche Links

# Kapitel aus R4DS
# https://r4ds.had.co.nz/transform.html

# Regular Expressions
# https://towardsdatascience.com/regular-expressions-clearly-explained-with-examples-822d76b037b4
# https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf


# Prerequisites -----------------------------------------------------------

pacman::p_load(sjlabelled,   # gelabelte Daten umlabeln
               sjmisc,       # verschiedene Datentransformationsfunktionen
               nycflights13, # Datensatz Flüge der NY-Flughäfen 2013
               tidyverse)    # für dplyr



# daten labelen -----------------------------------------------------------
df <- sjlabelled::read_spss("data/gen_z_small.sav") %>% 
  as_tibble()

df %>% 
  frq(politisches_interesse)

# Variablen-Label
(my_label <- get_label(df$politisches_interesse))

# Value Label
(my_labels <- get_labels(df$politisches_interesse))

# Value label löschen
df$politisches_interesse <- remove_labels(df$politisches_interesse, 
                                          labels = my_labels)

df %>% 
  sjmisc::frq(politisches_interesse)


# label wieder hinzufügen
df$politisches_interesse <- set_label(df$politisches_interesse, 
                                      label = my_label)

df$politisches_interesse <- set_labels(df$politisches_interesse, 
                                       labels = my_labels)

df %>% 
  sjmisc::frq(politisches_interesse) 



# filter() ----------------------------------------------------------------
# Den Datensatz nach bestimmten Kriterien filtern (SPSS "Fälle auswählen")

flights <- readRDS("data/flights.rds")

flights
?flights

flights_1_jan <- flights %>% 
  filter(month == 1 & day == 1) # Doppel-Gleichzeichen!

# btw, interessante Rundungsfehler:
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

# Deshalb besser:
near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)


# logische Operatoren
# == gleich
# ! nicht/Negation
# != ungleich
# & und
# | oder
# xor() Exklusives Oder (nur eins von beidem)
# () Klammerregeln gelten

# R hat auch die Operatoren && und ||. Die aber bitte nicht für Filter 
# verwenden, sondern für if-Befehle

# De Morgan’s law: 
# !(x & y) is the same as !x | !y, and 
# !(x | y) is the same as !x & !y


# Aufgaben:
# Alle Flüge im November & Dezember

flights %>% 
  filter(month == 11 | month == 12)

df2 <- flights %>% 
  filter(month >= 11)

# alternativ
nov_dec <- flights %>% 
  filter(month %in% c(11, 12))


# Flüge die nicht mehr als 2 Stunden Verspätung bei der Abreise (dep_delay) 
# und/oder der Ankunft (arr_delay) hatten

flights %>% 
  filter(!(arr_delay > 120 | dep_delay > 120))

flights %>% 
  filter(arr_delay <= 120 & dep_delay <= 120)

# Vergleiche mit fehlenden Werten
5 > NA
NA + 2
NA == NA

is.na(NA)

# nach fehlenden Werten filtern (dep_time)
flights %>% 
  filter(is.na(dep_time))

# und wenn ich die fehlenden rausschmeißen möchte?
flights %>% 
  filter(!is.na(dep_time))


# arrange() ---------------------------------------------------------------
# arrange sortiert die Zeilen nach einem oder mehreren Kriterien

flights %>% 
  arrange(month, day, sched_dep_time)


# desc() zum absteigend sortieren
flights %>% 
  arrange(desc(month), desc(day), desc(sched_dep_time))

# Missings werden ans Ende sortiert, egal ob mit oder ohne desc()
flights %>% 
  arrange(dep_time) %>% 
  tail()

flights %>% 
  arrange(desc(dep_time)) %>% 
  tail()

# Übung: Welcher Flug hatte die meiste Verspätung bei der Ankunft (arr_delay)?
flights %>% 
  arrange(desc(arr_delay)) %>% 
  View()


# select() ----------------------------------------------------------------
# Variablen/Spalten auswählen

# nach Namen
flights %>% 
  select(year, month, day)

# von... bis
flights %>% 
  select(year:day)

# alle, aber nicht diese
flights %>% 
  select(-(year:day))

?select

# alle die mit "arr_" beginnen
flights %>% 
  filter(dep_delay > 120) %>% 
  select(starts_with("arr_"), year, month, day)
  
flights %>% 
  filter(dep_delay > 120) %>% 
  select(starts_with("arr_"), day, everything())

# alle die "time" enthalten
flights %>% 
  select(contains("time")) # Achtung, nicht case-sensitiv!

# Mit "Regular Expressions"
flights %>% 
  select(matches(".+_.+_.+"))

# Umbennennen beim auswählen 
# (oft unpraktisch, weil alle anderen Variablen gelöscht werden)
flights %>% 
  select(TAG = day, MONAT = month, JAHR = year)

# rename() zum umbenennen
flights %>% 
  rename(TAG = day, MONAT = month, JAHR = year)

# Select zum umsortieren
flights %>% 
  select(tailnum, day, month, year, everything())

names(flights)

# Übung: Welche unterschiedlichen Arten fallen Euch ein, um nur die Variablen
# dep_time, dep_delay, arr_time und arr_delay auszuwählen?
