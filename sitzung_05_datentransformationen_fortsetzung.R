# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022/23
# Julia Niemann-Lenz
# Skript zur 5. Sitzung, 2022-11-30

# #########################################################################

# Inhalte: 
# - Neue Variablen mit mutate()
# - Daten zusammenfassen mit summarize()
# - Fälle gruppieren mit group_by()
# - Funktionen aus dem sjmisc-Paket
# - joins um Datensätze zusammenzuführen
# - pivot um Datensätze umzuformen

# Kapitel aus R4DS
# https://r4ds.had.co.nz/transform.html

# Prerequisites -----------------------------------------------------------

pacman::p_load(sjmisc,       # verschiedene Datentransformationsfunktionen
               nycflights13, # Datensatz Flüge der NY-Flughäfen 2013
               tidyverse,    # für dplyr
               janitor)    

flights <- readRDS("data/flights.rds")


# mutate() ----------------------------------------------------------------
# Mit mutate() kann man neue Variablen berechnen

# für die Übersichtlichkeit machen wir den Datensatz erstmal kleiner:
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)

# Berechnen neuer Variablen:
flights_sml %>% 
  mutate(gain = dep_delay - arr_delay,
         speed = distance / air_time * 60)


# Man kann die neuen Variablen direkt weiterverwenden
flights_sml <- flights_sml %>% 
  mutate(gain = dep_delay - arr_delay,
         hours = air_time / 60,
         gain_per_hour = gain / hours) %>% 
  arrange(desc(gain))


# Achtung, neue Variablen werden nicht "automatisch" gespeichert.
# Dazu brauchen wir eine neue Zuweisung:
flights_sml <- flights_sml %>% 
  mutate(gain = dep_delay - arr_delay,
         hours = air_time / 60,
         gain_per_hour = gain / hours)


# Exportieren um es wirklich auerhaft zu speichern (siehe Sitzung 3)

# Komplexe Berechnungen möglich, die wir meist nicht brauchen.
# Einige interessante Funktionen:
(x <- 1:10)
cumsum(x)
x / sum(x)

# Rang
flights_sml %>% 
  mutate(arr_delay_rank = min_rank(arr_delay)) %>% 
  arrange(arr_delay)

# Reihennummer der aktuellen Sortierung
flights_sml %>% 
  mutate(row_num_before = row_number(year),
         arr_delay_rank = min_rank(arr_delay)) %>% 
  arrange(arr_delay) %>% 
  mutate(row_num_after = row_number(year))


# case_when()
flights_sml %>% 
  mutate(delay_string = case_when(arr_delay < 0 ~ "überpünktlich",
                                  arr_delay == 0 ~ "pünktlich",
                                  arr_delay > 0 ~ "zu spät!",
                                  is.na(arr_delay) ~ "gar nicht angekommen :(")) %>% 
  tabyl(delay_string)


# Übung: mit dem starwars-Datensatz
# Berechnet den Bodymass-Index (BMI) der stwarwars-Figuren, 
# sortiert die Variablen so, dass der BMI direkt hinter der Variable mass
# kommt und sortiert absteigend nach dem BMI.
starwars %>%
  mutate(bmi = mass/ (height/100)^2,
         bmi_string = case_when(bmi <= 18.5 ~ "Untergewicht",
                                bmi <= 25 ~ "Normalgewicht",
                                TRUE ~ "zu schwer!")) %>% 
  select(name:mass, bmi, bmi_string, everything()) %>% 
  arrange(desc(bmi)) %>% 
  head(20)

# Fügt noch eine kategoriale Einteilung hinzu:
# untergewicht < 18.5
# übergewicht > 25
# adipös > 30


# summarise() & group_by() ------------------------------------------------

# summarise: Statistiken mit dem tidyverse ausgeben
flights  %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE),)


# Daten gruppieren
by_day <- flights %>% 
  group_by(month, day) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE))

# mehrere Operationen gleichzeitig um neue, aggregierte Datensätze zu erzeugen
flights %>% 
  group_by(dest) %>% 
  summarise(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL") %>% 
  arrange(desc(dist)) %>% 
  ggplot(mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)

?summarise() # "Useful functions"

# sum() mit logischen Operatoren verwednen
flights %>% 
  filter(!is.na(dep_time)) %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# Geht auch mit mean(), dann bekommt man den Anteil
flights %>% 
  filter(!is.na(dep_time)) %>% 
  group_by(year, month, day) %>% 
  summarise(prop_early = mean(dep_time < 500))

# Funktion ungroup(): Aus dem Datensatz wird ein normaler Datensatz, der 
# nicht mehr gruppiert ist.

# sjmisc ------------------------------------------------------------------

df <- sjlabelled::read_spss("data/gen_z_small.sav") %>% 
  as_tibble()

# recodieren mit sjmisc
frq(df$politisches_interesse)

df <- df %>% 
  rec(politisches_interesse, 
      rec = "1,2 = 1;
             3,4 = 2;
             99 = NA") 
names(df)

frq(df$politisches_interesse_r)

df <- df %>% 
  rec(politisches_interesse, 
      rec = "1,2 = gering;
             3,4 = hoch;
             99,NA = NA",
      suffix = "_andere_endung") 

frq(df$politisches_interesse_r2)


# weitere nützliche Funktionen aus dem Paket

# Z-Standardisieren
df <- df %>% 
  mutate(politisches_interesse_z = std(as.numeric(politisches_interesse))) %>% 
  select(starts_with("poli"))

# Summe oder Mittelwertindices über mehrere Spalten bilden
df <- df %>% 
  mutate_if(is.factor, ~ as.numeric(levels(.x))[.x]) %>% # geht leider nur mit numerischen Variablen.
  row_sums(infoquelle_tv_nachrichten:infoquelle_talkshows, 
           n = 0.5, 
           var = "infoquellen_sum") %>% 
  row_means(infoquelle_tv_nachrichten:infoquelle_talkshows, 
            n = 0.5, 
            var = "infoquellen_mx")

df %>% 
  select(`infoquellen_mx...40`)



# in tidycomm gibt es ebenfalls eine Funktion für Mittelwertindices, 
# damit kann man auch gleich Coronbachs´s Alpha und andere Konsistenzmaße ausgeben.


# join --------------------------------------------------------------------

# das ncflights13-Paket enthält noch weitere tibbles:
airports
planes
weather
airlines
flights

# save(flights, airports, airlines, planes, weather, file = "data/nyflights_data.rdata")
# load("data/nyflights_data.rdata")


# Die Datensätze sind über verschiedene Variablen (Keys) miteinander verknüpft.
# siehe:
# https://r4ds.had.co.nz/relational-data.html?q=join#inner-join

# Ein "Key" ist eine Variable (oder ein Set von Variablen) die einen Datensatz
# (FAll) eindeutig identifizieren.
# Anhand der Keys lassen sich die Datensätze verknüpfen
# "Primary Key": Identifikation des Datensatzes in der eigenen Tabelle
# "Foreign Key": Identifikation des Datensatzes in einer anderen Tabelle

# Primary Key für airlines?
airlines

# Foreign Key in flights
flights %>% 
  select(year:dep_time, carrier:tailnum, everything())

# Typischerweise "one-to-many"-Relationen


# Ein Key sollte unique sein! - Ist er aber leider nicht immer.
weather %>% 
  count(year, month, day, hour, origin, time_hour) %>% 
  filter(n > 1)

# Join von mehreren Datensätzen
# Beispiel: Airline-Name an den Flights-Datensatz anfügen

# typischerweise ein left_join(), 
# d.h. der Datensatz "links" gibt die Fallzahl vor. 
# Fälle, die nur im Datensatz rechts vorkommen, verschwinden.

flights %>% 
  # zur Übersichtlichkeit Variablen auswählen
  select(year:day, hour, origin, dest, tailnum, carrier) %>% 
  left_join(airlines, by = "carrier") 

# Ein kleiner Test mit einer Airline, die nicht in flights vorkommt:
airlines <- rbind(airlines, c("4Y", "Eurowings"))

flights %>% 
  # zur Übersichtlichkeit Variablen auswählen
  select(year:day, hour, origin, dest, tailnum, carrier) %>% 
  right_join(airlines, by = "carrier") %>%
  filter(carrier == "4Y")

# Außerdem gibt es noch inner_outer() und outer_join().

# Wenn Keys nicht unique sind, werden alle möglichen Kombinationen angelegt
# Beispiel:
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

# Wenn Keys in den Datentabellen unterschiedlich heißen:
flights %>% 
  left_join(airports, c("dest" = "faa"))


# Aufgabe:
# Matched die Zeitzonen für Abflug- und Ankunfts-Flughafen an den flights-
# Datensatz. Erstellt dann eine Variable, die aussagt, ob ein Flug in
# derselben Zeitzone gestartet und gelandet ist.


# Filtering joins wirken sich nur auf die Fälle aus, 
# sie fügen keine Variablen hinzu.

# semi_join(), alle Fälle die in x sind und auch in y
airlines %>% 
  semi_join(flights)

# anti_join(), alle Fälle die nicht in x sind aber in y
airlines %>% 
  anti_join(flights)


# pivot -------------------------------------------------------------------
# "tidy data"
table1

# Verschiedene Formen von nicht "tidy"
table2
table3

# Separate Tabellen 
table4a
table4b

# Nochmal zu table1: das ist ein "long format".
# Ein Fall ist hier ein Land in einem Jahr.
table1

# Möglicherweise ist aber auch ein "wide format" angemessener,
# wobei ein Fall ein Land ist und die cases und die population jeweils einmal
# für die beiden Jahre zu diesem Land gemessen wurden (also 5 Variablen).
# Dann könnte man auch sehr leicht die Veränderung der Bevölkerung berechnen.
table1 %>% 
  pivot_wider(names_from = year, values_from = c(cases, population))


# Das lässt sich natürlich auch wieder zurück konvertieren, ist aber aufwendig...
table1 %>% 
  # also, nochmal breiter machen:
  pivot_wider(names_from = year, values_from = c(cases, population)) %>% 
  # und jetzt wieder zurück:
  pivot_longer(cols = !country, names_to = "year", values_to = "value") %>% 
  mutate(type = str_extract(year, "[a-z]*"),
         year = str_extract(year, "[0-9]{4}")) %>% 
  pivot_wider(names_from = type, values_from = value)

# Vermutlich gibt es auch einen Befehl, der das alles in einer Zeile macht.
# Nachvollziehbarkeit ist aber auch ein Wert, denn Joins & Pivoting sind 
# tendenziell fehleranfällig.‚
# Immer gegenchecken, ob das Ergebnis tatsächlich plausibel ist und 
# den Erwartungen entspricht.
