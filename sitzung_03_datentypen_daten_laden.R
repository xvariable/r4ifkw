# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022/23
# Julia Niemann-Lenz
# Skript zur dritten Sitzung, 2022-11-16

# #########################################################################

# Inhalte: 
# - Atomic Data Types
# - Organisation von Daten in Vektoren, Matrizen, Listen & Dataframes
# - Datensätze im Tidyverse: tibble
# - Datensätze in R laden

# #########################################################################

# Nützliche Links zur Sitzung:

# Zu Vektoren, Listen & Subsetting:
# https://r4ds.had.co.nz/vectors.html

# Zu "factors": 
# https://r4ds.had.co.nz/factors.html

# Zu fehlenden Werten (user generated NAs)
# https://cran.r-project.org/web/packages/labelled/vignettes/missing_values.html

# Zur Arbeit mit gelabelten SPSS-Daten in R
# https://www.pipinghotdata.com/posts/2020-12-23-leveraging-labelled-data-in-r/

# Über Encodings:
# https://kunststube.net/encoding/

# #########################################################################

pacman::p_load(tidyverse, # für die Pipe, ggplot2 und forcats
               sjlabelled,# für SPSS-Daten und Label
               haven,     # für Daten in Fremdformaten wie SPSS
               readxl,    # für Excel-Daten
               janitor)   # für Häufigkeitsauszählungen


# Atomic Data Types in R -----------------------------------------------------

# Atomic Data Types
# Zahlen: integer & double (numeric)
typeof(2)
typeof(2L)

is.numeric(2)
is.double(2.3)
is.integer(2L)


# Character
typeof('2')


# Logical
typeof(TRUE)

# Logical ist das Ergebnis von logischen Vergleichen, z.B.
1 == 2
5 > 4.3

is.na(NA)
1:10 %% 3 == 0

x <- 3 == 6
isTRUE(x)


# Außerdem gibt es noch die atomic vector types complex & raw.


# Sonderwerte
Inf
-Inf
NA
NaN


# Organisation der atomic data types in Vektoren, Matrizen und Listen ---------

# Vektoren können immer nur den selben Datentyp enthalten
vec_1 <- c(1, 2, 5, 8, 8, 3)
typeof(vec_1)

# im Zweifel, werden die Elemente auf den gleichen Datentyp konvertiert
vec_2 <- c(1, 2, 5, 8, 8, "drei")
typeof(vec_2)

vec_3 <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
typeof(vec_3)


# Auf ein bestimmtes Element eines Vektors zugreifen:
vec_3[5] 

# oder mit doppelten eckigen Klammern (macht beim Vektor keinen Unterschied)
vec_3[[5]]


# Für viele statistische Verfahren sind Matrizen notwendig.
# Man kann Vektoren zeilenweise mit rbind() oder Spaltenweise mit cbind()
# zu Matrizen zusammenfügen.
# Matrizen können ebenfalls nur einen Datentyp enthalten
matrix_1 <- rbind(vec_1, vec_1, vec_1)
typeof(matrix_1)

matrix_1 <- rbind(vec_1, vec_2, vec_3)
typeof(matrix_1)
matrix_1

# Auf ein bestimmtes Element einer Matrix zugreifen:
matrix_1[1, 3] # [Reihe, Spalte]

# oder
matrix_1[[1, 3]]

# man kann hier auch Bereich angeben:
matrix_1[1:2, 3:5]


# Im Gegensatz zu Vektoren und Matrizen können Listen unterschiedliche 
# Datentypen enthalten
list_1 <- list(1, "zwei", TRUE)
list_1
typeof(list_1)


# Die Elemente der Listen kann man benennen
list_2 <- list(a = 1, b = "zwei", c = TRUE)
list_2
typeof(list_2)

# Die Elemente der Liste kann man über den Namen ansprechen
list_2$b

# man kann auch über den Index auf die Listenelemente zugreifen
# Einfache eckige Klammern [ extrahieren eine Sub-Liste 
# (= gleiche hierarchische Schachtelung, aber kürzer)
list_2[2]

# mehrere Elemente gleichzeitig:
list_2[2:3]

# Doppelte eckige Klammern extrahieren immer ein einzelnes Element.
# (dabei wird eine Hierarchieebene entfernt)
list_2[[2]]


# Eine Liste kann auch Vektoren (oder auch Listen) als Elemente enthalten:
list_3 <- list(a = vec_1, b = vec_2, c = vec_3)

list_3[[3]]



# Ein Dataframe ist im Prinzip eine Liste gleich langer Vektoren.
# In R kommt noch ein bisschen Meta-Info und Datenorganisation dazu, 
# aber man kann die Liste der Vektoren in einen Dataframe konvertieren:
my_df <- as.data.frame(list_3)
my_df


# Datentypen mit zusätzlichen Attributen/Features (augmented vectors):
# factors = nominale und ordinale Variablen mit Labels
# dates & date-times = Zeitformate
# tibbles = spezieller Dataframe des Tidyverse


# factors mit forcats ---------------------------------------------------
# forcats ist ein Paket aus dem Tidyverse zur Arbeit mit Faktoren.
# siehe R4DS

# Beispiel: Variable, in der Monate gespeichert sind:
x1 <- c("Dec", "Apr", "Jan", "Mar")
x1

# Achtung, Tippfehler!
x2 <- c("Dec", "Apr", "Jam", "Mar")

# Achtung, Sortierung nicht sinnvoll!
sort(x1)

# Lösung: Speichern von nominalen und ordinalen Variablen als factor.
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# nominal
y1 <- factor(x1, levels = month_levels)
y1

# ordinal
y1 <- factor(x1, levels = month_levels, ordered = TRUE)
y1

# Was passiert mit dem Rechtschreibfehler?
x2 <- c("Dec", "Apr", "Jam", "Mar")
y2 <- factor(x2, levels = month_levels)
y2

# Welche Level hatte mein factor nochmal?
levels(y2)
levels(gss_cat$race)


# Recodieren von factors 
# Die Syntax ist immer "neuer_name" = "alter_name"
# (Recodieren machen wir nächste Woche noch intensiver)
y3 <- fct_recode(y1,
                "Januar" = "Jan", 
                "Februar" = "Feb", 
                "März" = "Mar", 
                "April" = "Apr", 
                "Mai" = "May", 
                "Juni" = "Jun", 
                "Juli" = "Jul", 
                "August" = "Aug", 
                "September" = "Sep", 
                "Oktober" = "Oct", 
                "November" = "Nov", 
                "Dezember" = "Dec") 

y3

# Mehrere Ausprägungen zusammenfassen, 2 Möglichkeiten:
# a) Beim recodieren einfach gleich benennen
# b) fct_collapse()
y4 <- fct_collapse(y1,
                 Quartal_1 = c("Jan", "Feb", "Mar"), 
                 #Quartal_2 = c("Apr", "May", "Jun"),
                 #Quartal_3 = c("Jul", "Aug", "Sep"), 
                 #Quartal_4 = c("Oct", "Nov", "Dec")
                 ) 
y4

# möglicherweise auch interessant: fct_lump()
# Fasst Ausprägungen, die nur in geringer Anzahl auftreten zusammen,
# so dass sie zusammen weiterhin die kleinste Ausprägung ausmachen.
# Nicht immer sinnvoll...

# Original-Häufigkeitsauszählung
gss_cat$relig %>% 
  tabyl() %>% 
  arrange(desc(n)) # Sortiert die Tabelle absteigen

# Zusammengefasste Variable:
gss_cat$relig %>% 
  fct_lump() %>% 
  tabyl() %>% 
  arrange(desc(n)) 


# Daten laden mit R ##########################################################
# CSV Daten in R laden -------------------------------------------------------

# mit base-R
df_base <- read.csv2("data/gen_z_small.csv")

# Ergebnis: im Objekt df_base ist ein "dataframe" gespeichert
df_base

# Warum "2"? R ist international ausgerichtet, im deutschen Excel verwenden wir 
# aber das Komma als Dezimaltrenner (und andere Eigenheiten).

summary(df_base)
str(df_base)
View(df_base)
is.data.frame(df_base)


#setwd("/Users/julia/SD_home/LEHRE/LMU/R_schulung/")
# getwd()

# Daten laden mit dem Tidyverse (Paket readr)
df_tidy <- read_csv2("data/gen_z_small.csv")

# Unterschied, der Datensatz ist ein dataframe, aber auch ein "tibble".
is_tibble(df_tidy)
is_tibble(df_base)

df_tidy
View(starwars)

# Spezielle Eigenschaften des "tibble": Es werden einige unerwünschte 
# Verhaltensweisen von dataframes ausgeschaltet und es gibt ein paar Features
# - Stings werden nicht automatisch in Faktoren konvertiert
# - Zellen können selbst Listen enthalten (z.B. einen Datensatz im Datensatz?)
# - Auch Variablennamen möglich, die Sonderzeichen enthalten
# - "Recycling" von Vektoren nur bei Vektoren der Länge 1
# - Niemals Benennung der Zeilen (rownames)
# - Laden von Daten mit dem Tidyverse: geht schneller und liefert gleiche  
#   Ergebnisse auf unterschiedlichen Systemen


# Demo: Recycling von Vektoren
# Was denkt ihr passiert in Zeile 3?
vec1 <- c(1, 1, 1, 1, 1, 1)
vec2 <- c(2, 3)
vec1 * vec2


# Nützliche Features der read_csv()-Funktion: 
# Ein Blick in die Hilfe zur Funktion
# - skip: Zeilen am Anfang auslassen
# - comment = "#": Durch # gekennzeichnete Kommentarzeilen auslassen
# - col_names = FALSE: Wenn keine Variablennamen in der Datei sind
# - col_names = c("name1", "name2" ...): Umbenennung der Variablennamen 
#   beim laden
# - na: Benutzerdefinierte NAs festlegen


# Excel-Dateien -----------------------------------------------------------
# Für Excel-Daten braucht man nochmal ein spezielles Paket z.B. readxl (ist tidy)
df_ex <- read_excel("data/gen_z_small.xlsx", sheet = "Tabelle1")

df_ex

# Daten labeln mit dem Paket sjlabelled

# Skala als Vektor anlegen (ist ja wiederverwendbar...)
scale_pol_int<- c("Überhaupt nicht" = 1,
                  "Weniger stark" = 2,
                  "Eher stark" = 3,
                  "Sehr stark" = 4,
                  "Weiß nicht" = 99)


df_ex$politisches_interesse_2 <- # Labels zuweisen
                                 set_labels(df_ex$politisches_interesse, 
                                            labels = scale_pol_int) %>% 
                                 # Werte durch Label ersetzen und 
                                 # in factor umwandeln
                                 as_label()

df_ex %>% 
  count(politisches_interesse_2)

# Unterschiede z.B. in Plots:
ggplot(df_ex) +
  geom_bar(aes(politisches_interesse))

ggplot(df_ex) +
  geom_bar(aes(politisches_interesse_2))


# SPSS-Daten --------------------------------------------------------------
# Es gibt verschiedene Pakete mit denen man "Fremdformate" in R laden kann.
# Beispiele: haven aus dem Tidyverse, sjlabelled und foreign.
# Achtung: Die Daten haben je nach Paket leicht unterschiedliche Eigenschaften

# mit dem Tidyverse
df_sav <- haven::read_spss("data/gen_z_small.sav",
                   user_na = FALSE)

# Achtung, User-NAs verhalten sich nicht immer wie normale NAs *seufz*.

# Der mit haven aus SPSS importierte Datensatz enthält 
# "gelabelte Variablen" != "factors"
is.factor(df_sav$politisches_interesse)

df_sav %>% 
  count(politisches_interesse)


# Alternativ SPSS-Daten laden mit sjlabelled
df_sav2 <- sjlabelled::read_spss("data/gen_z_small.sav")

# Dieser aus SPSS gelabelte Datensatz enthält Faktoren
is.factor(df_sav2$politisches_interesse)

df_sav2 %>% 
  count(politisches_interesse)

# gelabelt sind sie aber auch (andere Art von labelled obbject)
get_labels(df_sav2$politisches_interesse)


# Daten speichern ---------------------------------------------------------
readr::write_csv2(df_sav, "data/new_name.csv")

haven::write_sav(df_sav, "data/new_name.sav")

sjlabelled::write_spss(df_sav, "data/new_name.sav")

# Keine entsprechende Funktion in readxl! Es gibt aber Pakete, mit denen man
# (zumindest alte?) Excel-Formate schreiben kann: xlsx


# R´s eigene Dateiformate -------------------------------------------------

# A) Rds ---

# .rds: Speichert ein einzelnes R-Objekt (muss kein Datensatz sein)
saveRDS(df_sav, "data/new_name.rds")

# Beim Laden kann das Objekt zu einem neuen Namen zugewiesen werden:
df <- readRDS("data/new_name.rds")


# B) RData / RDa ---

# Speichern aller Objekte in der Environment (nicht empfehlenswert)
save.image(file = "data/alle_objekte.rdata") 

# Speichern bestimmter Objekte (auch mehrere gleichzeitig)
save(df_tidy, df_base, file = "data/zwei_datensaetze.rdata")

# RData laden (kein Zuweisungsoperator)
load("data/zwei_datensaetze.rdata")


# Andere Dateiformate -----------------------------------------------------
# Ähnliche Funktionen für Dateiformate mit fester Breite
read_fwf()

# und auch für tsv-Files
read_tsv()

# oder für Stata / SAS
haven::read_stata()
haven::read_sas()

# Nützliche Pakete für Spezialformate:
# jsonlite für JSON
# xml2 für XML
# pdftools für PDF
# av für Audio- und Video-Formate
# ...

# Paket für den Zugriff auf (SQL-basierte) Datenbanken: DBI
