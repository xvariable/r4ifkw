# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022
# Julia Niemann-Lenz
# Skript zur ersten Sitzung, 2022-11-02

# #########################################################################

# Inhalte: 
# - Shortcuts
# - Dateien anlegen
# - Pakete laden und installieren
# - Objekte anlegen
# - Datensätze explorieren
# - Funktionen anwenden

# #########################################################################

# Shortcuts ---------------------------------------------------------------

# Ausführen von Code: Strg + Enter
# Zuweisungsoperator: Alt + -
# Sections: Strg + Shift + r
# Hilfe zu einer Funktion aufrufen: F1 (Cursor auf der Funktion) oder
#                                   ?funktion() ausführen


# Dateien anlegen ---------------------------------------------------------

# Das Basis-Datenformat von R ist das R-Skript mit der Dateiendung .R

# Für Forschungsprojekte immer ein R-Projekt anlegen. 
# In dem Ordner des R-Projekts werden alle zum Projekt gehörigen Dateien 
# gespeichert (R-Skripte, aber auch Datensätze)

# Wird das R über die Projektdatei geöffnet, befindet sich R gleich im 
# richtigen Arbeitsverzeichnis.

# Weitere interessante Dateiformate von R sind RMarkdown und Quarto 
# (z.B. Berichte oder Webseiten mit R anlegen) sowie RShiny (interaktive
# Datenausgaben).


# Pakete ------------------------------------------------------------------

# Pakete erweitern den Funktionsumfang von R. Es gibt Pakete für alle 
# möglichen Statistik-Anwendungen. 

# Pakete muss man 1x installieren und bei jeder neuen Sitzung in R laden,
# wenn man sie in dieser Sitzung braucht.

# Pakete installieren 
# Entweder über den Tab "Packages" oder über die folgende Funktion:
install.packages("psych") # Nachteil dieser Variante:
                          # Installation wird immer wieder ausgeführt,
                          # wenn man das hard-coded und das Skript einfach 
                          # neu durchlaufen lässt.

# Pakete laden
library(psych)

# Die Funktion p_load() aus dem Paket packman installiert Pakete nur, sofern 
# sie noch nicht installiert wurden und läd sie anschließend.
pacman::p_load(beepr,    # für Soundsignale
               psych,    # für deskriptive Statistiken
               tidycomm) # KoWi-Statistiken

# durch den Doppel-Doppelpunkt "::" in dem Skript oben, muss man das Paket 
# packman nicht erst laden, sondern kann direkt auf siene Funktion p_load()
# zugreifen

# Achtung! Manche Pakete enthalten Funktionen, die die gleichen Namen 
# haben, z.B. describe(). Man kann mit paketname::funktionsname() explizit 
# machen, aus welchem Paket die Funktion stammen soll, die man benutzen 
# möchte:
psych::describe(WoJ$ethics_1)
tidycomm::describe(WoJ$ethics_1)

# Macht man nicht explizit, aus welchem Paket die Funktion stammen soll,
# verwendet R das zuletzt geladene Paket. (Achtung, Fehlerquelle!)


# Kleine Aufgabe: Installiert beepr und
# findet heraus, welche Funktion das Paket hat
# Testet die Funktion mit dem Argument "8"
# Was hört ihr?

pacman::p_load(beepr)
beep(8)


# Objekte anlegen -----------------------------------------------------------------
# R als Taschenrechner
1 + 3 * 4

# Objekten Namen geben und einen Wert zuweisen mit dem 
# Zuweisungsoperator "<-"
my_object <- 1 + 3 * 4

# Das Objekt kann wiederverwendet werden
my_object + 7

# Selbst benannte Objekte erscheinen in der "Environment".

# Die Objekte der geladenen Pakete sind ebenfalls in der Environment,
# sie sind aber unsichtbar, z.B. die Funktionen der Pakete oder auch 
# Datensätze, die in den Paketen enthalten sind.
WoJ # Datensatz aus dem tidycomm-Paket


# Datensätze explorieren ----------------------------------------------

# Datensätze kann man sich auch in Tabellenform anzeigen lassen 
# (aber nicht editieren)
View(WoJ)

# Erste Funktionen zur Exploration von Datensätzen:

# Zusammenfassung der Variablen
summary(WoJ)

# Struktur des Datensatzes
str(WoJ)

# Namen der Variablen (colums)
names(WoJ)

# Mit "$" kann man auf die "Unter-Elemente" eines Objekts zugreifen. 
# Bei Datensätzen sind diese Unter-Elemente die Variablen 
# (in R heißen sie colums)

WoJ$ethics_1


# Funktionen anwenden ---------------------------------------------------------

# Funktionen bestehen aus dem Funktionsnamen, Klammern und Argumenten.
# In der Hilfe zur Funktion (F1 drücken, während der Cursor auf der Funktion
# ist), werden die Argumente beschrieben.

# Die Funktion mean() hat drei Argumente:
# 1. Variable, für die der Mean berechnet werden soll (zwingend erforderlich)
# 2. Mit "trim" kann man bestimmen ob man ein getrimmtes Mittel berechnen 
#    möchte und wenn ja, wieviel weg-getrimmt werden soll. (trim ist optional)
# 3. Mit "na.rm" kann man festlegen, das fehlende Werte (NAs) vor der Berechnung
#    des Mittelwerts aussortiert werden sollen. 

# Anlegen einer Zahlenreihe als Variable
my_var <- c(1, 2, 3, NA, 7, 9, 9999)

# Berechnung des Mittelwerts
mean(my_var, trim = 0.1, na.rm = TRUE)
mean(my_var, TRUE, 0.1)


# Mean immer mit SD berichten!
sd(my_var, na.rm = TRUE)

# Man kann das Ergebnis von Berechnungen in Objekten speichern...
my_mean <- mean(my_var, trim = 0.1, na.rm = TRUE)

# ...und dann damit weiterarbeiten
round(my_mean, 2)

# Man kann die Berechnungen auch direkt ineinander verschachteln:
round(mean(WoJ$ethics_1, trim = 0.1), 2) 

# Das wird allerdings schnell unübersichtlich!

# Ein anderer Weg ist die Pipe "%>%" mit der man das Ergebnis einer 
# Funktion an die nächste übergeben kann:
c(1, 2, 3, NA, 7, 9, 9999) %>% 
  mean(trim = 0.1, na.rm = TRUE) %>% 
  round(2)


# Die Funktion correlate() aus dem tidycomm-Paket funktioniert ein 
# bisschen anders als mean. Hier wird zuerst das Datenobjekt als 
# Argument übergeben und alle Variablennamen beziehen sich dann auf 
# diesen Datensatz. - Das ist der Tidyverse-Style. 
# Laut Hilfe der Funktion können beliebig viele Variablen angegeben
# werden ("..." soll das ausdrücken).
# Durch ":" kann man R sagen: nimm die Variablen von var_anfang bis 
# var_ende
correlate(WoJ, trust_parliament:trust_politicians)

# Das geht auch in Pipe-Schreibweise:
WoJ %>% correlate(trust_parliament:trust_politicians)
