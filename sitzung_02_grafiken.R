# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022
# Julia Niemann-Lenz
# Skript zur zweiten Sitzung, 2022-11-09

# #########################################################################

# Inhalte: 
# - Wiederholung deskriptive Statistiken
# - Häufigkeitsauszählungen
# - Grafiken mit base-R
# - Grafiken mit ggplot2 aus dem Tidyverse
#   (unterschiedliche Grafiktypen, Koordinatensysteme, Farben, Themes...)

# #########################################################################

# Nützliche Links zu Thema Grafiken

# Kapitel in Julia´s R-Kompendium
# https://xvariable.github.io/r_book/grafiken.html

# Entsprechendes Kapitel in R4DS:
# https://r4ds.had.co.nz/data-visualisation.html

# ggplot2 cheatsheet
# https://posit.co/wp-content/uploads/2022/10/data-visualization-1.pdf

# R-Graph-Gallery mit Grafiken und Code-Beispielen
# https://r-graph-gallery.com/



# #########################################################################

# prerequisits ------------------------------------------------------------

pacman::p_load(palmerpenguins, # für den Datensatz
               psych,          # für die deskriptiven Statistiken
               janitor,        # für die Häufigkeitsauszählungen
               tidyverse,      # für ggplot & die Pipe
               viridis)        # für schönere Farben

# Datensätze
View(penguins)
?penguins

# Wiederholung Mittelwerte ------------------------------------------------
# Die mean()-Funktion
mean(penguins$body_mass_g, trim = 0.1, na.rm = TRUE)

# funktioniert auch ohne Namen der Argumente
mean(penguins$body_mass_g, 0.1, TRUE)

# aber nur, wenn diese in der richtigen Reihenfolge kommen!
mean(penguins$body_mass_g, TRUE, 0.1)


# Wiederholung psych::describe() ------------------------------------------
psych::describe(penguins)

# Nicht-numerische Variablen NICHT konvertieren, sondern weglassen 
describe(penguins, omit = TRUE)

# Die Ausgabe weiter anpassen
describe(penguins, 
         skew = FALSE, 
         range = FALSE, 
         quant = c(.25, .50, .75), 
         omit = TRUE)

# Ausgabe nach Gruppen aufteilen
describeBy(penguins, group = "species")
describeBy(penguins, group = penguins$species)


# Häufigkeitsauszählungen mit base-R --------------------------------------

# absolute Häufigkeiten
table(penguins$island)


# Häufigkeitsauszählungen mit janitor -------------------------------------
janitor::tabyl(penguins, island)


# Mit der Pipe auch so:
penguins %>% 
  tabyl(island)


# Variable mit fehlenden Werten
penguins %>% 
  tabyl(sex)


# Runden der Ausgabe
# Mit der Pipe auch so:
penguins %>% 
  tabyl(sex) %>% 
  adorn_rounding(digits = 2)


# Erste Grafiken mit base-R -----------------------------------------------

# Scatterplot
plot(starwars$mass, starwars$height)

# Boxplot
boxplot(starwars$height)

# vergleichende Boxplots 
boxplot(height ~ sex, data = starwars, frame = FALSE)

# Säulen
barplot(mpg$cty, col = "steelblue")

# Histogram
hist(mpg$cty, breaks = 30)


# Erste Grafik mit dem Tidyverse (ggplot2) -------------------------------------

# Funktion, um einen (leeren) Plot anzulegen
ggplot()

# Geom hinzufügen
ggplot() +
  geom_histogram()

# Aethetik Mapping 
# (= Wie sollen die Variablen in der Grafik angewendet werden?)
ggplot(data = penguins, mapping = aes(x = island)) +
  geom_bar()

# In Pipe-Schreibweise:
penguins %>% 
  ggplot(mapping = aes(x = island)) +
  geom_bar()

# Für Minimalisten
penguins %>% 
  ggplot(aes(island)) +
  geom_bar()

# Das Mapping kann auch im Geom stattfinden
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island))

# Übung: Erstellt mit ggplot ein Histogramm für die Variable flipper_length_mm
# Probiert dabei auch das Argument "bindwith" aus und versucht einen optimalen 
# Wert zu finden.
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm)) +
  geom_histogram(binwidth = 5)


# Scatterplot -------------------------------------------------------------
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point()


# eine Regressionsgrade hinzufügen
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # se: mit oder ohgne CI



# Darstellung der Grafik verändern  -------------------------------------------
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(size = 5, color = "deeppink", alpha = .2, shape = 15) +
  geom_smooth(method = "lm", se = FALSE, color = "orange")
 


# Eine dritte Variable mit aesthetic mapping darstellen ----------------------
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm,
                       y = bill_length_mm, 
                       color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Was passiert, wenn ihr eine metrische Variable (z.B. body_mass_g) verwendet?
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm,
                       y = bill_length_mm, 
                       color = body_mass_g)) +
  geom_point()


# Metrische Variablen mit Grenzwert aufteilen
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm,
                       y = bill_length_mm, 
                       color = body_mass_g < 4000)) +
  geom_point()


# Was passiert, wenn ihr hier auch die Regressionsgerade einbaut?
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Wie macht ihr das, wenn ihr nur eine Regressionsgerade haben möchtet?
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm", se = FALSE) 

# Erstellt einen Boxplot der Variable body_mass_g, aufgeteilt nach species
penguins %>% 
  ggplot(mapping = aes(#x = species,
                       y = body_mass_g,
                       color = species)) +
  geom_boxplot()


# Alternative: Violin-Plot
penguins %>% 
  ggplot(mapping = aes(x = species,
                       y = body_mass_g,
                       color = species)) +
  geom_violin()


# Scatterplot bei ordinalen & nominalen Variablen nicht optimal
penguins %>% 
  ggplot(mapping = aes(x = species,
                       y = body_mass_g, 
                       color = species)) +
  geom_point(size = 5)

# Besser Jitter-Plot
penguins %>% 
  ggplot(mapping = aes(x = species,
                       y = body_mass_g, 
                       color = species)) +
  geom_jitter(size = 5, alpha = .3)



# Aufteilen in mehrere Grafiken (facets) ----------------------------------
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point() +
  facet_wrap(~ species, nrow = 2)


penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point() +
  facet_wrap(sex ~ species)


# Statistical transformation ----------------------------------------------
# Zurück zum Bar-Chart
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island))

# Die Variable liegt als Vektor vor:
penguins$island

# Während der Erstellung der Grafik macht ggplot automatisch eine 
# "statistische Transformation": Eine Häufigkeitsauszählung
table(penguins$island)

# Man kann die Transformation auf explizit machen 
# (die beiden Funktionen geom_bar() und stat_count() sind austauschbar)
penguins %>% 
  ggplot() +
  stat_count(mapping = aes(x = island))


# Bestimmen, das ggplot eine andere Transformation durchführen soll,
# z.B. Relative Häufigkeiten
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, y = stat(prop), group = 1))


# Manchmal liegen die Daten schon als Häufigkeitsauszählung vor.
demo <- penguins %>% 
  tabyl(island) 

demo

demo %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, y = percent), stat = "identity")


# Bunte Barcharts ---------------------------------------------------------
# Versucht mal die Säulen des Barcharts bunt einzufärben, je nach Insel.
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, color = island))

# Nicht die Linien, sondern die Flächen einfärben mit "fill"
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, fill = island))


# Gestapelte Barcharts ----------------------------------------------------
# Was passiert, wenn man eine andere Variable zu fill zuweist?
# Probiert es mit species oder sex.
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, fill = sex))
# = gestapeltes Balkendiagramm


# Balken nebeneinander
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, fill = sex), position = "dodge")



# Koordinaten-Systeme -----------------------------------------------------
# Die Achsen des Koordinatensystems tauschen
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, fill = sex), position = "dodge") +
  coord_flip()

# Rundes Koordinatensystem
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = island, fill = sex), position = "dodge")  +
  coord_polar()

# normales Kreisdiagramm
penguins %>% 
  ggplot() +
  geom_bar(mapping = aes(x = "", y = island, fill = island), stat="identity")  +
  coord_polar("y") 

# ..soweit, so hässlich.


# Farben anpassen ---------------------------------------------------------

# Eigene Farben anlegen
lmu_palette <- c("#00883A", 
                "#0F1987", 
                "#009FE3", 
                "#8C4091", 
                "#D71919", 
                "#F18700")

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  ggplot(mapping = aes(x = flipper_length_mm, 
                       y = bill_length_mm, 
                       color = species),
         size = 5) +
  geom_point() +
  scale_colour_manual(values = lmu_palette)

# für Balken: scale_fill_manual(values = lmu_palette)


# Vordefinierte Paletten nutzen, z.B. aus dem viridis-Paket
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, 
                       y = bill_length_mm, 
                       color = species),
         size = 5) +
  geom_point() +
  scale_color_viridis(discrete = TRUE)


# geht auch mit metrischen Variablen
penguins %>% 
  ggplot(mapping = aes(x = flipper_length_mm, 
                       y = bill_length_mm, 
                       color = body_mass_g),
         size = 5) +
  geom_point() +
  scale_color_viridis()



# Themes ------------------------------------------------------------------
penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal()  

# Meistens muss man trotzdem noch ganz viel umbauen :(
my_plot <- penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  ggplot(mapping = aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE, name="Spezies") +
  # Titel und Achsenbeschriftungen hinzufügen
  ggtitle("Schnabel- & Flossenlänge nach Spezies") +
  xlab("Länge der Flossen (mm)") + 
  ylab("Länge des Schnabels (mm)") +
  scale_x_continuous(expand = c(0, 0), # Damit die Achsen sich genau bei den Limits treffen
                     limits = c(150, 240), 
                     breaks = seq(150, 240, 10)) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(25, 65), 
                     breaks = seq(25, 65, 5)) +
  theme(text = element_text(size = 12, family="Comic Sans MS"),
        panel.background = element_blank(), 
        panel.grid.major =  element_line(colour = "#EEEEEE"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(hjust = 1),
        legend.key = element_rect(colour = NA, fill = NA),
        #legend.position = "bottom",
        legend.position = c(0.2,.7),
        plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0, unit = "cm"))
  

my_plot

# Speichern nicht vergessen!
# Dabei kann auch die Größe und die Auflösung des Plots angegeben werden. 
# Beides ist variabel, solange der Plot in RStudio ist.
ggsave(plot = my_plot, 
       width = 6, height = 4, unit = "cm", 
       dpi = 300, 
       filename = "my_plot.png")
