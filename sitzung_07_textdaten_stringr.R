# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022
# Julia Niemann-Lenz
# Skript zur sechsten Sitzung, 2022-12-21

# #########################################################################

# Inhalte: 
# - Control character (Stuereungszeichen, Sonderzeichen)
# - Vorstellung stringr-Paket


# Nützliche Links ---------------------------------------------------------

# Kapitel aus R4DS
# https://r4ds.had.co.nz/strings.html

# Regular Expressions
# https://towardsdatascience.com/regular-expressions-clearly-explained-with-examples-822d76b037b4
# https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf

# Achtung, es gibt RegEx in unterschiedlichen "Flavours".
# R unterstützt Extended Regular Expressions (POSIX 1003.2) = Standard
# und Perl Regular Expressions.
# Im Internet gibt Testumgebungen/Generatoren für RegEx, man muss dabei aber ggf.
# auf den Flavour achten. 


# Erster Einstieg in Textanalyse mit dem Paket tidytext
# Buchtutorial:
# https://www.tidytextmining.com/

# #########################################################################

pacman::p_load(tidyverse)


# String basics -----------------------------------------------------------

# Strings immer durch zwei Anführungszeichen einschließen
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
string3 <- "Otherwise, I need to \"escape\" the quotation marks"
string4 <- "Here are some \t control and special \ncharacters \u00b5"

# list of control characters:
?"'"

# Den String auf unterschiedliche Arten ausgeben
print(string4)
cat(string4)


# Länge von Strings
s <- c("a", "R for data science", NA)
str_length(s)


# Strings kombinieren
str_c("x", "y", "z")

str_c("x", "y", "z", sep = ", ")

# Auf Objekte referenzieren:
name <- c("Paula", "Max", "Anna")

str_c("Hallo ", name, ", frohe Weihnachten!")


# Was, wenn ich bereits einen Vektor habe, den ich zu einem String kombinieren will?
s <- c("x", "y", "z")
str_c(s, collapse = ", ")


# Subsetting
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

# Subsetting rückwärts
str_sub(x, -3, -1)

# Groß- und Kleinschreibung
s <- "Das ist ein Text in ganz normalem Deutsch."

str_to_lower(s)
str_to_upper(s)


# sortieren
x <- c("apple", "eggplant", "banana", "Äpfel")

str_sort(x, locale = "de") # haw for Hawaiian



# Matching patterns with regular expressions ------------------------------
x <- c("apple", "banana", "pear")

# Einfachste Form: Wenn man nur einen Buchstaben oder eine Kombi sucht
str_view(x, "an")

# Der . ist ein Platzhalter für ein beliebiges Zeichen
str_detect(x, ".a.")

# Was, wenn wir nach einem Punkt suchen wollen?
# 2 Backslashes um eine RegEx zu kreieren
dot <- "\\."

# In der Ausgabe ist dann nur ein Backslash enthalten:
cat(dot)

str_view(c("abc", "a.c", "bef"), "a\\.c")

# Frage: Wie sieht die RegEx für einen Backslash aus?
bs <- "\\\\"

cat(bs)


# Anfang und Ende
x <- c("apple pie", "apple", "apple cake")

str_view(x, "apple")

str_view(x, "^apple$")

# ^ Anfang
# $ Ende

# Klassen von Zeichen
# \\d alle Ziffern
# \\s alle Leerzeichen (Leerzeichen, Tab & Newline)
# [abc] a, b oder c
# [^abc] nicht a, b oder c

# oder
str_view(c("grey", "gray"), "gr[e|a]y")

# Wiederholung
# ? 0 oder 1 Mal
# + 1 oder öfter
# * 0 oder öfter

# {n} genau n Mal
# {n,} n Mal oder öfter
# {,n} mindestens n Mal
# {n,m} zwischen n und m Mal


# Übungen

# Given the corpus of common words in stringr::words, create regular expressions 
#that find all words that:

words %>% 
  str_view("ing|ise$", match = TRUE)

words %>% 
  str_detect("ing|ise$") %>% 
  sum()


# - End with “x”
# - Are exactly three letters long. (Don’t cheat by using str_length()!)
# - Have seven letters or more.
# - Start with a vowel.
# - That only contain consonants. (Hint: thinking about matching “not”-vowels.)
# - End with ing or ise.

# Since this list is long, you might want to use the match argument to str_view() to show only the matching or non-matching words.



# RegEx anwenden ----------------------------------------------------------
x <- c("apple", "banana", "pear")

str_detect(x, "e")

# Was passiert hier?
words %>% 
  str_detect("^t") %>% 
  sum()

# Und hier?
words %>% 
  str_detect("[aeiou]$") %>% 
  mean()

# Subsetting
str_subset(words, "x$")

# Normalerweise sind die Strings ja in einem Datensatz:
df <- tibble(
  word = words, 
  i = seq_along(word)
)

df

df %>% 
  filter(str_detect(word, "x$"))

# oder eine neue Variable bilden?

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )


# Matches überlappen niemals!
str_view_all("abababa", "aba")


# Extract Matches
sentences

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
head(matches)

# str_extract() sucht immer nur nach dem ersten Match. Um alle zu bekommen 
# braucht man str_extract_all()


# Splitting
sentences %>%
  head(5) %>% 
  str_split(" ")


# Weitere Funktionen
str_locate() # In Kombination mit str_sub()
str_trim()
str_trunc()

# Umfangreicheres Paket zur Analyse von Strings: stringi
