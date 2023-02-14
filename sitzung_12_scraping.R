# #########################################################################

# R für SPSS-Umsteiger
# Weiterbildung für das IfKW der LMU im WS 2022/23
# Julia Niemann-Lenz
# Skript zur zwölften Sitzung, 2023-02-08

# #########################################################################

# Inhalt: 
# Scraping der Abgeordneten des bayrischen Landtags

# #########################################################################

# Munzert, S., Rubba, C., Meißner, P. & Nyhuis, D. (2014). Automated Data 
# Collection with R: A Practical Guide to Web Scraping and Text Mining. Wiley.

# Atteveldt van, W., Trilling, D. & Arcila, C. (2022) Computational Analysis 
# of Communication. Wiley
# https://cssbook.net/content/chapter12.html

# #########################################################################
pacman::p_load(tidyverse,
               rvest,
               janitor)

# https://www.bayern.landtag.de/robots.txt

start_url <- "https://www.bayern.landtag.de/abgeordnete/abgeordnete-von-a-z/"

html <- read_html(start_url)
html



# Extraktion der Infos von der ersten Seite und der Links zu den Abgeordneten-Seiten -------------------------
abg_tr <- html %>% 
  html_elements(".gender") 

abg_tr

# nur der erste Abgeordnete:
abg_tr[1]

gender <- abg_tr[1] %>% 
  html_attr("class") %>% 
  str_extract(".$")

if(gender == "M"){
  gender <- "male"
} else if (gender == "W"){
  gender <- "female"
} else{
  gender <- "diverse"
} 

abg <- abg_tr[1] %>% 
  html_elements("td") %>% 
  html_text() %>% 
  str_trim() 


url <- abg_tr[1] %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  str_c("https://www.bayern.landtag.de", .)

df <- tibble(name = abg[2], 
             first_name = abg[3], 
             title = abg[4], 
             party = abg[5],
             gender = gender,
             url = url)
df


# Extraktion in for-Schleife ----------------------------------------------
df <- list(length = length(abg_tr)) 

for(i in seq_along(abg_tr)){
  gender <- abg_tr[i] %>% 
    html_attr("class") %>% 
    str_extract(".$")
  
  abg <- abg_tr[i] %>% 
    html_elements("td") %>% 
    html_text() %>% 
    str_trim() 
  
  url <- abg_tr[i] %>%
    html_element("a") %>% 
    html_attr("href") %>% 
    str_c("https://www.bayern.landtag.de/", .)
  
  df[[i]] <- tibble(name = abg[2], 
                    first_name = abg[3], 
                    title = abg[4], 
                    party = abg[5],
                    gender = gender,
                    url = url)
}
df

df <- bind_rows(df)

df # nice! 


# Extraktion als Funktion -------------------------------------------------

extract_abg_line <- function(abg_tr){
  gender <- abg_tr %>% 
    html_attr("class") %>% 
    str_extract(".$")
  
  abg <- abg_tr %>% 
    html_elements("td") %>% 
    html_text() %>% 
    str_trim() 
  
  url <- abg_tr %>%
    html_element("a") %>% 
    html_attr("href") %>% 
    str_c("https://www.bayern.landtag.de/", .)
  
  df <- tibble(name = abg[2], 
               first_name = abg[3], 
               title = abg[4], 
               party = abg[5],
               gender = gender,
               url = url)
  return(df)
}

# Test der Funktion:
extract_abg_line(abg_tr[3])


# Wir könnten die Funktion in eine Schleife packen:
df <- list(length = 20) # 20 ersetzen!
for(i in seq_along(abg_tr)){
  df[[i]] <- extract_abg_line(abg_tr[i])
}
df <- bind_rows(df)

df


# Noch besser: Benutzen von apply()/map() und R als funktionale Programmiersprache!
df <- map(abg_tr, extract_abg_line) %>% 
  bind_rows()

df


# Jetzt zur Seite von Abgeordnetem Nr. 1 ----------------------------------

df$url[1]

html_abg <- read_html(df$url[1])
html_abg

position <- html_abg %>% 
  html_element(".position") %>% 
  html_text()

religion <- html_abg %>% 
  html_element(".position") %>% 
  html_text()

birth_date <- html_abg %>%   
  html_elements(".teaser-body") %>% 
  html_text() %>% 
  str_extract("geboren ([0-9]{2}.[0-9]{2}.)?[0-9]{4}") %>% 
  str_remove("geboren ")

rel_denomination <- html_abg %>%   
  html_elements(".teaser-body") %>% 
  html_text2() %>% 
  str_extract("\\nKonfession/Bekenntnis .*") %>% 
  str_remove_all("\\nKonfession/Bekenntnis") %>% 
  str_trim()

# Auch hier wäre wieder eine SChleife oder eine Funktion möglich.
# Einen Vorschlag für das komplette Skriot findet ihr in der Datei mit dem
# Zusatz "complete".


# Speichern des Datensatzes -----------------------------------------------‚

file_name <- str_c("data/abgeordnete_landtag_bayern_", Sys.Date(), ".csv")
write_csv2(df, file_name)
