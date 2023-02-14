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

url <- "https://www.bayern.landtag.de/abgeordnete/abgeordnete-von-a-z/"

html <- read_html(url)
html

# get a list of the links to pages of every first letter (a, b, c...)
abc_links <- html %>% 
  html_elements(".tx-stbltabgeordnete-az a") %>% 
  html_attr("href") %>% 
  str_c("https://www.bayern.landtag.de", .) %>% 
  unlist()

# delete "alle"-page
abc_links <- abc_links[-length(abc_links)]


# some letters have more than 1 page, so we need to add second page
for(i in 1:length(abc_links)){
  next_page <- read_html(abc_links[i])%>%   
    html_element(".icon-Icon_Chevro-right") %>% 
    html_attr("href") %>% 
    str_c("https://www.bayern.landtag.de", .)
  
  if(!is.na(next_page) > 0){
    print(i)
    abc_links <- append(abc_links, next_page)
  }
}

# Extract parliamentarians table-lines from the overview page  
get_abg_line <- function(abc_link){
  abg_line <- read_html(abc_link) %>%   
    html_elements(".gender") 
  
  return(abg_line)
}


abg_lines <- abc_links %>% 
  map(get_abg_line)



# function to extract all the data for a single parliamentarian
get_abg_info <- function(abg_line){
  
  abg <- abg_line %>%
    html_elements("td") %>%  
    html_text() %>% 
    str_trim()
  
  gender <- abg_line %>%
    html_attr("class") %>% 
    str_extract(".$")
  
  url <- abg_line %>%
    html_element("a") %>% 
    html_attr("href") %>% 
    str_c("https://www.bayern.landtag.de", .)
  
  abg_info <- read_html(url)
  
  el_district <- abg_info %>%   
    html_element("p strong") %>% 
    html_text() %>% 
    str_remove(c("Wahlkreis|Stimmkreis")) %>% 
    str_trim()
  
  profession <- abg_info %>%   
    html_elements(".position") %>% 
    html_text()
  
  if(length(profession) == 0) profession <- NA
  
  birth_date <- abg_info %>%   
    html_elements(".teaser-body") %>% 
    html_text() %>% 
    str_extract("geboren ([0-9]{2}.[0-9]{2}.)?[0-9]{4}") %>% 
    str_remove("geboren ")
  
  birth_place <- abg_info %>%   
    html_elements(".teaser-body") %>% 
    html_text2() %>% 
    str_extract("\\nin .*") %>% 
    str_remove_all("\\nin") %>% 
    str_trim()
  
  martial_status <- abg_info %>%   
    html_elements(".teaser-body") %>% 
    html_text2() %>% 
    str_extract("\\nFamilienstand .*") %>% 
    str_remove_all("\\nFamilienstand") %>% 
    str_trim()
  
  rel_denomination <- abg_info %>%   
    html_elements(".teaser-body") %>% 
    html_text2() %>% 
    str_extract("\\nKonfession/Bekenntnis .*") %>% 
    str_remove_all("\\nKonfession/Bekenntnis") %>% 
    str_trim()
  
  membership <- abg_info %>%   
    html_elements(".teaser-body") %>% 
    html_text() %>% 
    str_extract("Mitglied des Landtags:.*") %>% 
    str_remove("Mitglied des Landtags:") %>% 
    str_remove("English") %>% 
    str_trim()
  
  email <- abg_info %>%   
    html_element(".box+ .box p a") %>% 
    html_text()
  
  facebook <- abg_info %>%   
    html_element(".icon-Facebook") %>% 
    html_attr("href")
  
  twitter <- abg_info %>%   
    html_element(".icon-Twitter") %>% 
    html_attr("href")
  
  instagram <- abg_info %>%   
    html_element(".icon-Instagram") %>% 
    html_attr("href")
  
  website <- abg_info %>%  
    html_element(".box+ .box") %>% 
    html_element(".icon-Icon_Arrow-right") %>% 
    html_attr("href")
  
  
  df <- tibble(name = abg[2], 
               first_name = abg[3], 
               title = abg[4], 
               party = abg[5],
               gender = gender,
               url = url,
               el_district = el_district,
               profession = profession,
               birth_date = birth_date,
               birth_place = birth_place,
               martial_status = martial_status,
               rel_denomination = rel_denomination,
               membership = membership,
               email = email,
               facebook = facebook,
               twitter = twitter,
               instagram = instagram,
               website = website
               )
  
  return(df)
}

# unnest the list of xml nodes
df <- list() 
for(i in seq_along(abg_lines)){
  for(j in seq_along(abg_lines[[i]])){
    df <- append(df, abg_lines[[i]][j])
  }
}

# do the actual extraction  
df <- df %>% 
  map(get_abg_info) %>% 
  bind_rows() %>% 
  arrange(name, first_name)

df %>% 
  tabyl(party) %>% 
  arrange(desc(n))

df %>% 
  tabyl(profession) %>% 
  arrange(desc(n)) %>% 
  filter(n > 2)

df %>% 
  tabyl(gender) %>% 
  arrange(desc(n))

df %>% 
  tabyl(rel_denomination) %>% 
  arrange(desc(n))

df %>% 
  summarise(email = sum(!is.na(email))/nrow(df),
            website = sum(!is.na(website))/nrow(df),
            facebook = sum(!is.na(facebook))/nrow(df),
            twitter = sum(!is.na(twitter))/nrow(df),
            instagram = sum(!is.na(instagram))/nrow(df))

# save dataset to disc
file_name <- str_c("data/abgeordnete_landtag_bayern_", Sys.Date(), ".csv")
write_csv2(df, file_name)
