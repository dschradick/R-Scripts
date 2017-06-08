########## DPLYR - ÜBERSICHT
# Referenz: https://dplyr.tidyverse.org/reference/index.html 
library(tidyverse)
library(lubridate)


glimpse(mtcars)
table(mtcars$cyl)

# Non-Standard & Standard Evaluation
mtcars %>% group_by(cyl)
mtcars %>% group_by_('cyl')

#### DIVERSES
## Spaltennamen
names(mtcars) = sub(" ", "_", names(mtcars))           # Leerzeichen => _
names(mtcars) = names(mtcars) %>% tolower()            # Kleinschreibung
## Reihennamen
m <- mtcars %>% rownames_to_column(var='carname')      # Reihennamen => Spalte 
m %>% column_to_rownames(var='carname')                # Spalte => Reihennamen           
remove_rownames(mtcars)                                # Reihennamen löschen

## Diskretisieren
mtcars %>% 
  mutate(hp_bins = case_when(
    hp < 150 ~ 'low',
    hp < 200 ~ 'medium',
    TRUE ~ 'high'))

## Werte umbennen 
# Level genordnet nach auftreten beim aufruf
am <- recode_factor(mtcars$am, '1' = 'automatic', '0' = 'manual')

## Reorder von Level
levels <- c('0','1')
mtcars$am <- factor(mtcars$am, levels)
levels(mtcars$am)




#### BEOBACHTUNGEN
### Extrahieren
mtcars %>% filter(cyl > 5)
mtcars %>% filter(between(cyl,4,5))
mtcars %>% filter(cyl > 5, am == 1)                    # Mehrere
mtcars %>% filter(hp > quantile(hp,.95))               # top 5%
mtcars %>% distinct(cyl)     
mtcars %>% sample_n(10)                                # Random sample - Anzahl
mtcars %>% sample_frac(.1)                             # Random sample - Prozent
mtcars %>% slice(1:5)                                  # Auswahl nach Position 
mtcars %>% head(5)     

### Sortieren
mtcars %>% arrange(qsec) 
mtcars %>% arrange(desc(qsec)) 


#### SPALTEN
## Auwahl
mtcars %>% select(1:3,5)                               # Position
mtcars %>% select(hp,am)                               # Name
mtcars %>% select(1,am)                                # Position + Name
mtcars %>% select(-am,-carb)                           # ALLE AUSSER
mtcars %>% select_if(is_numeric)                       # NUMERISCHE

## Hinzufügen
mtcars %>% add_column(new = 1:32)                      # Neue Spalte durch Werte
mtcars %>% mutate(mpg_per_hp = mpg / hp )              # Neue Spalte berechnen
mtcars %>% mutate(mpg_per_hp = mpg / hp )              # Neue Spalte berechnen

## Umbenennen
mtcars %>% rename(ps = hp)                             # Neue Spalte durch Werte



#### Summarizing
mtcars %>% summarise(mean_carb = mean(carb))          
mtcars %>% summarise_all(mean)

## Anzahl & Verhältnis
mtcars %>% summarise(count = n())                      # Anzahl Reihen
mtcars %>% summarise(count = n_distinct(cyl))          # Anzahl unique values
mtcars %>% summarise(prop_high_ps = sum(hp > 150))     # Anzahl mit Bed.   
mtcars %>% summarise(prop_high_ps = mean(hp > 150))    # Proportion   

## Count visualisieren
counts <- mtcars %>% group_by(cyl) %>% summarise(count = n()) %>% arrange(desc(cyl))
counts$cyl <- factor(counts$cyl, levels = counts$cyl[order(counts$count,decreasing=T)])
ggplot(counts, aes(x=as.factor(cyl), y = count)) + geom_bar(stat="identity")


### Window Funktionen
mtcars %>% 
  mutate(lagged_mpg = lag(mtcars$mpg)) %>% 
  select(mpg,lagged_mpg) %>% 
  head()

mtcars %>% 
  group_by(cyl) %>%
  arrange(cyl,mpg) %>%
  mutate(cumsum_mpg = cumsum(mpg)) %>%
  mutate(cummax_mpg = cummax(mpg)) %>%
  mutate(denserank_mpg = dense_rank(mpg)) %>%
  mutate(cume_dist_mpg = cume_dist(mpg)) %>%
  mutate(ntile_mpg = ntile(mpg,5)) %>%
  select(mpg,cumsum_mpg,cummax_mpg,denserank_mpg,cume_dist_mpg,ntile_mpg) %>%
  print(n=20)



## Lage 
mtcars %>% summarise(mean_hp = mean(hp))
mtcars %>% summarise(median_hp = median(hp))
## Varianz: IQR(), sd(), var()




