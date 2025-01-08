## Raw -> Clean datasets
## https://bookdown.org/rwnahhas/IntroToR/fixvalues.html
##
library('tidyverse')
library('readxl')
#f <- read_excel("./RheumArth_Tx_AgeComparisons.xlsx") 
#
## Fix mis-spellings or other invalid values



## https://causeweb.org/tshs/arthritis-treatment/
f0 <- f %>% 
  mutate(Sex = case_when(Sex %in% c("f", "female") ~ "F",
                         Sex ==   "m"              ~ "M",
			 ## TRUE means any other value
                         TRUE                      ~ Sex)) ## leave w/o changes

## Case-when resulting values had to have THE SAME type (number, string ...)
## 
f0 <- f %>%
  mutate(AgeGp2 = case_when(AgeGp_orig %in% c("40-49y", "50-59y")  ~ "40-59y",
                            AgeGp_orig %in% c("60-69y", "70-79y", "80-90y")  ~ "60-90y",
                            TRUE                ~ AgeGp_orig ))
                                                ############
## Eight big countries + the rest
## Nursing graduates
g0 <- read.csv("nursing_graduates_UE.csv", sep = ';', dec = ".",  header=T, na.string="NA" )
## Rename 8 countries + other
## Names of EU members (there is no name in g0)
members <- read.csv("eu_codes_members.csv", sep = ';', dec = ".",  header=T, na.string="NA" ) %>% 
  add_row(member = 'Other', geo = "OTHER")

members.codes <- members$geo
members.big <-c ('DE', 'ES', 'FR', 'IT', 'PL', 'RO', 'NL', 'BE')

g1 <- g0 %>%
  filter (year == 2018 & isco08 == 'OC2221_3221' & unit == 'NR') %>%
  ## filter out non-EU entries
  filter (geo %in%  members.codes) |>
  ## rename: 8 + other
  mutate(geo = case_when(geo %in% members.big  ~ geo,
                            TRUE  ~ 'OTHER' )) |>
  ## summarise OTHER
  group_by(geo) |> summarise(values = sum(values)) |>
  ungroup() |>
  ##
  mutate ( geo =  as.factor(geo)) |>
  left_join(members, by='geo') |>
  select (member, values) %>%
  mutate (total = sum (values)) %>%
  mutate (p=values / total * 100) |>
  select (member, values, p) %>%
  ## Add total row
  tibble::add_row(member="Total", values= sum(.$values), p = sum(.$p))

g1

## Repeat above with the following data set
## hlth_rs_grd2 