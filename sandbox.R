library("tidyverse")
##sandbox

sample(1:6, 3)

factor

install.packages("rgho")
##
library('rgho')

search_values("neonatal", dimension = "GHO")

## data are divided into dimensions
gho.dims <- get_gho_dimensions()

help(WHO)
## CM_03 Number of neonatal deaths (0 to 27 days)
nn <- get_gho_data(code='CM_03')
str(nn)

sex <- get_gho_values(dimension = "SEX")

gho <- get_gho_values(dimension = "GHO")
str(gho)
## logical_grep or grepl
gho.obesity <- as.data.frame(gho) |> filter ( grepl('obesity', tolower(Title)) )

gho.fertility <- as.data.frame(gho) |> 
  filter ( grepl('fertility', ignore.case = T, Title) )

tfr <- get_gho_data(code='WHS9_95')

tfr.pl <- tfr |>
  filter ( COUNTRY == 'POL' )

#### Housholds by number of persons
#### https://bdl.stat.gov.pl/bdl/metadane/cechy/4287

library("bdl")
library("knitr")
options(bdl.api_private_key ='19c1cfc8-faec-4db5-9e9a-08db64593a4a')

## This is census data every 10 years
## 1-person ... 5-and-more households
## 2021 census
p.vars <- c('1652550', '1652551', '1652552', '1652553', '1652554')

hholds <- get_data_by_variable(p.vars, 
                                unitParentId=NULL, ## unitLevel4 id
                                unitLevel=5)
write.csv(hholds, file='households_census_2021.csv', row.names = F)


hholds0 <- hholds |>
  select (name, h1=val_1652550, h2=val_1652551, h3=val_1652552, h4=val_1652553, h5=val_1652554) |>
  pivot_longer(cols=c(h1, h2, h3, h4, h5), names_to = 'h', values_to = 'v')


## get data for kwidzyn only
kw <- hholds0 |> filter (grepl('kwidzyński', name)) |>
  select(h, v) |>
  ## compute relative freqency
  ## compute total as a separate column
  mutate (t= sum(v)) |>
  mutate (p = v/t * 100) |>
  select(h, v, p) %>%
  # h is a factor anyway but we rename it's labels
  mutate (h = factor(h, labels = c('1', '2', '3', '4', '5 and more'))) |>
  mutate (cum = cumsum(v)) %>%
  # Add total row
  tibble::add_row(h="Total", v= sum(.$v), p= sum(.$p), cum = NA)

## print finally
kable(kw, digits=2, col.names = c('Persons', 'Households', '%', 'cum'), booktabs = TRUE)

##
library('owidR')
#install.packages('owidR')
owid_search("emissions")
# Not working

dA <- read.csv("fertility_rate_2003_2018.csv", sep = ';', header=T, na.string="NA");
d2018 <- dA %>% filter(yr==2018)

breaks00 <- seq(1, 6.5, by=.5)
breaks12 <- c(seq(1, 6.0, by=.5), 10)
breaks12

d2018 <- d2018 |>
  ##
  ## recode outliers to some resonable values (6.4)
  ##mutate (frate = case_when (frate < 6 ~ frate, TRUE ~ 6.4 ) ) |>
  mutate(frateClass = cut(frate, breaks=breaks12,
                          labels=c('1,0--1,5]', '1,5--2,0]',
                                   '2,0--2,5]', '2,5--3,0]', 
                                   '3,0--3,5]', '3,5--4,0]',
                                   '4,0--4,5]', '4,5--5,0]',
                                   '5,0--5,5]', '5,5--6,0]',
                                   '6,0 i więcej'
                          ) ))
d2018s <- d2018 %>% group_by(frateClass) |> summarise(n=n())
t2 <- kable(d2018s, col.names = c('Wsp. dzietności', 'liczba krajów'), booktabs = TRUE)
t2


breaks33 <- c(seq(1, 4.0, by=.3), 10)

d2018 <- d2018 |>
  mutate(frateClass = cut(frate, breaks=breaks12,
                          labels=c('1,0--1,3]', '1,3--1,6]',
                                   '1,6--1,9]', '1,9--2,2]', 
                                   '2,2--2,5]', '2,5--2,8]',
                                   '2,8--3,1]', '3,1--3,4]',
                                   '3,4--3,7]', '3,7--4,0]',
                                   '4,0 and more'
                          ) ))

d2018s <- d2018 %>% group_by(frateClass) |> summarise(n=n()) |>
  mutate (total = sum(n)) |> mutate (p = n/total * 100) |>
  select (frateClass, n, p) 

t2 <- kable(d2018s, col.names = c('Fertility rate', 'Countries', '%'), booktabs = TRUE)
t2

###
###
ggplot(data.frame(x = c(-2, 2)), aes(x)) +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_function(fun = dnorm, geom='area', xlim = c(-4,4), 
                color='blue',
                fill='red') + 
  stat_function(fun = dnorm, 
                xlim = c(-1.5,1.5),
                fill='lightblue',
                geom = "area") +
  annotate('text', x=-1.5, y=-0.02, label='-Zc') +
  annotate('text', x=+1.5, y=-0.02, label='+Zc') +
  annotate('text', x=+2.5, y=+0.05, label='½ 𝛼') +
  annotate('text', x=-2.5, y=+0.05, label='½ 𝛼') +
  geom_vline(aes(xintercept = 0))



ggplot(data.frame(x = c(-2, 2)), aes(x)) +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank()) +
  stat_function(fun = dnorm, geom='area', xlim = c(-4,4), 
                color='blue',
                fill='darkred') + 
  stat_function(fun = dnorm, 
                xlim = c(-2,2),
                fill='red',
                geom = "area") +
  stat_function(fun = dnorm, 
                xlim = c(-1.5,1.5),
                fill='lightblue',
                geom = "area") +
  ##
  ##
  annotate('text', x=-3, y=0.02, label='½ p') +
  annotate('text', x=+3, y=0.02, label='½ p') +
  annotate('text', x=+2.5, y=+0.05, label='½ 𝛼') +
  annotate('text', x=-2.5, y=+0.05, label='½ 𝛼') +
  ##
  annotate('text', x=-1.5, y=-0.02, label='-Zc') +
  annotate('text', x=+1.5, y=-0.02, label='+Zc') +
  annotate('text', x=-2.2, y=-0.02, label='-Ẑ') +
  annotate('text', x=+2.2, y=-0.02, label='+Ẑ') +
  geom_vline(aes(xintercept = 0))

###
sample.size <- 30
population1 <- rnorm(10000, mean=5, sd=2)
sample1 <- sample(population1, sample.size)

pop.mean <- mean(population1)
sample.mean <- mean(sample1)
pop.mean
sample.mean
####################### Testing hypotheses about the mean
## H_0:  m = 4.5
## H_1:  m != 4.5

Z <- (sample.mean - 4.5) / (2.0 / sqrt(sample.size))
Z
## Z-distribution
p_val_2 <- pnorm(- abs(Z))
p_val <- 2 * p_val_2
p_val
## not significant at 0,05

sample.mean <- function(x) {  m <- mean(sample(population1, sample.size)); return (m) }
samples <- lapply(1:1000 ,  function(i) sample.mean )

###
s <- data.frame(v=rnorm(n=5000, mean=5, sd=2))

library("ggplot2")
ggplot(s, aes(x=v)) +
  geom_histogram(binwidth= .1, alpha=.5, fill="steelblue")

s0 <- sample(s$v, 20)

xy <- data.frame()

for (i in 1:200) {
  s0 <- sample(s$v, 20)
  m <- mean(s0)
  mi <- 1.96 * 2/sqrt(20)
  lower <- m - mi
  upper <- m + mi
  xy.xy <- data.frame(lower, upper)
  xy <- xy |> bind_rows(xy.xy)
}
xy1 <- xy |> filter (lower > 5 | upper < 5)
xy1
nrow(xy1)/250

#####
##### fertility employment age
library("eurostat")
emp <- get_eurostat('lfsi_emp_a')
levels (as.factor(emp$age))

emp0 <- emp |> filter (age == 'Y15-64') |>
  filter (sex == 'F'  & unit == 'PC_POP') |>
  filter (indic_em == 'EMP_LFS') |>
  mutate (year = substr(TIME_PERIOD, 1, 4)) |>
  rename (emp=values)

emp0.pl <- emp0 |> filter (geo == 'PL')

## fertility

## education
## Population by educational attainment level, sex and age (%) - main indicators
edu <- get_eurostat('edat_lfse_03')
edu0 <- edu |> filter (sex == 'F' & isced11 == 'ED5-8' & age == 'Y15-64') |>
  mutate (year = substr(TIME_PERIOD, 1, 4)) |>
  rename (edu=values)

edu0.pl <- edu0 |> filter (geo == 'PL') 


## Age (females)
age <- get_eurostat('demo_pjanind')
age0 <- age |> filter (indic_de == 'FMEDAGEPOP') |>
  mutate (year = substr(TIME_PERIOD, 1, 4)) |>
  rename (age=values)
age0.pl <- age0 |> filter (geo == 'PL')

## Fertility
fert <- get_eurostat('demo_find')
fert0 <- fert |> filter (indic_de == 'TOTFERRT') |>
  mutate (year = substr(TIME_PERIOD, 1, 4)) |>
  rename (tfr=values)

fert0.pl <- fert0 |> filter (geo == 'PL')

##
data <- left_join(emp0.pl, edu0.pl, by='year') |>
  left_join(age0.pl, by='year') |>
  left_join(fert0.pl, by='year') |>
  select (tfr, emp, age, edu, year)

m1 <- lm(tfr ~ emp + age + edu, data = data)

summary(m1)

## correlation matrix
data |> select (-c(year)) |> na.omit() |>
cor(method='pearson')

## compute r for tfr vs edu for EU
data.eu <- left_join(emp0, edu0, by=c('year', 'geo')) |>
  left_join(age0, by=c('year', 'geo')) |>
  left_join(fert0, by=c('year', 'geo')) |>
  select (tfr, emp, age, edu, year, geo)

tfr_r <- data.eu |> select (geo, edu, tfr) |> na.omit() |>
  group_by(geo) %>%
  summarise (cor = cor(edu, tfr, method='pearson'))

tfr_r

ggplot(data, aes(x=as.numeric(year), y=tfr)) +
  geom_line()

###

library("bdl")
options(bdl.api_private_key ='19c1cfc8-faec-4db5-9e9a-08db64593a4a')
options(bdl.lang='en')
## zmienne podpięte pod określony temat
v <- get_variables('P2914')
p.vars <- c('199206', '199207') 
## ludność w podziale na gminy (na poziomie gmin uL=6)
##pop5 <- get_data_by_variable(p.vars, 
##                               unitParentId=NULL, ## unitLevel4 id
##                               unitLevel=5)
##write.csv(pop, file='pop2023.csv', row.names = F)
##write.csv(pop5, file='pop2023_5.csv', row.names = F)
pop <- read.csv('pop2023.csv', colClasses = c('character', 'character', 'character',
                                              rep('character', 10))) |>
  mutate (year = as.numeric(year),
          val_199206 = as.numeric(val_199206),
          val_199207 = as.numeric(val_199207))
## pop = poziom gmin
##
id2teryt <- function(id) {
  sprintf ("%s%s%s", substr(id, 3,4),
    substr(id, 8,9),
    substr(id, 10,12));
}
## Zmień NUTS => TERYT
id2teryt('011212001011')

pop0 <- pop |> filter (year >= 2000 ) %>%
  mutate (val = val_199206 + val_199207) |>
  select (id, name, year, val) |>
  mutate (id = id2teryt(id)) |>
  group_by(id) |>
  mutate(p = (val -first(val))/ first(val) * 100 )

##
## pop0 |> filter (name == 'Sopot')  
## Works

pop2023 <- pop0 |> filter (year == 2023)

## histogram
ggplot(pop2023, aes(x=p)) +
  ggtitle('Stan ludności wg gmin 2000/23 (%)') +
  xlab('%') +
  geom_histogram(binwidth = 5, color='skyblue', fill='skyblue')

##
## https://blog.prokulski.science/2021/05/09/jak-zostac-analitykiem/
## https://rpubs.com/MateuszMajak/806464
## https://geoforum.pl/upload2/files/rgugik_poradnik.html
install.packages('rgugik')
library('rgugik')
library("sf")
borders_download(type="administrative units")

st_layers("/home/tomek/Data/GiS/gis2022/")
gminy.PL <-read_sf("/home/tomek/Data/GiS/gis2022/","gminy")
##gminy.PL <-read_sf("/home/tomek/Data/GiS/A02_Granice_powiatow.shp","A02_Granice_powiatow")
plot(gminy.PL$geometry)
nrow(gminy.PL)

plot(st_geometry(gminy.PL[gminy.PL$JPT_NAZWA_ == 'Hel',]))
#### Plot shapes
gminy.PL %>% 
  ggplot() +
  geom_sf()
##
##
ep <- left_join(gminy.PL, pop2023, by=c("JPT_KOD_JE"='id'))

ep_symbol_pos <- st_centroid(ep, of_largest_polygon = TRUE)

r5 <- ggplot() +
  ##facet_wrap(~year) +
  ## Rysuje mapę
  geom_sf(data = gminy.PL, fill = "grey90")   +
  ##
  geom_sf(data = ep_symbol_pos,
          pch = 21, #### kształt
          ## http://www.sthda.com/english/wiki/ggplot2-point-shapes
          aes(size = p),
          fill = alpha("red", 0.3),
          col = alpha("red", 0.3))
r5
ggsave(r5, file='gminy.png', width=9)

min.p <- min(ep$p, na.rm = T)
max.p <- max(ep$p, na.rm =T)

nrow(ep)
cities <- c ('Hel', 'Gdańsk', 'Gdynia', 
              'Sztutowo', 'Stegna')

r2 <- ep %>% 
  ##filter (jpt_nazwa_ %in% cities) |>
  #mutate (pr = cut_interval(p, 8)) %>%
  mutate (pr = cut(p, breaks=c(min.p, 0,25,50,100,max.p))) %>%
  ggplot(aes(fill=pr)) +
  ggtitle('Zmiana stanu ludności (2000=0%)') +
  #scale_fill_discrete(name = "% zmiany") +
  guides(fill=guide_legend(title="% zmiany")) +
  geom_sf() +
  scale_fill_viridis_d()

r2
ggsave(r2, file='gminy_xx.png', width=9)

###  budownictwo mieszkaniowe
v <- get_variables('P3824')
## 748601 - mieszkania
## 748603 - powierzchnia



p.vars <- c('748601', '748603') 
## ludność w podziale na gminy (na poziomie gmin uL=6)
hous <- get_data_by_variable(p.vars, 
                             unitParentId=NULL, ## unitLevel4 id
                             unitLevel=6)

write.csv(hous, file='hous2023.csv', row.names = F)

hous0 <- hous |> filter (year >= 2000 ) %>%
  select (id, name, year, val=val_748601) |>
  mutate (id = id2teryt(id)) |>
  group_by(id) |>
  summarise(h = sum(val), year=last(year) )
## ujemne wartości
pop2023_diff <- pop0 |>
  group_by(id) |>
  mutate (f = first(val), l= last(val), diff = l - f) |>
  filter ( year == last(year))

eq <- left_join(gminy.PL, hous0, by=c("JPT_KOD_JE"='id')) |>
  left_join(pop2023_diff, by=c("JPT_KOD_JE"='id'))
eq_symbol_pos <- st_centroid(ep, of_largest_polygon = TRUE)

## na 1000 mieszkańców w roku l
eq0 <- eq |>
  mutate (l2 = h / l * 1000)

min.q <- min(eq0$l2, na.rm = T)
max.q <- max(eq0$l2, na.rm =T)

##eq0 |> filter (l2 > 199)

r3 <- eq0 %>% 
  #mutate (pr = cut_interval(p, 8)) %>%
  mutate (pr = cut(l2, breaks=c(min.q, 50,100,150,250,max.p))) %>%
  ggplot(aes(fill=pr)) +
  ggtitle('Mieszkania wybudowane na 1000 mieszkańców (2000--2023)') +
  #scale_fill_discrete(name = "% zmiany") +
  guides(fill=guide_legend(title="Mieszkania")) +
  geom_sf() +
  scale_fill_viridis_d()

r3
ggsave(r3, file='mieszkania_yy.png', width=9)

##
library(rgdal)
powiaty <- readOGR("/home/tomek/Data/GiS/PL2022/", "A02_Granice_powiatow")
powiaty_mapa <- left_join(powiaty, pop2023, by=c("JPT_KOD_JE"='id'))


powiaty %>% 
  ggplot() +
  geom_sf()

###########################################################################
#####
###########################################################################

s <- 10
beta1 <- 1.2
size <- 10 ## sample size
x <- seq (1:size)
epsilon <- rnorm(size, 0, s)
lsize <- 0.4

y.true <- 5 + beta1 * x ## true equation
## sample equations
y <- y.true + epsilon
ss <- 's1'

xy.true <- data.frame(x, y.true, ss)
xy <- data.frame(x, y, ss)
b1 <- c()

for (i in 1:29) {
  epsilon <- rnorm(size, 0, s)
  y <- y.true + epsilon
  ss <- sprintf("s%i", i)
  xy.xy <- data.frame(x, y, ss)
  xy <- xy |> bind_rows(xy.xy)
  ## remember b1
  m1 <- lm(y ~ x,  data=xy.xy)
  b1[i] <- coef(m1)[2]
}
##nrow(xy)

p1 <- ggplot(xy, aes(x=x, y=y, color=ss, group=ss)) +
  geom_point() +
  ## no legend
  guides(colour=FALSE) +
  geom_smooth(method='lm', se=F, size=lsize) +
  geom_smooth(data=xy.true, aes(x=x, y=y.true), 
              method='lm', se=F, size=1, color='black')

p2 <- xy |> filter (x > 3.5 & x < 6) |>
ggplot(aes(x=x, y=y, color=ss, group=ss)) +
  geom_point() +
  ## no legend
  guides(colour=FALSE) +
  geom_smooth(method='lm', se=F, size=lsize, fullrange = TRUE) +
  geom_smooth(data=xy.true, aes(x=x, y=y.true), 
              method='lm', se=F, size=1, color='black')
p2
p1

data("marketing", package = "datarium")
str(marketing)
## copy the structure of DF
marketing.fut <- marketing |> filter(FALSE)
## add one new row
marketing.fut <- marketing.fut |> add_row(youtube = 100, 
    facebook = 300, newspaper = 40, sales = NA)

library("lmtest")
##library('skedastic')
##install.packages('skedastic')
##install.packages("skedastic", dependencies = c("Depends", "Imports"))
bptest(model1)
library('datarium')
model1 <- lm(sales ~ youtube, data = marketing)
model2 <- lm(sales ~ youtube + facebook +newspaper, data = marketing)
summary(model1)
# components of lm model (formally R `object`):
names(model1)
bptest(model1, ~ youtube + I(youtube^2), data=marketing)

model1l <- lm(log(sales) ~ log(youtube), data = marketing)
bptest(model1l)

ggplot(marketing, aes(x=predict(model1), y= sales)) +
  geom_point() +
  geom_abline(intercept=0, slope=1)

lmtest::dwtest(model1)

### Average monthly expenditures
### https://bdl.stat.gov.pl/bdl/metadane/cechy/1870

library('eurostat')
library('bdl') ## local-data-bank or Bank Danych Lokalnych
options(bdl.api_private_key ='19c1cfc8-faec-4db5-9e9a-08db64593a4a')

p.vars <- c('7737',  ## Average monthly expenditures
            '216968' ## ditto income
)
## get data for p.vars for whole Poland
## quite big (380 counties )
d <- get_data_by_variable(p.vars, 
                                unitParentId=NULL, ## unitLevel4 id
                  ## 0 -PL; 1 - macroregion
                                unitLevel=0) |>
  select (year, expenditures=val_7737, income=val_216968) |>
  na.omit()
write.csv(d, file='exp_income_PL.csv', row.names = F)

model10 <- lm(expenditures ~ income, data=d)
summary(model10)
lmtest::dwtest(model10)

x=predict(model10)
ggplot(d, aes(y=predict(model10) - expenditures, x= as.numeric(year))) +
  geom_line()
###

library('AER')
d <- CPS1988
levels(d$ethnicity)
d1 <- d |> mutate (ethnicity = relevel(ethnicity, ref='afam'))

model3w <- lm ( log(wage) ~ experience + I(experience^2) + education + ethnicity,
                data = d1 )
summary(model3w)

names(model3w)

library('car')
? Salaries
model9 <- lm(salary ~ yrs.service + rank + discipline + sex, data=Salaries)
summary(model9)

anova(model2)
anova(model2, model1)
anova(model1)
sse <- sum((fitted(model2) - marketing$sales)^2)
#find ssr/ess
ssr <- sum((fitted(model2) - mean(marketing$sales))^2)
#find sst/tss
sst <- ssr + sse
sse
ssr
sst

model0 <- lm(sales ~ 1, data=marketing)
summary(model0)
ssr <- sum((fitted(model1) - mean(marketing$sales))^2)
ssr
sse <- sum((fitted(model1) - marketing$sales)^2)
sse
anova(model0)

seq_len(5)
seq(5)
sample_size <- floor(0.80 * nrow(marketing))
train_ind <- sample(seq(nrow(marketing)), size = sample_size)


## 80% for train; the rest for test
sample_size <- floor(0.80 * nrow(marketing))

## seq(n) = 1,2,3,4...,n
## sample(v, c) = generate c sample from v vector
train_ind <- sample(seq(nrow(marketing)), size = sample_size)

## fetch training data
train.data <- marketing[train_ind, ]
## fetch but not training data
test.data <- marketing[ -train_ind, ]

predictions1 <- predict(model1, test.data )

predictions1

plot(sales ~ youtube, data = train.data )
points (predictions1 ~ test.data$youtube, col='blue')
lines (predictions1 ~ test.data$youtube, col='red')

##

p.vars <- c('7737',  ## Average monthly expenditures
            '7739'   ## food and NA beverages
)
d <- get_data_by_variable(p.vars, 
                          unitParentId=NULL, ## unitLevel4 id
                          ## 0 -PL; 1 - macroregion
                          unitLevel=0) |>
  select (year, expenditures=val_7737, income=val_7739) |>
  na.omit()

model10 <- lm(expenditures ~ income, data=d)
summary(model10)

###
library('eurostat')
library('bdl') ## local-data-bank or Bank Danych Lokalnych
options(bdl.api_private_key ='19c1cfc8-faec-4db5-9e9a-08db64593a4a')
p.vars <- c('7737',  ## Average monthly expenditures
            '7739'   ## food and NA beverages
)
## na poziomie województw
## ostatnie dane = 1999
d <- get_data_by_variable(p.vars, 
                          unitParentId=NULL, ## unitLevel4 id
                          ## 0 -PL; 1 - macroregion
                          unitLevel=3) |>
  select (year, name, expenditures=val_7737, food=val_7739) |>
  na.omit() |>
  mutate(year = as.numeric(year)) |>
  group_by(name) |> arrange(year) |>
  filter (row_number()==n())

model11 <- lm(log(food) ~ log(expenditures) , data=d)
summary(model11)
