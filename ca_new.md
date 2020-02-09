Calcium Supplementation During Pregnancy
================
OD, AF
2/8/2020

### Required packages for laoding dataset, manipulation and exploration:

##### Code hidden

### Laoding dataset: \[output\_1.csv\]

##### Code hidden

### Final dataset: Converting columns \[final\_dataset.csv\]

##### Code hidden

<br> <br>

-----

<br> <br>

## I - Calculating Death Rate for each treatment group:

### Table 1.1: Crude death rate: CDR \[crude\_death\_rate.pdf\]

``` r
final_dataset %>%
  filter(item %in% c('death', 'person_time')) %>%
  group_by(input_draw, ca_supp, item ) %>% 
  summarise(sum = sum(value)) %>% 
  spread(item, sum) %>% 
  mutate(crude_death_rate = death / person_time) %>% 
  select(-c(death, person_time)) %>% 
  spread(ca_supp, crude_death_rate) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 2:4) %>% 
  group_by(calcium_supplementation) %>% 
  summarise(mean_CDR = round(1000 * mean(value),1)) %>% 
  kable("html") %>% 
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_CDR

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

1.0

</td>

</tr>

<tr>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

27.9

</td>

</tr>

<tr>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

26.8

</td>

</tr>

</tbody>

</table>

### Table-1.2: Death rate by gender

``` r
final_dataset %>%
  filter(item %in% c('death', 'person_time')) %>%
  group_by(input_draw, ca_supp, item , gender) %>% 
  summarise(sum = sum(value)) %>% 
  spread(item, sum) %>% 
  mutate(death_rate = death / person_time) %>% 
  select(-c(death, person_time)) %>% 
  spread(ca_supp, death_rate) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 3:5) %>% 
  group_by(gender, calcium_supplementation) %>% 
  summarise(mean_death_rate = round(1000 * mean(value),1)) %>% 
  kable("html") %>% 
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

gender

</th>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_death\_rate

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Female

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.8

</td>

</tr>

<tr>

<td style="text-align:left;">

Female

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

25.5

</td>

</tr>

<tr>

<td style="text-align:left;">

Female

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

24.7

</td>

</tr>

<tr>

<td style="text-align:left;">

Male

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

1.2

</td>

</tr>

<tr>

<td style="text-align:left;">

Male

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

30.2

</td>

</tr>

<tr>

<td style="text-align:left;">

Male

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

28.9

</td>

</tr>

</tbody>

</table>

### Table-1.3: Death rate by age groups

``` r
final_dataset %>% 
  filter(item %in% c('death', 'person_time')) %>%
  group_by(input_draw, ca_supp, item , age_group) %>% 
  summarise(sum = sum(value)) %>% 
  spread(item, sum) %>% 
  mutate(death_rate = death / person_time) %>% 
  select(-c(death, person_time)) %>% 
  spread(ca_supp, death_rate) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 3:5) %>% 
  group_by(age_group, calcium_supplementation) %>% 
  summarise(mean_death_rate = round(1000 * mean(value),1)) %>% 
  kable("html") %>% 
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

age\_group

</th>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_death\_rate

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

1\_to\_4

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.0

</td>

</tr>

<tr>

<td style="text-align:left;">

1\_to\_4

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

14.7

</td>

</tr>

<tr>

<td style="text-align:left;">

1\_to\_4

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

14.7

</td>

</tr>

<tr>

<td style="text-align:left;">

Early neonatal

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

222.1

</td>

</tr>

<tr>

<td style="text-align:left;">

Early neonatal

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

1813.2

</td>

</tr>

<tr>

<td style="text-align:left;">

Early neonatal

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

1591.0

</td>

</tr>

<tr>

<td style="text-align:left;">

Late neonatal

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

7.7

</td>

</tr>

<tr>

<td style="text-align:left;">

Late neonatal

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

115.3

</td>

</tr>

<tr>

<td style="text-align:left;">

Late neonatal

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

107.6

</td>

</tr>

<tr>

<td style="text-align:left;">

Post neonatal

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.1

</td>

</tr>

<tr>

<td style="text-align:left;">

Post neonatal

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

36.7

</td>

</tr>

<tr>

<td style="text-align:left;">

Post neonatal

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

36.6

</td>

</tr>

</tbody>

</table>

### Table-1.4: Death rate by causes

``` r
final_dataset %>% 
  filter(item %in% c('death', 'person_time')) %>%
  unite(causes, item, item_outcome) %>% 
  group_by(input_draw, ca_supp, causes) %>% 
  summarise(sum = sum(value)) %>%
  spread(causes, sum) %>% 
  rename(person_time = person_time_NA) %>%
  gather(causes, value, 3:10) %>%  
  mutate(death_rate = value / person_time) %>% 
  select(-c(value, person_time)) %>% 
  spread(ca_supp, death_rate) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 3:5) %>% 
  group_by(causes, calcium_supplementation) %>% 
  summarise(mean_death_rate = round(1000 * mean(value),1)) %>% 
  kable("html") %>%
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

causes

</th>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_death\_rate

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

death\_diarrheal\_diseases

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.0

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_diarrheal\_diseases

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

2.4

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_diarrheal\_diseases

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

2.4

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_hemolytic\_disease\_and\_other\_neonatal\_jaundice

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.0

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_hemolytic\_disease\_and\_other\_neonatal\_jaundice

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

0.2

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_hemolytic\_disease\_and\_other\_neonatal\_jaundice

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

0.2

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_lower\_respiratory\_infections

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.0

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_lower\_respiratory\_infections

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

1.9

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_lower\_respiratory\_infections

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

1.9

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_measles

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.0

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_measles

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

0.4

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_measles

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

0.4

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_encephalopathy\_due\_to\_birth\_asphyxia\_and\_trauma

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.1

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_encephalopathy\_due\_to\_birth\_asphyxia\_and\_trauma

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

1.3

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_encephalopathy\_due\_to\_birth\_asphyxia\_and\_trauma

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

1.2

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_preterm\_birth

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.9

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_preterm\_birth

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

4.9

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_preterm\_birth

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

4.0

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_sepsis\_and\_other\_neonatal\_infections

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.0

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_sepsis\_and\_other\_neonatal\_infections

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

0.7

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_neonatal\_sepsis\_and\_other\_neonatal\_infections

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

0.6

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_other\_causes

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0.0

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_other\_causes

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

16.1

</td>

</tr>

<tr>

<td style="text-align:left;">

death\_other\_causes

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

16.1

</td>

</tr>

</tbody>

</table>

<br> <br>

-----

<br>
<br>

## II - Calculating DALY for each treatment group:

\#\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

DALY (ylls + ylds) by age groups based on calcium supplementation

Reference table from Abie

| Ca\_Supp | mean\_DALY |
| -------- | ---------- |
| No:      | 242598.0   |
| Yes:     | 233372.0   |

# \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \* \*

### Table-2.1: DALY (ylls + ylds) based on calcium supplementation

``` r
final_dataset %>%
  filter(item %in% c('ylls', 'ylds', 'person_time')) %>% 
  group_by(input_draw, ca_supp, item) %>% 
  summarise(value = sum(value)) %>% 
  spread(item, value) %>% 
  mutate(total_dalys = ylds + ylls,
         dalys_per_year = total_dalys / person_time) %>% 
  select(input_draw, ca_supp, dalys_per_year) %>% 
  spread(ca_supp, dalys_per_year) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 2:4) %>% 
  group_by(calcium_supplementation) %>% 
  summarise(mean_Daly = round(100000 * mean(value),0))%>% 
  kable("html") %>%
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_Daly

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

9226

</td>

</tr>

<tr>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

242598

</td>

</tr>

<tr>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

233372

</td>

</tr>

</tbody>

</table>

### Table-2.2: Daly by gender

``` r
final_dataset %>%
  filter(item %in% c('ylls', 'ylds', 'person_time')) %>% 
  group_by(input_draw, ca_supp, item, gender) %>% 
  summarise(value = sum(value)) %>% 
  spread(item, value) %>% 
  mutate(total_dalys = ylds + ylls,
         dalys_per_year = total_dalys / person_time) %>% 
  select(input_draw, gender, ca_supp, dalys_per_year) %>% 
  spread(ca_supp, dalys_per_year) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 3:5) %>% 
  group_by(gender, calcium_supplementation) %>% 
  summarise(mean_Daly = round(100000 * mean(value),0))%>% 
  kable("html") %>%
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

gender

</th>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_Daly

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Female

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

7530

</td>

</tr>

<tr>

<td style="text-align:left;">

Female

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

222011

</td>

</tr>

<tr>

<td style="text-align:left;">

Female

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

214481

</td>

</tr>

<tr>

<td style="text-align:left;">

Male

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

10889

</td>

</tr>

<tr>

<td style="text-align:left;">

Male

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

262549

</td>

</tr>

<tr>

<td style="text-align:left;">

Male

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

251661

</td>

</tr>

</tbody>

</table>

### Table-2.3: Daly by age group

``` r
final_dataset %>%
  filter(item %in% c('ylls', 'ylds', 'person_time')) %>% 
  group_by(input_draw, ca_supp, item, age_group) %>% 
  summarise(value = sum(value)) %>% 
  spread(item, value) %>% 
  mutate(total_dalys = ylds + ylls,
         dalys_per_year = total_dalys / person_time) %>% 
  select(input_draw, age_group, ca_supp, dalys_per_year) %>% 
  spread(ca_supp, dalys_per_year) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 3:5) %>% 
  group_by(age_group, calcium_supplementation) %>% 
  summarise(mean_Daly = round(100000 * mean(value),0))%>% 
  kable("html") %>%
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

age\_group

</th>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_Daly

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

1\_to\_4

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

97

</td>

</tr>

<tr>

<td style="text-align:left;">

1\_to\_4

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

126336

</td>

</tr>

<tr>

<td style="text-align:left;">

1\_to\_4

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

126239

</td>

</tr>

<tr>

<td style="text-align:left;">

Early neonatal

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

1952335

</td>

</tr>

<tr>

<td style="text-align:left;">

Early neonatal

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

15936536

</td>

</tr>

<tr>

<td style="text-align:left;">

Early neonatal

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

13984201

</td>

</tr>

<tr>

<td style="text-align:left;">

Late neonatal

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

68216

</td>

</tr>

<tr>

<td style="text-align:left;">

Late neonatal

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

1015334

</td>

</tr>

<tr>

<td style="text-align:left;">

Late neonatal

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

947118

</td>

</tr>

<tr>

<td style="text-align:left;">

Post neonatal

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

575

</td>

</tr>

<tr>

<td style="text-align:left;">

Post neonatal

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

322113

</td>

</tr>

<tr>

<td style="text-align:left;">

Post neonatal

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

321538

</td>

</tr>

</tbody>

</table>

### Table-1.4: Daly by causes

``` r
final_dataset %>% 
  filter(item %in% c('ylls', 'ylds', 'person_time')) %>%
  unite(causes, item, item_outcome) %>% 
  group_by(input_draw, ca_supp, causes) %>% 
  summarise(sum = sum(value)) %>%
  spread(causes, sum) %>% 
  rename(person_time = person_time_NA) %>% 
  gather(causes, value, 4:18) %>% 
  separate(causes, c('type', 'causes'), sep = '_', extra = 'merge') %>%
  spread(type, value) %>% 
  mutate(total_dalys = ylds + ylls,
         dalys_per_year = total_dalys / person_time) %>% 
  select(input_draw, causes, ca_supp, dalys_per_year) %>% 
  spread(ca_supp, dalys_per_year) %>% 
  mutate(Delta = NO - YES) %>% 
  gather(calcium_supplementation, value, 3:5) %>% 
  group_by(causes, calcium_supplementation) %>% 
  summarise(mean_Daly = round(100000 * mean(value),0))%>% 
  kable("html") %>%
  kable_styling(full_width = F,font_size = 15)
```

<table class="table" style="font-size: 15px; width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

causes

</th>

<th style="text-align:left;">

calcium\_supplementation

</th>

<th style="text-align:right;">

mean\_Daly

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

diarrheal\_diseases

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

196

</td>

</tr>

<tr>

<td style="text-align:left;">

diarrheal\_diseases

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

21512

</td>

</tr>

<tr>

<td style="text-align:left;">

diarrheal\_diseases

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

21315

</td>

</tr>

<tr>

<td style="text-align:left;">

hemolytic\_disease\_and\_other\_neonatal\_jaundice

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

58

</td>

</tr>

<tr>

<td style="text-align:left;">

hemolytic\_disease\_and\_other\_neonatal\_jaundice

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

2098

</td>

</tr>

<tr>

<td style="text-align:left;">

hemolytic\_disease\_and\_other\_neonatal\_jaundice

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

2039

</td>

</tr>

<tr>

<td style="text-align:left;">

lower\_respiratory\_infections

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

26

</td>

</tr>

<tr>

<td style="text-align:left;">

lower\_respiratory\_infections

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

16601

</td>

</tr>

<tr>

<td style="text-align:left;">

lower\_respiratory\_infections

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

16574

</td>

</tr>

<tr>

<td style="text-align:left;">

measles

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

measles

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

3184

</td>

</tr>

<tr>

<td style="text-align:left;">

measles

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

3185

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_encephalopathy\_due\_to\_birth\_asphyxia\_and\_trauma

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

737

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_encephalopathy\_due\_to\_birth\_asphyxia\_and\_trauma

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

11283

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_encephalopathy\_due\_to\_birth\_asphyxia\_and\_trauma

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

10546

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_preterm\_birth

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

7947

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_preterm\_birth

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

43516

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_preterm\_birth

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

35569

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_sepsis\_and\_other\_neonatal\_infections

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

313

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_sepsis\_and\_other\_neonatal\_infections

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

5784

</td>

</tr>

<tr>

<td style="text-align:left;">

neonatal\_sepsis\_and\_other\_neonatal\_infections

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

5471

</td>

</tr>

<tr>

<td style="text-align:left;">

other\_causes

</td>

<td style="text-align:left;">

Delta

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

other\_causes

</td>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

other\_causes

</td>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>
