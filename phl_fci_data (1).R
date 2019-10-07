philly_2009 <- read.csv("Philadelphia_2009_ACS.csv")
philly_2017 <- read.csv("Philadelphia_2017_ACS.csv")
philly_tbl <- inner_join(philly_2017, philly_2009, by = c("Geo_TRACT_2017" = "Geo_TRACT_2009"))
phl_life_exp <- read.csv("phl_life_expectancy.csv")
philly_tbl <- inner_join(philly_tbl, phl_life_exp, by= c("Geo_FIPS_2017" = "tract_id"))

philly_tbl <- philly_tbl %>%
  dplyr::select(-c(STATE2KX, CNTY2KX, e.0., se.e.0.., Abridged.life.table.flag))

philly_tbl <- as_tibble(philly_tbl)

str(philly_tbl)

View(philly_tbl)
philly <- philly_tbl %>%
  dplyr::select(4:251)

#Standardize 

z_philly <- philly %>%
  psycho::standardize()

z_philly_tbl <- z_philly %>%
  cbind(philly_tbl$Geo_TRACT_2017)

z_philly_tbl <- z_philly_tbl %>%
  mutate(tract = philly_tbl$Geo_TRACT_2017) 

?clean_names

z_philly_tbl %>% clean_names(case = "old_janitor")

View(z_philly_tbl)
str(z_philly_tbl)

#Add trend variables

z_philly_tbl_trends <- z_philly_tbl %>%
  dplyr::select(Total_Population_2017, total_population_2009, Median_Household_Income_2017, Median_Household_Income_2009, Gini_Index_2017, Gini_Index_2009, Population_25_Years_and_Bachelors_2017, Population_25_Years_and_Bachelors_2009, tract) %>%
  mutate(
    income_change = Median_Household_Income_2017 - Median_Household_Income_2009,
    population_change = Total_Population_2017 - total_population_2009,
    inequality_change = Gini_Index_2017 - Gini_Index_2009,
    bachelors_change = Population_25_Years_and_Bachelors_2017 - Population_25_Years_and_Bachelors_2009
  ) 

#Sample correlation matrix

psych::describe(z_philly_tbl_trends)

psych::pairs.panels(z_philly_tbl_trends[], gap=0)

z_philly_matrix_trends <- as.matrix(z_philly_tbl_trends)

pmcorr <- rcorr(z_philly_matrix_trends)
pmcorr


#Sample plot
ggplot(data = z_philly_tbl_trends, aes(x=bachelors_change, y= income_change, label= tract)) +
  geom_point() +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = .05) + geom_smooth(method = "loess") +
  ggtitle(expression(atop("Scatterplot of Standardized Bachelors Degree Change and Income Change in Philadelphia Census Tracts", atop(italic("Between 2005-2009 ACS and 2013-2017 ACS"," "))))) +
  theme_tq()

z_philly_tbl_trends[-9] %>%
  boxplot()
