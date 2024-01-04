library(dplyr)
library(lubridate)

final_df <- read.csv("final.csv", sep = ";", header = TRUE)

final_df <- final_df %>%
  mutate(JOUR = as.Date(JOUR, format = "%Y-%m-%d"), 
         WeekDay = weekdays(JOUR),
         Day = format(JOUR, "%d"),
         DM=format(JOUR, "%d-%m"),
         Month = format(JOUR, "%m"),
         Year = format(JOUR, "%Y"))

summer_2017_start <- dmy("08/07/2017")
summer_2017_end <- dmy("30/08/2017")
halloween_2017_start <- dmy("21/10/2017")
halloween_2017_end <- dmy("05/11/2017")
christmas_2017_start <- dmy("23/12/2017")
christmas_2017_end <- dmy("07/01/2018")

winter_2018_start <- dmy("18/02/2018")
winter_2018_end <- dmy("04/03/2018")
spring_2018_start <- dmy("15/04/2018")
spring_2018_end <- dmy("29/04/2018")
summer_2018_start <- dmy("08/07/2018")
summer_2018_end <- dmy("02/09/2018")
halloween_2018_start <- dmy("21/10/2018")
halloween_2018_end <- dmy("04/11/2018")
christmas_2018_start <- dmy("23/12/2018")
christmas_2018_end <- dmy("06/01/2019")

winter_2019_start <- dmy("24/02/2019")
winter_2019_end <- dmy("10/03/2019")
spring_2019_start <- dmy("21/04/2019")
spring_2019_end <- dmy("05/05/2019")
summer_2019_start <- dmy("07/07/2019")
summer_2019_end <- dmy("07/09/2019")
halloween_2019_start <- dmy("20/10/2019")
halloween_2019_end <- dmy("03/12/2019")
christmas_2019_start <- dmy("22/12/2019")
christmas_2019_end <- dmy("05/01/2020")

winter_2020_start <- dmy("09/02/2020")
winter_2020_end <- dmy("23/02/2020")
spring_2020_start <- dmy("05/04/2020")
spring_2020_end <- dmy("19/04/2020")
summer_2020_start <- dmy("05/07/2020")
summer_2020_end <- dmy("31/08/2020")
halloween_2020_start <- dmy("18/10/2020")
halloween_2020_end <- dmy("01/11/2020")
christmas_2020_start <- dmy("21/12/2020")
christmas_2020_end <- dmy("03/01/2021")

winter_2021_start <- dmy("14/02/2021")
winter_2021_end <- dmy("28/02/2021")
spring_2021_start <- dmy("11/04/2021")
spring_2021_end <- dmy("25/04/2021")
summer_2021_start <- dmy("07/07/2021")
summer_2021_end <- dmy("01/09/2021")
halloween_2021_start <- dmy("24/10/2021")
halloween_2021_end <- dmy("07/11/2021")
christmas_2021_start <- dmy("19/12/2021")
christmas_2021_end <- dmy("02/01/2022")

winter_2022_start <- dmy("20/02/2022")
winter_2022_end <- dmy("06/03/2022")
spring_2022_start <- dmy("24/04/2022")
spring_2022_end <- dmy("08/05/2022")
summer_2022_start <- dmy("08/07/2022")
summer_2022_end <- dmy("31/08/2022")
halloween_2022_start <- dmy("23/10/2022")
halloween_2022_end <- dmy("06/11/2022")
christmas_2022_start <- dmy("18/12/2022")
christmas_2022_end <- dmy("02/01/2023")

winter_2023_start <- dmy("19/02/2023")
winter_2023_end <- dmy("05/03/2023")
spring_2023_start <- dmy("23/04/2023")
spring_2023_end <- dmy("08/05/2023")




# Create the Holiday column
final_df <- final_df %>%
  mutate(Holiday = ifelse((JOUR >= summer_2017_start & JOUR <= summer_2017_end) |
                            (JOUR >= halloween_2017_start & JOUR <= halloween_2017_end) |
                            (JOUR >= christmas_2017_start & JOUR <= christmas_2017_end) |
                            (JOUR >= winter_2018_start & JOUR <= winter_2018_end) |
                            (JOUR >= spring_2018_start & JOUR <= spring_2018_end) |
                            (JOUR >= summer_2018_start & JOUR <= summer_2018_end) |
                            (JOUR >= halloween_2018_start & JOUR <= halloween_2018_end) |
                            (JOUR >= christmas_2018_start & JOUR <= christmas_2018_end) |
                            (JOUR >= winter_2019_start & JOUR <= winter_2019_end) |
                            (JOUR >= spring_2019_start & JOUR <= spring_2019_end) |
                            (JOUR >= summer_2019_start & JOUR <= summer_2019_end) |
                            (JOUR >= halloween_2019_start & JOUR <= halloween_2019_end) |
                            (JOUR >= christmas_2019_start & JOUR <= christmas_2019_end) |
                            (JOUR >= winter_2020_start & JOUR <= winter_2020_end) |
                            (JOUR >= spring_2020_start & JOUR <= spring_2020_end) |
                            (JOUR >= summer_2020_start & JOUR <= summer_2020_end) |
                            (JOUR >= halloween_2020_start & JOUR <= halloween_2020_end) |
                            (JOUR >= christmas_2020_start & JOUR <= christmas_2020_end) |
                            (JOUR >= winter_2021_start & JOUR <= winter_2021_end) |
                            (JOUR >= spring_2021_start & JOUR <= spring_2021_end) |
                            (JOUR >= summer_2021_start & JOUR <= summer_2021_end) |
                            (JOUR >= halloween_2021_start & JOUR <= halloween_2021_end) |
                            (JOUR >= christmas_2021_start & JOUR <= christmas_2021_end) |
                            (JOUR >= winter_2022_start & JOUR <= winter_2022_end) |
                            (JOUR >= spring_2022_start & JOUR <= spring_2022_end) |
                            (JOUR >= summer_2022_start & JOUR <= summer_2022_end) |
                            (JOUR >= halloween_2022_start & JOUR <= halloween_2022_end) |
                            (JOUR >= christmas_2022_start & JOUR <= christmas_2022_end) |
                            (JOUR >= winter_2023_start & JOUR <= winter_2023_end) |
                            (JOUR >= spring_2023_start & JOUR <= spring_2023_end)
                          , TRUE, FALSE))

#Represents a df with only non-holiday data
non_holiday_df <- final_df %>%
  filter(Holiday == FALSE) 
#Represents a df with only holiday data  
  holiday_df <- final_df %>%
  filter(Holiday == TRUE) 

  library(dplyr)
  library(tidyr)
  library(ggplot2)

non_holiday_avg <-non_holiday_df %>%
group_by(WeekDay) %>%
summarise(non_holiday_Avg_NB_vald = mean(NB_VALD, na.rm = TRUE))

holiday_avg <- holiday_df %>%
group_by(WeekDay) %>%
summarise(holiday_Avg_NB_vald = mean(NB_VALD, na.rm = TRUE))

# Join the two data frames
joined_df <- full_join(non_holiday_avg, holiday_avg, by = "WeekDay")

# Rename the columns for clarity
names(joined_df) <- c("WeekDay", "Avg_NB_vald_NonHoliday", "Avg_NB_vald_Holiday")

long_df <- joined_df %>%
  pivot_longer(cols = starts_with("Avg_NB_vald"), names_to = "Type", values_to = "Avg_NB_vald")

# Create a barplot
#For each day of the week, compares how many avg_nb_vald for holiday and non-holiday 
ggplot(long_df, aes(x = WeekDay, y = Avg_NB_vald, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Week Day", y = "Average NB_vald", fill = "Type") + theme(legend.position = "bottom") +
  theme_minimal()



christmas_df <- holiday_df %>%
  filter(Holiday == ifelse((JOUR >= christmas_2017_start & JOUR <= christmas_2017_end) |
                             (JOUR >= christmas_2018_start & JOUR <= christmas_2018_end) |
                             (JOUR >= christmas_2019_start & JOUR <= christmas_2019_end) |
                             (JOUR >= christmas_2020_start & JOUR <= christmas_2020_end) |
                             (JOUR >= christmas_2021_start & JOUR <= christmas_2021_end) |
                             (JOUR >= christmas_2022_start & JOUR <= christmas_2022_end), 
                           TRUE, FALSE))


spring_break_df <- holiday_df %>%
  filter(Holiday == ifelse(                             (JOUR >= spring_2018_start & JOUR <= spring_2018_end) |
                            
                             (JOUR >= spring_2019_start & JOUR <= spring_2019_end) |
                          
                             (JOUR >= spring_2020_start & JOUR <= spring_2020_end) |
                            
                             (JOUR >= spring_2021_start & JOUR <= spring_2021_end) |
                            
                             (JOUR >= spring_2022_start & JOUR <= spring_2022_end) |
                             
                             (JOUR >= spring_2023_start & JOUR <= spring_2023_end), 
                           TRUE, FALSE))


christmas_df_avg <-christmas_df %>%
  group_by(Year) %>%
  summarise(non_holiday_Avg_NB_vald = mean(NB_VALD, na.rm = TRUE))

spring_break_df_avg <-spring_break_df %>%
  group_by(Year) %>%
  summarise(non_holiday_Avg_NB_vald = mean(NB_VALD, na.rm = TRUE))

# Create a barplot
#Average NB_vald on christmas on each Year
ggplot(christmas_df_avg, aes(x = Year, y = non_holiday_Avg_NB_vald)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Year", y = "Average NB_vald on christmas on each Year") +
  theme_minimal()
#Average NB_vald on christmas on each Year
ggplot(spring_break_df_avg, aes(x = Year, y = non_holiday_Avg_NB_vald)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Year", y = "Average NB_vald on spring on each Year") +
  theme_minimal()

byYearMonth <-final_df %>%
  group_by(Year,Month) %>%
  summarise(Avg_NB_vald = mean(NB_VALD, na.rm = TRUE))


names(byYearMonth) <- c("YEAR", "MONTH", "AVG_NB_VALD")
# Reshape the data to long format
# Create a barplot
# Create a barplot with more space between bars and better colors
ggplot(byYearMonth, aes(x = MONTH, y = AVG_NB_VALD, fill = YEAR)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.5) +
  scale_fill_grey(start = 0, end = 0.7) +
  labs(x = "Month", y = "Average NB_vald", fill = "Year") +
  theme_minimal()


# For each year, shows for each month the avg of NB
ggplot(byYearMonth, aes(x = MONTH, y = AVG_NB_VALD, fill = YEAR)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Month", y = "Average NB_vald", fill = "Year") +
  theme_minimal()


most_arrets_used_holidays <- holiday_df |> group_by(LIBELLE_ARRET) |> summarise(total_nb_vald = sum(NB_VALD)) |> arrange(desc(total_nb_vald)) |> head(5)
most_arrets_used_non_holidays <- non_holiday_df |> group_by(LIBELLE_ARRET) |> summarise(total_nb_vald = sum(NB_VALD)) |> arrange(desc(total_nb_vald)) |> head(5)
#Stations most used in Holidays
ggplot(most_arrets_used_holidays, aes(x = LIBELLE_ARRET, y = total_nb_vald)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Stations most used in Holidays",x = "Arrêt", y = "Total NB_vald") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
#Stations most used in Non-Holidays
ggplot(most_arrets_used_non_holidays, aes(x = LIBELLE_ARRET, y = total_nb_vald)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Stations most used in Non-Holidays",x = "Arrêt", y = "Total NB_vald") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


christmasdays <-christmas_df %>%
  group_by(Day)  %>% 
  summarise(Avg_NB_vald = mean(NB_VALD, na.rm = TRUE))
christmasdays$Day <- as.numeric(christmasdays$Day)
christmasdays$Day <- factor(christmasdays$Day, levels = c(18:31, 1:7))


# Average NB_vald on christmas days
ggplot(christmasdays, aes(x = Day, y = Avg_NB_vald)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Day", y = "Average NB_vald on christmas days") +
  theme_minimal()

spring_breakdays <- spring_break_df %>%
  group_by(DM) %>%
  summarise(Avg_NB_vald = mean(NB_VALD, na.rm = TRUE))
#Average NB_vald on spring days
ggplot(spring_breakdays, aes(x = DM, y = Avg_NB_vald)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Day", y = "Average NB_vald on spring days") +
  theme_minimal()



