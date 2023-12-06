library(readr)
library(dplyr)
X2017S1_NB_FER <- read_delim("2017-2022/2017S1_NB_FER.txt",  delim = "\t", escape_double = FALSE, trim_ws = TRUE)

X2017_S2_NB_FER <- read_delim("2017-2022/2017_S2_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2018_S1_NB_FER <- read_delim("2017-2022/2018_S1_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2018_S2_NB_Fer <- read_delim("2017-2022/2018_S2_NB_Fer.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2019_S1_NB_FER <- read_delim("2017-2022/2019_S1_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2019_S2_NB_FER <- read_delim("2017-2022/2019_S2_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2020_S1_NB_FER <- read_delim("2017-2022/2020_S1_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2020_S2_NB_FER <- read_delim("2017-2022/2020_S2_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2021_S1_NB_FER <- read_delim("2017-2022/2021_S1_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2021_S2_NB_FER <- read_delim("2017-2022/2021_S2_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2022_S1_NB_FER <- read_delim("2017-2022/2022_S1_NB_FER.txt", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

X2022_S2_NB_FER <- read_delim("2017-2022/2022_S2_NB_FER.txt", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)



data_frames_list <- list(
  X2017S1_NB_FER, X2017_S2_NB_FER, X2018_S1_NB_FER, X2018_S2_NB_Fer,
  X2019_S1_NB_FER, X2019_S2_NB_FER, X2020_S1_NB_FER, X2020_S2_NB_FER,
  X2021_S1_NB_FER, X2021_S2_NB_FER, X2022_S1_NB_FER, X2022_S2_NB_FER
)

# We are checking which columns has more than one datatype across all files.

new_df <- data.frame(matrix(ncol = length(colnames(data_frames_list[[1]])), nrow = 0)) 
colnames(new_df) <- colnames(data_frames_list[[1]])
for (df in data_frames_list) {
  column_types <- sapply(df, typeof)
  column_types_df <- as.data.frame(t(column_types))
  colnames(column_types_df) <- colnames(df)
  new_df <- rbind(new_df, column_types_df)
}

unique_values <- lapply(new_df, unique)

print(unique_values)



columns_to_convert <- c("CODE_STIF_RES", "CODE_STIF_ARRET", "NB_VALD")

for (df in data_frames_list) {
  # Convert specified columns to double
  df[columns_to_convert] <- lapply(df[columns_to_convert], as.double)
  for (column_name in columns_to_convert) {
    na_rows <- which(is.na(df[[column_name]]))
    print(na_rows)
  }
}
