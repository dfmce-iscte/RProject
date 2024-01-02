library(readr)
library(dplyr)

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
  X2017_S2_NB_FER, X2018_S1_NB_FER, X2018_S2_NB_Fer,
  X2019_S1_NB_FER, X2019_S2_NB_FER, X2020_S1_NB_FER, X2020_S2_NB_FER,
  X2021_S1_NB_FER, X2021_S2_NB_FER, X2022_S1_NB_FER, X2022_S2_NB_FER
)

data_frames_list_names <- list(
  "X2017_S2_NB_FER", "X2018_S1_NB_FER", "X2018_S2_NB_Fer",
  "X2019_S1_NB_FER", "X2019_S2_NB_FER", "X2020_S1_NB_FER", "X2020_S2_NB_FER",
  "X2021_S1_NB_FER", "X2021_S2_NB_FER", "X2022_S1_NB_FER", "X2022_S2_NB_FER"
)

# Remove the rows with the value "Inconnu" in the column LIBELLE_ARRET
data_frames_list <- lapply(data_frames_list, function(df) {
  subset(df, LIBELLE_ARRET != "Inconnu")
})

# Convert the columns CODE_STIF_RES and CODE_STIF_ARRET to numeric
data_frames_list <- lapply(data_frames_list, function(df) {
  df$CODE_STIF_RES <- as.numeric(df$CODE_STIF_RES)
  df$CODE_STIF_ARRET <- as.numeric(df$CODE_STIF_ARRET)
  return(df)
})

# Change the name of the column lda to ID_REFA_LDA in the dataframes that have this column
data_frames_list <- lapply(data_frames_list, function(df) {
  if ("lda" %in% names(df)) {
    df <- rename(df, ID_REFA_LDA = lda)
  }
  return(df)
})

combined_df <- bind_rows(data_frames_list)

combined_df <- mutate(combined_df, CATEGORIE_TITRE = ifelse(CATEGORIE_TITRE == "?", "NON DEFINI", CATEGORIE_TITRE))

write.csv(combined_df, "combined_df.csv", row.names = FALSE)

#-----------------------------------------#
# The following code is to get the columns specific columns above

# # We are checking which columns has more than one datatype across all files.
# new_df <- data.frame(matrix(ncol = length(colnames(data_frames_list[[1]])), nrow = 0)) 
# colnames(new_df) <- colnames(data_frames_list[[1]])
# for (df in data_frames_list) {
#   column_types <- sapply(df, typeof)
#   column_types_df <- as.data.frame(t(column_types))
#   colnames(column_types_df) <- colnames(df)
#   new_df <- rbind(new_df, column_types_df)
# }

# unique_values <- lapply(new_df, unique)

# print(unique_values)

# columns_to_convert <- c("CODE_STIF_RES", "CODE_STIF_ARRET")

# # check the character values in a column
# for (i in seq_along(data_frames_list)) {
#   print(data_frames_list_names[[i]])
#   col <- data_frames_list[[i]][, "CODE_STIF_RES"]
  
#   # Select only the values that are of type character
#   char_values <- col[sapply(col, is.character)]
#   print(char_values)
# }
