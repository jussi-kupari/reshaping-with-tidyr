library(readr)


read_delim(
  "import-data/data/swiss.csv",
  col_types = cols(
    Fertility = "_",
    Examination = "i",
    Education = "i",
    Catholic = "c"
  )
)

read_delim(
  "import-data/data/swiss.csv",
  col_select = c("Agriculture", "Examination", "Education"),
  col_types = cols(
    Agriculture = "d",
    Examination = "i",
    Education = "i")
)

read_delim(
  "import-data/data/swiss.csv",
  show_col_types = FALSE,
  col_select = Education:Catholic,
  n_max = 3
)

read_delim("import-data/data/swiss.csv",)

read_delim("import-data/data/swiss.csv", col_names = FALSE, show_col_types = FALSE)

read_delim(
  "https://basv53.uni.lu/lectures/data/example.csv", 
  col_names = c("animal", "color", "value"), 
  col_select = -color,
  col_types = cols(value = "i")
  )


# Continue in "Skipping lines"