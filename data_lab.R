library(SummarizedExperiment)
library(ggplot2)

load(file = "./tcgaLIHCdata_preprocessed.RData")

class(tcgaLIHCdata)
dim(tcgaLIHCdata)

gexp <- assay(tcgaLIHCdata)
rowAnnotation <- rowData(tcgaLIHCdata)
colAnnotation <- colData(tcgaLIHCdata)

names(colAnnotation)

unique(colAnnotation$OS)
unique(colAnnotation$PFI)


unique(colAnnotation$Age)

colAnnotation[, c("OS", "PFI")]

head(colAnnotation)

unique(colAnnotation$gender)

females <- colAnnotation[colAnnotation$gender == 'FEMALE',]

dim(females)
dim(colAnnotation)

gender_counts <- table(colAnnotation$gender)
df_gender_counts <- data.frame(gender_counts)
head(df_gender_counts)

names(df_gender_counts) <- c("Gender", "Freq")

# Create a pie chart using ggplot2
ggplot(df_gender_counts, aes(x = "Gender", y = Freq, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(fill = "Gender") +
  theme_void() +
  theme(legend.position = "bottom")

colAnnotation$vital_tatus <- ifelse(colAnnotation$OS == 1, "Deceased", "Alive")
table(colAnnotation$vital_tatus)
OS_counts <- table(colAnnotation$OS)
df_os_counts <- data.frame(OS_counts)
names(df_os_counts) <- "OS"
