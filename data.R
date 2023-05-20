library(survival)
library(survminer)
library(SummarizedExperiment)
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)


load(file = "./tcgaLIHCdata_preprocessed.RData")

class(tcgaLIHCdata)
dim(tcgaLIHCdata)

gexp <- assay(tcgaLIHCdata)
rowAnnotation <- rowData(tcgaLIHCdata)
colAnnotation <- colData(tcgaLIHCdata)

# Re-assign small groups in a new "ajcc_stage" varible ----

colAnnotation$ajcc_stage <- colAnnotation$ajcc_pathologic_tumor_stage
idx <- colAnnotation$ajcc_stage%in%c("Stage IIIA","Stage IIIB","Stage IIIC")
colAnnotation$ajcc_stage[idx] <- "Stage III"
idx <- colAnnotation$ajcc_stage%in%c("Stage IVA","Stage IVB")
colAnnotation$ajcc_stage[idx] <- "Stage IV"
idx <- colAnnotation$ajcc_stage%in%c("[Discrepancy]")
colAnnotation$ajcc_stage[idx] <- NA
table(colAnnotation$ajcc_stage, useNA="ifany")



rownames(gexp) <- rowAnnotation$SYMBOL




# Filter the 'gexp' matrix using correlation with "Tumor_Stage" variable ----

idx <- cor(t(gexp), colAnnotation$Tumor_Stage, method = "spearman", 
           use="complete.obs")
idx <- sort.list(abs(idx), decreasing = T)[1:100]
gexp_filt <- gexp[idx,]

# Get sample annotations, and remove NAs, Filter the 'colAnnotation', removing NAs ----

colAnnotation_filt <- colAnnotation[,c("Tumor_Stage"), drop=F]
colAnnotation_filt <- colAnnotation_filt[complete.cases(colAnnotation_filt),, drop=F]
gexp_filt <- gexp_filt[ ,rownames(colAnnotation_filt)]

# Re-scale the data using a column wise rank transformation ----

x <- gexp_filt
x <- t(apply(x, 1, rank))
x <- x/max(x)
x <- t(scale(t(x), center = TRUE, scale = F))

# Run semi- or unsupervised clustering analysis ----

# Set col annotations
colAnnotation_filt$Tumor_Stage <- as.factor(colAnnotation_filt$Tumor_Stage)

pal1 <- brewer.pal(4,"Set1")
names(pal1) <- levels(colAnnotation_filt$Tumor_Stage)
top_annotation <- columnAnnotation(df=colAnnotation_filt, 
                                   col=list('Tumor_Stage'=pal1))

# Set a color scheme
pal2 <- rev(brewer.pal(7,"RdYlBu"))
bks <- quantile(as.numeric(x), probs = seq(0,1, length.out = length(pal2)))
colors <- colorRamp2(breaks = bks, colors = pal2)

