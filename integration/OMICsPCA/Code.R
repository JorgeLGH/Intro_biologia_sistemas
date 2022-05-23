## ----include = TRUE, echo = FALSE, message = FALSE, warning = FALSE-----------
library(OMICsPCA)
library(OMICsPCAdata)
library(kableExtra)
library(rmarkdown)
library(knitr)
library(magick)
library(rgl)
library(grDevices)
library(MultiAssayExperiment)


## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#",
  eval = TRUE
)


## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(fig.pos = 'H')

## ----echo = FALSE-------------------------------------------------------------

Cell1 <- read.delim(
  system.file("extdata/factors2/demofactor/Cell1.bed",
              package = "OMICsPCAdata"),
  header=FALSE, sep = "\t")

names(Cell1) <- c("chromosome", "start","end","intensity")

annotation <- read.delim(
  system.file("extdata/annotation2/TSS_groups.bed",
              package = "OMICsPCAdata"),
  header=FALSE, sep = "\t")

names(annotation) <- c("chromosome", "start","end","TSS ID") 

TSS_list <- read.delim(
  system.file("extdata/annotation2/TSS_list",
              package = "OMICsPCAdata"),
  header=FALSE, sep = "\t")

names(TSS_list)[1] <- c("TSS ID") 


## ---- eval = TRUE, echo = FALSE, result = 'asis'------------------------------
#for html

knitr::kable(Cell1[1:5,], 
             caption = "Intensity of demo peaks",
             align = 'c') %>%  kable_styling("bordered",full_width = TRUE, position = "left")

## ---- eval = TRUE, echo = FALSE, result = 'asis'------------------------------
#for html

knitr::kable(annotation[1:5, 1:4], caption = "demo annotation file(`annofile`)",
             align = 'c') %>% kable_styling("bordered",full_width = TRUE,
                                            position = "left")

## ---- eval = TRUE, echo = FALSE, result = 'asis'------------------------------
#for html

knitr::kable(TSS_list[1:5,],
             caption = "demo TSSs (`annolist`)",
             align = 'c', col.names = "TSS ID") %>% 
  kable_styling("bordered",full_width = TRUE, position = "left")

## ----echo = TRUE--------------------------------------------------------------
anno <- system.file("extdata/annotation2/TSS_groups.bed",
                    package = "OMICsPCAdata")

list <- system.file("extdata/annotation2/TSS_list",
                    package = "OMICsPCAdata")

fact <- system.file("extdata/factors2/demofactor",
                    package = "OMICsPCAdata")


list.files(path = fact)

## ----eval = TRUE--------------------------------------------------------------

all_cells <- prepare_dataset(factdir = fact,
                             annofile = anno, annolist = list)

head(all_cells[c(1,14,15,16,20),])


## ----echo = TRUE--------------------------------------------------------------

library(MultiAssayExperiment)

datalist <- data(package = "OMICsPCAdata")
datanames <- datalist$results[,3]
data(list = datanames)

assaylist <- list("demofactor"  = all_cells)

demoMultiAssay <- MultiAssayExperiment(experiments = assaylist)



## ----echo = TRUE--------------------------------------------------------------
groupinfo <- create_group(name = multi_assay, group_names = c("WE" ,
                                                              "RE" ,"NE" ,"IntE"),
                          grouping_factor = "CAGE",
                          comparison = c(">=" ,"%in%" ,"==" ,"%in%"),
                          condition = c("25" ,"1:5" ,"0" ,"6:24"))

## ---- eval = TRUE, echo = FALSE, result = 'asis'------------------------------
#for html
knitr::kable(assays(multi_assay)[["CAGE"]][1:5,1:5], align = 'c') %>% 
  kable_styling("bordered",full_width = TRUE, position = "center")

## ---- eval = TRUE, echo = FALSE, result = 'asis'------------------------------
#for html

knitr::kable(groupinfo_ext[1:5,, drop = FALSE], align = 'c') %>% 
  kable_styling("bordered",full_width = TRUE, position = "center")

## -----------------------------------------------------------------------------
descriptor(name = multi_assay,
           factors = c(
             "H2az",
             "H3k4me1","H3k9ac"),
           groups = c("WE","RE"),
           choice = 1,
           title = "Distribution of percentage of cell types overlapping
with various factors",
           groupinfo = groupinfo)

## -----------------------------------------------------------------------------
descriptor(name = multi_assay,
           factors = c("H2az",
                       "H3k4me1","H3k9ac"),
           groups = c("WE","RE"),
           choice = 2,
           choice2group = "WE",
           title = "Distribution of percentage of cell types overlapping with
a factor in various combinations of epigenetic marks in the
selected group",
           groupinfo = groupinfo)


## ---- eval = TRUE,echo=FALSE, results='asis'----------------------------------
#for html
data <- data.frame(choice = c("table","scatter","hist", "all"),
                   functions = c(
                     "cor {stats}",
                     "pairs {graphics}",
                     "ggplot,geom_histogram,facet_wrap{ggplot2}",
                     "chart.Correlation {PerformanceAnalytics}"),
                   output = c("correlation table","scatterplot of each pair",
                              "histogram of each column","all of the above together"))

knitr::kable(data, caption = "Summary of choices", align = 'c') %>% 
  kable_styling("bordered",full_width = FALSE, position = "center")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
groups <- c("WE")
chart_correlation(name = multi_assay, Assay = "H2az",
                  groups = "WE", choice = "scatter", groupinfo = groupinfo)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
chart_correlation(name = multi_assay,
                  Assay = "H2az",
                  groups = "WE", choice = "table",
                  groupinfo = groupinfo)

## -----------------------------------------------------------------------------
chart_correlation(name = multi_assay, Assay = "H2az",
                  groups = "WE", choice = "hist", bins = 10,
                  groupinfo = groupinfo)

## -----------------------------------------------------------------------------
chart_correlation(name = multi_assay, Assay = "H2az",
                  groups = "WE", choice = "all",
                  groupinfo = groupinfo)

## ----eval = TRUE--------------------------------------------------------------
PCAlist <- integrate_variables(Assays = c("H2az","H3k4me1",
                                          "H3k9ac"), name = multi_assay,
                               groups = c("WE","RE", "NE", "IntE"), groupinfo = groupinfo,
                               scale.unit = FALSE, graph = FALSE)

## ----echo =FALSE--------------------------------------------------------------
choice_table <- data.frame(choice = 1:6, graphical_output = c(
  "Barplot of variance explained by each principal component (PC)
  or dimension",
  "Loadings (in terms of cos2, contrib or coord
  supplied through the argument var_type) of columns/variables
  (cell lines in this example) on PC1 and PC2",
  # Each Cell line(variable) has a loading on each
  #principal components. 
  #Loadings = Eigenvectorsâ‹…sqrt(Eigenvalues).
  "Matrix plot of correlations of each column/variable
  (Cell line in this example) with each PC",
  "Barplot of squarred loadings (or cos2) of each column/variable 
  (cell line in this example) on the PC/dimension of choice",
  "The contributions of each column/variable (cell line in
  this example) in accounting for the variability in a given
  PC/dimension. The contribution (in percentage) is calculated
  as : squared loading of the variable 
  (e.g. a cell line) * 100 / total squared loading of
  the given PC/dimension",
  "Variance explained by each of the
  first few PCs together with barplot explaining total 
  variance explained by the displayed PCs in each assay"
),

console_output = c(
  "Table of eigen values and corresponding variance",
  "Table of loadings in terms of coord, cos2 or contrib as
    supplied through  the argument var_type",
  "Table of correlations of variables (Cell lines) with 
    PCs/Dimensions",
  "Table of squarred loadings/cos2 of
    each variable (Cell line) on the PCs/ Dimensions",
  "Table of contribution of each variable on the selected
    PC/Dimension",
  "None"
),

function_used = c(
  "fviz_eig",
  "fviz_pca_var",
  "corrplot",
  "fviz_cos2",
  "fviz_contrib",
  "ggplot, plot_grid"
),

package = c(
  "factoextra",
  "factoextra",
  "factoextra and corrplot",
  "factoextra",
  "factoextra",
  "ggplot2, cowplot"
),

additional_arguments = c(
  "addlabels",
  "var_type",
  "is.corr",
  "PC",
  "PC",
  "various graphical arguments to ggplot2 and cowplot functions"
)
)

names(choice_table) <-  c("choice", "graphical\noutput","console\noutput",
                          "function\nused", "package", "additional\narguments")

library(kableExtra)

## ---- echo=FALSE, results='asis'----------------------------------------------
# for html
knitr::kable(choice_table, caption = "available choices", align = 'c') %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  column_spec(1, width = "1em") %>%
  column_spec(2, width = "18em") %>%
  column_spec(3, width = "8em") %>%
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "6em") %>%
  column_spec(6, width = "5em")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_variables(name = PCAlist, Assay = "H2az", choice = 1,
                  title = "variance barplot", addlabels = TRUE)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_variables(name = PCAlist, Assay = "H3k4me1",choice = 2,
                  title = "Loadings of cell lines on PC1 and PC2", xlab = "PCs")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_variables(name = PCAlist, Assay = "H2az",
                  choice = 3,title = "Correlation matrix", xlab = "PCs")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_variables(name = PCAlist, Assay = "H2az",
                  choice = 4,PC = 1,
                  title = "Squarred loadings of Cell lines on PC1")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_variables(name = PCAlist, Assay = "H2az",
                  choice = 5,PC=1,
                  title = "Contribution of Cell lines on PC1")

## ----eval = TRUE--------------------------------------------------------------
analyse_variables(name = PCAlist,
                  Assay = c("H3k9ac","H2az",
                            "H3k4me1"), choice = 6)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_individuals(name = PCAlist,
                    Assay = "H3k9ac", groupinfo = groupinfo,
                    choice = 1, PC = c(1,2))

## -----------------------------------------------------------------------------

# selecting top 40 TSS groups according to their contribution on
# PC 1 and PC 2 using the argument "select.ind"

selected <- analyse_individuals(name = PCAlist,
                                Assay = "H3k4me1",
                                choice = 1, PC = c(1,2),
                                select.ind = list(contrib = 100),
                                groupinfo = groupinfo)

# plot selected individuals
plot(selected)

# extracted information of the selected individuals
head(selected$data)

## -----------------------------------------------------------------------------

# The TSSs used in this example (each row) are obtained by combining
# many neighboring TSSs together. The combinations should be uncombined
# to find the corresponding annotations.

names  <- gsub(";",",",paste(as.character(selected$data$name),
                             collapse = ","))

names <- as.vector(strsplit(names, ",", fixed = TRUE)[[1]])

## The top 100 combined TSS_groups actually come from 115 TSSs

length(names)

# retrieve details of top 100 individuals
# transcript details contains the GENCODE v 17
# annotation of TSSs.

details <- transcript_details[
  transcript_details$transcript_id %in% names,,drop = FALSE]

head(details)

## checking the gene type

table(details$gene_type)

## ----eval = FALSE-------------------------------------------------------------
#  
#  # find GO annotation
#  
#  library(clusterProfiler)
#  library(org.Hs.eg.db)
#  
#  gene <- details$gene_name
#  
#  gene.df <- bitr(gene, fromType = "SYMBOL",
#                  toType = c("ENSEMBL", "ENTREZID"),
#                  OrgDb = org.Hs.eg.db)
#  
#  ggo <- groupGO(gene     = unique(gene.df$ENTREZID),
#                 OrgDb    = org.Hs.eg.db,
#                 ont      = "MF",
#                 level    = 5,
#                 readable = TRUE)
#  
#  # If we want to see the top results, we need to reorder
#  #the list. So, the following line is strictly optional.
#  
#  #ggo@result <- ggo@result[order(-ggo@result$Count),]
#  #head(ggo@result)
#  
#  
#  # The barplot may not fit to the Rstudio window, therefore
#  # we may plot it in a different window
#  
#  #grDevices::windows()
#  #barplot(ggo, drop=TRUE, showCategory=20)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_individuals(name = PCAlist, Assay = "H3k9ac",
                    choice = 2, PC = c(1,2,3),
                    col = c("RED", "BLACK"), groupinfo = groupinfo)

## ----eval = TRUE--------------------------------------------------------------
#head(groupinfo)
densityplot <- plot_density(name = PCAlist,
                            Assay = "H2az", groupinfo = groupinfo,
                            PC = 1, groups = c("WE","RE","IntE"),
                            adjust = 1)

## ----eval = TRUE, echo =TRUE--------------------------------------------------
library(ggplot2)
library(graphics)

densityplot <- densityplot+xlim(as.numeric(c("-8", "23")))
densityplot

## ----eval = TRUE, echo = TRUE-------------------------------------------------
densityplot <- plot_density(name = PCAlist, Assay = "H3k4me1",
                            PC = 1, groups = c("WE" ,"RE"), adjust = 2,
                            groupinfo = groupinfo)

densityplot <- densityplot+xlim(as.numeric(c("-8", "15")))

print(densityplot)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
plot_density_3D(name = PCAlist, Assay = "H2az",
                group = "WE", PC1 = 1,PC2 = 2, grid_size = 100,
                groupinfo = groupinfo,
                static = FALSE)

## ----eval = TRUE, echo = TRUE-------------------------------------------------
plot_density_3D(name = PCAlist, Assay = "H2az", group = "WE",
                groupinfo = groupinfo,
                PC1 = 1,PC2 = 2, grid_size = 100, static = TRUE, theta = -50,
                phi = 40, box = TRUE, shade = 0.1, ticktype = "detailed", d = 10)

## ----eval = TRUE--------------------------------------------------------------
Assays = c("H2az", "H3k9ac")

## ----eval = TRUE--------------------------------------------------------------
exclude <- list(0,c(1,9))

## ----eval = TRUE--------------------------------------------------------------
int_PCA <- integrate_pca(Assays = c("H2az",
                                    "H3k9ac"),
                         groupinfo = groupinfo,
                         name = multi_assay, mergetype = 2,
                         exclude = exclude, graph = FALSE)

## -----------------------------------------------------------------------------

start_end = int_PCA$start_end

name = int_PCA$int_PCA

## ----echo = FALSE-------------------------------------------------------------
start_end = list(start = c(1, 6), end = c(5, 16))

## ----eval = TRUE--------------------------------------------------------------

analyse_integrated_variables(start_end = start_end, name = name,
                             choice = 1, title = "variance barplot", Assay = 1, addlabels = TRUE)

## ----eval = TRUE--------------------------------------------------------------
analyse_integrated_variables(start_end = start_end, Assay = 1,
                             name = name, choice = 2 ,
                             title = "Loadings of cell lines on PC1 and PC2",
                             var_type = "contrib")

## ----eval = TRUE--------------------------------------------------------------
analyse_integrated_variables(start_end = start_end, Assay = 1,
                             name = name, choice = 3,
                             title = "Correlation of Cell lines with PCs of integrated Assays",
                             is.cor = TRUE)

## ----eval = TRUE--------------------------------------------------------------
analyse_integrated_variables(start_end = start_end, Assay = 1,
                             name = name, choice = 4, PC = 1,
                             title = "Squarred loadings of Cell lines on PC1 of integrated Assays")

## ----eval = TRUE--------------------------------------------------------------
analyse_integrated_variables(start_end = start_end, Assay = 1,
                             name = name, choice = 5, PC=1,
                             title = "Contribution of Cell lines on PC1 of integrated Assays")

## ----eval = TRUE, echo = TRUE-------------------------------------------------
analyse_integrated_individuals(
  name = name, choice = 1, PC = c(1,2),
  groupinfo = groupinfo)

## ----eval = TRUE--------------------------------------------------------------
analyse_integrated_individuals(
  name = name,
  choice = 2, PC = c(1,2,3),
  col = c("RED", "BLACK","GREEN"),
  groupinfo = groupinfo)

## -----------------------------------------------------------------------------
densityplot <- plot_integrated_density(name = name, PC = 1,
                                       groups = c("WE","RE","IntE","NE"), groupinfo = groupinfo)

# additional graphical functions (e.g. xlim, ylim, theme) may be
#added with densityplot (see section VIII. Density analysis)

densityplot

## -----------------------------------------------------------------------------
plot_integrated_density_3D(name = name, PC1 = 1, PC2 = 2,
                           group = c("RE","RE"), gridsize = 100, static = TRUE,
                           groupinfo = groupinfo)

## -----------------------------------------------------------------------------
plot_integrated_density_3D(name = name, PC1 = 1, PC2 = 2,
                           group = c("WE","RE"), gridsize = 100, static = FALSE,
                           groupinfo = groupinfo)

## -----------------------------------------------------------------------------

# extracting data from integrated PCA
data <- extract(name = name, PC = c(1:4),
                groups = c("WE","RE"), integrated = TRUE, rand = 600,
                groupinfo = groupinfo)

#or

# extracting PCA data from an individual assay
# if integrated = TRUE, an assay name should be entered by 
# Assay == "assayname"

data <- extract(name = PCAlist, PC = c(1:4),
                groups = c("WE","RE"), integrated = FALSE, rand = 600,
                Assay = "H2az", groupinfo = groupinfo)

## ----eval = FALSE-------------------------------------------------------------
#  
#  #### Using "clValid" ####
#  
#  clusterstats <- cluster_parameters(name = data,
#  optimal = FALSE, n = 2:6, comparisonAlgorithm = "clValid",
#  distance = "euclidean", clusteringMethods = c("kmeans","pam"),
#  validationMethods = c("internal","stability"))
#  
#  #### plot indexes vs cluster numbers returned by clValid
#  #plot(clusterstats)
#  
#  
#  #### using "NbClust"
#  
#  data <- extract(name = name, PC = c(1:4),
#  groups = c("WE","RE"),integrated = TRUE, rand = 400,
#  groupinfo = groupinfo)
#  
#  clusterstats <- cluster_parameters(name = data, n = 2:6,
#  comparisonAlgorithm = "NbClust", distance = "euclidean",
#  clusteringMethods = "kmeans",
#  validationMethods = "all")
#  
#  library(factoextra)
#  
#  fviz_nbclust(clusterstats, method = c("silhouette",
#  "wss", "gap_stat"))

## -----------------------------------------------------------------------------

data <- extract(name = name, PC = c(1:4),
                groups = c("WE","RE"), integrated = TRUE, rand = 1000,
                groupinfo = groupinfo)

library(factoextra)

### kmeans clustering

clusters <- cluster(name = data, n = 2, choice = "kmeans",
                    title = "kmeans on 2 clusters")

# visualization of clusters

print(clusters$plot)

clustered_data <- cbind(data,clusters$cluster$cluster)
plot3d(data[,1:3], col = clusters$cluster$cluster)

### density-based clustering

clusters <- cluster(name = data, choice = "density",
                    eps = 2, MinPts = 100, graph = TRUE,
                    title = "eps = 1 and MinPts = 1.5")

# visualization of clusters
print(clusters$plot)

clustered_data <- as.data.frame(cbind(data,clusters$cluster$cluster))

#removing unclustered points
clustered_data <- clustered_data[clustered_data$V5 != 0, ]

library(rgl)
#plotting clusters on first 3 PCs
plot3d(clustered_data[,1:3], col = clustered_data$V5)


## -----------------------------------------------------------------------------

#### Using silhouette index
# calculating the distance matrix
library(cluster)
dist <- daisy(data)

sil = silhouette(clusters$cluster$cluster,dist)

# RStudio sometimes does not display silhouette plots
#correctly. So sometimes, it is required to plot in separate
#window.

#grDevices::windows()
#plot(sil)

## -----------------------------------------------------------------------------
# Identification of misclassified data points
# silhouette width of misclassified points are negative
misclassified <- which(sil[,3] < 0)

head(sil[misclassified,])

data[misclassified[3:7],]

## -----------------------------------------------------------------------------
bp <- cluster_boxplot(name = multi_assay,
                      Assay = "H2az", clusterobject = clustered_data,
                      clustercolumn = 5)


bp <- bp+xlab("Cell lines") + ylab(
  "value")+ggtitle(
    "Distribution of H2az_cell_wise in various clusters")+theme(
      plot.title=element_text(hjust=0.5),
      legend.position = "top")+guides(
        fill=guide_legend("Cell lines"))

bp

