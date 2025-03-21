library(ggplot2)
library(ggrepel)
library(cowplot)
library(plspm)

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")

input <- function(inputfile) {
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
    pfix = prefix()
  if (length(pfix) != 0) {
     pfix <- paste(pfix, "/", sep="")
  }

mydata <- read.csv(paste(pfix, parameters["csvfile", 2], sep="/"), check.names=FALSE)
mydata <- as.data.frame(mydata)
numeric_vars <- readLines(paste(pfix, parameters["features", 2], sep="/"))

mydata_num <<- mydata[, numeric_vars]
mydata_num$classification <<- "normal"

parameters_cats <<- read.table(paste(pfix, parameters["categories", 2], sep="/"), as.is=T);
cats <- c()
for (i in 1:nrow(parameters_cats)) {
   cat_samples <- readLines(paste(pfix, parameters_cats[i,2], sep="/"))
   cats <- c(cats, cat_samples)
   mydata_num$classification[match(cat_samples, rownames(mydata_num))] <<- parameters_cats[i,1]
}

mydata_num_sub <<- match(
  cats,
  rownames(mydata_num)
)

mydata_num$label <<- ""
mydata_num$label[mydata_num_sub] <<- rownames(mydata_num)[mydata_num_sub]

#thecols <<- readLines(paste(pfix, parameters["columns", 2], sep="/"))
}

run <- function() {}

output <- function(outputfile) {
dist_cereals_num <- dist(mydata_num[, 1:as.integer(parameters["variables", 2])], method="canberra")
mds_cereals_num <- cmdscale(dist_cereals_num, eig=TRUE, k=as.integer(parameters["k", 2]))
mds_cereals_num <- data.frame(
			      MDS1 = mds_cereals_num$points[,1],
			      MDS2 = mds_cereals_num$points[,2],
			      label = mydata_num$label,
			      classifications = mydata_num$classification
			      )
yy <<- ggplot(mds_cereals_num, aes(x=MDS1, y=MDS2, label=label, col=mydata_num$classification)) + 
	geom_point() #+
	#geom_text_repl(cex = as.numeric(parameters["cex", 2]))
print(str(yy))
write.csv(yy$label, paste(outputfile, "csv", sep="."))
ggsave(outputfile)
}
