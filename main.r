#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#library(ggplot2)
#library(dplyr)

# default values variables
distanceTREGtoTCells = 100.0
inputFilename = 'data/TMA33AIP1_2D.csv'

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  #stop("At least one argument must be supplied (input file).csv", call.=FALSE)
  # HACK!!! to make it work in Replit.com
	distanceTREGtoTCells = 100.0
	inputFilename = 'data/TMA33AIP1_2D.csv'
	
} else if (length(args)==1) {
  # set the passed in 
  inputFilename = args[1]
  distanceTREGtoTCells = 100

} else if (length(args)==2) {
  # set the passed in 
  inputFilename = args[1]
  distanceTREGtoTCells = strtoi(args[2])

}

cat("\ninputFilename:", inputFilename,"\n")
cat("max distance TREG to TCells:", distanceTREGtoTCells,"\n\n")

# Import the data 
locationData <- read.csv(file = inputFilename)

# checking column names (optional)
#str("Phenotype")
#str("Cell.ID")
#str("Cell.X.Position")
#str("Cell.Y.Position")

#refining data structure
locationDataSmall<-locationData[ ,c("Cell.ID", "Phenotype","Cell.X.Position","Cell.Y.Position")]
#print(head(locationDataSmall))

#filter out the TC Phenotype and the TREG Phenotype
TClocationData <- subset(locationDataSmall, Phenotype=="TC")
TREGlocationData <- subset(locationDataSmall, Phenotype=="TREG")
#print(head(TClocationData))
cat("TC count:", nrow(TClocationData),"\n")
#print(head(TREGlocationData))
cat("TREG count:", nrow(TREGlocationData),"\n\n")

# #Test temp
# str(TREGlocationData)
# print(TREGlocationData[1,]) # print a row
# print(TREGlocationData[1,1]) # row 1 and column 1
# print(TREGlocationData[2,1]) # row 2 and column 1
# print(TREGlocationData[1,2]) # row 1 and column 2

# Add a TC count column
TREGlocationData[["TCcount"]] <- 0 
#print(TREGlocationData)

# # TEST
# # Set row 3's TCcount to 28
# TREGlocationData[["TCcount"]][3] <- 28
# print(TREGlocationData)
# # Test incrementing row 3's TCcount
# TREGlocationData[["TCcount"]][3] <- TREGlocationData[["TCcount"]][3] + 1 
# print(TREGlocationData)
# # Trying to get only the two XY fields - I didn't get it to work
# print(TREGlocationData[3,3-4])

#set up the big distance matrix
distanceMatrix <- matrix (, nrow(TREGlocationData), nrow(TClocationData))
#label rows (TREG cells) and columns (T Cells) with the Cell.IDs 
dimnames(distanceMatrix) <- list(TREGlocationData[["Cell.ID"]], TClocationData[["Cell.ID"]])

# set up outer loop
for (TREGidx in 1:nrow(TREGlocationData)) {

	TREGx <- TREGlocationData[["Cell.X.Position"]][TREGidx]
	TREGy <- TREGlocationData[["Cell.Y.Position"]][TREGidx]	
	
	# set up inner loop
	for (TCidx in 1:nrow(TClocationData)) {
		# calculate distance to two points
		tempDist <- sqrt((TClocationData[["Cell.X.Position"]][TCidx] - TREGx)^2 + (TClocationData[["Cell.Y.Position"]][TCidx] - TREGy)^2)	
		# update the distance matrix
		distanceMatrix[TREGidx, TCidx] <- tempDist
		
		# check if distance is less than distanceTREGtoTCells
		if (tempDist < distanceTREGtoTCells) {
			#  THEN increment the current TREG index's count by 1
			TREGlocationData[["TCcount"]][TREGidx] <- TREGlocationData[["TCcount"]][TREGidx] + 1 
		}

	}
}
print(TREGlocationData)
write.csv(distanceMatrix, file = sprintf('%s.out.csv', inputFilename))

# this is at temp thing
#print(distanceMatrix)

