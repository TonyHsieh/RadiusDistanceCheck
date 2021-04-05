#library(ggplot2)
#library(dplyr)

# variables
distanceTREGtoTCells = 100

# Import the data and look at the first six rows
locationData <- read.csv(file = 'data/TMA33AIP1_2D.csv')

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
cat("TREG count:", nrow(TREGlocationData),"\n")

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
print(distanceMatrix)

 