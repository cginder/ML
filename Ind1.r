# Personal HW 1
require("forcats") # package to handle categorical variables


## Load Data Sets
df.test= read.csv("HousingTest.csv",head=TRUE,sep=",")
df.train = read.csv("HousingTrain.csv",head=TRUE,sep=",")

## Data Cleansing, Normalization, and Standardization
tres.low <- 20 #threshold to combine values


combine.low <- function(CurrentColumn,ColumnName){
  CurrentColumn_Table <- table(CurrentColumn)
  if(length(levels(CurrentColumn)[CurrentColumn_Table <= tres.low])<=1){
    CurrentColumn
  }
  else{
    levels(CurrentColumn)[CurrentColumn_Table <= tres.low] = paste(ColumnName,"_Low", sep = "")
  CurrentColumn
  }
}

missing.cont <- function(CurrentColumn){
  CurrentColumn[is.na(CurrentColumn)] <- mean(CurrentColumn[!is.na(CurrentColumn)])
  # CurrentColumn[is.na(CurrentColumn)] <- median(CurrentColumn[!is.na(CurrentColumn)])
  CurrentColumn

}

# MS.SubClass
i <- "MS.SubClass"
df.train[[i]] <- as.factor(df.train[[i]])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)


# MS.Zoning
i <- "MS.Zoning"
df.train[[i]] <- as.factor(df.train[[i]])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)


# Lot Frontage
i <- "Lot.Frontage"
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- missing.cont(df.train[[i]])
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

# Lot Area
i <- "Lot.Area"
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- missing.cont(df.train[[i]])
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

# Street
i <- "Street"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])

# Alley
i <- "Alley"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
levels(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Alley"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Alley"

# Lot Shape
i <- "Lot.Shape"
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(4,1:3)])
table(df.train[[i]])

# Land Contour
i <- "Land.Contour"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)


# Utilities -- Should Drop This
i <- "Utilities"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Lot Config
i <- "Lot.Config"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Land Slope
i <- "Land.Slope"
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)
table(df.train[[i]])

# Neighborhood
i <- "Neighborhood"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Condition 1
i <- "Condition.1"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Condition 2
i <- "Condition.2"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# BldgType
i <- "Bldg.Type"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# House Style 
i <- "House.Style"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Overall Qual --- Could consider splitting this (<3, 3-7, >7)
i <- "Overall.Qual"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
table(df.train[[i]])

# Year Built -- numeric
i <- "Year.Built"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
#df.train[[i]] <- as.ordered(df.train[[i]])
#table(df.train[[i]])

# Year Remodel/Add -- Ordinal for now
i <- "Year.Remod.Add"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
#df.train[[i]] <- as.ordered(df.train[[i]])
#table(df.train[[i]])

# Roof Style
i <- "Roof.Style"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Roof Material
i <- "Roof.Mat1"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Exterior 1
i <- "Exterior.1st"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Exterior 2
i <- "Exterior.2nd"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Masonry Veneer Type 
i <- "Mas.Vnr.Type"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]][df.train[[i]] == ""] <- "None"
df.train[[i]] <- droplevels.factor(df.train[[i]])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Masonry Veneer Area
i <- "Mas.Vnr.Area"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]][is.na(df.train[[i]])] <- 0.0
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

# Exter.Qual ordinal
i <- "Exter.Qual"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,4,2)])
table(df.train[[i]])

# Exter.Cond ordinal
i <- "Exter.Cond"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4)])
table(df.train[[i]])

# Foundation
i <- "Foundation"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Basement Qual
i <- "Bsmt.Qual"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Bsmt"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Bsmt"
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4,6)])
table(df.train[[i]])

# Bsmt.Cond
i <- "Bsmt.Cond"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Bsmt"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Bsmt"
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4,6)])
table(df.train[[i]])

# Bsmt Expose
i <- "Bsmt.Exposure"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Bsmt"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Bsmt"
df.train[[i]][df.train[[i]] == ""] <- "No"
table(df.train[[i]])
df.train[[i]] <- droplevels(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(2,1,3,4,5)])
table(df.train[[i]])

# Bsmt Finish
i <- "BsmtFin.Type.1"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Bsmt"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Bsmt"
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(3,1,2,5,4,6,7)])
table(df.train[[i]])

# Bsmt Finish SF
i <- "BsmtFin.SF.1"   
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

# Bsmt Finish Type 2
i <- "BsmtFin.Type.2"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Bsmt"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Bsmt"
df.train[[i]][df.train[[i]] == ""] <- "Unf"
table(df.train[[i]])
df.train[[i]] <- droplevels(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(3,1,2,5,4,6,7)])
table(df.train[[i]])

## Bsmt Fin SF 2
i <-   "BsmtFin.SF.2"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

## Bsmt Unf SF
i <-     "Bsmt.Unf.SF"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

# Total Bsmt SF
i <-      "Total.Bsmt.SF"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
hist(df.train[[i]])

# Heating
i <-  "Heating" 
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Heating Quality
i <-   "Heating.QC"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4)])
table(df.train[[i]])

# Central Air
i <-       "Central.Air"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])

# Electrical
i <-      "Electrical"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(5,1,2,3,4)])
table(df.train[[i]])

# 1st Flr SF
i <- "X1st.Flr.SF"  
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

# 2nd Flr SF
i <-  "X2nd.Flr.SF"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
hist(df.train[[i]]) # log transform not really needed

# Low Quality Finished SF
i <-    "Low.Qual.Fin.SF" 
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

# Above Ground SF
i <- "Gr.Liv.Area"    
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

# bath/bed/room counts -- keeping all the same
i <- "Bsmt.Full.Bath" 
i <- "Bsmt.Half.Bath" 
i <-  "Full.Bath" 
i <-   "Half.Bath"
i <- "Bedroom.AbvGr"
i <- "Kitchen.AbvGr"  

# Kitchen Quality
i <-  "Kitchen.Qual"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4)])
table(df.train[[i]])

# rooms above ground
i <-   "TotRms.AbvGrd"  
hist(df.train[[i]])

# Functionality
i <-  "Functional"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(8,3,4,5,1,2,7,6)])
table(df.train[[i]])

# fireplaces
i <-     "Fireplaces" 
hist(df.train[[i]])

# fireplace quality
i <-      "Fireplace.Qu"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Fireplace"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Fireplace"
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4,6)])
table(df.train[[i]])

# garage type
i <-     "Garage.Type"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Garage"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Garage"
df.train[[i]] <- combine.low(df.train[[i]],i)

# Garage Build Year
i <- "Garage.Yr.Blt"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Garage"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Garage"
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(101,1:100)]) 
df.train[[i]][df.train[[i]] == "2207"] <- "2007" # one garage w/ 2207 as year
df.train[[i]] <- droplevels(df.train[[i]])
table(df.train[[i]])
df.train[[i]] <- as.numeric(df.train[[i]])


# garage finish
i <- "Garage.Finish"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Garage"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Garage"
df.train[[i]][df.train[[i]] == ""] <- "No.Garage"
df.train[[i]] <- droplevels(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4,6)])
table(df.train[[i]])

# Garage Capacity
i <- "Garage.Cars"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]][is.na(df.train[[i]])] <- 0
hist(df.train[[i]])

# Garage Area
i <- "Garage.Area"
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]][is.na(df.train[[i]])] <- 0
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)


i <- "Garage.Qual"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Garage"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Garage"
df.train[[i]][df.train[[i]] == ""] <- "No.Garage"
df.train[[i]] <- droplevels(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4,6)])
table(df.train[[i]])


i <- "Garage.Cond"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Garage"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Garage"
df.train[[i]][df.train[[i]] == ""] <- "No.Garage"
df.train[[i]] <- droplevels(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,5,2,4,6)])
table(df.train[[i]])

i <- "Paved.Drive"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(3,2,1)])
table(df.train[[i]])

i <- "Wood.Deck.SF"   
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]][is.na(df.train[[i]])] <- 0
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

i <- "Open.Porch.SF"  
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]][is.na(df.train[[i]])] <- 0
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]]+1)

i <-  "Enclosed.Porch"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <-   "X3Ssn.Porch"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <-      "Screen.Porch"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <- "Pool.Area"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <-      "Pool.QC" 
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Pool"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Pool"
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <-         "Fence"
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Fence"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Fence"
table(df.train[[i]])
df.train[[i]] <- as.ordered(df.train[[i]])
df.train[[i]] <- factor(df.train[[i]],levels(df.train[[i]])[c(1,3,2,4,5)])

i <-            "Misc.Feature"   
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]] <- factor(df.train[[i]], levels=c(levels(df.train[[i]]), "No.Features"))
df.train[[i]][is.na(df.train[[i]])] <- "No.Features"
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <- "Misc.Val" 
df.train[[i]] <- as.numeric(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
df.train[[i]][is.na(df.train[[i]])] <- 0
hist(df.train[[i]])
# keep this, add it on to predicted price


i <-        "Mo.Sold" 
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])

i <-         "Yr.Sold"  
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])

i <-        "Sale.Type"      
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <- "Sale.Condition" 
df.train[[i]] <- as.factor(df.train[[i]])
length(df.train[[i]][is.na(df.train[[i]])])
table(df.train[[i]])
df.train[[i]] <- combine.low(df.train[[i]],i)

i <-  "SalePrice" 
hist(df.train[[i]])
df.train[[i]] <- log(df.train[[i]])
