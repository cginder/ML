## TA Code Suggestions for Q1 of HW 3

#1. How to write a loop over columns of a data.frame (say, X here)?

for (i in names(X)){
  CurrentColumn=X[[i]]
  CurrentColumnVariableName=i
  #Then you do the computation on CurrentColumn, using function is.na, and save the result
  cat(i, mean(is.na(CurrentColumn)),'\n')
}

#2. How to drop columns of a data.frame indexed by a list?

ExcludeVars=c('Var1','Var2','Var100') #for example
idx=!(names(X) %in% ExcludeVars);
XS=X[,!(names(X) %in% ExcludeVars),with=FALSE]

#3. How to convert missing values into factors?

i="Var208" #for example

CurrentColumn=XS[[i]]                    #Extraction of column
idx=is.na(CurrentColumn)                 #Locate the NAs
CurrentColumn=as.character(CurrentColumn)#Convert from factor to characters
CurrentColumn[idx]=paste(i,'_NA',sep="") #Add the new NA level strings
CurrentColumn=as.factor(CurrentColumn)   #Convert back to factors
XS[[i]]=CurrentColumn                    #Plug-back to the data.frame

#4. How to aggregate a number of factors into new factors?
Thres_Low=249;
Thres_Medium=499;
Thres_High=999;
i="Var220" #for example, this one has 4291 levels
CurrentColumn=XS[[i]]                    #Extraction of column

CurrentColumn_Table=table(CurrentColumn) #Tabulate the frequency
levels(CurrentColumn)[CurrentColumn_Table<=Thres_Low]=paste(i,'_Low',sep="")

CurrentColumn_Table=table(CurrentColumn) #Tabulate the new frequency 
levels(CurrentColumn)[CurrentColumn_Table>Thres_Low & CurrentColumn_Table<=Thres_Medium ]=paste(i,'_Medium',sep="")

CurrentColumn_Table=table(CurrentColumn) #Tabulate the new frequency
levels(CurrentColumn)[CurrentColumn_Table>Thres_Medium & CurrentColumn_Table<=Thres_High ]=paste(i,'_High',sep="")

XS[[i]]=CurrentColumn                    #Plug-back to the data.frame
#We got 9 levels after cleaning



