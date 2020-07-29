# This R file measures the causality measure in Dufour and Taamouti (2010) using BigVAR package.

# Your input data should include the vectors for the VAR model.


rm(list=ls())


library('readxl')
library('writexl')
library('xlsx')
library('BigVAR')
library('readxl')
library('vars')
library('R.matlab')



Df <- read_excel("XXXXXX", 
                   sheet = "XXXX", col_names = FALSE)


#convert the dataframe to matrix
Groups <- as.matrix(Df)


# number of groups
G <- 16

# number of months (T)
m <- 12

#number of forecasts if h=1
t <- 11

#number of autoregressive
p <- 1

#gran
gran <- c(50,10) 


#construct the first row matrices (from 1 to 2 , ..., 11)
#first list all the groups
list_of_groups = list()
list_of_unrestricted_resid = list()
list_of_restricted_resid = list()
list_of_restr_sigmas = list()


################################################################################################################################
# here we construct the restricted models' residuals

for (l in 1:G){
  restricted_model = list_of_groups[[l]]
  Number_of_returns = dim(restricted_model)[2]
  
  restricted_varModel <- constructModel(restricted_model, p, struct="Basic", gran, RVAR = FALSE, h = 1, cv = "Rolling",
                                        MN = FALSE, verbose = TRUE, IC = TRUE, VARX = list(),
                                        T1 = floor(nrow(restricted_model)/3), T2 = floor(2 * nrow(restricted_model)/3), ONESE = FALSE,
                                        ownlambdas = FALSE, alpha = as.double(NULL), recursive = FALSE,
                                        C = as.double(NULL), dates = as.character(NULL), intercept = TRUE,
                                        tol = 1e-04, lagselect = FALSE, window.size = 0)
  results=cv.BigVAR(restricted_varModel) # Performs cross validation to select penalty parameters
  restricted_resid = results@resids
  
  
  list_of_restricted_resid[[l]]=restricted_resid
  
}


#now we can get restricted residuals
for (j in 1:G){
  rs_rsd = list_of_restricted_resid[[j]]
  init_rs_rsd = rs_rsd[1,] %*% t(rs_rsd[1,])
  for (i in 1:(m-2)) {
    init_rs_rsd = init_rs_rsd + rs_rsd[(i+1),] %*% t(rs_rsd[(i+1),])
    
    final_rs_rsd = 1/(m-1) * init_rs_rsd
    
  }
  list_of_restr_sigmas[[j]] = final_rs_rsd
}
#############################contsructing the causality measure####################################

# the measures is stored in "GC_from_list". each element is from causality
#from example GC_from_list[[3]]  is the 3rd row in causality table
GC_from_list <- list()
for (i in 1:G) {
  fromlist <- list()
  
  GC_from <- c()
  
  from_group <- list_of_groups[[i]]
  #remove the chosen group from the list and call it "list_of_groups_withoutFromGroup"
  list_of_groups_withoutFromGroup <- list_of_groups
  list_of_groups_withoutFromGroup[[i]] <- NULL
  
  
  #remove the chosen group from the list of sigmas and call it "list_of_groups_withoutFromGroup"
  new_list_restr_sigma_withoutFromGroup <- list_of_restr_sigmas
  new_list_restr_sigma_withoutFromGroup[[i]] <- NULL
  
  #contsruct a list of unrestricted models
  
  for (j in 1:(G-1)) {
    to_group <- list_of_groups_withoutFromGroup[[j]]
    unrt_model = cbind(to_group, from_group)
    fromlist[[j]] = unrt_model 
  }
  
  for (l in 1:(G-1)) {
    unrestricted_Model = fromlist[[l]]
    unrestricted_varModel <- constructModel(unrestricted_Model, p, struct="Basic", gran, RVAR = FALSE, h = 1, cv = "Rolling",
                                            MN = FALSE, verbose = TRUE, IC = TRUE, VARX = list(),
                                            T1 = floor(nrow(G1)/3), T2 = floor(2 * nrow(G1)/3), ONESE = FALSE,
                                            ownlambdas = FALSE, alpha = as.double(NULL), recursive = FALSE,
                                            C = as.double(NULL), dates = as.character(NULL), intercept = TRUE,
                                            tol = 1e-04, lagselect = FALSE, window.size = 0)
    results=cv.BigVAR(unrestricted_varModel) # Performs cross validation to select penalty parameters
    un_rsd = results@resids
    
    # write sigma here first
    
    init = un_rsd[1,] %*% t(un_rsd[1,])
    for (k in 1:(m-2)) {
      init = init + un_rsd[(k+1),] %*% t(un_rsd[(k+1),])
    }
    final_un_rsd = 1/(m-1) * init
    
    ncl_rw = dim(unrestricted_Model)[2]-dim(from_group)[2]
    sigma_denom_from = final_un_rsd[1:ncl_rw, 1:ncl_rw]
    CG_noLog = det(new_list_restr_sigma_withoutFromGroup[[l]])/det(sigma_denom_from)
    GC_from[l] = log(CG_noLog) 
    
    
  }
  GC_from_list[[i]] =  GC_from
  rm(from_group, list_of_groups_withoutFromGroup, new_list_restr_sigma_withoutFromGroup, to_group, unrestricted_varModel, final_un_rsd, CG_noLog, GC_from)
}

CG_TABLE <- matrix(,G,(G-1))
for (s in 1:G){
  CG_row = GC_from_list[[s]]
  CG_TABLE[s,] = CG_row
}


write.xlsx(CG_TABLE, file="CG_TABLES.xlsx", sheetName="XXXX", row.names=FALSE)
