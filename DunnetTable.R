
library(multcomp)
require(multcomp)
library(xtable)

data<-read.csv("13-0365Data.dat", stringsAsFactors = TRUE)
#remove unwanted columns 1,2, 3, 4, 7
data <- data[, -c(1:4,7)]

#this will make the stacked bar graph change to side by side bargraphs
data$Group<-as.factor(data$Group)

#Subset by time returns a list of dataframes. Each timepoint is a list

df_list <- split(data, as.factor(data$Time))
length(df_list)

colnames(df_list[[1]])


# new_names <- as.character((unique(data$Time)))
# new_names
# data_time <-list()
# for (i in 1:length(data)){
#   assign(paste0("Time",new_names[i], data_split[[i]]))
# }
#data_split[[1]]

# Creates data frames for each time point
# times <- unique(data$Time)
# data_time <- lapply(times, function(x){
#                 split(data,times)
# })
# data_time <- as.data.frame(data_time)
# head(data_time[[2]])
# length(data_time[[1]]$Time)

df_list[[3]]$Time


unique(df_list[[1]]$Time)

#Fit anova for each time and it's used to set interactions for Dunnet's test
fit <- aov (CD4 ~ Group, df_list[[1]])
coef(fit)
#print(xtable(fit),type="html")
#create contingency matrix for Dunnett test
K <- diag(length(coef(fit)))[-1,]
K
rownames(K) <- names(coef(fit))[-1]
rownames(K)
length(coef(fit))
#assign names to columns according to groups
colnames(K) <- names(coef(fit))
K
#assign names for dunnets
rownames(K) <- c( "Group 2 - Group 1","Group 3 - Group 1", "Group 4 - Group 1" , "Group 5 - Group 1")
rownames(K)
dunnett <- glht(fit, linfct = K)
summary(dunnett)
fit


#Test for loop for variables fitting Dunnett
# #only some variables. Works for 1 time only
# 
# calculate_pvalues <- function(data,varlist){
#                     df_list <- split(data, as.factor(data$Time))
#                     lapply(df_list,)
#                      mod2 <-lapply(varlist, function(x){
#                       fm <- eval(substitute(i ~ Group, list (i = as.name(x))))
#                       cat("The timepoint is",paste0("",unique(df_list[[i]]$Time),"\n"))
#                       cat("The variable is", as.name(x),"\n")
#                       results <- summary(glht((aov(fm, data = df_list[[i]])), linfct = K))
#                     })
#   }
# results
# test <-calculate_pvalues(data,varlist)
# test
#  values <- function(data,varlist){
#               df_list <- split(data, as.factor(data$Time))
#               for (i in seq_along(df_list)){
#                   anova <- function(data,varlist){
#                   fm <- eval(substitute(i ~ Group, list (i = as.name(x))))
#                   cat("The timepoint is",paste0("",unique(df_list[[i]]$Time),"\n"))
#                   cat("The variable is", as.name(x),"\n")
#                   results <- summary(glht((aov(fm, data = df_list[[i]])), linfct = K)) 
#                 }
#                 anova(data,varlist)
#               }
#                 
#  time_groups <- function(x){
#                   df_list <- split(x, as.factor(x$Time))
#                   #return(df_list)
#  }   
#  df_list2 <- split(data, as.factor(data$Time))
#  unique(df_list2[[1]]$Time)
#  df_list2 <- time_groups(data)  
#  df_list
# rm(df_list2)
varlist <- names(data)[3:5]

#Calcualtes Dunnets for each time. Each time must be entered in function:1,2,3.... 
mod <-lapply(varlist, function(x){
        fm <- eval(substitute(i ~ Group, list (i = as.name(x))))
        cat("The timepoint is",paste0("",unique(df_list[[1]]$Time),"\n"))
        cat("The variable is", as.name(x),"\n")
        # print(aov(fm, data = df_list[[1]]))
        print(summary(glht((aov(fm, data = df_list[[1]])), linfct = K)))
        #print((glht((aov(fm, data = df_list[[1]])), linfct = K))$test$pvalues)
         })
summary(mod[[3]])

#Extracts pvalues for all the variables 
mod[[1]]$test$pvalues
str(mod)
timeslist <- unique(data$Time)
timeslist
pvalues_times <- list()

for( i in seq_along(varlist)){
  pvalues_times[[i]] <-mod[[i]]$test$pvalues
  cat(paste0("",pvalues_times[[i]] ,"\n"))
}

# Extracts pvalues for each variable in a new matrix
p_group <- as.matrix(pvalues_times[[1]])
seq_along(pvalues_times)
p_group <- list()
 for(i in seq_along(pvalues_times)){
  p_group[[i]] <- as.matrix(pvalues_times[[i]])
}
p_group
#pvalues in col
# total <- do.call(rbind, lapply(p_group, unlist))
# total
p_group[[1]]
#this orders p values in as many columns as variables, and rows as comparisons
output <- matrix(unlist(p_group), ncol = 3, byrow = FALSE)
output
#Name col accorind to the var name, and row as dunnets comparison
colnames(output) <- varlist
rownames(output) <- rownames(K)

#save file with time included in file name
output <- write.csv(output, file = (paste0(unique(df_list[[1]]$Time),"testsDunnett.csv", sep = "")), col.names = TRUE, row.names = TRUE )
df_list[[1]]$Time
xtable(mod[[1]], type="html")

