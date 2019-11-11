Full_data = read.csv("Final_DAta.csv")
library(plyr)


# 
# sub_data = Full_data[,c("Serial.Number","New.Part.Description")]
# sub_data = na.omit(sub_data)
# 
# # staten = unique(sub_data$CRM_STATE_PROVINCE)
# 
# state = list()
# 
# df = ddply(sub_data, .(sub_data$Serial.Number, sub_data$`New.Part.Description`), nrow)
# df = df[1:10000,]
# 
# for(i in unique(df$`sub_data$Serial.Number`)){
#   
#   print(paste0("state_",i))
#   
#   Subset_data <- df[grep(i, df$`sub_data$Serial.Number`), ]
#   names(Subset_data) = c("Branch", "Part", "V1")
#   subset_data1 = Subset_data[grep("Mainboard", Subset_data$Part), ]
#   if(dim(Subset_data)[1]==0){
#     dsds = data.frame(Branch = i, Part = c("Mainboard"), Sum = c(0), percent = c(0))
#   }else{
#     subset_data1$percent = round((subset_data1$V1/sum(Subset_data$V1))*100,2)
#     if(dim(subset_data1)[1]==0){
#       dsds = data.frame(Branch = unique(Subset_data$Branch), Part = c("Mainboard"), Sum = c(0), percent = c(0))
#     }else{
#       dsds = data.frame(Branch = unique(subset_data1$Branch), Part = c("Mainboard"), Sum = sum(subset_data1$V1), percent = sum(subset_data1$percent))
#     }
#   }
#   
#   state[[i]] = dsds
# }
# 
# final = do.call(rbind, state)

mainboard = function(Full_data){
  
  Subset_data <- Full_data[grep("Mainboard", Full_data$`New.Part.Description`), ]
  
  branch = ddply(Subset_data, .(Subset_data$`Product.Line`, Subset_data$`New.Part.Description`), nrow)
  names(branch) = c("Product_Line", "New Part Description", "Sum")
  
  branch1 = ddply(branch, .(branch$Product_Line), summarise, Sum = sum(Sum))
  
  branch1$percentage = paste0(round((branch1$Sum/sum(branch1$Sum))*100,4), " %")
  branch1 = data.frame(branch1)
  percentage = branch1$percentage
  branch2 = data.frame(branch1[,c(1,2)], percentage)
  
}

mb = mainboard(Full_data)

library(lubridate)
library(dplyr)

warranty = function(Full_data){
  
  dates = data.frame(Full_data$Serial.Number,Full_data$`Case.Created.Date`, Full_data$WarrantyExpiryDate)
  names(dates) = c('Serial.Number', 'Case Created Date', 'WarrantyExpiryDate')
  
  dates$sum = as.Date(as.character(dates$WarrantyExpiryDate), format = '%d-%m-%Y')-
    dmy(as.character(dates$`Case Created Date`))
  
  zero = dates%>%filter(sum<0)
  thirty = dates%>%filter(sum>=0 & sum<30)
  thirty_sixty = dates%>%filter(sum>=30 & sum<60)
  sixty_ninety = dates%>%filter(sum>=60 & sum<90)
  morethan_ninety = dates%>%filter(sum>=90)
  
  zero1 = ddply(zero,.(zero$Serial.Number), nrow)
  thirty1 = ddply(thirty,.(thirty$Serial.Number), nrow)
  thirty_sixty1 = ddply(thirty_sixty,.(thirty_sixty$Serial.Number),nrow)
  sixty_ninety1 = ddply(sixty_ninety,.(sixty_ninety$Serial.Number),nrow)
  morethan_ninety1 = ddply(morethan_ninety,.(morethan_ninety$Serial.Number),nrow)
  names(zero1) = c("Branch", "Values")
  names(thirty1) = c("Branch", "Values")
  names(thirty_sixty1) = c("Branch", "Values")
  names(sixty_ninety1) = c("Branch", "Values")
  names(morethan_ninety1) = c("Branch", "Values")
  
  merged_final = merge(morethan_ninety1,thirty1, by = "Branch",all = TRUE)
  merged_final2 = merge(merged_final,thirty_sixty1, by = "Branch",all = TRUE)
  merged_final3 = merge(merged_final2,sixty_ninety1, by = "Branch",all = TRUE)
  merged_final4 = merge(merged_final3, zero1, by = "Branch",all = TRUE)
  
  names(merged_final4) = c("Branch", "Percen_Morethan_ninety", "Percen_Zero_Thirty", "Percen_Thirty_Sixty","Percen_Sixty_Ninety", "Percen_Beyond_Warranty")
  
  # merged_final4 = datatable(
  #   merged_final4, colnames = c("Branch", "% of Morethan_ninety", "% of Zero_Thirty", "% of Thirty_Sixty","% of Sixty_Ninety", "% of Beyond_Warranty")
  # )
  
  
  final_merged = merged_final4[,c(1,6,3,4,5,2)]
  final_merged[is.na(final_merged)] <- 0
  
  final_merged$sums = rowSums(final_merged[,2:6, drop = FALSE])
  
  final1 = round((final_merged[,2:6]/final_merged[,7])*100,2)
  # final1$Below_Thirty = paste0(final1$Below_Thirty, " %")
  # final1$Thirty_Sixty = paste0(final1$Thirty_Sixty, " %")
  # final1$Sixty_Ninety = paste0(final1$Sixty_Ninety, " %")
  # final1$Morethan_ninety = paste0(final1$Morethan_ninety, " %")
  
  final2 = data.frame(final_merged$Branch, final1, final_merged$sums)
  names(final2)[1] = c("Serial_number")
  names(final2)[7] = c("Sums")
  final2

}



Warranty = warranty(Full_data)
