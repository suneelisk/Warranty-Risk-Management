library(Rserve)
Rserve()

  

  
  Branch = Mainboard_Branch[,c(1,4)]
  Branch_serialno = Branch_MR[,6]
  Warranty = Warranty[,2]
  gfgf = data.frame(Branch, Branch_serialno, Warranty)
  names(gfgf) = c('Branch', "MB", "Morethan_10Times", "Beyond_Warranty")
  
  MB1 = mutate(gfgf, MB1 = case_when(MB<=3 ~ "Good",
                                     MB>3 & MB<=7  ~ 'Average',
                                     MB>7  ~ 'Bad')
  )
  
  Morethan_10Times1 = mutate(gfgf, Morethan_10Times1 = case_when(Morethan_10Times<=0.1 ~ "Good",
                                                                 Morethan_10Times>0.1 & Morethan_10Times<=0.2  ~ 'Average',
                                                                 Morethan_10Times>0.2  ~ 'Bad')
  )
  
  Below_Thirty1 = mutate(gfgf, Below_Thirty1 = case_when(Beyond_Warranty<=1 ~ "Good",
                                                         Beyond_Warranty>1 & Beyond_Warranty<=2  ~ 'Average',
                                                         Beyond_Warranty>2  ~ 'Bad')
  )
  
  
  
  gfgf1 = data.frame(gfgf, MB1[,5], Morethan_10Times1[,5], Below_Thirty1[,5])
  names(gfgf1)[5] = c("MB1")
  names(gfgf1)[6] = c("Morethan_10Times1")
  names(gfgf1)[7] = c('Below_Thirty1')
  gfgf1
  
  gfgf2 = gfgf1
  gfgf2$MB1 = ifelse(gfgf2$MB1 == 'Good',9,ifelse(gfgf2$MB1=='Average',5,0))
  gfgf2$Morethan_10Times1 = ifelse(gfgf2$Morethan_10Times1 == 'Good',9,ifelse(gfgf2$Morethan_10Times1=='Average',5,0))
  gfgf2$Below_Thirty1 = ifelse(gfgf2$Below_Thirty1 == 'Good',9,ifelse(gfgf2$Below_Thirty1=='Average',5,0))
  
  
  
  gfgf2$Compliance_Score = paste0(round((rowSums(gfgf2[,5:7])/27)*100,1)," %")
  
  final1 = data.frame(gfgf1, gfgf2[,8])
  names(final1)[8] = c("Compliance_Score")
  Summary_Branch = final1
  
  
  sub_data = Full_data[,c("Product.Line","New.Part.Description")]
  sub_data = na.omit(sub_data)
  
  # staten = unique(sub_data$CRM_STATE_PROVINCE)
  
  state1 = list()
  df = ddply(sub_data, .(sub_data$Product.Line, sub_data$`New.Part.Description`), nrow)
  
  for(i in unique(sub_data$Product.Line)){
    
    print(paste0("state_",i))
    
    
    #df[is.na(df$`sub_data$New.Part.Description`)] <- "ISK_PARt"
    Subset_data <- df[grep(i, df$`sub_data$Product.Line`), ]
    names(Subset_data) = c("Product.Line", "Part", "V1")
    subset_data1 = Subset_data[grep("Mainboard", Subset_data$Part), ]
    
    if(dim(Subset_data)[1]==0){
      dsds = data.frame(Product.Line = i, Part = c("Mainboard"), Sum = c(0), percent = c(0))
    }else{
      subset_data1$percent = round((subset_data1$V1/sum(Subset_data$V1))*100,2)
      if(dim(subset_data1)[1]==0){
        dsds = data.frame(Product.Line = unique(Subset_data$Product.Line), Part = c("Mainboard"), Sum = c(0), percent = c(0))
      }else{
        dsds = data.frame(Product.Line = unique(subset_data1$Product.Line), Part = c("Mainboard"), Sum = sum(subset_data1$V1), percent = sum(subset_data1$percent))
      }
    }
    
    state1[[i]] = dsds
  }
  
  Mainboard_PL = do.call(rbind, state1)
  
  Subset_data <- Full_data[grep("Mainboard", Full_data$`New.Part.Description`), ]
  chart_data = Subset_data[,c("Product.Line","Case.Created.Date", "New.Part.Description")]
  chart_data1 <- ddply(chart_data, .( chart_data$`Product.Line`, chart_data$`Case.Created.Date`), nrow)
  names(chart_data1)=c("ProductLine", "Date", "Frequency")
  chart_data1$Date = dmy(as.character(chart_data1$Date))
  chart_data1 = data.frame(chart_data1)
  chart_data1
  chart_data1$Year = substring(chart_data1$Date, 1,7)
  chart_data1$Day = substring(chart_data1$Date, 9,10)
  Monthlyview_PL = chart_data1
  
  sum1 = Full_data[,c("Product.Line", "Serial.Number")]
  
  library(plyr)
  ds <- ddply(sum1, .(sum1$`Product.Line`,sum1$`Serial.Number`), nrow)
  names(ds) = c("Product_Line", "Serial_number", "V1")
  
  
  ds1 = table(ds$Product_Line, ds$V1)
  ds1 = data.frame(ds1)
  
  ds1$Var2 = as.numeric(ds1$Var2)
  ones = ds1%>%filter(Var2 == 1)
  two_three = ds1%>%filter(Var2 >=2 & Var2<4)
  four_five = ds1%>%filter(Var2>=4 & Var2<6)
  six_ten = ds1%>%filter(Var2>=6 & Var2<11)
  morethanten = ds1%>%filter(Var2>=11)
  
  two_three1 = ddply(two_three,.(two_three$Var1), summarise, sum = sum(Freq))
  four_five1 = ddply(four_five,.(four_five$Var1), summarise, sum = sum(Freq))
  six_ten1 = ddply(six_ten,.(six_ten$Var1), summarise, sum = sum(Freq))
  morethanten1 = ddply(morethanten,.(morethanten$Var1), summarise, sum = sum(Freq))
  
  
  
  final = cbind(ones[,c(1,3)],two_three1[,2],four_five1[,2],six_ten1[,2],morethanten1[,2])
  names(final)= c("ProductLine", "Percen_Ones", "Percen_Two_Three", "Percen_Four_Five","Percen_Six_Ten", "Percen_Morethanten")
  final
  final$Sums = rowSums(final[,2:6, drop = FALSE])
  
  final1 = round((final[,2:6]/final[,7])*100,2)
  
  # final1$Percen_Ones = paste0(final1$Percen_Ones, " %")
  # final1$Percen_Two_Three = paste0(final1$Percen_Two_Three, " %")
  # final1$Percen_Four_Five = paste0(final1$Percen_Four_Five, " %")
  # final1$Percen_Six_Ten = paste0(final1$Percen_Six_Ten, " %")
  # final1$Percen_Morethanten = paste0(final1$Percen_Morethanten, " %")
  
  final1 = data.frame(final$ProductLine,final1,final$Sums)
  names(final1)[1] = c('Product Line')
  names(final1)[7] = c('Sums')
  PL_ML = final1
  
  dates = data.frame(Full_data$`Product.Line`,Full_data$`Case.Created.Date`, Full_data$WarrantyExpiryDate)
  names(dates) = c('Branch', 'Case Created Date', 'WarrantyExpiryDate')
  
  dates$sum = as.Date(as.character(dates$WarrantyExpiryDate), format = '%d-%m-%Y')-
    dmy(as.character(dates$`Case Created Date`))
  
  zero = dates%>%filter(sum<0)
  thirty = dates%>%filter(sum>=0 & sum<30)
  thirty_sixty = dates%>%filter(sum>=30 & sum<60)
  sixty_ninety = dates%>%filter(sum>=60 & sum<90)
  morethan_ninety = dates%>%filter(sum>=90)
  
  zero1 = ddply(zero,.(zero$Branch), nrow)
  thirty1 = ddply(thirty,.(thirty$Branch), nrow)
  thirty_sixty1 = ddply(thirty_sixty,.(thirty_sixty$Branch),nrow)
  sixty_ninety1 = ddply(sixty_ninety,.(sixty_ninety$Branch),nrow)
  morethan_ninety1 = ddply(morethan_ninety,.(morethan_ninety$Branch),nrow)
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
  
  # final1$Percen_Beyond_Warranty = paste0(final1$Percen_Beyond_Warranty, " %")
  # final1$Percen_Zero_Thirty = paste0(final1$Percen_Zero_Thirty, " %")
  # final1$Percen_Thirty_Sixty = paste0(final1$Percen_Thirty_Sixty, " %")
  # final1$Percen_Sixty_Ninety = paste0(final1$Percen_Sixty_Ninety, " %")
  # final1$Percen_Morethan_ninety = paste0(final1$Percen_Morethan_ninety, " %")
  
  final2 = data.frame(final_merged$Branch, final1, final_merged$sums)
  names(final2)[1] = c("Product Line")
  names(final2)[7] = c("Sums")
  Warranty_PL = final2
  
  Branch = Mainboard_PL[,c(1,4)]
  Branch_serialno = PL_ML[,c(6)]
  Warranty = Warranty_PL[,2]
  gfgf = data.frame(Branch, Branch_serialno, Warranty)
  names(gfgf) = c('Branch', "MB", "Morethan_10Times", "Beyond_Warranty")
  
  MB1 = mutate(gfgf, MB1 = case_when(MB<=0 ~ "Good",
                                     MB>0 & MB<=0.5  ~ 'Average',
                                     MB>0.5  ~ 'Bad')
  )
  
  Morethan_10Times1 = mutate(gfgf, Morethan_10Times1 = case_when(Morethan_10Times<=0 ~ "Good",
                                                                 Morethan_10Times>0 & Morethan_10Times<=0.5  ~ 'Average',
                                                                 Morethan_10Times>0.5  ~ 'Bad')
  )
  
  Below_Thirty1 = mutate(gfgf, Below_Thirty1 = case_when(Beyond_Warranty<=2 ~ "Bad",
                                                         Beyond_Warranty>2 & Beyond_Warranty<=4  ~ 'Average',
                                                         Beyond_Warranty>4  ~ 'Good')
  )
  
  
  
  gfgf1 = data.frame(gfgf, MB1[,5], Morethan_10Times1[,5], Below_Thirty1[,5])
  names(gfgf1)[5] = c("MB1")
  names(gfgf1)[6] = c("Morethan_10Times1")
  names(gfgf1)[7] = c('Below_Thirty1')
  gfgf1
  
  gfgf2 = gfgf1
  gfgf2$MB1 = ifelse(gfgf2$MB1 == 'Good',9,ifelse(gfgf2$MB1=='Average',5,0))
  gfgf2$Morethan_10Times1 = ifelse(gfgf2$Morethan_10Times1 == 'Good',9,ifelse(gfgf2$Morethan_10Times1=='Average',5,0))
  gfgf2$Below_Thirty1 = ifelse(gfgf2$Below_Thirty1 == 'Good',9,ifelse(gfgf2$Below_Thirty1=='Average',5,0))
  
  
  
  gfgf2$Compliance_Score = paste0(round((rowSums(gfgf2[,5:7])/27)*100,1)," %")
  
  final1 = data.frame(gfgf1, gfgf2[,8])
  names(final1)[8] = c("Compliance_Score")
  Summary_PL = final1

