library(ggplot2)
library(ggpubr)
library(dplyr)
library(psych)
library(strucchange)
library(forecast)
data<-read.csv("D:\\SEM 3\\Business Statistics Lab using R\\housedataprice.csv")
print(data)
city<-data$City
water<-data$Watersupply.in....
price<-data$Landvalue
electric<-data$Electricitybill.in....

while(TRUE){
  cat(paste("\n1.Using regression predict price rate of the city if water supply rate changes",'\n',"2.Using regression predict price rate of the city if electricity supply rate changes",'\n',"3.Using Anove choose the best city to stay based on WaterSupplyRate",'\n',"4.Correlation",'\n',"5.Mann Whitney Test to compare two cities land value ",'\n',"6.CUSUM",'\n',"7.TimeSeries for Forecasting next 12 months land value",'\n',"8.Exit"));
  choice<-readline(prompt="Enter your choice")
  choice<-as.integer(choice)
  if(choice==1){
    name<-readline(prompt="Enter the city name")
    name1<-toString(name)
    print(name1)
    val<-c()
    vall<-c()
    for(i in 1:nrow(data)){
      if(city[i]==name1){
        val<-append(val,water[i])
        vall<-append(vall,price[i])
      }
    }
    relation<-lm(vall~val)
    water<-as.numeric(readline(prompt="Enter water supply rate: "))
    az<-data.frame(val=water)
    result<-predict(relation,az)
    cat("Land value for the city ",result)
    
    
  }
  else if(choice==2){
    namee<-readline(prompt="Enter the city name")
    name1e<-toString(namee)
    print(name1e)
    vale<-c()
    valle<-c()
    for(i in 1:nrow(data)){
      if(city[i]==name1e){
        vale<-append(val,electric[i])
        valle<-append(vall,price[i])
      }
    }
    relatione<-lm(valle~vale)
    watere<-as.numeric(readline(prompt="Enter Electricity supply rate: "))
    a<-data.frame(vale=watere)
    resulte<-predict(relatione,a)
    print(resulte)
  }
  else if(choice==3){
    idd<-data$id
    uniq<-unique(idd,incomparables=FALSE)
    print(uniq)
    city1<-c()
    city2<-c()
    city3<-c()
    city4<-c()
    city5<-c()
    city6<-c()
    city7<-c()
    city8<-c()
    id<-data$id
    pric<-data$Watersupply.in....
    for(i in 1:nrow(data)){
      if(id[i]==uniq[1]){
        city1<-append(city1,pric[i])
      }
      else if(id[i]==uniq[2]){
        city2<-append(city2,pric[i])
      }
      else if(id[i]==uniq[3]){
        city3<-append(city3,pric[i])
      }
      else if(id[i]==uniq[4]){
        city4<-append(city4,pric[i])
      }
      else if(id[i]==uniq[5]){
        city5<-append(city5,pric[i])
      }
      else if(id[i]==uniq[6]){
        city6<-append(city6,pric[i])
      }
      else if(id[i]==uniq[7]){
        city7<-append(city7,pric[i])
      }
      else if(id[8]==uniq[8]){
        city8<-append(city8,pric[i])
      }
      
    }
    co<-length(city1)
    co1<-length(uniq)
    totals<-c()
    for(i in 1:co){
      b<-city1[i]+city2[i]+city3[i]+city4[i]+city5[i]+city6[i]+city7[i]+city8[i]
      totals<-append(totals,b)
    }
    totalssum<-sum(totals)
    fields<-co*co1
    CF<-(totalssum*totalssum)/fields
    print(paste("CF IS ",CF))
    totalss1<-0
    for(i in 1:co){
      as<-city1[i]*city1[i]
      totalss1<-totalss1+as
    }
    for(i in 1:co){
      as<-city2[i]*city2[i]
      totalss1<-totalss1+as
    }
    for(i in 1:co){
      as<-city3[i]*city3[i]
      totalss1<-totalss1+as
    }
    for(i in 1:co){
      as<-city4[i]*city4[i]
      totalss1<-totalss1+as
    }
    for(i in 1:co){
      as<-city5[i]*city5[i]
      totalss1<-totalss1+as
    }
    for(i in 1:co){
      as<-city6[i]*city6[i]
      totalss1<-totalss1+as
    }
    for(i in 1:co){
      as<-city7[i]*city7[i]
      totalss1<-totalss1+as
    }
    for(i in 1:co){
      as<-city8[i]*city8[i]
      totalss1<-totalss1+as
    }
    TSS<-totalss1-CF
    print(paste("TSS is",TSS))
    bsssum<-sum(city1)
    bsssum1<-sum(city2)
    bsssum2<-sum(city3)
    bsssum3<-sum(city4)
    bsssum4<-sum(city5)
    bsssum5<-sum(city6)
    bsssum6<-sum(city7)
    bsssum7<-sum(city8)
    BSS<-(((bsssum*bsssum)+(bsssum1*bsssum1)+(bsssum2*bsssum2)+(bsssum3*bsssum3)+(bsssum4*bsssum4)+(bsssum5*bsssum5)+(bsssum6*bsssum6)+(bsssum7*bsssum7))/12)-CF
    print(paste("BSS IS",BSS))
    
    ESS<-TSS-BSS
    print(paste("ESS IS",ESS))
    DFBS<-length(uniq)-1
    print(paste("DFBS IS",DFBS))
    DFWS<-fields-length(uniq)
    print(paste("DFWS IS",DFWS))
    MSSBS<-BSS/DFBS
    
    print(paste("MSSBS IS",MSSBS))
    MSSWS<-ESS/DFWS
    print(paste("MSSWS IS",MSSWS))
    FVAL<-MSSBS/MSSWS
    FVAL1<-round(FVAL,digits=1)
    print(FVAL1)
    F_tab<-qf(0.01,ncol(data)-2,(ncol(data)-1)*(ncol(data)-1),lower.tail=TRUE)
    
    if(FVAL>F_tab){
      print(" Conclusion : \n\tWe reject null hypothesis that the treatment means are equal.So atleast two means are equal.")
      print("Finding what means are equal...???\n\t")
      print("1 st Computing standard error ")
      s_e<-sqrt((2*MSSWS)/nrow(data))
      cat("The standard error is :",s_e)
      print(" 2 nd computing critical difference ")
      t_value<-qt(0.05,nrow(data)-1,lower.tail=TRUE)
      c_d<-abs(t_value)*s_e
      cat("The critical difference value is :",c_d)
      sumofcol<-c()
      z1<-sum(city1)
      sumofcol<-append(sumofcol,z1)
      z2<-sum(city2)
      sumofcol<-append(sumofcol,z2)
      z3<-sum(city3)
      sumofcol<-append(sumofcol,z3)
      z4<-sum(city4)
      sumofcol<-append(sumofcol,z4)
      z5<-sum(city5)
      sumofcol<-append(sumofcol,z5)
      z6<-sum(city6)
      sumofcol<-append(sumofcol,z6)
      z7<-sum(city7)
      sumofcol<-append(sumofcol,z7)
      z8<-sum(city8)
      sumofcol<-append(sumofcol,z8)
      
      order<-sumofcol/length(uniq)
      sort(order)
      barplot(order,xlab='cities',ylab='watersupplyrate',col='light pink',main='ANOVA',border='green')
      
      
      
      
    }
    if(FVAL<F_tab){
      cat(" Conclusion : \n\tWe reject null hypothesis and there is no significant difference between the mean values")
      
      
    }  
    
    
  }
  else if(choice==4){
    
    name<-readline(prompt="Enter the city name")
    name1<-toString(name)
    print(name1)
    val<-c()
    vall<-c()
    for(i in 1:nrow(data)){
      if(city[i]==name1){
        val<-append(val,water[i])
        vall<-append(vall,price[i])
      }
    }
    
    data5<-data.frame(val,vall)
    
    print(ggscatter(data5, x = 'val', y = 'vall', 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "watersupplyrate", ylab = "landvalue", main='BASE 1'))
    
  }
  else if(choice==5){
    name<-readline(prompt="Enter the first city name")
    name1<-toString(name)
    
    val<-c()
    vall<-c()
    for(i in 1:nrow(data)){
      if(city[i]==name1){
        
        val<-append(val,price[i])
      }
    }
    name11<-readline(prompt="Enter the Second city name")
    name12<-toString(name11)
    for(i in 1:nrow(data)){
      if(city[i]==name12){
        
        vall<-append(vall,price[i])
      }
    }
    colo<-c("red","orange")
    LANDVALUE=c(val,vall)
    CITYNAME=rep(c(name1,name12),each=12)
    dataset<-data.frame(CITYNAME,LANDVALUE,stringsAsFactors = TRUE)
    summary(dataset)
    boxplot(dataset$LANDVALUE~dataset$CITYNAME)
    print(ggboxplot(dataset, x = "CITYNAME", y = "LANDVALUE", 
                    color = 'red', palette = c("#FFA500", "#FF0000"),
                    ylab = "PRICE", xlab = "CITY"))
    
    res <- wilcox.test(dataset$LANDVALUE~dataset$CITYNAME,mu=0, alternative="two.sided", exact = FALSE,conf.int=TRUE,conf.level=0.95,paired=FALSE)
    print(res)
    
    
  }
  else if(choice==6)
  {
    name<-readline(prompt="Enter the city name")
    name1<-toString(name)
    
    WSR<-c()
    LVALUE<-c()
    ESR<-c()
    for(i in 1:nrow(data)){
      if(city[i]==name1){
        
        LVALUE<-append(LVALUE,price[i])
        WSR<-append(WSR,water[i])
        ESR<-append(ESR, electric[i])
      }
    }
    model<-efp(LVALUE~WSR+ESR)
    plot(model)
    
    
  }
  else if(choice==7)
  {
    print("Forecast for next 12 months land value")
    name<-readline(prompt="Enter the city name")
    name1<-toString(name)
    LVALUE<-c()
    
    for(i in 1:nrow(data)){
      if(city[i]==name1){
        
        LVALUE<-append(LVALUE,price[i])
        
      }
    }
    
    holt_mod <- holt(LVALUE, h = 12)
    print(summary(holt_mod))
    print(autoplot(holt_mod))
    
    
  }
  else{
    print("Thankyou")
    break
  } 
}