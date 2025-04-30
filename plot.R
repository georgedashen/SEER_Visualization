library(plotly)
cancer<-read.csv('cancer_freq.csv',header=T)
cancer<-cancer[which(cancer$Rural.Urban.Continuum.Code=="Metropolitan Counties"|cancer$Rural.Urban.Continuum.Code=="Nonmetropolitan Counties"),]
colnames(cancer)[1] <- "Sex"
population_per_state<-matrix(c(0.014969469339975,0.00222391061638478,0.0225731407881041,0.00921772045028248,0.119743046798181,
                               0.0176649204974569,0.0108190889054683,0.00300150584612908,0.0661046494547044,0.0325758871652385,
                               0.00427958879027117,0.00555678966230469,0.0382865832023474,0.020546053917047,0.009622375592527,
                               0.00886271075961017,0.0136181318280308,0.0141293291144777,0.0041066266162941,0.0184194966867957,
                               0.0209676874265673,0.0303145523584271,0.0172075296096323,0.00902385925058843,0.0187107203975072,
                               0.00328671321673457,0.00589331233823273,0.00954541632186899,0.00415570024180973,0.0270168679896388,
                               0.00640663876425202,0.0588152785486235,0.0322437596417135,0.00232778525286576,0.035566415776058,
                               0.0121080608777091,0.0129010862860973,0.0388819027932994,0.0032153809578036,0.0158713363633037,
                               0.00271531298806438,0.0209471868541132,0.0893045055175694,0.00988492283482634,0.00189598966432919,
                               0.0261292966177246,0.0234011053768752,0.00542865804283827,0.0177407665322814,0.00177122512701511),ncol=1,nrow=50)
population_per_state<-as.data.frame(population_per_state)
population_per_state$state_short<-c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
colnames(population_per_state)<-c('rate','state_short')
name<-read.csv('name.csv',header=F)
name$V1[1] <- "Alabama"
for(i in 1:50){
  population_per_state$state[i]<-tolower(name$V1[which(name$V2==population_per_state$state_short[i])])
}

map_plot<-function(type, sex, race, age, year){
  list_type<-which(cancer$Site...malignant..least.detail.==type)
  list_race<-which(cancer$Race.and.origin.recode..NHW..NHB..NHAIAN..NHAPI..Hispanic....no.total==race)
  list_age<-which(cancer$Age.recode.with..1.year.olds==age)
  list_year<-which(cancer$Year.of.diagnosis==year)
  if(sex=='Male'|sex=='Male and female'){
    list_sex<-which(cancer$Sex==sex)
    l<-intersect(intersect(intersect(intersect(list_type,list_age),list_race),list_sex),list_year)
    plot_list<-cancer[l,]
    total_num<-sum(plot_list$Count)
  } else if(sex=='Female'){
    list_sex1<-which(cancer$Sex=='Male')
    list_sex2<-which(cancer$Sex=='Male and female')
    l1<-intersect(intersect(intersect(intersect(list_type,list_age),list_race),list_sex1),list_year)
    l2<-intersect(intersect(intersect(intersect(list_type,list_age),list_race),list_sex2),list_year)
    plot_list1<-cancer[l1,]
    plot_list2<-cancer[l2,]
    total_num<-sum(plot_list2$Count)-sum(plot_list1$Count)
  }

  num_per_state<-round(total_num*population_per_state$rate)
  return(num_per_state)
}

