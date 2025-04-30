source('plot.R')
cancer<-read.csv('cancer_freq.csv',header=T)
colnames(cancer)[1] <- "Sex"
cancer<-cancer[which(cancer$Rural.Urban.Continuum.Code=="Metropolitan Counties"|cancer$Rural.Urban.Continuum.Code=="Nonmetropolitan Counties"|
                       cancer$Rural.Urban.Continuum.Code=='Unknown/missing/no match (Alaska or Hawaii - Entire State)'),]
factor<-read.csv('factor.csv',header=T)
State_Center<-read.csv('State_Center.csv',header=T)
State_Center$Place.Name<-tolower(State_Center$Place.Name)
colnames(factor)<-c('state','Regular colorectal cancer test over 50 years of age','Regular colorectal endoscopy over 50 years of age',
                    'Current smoking prevalence','Ever smoking prevalance','Regular home base FOBT over 50 years of age',
                    'Mammography prevalence within 2 years over 40 years of age','Pap smear test prevalence within 3 years over 18 years of age',
                    'Smoke free at home','Smoke free in workplace','BMI under 25','BMI over 30','Take fruit every week',
                    'Take physical activity every week','Take vegetables every week','All sites','Female breast',
                    'Uterine cervix','Colon   rectum','Uterine corpus','Leukemia','Lung   bronchus',
                    'Melanoma of the skin','Non  Hodgkin lymphoma','Prostate','Urinary bladder')
factor$state<-tolower(factor$state)
population<-read.csv('population.csv',header = T)
population<-population[0:950,]
for(i in 1:950){
  population$state[i]<-state.abb[which(state.name==population$State[i])]
}
state_name<-tolower(unique(population$State))
population_per_state_num<-matrix(ncol=50,nrow=1)
for(i in 1:50){
  population_per_state_num[1,i]<-sum(population$Count[which(tolower(population$State)==state_name[i])])
}
colnames(population_per_state_num)<-state_name
population_per_state<-population_per_state_num/sum(population_per_state_num)

library(maps)
library(mapproj)
library(ggplot2)
library(bslib)
usa_map <- map_data(map = "state")
name<-read.csv('name.csv',header=F)
name$V1[1] <- "Alabama"
# User interface ----

theme_cus <- bs_theme(
  bg = "#3399CC", fg = "white", primary = "#FCC780",
  base_font = font_google("Space Mono"),
  code_font = font_google("Space Mono")
)

ui <- fluidPage(
  theme = theme_cus,
  
  titlePanel(h1("Cancer Statistics Visualization System on SEER data", align = "center")),
  
      sidebarLayout(       
      sidebarPanel(width = 3,
                   style = paste0("height: 50vh; overflow-y: auto;"),
               selectInput("sex",
                           label = "Sex",
                           choices = c("Male and female","Female", "Male"),
                           selected = "Male and female"),
               selectInput("race", 
                           label = "Race and Ethnicity",
                           choices = c("Non-Hispanic White", 
                                       "Non-Hispanic Black","Non-Hispanic American Indian/Alaska Native", 
                                       "Non-Hispanic Asian or Pacific Islander", "Hispanic (All Races)"),
                           selected = "Non-Hispanic White"),
               selectInput("age", 
                           label = "Age",
                           choices = c('00 years','01-04 years','05-09 years','10-14 years','15-19 years',
                                       '20-24 years','25-29 years','30-34 years','35-39 years','40-44 years',
                                       '45-49 years','50-54 years','55-59 years','60-64 years','65-69 years',
                                       '70-74 years','75-79 years','80-84 years','85+ years'),
                           selected = "85+ years"),
               selectInput("type", 
                           label = "Cancer Type",
                           choices = c('Oral Cavity and Pharynx - mal','Esophagus - mal','Stomach - mal','Small Intestine - mal',
                                       'Colon and Rectum - mal','Anus, Anal Canal and Anorectum - mal','Liver and Intrahepatic Bile Duct - mal',
                                       'Gallbladder - mal','Other Biliary - mal','Pancreas - mal','Retroperitoneum - mal',
                                       'Peritoneum, Omentum and Mesentery - mal','Other Digestive Organs - mal','Respiratory System - mal',
                                       'Bones and Joints - mal','Soft Tissue including Heart - mal','Skin excluding Basal and Squamous - mal',
                                       'Breast - mal','Female Genital System - mal','Male Genital System - mal','Urinary System - mal',
                                       'Eye and Orbit - mal','Brain and Other Nervous System - mal','Endocrine System - mal','Lymphoma - mal',
                                       'Myeloma - mal','Leukemia - mal','Mesothelioma - mal','Kaposi Sarcoma - mal','Miscellaneous - mal'),
                           selected = 'Oral Cavity and Pharynx - mal'),
               selectInput("year", 
                           label = "Year",
                           choices = c('2000-2019','2000','2001','2002','2003','2004',
                                       '2005','2006','2007','2008','2009','2010','2011',
                                       '2012','2013','2014','2015','2016','2017','2018','2019'),
                           selected = '2019'),
               selectInput("factor",
                           label="Risk factor",
                           choices = c('Regular colorectal cancer test over 50 years of age','Regular colorectal endoscopy over 50 years of age',
                                       'Current smoking prevalence','Ever smoking prevalance','Regular home base FOBT over 50 years of age',
                                       'Mammography prevalence within 2 years over 40 years of age','Pap smear test prevalence within 3 years over 18 years of age',
                                       'Smoke free at home','Smoke free in workplace','BMI under 25','BMI over 30','Take fruit every week',
                                       'Take physical activity every week','Take vegetables every week'),
                           selected='Current smoking prevalence'),
               selectInput('type2',
                           label='Type of cancer to analyse correlation',
                           choices = c('Female breast',
                                       'Uterine cervix','Colon   rectum','Uterine corpus','Leukemia','Lung   bronchus',
                                       'Melanoma of the skin','Non  Hodgkin lymphoma','Prostate','Urinary bladder'),
                           selected = 'Femal breast'
               ),
               tags$head(tags$style(HTML(".selectize-input {height: 20px; width: 100%; font-size: 15px;}"))),
               tags$head(tags$style(HTML(".form-group {margin-bottom: -10; margin-top: -50;!important;}")))
             ),
           mainPanel(width = 9,
             plotlyOutput("map",height = 420,width = 700),verbatimTextOutput("info"))
      ),
  
    fluidRow(
      column(width = 6, offset = 0,style='margin-left:-0px;',
             mainPanel(plotOutput('bar',width = 800))
      ),
      column(width = 3, offset = 0,style='margin-left:-50px;',
             mainPanel(plotOutput('line',width = 430))
      ),
      column(width = 3, offset = 0,style='margin-left:10px;',
             mainPanel(plotOutput('correlation',width = 450))
      )
    )
)






# Server logic ----
server <- function(input, output) {
  population_inner <- reactive({
  num_per_state<-map_plot(input$type, input$sex, input$race, input$age, input$year)
  population_inner<-matrix(num_per_state,ncol=1)
  population_inner<-as.data.frame(population_inner)
  population_inner$state_short<-c('AL','AK','AZ','AR','CA','CO','CT','DE','FL',
                                  'GA','HI','ID','IL','IN','IA','KS','KY','LA',
                                  'ME','MD','MA','MI','MN','MS','MO','MT','NE',
                                  'NV','NH','NJ','NM','NY','NC','ND','OH','OK',
                                  'OR','PA','RI','SC','SD','TN','TX','UT','VT',
                                  'VA','WA','WV','WI','WY')
  colnames(population_inner)<-c('rate','state_short')
  for(i in 1:50){
    population_inner$state[i]<-tolower(name$V1[which(name$V2==population_inner$state_short[i])])
  }
  population_inner$state[1] <- "alabama"
  usa_pop_df <- merge(
    x = usa_map,
    y = population_inner,
    by.x = "region", # region 首字母小写的各州名称
    by.y = "state",
    all.x = TRUE,
    sort = FALSE
  )
  usa_pop_df <- usa_pop_df[order(usa_pop_df$order), ]
  population_inner <- merge(
    x=population_inner,
    y=State_Center,
    by.x='state',
    by.y='Place.Name',
    all.x=TRUE,
    sort=FALSE
  )
  colnames(population_inner)[2] <- "Count" #rate replaced by count
  return(population_inner)
  })
  
  output$map <- renderPlotly({
    p=ggplot(population_inner(), aes(map_id = state)) +
      geom_map(aes(fill=Count), map=usa_map,colour="black",size = 0.5) + # line color to black
      scale_fill_distiller(palette = "RdPu",direction= 1) +
      expand_limits(x = usa_map$long, y = usa_map$lat) + xlab("Latitude (\u00b0W)") + ylab("Longitude (\u00b0N)") + theme_bw(base_size = 25)+
      geom_point(aes(x=Longitude,y=Latitude),cex=15,alpha=0)
    ggplotly(ggplot(population_inner(), aes(map_id = state)) +
               geom_map(aes(fill=Count), map=usa_map,colour="black",size = 0.5) + # line color to black
               scale_fill_distiller(palette = "RdPu",direction= 1) +
               expand_limits(x = usa_map$long, y = usa_map$lat) + xlab("Latitude (\u00b0W)") + ylab("Longitude (\u00b0N)") + theme_bw(base_size = 25))

  })
  
  output$info <- renderPrint({
    nearPoints(population_inner()[,-3], input$plot_hover, xvar = 'Longitude',yvar='Latitude')
  })
  
  
  
  output$bar <- renderPlot({
    per_type<-matrix(ncol=length(unique(cancer$Site...malignant..least.detail.)),nrow=1)
    for(i in 1:length(unique(cancer$Site...malignant..least.detail.))){
      per_type[1,i]<-sum(cancer[which(cancer$Sex=='Male and female'&cancer$Year.of.diagnosis==input$year&
                                        cancer$Site...malignant..least.detail.==unique(cancer$Site...malignant..least.detail.)[i]),]$Count)
    }
    colnames(per_type)<-unique(cancer$Site...malignant..least.detail.)
    #per_type<-as.data.frame(per_type)
    #a<-as.matrix(sort(per_type,decreasing = T)[1:10],nrow=1)
    # text=paste(input$year,'Top 10 cancer in US')
    # barplot(a,main = text,ylab='Total')
    #per_type <- per_type %>% arrange(desc()) 
    b<-data.frame(t(per_type))
    b$type <- rownames(b)
    colnames(b)<-c('num','type')
    b <- b %>% arrange(desc(num)) %>% head(10)
    # ggplot(b)+geom_col(aes(x=num,y=reorder(type,num)))+
    #   xlab('Total')+
    #   ylab('Type')+
    #   ggtitle(paste('2019','Top 10 cancer in US'))
    cancerCol <- data.frame(type = unique(cancer$Site...malignant..least.detail.),
                            color = palette(rainbow(length(unique(cancer$Site...malignant..least.detail.)))))
    b <- merge(b, cancerCol, sort = F)
    b$type <- gsub(" - mal","",b$type)
    
    #add count number next to bar
    ggplot(b)+geom_col(aes(x=num,y=reorder(type,num),fill=color),show.legend = F) +
      geom_text(aes(x = num, y = reorder(type,num), label = num), stat = "identity",hjust = -0.3) +
      xlab('Count') + ylab('') +
      ggtitle(paste(input$year,'Top 10 cancer in US')) + scale_x_continuous(expand = c(0.005,0,0.1,0)) +
      theme_bw(base_size = 20) + theme(panel.grid.major.y = element_blank())
    
  })
  output$line<-renderPlot({
    male<-cancer[which(cancer$Sex=='Male'&cancer$Site...malignant..least.detail.==input$type),]
    male_num<-vector()
    for(i in 2:length(unique(male$Year.of.diagnosis))){
      male_num[i-1]<-sum(male[which(male$Year.of.diagnosis==unique(male$Year.of.diagnosis)[i]),]$Count) # male 2000 special type total num
    }
    male_num<-as.matrix(male_num)
    rownames(male_num)<-unique(male$Year.of.diagnosis)[2:length(unique(male$Year.of.diagnosis))]
    
    total<-cancer[which(cancer$Sex=='Male and female'&cancer$Site...malignant..least.detail.==input$type),]
    total_num<-vector()
    for(i in 2:length(unique(total$Year.of.diagnosis))){
      total_num[i-1]<-sum(total[which(total$Year.of.diagnosis==unique(total$Year.of.diagnosis)[i]),]$Count) # total 2000 special type total num
    }
    total_num<-as.matrix(total_num)
    rownames(total_num)<-unique(total$Year.of.diagnosis)[2:length(unique(total$Year.of.diagnosis))]
    
    female_num<-total_num-male_num
    text<-strsplit(input$type,split = ' -')[[1]][1]
    plot(male_num,col='lightblue',type='l',lwd=3,ylim=c(0,max(c(female_num,male_num))),xaxt='n',
         xlab='Year',ylab="Total",main=text)
    lines(female_num,col='pink',lwd=3)
    legend('topleft',legend=c('Male','Female'),lwd=c(3,3),col=c('lightblue','pink'))
    axis(1,c(5,10,15,20),labels=c('2004','2009','2014','2019'))
  })
  output$correlation<-renderPlot({
    tmp1<-input$factor
    tmp2<-input$type2
    a<-factor[,which((colnames(factor)==tmp1)==TRUE)]
    b<-factor[,which((colnames(factor)==tmp2)==TRUE)]/population_per_state_num*100
    c<-data.frame(a)
    c$b<-t(b)
    colnames(c)<-c('x','y')
    c$state<-factor$state
    # p<-ggplot(c)+geom_point(aes(x=x,y=y))
    lm <- lm( y~ x, data = c)
    new_x <- seq(min(c$x), max(c$x), 0.01)
    pred_y <- data.frame(predict(lm, newdata = data.frame(x = new_x),
                                 interval = "confidence"),
                         new_x = new_x)
    p_value<-summary(lm)$coefficients[2,'Pr(>|t|)']
    r_2<-summary(lm)$r.squared
    text<-paste(c('R^2:',round(r_2,3),' ','p_value:',round(p_value,3)),collapse = ' ')
# 
#     p +
#       geom_line(data = pred_y, mapping = aes(x = new_x, y = fit),
#                 color = "blue", size = 1, alpha = 0.5) +
#       geom_ribbon(data = pred_y, mapping = aes(x = new_x,
#                                                ymin = lwr, ymax = upr),
#                   fill = "grey", alpha = 0.5)+ggtitle('Correlation analysis')+
#       labs(x=tmp1,y=tmp2)+
#       annotate('text',x=-Inf,y=Inf,label=text,hjust=-1,vjust=2)
    # p_value<-summary(lm)$coefficients[2,'Pr(>|t|)']
    # r_2<-summary(lm)$r.squared
    # text<-paste(c('R^2:',round(r_2,3),'  ','p_value:',round(p_value,3)),collapse = ' ')
    # r_2<-cor(c$x,c$y)
    # p_value<-cor.test(c$x,c$y)$p.value
    # ggplot(c, aes(x,y)) + geom_point(size=3) + 
    #   geom_smooth(method="lm",se=F,size=3) + 
    #   theme_classic(base_size = 25)+
    #   ggtitle('Correlation analysis')+
    #   labs(x=tmp1,y=tmp2)+
    #   annotate('text',x=-Inf,y=Inf,label=text,hjust=-1,vjust=2)
    p<- ggplot(c,aes(x=x,y=y)) + geom_point(fill="pink", color="black", size=4, pch=21)
    
    # line184
    p +
      geom_ribbon(data = pred_y, mapping = aes(x = new_x,y = fit,
                                               ymin = lwr, ymax = upr),
                  fill = "grey", alpha = 0.5) + 
      geom_line(data = pred_y, mapping = aes(x = new_x, y = fit),
                color = "black", linewidth=1, alpha = 0.5) + ggtitle('Correlation analysis') +
      labs(x=tmp1,y=tmp2)+
      annotate('text',x=-Inf,y=-Inf,label=text,hjust=-1,vjust=-2,size=4,fontface = "bold") +
      theme_bw(base_size = 20) + theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size=12,hjust = 0.2))
      
  })
}

# Run app ----
shinyApp(ui, server, options = list(height = 1200))

# paste(unique(cancer$Age.recode.with..1.year.olds),collapse = '\',\'')
# 
# input<-data.frame(c('Oral Cavity and Pharynx','Male and Female',"All Races and Ethnicities",'85+ years','2019'))
# rownames(input)<-c('type','sex','race','age','year')
# 
# num_per_state<-map_plot(input['type',], input['sex',], input['race',], input['age',], input['year',])
# 
# 
# 
# 
# map_plot("Oral Cavity and Pharynx - mal"  , "Male"       ,    "Non-Hispanic White", "85+ years"                , '2015'    )
# cancer$Site...malignant..least.detail.[1]


