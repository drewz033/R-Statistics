library(dplyr)
library(ggplot2)
library(statsr)

data(arbhutnot)

arbuthnot$girls


arbuthnot$boys


ggplot(data=arbuthnot,aes(x=year,y=girls))+
  geom_point()


?ggplot


arbuthnot$boys+arbuthnot$girls


arbuthnot<-arbuthnot%>%
  mutate(total=boys+girls)

ggplot(data=arbuthnot, aes(x=year,y=total))+
  geom_line()+
  geom_point()

arbuthnot<-arbuthnot%>%
  mutate(more_boys=boys>girls)


range(present$year)

present<-present%>%
  mutate(total=boys+girls)

present<-present%>%
  mutate(pct_boys=boys/total)


ggplot(data=present,aes(x=year,y=pct_boys))+
  geom_point()+
  geom_line()


present<-present%>%
  mutate(more_boys=boys>girls)



present<-present%>%
  mutate(prob_boys_girls=boys/girls)

ggplot(data=present,aes(x=year,y=prob_boys_girls))+
  geom_point()+
  geom_line()



max(present$total)





data(nycflights)

names(nycflights)

str(nycflights)



#- `filter()`
#- `arrange()`
#- `select()` 
#- `distinct()`
#- `mutate()`
#- `summarise()`
#- `sample_n()`


ggplot(data=nycflights,aes(x=dep_delay))+
  geom_histogram(binwidth=10)



rdu<-nycflights%>%
  filter(dest=='RDU',month=='2')

ggplot(data=rdu,aes(x=dep_delay))+
  geom_histogram()


rdu%>%
  summarise(mean_dd=mean(dep_delay))

sf<-nycflights%>%
  filter(dest=='SFO',month==2)


ggplot(data=sf,aes(x=arr_delay))+
  geom_histogram(binwidth=20)



sf%>%
  group_by(carrier)%>%
  summarise(mean_airline=mean(arr_delay),IQR_airline=IQR(arr_delay))


nycflights%>%
  group_by(month)%>%
  summarise(median_dd=median(dep_delay))%>%
  arrange(desc(median_dd))
              

nycflights<-nycflights%>%
  mutate(dep_type=ifelse(dep_delay<5,'on time','delayed'))

nycflights%>%
  group_by(origin)%>%
  summarise(rate=sum(dep_type=='on time')/n())%>%
  arrange(desc(rate))


nycflights<-nycflights%>%
  mutate(speed=nycflights$distance/(nycflights$air_time*60))


nycflights%>%
  group_by(tailnum)%>%
  arrange(desc(speed))


ggplot(data=nycflights,aes(x=distance,y=speed))+
  geom_point()



library(dplyr)
library(ggplot2)
library(statsr)



data(kobe_basket)


kobe_streak<-calc_streak(kobe_basket$shot)


ggplot(data=kobe_streak,aes(x=length))+
  geom_histogram(binwidth=1)

median(kobe_streak$length)
max(kobe_streak$length)


coin_outcomes<-c('heads','tails')
coin_table<-sample(coin_outcomes,size=100,replace=TRUE,prob=c(.3,.7))
table(coin_table)


shot_outcomes<-c('H','M')
sim_basket<-sample(shot_outcomes,size=133,replace=TRUE,prob=c(.45,.55))


sim_streak<-calc_streak(sim_basket)

ggplot(data=sim_streak,aes(x=length))+
  geom_histogram(binwidth=1)



dbinom(600,1000,.56)


sum(dbinom(1:10,10,.07))

sum(dbinom(35:3000000,3000000,.00001))

dbinom(92,100,.90)

sum(dbinom(50:160,160,.28))



load("brfss2013.RData")

df<-brfss2013


df1<-df$asthnow

print(df1)


ex<-brfss2013[c('exerhmm1','exerhmm2')]



exercise<-brfss2013[c('marital','children','exerhmm1')]


married<-exercise$marital=='Married'


married_status<-exercise%>%
  filter(marital=='Married',children>0,exerhmm1>=0)

married_status%>%
  group_by(children)%>%
  summarise(mean(exerhmm1),median(exerhmm1),sd(exerhmm1))

ex_time<-married_status%>%
  group_by(children)%>%
  summarise(mean(exerhmm1),median(exerhmm1))




unmarried_status<-exercise%>%
  filter(marital!='Married',children==0,exerhmm1>=0)

unmarried_status%>%
  summarise(mean(exerhmm1))


test<-unmarried_status%>%
  group_by(exerhmm1,children)%>%
  tally()


ggplot(data=ex_time,aes(x=children,y=ex_time$`median(exerhmm1)`))+
  geom_histogram(stat='identity')+
  labs(x='Number of Children',y='Exercise Time (Minutes) Past Month')



income<-exercise<-brfss2013[c('income2','lsatisfy')]
income<-na.omit(income)
attach(income)


library(dummies)
income<-dummy.data.frame(income,names=c('lsatisfy','income2'))

income%>%
  summarise()


income$lsatisfy<-factor(income$lsatisfy)

           
income<-factor(lsatisfy,levels=c('Very Dissatisfied','Dissatisfied','Satisfied','Very Satisfied'),labels=c(0,1,2,3))
income2<-factor(income$income2,c(0,1,2,3,4,5,6,7),labels=c('Less than 10K','Less than 15K More than 10K','Less than 20k More than 15K',
                                                    'Less than 25K More than 20K','Less than 35K More than 25K',
                                                    'Less than 50K More than 35K','Less than 75K More than 50K','75K +'))

result<-table(lsatisfy)
addmargins(result)

result2<-table(income2)

addmargins(result2)


test<-bfrss2013

heart<-brfss2013[c('smoke100','drnk3ge5','cvdinfr4')]

heart<-na.omit(heart)


heart%>%
  group_by(smoke100)%>%
  summarise(mean(drnk3ge5))

quantile(heart$drnk3ge5)



ggplot(data=heart,aes(x=drnk3ge5))+
  geom_bar()


heart2<-heart%>%
  filter(smoke100=='Yes')


ggplot(data=heart2,aes(x=drnk3ge5))+
  geom_bar()

binge<-heart%>%
  filter(smoke100=='Yes',between(drnk3ge5,10,30))

no_binge<-heart%>%
  filter(smoke100=='Yes',between(drnk3ge5,0,9))


binge_new<-na.omit(binge)
no_binge<-na.omit(binge)


binge%>%
  count(cvdinfr4)


ggplot(data=binge_new,aes(x=drnk3ge5,fill=cvdinfr4))+
 geom_bar(position='fill')


income<-brfss2013[c('physhlth','income2')]

income<-na.omit(income)


income%>%
  group_by(income2)%>%
  summarise(median(physhlth))



b<-ggplot(data=income,aes(x=income2,y=physhlth))

b+geom_boxplot()+ coord_flip()

library(shiny)


data(ames)

ggplot(data = ames, aes(x = area)) +
  geom_histogram(binwidth = 250)

ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile


samp1 <- ames %>%
  sample_n(size = 50)

ggplot(data = samp1, aes(x = area)) +
  geom_histogram(binwidth = 250)


ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(area))

sample_means50 <- ames %>%
  rep_sample_n(size = 10, reps = 25, replace = TRUE) %>%
  summarise(x_bar = mean(area))







shinyApp(
  ui <- fluidPage(
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        selectInput("selected_var",
                    "Variable:",
                    choices = list("area", "price"),
                    selected = "area"),         
        
        numericInput("n_samp",
                     "Sample size:",
                     min = 1,
                     max = nrow(ames),
                     value = 30),
        
        numericInput("n_sim",
                     "Number of samples:",
                     min = 1,
                     max = 30000,
                     value = 15000) 
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("sampling_plot"),
        verbatimTextOutput("sampling_mean"),
        verbatimTextOutput("sampling_se")
      )
    )
  ),
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    # create sampling distribution
    sampling_dist <- reactive({
      ames[[input$selected_var]] %>%
        sample(size = input$n_samp * input$n_sim, replace = TRUE) %>%
        matrix(ncol = input$n_samp) %>%
        rowMeans() %>%
        data.frame(x_bar = .)
      #ames %>%
      #  rep_sample_n(size = input$n_samp, reps = input$n_sim, replace = TRUE) %>%
      #  summarise_(x_bar = mean(input$selected_var))
    })
    
    # plot sampling distribution
    output$sampling_plot <- renderPlot({
      x_min <- quantile(ames[[input$selected_var]], 0.1)
      x_max <- quantile(ames[[input$selected_var]], 0.9)
      
      ggplot(sampling_dist(), aes(x = x_bar)) +
        geom_histogram() +
        xlim(x_min, x_max) +
        ylim(0, input$n_sim * 0.35) +
        ggtitle(paste0("Sampling distribution of mean ", 
                       input$selected_var, " (n = ", input$n_samp, ")")) +
        xlab(paste("mean", input$selected_var)) +
        theme(plot.title = element_text(face = "bold", size = 16))
    })
    
    # mean of sampling distribution
    output$sampling_mean <- renderText({
      paste0("mean of sampling distribution = ", round(mean(sampling_dist()$x_bar), 2))
    })
    
    # mean of sampling distribution
    output$sampling_se <- renderText({
      paste0("SE of sampling distribution = ", round(sd(sampling_dist()$x_bar), 2))
    })
  },
  
  options = list(height = 500) 
)




n<-60
samp<-sample_n(ames,n)



ggplot(data = samp, aes(x = area)) +
  geom_histogram(binwidth = 250)

samp%>%
  summarise(mean(samp$area))


z_star_95<-qnorm(0.975)

samp%>%
  summarise(lower=mean(area)-z_star_95*(sd(area/sqrt(n))),
            upper=mean(area)+z_star_95*(sd(area)/sqrt(n)))

params<-ames%>%
  summarise(mean(area))

pt(-1.71,25)
pt(-1.32,25)


dt(2,99999)

ci <- ames %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(n)))

ci%>%
  slice(1:5)

pnorm()

print(z_star_95)


data(nc)


print(nc)


summary(nc$gained)


smoker<-nc%>%
  filter(habit=='smoker')

nonsmoker<-nc%>%
  filter(habit=='nonsmoker')


boxplot(smoker$weight,nonsmoker$weight)

boxplot(nonsmoker)


nc%>%
  group_by(habit)%>%
  summarise(mean_weight=mean(weight))

inference(y=weeks,data=nc,statistic='mean',type='ci',null=0,alternative='twosided',method='theoretical',conf_level=.99)

?inference


boxplot(nc$fage)

IQR(nc$fage,c(.25,.75))
