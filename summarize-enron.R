## put your script in this file

## put your script in this file
getwd()
#Question 1
#Installing required packages
install.packages("dplyr")
library("dplyr")
install.packages("stringr")
library(stringr)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("sqldf")
library(sqldf)
#install.packages("shiny")
#library(shiny)
#install.packages("shinydashboard")
#library(shinydashboard)
install.packages("gridExtra")
library(gridExtra)
#importing the csv to local repository
enron_data <- read.csv("https://raw.githubusercontent.com/redowl/RedOwl-Data-Science-Recruiting-Exam/public-master/enron-event-history-all.csv",header=FALSE)
#having a look at the data
View(enron_data)
#renaming to meaningful column names
colnames(enron_data) <- c('time','msg_identifier','sender','recipients','topic','mode')
#checking class of time variable to perform appropriate conversion
class(enron_data$time)
options(digits.secs=3)
enron_data$Cleaned_time_stamp=.POSIXct(enron_data$time/1000) #converting to time stamp#
#removing unwanted columns
enron_data_subset=enron_data[,c(-5,-6)]
#couting number of pipes in recipient column
enron_data_subset$pipe_ctr=str_count(enron_data_subset$recipients,pattern = "\\|")

#splitting recipeient names from each other
a=str_split_fixed(enron_data_subset$recipients,"\\|",enron_data_subset$pipe_ctr+1)

x_total=data.frame()#create a empty data frame to fill
#dim(a)[2]
#abc=as.data.frame(unique(a[,10]))
#test=as.data.frame(a[,2])
#test=as.data.frame(a[,26][a[,26] != ""])

#for loop to extract the list of all recipients in one column
for (i in 1:dim(a)[2])
{x=as.data.frame(a[,i][a[,i] != ""])
x_total=rbind(x_total,x)
}

colnames(x_total) <- c('recepients') #renaming to meaningful column name#
x_total$recepients_lower=tolower(x_total$recepients) #converting every name to lower case for aggregating
#counting number of times each recipient occurs which is equal to the number of times he has received a mail
receipient_cnt=x_total%>%group_by(recepients_lower)%>%summarise(rec_count=n())
#converting every name to lower case for aggregating
enron_data_subset$recipients_lower=tolower(enron_data_subset$recipients)
#converting every name to lower case for aggregating
enron_data_subset$sender_lower=tolower(enron_data_subset$sender)
#counting number of times each sender occurs which is equal to the number of times he has sent a mail
sender_cnt=enron_data_subset%>%group_by(sender_lower)%>%summarise(sent_count=n())

#generating the recipient list
recepient_list=as.data.frame(unique(tolower(x_total$recepients)))
colnames(recepient_list) <- c('people_name')
#test <- strsplit(enron_data_subset$recipients),"|")
people_list=data.frame()
#generating the sender list
sender_list=as.data.frame(unique(enron_data_subset$sender_lower))
colnames(sender_list)                   
colnames(sender_list) <- c('people_name')
#forming the appended list of senders and receivers
people_list=rbind(recepient_list,sender_list)
#converting every name to lower case for aggregating
people_list$people_name_lower=tolower(people_list$people_name)
#Avoiding duplicates by name
final_people_list=as.data.frame(unique(people_list$people_name_lower))

#renaming to meaningful column name#
names(final_people_list)[names(final_people_list) == 'unique(people_list$people_name_lower)'] <- 'recipients_name'
colnames(receipient_cnt)
colnames(final_people_list)
colnames(sender_cnt)
#left join of list of people on recipient count table to get the number to times each invidual has received a mail
answer1=merge(final_people_list,receipient_cnt,by.x="recipients_name",by.y = "recepients_lower",all.x = TRUE)
#left join of list of people on sender count table to get the number to times each invidual has sent a mail
answer1_final=merge(answer1,sender_cnt,by.x="recipients_name",by.y = "sender_lower",all.x = TRUE)

#check those names which doesnt have a single alphabet (junk values)
is.letter <- function(x) grepl("[[:alpha:]]", x)
answer1_final$only_name=is.letter(answer1_final$recipients_name)
#Removing junk values identified above
answer1_cleaned=answer1_final[answer1_final$only_name!='FALSE',]
colnames(answer1_cleaned)
names(answer1_cleaned)[names(answer1_cleaned) == 'recipients_name'] <- 'person'
names(answer1_cleaned)[names(answer1_cleaned) == 'sent_count'] <- 'sent'
names(answer1_cleaned)[names(answer1_cleaned) == 'rec_count'] <- 'received'

answer1_upload=answer1_cleaned[,c(1,3,2)]
#generating all these plots to a csv in the local repository
write.csv(answer1_upload, file = "answer1.csv",row.names = FALSE)


#question 2#
#I define prolific senders as the people -
#-who have reached out to most number of people in a comparable time frame (daily average)
#Extracting date part from cleaned time stamp
enron_data_subset$date.only= format(enron_data_subset$Cleaned_time_stamp,'%m/%d/%Y')
#identifying count of number of people to whom the mails were sent at each time
enron_data_subset$no_of_sent_mails=enron_data_subset$pipe_ctr+1
colnames(enron_data_subset)
#summarising the count of people on a daily basis
daywise_sent_cnt=enron_data_subset%>%group_by(sender_lower,date.only)%>%summarise(sent_count=sum(no_of_sent_mails))
#taking daily average of count of people
daywise_sent_avg=daywise_sent_cnt%>%group_by(sender_lower)%>%summarise(sent_avg=mean(sent_count))

#check those names which doesnt have a single alphabet (junk values)
is.letter <- function(x) grepl("[[:alpha:]]", x)
daywise_sent_avg$only_name=is.letter(daywise_sent_avg$sender_lower)
#Removing junk values identified above
daywise_sent_avg=daywise_sent_avg[daywise_sent_avg$only_name!='FALSE',]
#identifying 25th,50th and 75th percentile based on daily average of people reached out
quantile(daywise_sent_avg$sent_avg, c(.25, .50, .75)) 

#Classying the top 25% as prolific senders
profilic_senders=daywise_sent_avg%>%filter(sent_avg>3)%>%select(sender_lower)

#Visualization
#Extracting day information from time
enron_data_subset$day_of_week=weekdays(enron_data_subset$Cleaned_time_stamp)
#Classifying them into weekend and weekdays
enron_data_subset$weekend_weekday=ifelse((enron_data_subset$day_of_week=="Saturday" |enron_data_subset$day_of_week=="Sunday"),"Weekend","Weekday")
#Extracting hour information from time
enron_data_subset$hour= hour(enron_data_subset$Cleaned_time_stamp)
#Adding a flag to these prolific senders
profilic_senders$sender_flag="Prolific sender"
colnames(profilic_senders)
#Changing colnames before merging
names(profilic_senders)[names(profilic_senders) == 'sender_lower'] <- 'sender_name'
#Merging this information back to enron data set
enron_data_subset=merge(enron_data_subset,profilic_senders,by.x="sender_lower",by.y = "sender_name",all.x = TRUE)
#Flagging other senders as non proilifc
enron_data_subset$sender_flag[is.na(enron_data_subset$sender_flag)]<-'Non Prolific'
colnames(enron_data_subset)

#Hourwise count of no of people reached out to across prolific and non prolific
hourwise_sent_cnt=enron_data_subset%>%group_by(sender_flag,hour)%>%summarise(sent_count=sum(no_of_sent_mails))

#Hourwise percentage across prolific and non prolific for fair comparison between the two groups
hourwise_sent_cnt_perc=group_by(hourwise_sent_cnt, sender_flag) %>% mutate(percent = sent_count/sum(sent_count))

#plotting this info
ggplot(data=hourwise_sent_cnt_perc, aes(x = interaction(sender_flag, hour), y = percent, fill = factor(sender_flag))) +
  geom_bar(stat = "identity", position = position_dodge())
#recoding hour column based on the first plot
#8PM to 4AM is off hours and rest are work hours
hourwise_sent_cnt_perc$recoded_hr=ifelse((hourwise_sent_cnt_perc$hour>=20 | hourwise_sent_cnt_perc$hour<=4),"Off Hours","Work Hours")

#Shiftwise count of no of people reached out to across prolific and non prolific
shiftwise_sent_cnt=hourwise_sent_cnt_perc%>%group_by(sender_flag,recoded_hr)%>%summarise(count=sum(sent_count))
#Shiftwise percentage across prolific and non prolific for fair comparison between the two groups
shiftwise_sent_cnt_perc=group_by(shiftwise_sent_cnt, sender_flag) %>% mutate(percent = count/sum(count))
#plotting this info
q2.2=ggplot(data=shiftwise_sent_cnt_perc, aes(x = interaction(recoded_hr,sender_flag), y = percent, fill = factor(sender_flag))) +
  geom_bar(stat = "identity", position = position_dodge())+ggtitle("Comparion of Number of People Reached Over Emails across different shift") +
  labs(x="",y="% of no of people reached over email")+ annotate("text", x = 4, y=0.8, label = "70%")+ annotate("text", x = 3, y=0.4, label = "30%")+ annotate("text", x = 2, y=0.8, label = "76%")+ annotate("text", x = 1, y=0.4, label = "24%")+
  theme(axis.title.x = element_text(size=10,color='black',face='bold'),axis.text.x = element_text(size=10,color='black'),
        title =element_text(size=8, face='bold'))



#Day of week wise count of no of people reached out to across prolific and non prolific senders
weekdaywise_sent_cnt=enron_data_subset%>%group_by(sender_flag,day_of_week)%>%summarise(sent_count=sum(no_of_sent_mails))
#Day of week wise percentage across prolific and non prolific for fair comparison between the two groups
weekdaywise_sent_cnt_perc=group_by(weekdaywise_sent_cnt, sender_flag) %>% mutate(percent = sent_count/sum(sent_count))

#Ordering the factor variables
weekdaywise_sent_cnt_perc$sender_flag <- factor(weekdaywise_sent_cnt_perc$sender_flag, levels=c("Non Prolific", "Prolific sender"))
weekdaywise_sent_cnt_perc$day_of_week <- factor(weekdaywise_sent_cnt_perc$day_of_week, levels=c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"))

weekdaywise_sent_cnt_perc$day_of_week_no=ifelse(weekdaywise_sent_cnt_perc$day_of_week=="Monday",1,
                                                ifelse(weekdaywise_sent_cnt_perc$day_of_week=="Tuesday",2,  
                                                       ifelse(weekdaywise_sent_cnt_perc$day_of_week=="Wednesday",3,
                                                              ifelse(weekdaywise_sent_cnt_perc$day_of_week=="Thursday",4,
                                                                     ifelse(weekdaywise_sent_cnt_perc$day_of_week=="Friday",5,
                                                                            ifelse(weekdaywise_sent_cnt_perc$day_of_week=="Saturday",6,7))))))

weekdaywise_sent_cnt_perc$day_of_week <- factor(weekdaywise_sent_cnt_perc$day_of_week, levels = weekdaywise_sent_cnt_perc$day_of_week[order(weekdaywise_sent_cnt_perc$day_of_week_no)])
weekdaywise_sent_cnt_perc$day_of_week

#plotting this info
q2.3=weekdaywise_sent_cnt_perc %>%
  arrange(day_of_week)%>%
  ggplot(aes(x = interaction(day_of_week,sender_flag), y = percent, fill = factor(sender_flag))) +
  geom_bar(stat = "identity", position = position_dodge())+
  annotate(geom = "text", x = seq_len(nrow(weekdaywise_sent_cnt_perc)), y = -0.01, label = weekdaywise_sent_cnt_perc$day_of_week, size = 3) +
  annotate(geom = "text", c(4.5, 10.5), y = -0.02, label = unique(weekdaywise_sent_cnt_perc$sender_flag), size = 3) +  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),title =element_text(size=8, face='bold'))+
  ggtitle("Comparion of Number of People Reached Over Emails across different days")+
  labs(x="",y="% of no of people reached over email")

#calculating frequencies
#Frequency is time difference between successive emails in a day (measured in hours)

colnames(enron_data_subset)
#sorting by sender and time (Ascending)
enron_data_subset <- enron_data_subset[order(enron_data_subset$sender_lower,enron_data_subset$Cleaned_time_stamp),]

#creating a lag function that pushes a column down
lg <- function(x)c(NA, x[1:(length(x)-1)])

#create a lag of time field
enron_data_subset$time_lag <- lg(enron_data_subset$time)
enron_data_subset$time_stamp_lag=.POSIXct(enron_data_subset$time_lag/1000)
#create a lag of sender field
enron_data_subset$sender_lag <- lg(enron_data_subset$sender_lower)

#Calculcate difference between successive emails in hours
enron_data_subset$time_diff <- difftime(enron_data_subset$Cleaned_time_stamp,enron_data_subset$time_stamp_lag,units="hours")
#calculate time difference between emails within a day for each sender
freq <- sqldf('SELECT sender_lower, avg(time_diff) AS Freq 
              FROM enron_data_subset 
              WHERE time_diff<24 AND time_diff>0  and sender_lag=sender_lower
              GROUP BY sender_lower')

#Round of by 2 digits
freq$Freq <- round(freq$Freq, digits = 2)
#Rename the column to merge
names(freq)[names(freq) == 'sender_lower'] <- 'sender_freq'
#Merge this information back to original enron data set
enron_data_subset=merge(enron_data_subset,freq,by.x="sender_lower",by.y = "sender_freq",all.x = TRUE)

#Classiying ppl with no multiple calls per day as non freq and others as freq
enron_data_subset$multiple_in_day=ifelse(is.na(enron_data_subset$Freq),"Never sent multiple emails in a day","Sent multiple emails in a day")
colnames(enron_data_subset)   
#counting people who've sent mutiple emails per day acorss proflic and non prolific senders
freq_cnt=enron_data_subset%>%group_by(sender_flag,multiple_in_day)%>%summarise(count=n())
#Finding percentage of people who've sent mutiple emails per day acorss proflic and non prolific senders
freq_cnt_perc=group_by(freq_cnt, sender_flag) %>% mutate(percent = count/sum(count))


#Plotting this info
q2.4=ggplot(data=freq_cnt_perc, aes(x = interaction(multiple_in_day,sender_flag), y = percent, fill = factor(sender_flag))) +
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("Prolific vs Non Prolific Sender")+
  labs(x="",y="% of people")+
  annotate(geom = "text", x = seq_len(nrow(freq_cnt_perc)), y = -0.01, label = freq_cnt_perc$multiple_in_day, size = 3) + 
  annotate("text", x = 4, y=1.0, label = "98%")+ annotate("text", x = 3, y=0.4, label = "2%")+ annotate("text", x = 2, y=0.8, label = "72%")+ annotate("text", x = 1, y=0.4, label = "28%") +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),title =element_text(size=8, face='bold'))

#theme(axis.title.x = element_text(size=2,color='black',face='bold'),axis.text.x = element_text(size=5,color='black'),
#      title =element_text(size=8, face='bold'))


#Finding daily averages of number of distinct emails
daywise_dist_msg=enron_data_subset%>%group_by(sender_flag,date.only)%>%summarise(dist_msg_count=n_distinct(msg_identifier))
daywise_dist_msg_avg=daywise_dist_msg%>%group_by(sender_flag)%>%summarise(dist_msg_daily_avg=mean(dist_msg_count))
#Plotting this info
q2.5=ggplot(daywise_dist_msg_avg, aes(x = sender_flag, y = dist_msg_daily_avg,fill = factor(sender_flag))) + 
  geom_bar(stat = "identity", position = position_dodge())+ coord_flip()+
  ggtitle("Prolific vs Non Prolific - Number of distinct emails sent per day")+
  labs(x="",y="Daily Average")

#generating all these plots to a image (png) in the local repository
png(file="answer2.png",bg = "transparent",width=2000,height=1000)
grid.arrange(q2.2,q2.3,q2.4,q2.5)
dev.off()


#Question 3
#converting proflic senders into matrix
ps <- as.matrix(profilic_senders) 


#The following function pulls time when each of the prolific senders have been contacted
y_total <- data.frame()
for(i in 1:length(ps))
{
  if(length(grep(ps[i,1], enron_data_subset$recipients_lower))>0)
  {
    enron_data_subset[grep(ps[i,1], enron_data_subset$recipients_lower), "check"] <- "yes"
    y=enron_data_subset[enron_data_subset$check=="yes",c(1,6)]
    y=y[!is.na(y$sender_lower),]
    y$receiver <- ps[i,1]
    y_total <- rbind(y_total,y) 
    enron_data_subset$check <- NULL
    remove(y)
  }
}

#Extract day of week from time
y_total$day_of_week=weekdays(y_total$Cleaned_time_stamp)
#Classifying them as weekeday weekend
y_total$weekend_weekday=ifelse((y_total$day_of_week=="Saturday" |y_total$day_of_week=="Sunday"),"Weekend","Weekday")

y_total$hour= hour(y_total$Cleaned_time_stamp)#Extract hour from time
y_total$month= month(y_total$Cleaned_time_stamp)#Extract month from time
y_total$year= year(y_total$Cleaned_time_stamp)#Extract year from time
y_total$week= week(y_total$Cleaned_time_stamp)#Extract week from time

#finding yearwise trend of number of people who contact the prolific senders
year_wise_cotact=y_total%>%group_by(year)%>%summarise(contact_count=n_distinct(Cleaned_time_stamp))
ggplot(data=year_wise_cotact, aes(x = year, y = contact_count))+ 
  geom_bar(stat = "identity", position = position_dodge())

#Finding sender wise yearly average of number of times he/she has been contacted
year_sender_wise_contact=y_total%>%group_by(receiver,year)%>%summarise(people_count=n_distinct(sender_lower))
year_sender_wise_avg_contact=year_sender_wise_contact%>%group_by(receiver)%>%summarise(Avg_count=mean(people_count))
#plot this info
ggplot(year_sender_wise_avg_contact, aes(Avg_count) ) +
  geom_histogram(color="white")+
  xlim(c(0,100))

#Based on the above plot Recode evrything above 50%
year_sender_wise_avg_contact$Yearly_Contacts=ifelse(year_sender_wise_avg_contact$Avg_count==0,"0",
                                                    ifelse(year_sender_wise_avg_contact$Avg_count>=1 & year_sender_wise_avg_contact$Avg_count<=10,"1-10",
                                                           ifelse(year_sender_wise_avg_contact$Avg_count>=11 & year_sender_wise_avg_contact$Avg_count<=20,"11-20",  
                                                                  ifelse(year_sender_wise_avg_contact$Avg_count>=21 & year_sender_wise_avg_contact$Avg_count<=30,"21-30",
                                                                         ifelse(year_sender_wise_avg_contact$Avg_count>=31 & year_sender_wise_avg_contact$Avg_count<=40,"31-40",
                                                                                ifelse(year_sender_wise_avg_contact$Avg_count>=41 & year_sender_wise_avg_contact$Avg_count<=50,"41-50",">50"))))))

year_sender_avg_contact_rec=year_sender_wise_avg_contact%>%group_by(Yearly_Contacts)%>%summarise(contact_count=n())

year_sender_avg_contact_rec$Yearly_Contacts <- factor(year_sender_avg_contact_rec$Yearly_Contacts, levels=c("1-10", "11-20","21-30","31-40","41-50",">50"))

ggplot(data=year_sender_avg_contact_rec, aes(x = Yearly_Contacts, y = contact_count))+ 
  geom_bar(stat = "identity", position = position_dodge())



#finding monthwise trend of number of people who contact the prolific senders
month_year_wise_cotact=y_total%>%group_by(month,year)%>%summarise(contact_count=n_distinct(Cleaned_time_stamp))
#calcualting monthly average to make comparison fair
monthly_contact_avg=month_year_wise_cotact%>%group_by(month)%>%summarise(avg_cont=mean(contact_count))
#plotting this info
ggplot(data=monthly_contact_avg, aes(x = factor(month), y = avg_cont))+ 
  geom_bar(stat = "identity", position = position_dodge())

#finding daywise trend of number of people who contact the prolific senders
day_year_wise_cotact=y_total%>%group_by(year,week,day_of_week)%>%summarise(contact_count=n_distinct(Cleaned_time_stamp))
#calcualting daily average to make comparison fair
daily_contact_avg=day_year_wise_cotact%>%group_by(day_of_week)%>%summarise(avg_cont=mean(contact_count))
#Fixing order of days
daily_contact_avg$day_of_week <- factor(daily_contact_avg$day_of_week, levels=c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"))
#plotting this info
ggplot(data=daily_contact_avg, aes(x = factor(day_of_week), y = avg_cont))+ 
  geom_bar(stat = "identity", position = position_dodge())

#Count of distinct people who contacted each of prolific senders

answer3=y_total%>%group_by(receiver)%>%summarise(contact_count=n_distinct(sender_lower))

Histogram = ggplot(data=year_sender_avg_contact_rec, aes(x = Yearly_Contacts, y = contact_count))+ 
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("Number of unique people who contact the prolific senders")+
  labs(x="Yearly average of unique people who contact",y="Number of Senders")

Yearly = ggplot(data=year_wise_cotact, aes(x = year, y = contact_count))+ 
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("Number of unique people who contact the prolific senders across years")+
  labs(x="Year",y="Number of Senders")
Monthly = ggplot(data=monthly_contact_avg, aes(x = factor(month), y = avg_cont))+ 
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("Number of unique people who contact the prolific senders across months")+
  labs(x="Month",y="Number of Senders")
Daily= ggplot(data=daily_contact_avg, aes(x = factor(day_of_week), y = avg_cont))+ 
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("Number of unique people who contact the prolific senders across days")+
  labs(x="Days",y="Number of Senders")

#generating all these plots to a image (png) in the local repository
png(file="answer3.png",bg = "transparent",width=2000,height=1000)
grid.arrange(Histogram,Yearly,Monthly,Daily)
dev.off()
