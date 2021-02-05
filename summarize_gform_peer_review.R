require(pacman)
p_load(ggplot2,tidyverse,reshape2)

entries<-read_csv("story entrants.csv")
x<-read_csv(file.choose())
names(x)
names(x)<-c("timestamp","reviewer","revRole","story","likes","choices","creativity","orthography","engagement","major","minor","kudos")
(x2.0<-x %>% group_by(story) %>% summarise(nReviews=n(),avg_creativity=mean(creativity),avg_orthography=mean(orthography),avg_engagement=mean(engagement) ))

#add story title and author
x2<-left_join(x2.0,entries,by=c("story"="Story #"))

#////////////////////////////////////////////////////
# Review summary...which stories still need reviews?

#Threshold number of reviews we want for every story
thresh<-3
x2$`Needs More Ratings?`<-as.factor(ifelse(x2$nReviews<thresh,"Yes","No"))
ggplot(x2,aes(y=nReviews,x=story,fill=`Needs More Ratings?`))+geom_col()+theme_bw()+scale_x_continuous(breaks=seq(1,13,1),labels=1:13)+scale_y_continuous(breaks=seq(0,max(x2$nReviews),3),minor_breaks = 1:x2$nReviews)+scale_fill_manual(values=c("gray10","#63c0f5"))+labs(title=paste0("Please review stories in blue, which do not have at least ", thresh," ratings"))+xlab("Story Number")+ylab("Number of Reviews")+geom_abline(intercept=thresh,slope=0,linetype="dashed")+theme(text=element_text(size=18))
ggsave("num_reviews_barplot.jpg")


#////////////////////////////////////////////////////
# Plot bargraphs of individual and overall score averages
x2$overall<-x2$avg_creativity+x2$avg_orthography+x2$avg_engagement
x3<-x2 %>% select(story,avg_creativity,avg_orthography,avg_engagement,overall) %>% melt(id="story")
ggplot(x3,aes(x=story,y=value))+geom_col()+facet_wrap(~variable)+theme_bw()+scale_x_continuous(breaks=seq(1,13,1),labels=1:13)+scale_fill_manual(values=c("gray10","#63c0f5"))+theme(text=element_text(size=18))


#////////////////////////////////////////////////////
# Summarize general public voting results
x2 %>% select(story,Title,Author,nReviews,overall) %>% arrange(desc(overall))

#////////////////////////////////////////////////////
# Summarize finalist judging results
finalJudges<-c("Tahlia Kirk","Dana Fraedrich")
semifinalists<-c(1,2,5,7,9,10,11)

#professional judge results
(x4<-x %>% group_by(story) %>% filter(reviewer%in%finalJudges&story%in%semifinalists) %>% summarise(nReviews=n(),avg_creativity=mean(creativity),avg_orthography=mean(orthography),avg_engagement=mean(engagement) ) %>% mutate(overall=rowSums(.[c("avg_creativity","avg_orthography","avg_engagement")])) %>% arrange(desc(overall)) %>% left_join(.,entries,by=c("story"="Story #"))%>% select(story,Title,Author,nReviews,overall)  )

#public results minus judge results
x %>% group_by(story) %>% filter(!reviewer%in%finalJudges) %>% summarise(nReviews=n(),avg_creativity=mean(creativity),avg_orthography=mean(orthography),avg_engagement=mean(engagement) ) %>% mutate(overall=rowSums(.[c("avg_creativity","avg_orthography","avg_engagement")]))%>% left_join(.,entries,by=c("story"="Story #")) %>% select(story,Title,Author,nReviews,overall) %>% arrange(desc(overall))

# x %>% filter(reviewer%in%finalJudges)
# x2<-left_join(x2.0,entries,by=c("story"="Story #"))

# Aggregate feedback for each student
x$judge_TF<-ifelse(x$reviewer%in%finalJudges,1,0)
feedback<-x %>% group_by(story) %>% arrange(story,desc(judge_TF)) 


