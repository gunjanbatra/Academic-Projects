##################################JOB RECOMMENDER SYSTEM#######################################
#Author: Gunjan Batra
###############################################################################################
library(plyr) 
library(tm)
library(topicmodels)
library(MASS)
library(stringr)
library(text2vec)
library(stringdist)
library(Matrix)
library(matrixStats)


rm(list=ls())

setwd("C:/Users/gunjan/Desktop/Spring 2017/JobData/")
Usersdata = read.delim('users.tsv', header = TRUE)
new_Usersdata<-Usersdata[which(Usersdata$WindowID==6),]
new_Usersdata2<-new_Usersdata[which(new_Usersdata$Split=='Train'),]
Usersdata<-new_Usersdata2

UsersHistory = read.delim('user_history.tsv', header = TRUE)
new_Usershistory<-UsersHistory[which(UsersHistory$WindowID==6),]
new_Usershistory2<-new_Usershistory[which(new_Usershistory$Split=='Train'),]
Usershistorydata<-new_Usershistory2
head(Usershistorydata)
summary(Usershistorydata)

apps<-read.delim(file = 'apps.tsv', header = TRUE)
#summary(apps)
head(apps)
dim(apps)

#TestUsers = read.delim('test_users.tsv', header = TRUE)
#apps = read.delim('apps.tsv', header = TRUE)
#There are 7 jobs file , Right now loading only one Jobs file
jobs_data<-read.csv('finalJobs.csv', header = TRUE, stringsAsFactors = FALSE)
#*** getting the flavour of the data **#
#head(Usersdata)
#head(UsersHistory)
#head(TestUsers)
#head(apps)
head(jobs_data)
#fix(jobs_data)



###############Pre-Process on JOBS to approach LDA
#Before Apply LDA to our datasets, We have to do some preprocessing . Lets first pick Jobs dataset


#load text mining library


#set working directory
setwd("C:/Users/gunjan/Desktop/Spring 2017/JobData")


#load files into corpus
#filenames<-read.delim("C:/Users/gunjan/Desktop/Spring 2017/JobData", header = TRUE)
jobdesc=jobs_data$Description


#read files into a character vector
files = as.vector(jobdesc)

#create corpus from vector
docs <- Corpus(VectorSource(files))


#inspect a particular document in corpus
writeLines(as.character(docs[[11]]))


#start preprocessing
#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))
docs1=docs # backup
#docs=docs1

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, "", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "'")


#docs <- tm_map(docs, toSpace, "<br>")
#Lets Check Again

writeLines(as.character(docs[[11]]))

#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove whitespace
docs <- tm_map(docs, stripWhitespace)

#remove stopwords

docs <- tm_map(docs, removeWords, stopwords("english"))
typeof(docs)

#Lets Check Again
writeLines(as.character(docs[[11]]))

#At tthis point backup
docs2=docs
#docs=docs2

#define and eliminate all custom stopwords
# myStopwords<-c("the", "please", "must", "can", "say", "one", "way", "use", "from" , "appropriate","required","high","limited","well",
#                "at", "him", "her", "all", "into", "also", "rather", "tend", "amp","help","ability","opportunities","plan","andor","degree",
#                " br " , " n " ," brbr ", "???", "this", "have", "for", "with", "and", "will","can", "say", "one", "way", "use", "from" , "at", 
#                "him", "her", "all", "into", "also", "nurse","team","build","offers","national","duties","rather", "tend", "brbsource","please",
#                "must","description","will","would","sentinel","rbr","patient","benefits","life","strong","service", "resume","plus","great",
#                "brbrsource","sunsentinel","seeking","refer","work","seeking","job","florida","orlando","styletextdecoration","spanprp","lir",
#                "ulr","brr","styletextalign","experience","years","span","working","including","new","center","position","looking","skills","knowledge","tmobile","company","product","customer")
# docs <- tm_map(docs, removeWords, myStopwords)
myStopwords<-c("the", "please", "must", "can", "say", "one", "way", "use", "from" , "appropriate","required","high","limited","well",
               "at", "him", "her", "all", "into", "also", "rather", "tend", "amp","help","ability","opportunities","plan","andor","degree",
               " br " , " n " ," brbr ", "this", "have", "for", "with", "and", "will",
               "can", "say", "one", "way", "use", "from" , "at", "him", "her", "all", "into", "also", "nurse","team","build","offers","national","duties",
               "rather", "tend", "brbsource","please","must","description","will","would","sentinel","rbr","patient","benefits","life","strong","service",
               "resume","plus","great","brbrsource","sunsentinel","seeking","refer","work","seeking","job","florida","orlando","styletextdecoration","spanprp","lir","ulr","brr",
               "styletextalign","experience","years","span","working","including","new","center","position","looking","skills","knowledge","tmobile","company","product","customer")


docs <- tm_map(docs, removeWords, myStopwords)

#Lets check again
writeLines(as.character(docs[[4]]))


#Stem document
#docs <- tm_map(docs,stemDocument)

#We now plan to create document-term matrix
#on which we can apply our LDA model on



dtm <- DocumentTermMatrix(docs)

findFreqTerms(dtm, 5)

#convert rownames to filenames
#rownames(dtm) <- filenames
#collapse matrix by summing over columns
#freq <- colSums(as.matrix(dtm))
#length should be total number of terms
#length(freq)
#create sort order (descending)
#ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
#freq[ord]
#write.csv(freq[ord],"word_freq.csv")


rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm  <- dtm[rowTotals> 0, ]
empty.rows <- dtm[rowTotals == 0, ]$dimnames[1][[1]]
docs <- docs[-as.numeric(empty.rows)]





#load topic models library



#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE


#Number of topics
k <- 6
#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#dtm.new   <- dtm[rowTotals> 10, ]

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))



ldaOut.terms <- as.matrix(terms(ldaOut,10))

ldaOut.terms
dim(ldaOut.topics)




####################################JOB RANKING############################################
#insert a row column
jobs_data$RowID <- seq.int(nrow(jobs_data))
dim(jobs_data)
jobsiddata<-jobs_data[c("JobID", "RowID")]   # inserting Row ID to make jobsiddata
dim(jobsiddata)

ldatopics<-data.frame(ldaOut.topics)
ldatopics$RowID<-seq.int(nrow(ldatopics))
dim(ldatopics)

jobid_topic<-merge(jobsiddata, ldatopics, by="RowID")
head(jobid_topic)

###############################Cluster1######################################
clust1_jobID<-jobid_topic[which(ldaOut.topics==1),]
jobs_data_new<-jobs_data[,c(-4,-5)]  # remove Desc column and Req column
cluster1_jobs<-merge(clust1_jobID, jobs_data_new, by="JobID")

apps_cluster<-data.frame(merge(apps, jobid_topic, by="JobID" ))
#which(apps_cluster$UserID==760747)

UserID<-unique(apps_cluster$UserID)

#For each job cluster, a binary classifier is trained and
#will be used to predict if a new candidate belongs or not
#to that job cluster (a candidate may belong to different Job
#Clusters). To train the classifier for a job cluster, all candidates
#with a matching with one of the job offers represented
#by it are used as positive examples and the rest as negative
#ones.


#dat <- dat[, c("A", "C", "B")]

#UserID, Cluster1, Cluster2, Cluster3, Cluster4, Cluster5
relation_cluster1<-data.frame(UserID)
relation_cluster1["Clust1"] <- NA
for(i in 1:nrow(relation_cluster1)){
  #print(x)
  temp<-apps_cluster[which(apps_cluster$UserID==relation_cluster1[i,]$UserID),]
  for(j in nrow(temp)){
    if(temp[j,]$ldaOut.topics==1){
      relation_cluster1[i,]$Clust1<-1
    }else{
      relation_cluster1[i,]$Clust1<-0
    }
  }
}
cluster1_user<-merge(relation_cluster1, Usersdata, by="UserID") #user data
cluster1_user<-cluster1_user[which(cluster1_user$Clust1==1),] 

# userclusterrelation= apps_cluster[,c("UserID","ldaOut.topics")]
# head(userclusterrelation)
# 
# userclusterrelation_table=table(userclusterrelation)
# 
# head(userclusterrelation_table)



#jobs_data_new<-jobs_data_new[,-4] #jobs data
#cluster1_user[1,]$Country==jobs_data_new$Country 



#calculate Similarity between Country of User and Jobs
# User data is cluster1_user and jobs data is jobs_data_new

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
#data_clean1 = prep_fun(cluster1_user[1,]$Country)
#data_clean2 = prep_fun(jobs_data_new[1,]$Country)

#stringdist(data_clean1, data_clean2, method='cosine', q=2)

clean_usercountry_vector<-sapply(cluster1_user$Country, prep_fun)
clean_jobcountry_vector<-sapply(cluster1_jobs$Country, prep_fun)

dist_matrix_country <- matrix( nrow = length(clean_usercountry_vector), 
                               ncol =length(clean_jobcountry_vector))
for (f in 1:length(clean_usercountry_vector)){
  for(g in 1:length(clean_jobcountry_vector)){
    dist_matrix_country[f,g]<-stringdist(clean_usercountry_vector[f],clean_jobcountry_vector[g],
                                         method='cosine',q=2)
  }
}

#sim distance on basis of city
clean_usercity_vector<-sapply(cluster1_user$City, prep_fun)
clean_jobcity_vector<-sapply(cluster1_jobs$City, prep_fun)

dist_matrix_city <- matrix(nrow = length(clean_usercity_vector), ncol =length(clean_jobcity_vector))
for (f in 1:length(clean_usercity_vector)){
  for(g in 1:length(clean_jobcity_vector)){
    dist_matrix_city[f,g]<-stringdist(clean_usercity_vector[f],clean_jobcity_vector[g],
                                      method='cosine',q=2)
  }
}

#sim distance on basis of zip
clean_userzip_vector<-sapply(as.character(cluster1_user$ZipCode), prep_fun)
clean_jobzip_vector<-sapply(as.character(cluster1_jobs$Zip5), prep_fun)

dist_matrix_zip <- matrix(nrow = length(clean_userzip_vector), ncol =length(clean_jobzip_vector))
for (f in 1:length(clean_userzip_vector)){
  for(g in 1:length(clean_jobzip_vector)){
    dist_matrix_zip[f,g]<-stringdist(clean_userzip_vector[f],clean_jobzip_vector[g],
                                     method='cosine',q=2)
  }
}

#sim distance on basis of state
clean_userstate_vector<-sapply(cluster1_user$State, prep_fun)
clean_jobstate_vector<-sapply(cluster1_jobs$State, prep_fun)

dist_matrix_state <- matrix(nrow = length(clean_userstate_vector), ncol =length(clean_jobstate_vector))
for (f in 1:length(clean_userstate_vector)){
  for(g in 1:length(clean_jobstate_vector)){
    dist_matrix_state[f,g]<-stringdist(clean_userstate_vector[f],clean_jobstate_vector[g],
                                       method='cosine',q=2)
  }
}



#calculate Similarity between Title of User and Jobs
#User data is cluster1_user_jobtitleadded
m<-cluster1_user$UserID #picking useful UserID in x
UserHist2<-Usershistorydata
totaljobs <- aggregate(JobTitle~UserID,UserHist2,paste, collapse = ",")
cluster1_user_jobtitleadded<-merge(x=cluster1_user, y=totaljobs, by="UserID", all.x=TRUE)

clean_usertitle_vector<-sapply(cluster1_user_jobtitleadded$JobTitle, prep_fun)
clean_jobtitle_vector<-sapply(cluster1_jobs$Title, prep_fun)

dist_matrix_title <- matrix( nrow = length(clean_usertitle_vector), ncol =length(clean_jobtitle_vector))
for (f in 1:length(clean_usertitle_vector)){
  for(g in 1:length(clean_jobtitle_vector)){
    dist_matrix_title[f,g]<-stringdist(clean_usertitle_vector[f],clean_jobtitle_vector[g],
                                       method='cosine',q=2)
  }
}



Clust1_distancematrix<-(dist_matrix_title+
                          dist_matrix_state+
                          dist_matrix_zip+ 
                          dist_matrix_city+
                          dist_matrix_country)

Clust1_distancematrix

#Getting the Column Index in x for the jobs with max distance value
B = Clust1_distancematrix 
#i=nrow(B)
#j=ncol(B)
x <- matrix(data=NA,nrow=nrow(B),ncol=5)

for(i in 1:nrow(B)){
  m<-tail(sort.int(B[i,],index.return = TRUE)$ix,5)
  for(j in 1:5){
    x[i,j]<-m[j]
  }
}

y <- matrix(data=NA,nrow=nrow(B),ncol=5)
#Getting the Actual User Id from User Data 
#and Job IDs from the Jobs Data

for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    y[i,j]<-cluster2_jobs[x[i,j],]$JobID
  }
}
################################Cluster 2############################################
Clust2_jobID<-jobid_topic[which(ldaOut.topics==2),]
jobs_data_new<-jobs_data[,c(-4,-5)]  # remove Desc column and Req column
cluster2_jobs<-merge(Clust2_jobID, jobs_data_new, by="JobID")

apps_cluster<-data.frame(merge(apps, jobid_topic, by="JobID" ))
#which(apps_cluster$UserID==760747)

UserID<-unique(apps_cluster$UserID)

#For each job cluster, a binary classifier is trained and
#will be used to predict if a new candidate belongs or not
#to that job cluster (a candidate may belong to different Job
#Clusters). To train the classifier for a job cluster, all candidates
#with a matching with one of the job offers represented
#by it are used as positive examples and the rest as negative
#ones.


#dat <- dat[, c("A", "C", "B")]

#UserID, Cluster1, Cluster2, Cluster3, Cluster4, Cluster5
relation_cluster2<-data.frame(UserID)
relation_cluster2["Clust2"] <- NA
for(i in 1:nrow(relation_cluster2)){
  #print(x)
  temp<-apps_cluster[which(apps_cluster$UserID==relation_cluster2[i,]$UserID),]
  for(j in nrow(temp)){
    if(temp[j,]$ldaOut.topics==2){
      relation_cluster2[i,]$Clust2<-1
    }else{
      relation_cluster2[i,]$Clust2<-0
    }
  }
}
cluster2_user<-merge(relation_cluster2, Usersdata, by="UserID") #user data
cluster2_user<-cluster2_user[which(cluster2_user$Clust2==1),] 


# userclusterrelation= apps_cluster[,c("UserID","ldaOut.topics")]
# head(userclusterrelation)
# 
# userclusterrelation_table=table(userclusterrelation)
# 
# head(userclusterrelation_table)



#jobs_data_new<-jobs_data_new[,-4] #jobs data
#cluster1_user[1,]$Country==jobs_data_new$Country 



#calculate Similarity between Country of User and Jobs
# User data is cluster1_user and jobs data is jobs_data_new

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
#data_clean1 = prep_fun(cluster1_user[1,]$Country)
#data_clean2 = prep_fun(jobs_data_new[1,]$Country)

#stringdist(data_clean1, data_clean2, method='cosine', q=2)

clean_usercountry_vector<-sapply(cluster2_user$Country, prep_fun)
clean_jobcountry_vector<-sapply(cluster2_jobs$Country, prep_fun)

dist_matrix_country <- matrix( nrow = length(clean_usercountry_vector), 
                               ncol =length(clean_jobcountry_vector))
for (f in 1:length(clean_usercountry_vector)){
  for(g in 1:length(clean_jobcountry_vector)){
    dist_matrix_country[f,g]<-stringdist(clean_usercountry_vector[f],clean_jobcountry_vector[g],
                                         method='cosine',q=2)
  }
}

#sim distance on basis of city
clean_usercity_vector<-sapply(cluster2_user$City, prep_fun)
clean_jobcity_vector<-sapply(cluster2_jobs$City, prep_fun)

dist_matrix_city <- matrix(nrow = length(clean_usercity_vector), ncol =length(clean_jobcity_vector))
for (f in 1:length(clean_usercity_vector)){
  for(g in 1:length(clean_jobcity_vector)){
    dist_matrix_city[f,g]<-stringdist(clean_usercity_vector[f],clean_jobcity_vector[g],
                                      method='cosine',q=2)
  }
}

#sim distance on basis of zip
clean_userzip_vector<-sapply(as.character(cluster2_user$ZipCode), prep_fun)
clean_jobzip_vector<-sapply(as.character(cluster2_jobs$Zip5), prep_fun)

dist_matrix_zip <- matrix(nrow = length(clean_userzip_vector), ncol =length(clean_jobzip_vector))
for (f in 1:length(clean_userzip_vector)){
  for(g in 1:length(clean_jobzip_vector)){
    dist_matrix_zip[f,g]<-stringdist(clean_userzip_vector[f],clean_jobzip_vector[g],
                                     method='cosine',q=2)
  }
}

#sim distance on basis of state
clean_userstate_vector<-sapply(cluster2_user$State, prep_fun)
clean_jobstate_vector<-sapply(cluster2_jobs$State, prep_fun)

dist_matrix_state <- matrix(nrow = length(clean_userstate_vector), ncol =length(clean_jobstate_vector))
for (f in 1:length(clean_userstate_vector)){
  for(g in 1:length(clean_jobstate_vector)){
    dist_matrix_state[f,g]<-stringdist(clean_userstate_vector[f],clean_jobstate_vector[g],
                                       method='cosine',q=2)
  }
}



#calculate Similarity between Title of User and Jobs
#User data is cluster1_user_jobtitleadded
m<-cluster2_user$UserID #picking useful UserID in x
UserHist2<-Usershistorydata
totaljobs <- aggregate(JobTitle~UserID,UserHist2,paste, collapse = ",")
cluster2_user_jobtitleadded<-merge(x=cluster2_user, y=totaljobs, by="UserID", all.x=TRUE)

clean_usertitle_vector<-sapply(cluster2_user_jobtitleadded$JobTitle, prep_fun)
clean_jobtitle_vector<-sapply(cluster2_jobs$Title, prep_fun)

dist_matrix_title <- matrix( nrow = length(clean_usertitle_vector), ncol =length(clean_jobtitle_vector))
for (f in 1:length(clean_usertitle_vector)){
  for(g in 1:length(clean_jobtitle_vector)){
    dist_matrix_title[f,g]<-stringdist(clean_usertitle_vector[f],clean_jobtitle_vector[g],
                                       method='cosine',q=2)
  }
}



Clust2_distancematrix<-(dist_matrix_title+
                          dist_matrix_state+
                          dist_matrix_zip+ 
                          dist_matrix_city+
                          dist_matrix_country)
Clust2_distancematrix

#Getting the Column Index in x for the jobs with max distance value
B = Clust2_distancematrix 
#i=nrow(B)
#j=ncol(B)
x <- matrix(data=NA,nrow=nrow(B),ncol=5)

for(i in 1:nrow(B)){
  m<-tail(sort.int(B[i,],index.return = TRUE)$ix,5)
  for(j in 1:5){
    x[i,j]<-m[j]
  }
}

y <- matrix(data=NA,nrow=nrow(B),ncol=5)
#Getting the Actual User Id from User Data 
#and Job IDs from the Jobs Data

for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    y[i,j]<-cluster2_jobs[x[i,j],]$JobID
  }
}


####################################################################
#Similarly we repeat the ranking process for Cluster 3- Cluster 6
#####################################################################