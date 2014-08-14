library(crqa)
library(lme4)
library(ggplot2)
setwd('~/Dropbox/projects/studies/alex_main_conflict_data')
wsz = 20 # width of the diagonal recurrence profile
  
# various categories
#neg
#a_target_emotion = c('contempt' , 'criticism' , 'stonewalling' , 'belligerence' , 'defensiveness' , 'domineering' , 'anger')
#p_target_emotion = c('contempt' , 'criticism' , 'stonewalling' , 'belligerence' , 'defensiveness' , 'domineering' , 'anger')
#pos
#a_target_emotion = c('affection', 'enthusiasm' , 'humor')
#p_target_emotion = c('affection', 'enthusiasm' , 'humor')
#val
a_target_emotion = c('backchannels' , 'direct expressions of understanding' , 'sentence finishing ' , 'paraphrasing' , 'mirroring with validation'  , 'apologizing' , 'identification' , 'acknowledging different point of view')
p_target_emotion = c('backchannels' , 'direct expressions of understanding' , 'sentence finishing ' , 'paraphrasing' , 'mirroring with validation'  , 'apologizing' , 'identification' , 'acknowledging different point of view')
#int
#a_target_emotion = c('mirroring with interest' , 'positive nonverbal attention' , 'open-ended questions' , 'elaboration and clarification seeking')
#p_target_emotion = c('mirroring with interest' , 'positive nonverbal attention' , 'open-ended questions' , 'elaboration and clarification seeking')

dyads = c(2:4,6:8,10:49)

drps = data.frame()

for (dyad in dyads) {
  
  a = read.table(paste('Cross-Recurrence-Data_cleaned-and-renamed/',dyad,'.txt',sep=''),skip=1,sep='\t')  
  
  parent = as.character(tolower(a[,2])) # column 2, all rows, turn it into string
  adolescent = as.character(tolower(a[,3])) # column 3, all rows, turn it into string
  adolescent[adolescent==""] = " adol_nothing" # wipe out junk
  parent[parent==""] = " parent_nothing" # wipe out junk
    
  # let's get emotion state out (string)
  for (i in 1:length(parent)) {
    parent[i] = unlist(strsplit(parent[i]," "))[2] # without the P / A prefix
    adolescent[i] = unlist(strsplit(adolescent[i]," "))[2]
  }
    
  # let's relabel for pairwise temporal relations (FILTERING IF YOU WANT)
  parent[(parent %in% p_target_emotion)==F] = 'parent_nothing'
  adolescent[(adolescent %in% a_target_emotion)==F] = 'adol_nothing'
  parent[parent %in% p_target_emotion] = 'target'
  adolescent[adolescent %in% a_target_emotion] = 'target'
  
  uniques = unique(c(parent,adolescent)) # get unique affect terms; to be used as #'s below
    
  # let's generate unique numbers
  pix = (1:length(parent))*0;aix = (1:length(parent))*0 # default to 0's
  for (i in 1:length(parent)) {
    pix[i] = which(uniques==parent[i]) # which = get the position in uniques to numerically identify...
    aix[i] = which(uniques==adolescent[i]) # ...the affect categories
  }    
  
  # make sure that there are targets occurring in this dyad, and enough data (wsz)
  if (sum(pix==which(uniques=="target"))>0 & sum(aix==which(uniques=="target"))>0 & length(pix)>wsz*2) {
    drp_results = drpdfromts(pix,aix,ws=wsz,datatype="categorical")      
    if (dyad==2) {
      drps = data.frame(dyad,-wsz:wsz,drp_results$profile) # if the first dyad, start the data frame
    }
    else {
      drps = rbind(drps,data.frame(dyad,-wsz:wsz,drp_results$profile)) # subsequently, rbind it
    }
  }
}

colnames(drps) = list('dyad,','Lag','RR')
drps$quadratic = drps$Lag^2 # look at the quadratic term, assuming organization around Lag = 0
drps$dyad = as.factor(drps$dyad)
lmo = lmer(RR~Lag+quadratic+(1+Lag+quadratic|dyad),data=drps)
coefs = data.frame(summary(lmo)@coefs)
coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
coefs

p <- ggplot(drps,aes(Lag,RR)) 
p + stat_smooth(method = "loess", formula = y ~ x) # spec loess because > 1,000 points
# http://docs.ggplot2.org/0.9.3.1/stat_smooth.html





