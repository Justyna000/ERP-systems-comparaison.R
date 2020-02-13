# COMPARAISON OF DIFFERENT ERP SOFTWARES FOR FINANCE 
# AUTHOR: JUSTYNA DMOWSKA

# PART 1: DATA LOAD
# Based on the reviews form https://www.trustradius.com/erp (access: 2/9/2020)

# Loading all necessary libraries
library(rvest)
library(magrittr)
library(tm)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(wordcloud)
library(reshape2)

# ORACLE ERP CLOUD 
# Load website
ws1 <- read_html("https://www.trustradius.com/products/oracle-erp-cloud/reviews")

# Create dataframe with opinions
df1 <- ws1 %>% 
  html_nodes(".ugc") %>%
  html_text()
View(df1)

# transform to data frame and change column name
df1 <- df1 %>% unlist() %>%  as.data.frame()
colnames(df1)[1] <- "ORACLE_ERP_CLOUD"
View(df1)


# Oracle eBusiness Suite
# Load website
ws3 <- read_html("https://www.trustradius.com/products/oracle-ebusiness-suite/reviews")

# Create dataframe with opinions
df3 <- ws3 %>% 
  html_nodes(".ugc") %>%
  html_text()
View(df3)

# transform to data frame and change column name
df3 <- df3 %>% unlist() %>%  as.data.frame()
colnames(df3)[1] <- "Oracle_eBusiness_Suite"
View(df3)


# NetSuite 
# Load website
ws4 <- read_html("https://www.trustradius.com/products/netsuite/reviews")

# Create dataframe with opinions
df4 <- ws4 %>% 
  html_nodes(".ugc") %>%
  html_text()
View(df4)

# transform to data frame and change column name
df4 <- df4 %>% unlist() %>%  as.data.frame()
colnames(df4)[1] <- "NetSuite"
View(df4)

# Sage Intacct 
# Load website
ws5 <- read_html("https://www.trustradius.com/products/intacct/reviews")

# Create dataframe with opinions
df5 <- ws5 %>% 
  html_nodes(".ugc") %>%
  html_text()
View(df5)

# transform to data frame and change column name
df5 <- df5 %>% unlist() %>%  as.data.frame()
colnames(df5)[1] <- "Sage_Intacct"
View(df5)


# SAP S/4HANA
# Load website
ws6 <- read_html("https://www.trustradius.com/products/sap-s-4hana/reviews")

# Create dataframe with opinions
df6 <- ws6 %>% 
  html_nodes(".ugc") %>%
  html_text()
View(df6)

# transform to data frame and change column name
df6 <- df6 %>% unlist() %>%  as.data.frame()
colnames(df6)[1] <- "SAP_S4HANA"
View(df6)


#SAP ERP
# Load website
ws7 <- read_html("https://www.trustradius.com/products/sap-erp/reviews#2")

# Create dataframe with opinions
df7 <- ws7 %>% 
  html_nodes(".ugc") %>%
  html_text()
View(df7)

# transform to data frame and change column name
df7 <- df7 %>% unlist() %>%  as.data.frame()
colnames(df7)[1] <- "SAP_ERP"
View(df7)


# Odoo
# Load website
ws9 <- read_html("https://www.trustradius.com/products/odoo/reviews#2")

# Create dataframe with opinions
df9 <- ws9 %>% 
  html_nodes(".ugc") %>%
  html_text()
View(df9)

# transform to data frame and change column name
df9 <- df9 %>% unlist() %>%  as.data.frame()
colnames(df9)[1] <- "Odoo"
View(df9)

#############################################################################################################

# After creating 7 data frames for 7 different softwares - TOKENIZING
# Dividing softwares into to groups - softwares for big corporations and for small and medium companies

#df1 - ORACLE ERP CLOUD 
#df2 - ORACLE FINANCIALS 
#df3 - Oracle eBusiness Suite
#df4 - NetSuite 
#df5 - Sage Intacct
#df6 - SAP S/4HANA
#df7 - SAP ERP
#df8 - Microsoft Dynamics
#df9 - Odoo

# Big corporations:
#df1 - ORACLE ERP CLOUD 
#df3 - Oracle eBusiness Suite
#df6 - SAP S/4HANA
#df7 - SAP ERP


# Changing factors to characters to be able to tokenize
df1$ORACLE_ERP_CLOUD <- as.character(df1$ORACLE_ERP_CLOUD)
df3$Oracle_eBusiness_Suite <- as.character(df3$Oracle_eBusiness_Suite)
df4$NetSuite <- as.character(df4$NetSuite)
df5$Sage_Intacct <- as.character(df5$Sage_Intacct)
df6$SAP_S4HANA <- as.character(df6$SAP_S4HANA)
df7$SAP_ERP <- as.character(df7$SAP_ERP)
df9$Odoo <- as.character(df9$Odoo)


# Small and medium corporations:
#df4 - NetSuite 
#df5 - Sage Intacct
#df9 - Odoo

# Tokenize NetSuite df
my_df4_token <- df4 %>%
  unnest_tokens(word,NetSuite) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

# Tokenize Sage Intacct
my_df5_token <- df5 %>%
  unnest_tokens(word,Sage_Intacct) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

# Tokenize Odoo
my_df9_token <- df9 %>%
  unnest_tokens(word,Odoo) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

# Creating frequency matrix for 3 softwares - NetSuite as a benchmark
frequency2 <- bind_rows(mutate(my_df4_token, team="NetSuite"),
                        mutate(my_df5_token, team= "Sage_Intacct"),
                        mutate(my_df9_token, team="Odoo")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(team, word) %>%
  group_by(team) %>%
  mutate(proportion = n/sum(n))%>% #create new variable of % of frequency in the whole book
  select(-n) %>%
  spread(team, proportion) %>%
  gather(team, proportion, `Sage_Intacct`, `Odoo`)


View(frequency2)

# Creating correlogram 
ggplot(frequency2, aes(x=proportion, y=`NetSuite`, 
                       color = abs(`NetSuite`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~team, ncol=2)+
  theme(legend.position = "none",  plot.title = element_text(hjust = 0.5))+
  labs(y= "NetSuite", x=NULL) +
  ggtitle("Correlogram - ERP for Small and Medium Companies")


# Correlation tests - S&M companies
cor.test(data=frequency2[frequency2$team == "Sage_Intacct",],
         ~proportion + `NetSuite`)

cor.test(data=frequency2[frequency2$team == "Odoo",],
         ~proportion + `NetSuite`)

#############################################################################################################

# Big corporations:
#df1 - ORACLE ERP CLOUD 
#df3 - Oracle eBusiness Suite
#df6 - SAP S/4HANA
#df7 - SAP ERP

# Tokenize ORACLE ERP CLOUD 
my_df1_token <- df1 %>%
  unnest_tokens(word,ORACLE_ERP_CLOUD) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

# Tokenize Oracle eBusiness Suite 
my_df3_token <- df3 %>%
  unnest_tokens(word, Oracle_eBusiness_Suite) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

# Tokenize SAP S/4HANA
my_df6_token <- df6 %>%
  unnest_tokens(word, SAP_S4HANA) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

# Tokenize SAP ERP
my_df7_token <- df7 %>%
  unnest_tokens(word, SAP_ERP) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

# Creating frequency matrix for 3 softwares - Oracle Cloud as a benchmark
frequency3 <- bind_rows(mutate(my_df1_token, team="ORACLE_ERP_CLOUD"),
                        mutate(my_df3_token, team="Oracle_eBusiness_Suite"),
                        mutate(my_df6_token, team="SAP_S4HANA"),
                        mutate(my_df7_token, team="SAP_ERP")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(team, word) %>%
  group_by(team) %>%
  mutate(proportion = n/sum(n))%>% #create new variable of % of frequency in the whole book
  select(-n) %>%
  spread(team, proportion) %>%
  gather(team, proportion, `Oracle_eBusiness_Suite`, `SAP_S4HANA`, `SAP_ERP`)

View(frequency3)

# Creating correlogram
ggplot(frequency3, aes(x=proportion, y=`ORACLE_ERP_CLOUD`, 
                       color = abs(`ORACLE_ERP_CLOUD`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~team, ncol=3)+
  theme(legend.position = "none",  plot.title = element_text(hjust = 0.5)) +
  labs(y= "ORACLE_ERP_CLOUD", x=NULL) +
  ggtitle("Correlogram - ERP for Big Corporations")
  

# Correlation tests
cor.test(data=frequency3[frequency3$team == "Oracle_eBusiness_Suite",],
         ~proportion + `ORACLE_ERP_CLOUD`)

cor.test(data=frequency3[frequency3$team == "SAP_S4HANA",],
         ~proportion + `ORACLE_ERP_CLOUD`)

cor.test(data=frequency3[frequency3$team == "SAP_ERP",],
         ~proportion + `ORACLE_ERP_CLOUD`)

#############################################################################################################

# PART 2: Which words are used most frequently in the reviews of the software?

# Softwares for big corporations - creating bar charts with the most frequently used words
gg1 <- my_df1_token %>%
            filter(n > 15) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n)) +
            geom_col(fill = "green") +
            xlab(NULL) +
            coord_flip() +
            ggtitle("ORACLE_ERP_CLOUD")

gg2 <- my_df3_token %>%
            filter(n > 15) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n)) +
            geom_col(fill = "grey") +
            xlab(NULL) +
            coord_flip() +
            ggtitle("Oracle_eBusiness_Suite")

gg3 <- my_df6_token %>%
            filter(n > 15) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n)) +
            geom_col(fill = "blue") +
            xlab(NULL) +
            coord_flip() +
            ggtitle("SAP_S4HANA")

gg4 <- my_df7_token %>%
            filter(n > 20) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n)) +
            geom_col(fill = "red") +
            xlab(NULL) +
            coord_flip() +
            ggtitle("SAP_ERP")

# Arranging all graphs on one graph
grid.arrange(gg1, gg2, gg3, gg4, nrow = 2)


# Softwares for small and medium companies - creating bar charts with the most frequently used words
gg5 <-my_df4_token %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "green") +
  xlab(NULL) +
  coord_flip() +
  ggtitle("NetSuite")

gg6 <- my_df5_token %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "grey") +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Sage Intacct")

gg7 <- my_df9_token %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue") +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Odoo")


# Arranging all graphs on one graph
grid.arrange(gg5, gg6, gg7, nrow = 1)


#############################################################################################################

# PART 3: SENTIMENT ANALYSIS BASED ON BING

# Big corporations:
#df1 - ORACLE ERP CLOUD 
#df3 - Oracle eBusiness Suite
#df6 - SAP S/4HANA
#df7 - SAP ERP

# Getting sentiments from bing
# Oracle ERP Cloud
bing_counts2 <- my_df1_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Grouping by sentiment - positive/negative
new2 <- bing_counts2 %>%
  group_by(sentiment) %>%
  count(sentiment, sort=TRUE)

# Creating plots showing the % of positive vs negative words used in reviews of each software
new2 <- as.data.frame(new2)
new2$pc <- percent(new2$n/sum(new2$n), 1)
fill <- c("red3", "forestgreen")
p2 <- ggplot() + geom_bar(aes(y = sentiment, x = pc, fill = sentiment), data = new2,
                          stat="identity") + ggtitle("SENTIMENTS Oracle ERP Cloud") + 
                          scale_fill_manual(values=fill) + 
                          xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2

# Oracle eBusiness Suite
bing_counts3 <- my_df3_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

new3 <- bing_counts3 %>%
  group_by(sentiment) %>%
  count(sentiment, sort=TRUE)

new3 <- as.data.frame(new3)
new3$pc <- percent(new3$n/sum(new3$n), 1)

fill <- c("red3", "forestgreen")
p3 <- ggplot() + geom_bar(aes(y = sentiment, x = pc, fill = sentiment), data = new3,
                          stat="identity") + ggtitle("SENTIMENTS Oracle eBusiness Suite") + 
                          scale_fill_manual(values=fill) + 
                          xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p3


#df6 - SAP S/4HANA
bing_counts4 <- my_df6_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

new4 <- bing_counts4 %>%
  group_by(sentiment) %>%
  count(sentiment, sort=TRUE)

new4 <- as.data.frame(new4)
new4$pc <- percent(new4$n/sum(new4$n), 1)

fill <- c("red3", "forestgreen")
p4 <- ggplot() + geom_bar(aes(y = sentiment, x = pc, fill = sentiment), data = new4,
                          stat="identity") + ggtitle("SENTIMENTS SAP S/4HANA") + 
                          scale_fill_manual(values=fill) + 
                          xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    
p4

#df7 - SAP ERP
bing_counts5 <- my_df7_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

new5 <- bing_counts5 %>%
  group_by(sentiment) %>%
  count(sentiment, sort=TRUE)

new5 <- as.data.frame(new5)
new5$pc <- percent(new5$n/sum(new5$n), 1)

fill <- c("red3", "forestgreen")
p5 <- ggplot() + geom_bar(aes(y = sentiment, x = pc, fill = sentiment), data = new5,
                          stat="identity") + ggtitle("SENTIMENTS SAP ERP") + 
                          scale_fill_manual(values=fill) + 
                          xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p5


# Arranging all graphs on one graph - ERP for big corporations
grid.arrange(p2, p3, p4, p5, nrow = 2)


#############################################################################################################

# Small and medium corporations:
#df4 - NetSuite 
#df5 - Sage Intacct
#df9 - Odoo

#df4 - NetSuite 
bing_counts6 <- my_df4_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

new6 <- bing_counts6 %>%
  group_by(sentiment) %>%
  count(sentiment, sort=TRUE)

new6 <- as.data.frame(new6)
new6$pc <- percent(new6$n/sum(new6$n), 1)

fill <- c("red3", "forestgreen")
p6 <- ggplot() + geom_bar(aes(y = sentiment, x = pc, fill = sentiment), data = new6,
                          stat="identity") + ggtitle("SENTIMENTS NetSuite") + 
                          scale_fill_manual(values=fill) + 
                          xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p6


#df5 - Sage Intacct
bing_counts7 <- my_df5_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

new7 <- bing_counts7 %>%
  group_by(sentiment) %>%
  count(sentiment, sort=TRUE)

new7 <- as.data.frame(new7)
new7$pc <- percent(new7$n/sum(new7$n), 1)

fill <- c("red3", "forestgreen")
p7 <- ggplot() + geom_bar(aes(y = sentiment, x = pc, fill = sentiment), data = new7,
                          stat="identity") + ggtitle("SENTIMENTS Sage Intacct") + 
                          scale_fill_manual(values=fill) + 
                          xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p7


#df9 - Odoo
bing_counts8 <- my_df9_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

new8 <- bing_counts8 %>%
  group_by(sentiment) %>%
  count(sentiment, sort=TRUE)

new8 <- as.data.frame(new8)
new8$pc <- percent(new8$n/sum(new8$n), 1)

fill <- c("red3", "forestgreen")
p8 <- ggplot() + geom_bar(aes(y = sentiment, x = pc, fill = sentiment), data = new8,
                          stat="identity") + ggtitle("SENTIMENTS Odoo") + 
                          scale_fill_manual(values=fill) + 
                          xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p8


# Arranging all graphs on one graph - ERP for small and medium companies
grid.arrange(p6, p7, p8, nrow = 1)


#############################################################################################################

# PART 4: SENTIMENT ANALYSIS WORD CLOUDS, 'BING' POSITIVE / NEGATIVE SENTIMENTS AND TF_IDF

# merging all the softwares in one data frame
# Big corporations:
#df1 - ORACLE ERP CLOUD 
#df3 - Oracle eBusiness Suite
#df6 - SAP S/4HANA
#df7 - SAP ERP
# Small and medium corporations:
#df4 - NetSuite 
#df5 - Sage Intacct
#df9 - Odoo

# Creating data frame with all merged reviews 
all_merged <- bind_rows(
  mutate(my_df1_token, ERP = 'ORACLE_CLOUD'),
  mutate(my_df3_token, ERP = 'ORACLE_EBS'),
  mutate(my_df6_token, ERP = 'SAP_S4HANA'),
  mutate(my_df7_token, ERP = 'SAP_ERP'),
  mutate(my_df4_token, ERP = 'NetSuite'),
  mutate(my_df5_token, ERP = 'Sage_Intacct'),
  mutate(my_df9_token, ERP = 'Odoo'),
)
View(all_merged)

# Cloud of most negative and most positive words in all ERP reviews
all_merged %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words=100,
                   scale = c(1.0,1.0),
                   fixed.asp=TRUE,
                   title.size=3
                   
  )


# Getting sentiments for merged data frame
all_merged_sent <- all_merged%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Creating the graph with most used negative and positive sentiments
all_merged_sent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment all softwares", x=NULL)+
  coord_flip()

# Creating the list support for the most positive words - based on the above 
support <- all_merged %>%
              filter(str_detect(word, c("support", "robust", "friendly", "fast", "easy", "easier", 
                                        "powerful", "intuitive", "improvement", "efficient" ))) %>%
              group_by(ERP)
           
View(support)

# Grouping by ERP
grouped_support <- support %>%
  group_by(ERP)  %>%
  summarise(Total_n = sum(n)) %>%
  arrange(desc(Total_n))

View(grouped_support)

# Create graph showing which ERP has the most of positive words in reviews
fill3 <- c("forestgreen","grey","grey","grey","grey","grey")
supportp <- ggplot() + geom_bar(aes(y = Total_n, x = reorder(ERP, -Total_n), fill=reorder(ERP, -Total_n)), data = grouped_support,
                                stat="identity") + scale_fill_manual(values=fill3) + ggtitle("Frequency of Positive Sentiment by ERP") + 
                                xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

supportp 

# Creating the list support for the most negative words - based on the above 
bad <- all_merged %>%
  filter(str_detect(word, c("issues", "complex", "expensive", "difficult", "lack", "issue", "hard", "complicated", "limited", "cumbersome" ))) %>%
  group_by(ERP)

# Grouping by ERP
grouped_bad <- bad %>%
  group_by(ERP)  %>%
  summarise(Total_n = sum(n)) %>%
  arrange(desc(Total_n))

# Create graph showing which ERP has the most of negative words in reviews
fill2 <- c("red3", "grey","grey","grey","grey","grey","grey")
badp <- ggplot() + geom_bar(aes(y = Total_n, x = reorder(ERP, -Total_n), fill=reorder(ERP, -Total_n)), data = grouped_bad,
                            stat="identity") + scale_fill_manual(values=fill2) + ggtitle("Frequency of Negative Sentiment by ERP") + 
                            xlab(NULL) + ylab(NULL) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


badp

# Arranging good and positive in one graph
grid.arrange(supportp, badp, nrow = 1)

#############################################################################################################

# TF_IDF per ERP

# tf_idf
all_merged <- all_merged %>%
  bind_tf_idf(word, ERP, n) # we need to add location information

all_merged # we get all the zeors because we are looking at stop words ... too common

all_merged %>%
  arrange(desc(tf_idf))


#############
# graphical apprach: the most unique words per ERP
all_merged %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(ERP) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=ERP))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~ERP, ncol=2, scales="free")+
  coord_flip()

#############################################################################################################


