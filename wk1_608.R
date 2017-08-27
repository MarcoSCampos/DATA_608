
# DATA 608
# Assignment wk 1
#
# Marco Siqueira Campos

# Load packaging
library(RCurl)
library(ggplot2)

# Data loading
urlfile="https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture1/Data/inc5000_data.csv"
urlData=getURL(urlfile)
fast=read.csv(text=urlData, header = TRUE, stringsAsFactors = TRUE ,sep = ",")

# Check Data
head(fast)
tail(fast)

# Graphic Question 1
ggplot(fast, aes(State)) +
        coord_flip() +
        geom_bar(fill = "light blue") +
        ggtitle("Companies distribuition by state") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "grey15")) +
        theme(axis.title.x = element_text(colour = "grey15"),
              axis.title.y = element_text(colour = "grey15")) +
        theme(axis.text.x = element_text(colour="darkgrey"), axis.text.y = element_text(colour="darkgrey")) +
        theme(axis.text.y = element_text(size=6)) +
        scale_x_discrete(limits = rev(levels(fast$State)))

# Save the chart
ggsave("wk1_Figure1.png")

# Graphic Question 2 

# Remove incomplete cases
fast1<-fast[complete.cases(fast),]

# Select NY
fast1<-fast1[which(fast1$State=="NY"),]

# Remove outliers
fast1<-fast1[!fast1$Employees %in% boxplot.stats(fast1$Employees)$out,]

# Graphic

fast1$min<-min(fast1$Employees)
fast1$max<-max(fast1$Employees)
fast1$mean<-mean(fast1$Employees)

ggplot(fast1, aes(x= State, y= Employees)) +
        geom_bar(stat="summary",fun.y="mean",
                 fill = "lightblue",  width = 0.2, size=0.7) +
        geom_linerange(data=fast1, mapping=aes(x=State, ymin=min, ymax=max), size=1, color="blue") +
        geom_point(data=fast1, mapping=aes(x=State, y=mean), size=4, shape=21, fill="white") +
        ggtitle("Average and range of employees at NY") +
        theme_minimal() +
        theme(axis.title.x = element_text(colour = "grey25"),
                axis.title.y = element_text(colour = "grey25")) +
        theme(axis.text.x = element_text(colour="darkgrey"), axis.text.y = element_text(colour="darkgrey")) +
        theme(plot.title = element_text(hjust = 0.5, color = "grey15")) 
   

# Save the chart
ggsave("wk1_Figure2.png")

# Graphic Question 3

# Remove incomplete cases
fast2<-fast[complete.cases(fast),]

# Calculate Revenue per employees
fast2$rev_empl<-fast2$Revenue/fast2$Employees/1000000


# Order Data Frame
fast2<-fast2[order(-fast2$rev_empl),]

# top 30
fast2<-fast2[1:30,]

fast2$Name2<-factor(fast2$Name, levels = fast2$Name[order(fast2$rev_empl)])

ggplot(fast2, aes(x=Name2, y = rev_empl)) +
        coord_flip() +
        theme_minimal() +
        geom_bar(stat="identity",fill = "lightblue") +
        ggtitle("Revenue per employee top 30") +
        theme(plot.title = element_text(hjust = 0.5, color = "grey15")) +
        ylab("Revenue per employee per million") +
        xlab("Company")

ggsave("wk1_Figure3.png")


# References: http://sape.inf.usi.ch/quick-reference/ggplot2/geom_linerange
# https://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html


