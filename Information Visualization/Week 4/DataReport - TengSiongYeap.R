#load the dataset
parking <- read.csv("parking-citations.csv", header = TRUE, stringsAsFactors = FALSE)

#drop the empty values
parking <- parking[complete.cases(parking),]

#Which Car Plate has the most parking citations, show top 10 (exclude CA)
plate = sort(table(parking$RP.State.Plate), decreasing = TRUE)
plate
barplot(plate[2:11],
        main = "The Top 10 Car Plate which has the Most Parking Citation", 
        xlab = "Car Make", ylab = "Count", col = "darkseagreen3",ylim = c(0,1800))

#Which Car Make has the most parking citations
make = sort(table(parking$Make), decreasing = TRUE)
make
barplot(make[1:10],
        main = "The Top 10 Car Make which has the Most Parking Citation", 
        xlab = "Car Make", ylab = "Count", col = "darkseagreen3",ylim = c(0,50000))

#In among CA plate, show the top 3 car make for each citation
cali <- parking[parking$RP.State.Plate=='CA',]
cali.table <- sort(table(cali$Make), decreasing = TRUE)
cali.df <- as.data.frame(cali.table)
cali.df

fill_col=c("#FF3030","#EEEE00","#63B8FF","#43CD80")
cali_count<-tapply(cali.df$Freq,list(parking$Violation.Description,parking$Make),sum)
cali_count


