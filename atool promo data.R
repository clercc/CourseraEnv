library(RODBC)
library(plyr)
library(dplyr)
library(ggplot2)
library(car)


dbhandle <- odbcDriverConnect('driver={SQL Server};server=OSPREYDBS;database=Osprey;trusted_connection=true')
atool <- sqlQuery(dbhandle, 
                
"
SELECT a.fdate
, SUM(a.MIRExpense) AS Promo
, SUM(a.qty) AS Volume

FROM atool AS a 

WHERE a.channel = 'Costco' AND (a.handsetcarrier = 'Verizon' OR (a.handsetcarrier IS NULL AND a.carrier = 'Verizon')) AND a.Fdate BETWEEN '20170101' AND '20170228'

GROUP BY a.fdate
")

atool <- atool[,c(2,3,4)]

data <- atool
data$PromoAmount <- rep(NA, 58)
data$Month <- rep(NA, 58)
data$time <- rep(NA, 58)

for(i in 1:length(data$Promo)) {
                if(data$Promo[i] >= -35000) {
                        data$PromoAmount[i] = "Low"
                } else {data$PromoAmount[i] = "High"}
}

for(i in 1:length(data$Promo)) {
        if(data$fdate[i] < "2017-02-01") {
                data$Month[i] = "Jan"
        } else {data$Month[i] = "Feb"}
}

for( i in 1:length(data$Promo)) {
        if(data$Month[i] == "Jan" & data$PromoAmount[i] == "Low") {
                data$time[i] = 1
        } else if(data$Month[i] == "Jan" & data$PromoAmount[i] == "High") {
                data$time[i] = 2
        } else if(data$Month[i] == "Feb" & data$PromoAmount[i] == "High") {
                data$time[i] = 3
        } else {data$time[i] = 4}
}

data$PromoAmount <- as.factor(data$PromoAmount)
data$Month <- factor(data$Month, levels = c("Jan", "Feb"))
data$time <- factor(data$time, levels = c("1","2","3","4"), labels = c("Jan Low", "Jan High", "Feb High", "Feb Low"))

lm_eqn = function(data) {
        m = lm(Promo ~ Volume, data);
        eq <- substitute(italic(r)^2~"="~r2,list(r2 = format(summary(m)$r.squared, digits = 3)))
        as.character(as.expression(eq));
}

eq <- ddply(data,.(PromoAmount, Month), lm_eqn)

        #An initial look at the data matching Promotional Dollars to Volume by day shows a clear segemenation of money spent to drive activations.
        #On the Low promotion type, January and February glend together. Yet, the high promotion type although close together illustrates no overlap
        #between the months. At first glance, it appears three data points in the low set could be potential outliers. Due to the segemented nature,
        #further breakdown of the data must happen before looking at the goodness of fit of the data.


ggplot(data, aes(Volume, Promo)) + 
        geom_point(aes(color = PromoAmount, shape = Month)) + 
        geom_tile(aes(fill = PromoAmount)) +
        theme (
                legend.title = element_text(face = "bold"),
                legend.background = element_rect(),
                legend.key = element_blank(),
                panel.background = element_rect(fill = NA),
                panel.border = element_rect(color = "black", fill = NA),
                plot.title = element_text(face = "bold", hjust = 0.5),
                axis.title = element_text(face = "bold")
        ) +
        scale_shape_discrete(guide = guide_legend(reverse = FALSE)) +
        scale_color_discrete(name = "Promo Type") +
        scale_fill_discrete(name = "Promo Type") +
        labs(y = "Promo $", title = "Verizon Costco WHSE\nDaily Promotion & Volume Scatterplot\n1/1/2017 - 2/28/17") 


        #After having split up the data, clear trends emerge. The high promotion group depicts a strong relationship between promotional dollars spent
        #and the volume generated. Similarly, it's expected that low promotional spending is weakly correclated with generated volume. Colors and shapes
        #mirror the previous plot. As volume shifts depending on the month and promo type, the three potential outliers previously discussed are matched
        #to February data with a stronger positive shift in volume than January data. 

ggplot(data, aes(Volume, Promo)) + 
        geom_point(aes(color = PromoAmount, shape = Month)) + 
        geom_smooth(method = "lm", se = FALSE, color = "black", size = .75) +
        facet_grid(PromoAmount ~ Month) +
        geom_text(data = eq, aes(x = 1750, y = -125000, label = V1), parse = TRUE, inherit.aes = FALSE) + facet_grid(PromoAmount ~ Month) +
        theme(
                legend.position = "none",
                panel.background = element_rect(fill = NA),
                panel.border = element_rect(color = "black", fill = NA),
                plot.title = element_text(face = "bold", hjust = 0.5),
                strip.background = element_rect(color = "black", fill = "steelblue2"),
                strip.text = element_text(face = "bold"),
                axis.title = element_text(face = "bold")
        ) +
        scale_color_discrete(name = "Promo Type") +
        labs(y = "Promo $", title = "Segmented Verizon Promotions")
        
atool_low <- atool %>% filter(Promo >= -35000)
atool_high <- atool %>% filter(Promo <= -35000)

        #I explored diagnostic plots and compared the results of the residuals vs fitted to the studentized residuals vs fitted values. Since the
        #values are within respectible limits, I concluded that no data should be removed.

par(mfrow = c(2,2))

lm.low <- lm(Volume ~ Promo, atool_low)
plot(lm.low) 
mtext("Diagnostics Plot For Low Promos", side = 3, line = -2, outer = TRUE, font = 2)

lm.high <- lm(Volume ~ Promo, atool_high)
plot(lm.high)
mtext("Diagnostics Plot For High Promos", side = 3, line = -2, outer = TRUE, font = 2)

plot(lm.low$fitted.values, rstudent(lm.low), ylab = "Studentized Residuals", xlab = "Fitted-Values", main = "Studentized Residual vs Fitted Values\nfor Low Promos")
plot(lm.high$fitted.values, rstudent(lm.high), ylab = "Studentized Residuals", xlab = "Fitted-Values", main = "Studentized Residual vs Fitted Values\nfor High Promos")

#The plot shows the residual on the vertical axis, leverage on the horizontal axis, and the 
#point size is the square root of Cook's D statistic, a measure of the influence of the point.
#You can identify outliers as those cases with a large residual.
#Leverage is the potential for a case to have an influence on the model. You can identify points with 
#high leverage as those furthest to the right. 
influencePlot(lm.low, id.method = "identify", main = "Influence Plot for Low Promos", sub = "Circle size is proportional to Cook's Distance")
influencePlot(lm.high, id.method = "identify", main = "Influence Plot for High Promos", sub = "Circle size is proportional to Cook's Distance")

        #To better understand the segmented data, I plotted the segments as a time series. The data appear to be separated within defined
        #date ranges. This is further evidence into reasoning behind promotions driving volume. Although the goal of this analysis is not
        #to determine the trends of the data but to determine the efficiency of the planned decisions to drive volume via promotional activity.
        #The low promo type time series plot displays two distinct date ranges: the first half of January and the later half of February. This
        #leaves everything in between to the high promo type category.

x <- as.matrix(rep(1,18),18,1)
y <- as.matrix(rep(2,16),15,1)
z <- rbind(x,y)
atool_low <- cbind(atool_low,z)
colnames(atool_low)[4] <- "Month"

ggplot(atool, aes(fdate, Volume)) +
        geom_line() +
        theme(
                panel.background = element_rect(fill = NA),
                panel.border =  element_rect(color = "black", fill = NA),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold"),
                plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        labs(title = "Verizon Volume as Time Series", x = "Date")

ggplot(atool_low, aes(fdate, Volume)) +
        geom_line(aes(group = Month)) +
        theme(
                panel.background = element_rect(fill = NA),
                panel.border =  element_rect(color = "black", fill = NA),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold"),
                plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        labs(title = "Verizon Volume as Time Series\nPromo Type = Low", x = "Date")

ggplot(atool_high, aes(fdate, Volume)) +
        geom_line() +
        theme(
                panel.background = element_rect(fill = NA),
                panel.border =  element_rect(color = "black", fill = NA),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold"),
                plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        labs(title = "Verizon Volume as Time Series\nPromo Type = High", x = "Date")



ggplot(data,aes(Month, Volume, fill = Month)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4) +
        theme(legend.position = "none",
              panel.background = element_rect(fill = NA),
              panel.border = element_rect(color = "black", fill = NA),
              axis.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        scale_fill_manual(values = c("steelblue2", "pink")) +
        labs(title = "Verizon Costco WHSE\nDaily volume BoxPlot by Month")

        #By creating a boxplot of each segment along the time series, it becomes evident of the impact the promotions drove volume,
        #and the level of efficiency. 

ggplot(data,aes(time, Volume, fill = time)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4) +
        theme(legend.position = "none",
              panel.background = element_rect(fill = NA),
              panel.border = element_rect(color = "black", fill = NA),
              axis.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        labs(title = "Verizon Costco WHSE\nDaily volume BoxPlot by Promo Type", x = "Time")
