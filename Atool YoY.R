atoolmar <- sqlQuery(dbhandle, 
                  
                  "
                  SELECT a.fdate
                  , SUM(a.MIRExpense) AS Promo
                  , SUM(a.qty) AS Volume
                  
                  FROM atool AS a 
                  
                  WHERE a.channel = 'Costco' AND (a.handsetcarrier = 'Verizon' OR (a.handsetcarrier IS NULL AND a.carrier = 'Verizon')) AND (a.Fdate >= '20170101' OR (a.Fdate BETWEEN '20160101' AND '20160323'))
                  
                  GROUP BY a.fdate
                  ")
atoolmar$Month <- rep("March", 23)
ggplot(atoolmar, aes(Month, Volume)) +
        geom_boxplot()
plot(Volume~fdate, atoolmar, type = "l")
atoolmar$Year <- rep(NA, 163)
atoolmar$Month <- rep(NA, 163)

for(i in 1:length(atoolmar$fdate)){
        if(atoolmar$fdate[i] <= '2016-12-31'){
                atoolmar$Year[i] = "2016"
        } else {atoolmar$Year[i] = "2017"}
}

for(i in 1:length(atoolmar$fdate)){
        if(atoolmar$fdate[i] %between% c('2016-01-01', '2016-01-31')) {
                atoolmar$Month[i] = "1"
        } else if (atoolmar$fdate[i] %between% c('2016-02-01', '2016-02-29')) {
                atoolmar$Month[i] = "2"
        } else if (atoolmar$fdate[i] %between% c('2016-03-01', '2016-03-31')) {
                atoolmar$Month[i] = "3"
        } else if (atoolmar$fdate[i] %between% c('2017-01-01', '2017-01-31')) {
                atoolmar$Month[i] = "1"
        } else if (atoolmar$fdate[i] %between% c('2017-02-01', '2017-02-28')) {
                atoolmar$Month[i] = "2"
        } else { atoolmar$Month[i] = "3" }
} 

atoolmar$Year <- as.factor(atoolmar$Year)
atoolmar$Month <- as.factor(atoolmar$Month)

ggplot(atoolmar, aes(Year, Volume)) +
        geom_boxplot(aes(fill = Month)) +
        facet_grid(.~Month)