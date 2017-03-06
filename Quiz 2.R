myapp <- oauth_app("R Quiz", "bcbd7703cd342a40d80a", secret = "039efcbcf65c0642a65c4c2e246c98223873e006")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))

acs <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "C:/Users/Christian/Github/datasciencecoursera/acs.csv", mode = "wb")

con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode <- readLines(con)
close(con)
htmlCode

nchar(htmlCode[c(10,20,30,100)])


x <- read.fwf(file = url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"), skip = 4, 
              widths = c(12,7,4,9,4,9,4,9,4))
head(x)
sum(x[,4])


