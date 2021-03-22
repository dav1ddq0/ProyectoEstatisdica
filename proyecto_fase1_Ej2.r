options(digits = 5)

stats <- function(x){
    round_x = as.numeric(as.character(formatC(x, digits = 4, format = "f")))
    x_mean = as.numeric(as.character(formatC(mean(x), digits = 4, format = "f")))
    ux <- unique(round_x)
    tab <- tabulate(match(round_x, ux))
    x_mode <- ux[tab == max(tab)]
    x_median <- as.numeric(as.character(formatC(median(x), digits = 4, format = "f")))
    x_sd <- as.numeric(as.character(formatC(sd(x), digits = 4, format = "f")))
    x_varc <- as.numeric(as.character(formatC(var(x), digits = 4, format = "f")))
    x_var_coeff <- as.numeric(as.character(formatC(x_sd/x_mean, digits = 4, format = "f")))
    quantiles <- as.numeric(as.character(formatC(quantile(x), digits = 4, format = "f")))
    answer <- list("mean" = x_mean, "mode" = x_mode, "median" = x_median, "sd" = x_sd, "varc" = x_varc, "var_coeff" = x_var_coeff, "quantiles" = quantiles)
}

export_data <- function(statistics, file_name){
    fd <- file(file_name)
    open(fd, "w+b")
    write(paste("\n\tmean ------> ", statistics$mean), file = file_name, append = TRUE)
    write(paste("\n\tmode ------> ", statistics$mode), file = file_name, append = TRUE)
    write(paste("\n\tmedian ------> ", statistics$median), file = file_name, append = TRUE)
    write(paste("\n\tsd ------> ", statistics$sd), file = file_name, append = TRUE)
    write(paste("\n\tvarc ------> ", statistics$varc), file = file_name, append = TRUE)
    write(paste("\n\tvar_coeff ------> ", statistics$var_coeff), file = file_name, append = TRUE)
    write(paste("\n\t1st quantile ------> ", statistics$quantiles[2]), file = file_name, append = TRUE)
    write(paste("\n\t2rd quantile ------> ", statistics$quantiles[3]), file = file_name, append = TRUE)
    write(paste("\n\t3rd quantile ------> ", statistics$quantiles[4]), file = file_name, append = TRUE)
    close(fd)
}

raw_csv <- read.csv("listings-antwerp.csv")
data <- raw_csv[c("accommodates", "bedrooms", "price", "reviews_per_month")]

reviews_per_month_avg_notNAs <- mean(data[["reviews_per_month"]], na.rm = TRUE)
data[is.na(data[["reviews_per_month"]]), "reviews_per_month"] <- reviews_per_month_avg_notNAs

bedrooms_avg_notNAs <- mean(data[["bedrooms"]], na.rm = TRUE)
data[is.na(data[["bedrooms"]]), "bedrooms"] <- floor(bedrooms_avg_notNAs) 

accommodates <- data[,"accommodates"]
string_raw_prices <- data [,"price"]
reviews_per_month <- data[,"reviews_per_month"]
amount_of_bedrooms <- data[,"bedrooms"]

print(amount_of_bedrooms)

string_prices <- gsub(pattern =  "\\$", replacement =  "", string_raw_prices)
string_prices2 <- gsub(pattern =  ",", replacement =  "", string_prices)
prices <- as.numeric(as.character(string_prices2))

#histograms
png("./datos generados (Ejercicio 2)/accomodates_hist.png")
hist(accommodates)
dev.off()
png("./datos generados (Ejercicio 2)/prices_hist.png")
dev.new(hist(prices))
dev.off()
png("./datos generados (Ejercicio 2)/reviews_per_month_hist.png")
dev.new(hist(reviews_per_month))
dev.off()

#boxplots
png("./datos generados (Ejercicio 2)/accommodates.png")
dev.new(boxplot(accommodates, main = "Boxplot of accomodates"))
dev.off()
png("./datos generados (Ejercicio 2)/prices_boxplot.png")
dev.new(boxplot(prices, main = "Boxplot of prices"))
dev.off()
png("./datos generados (Ejercicio 2)/reviews_per_month_boxplot.png")
boxplot(reviews_per_month, main = "Boxplot of reviews per month")
dev.off()

export_data(stats(accommodates), "./datos generados (Ejercicio 2)/accomodates.txt")
export_data(stats(prices), "./datos generados (Ejercicio 2)/prices.txt")
export_data(stats(reviews_per_month), "./datos generados (Ejercicio 2)/reviews_per_month.txt")

one_bedroom_houses <- c()
i1 <- 1
several_bedroom_houses <- c()
i2 <- 1

for(i in 1:length(amount_of_bedrooms)){
    if (amount_of_bedrooms[i] == 1) {
       one_bedroom_houses[i1] <- prices[i]
       i1 <- i1 + 1
    }
    else {
        several_bedroom_houses[i2] <- prices[i]
        i2 <- i2 + 1
    }
}

print(one_bedroom_houses)
print(several_bedroom_houses)
print(t.test(one_bedroom_houses, several_bedroom_houses, alternative = "greater"))

#file_name = "./datos generados (Ejercicio 3)/answer.txt"
#fd <- file(file_name)
#open(fd, "w+b")
#write(paste("\n\tone bedroom average price ------> ", as.numeric(as.character(formatC(one_bedroom_price_sum/one_bedroom_amount, digits = 3, format = "f")))), file = file_name, append = TRUE)
#write(paste("\n\tseveral bedroom average price ------> ", as.numeric(as.character(formatC(several_bedroom_price_sum/several_bedroom_amount, digits = 3, format = "f")))), file = file_name, append = TRUE)
#close(fd)
