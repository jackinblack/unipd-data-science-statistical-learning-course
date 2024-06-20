data.col.names = c("Date", "Rented.Bike.Count", "Hour", "Temperature"
	,"Humidity", "Wind.speed", "Visibility", "Dew.point.temperature"
	,"Solar.Radiation", "Rainfall", "Snowfall", "Seasons"
	,"Holiday", "Functioning.Day");
data <- read.csv(
	file = "data/SeoulBikeData.csv"
	,sep = ","
	,check.names = FALSE
	,col.names = data.col.names
);
nrow(data); ncol(data); summary(data);

timestamp <- strptime(
	paste( data$Date, data$Hour )
	,format="%d/%m/%Y %H"
);
timestamp <- as.Date( timestamp );

data$Date <- as.numeric(as.POSIXct(timestamp))

library(corrplot, warn.conflicts = FALSE); corrplot(
	cor(data[1 : 11])
	,method = "number"
	,diag = FALSE
	,tl.cex = 0.8
	,number.cex = 0.6
	,tl.col = "black"
	);


# in this test we uncover that there is a small but positive correlation between date and 

test <- c(1,2,3)

for (name in names(data + test)) {
  print(name)
}

first = -1; first.idx = -1;
for (i in 1:length(data$Humidity)){
	if (data$Humidity[i] <= 0 & first <= 0){ first = data$Humidity[i - 1]; first.idx = i - 1; }
	if (data$Humidity[i] > 0 & first > 0 ){
		data$Humidity[(first.idx + 1) : (i - 1)] = as.integer((first + data$Humidity[i]) / 2);
		first = -1; first.idx = -1;
	}
}
data$Humidity

find_differences <- function(list1, list2) {
  # Check if lists are of the same length
  if (length(list1) != length(list2)) {
    stop("Lists must be of the same length")
  }
  
  # Identify indices where elements differ
  diff_indices <- which(list1 != list2)
  
  # Extract differing values
  differences <- data.frame(
    Index = diff_indices,
    List1_Value = list1[diff_indices],
    List2_Value = list2[diff_indices]
  )
  
  return(differences)
}

# Call the function
differences <- find_differences(list1, list2)
print(differences)