library(tidyverse)
library(readxl)
library(googlesheets4)
library(broom)

gs4_auth()

#as.POSIXct
#group_by, map,

#Assign Variables
calibrationLink = "https://docs.google.com/spreadsheets/d/1NYN9nsvnWhgacy0XAkqLbMublKPF0JVekISW6Z8raEs/edit?gid=0#gid=0"
dataLink = "https://docs.google.com/spreadsheets/d/1a9A-cXPrbydEx-SkLjtom8_Vz4tvXKqDvl7rAOupK4o/edit?gid=0#gid=0"
startRow = 581
numDendros = 8
voltCol = TRUE



#Create standard column names for data
dendroList = c()
for (num in 1:numDendros){
  name = paste0("sensor_", num)
  dendroList = c(dendroList, name)
}

colList = c("datetime_dd_mm_yy", dendroList)
if(voltCol) {
  colList = c(colList, "Voltage")
}

#read data
calibration <- read_sheet(ss = calibrationLink)
dendrometer_log <- read_sheet(skip = startRow-1, ss = dataLink,col_names = colList)

#filter data
calibration <- filter(calibration, width != 7)
dendrometer_log <- filter(dendrometer_log, sensor_8 >= 24500)

newCalibration <- calibration |> 
  group_by(sensor) |> 
  nest() |> 
  mutate(model = map(data, ~ summary(lm(no_resistor_output ~ width, data = .x))), 
         tidy_results = map(model, tidy)) |>
  rowwise() |> 
  mutate( rsquared = model$r.squared) |> 
  unnest(tidy_results) |> 
  select(!std.error:p.value) |> 
  pivot_wider(names_from = term, values_from = estimate)


#plot calibration curve (change sensor number for diff. sensor's lines)
# ggplot(calibration, mapping = aes(x = width, y = sensor_1)) +  
#   geom_point() + geom_smooth(formula = y ~ x, method = "lm")
# 
# #Get best fit line equations



#put dates into date format
dendrometer_log <-  dendrometer_log |> 
  mutate(datetime_dd_mm_yy = dmy_hms(datetime_dd_mm_yy))

#Plot sensor reading vs time
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_1)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_2)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_3)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_4)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_5)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_6)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_7)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_8)) +  geom_point()


#Convert sensor readings to mm
dendrometer_mm <- dendrometer_log|> 
  mutate(sensor_1 = (sensor_1-newCalibration$`(Intercept)`[1])/newCalibration$width[1],
         sensor_2 = (sensor_2-newCalibration$`(Intercept)`[2])/newCalibration$width[2], 
         sensor_3 = (sensor_3-newCalibration$`(Intercept)`[3])/newCalibration$width[3],
         sensor_4 = (sensor_4-newCalibration$`(Intercept)`[4])/newCalibration$width[4],
         sensor_5 = (sensor_5-newCalibration$`(Intercept)`[5])/newCalibration$width[5],
         sensor_6 = (sensor_6-newCalibration$`(Intercept)`[6])/newCalibration$width[6], 
         sensor_7 = (sensor_7-newCalibration$`(Intercept)`[7])/newCalibration$width[7],
         sensor_8 = (sensor_8-newCalibration$`(Intercept)`[8])/newCalibration$width[8])

#Plot plant diameter in mm (non-temperature corrected)
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_1)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_2)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_3)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_4)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_5)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_6)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_7)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_8)) +  geom_point()


#Temperature Correction 
dendrometer_mm_corr <- dendrometer_mm |> 
  mutate(sensor_1 = sensor_1-sensor_4, 
         sensor_2 = sensor_2-sensor_4, 
         sensor_3 = sensor_3-sensor_4)
         
#Plot corrected mm graphs (plus temperature) separately,
ggplot(dendrometer_mm_corr, mapping = aes(x = datetime_dd_mm_yy, y = sensor_1)) +  geom_point()
ggplot(dendrometer_mm_corr, mapping = aes(x = datetime_dd_mm_yy, y = sensor_2)) +  geom_point()
ggplot(dendrometer_mm_corr, mapping = aes(x = datetime_dd_mm_yy, y = sensor_3)) +  geom_point()
ggplot(dendrometer_mm_corr, mapping = aes(x = datetime_dd_mm_yy, y = sensor_4)) +  geom_point()


#all on one graph,
ggplot(dendrometer_mm_corr, 
       mapping = aes(x = datetime_dd_mm_yy)) +  
  geom_point(aes(y = sensor_1, color = "Sensor 1")) +
  geom_point(aes(y = sensor_2, color = "Sensor 2")) +
  geom_point(aes(y = sensor_3, color = "Sensor 3")) +
  geom_point(aes(y = sensor_4, color = "Temp"))

#and as four seperate graphs next to each other
wider <- dendrometer_mm_corr |> 
  # select(-sensor_4) |> 
  pivot_longer(cols = sensor_1:sensor_4, values_to = "value", names_to = "sensor")
  
ggplot(wider, aes(x = datetime_dd_mm_yy, y = value,color = sensor)) +
  geom_point() +
  facet_wrap(~ sensor, scales = "free_y",
             nrow = 4)



# ctrl shift c (comment or uncomment)
# ctrl i (align code)
# ctrl shift m (pipe operator)