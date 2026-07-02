library(tidyverse)
library(readxl)
library(googlesheets4)
library(broom)

gs4_auth()

#group_by, map,

#Assign Variables
calibrationLink = "https://docs.google.com/spreadsheets/d/1NYN9nsvnWhgacy0XAkqLbMublKPF0JVekISW6Z8raEs/edit?gid=0#gid=0"
dataLink = "https://docs.google.com/spreadsheets/d/1a9A-cXPrbydEx-SkLjtom8_Vz4tvXKqDvl7rAOupK4o/edit?gid=0#gid=0"
numDendros = 4
startRow = 1
voltCol = TRUE  #Do you have a voltage column? (as last column)


#Create standard column names for data
# dendroList = c()
# for (num in 1:numDendros){
#   name = paste0("sensor_", num)
#   dendroList = c(dendroList, name)
# }

# colList = c("datetime_dd_mm_yy", dendroList)
# if(voltCol) {
#   colList = c(colList, "Voltage")
# }

#read data
calibration <- read_sheet(ss = calibrationLink)
dendrometer_log <- read_sheet(col_names = colList, 
                              skip = startRow-1, ss = dataLink)

newCalibration <- calibration |> 
  group_by(sensor) |> 
  nest() |> 
  mutate(model = map(data, ~ summary(lm(output ~ width, data = .x))), 
         tidy_results = map(model, tidy)
         ) |>
  rowwise() |> 
  mutate( rsquared = model$r.squared) |> 
  unnest(tidy_results) |> 
  select(!std.error:p.value) |> 
  pivot_wider(names_from = term, values_from = estimate)


# model_rs# model_results <- mtcars %>% 
#   group_by(am) %>% 
#   nest() %>% 
#   mutate(model = map(data, ~ lm(mpg ~ hp, data = .x)), tidy_results = map(model, tidy) ) 
# final_output <- model_results %>% unnest(tidy_results) print(final_output)



#plot calibration curve (change sensor number for diff. sensor's lines)
# ggplot(calibration, mapping = aes(x = width, y = sensor_1)) +  
#   geom_point() + geom_smooth(formula = y ~ x, method = "lm")
# 
# #Get best fit line equations
# line_1 <- lm(sensor_1 ~ width, calibration)
# line_2 <- lm(sensor_2 ~ width, calibration)
# line_3 <- lm(sensor_3 ~ width, calibration)
# line_4 <- lm(sensor_4 ~ width, calibration)
# 
# glance(line_1)
# 
# slope_1 = coef(line_1)[2]
# inter_1 = coef(line_1)[1]
# slope_2 = coef(line_2)[2]
# inter_2 = coef(line_2)[1]
# slope_3 = coef(line_3)[2]
# inter_3 = coef(line_3)[1]
# slope_4 = coef(line_4)[2]
# inter_4 = coef(line_4)[1]


#put dates into date format
dendrometer_log <-  dendrometer_log |> 
  mutate(datetime_dd_mm_yy = dmy_hms(datetime_dd_mm_yy))

#Plot sensor reading vs time
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_1)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_2)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_3)) +  geom_point()
ggplot(dendrometer_log, mapping = aes(x = datetime_dd_mm_yy, y = sensor_4)) +  geom_point()


#Convert sensor readings to mm
dendrometer_mm <- dendrometer_log|> 
  mutate(sensor_1 = (sensor_1-inter_1)/slope_1,
         sensor_2 = (sensor_2-inter_2)/slope_2, 
         sensor_3 = (sensor_3-inter_3)/slope_3,
         sensor_4 = (sensor_4-inter_4)/slope_4)

#Plot plant diameter in mm (non-temperature corrected)
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_1)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_2)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_3)) +  geom_point()
ggplot(dendrometer_mm, mapping = aes(x = datetime_dd_mm_yy, y = sensor_4)) +  geom_point()

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