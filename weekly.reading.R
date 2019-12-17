## Create weekly sub-meter readings

weekly.grouping <- function(df) {
  #sub1
  energy.merged.weekly.sub1 <- energy.merged %>%
    group_by(Week, Month, Year) %>%
    summarise(avg.sub1 = mean(Sub_metering_1), avg.global = mean(Global_active_power))
  #sub2
  energy.merged.weekly.sub2 <- energy.merged %>%
    group_by(Week, Month, Year) %>%
    summarise(avg.sub2 = mean(Sub_metering_2), avg.global = mean(Global_active_power))
  #sub3
  energy.merged.weekly.sub3 <- energy.merged %>%
    group_by(Week, Month, Year) %>%
    summarise(avg.sub3 = mean(Sub_metering_3), avg.global = mean(Global_active_power))
  
  return(list(sub1 = energy.merged.weekly.sub1, sub2 = energy.merged.weekly.sub2, 
              sub3 = energy.merged.weekly.sub3))
}

