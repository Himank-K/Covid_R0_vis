library(readxl)
library(R0)
library(ggplot2)
library(grid)
library(dplyr)

District_ro_calculation <- read_excel("District_ro_calculation.xlsx")

gt_covid <- generation.time("gamma",c(5.2,2.8))

## Function to plot r0 and cases

plot.R0 <- function(epid_df, epid_r0, distr){
  
  r0_df <- data.frame(date = head(epid_df$date, -1), r0 = head(epid_r0$R, -1),
                      ci_lb = head(epid_r0$conf.int$lower, -1),
                      ci_ub = head(epid_r0$conf.int$upper, -1))
  
  ma_df <- data.frame(date = head(epid_df$date, -1), cases = head(epid_df$cases, -1))
  
  p1 <- ggplot(ma_df, aes(as.Date(date), cases))+geom_line()+theme_minimal()+
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), panel.grid.minor.x = element_blank())+
    scale_x_date(date_breaks = "1 month")+
    labs(y = "Daily cases (moving average of 7 days)", title =paste("Daily cases and R(t) for", distr, sep = " "))
  p2 <- ggplot(r0_df, aes(as.Date(date), r0))+ylim(0,NA)+geom_line()+theme_minimal()+
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub), alpha = 0.25)+
    labs(x = "Date", y = "R(t)")+
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
    theme(axis.text.x = element_text(angle = 90), panel.grid.minor.x = element_blank())
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  
}


# dataframe size
days = nrow(District_ro_calculation)


### District-wise plot :
# Mumbai 7 day moving average 

mumbai_ma <- data.frame(date = District_ro_calculation$Date[5:(length(District_ro_calculation$Mumbai_daily)-4)],
                        cases = District_ro_calculation$mumbai_daily_7ma[5:(length(District_ro_calculation$Mumbai_daily)-4)])
mumbai_r0 <- est.R0.TD(epid = District_ro_calculation$mumbai_daily_7ma[5:(length(District_ro_calculation$Mumbai_daily)-4)],
                       GT = gt_covid,
                       nsim = 1000,
                       begin = 1,
                       end = (nrow(District_ro_calculation)-8))

plot.R0(mumbai_ma, mumbai_r0, "Mumbai")


# Thane 7 days moving average

thane_ma <- data.frame(date = District_ro_calculation$Date[5:(length(District_ro_calculation$Thane_daily)-4)],
                       cases = District_ro_calculation$thane_daily_7ma[5:(length(District_ro_calculation$Thane_daily)-4)])


thane_r0 <- est.R0.TD(epid = District_ro_calculation$thane_daily_7ma[5:(length(District_ro_calculation$Thane_daily)-4)],
                       GT = gt_covid,
                       nsim = 1000,
                       begin = 1,
                       end = (nrow(District_ro_calculation)-8))

plot.R0(thane_ma, thane_r0, "Thane")


# Pune 7 days moving average

pune_ma <- data.frame(date = District_ro_calculation$Date[5:(length(District_ro_calculation$Pune_daily)-4)],
                      cases = District_ro_calculation$pune_daily_7ma[5:(length(District_ro_calculation$Pune_daily)-4)])


pune_r0 <- est.R0.TD(epid = District_ro_calculation$pune_daily_7ma[5:(length(District_ro_calculation$Pune_daily)-4)],
                      GT = gt_covid,
                      nsim = 1000,
                      begin = 1,
                      end = (nrow(District_ro_calculation)-8))

plot.R0(pune_ma, pune_r0, "Pune")

# Maharashtra 7 days moving average

maha_ma <- data.frame(date = District_ro_calculation$Date[5:(length(District_ro_calculation$Maharashtra_daily)-3)],
                       cases = District_ro_calculation$Maharashtra_daily_7ma[5:(length(District_ro_calculation$Maharashtra_daily)-3)])


maha_r0 <- est.R0.TD(epid = District_ro_calculation$Maharashtra_daily_7ma[5:(length(District_ro_calculation$Maharashtra_daily)-3)],
                      GT = gt_covid,
                      nsim = 1000,
                      begin = 1,
                      end = (nrow(District_ro_calculation)-7))

plot.R0(maha_ma, maha_r0, "Maharashtra")


# India 7 days moving average

india_ma <- data.frame(date = District_ro_calculation$Date[5:(days-4)],
                       cases = District_ro_calculation$India_daily_7ma[5:(days-4)])

india_r0 <- est.R0.TD(epid = District_ro_calculation$India_daily_7ma[5:(days-4)],
                      GT = gt_covid,
                      nsim = 1000,
                      begin = 1,
                      end = (days-8))

plot.R0(india_ma, india_r0, "India")

# India (nonMH) 7 days moving average

india_non_mh_ma <- data.frame(date = District_ro_calculation$Date[5:(days-4)],
                       cases = District_ro_calculation$India_nonMH_daily_7ma[5:(days-4)])

india_non_mh_r0 <- est.R0.TD(epid = District_ro_calculation$India_nonMH_daily_7ma[5:(days-4)],
                      GT = gt_covid,
                      nsim = 1000,
                      begin = 1,
                      end = (days-8))

plot.R0(india_non_mh_ma, india_non_mh_r0, "India (non-Maharashtra)")


# KDMC 7 days moving average

kdmc_ma <- data.frame(date = District_ro_calculation$Date[5:(days-3)],
                      cases = District_ro_calculation$KDMC_daily_7ma[5:(days-3)])

kdmc_r0 <- est.R0.TD(epid = District_ro_calculation$KDMC_daily_7ma[5:(days-3)],
                     GT = gt_covid,
                     nsim = 1000,
                     begin = 1,
                     end = (days-7))

plot.R0(kdmc_ma, kdmc_r0, "KDMC")

# Kerala 7 days moving average

kerala_ma <- data.frame(date = District_ro_calculation$Date[5:(days-4)],
                       cases = District_ro_calculation$Kerala_daily_7ma[5:(days-4)])

kerala_r0 <- est.R0.TD(epid = District_ro_calculation$Kerala_daily_7ma[5:(days-4)],
                       GT = gt_covid,
                       nsim = 1000,
                       begin = 1,
                       end = (days-8))

plot.R0(kerala_ma, kerala_r0, "Kerala")


# write data file for python plotting

R0data_frame <- data.frame(date = District_ro_calculation$Date[5:(days-4)],
                           mumbai_ma7 = District_ro_calculation$mumbai_daily_7ma[5:(days-4)],
                           R0 = mumbai_r0$R)

write.csv(R0data_frame, file = "R0dataframe.csv", row.names = FALSE)
