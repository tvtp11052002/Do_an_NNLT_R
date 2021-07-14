#----------------------------------------------------------------
# Họ và tên: Trần Văn Tuấn Phong
# Mã SV: 20E1020020
# Phân tích dữ liệu tại bang Ohio và Texas
#----------------------------------------------------------------
# Cài đặt và cập nhật thư viện
update.packages("tools")
install.packages("ggplot2", lib="C:\\Users\\Admin\\Documents\\R\\win-library\\3.3")
update.packages("ggplot2")
update.packages("data.table")
library(data.table)
#----------------------------------------------------------------
# Di chuyển đến thư mục
setwd("C:User/tvtp1/Project_HUET_PhanTichDuLieuCovid/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports_us")
#----------------------------------------------------------------
# Đọc dữ liệu
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill=TRUE)
names(data)
View(data)
#----------------------------------------------------------------
# Tạo các list và dataframe về
# Số lượng người nhiễm và người chết do covid
# Số người hồi phục
# Tỉ lệ tử vong
# Tại bang Ohio, Texas ở Mỹ
Ohio <- data[Province_State=="Ohio"]
Texas <- data[Province_State=="Texas"]
Texas$Case_Fatality_Ratio <- format(round(Texas$Case_Fatality_Ratio, 2), nsmall = 2)
Ohio$Case_Fatality_Ratio <- format(round(Ohio$Case_Fatality_Ratio, 2), nsmall = 2)
library("ggplot2", lib.loc="~/R/win-library/4.0")
#----------------------------------------------------------------
# Đọc dữ liệu về covid của các bang tại Mỹ vào ngày 01/01/2021
df1 <- read.table("01-01-2021.csv", header = TRUE, sep = ",")
names(df1)
# Chuyển số thập phân có 2 chữ số sau dấu phẩy
df1$Case_Fatality_Ratio <- format(round(df1$Case_Fatality_Ratio, 2), nsmall = 2)
#----------------------------------------------------------------
# Đọc dữ liệu về covid của các bang tại Mỹ vào ngày 05/12/2020
df <- read.table("05-12-2020.csv", header = TRUE, sep = ",")
names(df)
# Chuyển số thập phân có 2 chữ số sau dấu phẩy
df$Mortality_Rate <- format(round(df$Mortality_Rate, 2), nsmall = 2)
#----------------------------------------------------------------
# Đồ thị 1
ggplot(Ohio) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong gây ra bởi covid được xác nhận tại Ohio - Mỹ
                                  từ 04/2020 đến 07/2021", x="Last Update", y="Case Fatality Ratio")
#----------------------------------------------------------------
# Đồ thị 2
ggplot(Texas, aes(y=Case_Fatality_Ratio, x=Last_Update, fill= Case_Fatality_Ratio)) + 
  geom_point(aes(color=Case_Fatality_Ratio)) + 
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong gây ra bởi covid được xác nhận tại Texas - Mỹ
                                    từ 04/2020 đến 07/2021", x="Last Update", y="Case Fatality Ratio")
#----------------------------------------------------------------
# Đồ thị 3
ggplot(df1[5:15,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện số lượng ca hồi phục sau khi nhiễm covid 19 ở một số bang/thành phố tại Mỹ 
                                                        trong ngày 2021-01-01") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 4
ggplot(Texas[1:11,], aes(x='', y=Case_Fatality_Ratio, fill=Last_Update)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong bởi covid 19 ở Texas - Mỹ
                              từ 04/2020 đến 07/2021") +
  geom_text(aes(label = paste0(Case_Fatality_Ratio)), position = position_stack(vjust=0.5))
#----------------------------------------------------------------
# Đồ thị 5
ggplot(df1[15:30,], aes(x=Case_Fatality_Ratio, y=Province_State, color=Province_State)) + 
  geom_point() + labs(title="Biểu đồ thể hiện tỉ lệ tử vong bởi covid 19 ở một số bang/thành phố tại Mỹ 
                                                  ngày 01-01-2021", x="Case Fatality Ratio", y="Province")
#----------------------------------------------------------------
# Đồ thị 6
ggplot(df1[20: 26,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện số lượng ca nhiễm covid 19 được xác nhận ở một số bang/thành phố ở Mỹ
                                                         trong ngày 01-01-2021") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 7
ggplot(df1, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="Biểu đồ thể hiện số lượng ca hồi phục sau khi nhiễm covid 19 được xác nhận
                              ở các bang/thành phố trong ngày 01-01-2021",x = "Confirmed", y="Province")
#----------------------------------------------------------------
# Đồ thị 8
ggplot(Ohio, aes(x=Confirmed, y=Last_Update, fill = Confirmed)) + 
  geom_point(aes(colour = Confirmed)) +
  labs(title="Biểu đồ thể hiện số lượng ca nhiễm covid 19 được xác nhận ở Ohio - Mỹ 
                                    từ 04/2020 tới 07/2021", x = "Confirmed", y="Last Update")
#----------------------------------------------------------------
# Đồ thị 9
ggplot(Texas) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  theme_gray() +
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong gây ra bởi covid được xác nhận tại Texas - Mỹ
                                        từ 04/2020 đến 07/2021", x="Last Update", y="Case Fatality Ratio")
#----------------------------------------------------------------
# Đồ thị 10
ggplot(df1, aes(x=Confirmed, y=Province_State, fill= Confirmed)) + 
  geom_point(aes(color=Confirmed)) +
  labs(title="Biểu đồ thể hiện số lượng ca nhiễm covid 19 được xác nhận ở các bang/thành phố tại Mỹ
                                          trong ngày 01-01-2021",x = "Confirmed", y="Province")
#----------------------------------------------------------------
# Đồ thị 11
ggplot(df1[1:11,], aes(x=Deaths, y=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill = Province_State)) + 
  labs(title="Biểu đồ thể hiện số lượng ca tử vong do covid 19 được xác nhận ở một số bang/thành phố tại Mỹ
                                                  trong ngày 01-01-2021",x = "Deaths", y="Province")
#----------------------------------------------------------------
# Đồ thị 12
ggplot(df1[20:29,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="Biểu đồ thể hiện số lượng ca tử vong bởi covid 19 được xác nhận ở một số bang/thành phố tại Mỹ
                                                    trong ngày 01-01-2021")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 13
ggplot(df[1:10,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện số lượng ca hồi phục sau khi nhiễm covid 19 được xác nhận ở một số bang/thành phố 
                                                           tại Mỹ trong ngày 05-12-2020") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 14
ggplot(df[12:20,], aes(x='', y=Mortality_Rate, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong bởi covid 19 ở một số bang/thành phố
                               tại Mỹ trong ngày 05-12-2020")  +
  geom_text(aes(label = paste0(Mortality_Rate)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 15
ggplot(df[10:25,], aes(x=Mortality_Rate, y=Province_State, color=Province_State)) + 
  geom_point() + 
  labs(title="Đồ thị thể hiện tỉ lệ tử vong bởi covid 19 ở một số bang/thành phố tại Mỹ
                                    trong ngày 05-12-2020",x="Mortality Rate", y="Province")
#----------------------------------------------------------------
# Đồ thị 16
ggplot(df[20: 26,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Đồ thị thể hiện số lượng ca nhiễm covid 19 được xác nhận ở một số bang/thành phố tại Mỹ
                                                      trong ngày 05-12-2020") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 17
ggplot(df, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="Đồ thị thể hiện số lượng ca hồi phục sau khi nhiễm covid 19 được xác nhận ở các bang/thành phố tại Mỹ 
                                             trong ngày 05-12-2020",x = "Recovered", y="Province")
#----------------------------------------------------------------
# Đồ thị 18
ggplot(df, aes(x=Confirmed, y=Province_State, fill= Confirmed)) + 
  geom_point(aes(color=Confirmed)) +
  labs(title="Đồ thị thể hiện số lượng ca nhiễm covid 19 được xác nhận ở các bang/thành phố tại Mỹ 
                                           trong ngày 05-12-2020",x = "Confirmed", y="Province")
#----------------------------------------------------------------
# Đồ thị 19
ggplot(df, aes(x=Deaths, color=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill =Province_State)) + 
  theme_grey() +
  labs(title="Đồ thị thể hiện số lượng ca tử vong bởi covid 19 được xác nhận ở các bang/thành phố tại Mỹ
                                                    trong ngày 05-12-2020")
#----------------------------------------------------------------
# Đồ thị 20
ggplot(Texas, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Deaths), colour = "orange") +
  labs(title="Đồ thị thể hiện số lượng ca tử vong bởi covid 19 được xác nhận ở Texas - Mỹ
                                        từ 04/2020 đến 07/2021",x = "Deaths", y="Last Update")
#----------------------------------------------------------------
# Đồ thị 21
ggplot(df[5:11,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="Đồ thị thể hiện số lượng ca tử vong bởi covid 19 được xác nhận ở một số bang/thành phố tại Mỹ
                                                            trong ngày 05-12-2020")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 22
ggplot(Ohio, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Deaths), colour = "green") + 
  labs(title="Đồ thị thể hiện số lượng ca tử vong bởi covid 19 được xác nhận ở Ohio tại Mỹ
                                        từ 04/2020 đến 07/2021")