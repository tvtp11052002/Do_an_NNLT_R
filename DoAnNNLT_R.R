setwd("C:\\Users\\tvtp1\\Downloads\\COVID-19-master\\COVID-19-master\\csse_covid_19_data\\csse_covid_19_daily_reports_us")

#install va update thu vien 
update.packages("tools")
install.packages("ggplot2", lib="C:/Users/DUC-PC/Documents/R/win-library/3.3")
update.packages("ggplot2")
update.packages("data.table")
library(data.table)  

#Doc tat ca cac file vao mot dataframe
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill=TRUE)
names(data)
View(data)

#Tao cac list va dataframe ve so luong nguoi nhiem covid, nguoi chet
#do covid, nguoi da hoi phuc, ti le tu vong
#ung voi tung ngay o bang Alabama, Alaska o US
#tao dataframe
Alk <- data[Province_State=="Alaska"]
Albm <- data[Province_State=="Alabama"]
Albm$Case_Fatality_Ratio <- format(round(Albm$Case_Fatality_Ratio, 2), nsmall = 2)
Alk$Case_Fatality_Ratio <- format(round(Alk$Case_Fatality_Ratio, 2), nsmall = 2)
#doc file du lieu ve covid cua cac bang/thanh pho
#tai My ngay 01/01/2021
df1 <- read.table("01-01-2021.csv", 
                  header = TRUE,
                  sep = ",")
names(df1)
#Chuyen sang so thap phan co 2 chu so sau dau phay
df1$Case_Fatality_Ratio <- format(round(df1$Case_Fatality_Ratio, 2), nsmall = 2)
df1$Case_Fatality_Ratio

#doc file du lieu ve covid cua cac bang/thanh pho
#tai My ngay 05/12/2020
df <- read.table("05-12-2020.csv", 
                 header = TRUE,
                 sep = ",")
names(df)
View(df)
df$Mortality_Rate <- format(round(df$Mortality_Rate, 2), nsmall = 2)
#doc file du lieu ve covid cua cac bang/thanh pho
#tai My ngay 05/12/2020 va ngay 01/01/2021
#dung thu vien ggplot2 de ve do thi
library("ggplot2", lib.loc="~/R/win-library/4.0")

#Do thi the hien so luong nguoi chet boi covid theo tung ngay o bang Alabama
#layers trong ggplot2 goi la 'geoms'
#su dung goem_point 
#do thi 1: Bieu do diem
ggplot(Alk, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Deaths), colour = "green") + 
  labs(title="Ð??? th??? th??? hi???n s??? lu???ng ca t??? vong b???i covid 19 du???c xác nh???n ??? Alaska t???i M???
                                        t??? 04/2020 d???n 07/2021")

#do thi 2: bieu do banh
#Su dung ggplot ve pie chart
ggplot(df[5:11,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="Ð??? th??? th??? hi???n s??? lu???ng ca t??? vong b???i covid 19 du???c xác nh???n ??? m???t s??? bang/thành ph??? t???i M???
                                                            trong ngày 05-12-2020")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 

# do thi 3: bieu do diem
ggplot(Albm, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Deaths), colour = "orange") +
  labs(title="Ð??? th??? th??? hi???n s??? lu???ng ca t??? vong b???i covid 19 du???c xác nh???n ??? Alabama - M???
                                        t??? 04/2020 d???n 07/2021",x = "Deaths", y="Last Update")

#do thi 4: bieu do cot
ggplot(df, aes(x=Deaths, color=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill =Province_State)) + 
  theme_grey() +
  labs(title="Ð??? th??? th??? hi???n s??? lu???ng ca t??? vong b???i covid 19 du???c xác nh???n ??? các bang/thành ph??? t???i M???
                                                    trong ngày 05-12-2020")

#do thi 5: bieu do diem
ggplot(df, aes(x=Confirmed, y=Province_State, fill= Confirmed)) + 
  geom_point(aes(color=Confirmed)) +
  labs(title="Ð??? th??? th??? hi???n s??? lu???ng ca nhi???m covid 19 du???c xác nh???n ??? các bang/thành ph??? t???i M??? 
                                           trong ngày 05-12-2020",x = "Confirmed", y="Province")

#do thi 6: bieu do diem
ggplot(df, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="Ð??? th??? th??? hi???n s??? lu???ng ca h???i ph???c sau khi nhi???m covid 19 du???c xác nh???n ??? các bang/thành ph??? t???i M??? 
                                             trong ngày 05-12-2020",x = "Recovered", y="Province")

#do thi 7: bieu do banh
ggplot(df[20: 26,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Ð??? th??? th??? hi???n s??? lu???ng ca nhi???m covid 19 du???c xác nh???n ??? m???t s??? bang/thành ph??? t???i M???
                                                      trong ngày 05-12-2020") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 

#do thi 8: bieu do diem 
ggplot(df[10:25,], aes(x=Mortality_Rate, y=Province_State, color=Province_State)) + 
  geom_point() + 
  labs(title="Ð??? th??? th??? hi???n t??? l??? t??? vong b???i covid 19 ??? m???t s??? bang/thành ph??? t???i M???
                                    trong ngày 05-12-2020",x="Mortality Rate", y="Province")


#do thi 9: bieu do banh
ggplot(df[12:20,], aes(x='', y=Mortality_Rate, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="Bi???u d??? th??? hi???n t??? l??? t??? vong b???i covid 19 ??? m???t s??? bang/thành ph???
                               t???i M??? trong ngày 05-12-2020")  +
  geom_text(aes(label = paste0(Mortality_Rate)), position = position_stack(vjust=0.5)) 

#do thi 10: bieu do banh
ggplot(df[1:10,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca h???i ph???c sau khi nhi???m covid 19 du???c xác nh???n ??? m???t s??? bang/thành ph??? 
                                                           t???i M??? trong ngày 05-12-2020") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 

#do thi 11: bieu do banh
#Su dung ggplot ve pie chart
ggplot(df1[20:29,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca t??? vong b???i covid 19 du???c xác nh???n ??? m???t s??? bang/thành ph??? t???i M???
                                                    trong ngày 01-01-2021")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 

#do thi 12: bieu do cot
ggplot(df1[1:11,], aes(x=Deaths, y=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill = Province_State)) + 
  labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca t??? vong do covid 19 du???c xác nh???n ??? m???t s??? bang/thành ph??? t???i M???
                                                  trong ngày 01-01-2021",x = "Deaths", y="Province")

# do thi 13: bieu do diem
ggplot(df1, aes(x=Confirmed, y=Province_State, fill= Confirmed)) + 
  geom_point(aes(color=Confirmed)) +
  labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca nhi???m covid 19 du???c xác nh???n ??? các bang/thành ph??? t???i M???
                                          trong ngày 01-01-2021",x = "Confirmed", y="Province")

#do thi 14: bieu do duong
ggplot(Albm) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  theme_gray() +
  labs(title="Bi???u d??? th??? hi???n t??? l??? t??? vong gây ra b???i covid du???c xác nh???n t???i Alabama - M???
                                        t??? 04/2020 d???n 07/2021", x="Last Update", y="Case Fatality Ratio")

#do thi 15: bieu do diem
ggplot(Alk, aes(x=Confirmed, y=Last_Update, fill = Confirmed)) + 
  geom_point(aes(colour = Confirmed)) +
  labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca nhi???m covid 19 du???c xác nh???n ??? Alaska - M??? 
                                    t??? 04/2020 t???i 07/2021", x = "Confirmed", y="Last Update")

#do thi 16: bieu do diem
ggplot(df1, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca h???i ph???c sau khi nhi???m covid 19 du???c xác nh???n
                              ??? các bang/thành ph??? trong ngày 01-01-2021",x = "Confirmed", y="Province")

#do thi 17: bieu do banh
ggplot(df1[20: 26,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca nhi???m covid 19 du???c xác nh???n ??? m???t s??? bang/thành ph??? ??? M???
                                                         trong ngày 01-01-2021") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 

#do thi 18: bieu do diem
ggplot(df1[15:30,], aes(x=Case_Fatality_Ratio, y=Province_State, color=Province_State)) + 
  geom_point() + labs(title="Bi???u d??? th??? hi???n t??? l??? t??? vong b???i covid 19 ??? m???t s??? bang/thành ph??? t???i M??? 
                                                  ngày 01-01-2021", x="Case Fatality Ratio", y="Province")


#do thi 19: bieu do banh
ggplot(Albm[1:11,], aes(x='', y=Case_Fatality_Ratio, fill=Last_Update)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="Bi???u d??? th??? hi???n t??? l??? t??? vong b???i covid 19 ??? Alabama - M???
                              t??? 04/2020 d???n 07/2021") +
  geom_text(aes(label = paste0(Case_Fatality_Ratio)), position = position_stack(vjust=0.5)) 

#do thi 20: bieu do banh
ggplot(df1[5:15,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Bi???u d??? th??? hi???n s??? lu???ng ca h???i ph???c sau khi nhi???m covid 19 ??? m???t s??? bang/thành ph??? t???i M??? 
                                                        trong ngày 2021-01-01") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 

#do thi 21: Bieu do diem
ggplot(Albm, aes(y=Case_Fatality_Ratio, x=Last_Update, fill= Case_Fatality_Ratio)) + 
  geom_point(aes(color=Case_Fatality_Ratio)) + 
  labs(title="Bi???u d??? th??? hi???n t??? l??? t??? vong gây ra b???i covid du???c xác nh???n t???i Alabama - M???
                                    t??? 04/2020 d???n 07/2021", x="Last Update", y="Case Fatality Ratio")

#do thi 22: Bieu do duong
ggplot(Alk) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  labs(title="Bi???u d??? th??? hi???n t??? l??? t??? vong gây ra b???i covid du???c xác nh???n t???i Alaska - M???
                                  t??? 04/2020 d???n 07/2021", x="Last Update", y="Case Fatality Ratio")