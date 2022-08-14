install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
library(ggplot2)
library(readxl)
library(dplyr)
df<-read_excel("2014년도 주거실태조사_공표자료(시군구 명칭 수정).xlsx",sheet="조사결과")
df2 <- df %>% select(sigungu,q20_1_a,q8_5_a,q50_1,q49_5,q53_1_4,dq2_4_tm,q12_1,q52_4,sido,dq2_3,q12_1,q37_1_a)
df2 <- rename(df2,area_now=q20_1_a,
              area_first=q8_5_a,
              house_money=q12_1,
              commute_time=dq2_4_tm,
              living_expense=q50_1,
              debt=q53_1_4,
              asset=q52_4,
              income=q49_5,
              trans=dq2_3,
              house_money=q12_1,
              area_want=q37_1_a)
df2$sido<-ifelse(df2$sido==11,"서울특별시",ifelse(df2$sido==21,"부산광역시",ifelse(df2$sido==22,"대구광역시",ifelse(df2$sido==23,"인천광역시",ifelse(df2$sido==24,"광주광역시",ifelse(df2$sido==25,"대전광역시",ifelse(df2$sido==26,"울산광역시",ifelse(df2$sido==29,"세종특별자치시",ifelse(df2$sido==31,"경기도",ifelse(df2$sido==32,"강원도", ifelse(df2$sido==33,"충청북도",ifelse(df2$sido==34,"충청남도",ifelse(df2$sido==35,"전라북도",ifelse(df2$sido==36,"전라남도",ifelse(df2$sido==37,"경상북도",ifelse(df2$sido==38,"경상남도",ifelse(df2$sido==39,"제주도","기타")))))))))))))))))


#1 서울 경기도 통근시간 차이

commute<-df2 %>% 
  select(sido,commute_time) %>% 
  filter(sido %in% c("경기도","서울특별시"))
commute=na.omit(commute)
var.test(commute_time~sido,data=commute)
t.test(commute_time~sido,data=commute,var.equal=F)


                          
#2 주택 가격 차이


sido_price<-df2 %>% 
  select(sigungu,house_money) %>% 
  filter(sigungu %in% c('강남구', '수성구', '해운대구')) 
sido_price = na.omit(sido_price) 
bartlett.test(house_money~sigungu,data=sido_price)
oneway.test(house_money~sigungu, data=sido_price,var.equal = T)


#3 현재 주택 면적에 영향을 주는 변수
df2$debt <- ifelse(df2$debt == 9999999, NA, df2$debt)
df2$income <- ifelse(df2$income == 9999999, NA, df2$income)
df2$asset <- ifelse(df2$asset == 9999999, NA, df2$asset)
df2$house_money <- ifelse(df2$house_money == 9999999, NA, df2$house_money)
df2$area_now <- ifelse(df2$area_now == 999, NA, df2$area_now)
df2$area_first <- ifelse(df2$area_first == 999, NA, df2$area_first)
df2$area_want <- ifelse(df2$area_want == 999, NA, df2$area_want)
df2$living_expense <- ifelse(df2$living_expense == 9999, NA, df2$living_expense)

money<-df2 %>% 
  select(area_first,area_now,asset,house_money,income,debt,area_want,living_expense) %>% 
  filter(!is.na(asset)) %>%
  filter(!is.na(income)) %>% 
  filter(!is.na(house_money)) %>% 
  filter(!is.na(debt)) %>% 
  filter(!is.na(area_first)) %>% 
  filter(!is.na(area_now)) %>% 
  filter(!is.na(living_expense))

plot(area_now~area_first,data=money)
plot(area_now~debt,data=money)
plot(area_now~income,data=money)
plot(area_now~asset,data=money)
plot(area_now~house_money,data=money)
plot(area_now~living_expense,data=money)
regression1=lm(area_now~area_first+income+asset+house_money+debt+living_expense,data=money)
summary(regression1)
regression2=lm(area_now~area_first+asset+house_money+debt+living_expense,data=money)
summary(regression2)


