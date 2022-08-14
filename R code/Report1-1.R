install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
library(dplyr)
library(ggplot2)
library(readxl)
df<-read_excel("2014년도 주거실태조사_공표자료(시군구 명칭 수정).xlsx")
df2 <- df %>% select(Stype, sido, dq1, q4, q6, q7, q9_1, q18, q23_13, q24_1, q24_2, q26_1, q26_2, dq2_1, q41, q41_1, dq2_3, q40_2, q40_3, q49_5, q48, q8_1, q47_a2_1, q23_9, q42_2, q47_a3_1)

df2 <- rename(df2, house_type = q4,
              house_location = q6,
              house_money = q7,
              no_house_time = q9_1,
              house_when = q18,
              friendly = q23_13,
              satisfy = q24_1,
              satisfy2 = q24_2,
              camera = q26_1,
              ring = q26_2,
              job = dq2_1,
              thinking = q41,
              thinking2 = q41_1,
              vihicle = dq2_3,
              school = dq1,
              money1 = q40_2,
              money2 = q40_3,
              earn = q49_5,
              poor = q48,
              when = q8_1,
              age = q47_a2_1,
              danger = q23_9, 
              want = q42_2,
              master = q47_a3_1) 

df2$sido<-ifelse(df2$sido==11,"서울특별시",ifelse(df2$sido==21,"부산광역시",ifelse(df2$sido==22,"대구광역시",ifelse(df2$sido==23,"인천광역시",ifelse(df2$sido==24,"광주광역시",ifelse(df2$sido==25,"대전광역시",ifelse(df2$sido==26,"울산광역시",ifelse(df2$sido==29,"세종특별자치시",ifelse(df2$sido==31,"경기도",ifelse(df2$sido==32,"강원도", ifelse(df2$sido==33,"충청북도",ifelse(df2$sido==34,"충청남도",ifelse(df2$sido==35,"전라북도",ifelse(df2$sido==36,"전라남도",ifelse(df2$sido==37,"경상북도",ifelse(df2$sido==38,"경상남도",ifelse(df2$sido==39,"제주도","기타")))))))))))))))))

#1 주택유형 빈도수
#아파트가 젤 많다

df2$house_type <- ifelse(df2$house_type == 1, '일반단독주택', ifelse(df2$house_type == 2, "다가구단독주택", ifelse(df2$house_type == 3, '영업겸용단독주택', ifelse(df2$house_type == 4, '아파트',  ifelse(df2$house_type == 5, '연립주택', ifelse(df2$house_type == 6, '다세대주택', ifelse(df2$house_type == 7, "비거주용건물", ifelse(df2$house_type == 8, "오피스텔", ifelse(df2$house_type == 9, "판잣집", '기타')))))))))

table(df2$house_type)
qplot(df2$house_type)+coord_flip()

# 성별 나이 구간별 평균 소득 분석
df2$master<-ifelse(df2$master==1,"남자","여자")
sex_income <- df2 %>%
  group_by(ageg, master) %>%
  summarise(mean_income = mean(earn, na.rm = T))
sex_income
ggplot(data = sex_income, aes(x = ageg,
                              y = mean_income,
                              fill = master)) + geom_col()



# 성별별 나이구간 평균 소득 분석
df2$master<-ifelse(df2$master==1,"남자","여자")
sex_age = df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, master) %>%
  summarise(mean_income = mean(earn))
sex_age
ggplot(data = sex_age, aes(x = real_age, y = mean_income, col = master)) + geom_line()

#2 최종 졸업 학력이 주거 형태에 미치는 영향
#아파트에 대졸이 많이 산다

df2$school<-ifelse(df2$school==1,"초등학교 졸업 이하",
                   ifelse(df2$school==2,"중학교 졸업",
                          ifelse(df2$school==3,"고등학교 졸업","대학 졸업 이상")))
school_df <- df2 %>% 
  group_by(school, house_type) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = round(n / total * 100, 1))

ggplot(data = school_df, aes(x = house_type, y = pct, fill = school)) + geom_col() + coord_flip()

#3 주택 위치와 카메라
#위험한 위치에 카메라가 더 없다

df2$house_location <- ifelse(df2$house_location == 1, "지상",
                             ifelse(df2$house_location == 2, "반지하",
                                    ifelse(df2$house_location == 3, "지하", "옥상")))

df2$camera <- ifelse(df2$camera == 1, "있음",
                     ifelse(df2$camera == 2, "없음", "모르겠음"))


camera_location <- df2 %>% group_by(house_location, camera) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = round(n / total * 100, 1)) %>%
  filter(camera == '없음') %>% 
  select(house_location, pct)

ggplot(camera_location, aes(x = house_location,y = pct)) + geom_col()

#4.나이에 따른 향후 살고싶은 생활양식
#연령대 모두 도시적 생활을 선호 나이가 들수록 전원생활 선호

df2 <- df2 %>% mutate(ageg = ifelse(real_age < 30, 'young',
                                    ifelse(real_age <= 59, 'middle', 'old')))
df2$want<-ifelse(df2$want==1,"도시적 생활",
                 ifelse(df2$want==2,"전원생활","잘모르겠음"))
want_table = df2 %>%
  group_by(ageg, want) %>%
  summarise(n = n())
want_table

ggplot(data = want_table, aes(x = ageg, y = n, fill = want)) + geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old'))


#5 주거형태별 소득 박스 플롯
#지상에 사는 사람이 소득이 가장 높이 분포해 있고 반지하, 지하, 옥상 순으로 분포

df2$earn <- ifelse(df2$earn == 9999999, NA, df2$earn)
ggplot(data = df2, aes(x = df2$house_location, y = df2$earn)) + geom_boxplot() + ylim(0, 700)

#6 내집 마련 찬반/ 이유

df2$thinking <- ifelse(df2$thinking == 1, "그렇다", "아니다")
qplot(df2$thinking)
df2$thinking2<- ifelse(df2$thinking2==1, "주거안정",
                       ifelse(df2$thinking2==2, "자산증식","기타"))
think <- df2 %>% 
  group_by(thinking, thinking2) %>% 
  summarise(n = n())%>% 
  na.omit()

ggplot(data = think, aes(thinking, y = n, fill = thinking2)) + geom_col(position = 'dodge')

#7 직장까지 교통수단
table(is.na(df2$vihicle))

df2$vihicle<-ifelse(df2$vihicle==1,"승용차",ifelse(df2$vihicle==2,"대중교통",ifelse(df2$vihicle==3,"보도",ifelse(df2$vihicle==4,"자전거",ifelse(df2$vihicle==5,"오토바이","기타")))))

vihicle_nomiss<-df2 %>% 
  filter(!is.na(vihicle)) %>% 
  group_by(vihicle) %>% 
  summarise(n=n())
vihicle_nomiss

ggplot(data = vihicle_nomiss, aes(x =vihicle, y=n)) + geom_col()+ ylim(0,8000)


#8 서울 아파트 자가 비율


df2$house_money<-ifelse(df2$house_money==1,"자가",ifelse(df2$house_money==2,"전세",ifelse(df2$house_money==3,"보증금 있는 월세",ifelse(df2$house_money==4,"보증금 없는 월세",ifelse(df2$house_money==5,"사글세 또는 연세",ifelse(df2$house_money==6,"일세","무상"))))))
                                               
                                        
current_mine <- df2 %>%                                          
  filter(sido == "서울특별시" & house_type == "아파트") %>% 
  group_by(house_money) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n), pct = n / total * 100)
current_mine

#9 가장 위험한 지역

danger_table = df2 %>% 
  group_by(sido, danger) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n),
         pct = n / total * 100) %>%
  filter(danger == 1) %>%
  arrange(-pct)
danger_table

#10 가구주 여자가 많나 남자가 많나

df2$master <- ifelse(df2$master == 1, 'male', 'female')
table(df2$master)
qplot(df2$master)

real_age <- 2014 - df2$age + 1
real_age

sex_age = df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, master) %>%
  summarise(mean_income = mean(earn))

sex_age

ggplot(data = sex_age, aes(x = real_age, y = mean_income, col = master)) + geom_line()

df2$real_age <- real_age

income_age <- df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, house_type) %>%
  summarise(mean_income = mean(earn))
income_age


ggplot(data = income_age, aes(x = real_age,
                              y = mean_income, col = house_type)) + geom_boxplot() + ylim(0, 500)


#--

count_sido <- df2 %>% filter(df2$house_type == '아파트') %>%
  group_by(sido) %>%
  summarise(n = n()) %>%
  arrange(-n)

sido_satisfy <- df2 %>% filter(df2$house_type == '아파트') %>%
  group_by(sido) %>%
  summarise(satisfy_mean = mean(satisfy),
            satisfy_mean2 = mean(satisfy2)) %>%
  mutate(total = (satisfy_mean + satisfy_mean2) / 2) %>%
  arrange(-total)

ggplot(data = sido_satisfy, aes(x = sido, y = total)) + geom_line()








#--

summary(df2$money1)
df2$money1 <- ifelse(df2$money1 == 9999999, NA, df2$money1)
summary(df2$money1)
qplot(df2$money1)


#--

boxplot(df2$no_house_time)

#--



#--

df2$poor <- ifelse(df2$poor == 1, '기초생활수급자', '비수급자')

poor_table <- df2 %>% group_by(house_location, poor) %>%
  summarise(mean_income = mean(earn))
poor_table

ggplot(data = poor_table, aes(x = house_location, y = mean_income, fill = poor)) +
  geom_col()

#--

ages <- df2$when - df2$age
ages

boxplot(ages)

#--

real_age <- 2014 - df2$age + 1
real_age

df2$real_age <- real_age

income_age <- df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, house_type) %>%
  summarise(mean_income = mean(earn))
income_age


ggplot(data = income_age, aes(x = real_age,
                              y = mean_income, col = house_type)) + geom_boxplot() + ylim(0, 500)

#가장 위험한 지역

danger_table = df2 %>% 
  group_by(sido, danger) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n),
         pct = n / total * 100) %>%
  filter(danger == 1) %>%
  arrange(-pct)
danger_table

#가구주 여자가 많나 남자가 많나

df2$master <- ifelse(df2$master == 1, 'male', 'female')
table(df2$master)
qplot(df2$master)

#--

sex_income <- df2 %>%
  group_by(real_age, master) %>%
  summarise(mean_income = mean(earn))

ggplot(data = sex_income, aes(x = real_age,
                              y = mean_income,
                              fill = master)) + geom_col()

sex_age = df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, master) %>%
  summarise(mean_income = mean(earn))

sex_age

ggplot(data = sex_age, aes(x = real_age, y = mean_income, col = master)) + geom_line()

real_age <- 2014 - df2$age + 1
sex_age = df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, master) %>%
  summarise(mean_income = mean(earn))
sex_age
ggplot(data = sex_age, aes(x = real_age, y = mean_income, col = master)) + geom_line()

sex_income <- df2 %>%
  group_by(ageg, master) %>%
  summarise(mean_income = mean(earn, na.rm = T))
sex_income
ggplot(data = sex_income, aes(x = ageg,
                              y = mean_income,
                              fill = master)) + geom_col()+scale_x_discrete(limits = c('young', 'middle', 'old'))


