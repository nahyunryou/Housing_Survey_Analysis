library(readxl)
library(dplyr)
library(ggplot2)

#데이터 불러오기
df <- read_excel('2014년도 주거실태조사_공표자료(시군구 명칭 수정).xlsx')



#데이터 원하는 부분만 추출
df2 <- df %>% select(Stype, sido, dq1, q4, q6, q7, q9_1, q18, q23_13, q24_1, q24_2, q26_1, q26_2, dq2_1, q41, q41_1, dq2_3, q40_2, q40_3, q49_5, q48, q8_1, q47_a2_1, q23_9, q42_2, q47_a3_1)



#데이터 변수명 바꾸기
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
              danger = q23_9, # 범죄 등 방범 상태
              want = q42_2,
              master = q47_a3_1)



# 데이터 전처리
df2$house_type <- ifelse(df2$house_type == 1, '일반단독주택', ifelse(df2$house_type == 2, "다가구단독주택", ifelse(df2$house_type == 3, '영업겸용단독주택', ifelse(df2$house_type == 4, '아파트',  ifelse(df2$house_type == 5, '연립주택', ifelse(df2$house_type == 6, '다세대주택', ifelse(df2$house_type == 7, "비거주용건물", ifelse(df2$house_type == 8, "오피스텔", ifelse(df2$house_type == 9, "판잣집", '기타')))))))))
df2$house_location <- ifelse(df2$house_location == 1, "지상",
                             ifelse(df2$house_location == 2, "반지하",
                                    ifelse(df2$house_location == 3, "지하", '옥탑방')))
df2$camera <- ifelse(df2$camera == 1, "있음",
                     ifelse(df2$camera == 2, "없음", '모르겠음'))
df2$thinking <- ifelse(df2$thinking == 1, '그렇다', '아니다')
df2$poor <- ifelse(df2$poor == 1, '기초생활수급자', '비수급자')
real_age <- 2014 - df2$age + 1
df2 <- df2 %>% mutate(ageg = ifelse(real_age < 30, 'young',
                                    ifelse(real_age <= 59, 'middle', 'old')))
df2$master <- ifelse(df2$master == 1, 'male', 'female')
df2$money1 <- ifelse(df2$money1 == 9999999, NA, df2$money1)
df2$earn <- ifelse(df2$earn == 9999999, NA, df2$earn)
df2$real_age <- real_age
df2$sido<-ifelse(df2$sido==11,"서울특별시",ifelse(df2$sido==21,"부산광역시",ifelse(df2$sido==22,"대구광역시",ifelse(df2$sido==23,"인천광역시",ifelse(df2$sido==24,"광주광역시",ifelse(df2$sido==25,"대전광역시",ifelse(df2$sido==26,"울산광역시",ifelse(df2$sido==29,"세종특별자치시",ifelse(df2$sido==31,"경기도",ifelse(df2$sido==32,"강원도", ifelse(df2$sido==33,"충청북도",ifelse(df2$sido==34,"충청남도",ifelse(df2$sido==35,"전라북도",ifelse(df2$sido==36,"전라남도",ifelse(df2$sido==37,"경상북도",ifelse(df2$sido==38,"경상남도",ifelse(df2$sido==39,"제주도","기타")))))))))))))))))

# 거주유형 빈도 분석
table(df2$house_type)
qplot(df2$house_type)



# 지역별 아파트 거주 수 분석
count_sido <- df2 %>% filter(df2$house_type == '아파트') %>%
  group_by(sido) %>%
  summarise(n = n()) %>%
  arrange(-n)
count_sido



# 지역별 아파트 거주하는 사람중 만족도 분석
sido_satisfy <- df2 %>% filter(df2$house_type == '아파트') %>%
  group_by(sido) %>%
  summarise(satisfy_mean = mean(satisfy),
            satisfy_mean2 = mean(satisfy2)) %>%
  mutate(total = (satisfy_mean + satisfy_mean2) / 2) %>%
  arrange(-total)
sido_satisfy



# 반지하, 옥탑방, 지상, 지하 중 CCTV가 없는 비율 분석 및 가시화
df3 <- df2 %>% group_by(house_location, camera) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = round(n / total * 100, 1)) %>%
  filter(camera == '없음') %>% select(house_location, pct)
df3
ggplot(df3, aes(x = house_location,
                y = pct)) + geom_col()



# 꼭 집을 가지고 싶어하는 사람과 그 이유 빈도 분석
think <- df2 %>% group_by(thinking, thinking2) %>% summarise(n = n()) %>% na.omit()
think



# 교통수단 이용 빈도 수 분석
ggplot(data = df2, aes(x = vihicle)) + geom_bar()



# 학력별 사는 곳 분석 및 가시화
school_df <- df2 %>% group_by(school, house_type) %>% summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = round(n / total * 100, 1))
school_df
ggplot(data = school_df, aes(x = house_type, y = pct, fill = school)) +
  geom_col() + coord_flip()



# 소득 구간 막대 그래프
qplot(df2$money1)



# 무주택자 기간 분석
boxplot(df2$no_house_time)




# 주거형태별 소득구간 박스 플롯
ggplot(data = df2, aes(x = df2$house_location,
                       y = df2$earn)) + geom_boxplot() + ylim(0, 1000)




# 주거형태별 평균 소득 분석 및 가시화
poor_table <- df2 %>% group_by(house_location, poor) %>%
  summarise(mean_income = mean(earn))
poor_table
ggplot(data = poor_table, aes(x = house_location, y = mean_income, fill = poor)) +
  geom_col()



# 무주택자를 벗어나기까지 걸리는 시간 분석 및 가시화
ages <- df2$when - df2$age
ages
boxplot(ages)



# 나이별 거주유형별 평균 소득 분석
income_age <- df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, house_type) %>%
  summarise(mean_income = mean(earn))
income_age



# 치안 및 범죄에 대한 시도별 만족도 분석
danger_table = df2 %>%
  group_by(sido, danger) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         pct = n / total * 100) %>%
  filter(danger == 1) %>%
  arrange(-pct)
danger_table



# 나이 구간별 살고 싶은 곳 분석 및 가시화
want_table = df2 %>%
  group_by(ageg, want) %>%
  summarise(n = n())
want_table
ggplot(data = want_table, aes(x = ageg, y = n, fill = want)) + geom_col() +
  scale_x_discrete(limits = c('young', 'middle', 'old'))



# 데이터 남녀 비율 분석
table(df2$master)
qplot(df2$master)



# 성별 나이 구간별 평균 소득 분석
sex_income <- df2 %>%
  group_by(ageg, master) %>%
  summarise(mean_income = mean(earn, na.rm = T))
sex_income
ggplot(data = sex_income, aes(x = ageg,
                              y = mean_income,
                              fill = master)) + geom_col()



# 성별별 나이구간 평균 소득 분석
sex_age = df2 %>%
  filter(!is.na(earn)) %>%
  group_by(real_age, master) %>%
  summarise(mean_income = mean(earn))
sex_age
ggplot(data = sex_age, aes(x = real_age, y = mean_income, col = master)) + geom_line()
