setwd("D:/CNU/3-2/범주형자료분석/프로젝트")
weatherAUS <- read.csv("wind_dir.csv",fileEncoding = "EUC-KR" , header = TRUE)
head(weatherAUS)

data <- weatherAUS
names(data) <- c("X", "최저기온", "최고기온", "일일강수량", "돌풍향", "돌풍속", "돌풍향9시", "돌풍향3시", 
                 "돌풍속9시", "돌풍속3시", "습도9시", "습도3시", "기압9시", "기압3시", 
                 "기온9시", "기온3시", "오늘강수여부", "내일강수여부", "지역분류", "계절" )


cor(data)
View(data)
data[2]
sum(as.numeric(is.na(data$돌풍향)))
# install.packages("dplyr")
library(dplyr)

mat <- data %>% select(2:4,6,9:18) %>% na.omit() %>% cor() %>% round(digits = 3) 
hist(mat,breaks = 20)

# 상관 행렬을 long 형식으로 변환
install.packages("gtable")
install.packages("colorspace")
install.packages("farver")
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)


cor_melted <- melt(mat)

# ggplot을 사용한 히트맵 생성 (숫자 포함)
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(x = "", y = "", title = "Correlation Matrix")


##########################
# 각 변수에 대한 boxplot 생성 함수
create_boxplot <- function(data, var) {
  ggplot(data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "lightblue", color = "blue") +
    theme_minimal() +
    labs(title = var, y = NULL, x = NULL) +
    theme(plot.title = element_text(hjust = 0.5))
}

data_con <- data %>% select(2:4,6,9:16) %>% na.omit()

install.packages("patchwork")
install.packages("purrr")
library(patchwork)
library(purrr)
# 모든 변수에 대해 boxplot 생성
plots <- data_con %>%
  names() %>%
  map(~create_boxplot(data_con, .)) %>%
  reduce(`+`)

# 그래프 배열 설정 및 출력
plots + plot_layout(ncol = 4) +
  plot_annotation(title = "Boxplots of weather Variables",
                  theme = theme(plot.title = element_text(hjust = 0.5)))


########### 산점도 ############
install.packages("GGally")
library(GGally)

ggpairs(data_con) +
  ggtitle("Scatterplot Matrix of weather")
###  psych 패키지 ###
install.packages("psych")
library(psych)
pairs.panels(data_con,
             main = "Scatterplot Matrix of weather")
###############################


str(weatherAUS)


glimpse(weatherAUS)

sum(is.na(data))  # 전체 결측치 개수
colSums(is.na(weatherAUS))  # 변수별 결측치 개수
nas <- round(colSums(is.na(weatherAUS)) / length(weatherAUS[,1]), 4)*100 # 변수별 결측치 백분율
barplot(nas)

# 결측치 처리 예시
# data_clean <- na.omit(data)  # 결측치가 있는 행 제거
# 또는
# data$variable <- ifelse(is.na(data$variable), mean(data$variable, na.rm = TRUE), data$variable)  # 평균으로 대체

summary(weatherAUS)

names(weatherAUS)
length(weatherAUS)
# 변수 나누기
# categorical <- weatherAUS[c(1,2,8,10,11,18,19,22,23)]
# continuous <-  weatherAUS[c(1,3,4,5,6,7,9,12,13,14,15,16,17,20,21)]
# table(categorical$WindGustDir)
# hist(continuous$MinTemp)

# locs <- unique(weatherAUS$Location)


# 각 지역별로 날짜에는 중복이 없다. 데이터 양은 차이가 있다.
for (i in locs) {
  ss <- subset(weatherAUS,Location==i)
  print(i)
  print(length(ss$Date)==length(unique(ss$Date)) )
  print(length(ss$Date))
}

# 범주형 테이블
cat_names <- names(categorical[-c(1,9)])
cnt=0
for(i in categorical[-c(1,9)]) {
  cnt = cnt + 1
  print(table(i, main = cat_names[cnt]))
}
length(categorical)

# 연속형 히스토그램
con_names <- names(continuous[-1])
cnt=0
for (i in continuous[-1]) {
  cnt = cnt + 1
  hist(i, main = con_names[cnt])
}

############################## recode ##################################################


# 필요한 라이브러리 로드
library(dplyr)
  
# 지역 목록을 데이터 프레임으로 생성
regions <- data.frame(
  region = c("Albury", "BadgerysCreek", "Cobar", "CoffsHarbour", "Moree", "Newcastle", 
             "NorahHead", "NorfolkIsland", "Penrith", "Richmond", "Sydney", "SydneyAirport", 
             "WaggaWagga", "Williamtown", "Wollongong", "Canberra", "Tuggeranong", "MountGinini", 
             "Ballarat", "Bendigo", "Sale", "MelbourneAirport", "Melbourne", "Mildura", 
             "Nhil", "Portland", "Watsonia", "Dartmoor", "Brisbane", "Cairns", 
             "GoldCoast", "Townsville", "Adelaide", "MountGambier", "Nuriootpa", "Woomera", 
             "Albany", "Witchcliffe", "PearceRAAF", "PerthAirport", "Perth", "SalmonGums", 
             "Walpole", "Hobart", "Launceston", "AliceSprings", "Darwin", "Katherine", "Uluru")
)

### recode 함수를 사용하여 Location을 해안/내륙과 방위로 분류 -> location_type
data <- data %>%
  mutate(location_type  = recode(data$Location,
                              "CoffsHarbour" = "Eastern Coastal",
                              "Newcastle" = "Eastern Coastal",
                              "NorahHead" = "Eastern Coastal",
                              "NorfolkIsland" = "Eastern Coastal",
                              "Sydney" = "Eastern Coastal",
                              "SydneyAirport" = "Eastern Coastal",
                              "Wollongong" = "Eastern Coastal",
                              "Brisbane" = "Eastern Coastal",
                              "Cairns" = "Northern Coastal",
                              "GoldCoast" = "Eastern Coastal",
                              "Townsville" = "Northern Coastal",
                              "Adelaide" = "Southern Coastal",
                              "Albany" = "Southern Coastal",
                              "Perth" = "Western Coastal",
                              "Hobart" = "Southern Coastal",
                              "Launceston" = "Southern Coastal",
                              "Darwin" = "Northern Coastal",
                              "Katherine" = "Northern Coastal",
                              "MountGambier" = "Southern Coastal",
                              "PerthAirport" = "Western Coastal",
                              
                              # 내륙 지역
                              "Albury" = "Eastern Inland",
                              "BadgerysCreek" = "Eastern Inland",
                              "Cobar" = "Western Inland",
                              "Moree" = "Eastern Inland",
                              "Penrith" = "Eastern Inland",
                              "Richmond" = "Eastern Inland",
                              "WaggaWagga" = "Eastern Inland",
                              "Williamtown" = "Eastern Inland",
                              "Canberra" = "Eastern Inland",
                              "Tuggeranong" = "Eastern Inland",
                              "MountGinini" = "Eastern Inland",
                              "Ballarat" = "Southern Inland",
                              "Bendigo" = "Southern Inland",
                              "Sale" = "Southern Inland",
                              "MelbourneAirport" = "Southern Inland",
                              "Melbourne" = "Southern Inland",
                              "Mildura" = "Western Inland",
                              "Nhil" = "Western Inland",
                              "Portland" = "Southern Inland",
                              "Watsonia" = "Southern Inland",
                              "Dartmoor" = "Southern Inland",
                              "Woomera" = "Central Inland",
                              "Witchcliffe" = "Western Inland",
                              "PearceRAAF" = "Western Inland",
                              "SalmonGums" = "Western Inland",
                              "Walpole" = "Western Inland",
                              "AliceSprings" = "Central Inland",
                              "Uluru" = "Central Inland",
                              "Nuriootpa" = "Southern Inland",
                              .default = "Unknown"
  ))

locs_type <- unique(data$location_type)

View(data)

###
# 날짜열에 계절을 추가하는 함수
get_season <- function(date_str) {
  date <- as.Date(date_str, format = "%Y-%m-%d")
  month <- as.numeric(format(date, "%m"))
  
  if (month >= 9 & month <= 11) {
    return("Spring")   # 봄
  } else if (month >= 12 | month <= 2) {
    return("Summer")   # 여름
  } else if (month >= 3 & month <= 5) {
    return("Autumn")   # 가을
  } else {
    return("Winter")   # 겨울
  }
}

# 데이터프레임에 계절을 추가
data <- data %>%
  mutate(season = sapply(Date, get_season))

# RainToday, RainTommorow 0(No),1(Yes) 코딩
data$RainToday <- ifelse(data$RainToday == "Yes", 1, 0)
data$RainTomorrow <- ifelse(data$RainTomorrow == "Yes", 1, 0)

#####################################################################################
# 풍향확인
unique(weatherAUS$WindGustDir); length(unique(weatherAUS$WindGustDir))


names(data)
install.packages("dplyr")
library(dplyr)


# 풍향 (순서형 팩터) 
wind_directions <- factor(
  c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"), 
  levels = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"), 
  ordered = TRUE
  )

# 변수에 factor 삽입
data$돌풍향 <- factor(data$돌풍향, levels = levels(wind_directions), ordered = TRUE)
# 풍향을 각도로 변환 
data$돌풍향 <- as.numeric(data$돌풍향) * (360 / 16) 
# 사인과 코사인 변환 
data$돌풍향_sin <- sin(data$돌풍향 * pi / 180) 
data$돌풍향_cos <- cos(data$돌풍향 * pi / 180) 

### 9시
# 변수에 factor 삽입
data$돌풍향9시 <- factor(data$돌풍향9시, levels = levels(wind_directions), ordered = TRUE)
# 풍향을 각도로 변환 
data$돌풍향9시 <- as.numeric(data$돌풍향9시) * (360 / 16) 
# 사인과 코사인 변환 
data$돌풍향9시_sin <- sin(data$돌풍향9시 * pi / 180) 
data$돌풍향9시_cos <- cos(data$돌풍향9시 * pi / 180) 

### 3시
# 변수에 factor 삽입
data$돌풍향3시 <- factor(data$돌풍향3시, levels = levels(wind_directions), ordered = TRUE)
# 풍향을 각도로 변환 
data$돌풍향3시 <- as.numeric(data$돌풍향3시) * (360 / 16) 
# 사인과 코사인 변환 
data$돌풍향3시_sin <- sin(data$돌풍향3시 * pi / 180) 
data$돌풍향3시_cos <- cos(data$돌풍향3시 * pi / 180) 


### 회귀 모델 구축 # 모든 변수 사용
model <- glm(내일강수여부 ~ 최저기온 + 최고기온 + 일일강수량 + 
               돌풍속 + 돌풍속9시 + 돌풍속3시 + 습도9시 + 습도3시 + 기압9시 + 기압3시 + 
               기온9시 + 기온3시 + 오늘강수여부 + 지역분류 + 계절 + 
               돌풍향_sin + 돌풍향_cos + 돌풍향9시_sin + 돌풍향9시_cos + 돌풍향3시_sin + 돌풍향3시_cos, 
             data = data, 
             family = binomial
             )

summary(model)

# vif
install.packages("quantreg")
install.packages("car")
install.packages("MASS")
library(MASS)
library(car)
vif(model)

# write.csv(data, file = 'C:/Users/pc18/Downloads/wind_dir.csv', fileEncoding = 'EUC-KR')

######## 돌풍향 상관행렬 ########

# 상관 행렬을 long 형식으로 변환
install.packages("gtable")
install.packages("colorspace")
install.packages("farver")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("stringi")
library(ggplot2)
library(reshape2)


mat <- data %>% select(21:26) %>% na.omit() %>% cor() %>% round(digits = 3) 
cor_melted <- melt(mat)

# ggplot을 사용한 히트맵 생성 (숫자 포함)
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(x = "", y = "", title = "Correlation Matrix")

#####################################################
data2 <- data[-c(1,5,7,8,18)]
data3 <- data2[-c(15,16)] # PCA
head(data2)

### 수치형 변수만 PCA
dat.pca <- prcomp(data3, center = T, scale. = T)
plot(dat.pca, type='l')
summary(dat.pca)

### 범주형 포함 PCA
# install.packages("FactoMineR")
library(FactoMineR)
result <- FAMD(data2, graph = FALSE)
summary(result)

### 인자분석
install.packages("psych")
library(psych)
library(dplyr)

# 범주형 변수를 더미 변수로 변환 (필요한 경우)
data_encoded <- data2 %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

# 인자 수를 지정하여 인자분석 수행
fa_result <- fa(data_encoded, nfactors = 5, rotate = "varimax", fm = "ml")
fa_result2 <- fa(data_encoded, nfactors = 7, rotate = "varimax", fm = "ml")
fa_result3 <- fa(data_encoded, nfactors = 8, rotate = "varimax", fm = "ml")
fa_result4 <- fa(data_encoded, nfactors = 6, rotate = "varimax", fm = "ml")



# 결과 요약
print(fa_result)
print(fa_result2)

# 인자 부하량 확인
print(fa_result$loadings, cutoff = 0.4)
print(fa_result2$loadings, cutoff = 0.4) 
print(fa_result3$loadings, cutoff = 0.4)
print(fa_result4$loadings, cutoff = 0.4) 

# 스크리 플롯
scree(data_encoded)


####### FA 결과물로 GLM 생성 ######
# 인자 점수 계산
factor_scores <- factor.scores(data_encoded, fa_result4)$scores


# 추가하고 싶은 변수 선택
additional_vars <- data2[, c("일일강수량", "지역분류", "계절")]

# 인자 점수와 추가 변수 결합
combined_data <- cbind(as.data.frame(factor_scores), additional_vars)
cdat <- cbind(target = data$내일강수여부, combined_data)
names(cdat) <- c("내일강수여부", "기온", "돌풍속", "기압", "습도", "돌풍향_cos", "돌풍향_sin", "일일강수량", "지역분류", "계절" )
# GLM 모델 생성 (예: 이진 분류를 위한 로지스틱 회귀)
glm_model <- glm(내일강수여부 ~ ., 
                 data = cdat,
                 family = binomial())

summary(glm_model)

### 인자에 영향 적은 변수 제외하고 2차 인자분석
# FA 부분
data4 <- data2[,-c(3,14,15)] # data2에서 인자랑 별 관련 없던 변수 제거
fa_result5 <- fa(data4, nfactors = 6, rotate = "varimax", fm = "ml")
print(fa_result5$loadings, cutoff = 0.4) # 인자 분류 변수는 동일

# 모형 적합 부분
factor_scores2 <- factor.scores(data4, fa_result5)$scores
combined_data2 <- cbind(as.data.frame(factor_scores2), additional_vars)
cdat2 <- cbind(target = data$내일강수여부, combined_data2)
names(cdat2) <- c("내일강수여부", "기온", "돌풍속", "기압", "습도", "돌풍향_cos", "돌풍향_sin", "일일강수량", "지역분류", "계절" )
glm_model2 <- glm(내일강수여부 ~ ., 
                 data = cdat2,
                 family = binomial())
summary(glm_model2)


# 변수 중요도 확인
importance <- abs(coef(glm_model))[-1]  # 절편 제외
importance_df <- data.frame(variable = names(importance),
                            importance = as.vector(importance))
importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
print(importance_df)

# 예측
predictions <- predict(glm_model, newdata = cdat, type = "response")
names(cdat)
# ROC 곡선 (이진 분류의 경우)
library(pROC)
roc_curve <- roc(cdat$내일강수여부, predictions)
plot(roc_curve)
auc(roc_curve)



#####################
# hold-out으로 평가 #
#####################

# 데이터를 8:2로 쪼개기 위한 글로벌 시드 고정 및 인덱스 추출
set.seed(42)
train_idx <- sample(1:nrow(cdat), size = 0.8 * nrow(cdat))

train_set <- cdat[train_idx, ]
test_set  <- cdat[-train_idx, ]

# 학습(Train) 데이터로만 모델을 만듭니다.
tuned_glm <- glm(내일강수여부 ~ ., data = train_set, family = binomial())

# 테스트(Test) 데이터로 예측을 수행하여 진짜 성능을 봅니다.
test_preds <- predict(tuned_glm, newdata = test_set, type = "response")

roc_curve_hold_out <- roc(test_set$내일강수여부, test_preds)
plot(roc_curve_hold_out)
auc(roc_curve_hold_out)


##########################
# 정확도, 민감도, 특이도 #
##########################

boxplot(test_preds) # 0.5 기준으로 예측하면 좋지 않을것처럼 보임


# 1. 임계값(Threshold) 서치 그리드 및 결과 저장 장부 생성
thresholds <- seq(0.1, 0.9, by = 0.01)
tuning_results <- data.frame(Threshold = thresholds, Accuracy = NA, Sensitivity = NA, Specificity = NA, F1_Score = NA)

actual_labels <- test_set$내일강수여부

# 2. 임계값 반복 순회 연산 (Grid Search)
for(i in 1:length(thresholds)) {
  t <- thresholds[i]
  pred_binary <- ifelse(test_preds > t, 1, 0)
  
  # 팩터 레벨 강제로 고정하여 빈 테이블 방지
  cm <- table(factor(actual_labels, levels=c(0,1)), factor(pred_binary, levels=c(0,1)))
  
  TN <- cm[1,1]; FP <- cm[1,2]; FN <- cm[2,1]; TP <- cm[2,2]
  
  # 평가지표 산출
  acc  <- (TP + TN) / sum(cm)
  sens <- ifelse((TP + FN) > 0, TP / (TP + FN), 0) # Sensitivity (Recall)
  spec <- ifelse((TN + FP) > 0, TN / (TN + FP), 0) # Specificity
  prec <- ifelse((TP + FP) > 0, TP / (TP + FP), 0) # Precision
  
  # Precision과 Sensitivity의 조화평균 (F1-Score)
  f1   <- ifelse((prec + sens) > 0, 2 * (prec * sens) / (prec + sens), 0)
  
  tuning_results[i, 2:5] <- c(acc, sens, spec, f1)
}

# 3. F1-Score를 최대로 끌어올리는 최적 임계값 행 인출
best_row <- tuning_results[which.max(tuning_results$F1_Score), ]

cat("\n============================================\n")
cat("   F1-Score 기준 최적 임계값(Threshold) 탐색 결과   \n")
cat("============================================\n")
cat("최적 임계값 (Best Threshold) : ", round(best_row$Threshold, 4), "\n", sep="")
cat("F1-Score             : ", round(best_row$F1_Score, 4), "\n", sep="")
cat("정확도 (Accuracy)     : ", round(best_row$Accuracy, 4), "\n", sep="")
cat("민감도 (Sensitivity)  : ", round(best_row$Sensitivity, 4), "n", sep="")
cat("특이도 (Specificity)  : ", round(best_row$Specificity, 4), "\n", sep="")
cat("============================================\n")







