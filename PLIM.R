# 必要なライブラリを読み込み
library(ggplot2)
library(dplyr)
library(tidyr)

# 試験的データの生成
set.seed(123)  # 再現性のため

# 実験条件の設定
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)

# データフレームの作成
data <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points
) %>%
  rowwise() %>%
  mutate(
    # Shannon Diversityの試験的な値を生成
    # 一般的に2-4の範囲で、時間とともに変化するパターンを想定
    Shannon_Diversity = case_when(
      Sample == "UAC" ~ 3.5 + 0.3 * sin(Time/24) + 
        ifelse(Gravity == "0g", -0.5, 0) + 
        ifelse(Gravity == "1/6g", -0.2, 0) + 
        rnorm(1, 0, 0.1),
      Sample == "MAC" ~ 2.8 + 0.4 * cos(Time/30) + 
        ifelse(Gravity == "0g", -0.7, 0) + 
        ifelse(Gravity == "1/3g", 0.2, 0) + 
        rnorm(1, 0, 0.15),
      Sample == "AC" ~ 3.2 + 0.2 * (Time/72) + 
        ifelse(Gravity == "0g", -0.3, 0) + 
        ifelse(Gravity == "1/6g", 0.1, 0) + 
        rnorm(1, 0, 0.12)
    )
  ) %>%
  ungroup() %>%
  # Shannon Diversityが負の値にならないよう調整
  mutate(Shannon_Diversity = pmax(Shannon_Diversity, 0.5))

# 重力環境の順序を設定
data$Gravity <- factor(data$Gravity, levels = c("1g", "1/3g", "1/6g", "0g"))
# メインプロット: サンプル別にファセット分割
p1 <- ggplot(data, aes(x = Time, y = Shannon_Diversity, color = Gravity)) +
  geom_line(size = 0.8, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 1) +
  facet_wrap(~ Sample, nrow = 1) +
  scale_color_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                "1/6g" = "#808080", "0g" = "#C0C0C0"),
                     name = "Gravity") +
  scale_x_continuous(breaks = c(0, 24, 48, 72),
                     labels = c("0", "24", "48", "72")) +
  labs(
    x = "Time (h)",
    y = "Shannon Diversity"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7)
  )

# 代替プロット: 重力環境別にファセット分割
p2 <- ggplot(data, aes(x = Time, y = Shannon_Diversity, color = Sample)) +
  geom_line(size = 0.8, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 1) +
  facet_wrap(~ Gravity, nrow = 2) +
  scale_color_manual(values = c("UAC" = "#000000", "MAC" = "#505050", "AC" = "#A0A0A0"),
                     name = "Sample") +
  scale_x_continuous(breaks = c(0, 24, 48, 72),
                     labels = c("0", "24", "48", "72")) +
  labs(
    x = "Time (h)",
    y = "Shannon Diversity"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7)
  )

# 統計サマリーの表示
print("データサマリー:")
print(data %>%
        group_by(Sample, Gravity) %>%
        summarise(
          Mean_Shannon = round(mean(Shannon_Diversity), 3),
          SD_Shannon = round(sd(Shannon_Diversity), 3),
          .groups = "drop"
        ))

# プロットの表示
print(p1)
print(p2)







#文字サイズ修正
# 必要なライブラリを読み込み
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)  # 表の整形用

# 試験的データの生成
set.seed(123)  # 再現性のため

# 実験条件の設定
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)

# データフレームの作成
data <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points
) %>%
  rowwise() %>%
  mutate(
    # Shannon Diversityの試験的な値を生成
    # 一般的に2-4の範囲で、時間とともに変化するパターンを想定
    Shannon_Diversity = case_when(
      Sample == "UAC" ~ 3.5 + 0.3 * sin(Time/24) + 
        ifelse(Gravity == "0g", -0.5, 0) + 
        ifelse(Gravity == "1/6g", -0.2, 0) + 
        rnorm(1, 0, 0.1),
      Sample == "MAC" ~ 2.8 + 0.4 * cos(Time/30) + 
        ifelse(Gravity == "0g", -0.7, 0) + 
        ifelse(Gravity == "1/3g", 0.2, 0) + 
        rnorm(1, 0, 0.15),
      Sample == "AC" ~ 3.2 + 0.2 * (Time/72) + 
        ifelse(Gravity == "0g", -0.3, 0) + 
        ifelse(Gravity == "1/6g", 0.1, 0) + 
        rnorm(1, 0, 0.12)
    )
  ) %>%
  ungroup() %>%
  # Shannon Diversityが負の値にならないよう調整
  mutate(Shannon_Diversity = pmax(Shannon_Diversity, 0.5))

# 重力環境の順序を設定
data$Gravity <- factor(data$Gravity, levels = c("1g", "1/3g", "1/6g", "0g"))

# メインプロット: サンプル別にファセット分割
p1 <- ggplot(data, aes(x = Time, y = Shannon_Diversity, color = Gravity)) +
  geom_line(size = 0.8, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 1) +
  facet_wrap(~ Sample, nrow = 1) +
  scale_color_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                "1/6g" = "#808080", "0g" = "#C0C0C0"),
                     name = "Gravity") +
  scale_x_continuous(breaks = c(0, 24, 48, 72),
                     labels = c("0", "24", "48", "72")) +
  labs(
    x = "Time (h)",
    y = "Shannon Diversity"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7)
  )

# 代替プロット: 重力環境別にファセット分割
p2 <- ggplot(data, aes(x = Time, y = Shannon_Diversity, color = Sample)) +
  geom_line(size = 0.8, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 1) +
  facet_wrap(~ Gravity, nrow = 2) +
  scale_color_manual(values = c("UAC" = "#000000", "MAC" = "#505050", "AC" = "#A0A0A0"),
                     name = "Sample") +
  scale_x_continuous(breaks = c(0, 24, 48, 72),
                     labels = c("0", "24", "48", "72")) +
  labs(
    x = "Time (h)",
    y = "Shannon Diversity"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7)
  )

# 統計サマリーの表示
print("データサマリー:")
print(data %>%
        group_by(Sample, Gravity) %>%
        summarise(
          Mean_Shannon = round(mean(Shannon_Diversity), 3),
          SD_Shannon = round(sd(Shannon_Diversity), 3),
          .groups = "drop"
        ))

# プロットの表示
print(p1)
print(p2)




















# 増減殖傾向のある細菌種の遺伝子発現相対比率の変化をリッジラインプロット
# 必要なライブラリを読み込み
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)

# 試験的データの生成
set.seed(789)

# 実験条件の設定
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)

# 重力環境に応じて増減殖傾向のある細菌種
bacteria_species <- c("Lactobacillus", "Bifidobacterium", "Bacteroides", 
                      "Clostridium", "Escherichia", "Enterococcus")

# 各細菌種の重力感受性パターンを定義
gravity_sensitivity <- tibble(
  Species = bacteria_species,
  # 重力減少に対する基本応答（正: 増殖促進, 負: 増殖抑制）
  gravity_response = c(
    "Lactobacillus" = -0.8,     # 重力減少で減殖
    "Bifidobacterium" = 0.6,    # 重力減少で増殖
    "Bacteroides" = -0.3,       # 軽度減殖
    "Clostridium" = 0.9,        # 強い増殖促進
    "Escherichia" = -1.2,       # 強い減殖
    "Enterococcus" = 0.4        # 軽度増殖
  )
)

# 各条件で細菌増減殖データを生成
data <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points,
  Species = bacteria_species
) %>%
  left_join(gravity_sensitivity, by = "Species") %>%
  rowwise() %>%
  mutate(
    # 重力効果の強度（1g=0, 1/3g=0.33, 1/6g=0.67, 0g=1.0）
    gravity_effect = case_when(
      Gravity == "1g" ~ 0,
      Gravity == "1/3g" ~ 0.33,
      Gravity == "1/6g" ~ 0.67,
      Gravity == "0g" ~ 1.0
    ),
    # 時間係数（時間とともに効果が蓄積）
    time_factor = (Time / 24) * 0.5 + 0.5,
    # 各条件で20-25個の増減殖測定値を生成
    n_measurements = sample(20:25, 1)
  ) %>%
  ungroup() %>%
  # 各行を測定値の数だけ展開
  slice(rep(1:n(), times = .$n_measurements)) %>%
  group_by(Sample, Gravity, Time, Species) %>%
  mutate(
    Measurement_ID = row_number(),
    # 増減殖比率計算（log2 fold change）
    Growth_Ratio = gravity_response * gravity_effect * time_factor + 
      rnorm(1, 0, 0.4) +  # ベースノイズ
      # サンプル間の差
      case_when(
        Sample == "MAC" ~ rnorm(1, 0.1, 0.2),
        Sample == "AC" ~ rnorm(1, -0.1, 0.2),
        TRUE ~ 0
      )
  ) %>%
  ungroup()

# 重力とサンプルの順序を設定
data$Gravity <- factor(data$Gravity, levels = c("1g", "1/3g", "1/6g", "0g"))
data$Sample <- factor(data$Sample, levels = c("UAC", "MAC", "AC"))
data$Species <- factor(data$Species, levels = rev(bacteria_species))

# 各条件での平均値を計算
mean_values <- data %>%
  group_by(Sample, Gravity, Time, Species) %>%
  summarise(Mean_Growth = mean(Growth_Ratio), .groups = "drop")

# メインプロット: 重力環境別ファセット、縦軸が菌種名、横軸が時間
p1 <- data %>%
  filter(Sample == "UAC") %>%
  ggplot(aes(x = Time, y = Species)) +
  geom_density_ridges(aes(height = after_stat(density), fill = after_stat(x)), 
                      stat = "density_ridges", alpha = 0.8, scale = 1.2,
                      rel_min_height = 0.01, color = "black", size = 0.3) +
  # 平均値を数値で表示
  geom_text(data = mean_values %>% filter(Sample == "UAC"),
            aes(x = Time, y = Species, label = round(Mean_Growth, 2)),
            vjust = -0.3, hjust = 0.5, size = 2.8, color = "black", fontface = "bold") +
  # 増減殖を色で表現（赤: 減殖, 青: 増殖）
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
                       midpoint = 0, name = "Growth\nRatio") +
  scale_x_continuous(breaks = time_points, labels = paste0(time_points, "h")) +
  labs(
    x = "Time (h)",
    y = "Bacterial Species"
  ) +
  facet_wrap(~ Gravity, nrow = 1) +
  theme_ridges() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank()
  )

# モノクロ版（学術用）
p2 <- data %>%
  filter(Sample == "UAC") %>%
  ggplot(aes(x = Time, y = Species)) +
  geom_density_ridges(aes(height = after_stat(density), fill = Gravity), 
                      stat = "density_ridges", alpha = 0.7, scale = 1.0,
                      rel_min_height = 0.01, color = "black", size = 0.3) +
  # 平均値を数値で表示
  geom_text(data = mean_values %>% filter(Sample == "UAC"),
            aes(x = Time, y = Species, label = round(Mean_Growth, 2)),
            vjust = -0.3, hjust = 0.5, size = 2.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                               "1/6g" = "#808080", "0g" = "#C0C0C0"),
                    name = "Gravity") +
  scale_x_continuous(breaks = time_points, labels = paste0(time_points, "h")) +
  labs(
    x = "Time (h)",
    y = "Bacterial Species"
  ) +
  facet_wrap(~ Gravity, nrow = 2) +
  theme_ridges() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank()
  )

# 重力感受性パターンの比較プロット
p3 <- data %>%
  filter(Sample == "UAC", Time == 72) %>%  # 最終時点での比較
  ggplot(aes(x = Gravity, y = Species)) +
  geom_density_ridges(aes(height = after_stat(density), fill = after_stat(x)), 
                      stat = "density_ridges", alpha = 0.8, scale = 1.5,
                      rel_min_height = 0.01, color = "black", size = 0.3) +
  # 平均値を数値で表示
  geom_text(data = mean_values %>% filter(Sample == "UAC", Time == 72),
            aes(x = Gravity, y = Species, label = round(Mean_Growth, 2)),
            vjust = -0.3, hjust = 0.5, size = 3, color = "black", fontface = "bold") +
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
                       midpoint = 0, name = "Growth\nRatio") +
  labs(
    x = "Gravity Environment",
    y = "Bacterial Species",
    title = "Gravity Response at 72h"
  ) +
  theme_ridges() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank()
  )

# 統計サマリー：重力環境別の増減殖パターン
print("重力環境別細菌増減殖パターン（UAC サンプル）:")
gravity_summary <- data %>%
  filter(Sample == "UAC") %>%
  group_by(Species, Gravity, Time) %>%
  summarise(
    Mean_Growth = round(mean(Growth_Ratio), 3),
    SD_Growth = round(sd(Growth_Ratio), 3),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Time, values_from = c(Mean_Growth, SD_Growth),
              names_sep = "_h_") %>%
  arrange(Species, Gravity)

print(gravity_summary)

# 重力感受性ランキング（72h時点）
print("重力感受性ランキング（72h、UAC）:")
sensitivity_ranking <- data %>%
  filter(Sample == "UAC", Time == 72) %>%
  group_by(Species, Gravity) %>%
  summarise(Mean_Growth = mean(Growth_Ratio), .groups = "drop") %>%
  group_by(Species) %>%
  summarise(
    Gravity_Sensitivity = Mean_Growth[Gravity == "0g"] - Mean_Growth[Gravity == "1g"],
    .groups = "drop"
  ) %>%
  arrange(desc(abs(Gravity_Sensitivity)))

print(sensitivity_ranking)

# プロットの表示
print(p1)
print(p2) 
print(p3)














# 必要なライブラリを読み込み
library(vegan)      # 生態学的解析
library(plotly)     # 3Dプロット
library(dplyr)      # データ操作
library(tidyr)      # データ整形
library(ggplot2)    # 基本プロット

# 試験的細菌叢データの生成
set.seed(123)

# 実験条件の設定
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)

# 代表的な細菌種（OTU/ASV）
bacteria_otus <- paste0("OTU_", sprintf("%03d", 1:20))

# 各条件のメタデータ作成
metadata <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points
) %>%
  mutate(
    Sample_ID = paste(Sample, Gravity, paste0(Time, "h"), sep = "_"),
    Condition = paste(Sample, Gravity, sep = "_")
  )

# 細菌叢組成データ（相対存在量）を生成
generate_microbiome_data <- function(sample, gravity, time) {
  # 基本的な細菌叢組成パターン
  base_composition <- c(
    # Firmicutes系（乳酸菌など）
    runif(5, 0.05, 0.25),
    # Bacteroidetes系
    runif(5, 0.03, 0.20),
    # Proteobacteria系  
    runif(5, 0.01, 0.15),
    # その他
    runif(5, 0.001, 0.10)
  )
  
  # サンプル特異的効果
  sample_effect <- switch(sample,
                          "UAC" = c(rep(1.2, 5), rep(0.8, 5), rep(1.0, 5), rep(0.9, 5)),
                          "MAC" = c(rep(0.9, 5), rep(1.3, 5), rep(0.8, 5), rep(1.1, 5)),
                          "AC" = c(rep(1.1, 5), rep(0.9, 5), rep(1.2, 5), rep(0.8, 5))
  )
  
  # 重力効果
  gravity_factor <- switch(gravity,
                           "1g" = 1.0,
                           "1/3g" = 0.9,
                           "1/6g" = 0.8,
                           "0g" = 0.7
  )
  
  gravity_effect <- c(
    rep(gravity_factor, 5),           # Firmicutes：重力減少で減少
    rep(2 - gravity_factor, 5),       # Bacteroidetes：重力減少で増加
    rep(1.5 - 0.5*gravity_factor, 5), # Proteobacteria：中程度の変化
    rep(1.0, 5)                       # その他：変化なし
  )
  
  # 時間効果（0-72hの変化）
  time_factor <- time / 72
  time_effect <- c(
    rep(1 - 0.3*time_factor, 5),      # Firmicutes：時間で減少
    rep(1 + 0.4*time_factor, 5),      # Bacteroidetes：時間で増加
    rep(1 + 0.2*sin(time_factor*pi), 5), # Proteobacteria：振動パターン
    rep(1 - 0.1*time_factor, 5)       # その他：軽微な減少
  )
  
  # 最終組成計算
  composition <- base_composition * sample_effect * gravity_effect * time_effect
  
  # ノイズ追加
  composition <- composition * rlnorm(20, 0, 0.3)
  
  # 相対存在量に正規化
  composition <- composition / sum(composition)
  
  return(composition)
}

# 全条件での細菌叢データ作成
microbiome_matrix <- matrix(0, nrow = nrow(metadata), ncol = length(bacteria_otus))
rownames(microbiome_matrix) <- metadata$Sample_ID
colnames(microbiome_matrix) <- bacteria_otus

for (i in 1:nrow(metadata)) {
  microbiome_matrix[i, ] <- generate_microbiome_data(
    metadata$Sample[i], 
    metadata$Gravity[i], 
    metadata$Time[i]
  )
}

# Bray-Curtis距離行列の計算
bray_curtis_dist <- vegdist(microbiome_matrix, method = "bray")

# NMDS（非計量多次元尺度法）による次元削減
nmds_result <- metaMDS(bray_curtis_dist, k = 2, trymax = 100)

# NMDS座標をメタデータに追加
coords_2d <- data.frame(
  NMDS1 = nmds_result$points[, 1],
  NMDS2 = nmds_result$points[, 2],
  Sample_ID = rownames(nmds_result$points)
) %>%
  left_join(metadata, by = "Sample_ID")

# 時間軸を3次元目として追加（正規化）
coords_3d <- coords_2d %>%
  mutate(
    Time_norm = (Time - min(Time)) / (max(Time) - min(Time)), # 0-1に正規化
    Time_3d = Time_norm * 2 - 1  # -1 to 1の範囲に変換
  )

# 重力環境と時間による色の設定
coords_3d$Gravity <- factor(coords_3d$Gravity, levels = c("1g", "1/3g", "1/6g", "0g"))

# スムーズな軌跡のための補間関数
create_smooth_trajectory <- function(data, n_points = 20) {
  if (nrow(data) < 2) return(data)
  
  # 時間に基づく補間
  time_seq <- seq(min(data$Time), max(data$Time), length.out = n_points)
  
  # スプライン補間
  nmds1_smooth <- spline(data$Time, data$NMDS1, xout = time_seq)$y
  nmds2_smooth <- spline(data$Time, data$NMDS2, xout = time_seq)$y
  time_3d_smooth <- spline(data$Time, data$Time_3d, xout = time_seq)$y
  
  return(data.frame(
    NMDS1 = nmds1_smooth,
    NMDS2 = nmds2_smooth,
    Time_3d = time_3d_smooth,
    Time = time_seq,
    Sample = data$Sample[1],
    Gravity = data$Gravity[1],
    Condition = data$Condition[1]
  ))
}

# 変化量の計算
calculate_changes <- function(data) {
  changes <- data %>%
    group_by(Sample, Gravity) %>%
    arrange(Time) %>%
    mutate(
      # 前時点からの距離変化
      NMDS1_lag = lag(NMDS1, default = first(NMDS1)),
      NMDS2_lag = lag(NMDS2, default = first(NMDS2)),
      Time_3d_lag = lag(Time_3d, default = first(Time_3d)),
      
      # ステップごとの変化量
      Step_Change = sqrt((NMDS1 - NMDS1_lag)^2 + 
                           (NMDS2 - NMDS2_lag)^2 + 
                           (Time_3d - Time_3d_lag)^2),
      
      # 0h基準からの累積変化量
      Cumulative_Change = sqrt((NMDS1 - first(NMDS1))^2 + 
                                 (NMDS2 - first(NMDS2))^2 + 
                                 (Time_3d - first(Time_3d))^2)
    ) %>%
    ungroup()
  
  return(changes)
}

# 変化量データの計算
coords_with_changes <- calculate_changes(coords_3d)

# スムーズな軌跡データの生成
smooth_trajectories <- coords_3d %>%
  group_by(Sample, Gravity) %>%
  do(create_smooth_trajectory(.)) %>%
  ungroup()

# スムーズな軌跡付き3Dプロット作成関数
create_smooth_3d_plot <- function(data, smooth_data) {
  # 基本的な3Dプロット
  p <- plot_ly(data, x = ~NMDS1, y = ~NMDS2, z = ~Time_3d,
               color = ~Gravity, symbol = ~Sample,
               colors = c("1g" = "#000000", "1/3g" = "#404040", 
                          "1/6g" = "#808080", "0g" = "#C0C0C0"),
               symbols = c("UAC" = "circle", "MAC" = "square", "AC" = "diamond"),
               size = I(8),
               text = ~paste("Sample:", Sample, "<br>",
                             "Gravity:", Gravity, "<br>",
                             "Time:", Time, "h"),
               hovertemplate = "%{text}<extra></extra>") %>%
    add_markers() %>%
    layout(
      scene = list(
        xaxis = list(title = "NMDS1"),
        yaxis = list(title = "NMDS2"),
        zaxis = list(title = "Time (normalized)"),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
      ),
      title = "Smooth Bacterial Community Trajectories in 3D",
      showlegend = TRUE
    )
  
  # スムーズな軌跡線を追加
  for (sample in samples) {
    for (grav in gravity) {
      trajectory_data <- smooth_data %>%
        filter(Sample == sample, Gravity == grav) %>%
        arrange(Time)
      
      if (nrow(trajectory_data) > 1) {
        line_color <- case_when(
          grav == "1g" ~ "#000000",
          grav == "1/3g" ~ "#404040",
          grav == "1/6g" ~ "#808080",
          grav == "0g" ~ "#C0C0C0"
        )
        
        p <- p %>%
          add_trace(
            data = trajectory_data,
            x = ~NMDS1, y = ~NMDS2, z = ~Time_3d,
            type = "scatter3d",
            mode = "lines",
            line = list(color = line_color, width = 4, smoothing = 1.3),
            name = paste(sample, grav, "trajectory"),
            showlegend = FALSE,
            hoverinfo = "skip"
          )
      }
    }
  }
  
  return(p)
}

# サンプル別の軌跡比較（スムーズ版）
create_sample_3d_plot <- function(data, smooth_data) {
  p <- plot_ly(data, x = ~NMDS1, y = ~NMDS2, z = ~Time_3d,
               color = ~Sample, 
               colors = c("UAC" = "#000000", "MAC" = "#606060", "AC" = "#A0A0A0"),
               size = I(6),
               text = ~paste("Sample:", Sample, "<br>",
                             "Gravity:", Gravity, "<br>",
                             "Time:", Time, "h")) %>%
    add_markers() %>%
    layout(
      scene = list(
        xaxis = list(title = "NMDS1"),
        yaxis = list(title = "NMDS2"),
        zaxis = list(title = "Time (normalized)")
      ),
      title = "Sample-wise Smooth Bacterial Community Trajectories"
    )
  
  # サンプル別のスムーズ軌跡線を追加
  for (sample in samples) {
    for (grav in gravity) {
      trajectory_data <- smooth_data %>%
        filter(Sample == sample, Gravity == grav) %>%
        arrange(Time)
      
      if (nrow(trajectory_data) > 1) {
        line_color <- case_when(
          sample == "UAC" ~ "#000000",
          sample == "MAC" ~ "#606060",
          sample == "AC" ~ "#A0A0A0"
        )
        
        p <- p %>%
          add_trace(
            data = trajectory_data,
            x = ~NMDS1, y = ~NMDS2, z = ~Time_3d,
            type = "scatter3d",
            mode = "lines",
            line = list(color = line_color, width = 3, smoothing = 1.3),
            name = paste(sample, grav, "smooth"),
            showlegend = FALSE,
            hoverinfo = "skip"
          )
      }
    }
  }
  
  return(p)
}

# 各種プロットの生成
plot_3d_smooth <- create_smooth_3d_plot(coords_3d, smooth_trajectories)
plot_3d_samples_smooth <- create_sample_3d_plot(coords_3d, smooth_trajectories)

# 変化量の箱ひげ図（ステップごとの変化）
boxplot_step_changes <- coords_with_changes %>%
  filter(Time > 0) %>%  # 0h除外（変化量0のため）
  ggplot(aes(x = Gravity, y = Step_Change, fill = Gravity)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                               "1/6g" = "#808080", "0g" = "#C0C0C0"),
                    guide = "none") +
  facet_wrap(~ Sample, nrow = 1) +
  labs(
    x = "Gravity Environment",
    y = "Step-wise Change Magnitude",
    title = "Bacterial Community Step-wise Changes"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 変化量の箱ひげ図（累積変化）
boxplot_cumulative_changes <- coords_with_changes %>%
  filter(Time > 0) %>%
  ggplot(aes(x = factor(Time), y = Cumulative_Change, fill = Gravity)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1.5,
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                               "1/6g" = "#808080", "0g" = "#C0C0C0"),
                    name = "Gravity") +
  facet_wrap(~ Sample, nrow = 1) +
  labs(
    x = "Time (h)",
    y = "Cumulative Change from 0h",
    title = "Cumulative Bacterial Community Changes"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 時間別変化率の箱ひげ図
boxplot_time_changes <- coords_with_changes %>%
  filter(Time > 0) %>%
  mutate(Change_Rate = Step_Change / (Time - lag(Time, default = 0))) %>%
  filter(!is.infinite(Change_Rate), !is.na(Change_Rate)) %>%
  ggplot(aes(x = Gravity, y = Change_Rate)) +
  geom_boxplot(aes(fill = Gravity), alpha = 0.7, outlier.shape = 16) +
  geom_point(aes(color = Sample), position = position_jitter(width = 0.2), 
             size = 2, alpha = 0.8) +
  scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                               "1/6g" = "#808080", "0g" = "#C0C0C0"),
                    guide = "none") +
  scale_color_manual(values = c("UAC" = "#000000", "MAC" = "#606060", "AC" = "#A0A0A0"),
                     name = "Sample") +
  facet_wrap(~ paste("Time:", Time, "h"), nrow = 1) +
  labs(
    x = "Gravity Environment",
    y = "Change Rate (per hour)",
    title = "Bacterial Community Change Rates by Time Point"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 2D投影図も作成（比較用）
plot_2d <- ggplot(coords_3d, aes(x = NMDS1, y = NMDS2)) +
  geom_path(aes(group = Condition), color = "grey70", alpha = 0.7) +
  geom_point(aes(fill = Gravity, shape = Sample, size = Time), 
             stroke = 0.5, color = "black") +
  scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                               "1/6g" = "#808080", "0g" = "#C0C0C0")) +
  scale_shape_manual(values = c("UAC" = 21, "MAC" = 22, "AC" = 23)) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    title = "Bacterial Community Changes (2D Projection)"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# 統計情報の表示
cat("NMDS解析結果:\n")
cat("Stress値:", round(nmds_result$stress, 4), "\n")
cat("収束:", nmds_result$converged, "\n\n")

# Bray-Curtis距離の統計サマリー
cat("Bray-Curtis距離の統計サマリー:\n")
print(summary(bray_curtis_dist))

# 時間変化の軌跡長と変化パターンの統計
trajectory_stats <- coords_with_changes %>%
  group_by(Sample, Gravity) %>%
  summarise(
    Total_Trajectory_Length = sum(Step_Change, na.rm = TRUE),
    Max_Single_Change = max(Step_Change, na.rm = TRUE),
    Final_Distance_from_Start = last(Cumulative_Change),
    Average_Change_Rate = mean(Step_Change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Trajectory_Length))

cat("\n各条件での軌跡統計:\n")
print(trajectory_stats)

# 変化量の統計サマリー
change_summary <- coords_with_changes %>%
  filter(Time > 0) %>%
  group_by(Gravity) %>%
  summarise(
    Mean_Step_Change = round(mean(Step_Change, na.rm = TRUE), 4),
    SD_Step_Change = round(sd(Step_Change, na.rm = TRUE), 4),
    Mean_Cumulative_Change = round(mean(Cumulative_Change, na.rm = TRUE), 4),
    SD_Cumulative_Change = round(sd(Cumulative_Change, na.rm = TRUE), 4),
    .groups = "drop"
  )

cat("\n重力環境別変化量サマリー:\n")
print(change_summary)

# プロット表示
cat("\n=== 3D可視化プロット ===\n")
print("スムーズな軌跡3Dプロット:")
print(plot_3d_smooth)

print("サンプル別軌跡:")
print(plot_3d_samples_smooth)

cat("\n=== 変化量の箱ひげ図 ===\n")
print("ステップごとの変化量:")
print(boxplot_step_changes)

print("累積変化量:")
print(boxplot_cumulative_changes)

print("時間別変化率:")
print(boxplot_time_changes)

cat("\n=== 2D投影図 ===\n")
print(plot_2d)





















# 必要なライブラリを読み込み
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(igraph)
library(pheatmap)
library(RColorBrewer)
library(plotly)

# ダイナミックトピックモデルのシミュレーション
set.seed(456)

# 実験条件の設定
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)

# 細菌種（分類群）の定義
bacteria_taxa <- c("Lactobacillus", "Bifidobacterium", "Bacteroides", 
                   "Clostridium", "Escherichia", "Enterococcus",
                   "Streptococcus", "Akkermansia", "Prevotella", 
                   "Roseburia", "Faecalibacterium", "Ruminococcus")

n_taxa <- length(bacteria_taxa)
n_topics <- length(gravity)  # 重力環境 = トピック

# トピック（重力環境）特異的な細菌組成パターンを定義
define_topic_composition <- function(gravity_condition, time_point) {
  # 基本組成
  base_composition <- rep(1/n_taxa, n_taxa)
  
  # 重力特異的パターン
  gravity_pattern <- switch(gravity_condition,
                            "1g" = c(0.25, 0.20, 0.15, 0.12, 0.08, 0.08, 0.04, 0.03, 0.02, 0.01, 0.01, 0.01),
                            "1/3g" = c(0.15, 0.25, 0.18, 0.10, 0.06, 0.10, 0.06, 0.04, 0.03, 0.01, 0.01, 0.01),
                            "1/6g" = c(0.10, 0.30, 0.20, 0.08, 0.05, 0.12, 0.05, 0.04, 0.03, 0.01, 0.01, 0.01),
                            "0g" = c(0.05, 0.35, 0.25, 0.06, 0.04, 0.15, 0.03, 0.03, 0.02, 0.01, 0.01, 0.01)
  )
  
  # 時間依存的な変化
  time_factor <- time_point / 72
  time_modification <- switch(gravity_condition,
                              "1g" = c(-0.05*time_factor, 0.02*time_factor, 0.01*time_factor, 
                                       0.01*time_factor, 0.005*time_factor, 0.005*time_factor,
                                       rep(0.002*time_factor, 6)),
                              "1/3g" = c(-0.03*time_factor, 0.04*time_factor, 0.02*time_factor,
                                         -0.01*time_factor, -0.005*time_factor, 0.01*time_factor,
                                         rep(0.001*time_factor, 6)),
                              "1/6g" = c(-0.02*time_factor, 0.06*time_factor, 0.03*time_factor,
                                         -0.02*time_factor, -0.01*time_factor, 0.02*time_factor,
                                         rep(0.001*time_factor, 6)),
                              "0g" = c(-0.01*time_factor, 0.08*time_factor, 0.04*time_factor,
                                       -0.03*time_factor, -0.015*time_factor, 0.03*time_factor,
                                       rep(0.001*time_factor, 6))
  )
  
  # 最終組成計算
  final_composition <- gravity_pattern + time_modification
  final_composition <- pmax(final_composition, 0.001)  # 最小値制限
  final_composition <- final_composition / sum(final_composition)  # 正規化
  
  return(final_composition)
}

# 各条件でのトピック分布生成
generate_topic_distribution <- function(sample_type, gravity_condition, time_point) {
  # サンプル特異的な重力感受性
  sensitivity <- switch(sample_type,
                        "UAC" = c("1g"=0.9, "1/3g"=0.95, "1/6g"=1.0, "0g"=1.05),
                        "MAC" = c("1g"=1.0, "1/3g"=1.0, "1/6g"=0.95, "0g"=0.9),
                        "AC" = c("1g"=1.1, "1/3g"=1.05, "1/6g"=1.0, "0g"=0.95)
  )
  
  # 基本トピック分布（主に対応する重力環境が優勢）
  base_distribution <- rep(0.1, n_topics)
  main_topic_idx <- which(gravity == gravity_condition)
  base_distribution[main_topic_idx] <- 0.7
  
  # 時間による混合比率の変化
  time_factor <- time_point / 72
  other_topics_boost <- 0.2 * time_factor * sensitivity[gravity_condition]
  base_distribution[main_topic_idx] <- base_distribution[main_topic_idx] - other_topics_boost
  base_distribution[-main_topic_idx] <- base_distribution[-main_topic_idx] + 
    other_topics_boost / (n_topics - 1)
  
  # 正規化
  base_distribution <- pmax(base_distribution, 0.01)
  base_distribution <- base_distribution / sum(base_distribution)
  
  return(base_distribution)
}

# DTMデータの生成
dtm_data <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    # 各条件でのトピック分布
    Topic_Distribution = list(generate_topic_distribution(Sample, Gravity, Time)),
    # 各トピックでの細菌組成
    Topic_Compositions = list(sapply(gravity, function(g) 
      define_topic_composition(g, Time), simplify = FALSE))
  ) %>%
  ungroup()

# トピック-分類群マトリックスの作成（時間平均）
create_topic_taxa_matrix <- function(time_point) {
  matrix_data <- matrix(0, nrow = n_topics, ncol = n_taxa)
  rownames(matrix_data) <- paste0("Topic_", gravity)
  colnames(matrix_data) <- bacteria_taxa
  
  for (i in 1:n_topics) {
    composition <- define_topic_composition(gravity[i], time_point)
    matrix_data[i, ] <- composition
  }
  
  return(matrix_data)
}

# 各時間点でのトピック-分類群マトリックス
topic_taxa_matrices <- lapply(time_points, create_topic_taxa_matrix)
names(topic_taxa_matrices) <- paste0("Time_", time_points, "h")

# 時系列データの整理（折れ線プロット用）
time_series_data <- data.frame()
for (t in time_points) {
  for (g in gravity) {
    composition <- define_topic_composition(g, t)
    temp_data <- data.frame(
      Time = t,
      Topic = paste0("Topic_", g),
      Gravity = g,
      Taxa = bacteria_taxa,
      Proportion = composition,
      stringsAsFactors = FALSE
    )
    time_series_data <- rbind(time_series_data, temp_data)
  }
}

# 主要分類群のみ抽出（上位6種）
major_taxa <- time_series_data %>%
  group_by(Taxa) %>%
  summarise(Mean_Proportion = mean(Proportion), .groups = "drop") %>%
  arrange(desc(Mean_Proportion)) %>%
  slice(1:6) %>%
  pull(Taxa)

time_series_major <- time_series_data %>%
  filter(Taxa %in% major_taxa)

# 1. 時系列折れ線プロット
plot_time_series <- ggplot(time_series_major, 
                           aes(x = Time, y = Proportion, color = Taxa)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~ paste("Topic", Gravity), nrow = 2) +
  scale_color_manual(values = c("#000000", "#333333", "#666666", 
                                "#999999", "#BBBBBB", "#DDDDDD")) +
  scale_x_continuous(breaks = time_points, labels = paste0(time_points, "h")) +
  labs(
    x = "Time (h)",
    y = "Taxa Proportion in Topic",
    title = "Dynamic Topic Model: Taxa Composition Changes",
    subtitle = "Temporal evolution of bacterial taxa within gravity-specific topics"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 2. 環境特異的トピックの評価
calculate_topic_specificity <- function() {
  specificity_data <- data.frame()
  
  for (taxa in bacteria_taxa) {
    for (t in time_points) {
      proportions <- sapply(gravity, function(g) {
        composition <- define_topic_composition(g, t)
        idx <- which(bacteria_taxa == taxa)
        return(composition[idx])
      })
      
      # Gini係数による特異性計算
      gini_coef <- sum(abs(outer(proportions, proportions, "-"))) / 
        (2 * length(proportions) * sum(proportions))
      
      max_topic <- gravity[which.max(proportions)]
      
      specificity_data <- rbind(specificity_data, data.frame(
        Taxa = taxa,
        Time = t,
        Gini_Coefficient = gini_coef,
        Max_Topic = max_topic,
        Max_Proportion = max(proportions),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(specificity_data)
}

specificity_data <- calculate_topic_specificity()

# 環境特異性の可視化
plot_specificity <- ggplot(specificity_data, 
                           aes(x = Time, y = Gini_Coefficient, color = Max_Topic)) +
  geom_line(aes(group = Taxa), alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Taxa, nrow = 3) +
  scale_color_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                "1/6g" = "#808080", "0g" = "#C0C0C0"),
                     name = "Dominant Topic") +
  scale_x_continuous(breaks = time_points, labels = paste0(time_points, "h")) +
  labs(
    x = "Time (h)",
    y = "Topic Specificity (Gini Coefficient)",
    title = "Environmental Specificity of Bacterial Taxa",
    subtitle = "Higher values indicate stronger gravity-specific preferences"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 3. ヒートマップ（時間平均）
average_topic_matrix <- Reduce("+", topic_taxa_matrices) / length(topic_taxa_matrices)

# ヒートマップ作成
create_heatmap <- function(matrix_data, title_suffix = "") {
  pheatmap(matrix_data,
           color = colorRampPalette(c("white", "grey50", "black"))(50),
           cluster_rows = TRUE,
           cluster_cols = TRUE,
           display_numbers = TRUE,
           number_format = "%.3f",
           fontsize = 10,
           fontsize_number = 8,
           main = paste("Topic-Taxa Association", title_suffix),
           cellwidth = 25,
           cellheight = 20)
}

# 4. ネットワーク図
create_network_plot <- function(matrix_data, threshold = 0.1) {
  # エッジリストの作成
  edges <- data.frame()
  for (i in 1:nrow(matrix_data)) {
    for (j in 1:ncol(matrix_data)) {
      if (matrix_data[i, j] > threshold) {
        edges <- rbind(edges, data.frame(
          from = rownames(matrix_data)[i],
          to = colnames(matrix_data)[j],
          weight = matrix_data[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # グラフオブジェクトの作成
  g <- graph_from_data_frame(edges, directed = FALSE)
  
  # ノードの属性設定
  V(g)$type <- ifelse(V(g)$name %in% rownames(matrix_data), "Topic", "Taxa")
  V(g)$color <- ifelse(V(g)$type == "Topic", "#666666", "#CCCCCC")
  V(g)$size <- ifelse(V(g)$type == "Topic", 20, 15)
  
  # エッジの太さ
  E(g)$width <- E(g)$weight * 10
  
  # レイアウト
  layout <- layout_with_fr(g)
  
  # プロット
  plot(g, 
       layout = layout,
       vertex.label.cex = 0.8,
       vertex.label.color = "black",
       edge.color = "grey50",
       main = "Topic-Taxa Network")
  
  return(g)
}

# 5. トピック分布の時間変化
topic_distribution_data <- dtm_data %>%
  rowwise() %>%
  mutate(
    Topic_1g = Topic_Distribution[[1]][1],
    Topic_1_3g = Topic_Distribution[[1]][2],
    Topic_1_6g = Topic_Distribution[[1]][3],
    Topic_0g = Topic_Distribution[[1]][4]
  ) %>%
  select(-Topic_Distribution, -Topic_Compositions) %>%
  pivot_longer(cols = starts_with("Topic_"), 
               names_to = "Topic", values_to = "Proportion") %>%
  mutate(Topic = gsub("Topic_", "", Topic),
         Topic = gsub("_", "/", Topic))

plot_topic_distribution <- ggplot(topic_distribution_data, 
                                  aes(x = Time, y = Proportion, color = Topic)) +
  geom_line(size = 1.0, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  facet_grid(Sample ~ Gravity) +
  scale_color_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                "1/6g" = "#808080", "0g" = "#C0C0C0")) +
  scale_x_continuous(breaks = c(0, 24, 48, 72), labels = c("0h", "24h", "48h", "72h")) +
  labs(
    x = "Time",
    y = "Topic Proportion",
    title = "Dynamic Topic Distribution Changes",
    subtitle = "Evolution of gravity-specific topic mixtures in each sample"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 統計サマリー
cat("=== Dynamic Topic Model Analysis Results ===\n\n")

cat("1. Topic-specific bacterial composition (Time-averaged):\n")
print(round(average_topic_matrix, 3))

cat("\n2. Most environmentally specific taxa:\n")
top_specific <- specificity_data %>%
  group_by(Taxa) %>%
  summarise(Mean_Specificity = mean(Gini_Coefficient), .groups = "drop") %>%
  arrange(desc(Mean_Specificity)) %>%
  slice(1:5)
print(top_specific)

cat("\n3. Temporal trends in topic specificity:\n")
temporal_trends <- specificity_data %>%
  group_by(Time) %>%
  summarise(
    Mean_Specificity = mean(Gini_Coefficient),
    SD_Specificity = sd(Gini_Coefficient),
    .groups = "drop"
  )
print(temporal_trends)

# プロット表示
cat("\n=== Visualizations ===\n")
print("1. Time series of taxa proportions in topics:")
print(plot_time_series)

print("2. Environmental specificity analysis:")
print(plot_specificity)

print("3. Topic-Taxa heatmap:")
create_heatmap(average_topic_matrix, "(Time-averaged)")

print("4. Topic-Taxa network:")
network_graph <- create_network_plot(average_topic_matrix, threshold = 0.08)

print("5. Topic distribution evolution:")
print(plot_topic_distribution)
