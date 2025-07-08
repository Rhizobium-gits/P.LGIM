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














# 必要なライブラリを読み込み
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(viridis)
library(scales)
library(knitr)

# 試験的データの生成
set.seed(456)  # 再現性のため

# 実験条件の設定
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)

# 代表的な細菌種
bacteria_species <- c("Lactobacillus", "Bifidobacterium", "Bacteroides", 
                      "Clostridium", "Escherichia", "Enterococcus",
                      "Streptococcus", "Akkermansia", "Prevotella", 
                      "Roseburia", "Faecalibacterium", "Others")

# 細菌叢組成データの生成
generate_composition <- function(sample_type, gravity_env, time_point) {
  # 基本的な組成パターン（相対存在量）
  base_composition <- switch(sample_type,
                             "UAC" = c(0.25, 0.20, 0.15, 0.10, 0.08, 0.06, 0.04, 0.03, 0.03, 0.02, 0.02, 0.02),
                             "MAC" = c(0.20, 0.25, 0.18, 0.08, 0.06, 0.08, 0.05, 0.03, 0.02, 0.02, 0.02, 0.01),
                             "AC" = c(0.22, 0.22, 0.16, 0.12, 0.07, 0.07, 0.04, 0.03, 0.02, 0.02, 0.02, 0.01)
  )
  
  # 重力環境による影響
  gravity_factor <- switch(gravity_env,
                           "1g" = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                           "1/3g" = c(0.9, 1.1, 1.05, 0.95, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                           "1/6g" = c(0.8, 1.3, 1.1, 0.9, 0.8, 1.1, 1.0, 1.05, 1.0, 1.0, 1.0, 1.0),
                           "0g" = c(0.6, 1.5, 1.2, 0.8, 0.7, 1.3, 0.9, 1.1, 1.0, 1.0, 1.0, 1.0)
  )
  
  # 時間による変化
  time_factor <- time_point / 72
  time_effect <- c(
    1 - 0.2*time_factor,    # Lactobacillus：時間で減少
    1 + 0.3*time_factor,    # Bifidobacterium：時間で増加
    1 + 0.1*time_factor,    # Bacteroides：軽微増加
    1 - 0.15*time_factor,   # Clostridium：軽微減少
    1 - 0.1*time_factor,    # Escherichia：軽微減少
    1 + 0.2*time_factor,    # Enterococcus：軽微増加
    1 - 0.05*time_factor,   # その他は軽微な変化
    1 + 0.1*time_factor,
    1 + 0.05*time_factor,
    1 + 0.05*time_factor,
    1 + 0.05*time_factor,
    1.0
  )
  
  # 最終組成計算
  final_composition <- base_composition * gravity_factor * time_effect
  
  # ランダムノイズ追加
  final_composition <- final_composition * rlnorm(length(bacteria_species), 0, 0.1)
  
  # 相対存在量に正規化（合計が1になるように）
  final_composition <- final_composition / sum(final_composition)
  
  return(final_composition)
}

# データフレームの作成
composition_data <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    Composition = list(generate_composition(Sample, Gravity, Time))
  ) %>%
  ungroup()

# 長形式データに変換
long_data <- composition_data %>%
  mutate(
    Species = list(bacteria_species)
  ) %>%
  unnest(c(Composition, Species)) %>%
  rename(Relative_Abundance = Composition) %>%
  mutate(
    Relative_Abundance_Percent = Relative_Abundance * 100
  )

# 重力環境の順序を設定
long_data$Gravity <- factor(long_data$Gravity, levels = c("1g", "1/3g", "1/6g", "0g"))
long_data$Species <- factor(long_data$Species, levels = bacteria_species)

# 配色の設定
colors_bacteria <- c(
  brewer.pal(9, "Set1"),
  brewer.pal(3, "Set2")
)
names(colors_bacteria) <- bacteria_species

# 1. 積み上げ棒グラフ（サンプル別ファセット）
plot_stacked_bar_sample <- ggplot(long_data, aes(x = factor(Time), y = Relative_Abundance_Percent, 
                                                 fill = Species)) +
  geom_bar(stat = "identity", position = "stack", color = "white", size = 0.2) +
  facet_grid(Sample ~ Gravity) +
  scale_fill_manual(values = colors_bacteria, name = "Bacterial Species") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x = "Time (h)",
    y = "Relative Abundance (%)",
    title = "Bacterial Composition Changes Over Time",
    subtitle = "Stacked bar chart showing relative abundance of each species"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 2. 主要菌種のみの時系列プロット
major_species <- c("Lactobacillus", "Bifidobacterium", "Bacteroides", 
                   "Clostridium", "Escherichia", "Enterococcus")

plot_major_species_trends <- long_data %>%
  filter(Species %in% major_species) %>%
  ggplot(aes(x = Time, y = Relative_Abundance_Percent, color = Species)) +
  geom_line(size = 1.0, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  facet_grid(Sample ~ Gravity) +
  scale_color_manual(values = colors_bacteria[major_species], name = "Major Species") +
  scale_x_continuous(breaks = time_points, labels = paste0(time_points, "h")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x = "Time (h)",
    y = "Relative Abundance (%)",
    title = "Major Bacterial Species Trends",
    subtitle = "Time-series changes of dominant species across conditions"
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

# 3. ヒートマップ（時間平均での種構成）
heatmap_data <- long_data %>%
  group_by(Sample, Gravity, Species) %>%
  summarise(Mean_Abundance = mean(Relative_Abundance_Percent), .groups = "drop") %>%
  filter(Species != "Others")  # Othersを除外

plot_heatmap <- ggplot(heatmap_data, aes(x = Gravity, y = Species, fill = Mean_Abundance)) +
  geom_tile(color = "white", size = 0.5) +
  facet_wrap(~ Sample, nrow = 1) +
  scale_fill_viridis_c(name = "Mean\nAbundance\n(%)", option = "plasma") +
  labs(
    x = "Gravity Environment",
    y = "Bacterial Species",
    title = "Bacterial Composition Heatmap",
    subtitle = "Time-averaged relative abundance across conditions"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    panel.grid = element_blank()
  )

# 4. 重力環境効果の比較（72h時点）
plot_gravity_effect <- long_data %>%
  filter(Time == 72, Species %in% major_species) %>%
  ggplot(aes(x = Gravity, y = Relative_Abundance_Percent, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~ Sample, nrow = 1) +
  scale_fill_manual(values = colors_bacteria[major_species], name = "Species") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x = "Gravity Environment",
    y = "Relative Abundance (%)",
    title = "Gravity Effects on Bacterial Composition (72h)",
    subtitle = "Final composition of major species across gravity conditions"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 5. 円グラフ（代表的な条件での比較）
pie_data <- long_data %>%
  filter(Sample == "UAC", Time == 72) %>%
  select(Gravity, Species, Relative_Abundance_Percent)

create_pie_chart <- function(gravity_condition) {
  data_subset <- pie_data %>%
    filter(Gravity == gravity_condition) %>%
    arrange(desc(Relative_Abundance_Percent))
  
  ggplot(data_subset, aes(x = "", y = Relative_Abundance_Percent, fill = Species)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors_bacteria, name = "Species") +
    labs(
      title = paste("UAC Sample at 72h -", gravity_condition),
      subtitle = "Bacterial composition distribution"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 12, color = "black", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, color = "black", hjust = 0.5),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "black")
    )
}

# 各重力環境の円グラフ
pie_1g <- create_pie_chart("1g")
pie_0g <- create_pie_chart("0g")

# 統計サマリー
cat("=== 細菌叢組成解析結果 ===\n\n")

# 主要菌種の統計サマリー
major_stats <- long_data %>%
  filter(Species %in% major_species) %>%
  group_by(Species, Gravity) %>%
  summarise(
    Mean_Abundance = round(mean(Relative_Abundance_Percent), 2),
    SD_Abundance = round(sd(Relative_Abundance_Percent), 2),
    Min_Abundance = round(min(Relative_Abundance_Percent), 2),
    Max_Abundance = round(max(Relative_Abundance_Percent), 2),
    .groups = "drop"
  )

cat("主要菌種の重力環境別統計:\n")
print(kable(major_stats,
            format = "simple",
            col.names = c("Species", "Gravity", "Mean (%)", "SD (%)", "Min (%)", "Max (%)"),
            align = c("l", "l", "r", "r", "r", "r"),
            caption = "Major Species Statistics by Gravity Environment"))

# 時間変化パターンの統計
temporal_stats <- long_data %>%
  filter(Species %in% major_species) %>%
  group_by(Species, Time) %>%
  summarise(
    Mean_Abundance = round(mean(Relative_Abundance_Percent), 2),
    SD_Abundance = round(sd(Relative_Abundance_Percent), 2),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Time, values_from = c(Mean_Abundance, SD_Abundance),
              names_sep = "_h_")

cat("\n主要菌種の時間変化パターン:\n")
print(kable(temporal_stats,
            format = "simple",
            align = "l"))

# 多様性指標
diversity_stats <- long_data %>%
  group_by(Sample, Gravity, Time) %>%
  summarise(
    Simpson_Index = 1 - sum((Relative_Abundance/100)^2),
    Shannon_Index = -sum((Relative_Abundance/100) * log(Relative_Abundance/100 + 1e-10)),
    Richness = sum(Relative_Abundance > 0.1),  # 0.1%以上の種数
    .groups = "drop"
  )

cat("\n多様性指標の統計:\n")
diversity_summary <- diversity_stats %>%
  group_by(Gravity) %>%
  summarise(
    Mean_Simpson = round(mean(Simpson_Index), 3),
    Mean_Shannon = round(mean(Shannon_Index), 3),
    Mean_Richness = round(mean(Richness), 1),
    .groups = "drop"
  )

print(kable(diversity_summary,
            format = "simple",
            col.names = c("Gravity", "Simpson Index", "Shannon Index", "Richness"),
            align = c("l", "r", "r", "r"),
            caption = "Diversity Indices by Gravity Environment"))

# プロット表示
cat("\n=== 可視化結果 ===\n")
print("1. 積み上げ棒グラフ（全菌種構成）:")
print(plot_stacked_bar_sample)

print("2. 主要菌種の時系列変化:")
print(plot_major_species_trends)

print("3. ヒートマップ（時間平均構成）:")
print(plot_heatmap)

print("4. 重力環境効果（72h時点）:")
print(plot_gravity_effect)

print("5. 円グラフ（1g環境、UAC、72h）:")
print(pie_1g)

print("6. 円グラフ（0g環境、UAC、72h）:")
print(pie_0g)
































# 必要なライブラリを読み込み
library(ggplot2)
library(dplyr)
library(tidyr)
library(networkD3)
library(ggalluvial)
library(plotly)
library(RColorBrewer)

# 試験的データの生成
set.seed(789)

# 実験条件の設定
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)

# 主要な細菌種（Sankey用に数を絞る）
bacteria_species <- c("Lactobacillus", "Bifidobacterium", "Bacteroides", 
                      "Clostridium", "Escherichia", "Enterococcus", "Others")

# 細菌叢組成データの生成
generate_microbiome_composition <- function(sample_type, gravity_env, time_point) {
  base_composition <- switch(sample_type,
                             "UAC" = c(0.30, 0.20, 0.18, 0.12, 0.08, 0.07, 0.05),
                             "MAC" = c(0.25, 0.25, 0.20, 0.10, 0.08, 0.07, 0.05),
                             "AC" = c(0.28, 0.22, 0.19, 0.11, 0.08, 0.07, 0.05)
  )
  
  # 重力環境による影響
  gravity_effect <- switch(gravity_env,
                           "1g" = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                           "1/3g" = c(0.9, 1.1, 1.05, 0.95, 0.9, 1.0, 1.0),
                           "1/6g" = c(0.7, 1.4, 1.1, 0.8, 0.7, 1.1, 1.0),
                           "0g" = c(0.5, 1.8, 1.2, 0.6, 0.5, 1.3, 1.0)
  )
  
  # 時間による変化
  time_factor <- time_point / 72
  time_effect <- c(
    1 - 0.3*time_factor,    # Lactobacillus：減少
    1 + 0.5*time_factor,    # Bifidobacterium：増加
    1 + 0.1*time_factor,    # Bacteroides：軽微増加
    1 - 0.2*time_factor,    # Clostridium：減少
    1 - 0.15*time_factor,   # Escherichia：減少
    1 + 0.2*time_factor,    # Enterococcus：増加
    1.0                     # Others：変化なし
  )
  
  # 最終組成計算
  final_composition <- base_composition * gravity_effect * time_effect
  final_composition <- final_composition * rlnorm(length(bacteria_species), 0, 0.08)
  final_composition <- final_composition / sum(final_composition)
  
  return(final_composition)
}

# データフレーム作成
microbiome_data <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    Composition = list(generate_microbiome_composition(Sample, Gravity, Time))
  ) %>%
  unnest_wider(Composition, names_sep = "_") %>%
  rename_with(~bacteria_species, starts_with("Composition_")) %>%
  pivot_longer(cols = all_of(bacteria_species), 
               names_to = "Species", values_to = "Abundance")

# 1. networkD3を使ったインタラクティブSankey
create_sankey_data <- function(sample_name, gravity_name) {
  # 指定条件のデータを抽出
  subset_data <- microbiome_data %>%
    filter(Sample == sample_name, Gravity == gravity_name) %>%
    mutate(
      Abundance_percent = Abundance * 100,
      Time_Species = paste0("T", Time, "_", Species)
    )
  
  # ノードの作成
  nodes <- data.frame(
    name = unique(subset_data$Time_Species),
    stringsAsFactors = FALSE
  )
  
  # リンクの作成（時間間のフロー）
  links <- data.frame()
  
  for (species in bacteria_species) {
    species_data <- subset_data %>%
      filter(Species == species) %>%
      arrange(Time)
    
    for (i in 1:(nrow(species_data)-1)) {
      source_node <- paste0("T", species_data$Time[i], "_", species)
      target_node <- paste0("T", species_data$Time[i+1], "_", species)
      
      # フローの値（2時点の平均を使用）
      flow_value <- mean(c(species_data$Abundance_percent[i], 
                           species_data$Abundance_percent[i+1]))
      
      links <- rbind(links, data.frame(
        source = which(nodes$name == source_node) - 1,  # 0-indexed
        target = which(nodes$name == target_node) - 1,
        value = flow_value,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(list(nodes = nodes, links = links))
}

# UAC, 0g条件でのSankey作成
sankey_data_uac_0g <- create_sankey_data("UAC", "0g")

sankey_plot <- sankeyNetwork(
  Links = sankey_data_uac_0g$links,
  Nodes = sankey_data_uac_0g$nodes,
  Source = "source",
  Target = "target", 
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 30,
  height = 600,
  width = 1000
)

# 2. ggalluvialを使った静的Sankey風プロット
create_alluvial_plot <- function(sample_name, gravity_name) {
  plot_data <- microbiome_data %>%
    filter(Sample == sample_name, Gravity == gravity_name) %>%
    mutate(
      Abundance_percent = Abundance * 100,
      Time_label = paste0(Time, "h")
    ) %>%
    # 小さな値をまとめる
    group_by(Time_label) %>%
    mutate(
      Species_grouped = ifelse(Abundance_percent < 3, "Minor species", Species)
    ) %>%
    ungroup() %>%
    group_by(Time_label, Species_grouped) %>%
    summarise(Total_Abundance = sum(Abundance_percent), .groups = "drop")
  
  # 色の設定
  species_colors <- c(
    brewer.pal(length(bacteria_species)-1, "Set2"),
    "#CCCCCC"  # Minor species用
  )
  names(species_colors) <- c(bacteria_species[bacteria_species != "Others"], "Minor species")
  
  ggplot(plot_data, aes(x = Time_label, stratum = Species_grouped, 
                        alluvium = Species_grouped, y = Total_Abundance,
                        fill = Species_grouped)) +
    geom_flow(stat = "alluvium", lode.guidance = "frontback", 
              color = "white", alpha = 0.8) +
    geom_stratum(alpha = 0.9, color = "white", size = 0.5) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
              size = 3, color = "black", fontface = "bold") +
    scale_fill_manual(values = species_colors, name = "Bacterial Species") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      x = "Time Points",
      y = "Relative Abundance (%)",
      title = paste("Bacterial Community Flow:", sample_name, "-", gravity_name),
      subtitle = "Alluvial diagram showing temporal changes in bacterial composition"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 10, color = "black"),
      plot.title = element_text(size = 14, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 11, color = "black"),
      legend.title = element_text(size = 11, color = "black"),
      legend.text = element_text(size = 10, color = "black"),
      panel.grid = element_blank()
    )
}

# 各条件でのAlluvial plot作成
alluvial_uac_1g <- create_alluvial_plot("UAC", "1g")
alluvial_uac_0g <- create_alluvial_plot("UAC", "0g")
alluvial_mac_0g <- create_alluvial_plot("MAC", "0g")

# 3. 重力環境間のフロー（72h時点）
create_gravity_flow <- function(time_point) {
  flow_data <- microbiome_data %>%
    filter(Time == time_point) %>%
    mutate(
      Abundance_percent = Abundance * 100,
      Gravity_Species = paste(Gravity, Species, sep = "_")
    ) %>%
    group_by(Sample, Gravity, Species) %>%
    summarise(Mean_Abundance = mean(Abundance_percent), .groups = "drop") %>%
    filter(Mean_Abundance > 2)  # 2%以上の種のみ
  
  # 色の設定
  colors_gravity <- c("1g" = "#000000", "1/3g" = "#404040", 
                      "1/6g" = "#808080", "0g" = "#C0C0C0")
  
  ggplot(flow_data, aes(x = Gravity, stratum = Species, alluvium = Species, 
                        y = Mean_Abundance, fill = Species)) +
    geom_flow(stat = "alluvium", lode.guidance = "frontback", 
              color = "white", alpha = 0.7) +
    geom_stratum(alpha = 0.8, color = "white", size = 0.5) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
              size = 2.5, color = "black", angle = 90) +
    scale_fill_brewer(type = "qual", palette = "Set3", name = "Species") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    facet_wrap(~ Sample, nrow = 1) +
    labs(
      x = "Gravity Environment",
      y = "Relative Abundance (%)",
      title = paste("Bacterial Composition Across Gravity Environments (", time_point, "h)"),
      subtitle = "Flow diagram showing species distribution changes with gravity"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 10, color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 11, color = "black"),
      legend.title = element_text(size = 11, color = "black"),
      legend.text = element_text(size = 9, color = "black"),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(size = 11, color = "black", face = "bold"),
      panel.grid = element_blank()
    )
}

gravity_flow_72h <- create_gravity_flow(72)

# 4. 菌種優勢度の変遷Sankey
create_dominance_sankey <- function(sample_name) {
  # 各時点での上位3菌種を特定
  dominance_data <- microbiome_data %>%
    filter(Sample == sample_name) %>%
    group_by(Gravity, Time) %>%
    arrange(desc(Abundance)) %>%
    slice(1:3) %>%
    mutate(Rank = row_number()) %>%
    ungroup() %>%
    mutate(
      Abundance_percent = Abundance * 100,
      Rank_label = paste0("Rank_", Rank),
      Time_Rank = paste0("T", Time, "_", Rank_label)
    )
  
  ggplot(dominance_data, aes(x = factor(Time), stratum = Rank_label, 
                             alluvium = paste(Gravity, Rank_label), 
                             y = Abundance_percent, fill = Species)) +
    geom_flow(stat = "alluvium", lode.guidance = "frontback", 
              color = "white", alpha = 0.8) +
    geom_stratum(alpha = 0.9, color = "white", size = 0.5) +
    geom_text(stat = "stratum", aes(label = paste("Top", 1:3)), 
              size = 3, color = "black") +
    scale_fill_brewer(type = "qual", palette = "Set1", name = "Dominant Species") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    facet_wrap(~ Gravity, nrow = 2) +
    labs(
      x = "Time (h)",
      y = "Relative Abundance (%)",
      title = paste("Dominant Species Succession:", sample_name),
      subtitle = "Flow of top 3 bacterial species over time"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 10, color = "black"),
      plot.title = element_text(size = 14, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 11, color = "black"),
      legend.title = element_text(size = 11, color = "black"),
      legend.text = element_text(size = 10, color = "black"),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(size = 10, color = "black", face = "bold"),
      panel.grid = element_blank()
    )
}

dominance_uac <- create_dominance_sankey("UAC")

# 統計サマリー
cat("=== Sankey Diagram分析結果 ===\n\n")

# フローの変化量統計
flow_stats <- microbiome_data %>%
  group_by(Sample, Gravity, Species) %>%
  arrange(Time) %>%
  summarise(
    Initial_Abundance = first(Abundance) * 100,
    Final_Abundance = last(Abundance) * 100,
    Net_Change = Final_Abundance - Initial_Abundance,
    Max_Abundance = max(Abundance) * 100,
    .groups = "drop"
  ) %>%
  filter(abs(Net_Change) > 2)  # 2%以上の変化のみ

cat("主要な組成変化（2%以上の変化）:\n")
print(flow_stats %>%
        arrange(desc(abs(Net_Change))) %>%
        head(20))

# 最大フロー変化
max_flows <- flow_stats %>%
  group_by(Gravity) %>%
  slice_max(abs(Net_Change), n = 3) %>%
  ungroup()

cat("\n重力環境別の最大変化菌種:\n")
print(max_flows)

# プロット表示
cat("\n=== Sankey可視化結果 ===\n")
print("1. インタラクティブSankey（UAC, 0g）:")
print(sankey_plot)

print("2. Alluvial Plot（UAC, 1g）:")
print(alluvial_uac_1g)

print("3. Alluvial Plot（UAC, 0g）:")
print(alluvial_uac_0g)

print("4. Alluvial Plot（MAC, 0g）:")
print(alluvial_mac_0g)

print("5. 重力環境間フロー（72h）:")
print(gravity_flow_72h)

print("6. 優勢種遷移（UAC）:")
print(dominance_uac)




























# 必要なライブラリを読み込み
library(vegan)      # 生態学的解析
library(plotly)     # 3Dプロット
library(dplyr)      # データ操作
library(tidyr)      # データ整形
library(ggplot2)    # 基本プロット

# 試験的細菌叢データの生成
set.seed(123)

# 実験条件の設定（複製数を追加）
samples <- c("UAC", "MAC", "AC")
gravity <- c("1g", "1/3g", "1/6g", "0g")
time_points <- c(0, 24, 48, 72)
n_replicates <- 3

# 代表的な細菌種（OTU/ASV）
bacteria_otus <- paste0("OTU_", sprintf("%03d", 1:20))

# 各条件のメタデータ作成（複製を含む）
metadata <- expand.grid(
  Sample = samples,
  Gravity = gravity,
  Time = time_points,
  Replicate = 1:n_replicates
) %>%
  mutate(
    Sample_ID = paste(Sample, Gravity, paste0(Time, "h"), paste0("R", Replicate), sep = "_"),
    Condition = paste(Sample, Gravity, sep = "_")
  )

# 細菌叢組成データ（相対存在量）を生成
generate_microbiome_data <- function(sample, gravity, time, replicate) {
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
  
  # 複製間のばらつき
  replicate_effect <- rlnorm(20, 0, 0.2 + 0.1*replicate/n_replicates)
  
  # 最終組成計算
  composition <- base_composition * sample_effect * gravity_effect * time_effect * replicate_effect
  
  # 追加ノイズ
  composition <- composition * rlnorm(20, 0, 0.15)
  
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
    metadata$Time[i],
    metadata$Replicate[i]
  )
}

# 全データに対してBray-Curtis距離行列の計算
bray_curtis_dist <- vegdist(microbiome_matrix, method = "bray")

# 全データに対して一度だけNMDS（非計量多次元尺度法）を実行
nmds_result <- metaMDS(bray_curtis_dist, k = 2, trymax = 200)

# NMDS座標をメタデータに追加
coords_3d <- data.frame(
  NMDS1 = nmds_result$points[, 1],
  NMDS2 = nmds_result$points[, 2],
  Sample_ID = rownames(nmds_result$points)
) %>%
  left_join(metadata, by = "Sample_ID") %>%
  mutate(
    # 時間軸（Z軸）は実際の時間値を使用
    Time_Z = Time
  )

# 重力環境とサンプルの順序を設定
coords_3d$Gravity <- factor(coords_3d$Gravity, levels = c("1g", "1/3g", "1/6g", "0g"))
coords_3d$Sample <- factor(coords_3d$Sample, levels = c("UAC", "MAC", "AC"))

# スムーズな軌跡のための補間関数（複製の平均値で）
create_smooth_trajectory <- function(data, n_points = 15) {
  if (nrow(data) < 2) return(data)
  
  # 複製の平均を計算
  avg_data <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      Time_Z_mean = mean(Time_Z),
      .groups = "drop"
    ) %>%
    arrange(Time)
  
  if (nrow(avg_data) < 2) return(avg_data)
  
  # 時間に基づく補間
  time_seq <- seq(min(avg_data$Time), max(avg_data$Time), length.out = n_points)
  
  # スプライン補間
  nmds1_smooth <- spline(avg_data$Time, avg_data$NMDS1_mean, xout = time_seq)$y
  nmds2_smooth <- spline(avg_data$Time, avg_data$NMDS2_mean, xout = time_seq)$y
  time_z_smooth <- time_seq
  
  return(data.frame(
    NMDS1 = nmds1_smooth,
    NMDS2 = nmds2_smooth,
    Time_Z = time_z_smooth,
    Time = time_seq,
    Sample = data$Sample[1],
    Gravity = data$Gravity[1]
  ))
}

# 変化量の計算（複製平均で）
calculate_trajectory_metrics <- function(data) {
  # 複製の平均を計算
  avg_data <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      Time_Z_mean = mean(Time_Z),
      .groups = "drop"
    ) %>%
    arrange(Time)
  
  # 変化量計算
  changes <- avg_data %>%
    group_by(Sample, Gravity) %>%
    arrange(Time) %>%
    mutate(
      # 前時点からの距離変化（2D空間での）
      NMDS1_lag = lag(NMDS1_mean, default = first(NMDS1_mean)),
      NMDS2_lag = lag(NMDS2_mean, default = first(NMDS2_mean)),
      
      # ステップごとの変化量（2D距離）
      Step_Change_2D = sqrt((NMDS1_mean - NMDS1_lag)^2 + (NMDS2_mean - NMDS2_lag)^2),
      
      # 0h基準からの累積変化量
      Cumulative_Change_2D = sqrt((NMDS1_mean - first(NMDS1_mean))^2 + 
                                    (NMDS2_mean - first(NMDS2_mean))^2),
      
      # 変化速度（単位時間あたり）
      Change_Rate = Step_Change_2D / (Time - lag(Time, default = 0))
    ) %>%
    ungroup()
  
  return(changes)
}

# 軌跡統計の計算
trajectory_metrics <- calculate_trajectory_metrics(coords_3d)

# スムーズな軌跡データの生成
smooth_trajectories <- coords_3d %>%
  group_by(Sample, Gravity) %>%
  do(create_smooth_trajectory(.)) %>%
  ungroup()

# 3Dプロット作成関数（スムーズな軌跡付き）
create_3d_microbiome_plot <- function(data, smooth_data, title_text) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      Time_Z_mean = mean(Time_Z),
      .groups = "drop"
    )
  
  # 基本3Dプロット（個別の複製点）
  p <- plot_ly(data, x = ~NMDS1, y = ~NMDS2, z = ~Time_Z,
               color = ~Gravity, symbol = ~Sample,
               colors = c("1g" = "#000000", "1/3g" = "#404040", 
                          "1/6g" = "#808080", "0g" = "#C0C0C0"),
               symbols = c("UAC" = "circle", "MAC" = "square", "AC" = "diamond"),
               size = I(4), alpha = 0.6,
               text = ~paste("Sample:", Sample, "<br>",
                             "Gravity:", Gravity, "<br>",
                             "Time:", Time, "h<br>",
                             "Replicate:", Replicate),
               hovertemplate = "%{text}<extra></extra>") %>%
    add_markers() %>%
    
    # 平均位置を大きなポイントで表示
    add_trace(data = avg_positions, 
              x = ~NMDS1_mean, y = ~NMDS2_mean, z = ~Time_Z_mean,
              color = ~Gravity, symbol = ~Sample,
              colors = c("1g" = "#000000", "1/3g" = "#404040", 
                         "1/6g" = "#808080", "0g" = "#C0C0C0"),
              symbols = c("UAC" = "circle", "MAC" = "square", "AC" = "diamond"),
              size = I(10), alpha = 1.0,
              text = ~paste("MEAN - Sample:", Sample, "<br>",
                            "Gravity:", Gravity, "<br>",
                            "Time:", Time, "h"),
              name = "Mean positions",
              showlegend = FALSE) %>%
    
    layout(
      scene = list(
        xaxis = list(title = "NMDS1"),
        yaxis = list(title = "NMDS2"),
        zaxis = list(title = "Time (h)"),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
      ),
      title = title_text,
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
            x = ~NMDS1, y = ~NMDS2, z = ~Time_Z,
            type = "scatter3d",
            mode = "lines",
            line = list(color = line_color, width = 5, smoothing = 1.0),
            name = paste(sample, grav, "trajectory"),
            showlegend = FALSE,
            hoverinfo = "skip"
          )
      }
    }
  }
  
  return(p)
}

# メイン3Dプロット
plot_3d_main <- create_3d_microbiome_plot(
  coords_3d, smooth_trajectories, 
  "Bacterial Community Dynamics: 3D Trajectory Analysis"
)

# 重力環境別比較（特定サンプルにフォーカス）
plot_3d_gravity_focus <- coords_3d %>%
  filter(Sample == "UAC") %>%
  create_3d_microbiome_plot(
    smooth_trajectories %>% filter(Sample == "UAC"),
    "Gravity Environment Effects on UAC Sample"
  )

# 変化量の箱ひげ図
boxplot_trajectory_changes <- trajectory_metrics %>%
  filter(Time > 0) %>%
  ggplot(aes(x = Gravity, y = Step_Change_2D, fill = Gravity)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(aes(color = Sample), width = 0.2, alpha = 0.8, size = 2.5) +
  scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                               "1/6g" = "#808080", "0g" = "#C0C0C0"),
                    guide = "none") +
  scale_color_manual(values = c("UAC" = "#FF0000", "MAC" = "#00AA00", "AC" = "#0000FF"),
                     name = "Sample") +
  facet_wrap(~ paste("Time:", Time, "h"), nrow = 1) +
  labs(
    x = "Gravity Environment",
    y = "Step-wise Change (Bray-Curtis distance)",
    title = "Bacterial Community Change Magnitude",
    subtitle = "2D NMDS distance between consecutive time points"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 累積変化の時系列プロット
plot_cumulative_changes <- trajectory_metrics %>%
  filter(Time > 0) %>%
  ggplot(aes(x = Time, y = Cumulative_Change_2D, color = Gravity)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~ Sample, nrow = 1) +
  scale_color_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                "1/6g" = "#808080", "0g" = "#C0C0C0"),
                     name = "Gravity") +
  scale_x_continuous(breaks = time_points, labels = paste0(time_points, "h")) +
  labs(
    x = "Time",
    y = "Cumulative Change from 0h",
    title = "Bacterial Community Divergence Over Time",
    subtitle = "NMDS distance from initial community state"
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

# 2D軌跡プロット（時間列変化を平面で表示）
create_2d_trajectory_plot <- function(data, show_arrows = TRUE) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time)
  
  # 基本2Dプロット
  p <- ggplot(avg_positions, aes(x = NMDS1_mean, y = NMDS2_mean)) +
    # 軌跡線（時間順）
    geom_path(aes(group = paste(Sample, Gravity), color = Gravity),
              size = 1.5, alpha = 0.8, arrow = if(show_arrows) arrow(length = unit(0.2, "cm")) else NULL) +
    # 時間点
    geom_point(aes(fill = Gravity, shape = Sample, size = Time), 
               stroke = 1, color = "black", alpha = 0.9) +
    # 開始点を強調
    geom_point(data = avg_positions %>% filter(Time == 0),
               aes(fill = Gravity, shape = Sample), 
               size = 8, stroke = 2, color = "red", alpha = 1.0) +
    # 終了点を強調
    geom_point(data = avg_positions %>% filter(Time == 72),
               aes(fill = Gravity, shape = Sample), 
               size = 8, stroke = 2, color = "blue", alpha = 1.0) +
    scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                 "1/6g" = "#808080", "0g" = "#C0C0C0")) +
    scale_color_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                  "1/6g" = "#808080", "0g" = "#C0C0C0")) +
    scale_shape_manual(values = c("UAC" = 21, "MAC" = 22, "AC" = 23)) +
    scale_size_continuous(range = c(3, 7), breaks = time_points,
                          labels = paste0(time_points, "h"), name = "Time") +
    labs(
      x = "NMDS1",
      y = "NMDS2",
      title = "Bacterial Community Trajectory Analysis (2D)",
      subtitle = paste0("Red: Start (0h), Blue: End (72h), Arrows: Time direction, Stress = ", round(nmds_result$stress, 3))
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(size = 12, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "black"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# 2D軌跡プロット（サンプル別ファセット）
create_2d_trajectory_faceted <- function(data) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time)
  
  p <- ggplot(avg_positions, aes(x = NMDS1_mean, y = NMDS2_mean)) +
    # 軌跡線
    geom_path(aes(group = Gravity, color = Gravity),
              size = 1.2, alpha = 0.8, 
              arrow = arrow(length = unit(0.15, "cm"))) +
    # 時間点
    geom_point(aes(fill = Gravity, size = Time), 
               shape = 21, stroke = 0.8, color = "black", alpha = 0.9) +
    # 時間ラベル
    geom_text(aes(label = Time), size = 2.5, color = "white", fontface = "bold") +
    facet_wrap(~ Sample, nrow = 1) +
    scale_fill_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                 "1/6g" = "#808080", "0g" = "#C0C0C0")) +
    scale_color_manual(values = c("1g" = "#000000", "1/3g" = "#404040", 
                                  "1/6g" = "#808080", "0g" = "#C0C0C0")) +
    scale_size_continuous(range = c(4, 8), breaks = time_points,
                          labels = paste0(time_points, "h"), name = "Time (h)") +
    labs(
      x = "NMDS1", 
      y = "NMDS2",
      title = "Sample-specific Bacterial Community Trajectories",
      subtitle = "Temporal evolution of microbiome structure under different gravity conditions"
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
  
  return(p)
}

# 重力環境別軌跡比較プロット
create_gravity_trajectory_plot <- function(data) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time)
  
  p <- ggplot(avg_positions, aes(x = NMDS1_mean, y = NMDS2_mean)) +
    # 軌跡線
    geom_path(aes(group = Sample, color = Sample, linetype = Sample),
              size = 1.0, alpha = 0.8) +
    # 時間点
    geom_point(aes(fill = Sample, size = Time), 
               shape = 21, stroke = 0.8, color = "black", alpha = 0.9) +
    # 開始点と終了点の強調
    geom_point(data = avg_positions %>% filter(Time == 0),
               aes(color = Sample), size = 6, shape = 1, stroke = 3) +
    geom_point(data = avg_positions %>% filter(Time == 72),
               aes(color = Sample), size = 6, shape = 0, stroke = 3) +
    facet_wrap(~ paste("Gravity:", Gravity), nrow = 2) +
    scale_fill_manual(values = c("UAC" = "#000000", "MAC" = "#606060", "AC" = "#A0A0A0")) +
    scale_color_manual(values = c("UAC" = "#000000", "MAC" = "#606060", "AC" = "#A0A0A0")) +
    scale_linetype_manual(values = c("UAC" = "solid", "MAC" = "dashed", "AC" = "dotted")) +
    scale_size_continuous(range = c(3, 6), breaks = time_points,
                          labels = paste0(time_points, "h"), name = "Time (h)") +
    labs(
      x = "NMDS1",
      y = "NMDS2", 
      title = "Gravity-specific Bacterial Community Trajectories",
      subtitle = "Circle: Start (0h), Square: End (72h)"
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
  
  return(p)
}

# 軌跡の統計的解析
trajectory_statistics <- function(data) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time)
  
  # 軌跡の特徴量計算
  trajectory_features <- avg_positions %>%
    group_by(Sample, Gravity) %>%
    arrange(Time) %>%
    summarise(
      # 総軌跡長
      Total_Length = sum(sqrt(diff(NMDS1_mean)^2 + diff(NMDS2_mean)^2)),
      
      # 直線距離（開始→終了）
      Straight_Distance = sqrt((last(NMDS1_mean) - first(NMDS1_mean))^2 + 
                                 (last(NMDS2_mean) - first(NMDS2_mean))^2),
      
      # 軌跡の曲がり具合（直線性）
      Trajectory_Straightness = Straight_Distance / Total_Length,
      
      # 最大変位
      Max_Displacement = max(sqrt((NMDS1_mean - first(NMDS1_mean))^2 + 
                                    (NMDS2_mean - first(NMDS2_mean))^2)),
      
      # 軌跡の重心
      Centroid_X = mean(NMDS1_mean),
      Centroid_Y = mean(NMDS2_mean),
      
      .groups = "drop"
    )
  
  return(trajectory_features)
}

# 色・濃度による時間・サンプル表現の2D軌跡プロット
create_color_coded_trajectory_plot <- function(data) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time) %>%
    mutate(
      # 時間を0-1に正規化（色の濃さ用）
      Time_normalized = (Time - min(Time)) / (max(Time) - min(Time))
    )
  
  # サンプル別の基本色設定
  sample_colors <- c("UAC" = "#E31A1C", "MAC" = "#1F78B4", "AC" = "#33A02C")
  
  p <- ggplot(avg_positions, aes(x = NMDS1_mean, y = NMDS2_mean)) +
    # 軌跡線（重力環境ごと、サンプル色）
    geom_path(aes(group = paste(Sample, Gravity), color = Sample, alpha = Time_normalized),
              size = 2.0, arrow = arrow(length = unit(0.2, "cm"))) +
    # 時間点（色の濃さで時間、色相でサンプル）
    geom_point(aes(fill = Sample, alpha = Time_normalized, shape = Gravity), 
               size = 6, stroke = 1, color = "black") +
    # 開始点（0h）を特別に強調
    geom_point(data = avg_positions %>% filter(Time == 0),
               aes(fill = Sample, shape = Gravity), 
               size = 8, stroke = 3, color = "white", alpha = 1.0) +
    scale_fill_manual(values = sample_colors, name = "Sample") +
    scale_color_manual(values = sample_colors, name = "Sample") +
    scale_shape_manual(values = c("1g" = 21, "1/3g" = 22, "1/6g" = 23, "0g" = 24),
                       name = "Gravity") +
    scale_alpha_identity() +  # 透明度を直接使用
    labs(
      x = "NMDS1",
      y = "NMDS2",
      title = "Color-coded Temporal Bacterial Community Trajectories",
      subtitle = "Color: Sample type, Opacity: Time progression (darker = later), Shape: Gravity, White border: Start (0h)"
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(size = 12, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "black"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# 時間グラデーション特化プロット
create_time_gradient_plot <- function(data) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time)
  
  p <- ggplot(avg_positions, aes(x = NMDS1_mean, y = NMDS2_mean)) +
    # 軌跡線
    geom_path(aes(group = paste(Sample, Gravity), color = Sample),
              size = 1.5, alpha = 0.7, arrow = arrow(length = unit(0.15, "cm"))) +
    # 時間グラデーション点
    geom_point(aes(fill = Time, shape = Sample, size = Gravity), 
               stroke = 1, color = "black", alpha = 0.9) +
    scale_fill_gradient(low = "lightblue", high = "darkred", 
                        name = "Time (h)", 
                        breaks = time_points,
                        labels = paste0(time_points, "h")) +
    scale_color_manual(values = c("UAC" = "#000000", "MAC" = "#404040", "AC" = "#808080"),
                       name = "Sample") +
    scale_shape_manual(values = c("UAC" = 21, "MAC" = 22, "AC" = 23),
                       name = "Sample") +
    scale_size_manual(values = c("1g" = 4, "1/3g" = 5, "1/6g" = 6, "0g" = 7),
                      name = "Gravity") +
    labs(
      x = "NMDS1",
      y = "NMDS2", 
      title = "Time-gradient Bacterial Community Evolution",
      subtitle = "Color intensity: Time progression, Size: Gravity level, Shape: Sample type"
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(size = 12, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "black"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# サンプル色相×時間濃度の組み合わせプロット
create_hue_intensity_plot <- function(data) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time) %>%
    mutate(
      # サンプル×時間の複合色作成
      Sample_Time = paste(Sample, Time, sep = "_"),
      Time_factor = Time / max(Time)  # 0-1正規化
    )
  
  # サンプル×時間の色パレット作成
  create_sample_time_colors <- function() {
    colors <- c()
    base_colors <- c("UAC" = "#E31A1C", "MAC" = "#1F78B4", "AC" = "#33A02C")
    
    for (sample in names(base_colors)) {
      for (time in time_points) {
        time_factor <- time / max(time_points)
        # 時間が進むほど濃く
        color_name <- paste(sample, time, sep = "_")
        # HSVで明度を調整
        base_rgb <- col2rgb(base_colors[sample])
        # 時間係数で明度調整（0.3から1.0）
        intensity <- 0.3 + 0.7 * time_factor
        adjusted_rgb <- base_rgb * intensity
        colors[color_name] <- rgb(adjusted_rgb[1], adjusted_rgb[2], adjusted_rgb[3], 
                                  maxColorValue = 255)
      }
    }
    return(colors)
  }
  
  sample_time_colors <- create_sample_time_colors()
  
  p <- ggplot(avg_positions, aes(x = NMDS1_mean, y = NMDS2_mean)) +
    # 軌跡線（重力環境ごと）
    geom_path(aes(group = paste(Sample, Gravity), color = Sample),
              size = 1.2, alpha = 0.6, 
              arrow = arrow(length = unit(0.15, "cm"))) +
    # 色相×濃度点
    geom_point(aes(fill = Sample_Time, shape = Gravity), 
               size = 5, stroke = 1, color = "black", alpha = 0.9) +
    # 時間ラベル
    geom_text(aes(label = Time), size = 2, color = "white", fontface = "bold") +
    scale_fill_manual(values = sample_time_colors, 
                      name = "Sample × Time",
                      guide = "none") +  # 凡例は複雑すぎるので非表示
    scale_color_manual(values = c("UAC" = "#E31A1C", "MAC" = "#1F78B4", "AC" = "#33A02C"),
                       name = "Sample") +
    scale_shape_manual(values = c("1g" = 21, "1/3g" = 22, "1/6g" = 23, "0g" = 24),
                       name = "Gravity") +
    labs(
      x = "NMDS1",
      y = "NMDS2",
      title = "Sample Hue × Time Intensity Trajectory Plot", 
      subtitle = "Color hue: Sample type, Color intensity: Time progression (darker = later)"
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(size = 12, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "black"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ヒートマップスタイルの時間変化プロット
create_heatmap_trajectory_plot <- function(data) {
  # 複製の平均位置を計算
  avg_positions <- data %>%
    group_by(Sample, Gravity, Time) %>%
    summarise(
      NMDS1_mean = mean(NMDS1),
      NMDS2_mean = mean(NMDS2),
      .groups = "drop"
    ) %>%
    arrange(Sample, Gravity, Time)
  
  p <- ggplot(avg_positions, aes(x = NMDS1_mean, y = NMDS2_mean)) +
    # 軌跡線
    geom_path(aes(group = paste(Sample, Gravity)),
              color = "grey30", size = 0.8, alpha = 0.6,
              arrow = arrow(length = unit(0.1, "cm"))) +
    # ヒートマップスタイル点
    geom_point(aes(fill = Time, color = Sample, shape = Gravity), 
               size = 6, stroke = 2, alpha = 0.9) +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
                         midpoint = median(time_points),
                         name = "Time (h)",
                         breaks = time_points,
                         labels = paste0(time_points, "h")) +
    scale_color_manual(values = c("UAC" = "#000000", "MAC" = "#FFFFFF", "AC" = "#808080"),
                       name = "Sample") +
    scale_shape_manual(values = c("1g" = 21, "1/3g" = 22, "1/6g" = 23, "0g" = 24),
                       name = "Gravity") +
    labs(
      x = "NMDS1",
      y = "NMDS2",
      title = "Heatmap-style Temporal Trajectories",
      subtitle = "Fill: Time heatmap (blue→yellow→red), Border: Sample type"
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      plot.title = element_text(size = 12, color = "black", face = "bold"),
      plot.subtitle = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 9, color = "black"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# 各種色彩コード軌跡プロットの生成
plot_color_coded <- create_color_coded_trajectory_plot(coords_3d)
plot_time_gradient <- create_time_gradient_plot(coords_3d)
plot_hue_intensity <- create_hue_intensity_plot(coords_3d)
plot_heatmap_style <- create_heatmap_trajectory_plot(coords_3d)

# 軌跡統計の計算
trajectory_stats <- trajectory_statistics(coords_3d)

# 統計サマリー
cat("=== β多様性解析結果 ===\n\n")
cat("NMDS解析結果:\n")
cat("Stress値:", round(nmds_result$stress, 4), "\n")
cat("収束:", nmds_result$converged, "\n")
cat("反復回数:", nmds_result$tries, "\n\n")

cat("Bray-Curtis距離の統計サマリー:\n")
print(summary(bray_curtis_dist))

# 軌跡長の統計
trajectory_summary <- trajectory_metrics %>%
  group_by(Sample, Gravity) %>%
  summarise(
    Total_Trajectory_Length = sum(Step_Change_2D, na.rm = TRUE),
    Max_Single_Change = max(Step_Change_2D, na.rm = TRUE),
    Final_Distance_from_Start = last(Cumulative_Change_2D),
    Average_Change_Rate = mean(Step_Change_2D, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Trajectory_Length))

cat("\n基本軌跡長ランキング（総変化量）:\n")
print(trajectory_summary)

cat("\n詳細軌跡統計解析結果:\n")
print(trajectory_stats)

cat("\n軌跡の特徴量サマリー:\n")
trajectory_summary_stats <- trajectory_stats %>%
  group_by(Gravity) %>%
  summarise(
    Mean_Total_Length = round(mean(Total_Length), 4),
    Mean_Straightness = round(mean(Trajectory_Straightness), 4),
    Mean_Max_Displacement = round(mean(Max_Displacement), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Total_Length))

print(trajectory_summary_stats)

# プロット表示
cat("\n=== 可視化結果 ===\n")
print("1. メイン3Dプロット（全サンプル・全重力条件）:")
print(plot_3d_main)

print("2. 重力環境効果フォーカス（UAC）:")
print(plot_3d_gravity_focus)

print("3. 色・濃度コード軌跡プロット:")
print(plot_color_coded)

print("4. 時間グラデーション軌跡:")
print(plot_time_gradient)

print("5. 色相×濃度組み合わせ軌跡:")
print(plot_hue_intensity)

print("6. ヒートマップスタイル軌跡:")
print(plot_heatmap_style)

print("7. 従来の2D軌跡プロット:")
print(plot_2d_trajectory_main)

print("8. サンプル別2D軌跡比較:")
print(plot_2d_trajectory_faceted)

print("9. 重力環境別軌跡比較:")
print(plot_2d_gravity_comparison)

print("10. 変化量の箱ひげ図:")
print(boxplot_trajectory_changes)

print("11. 累積変化の時系列:")
print(plot_cumulative_changes)

