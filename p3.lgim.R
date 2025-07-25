# 重力勾配における細菌増殖特異性の2次元可視化
# 2D Visualization of Bacterial Growth Specificity along Gravity Gradient

# 必要なライブラリ
library(tidyverse)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(scales)
library(ggrepel)
library(plotly)

# 前提：microbiome_data, metadata, bacteria_speciesが既に読み込まれていることを想定

# 1. 重力勾配における細菌増殖データの準備
# 重力勾配の定義
# ここでは分数を有限小数として書いている
gravity_levels <- data.frame(
  Gravity = c("1g", "1/3g", "1/6g", "0g"),
  Gravity_numeric = c(1.0, 0.33, 0.17, 0.0),
  Gravity_index = c(4, 3, 2, 1),  # 勾配インデックス
  Environment_type = c("Earth", "Mars", "Moon", "Space"),
  stringsAsFactors = FALSE
)

# 各重力環境での細菌増殖データ計算
calculate_gravity_growth_profile <- function(microbiome_data, metadata) {
  
  # 各重力×運動×時間条件での平均存在量
  growth_profiles <- microbiome_data %>%
    left_join(metadata, by = "Sample_ID") %>%
    group_by(Gravity, Exercise, Time) %>%
    summarise(across(all_of(bacteria_species), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  
  # 重力勾配データに変換
  gravity_growth <- growth_profiles %>%
    left_join(gravity_levels, by = "Gravity") %>%
    pivot_longer(cols = all_of(bacteria_species), names_to = "Bacteria", values_to = "Abundance")
  
  # 各細菌の重力勾配における最適増殖条件を計算
  bacteria_optima <- gravity_growth %>%
    group_by(Bacteria, Exercise) %>%
    summarise(
      # 最大増殖を示す重力レベル
      Optimal_gravity = Gravity_numeric[which.max(Abundance)],
      Optimal_environment = Environment_type[which.max(Abundance)],
      Max_abundance = max(Abundance),
      Min_abundance = min(Abundance),
      # 重力勾配における増殖範囲
      Growth_range = max(Abundance) - min(Abundance),
      # 重力特異性指数（変動係数の逆数）
      Gravity_specificity = ifelse(mean(Abundance) > 0, 
                                   sd(Abundance) / mean(Abundance), 
                                   0),
      # 重力勾配における増殖効率（面積下曲線）
      Growth_efficiency = sum(Abundance * Gravity_numeric) / sum(Gravity_numeric),
      .groups = "drop"
    ) %>%
    mutate(
      # 特異性レベルの分類
      Specificity_level = case_when(
        Gravity_specificity > 0.8 ~ "Very High",
        Gravity_specificity > 0.5 ~ "High", 
        Gravity_specificity > 0.3 ~ "Medium",
        TRUE ~ "Low"
      ),
      # 環境適応タイプの分類
      Adaptation_type = case_when(
        Optimal_gravity >= 0.8 ~ "Earth-adapted",
        Optimal_gravity >= 0.3 ~ "Mars-adapted", 
        Optimal_gravity >= 0.15 ~ "Moon-adapted",
        TRUE ~ "Space-adapted"
      )
    )
  
  return(list(
    growth_data = gravity_growth,
    optima_data = bacteria_optima
  ))
}

# 2. 重力勾配特異性の詳細解析
# 各細菌の重力応答曲線を計算
calculate_gravity_response_curves <- function(growth_data) {
  
  # 各細菌の重力応答を平滑化
  response_curves <- growth_data %>%
    group_by(Bacteria, Exercise) %>%
    arrange(Gravity_numeric) %>%
    mutate(
      # 重力勾配での相対増殖率
      Relative_growth = Abundance / max(Abundance),
      # 勾配での変化率
      Growth_gradient = c(0, diff(Abundance)) / c(1, diff(Gravity_numeric)),
      # 重力ストレス応答
      Gravity_stress_response = 1 - Gravity_numeric,
      Stress_tolerance = Abundance * Gravity_stress_response
    ) %>%
    ungroup()
  
  # 重力勾配における特徴的な応答パターンを分類
  response_patterns <- response_curves %>%
    group_by(Bacteria, Exercise) %>%
    summarise(
      # 増加型（低重力で増加）
      Increasing_pattern = cor(Gravity_stress_response, Abundance, use = "complete.obs"),
      # 減少型（低重力で減少）  
      Decreasing_pattern = cor(Gravity_numeric, Abundance, use = "complete.obs"),
      # U字型（中間重力で最小）
      Quadratic_coef = {
        if(n() >= 3) {
          tryCatch({
            lm_result <- lm(Abundance ~ I(Gravity_numeric^2) + Gravity_numeric)
            coef(lm_result)[2]
          }, error = function(e) 0)
        } else { 0 }
      },
      # 最大変化点
      Max_gradient_point = Gravity_numeric[which.max(abs(Growth_gradient))],
      .groups = "drop"
    ) %>%
    mutate(
      # 応答パターンの分類
      Response_pattern = case_when(
        Increasing_pattern > 0.7 ~ "Low-gravity preferring",
        Decreasing_pattern > 0.7 ~ "High-gravity preferring", 
        abs(Quadratic_coef) > 0.1 ~ "Intermediate optimum",
        TRUE ~ "Stable across gradient"
      )
    )
  
  return(list(
    curves = response_curves,
    patterns = response_patterns
  ))
}

# 3. 2次元可視化関数群

# オプション1: 重力勾配散布図（細菌特異性マップ）
plot_gravity_specificity_map <- function(bacteria_optima, response_patterns) {
  
  # データを統合
  plot_data <- bacteria_optima %>%
    left_join(response_patterns, by = c("Bacteria", "Exercise")) %>%
    mutate(
      Bacteria_clean = str_replace_all(Bacteria, "_", " "),
      # 点のサイズ（特異性の強度）
      Point_size = rescale(Gravity_specificity, to = c(2, 12)),
      # 透明度（増殖効率）
      Point_alpha = rescale(Growth_efficiency, to = c(0.4, 1.0))
    )
  
  p1 <- ggplot(plot_data, aes(x = Optimal_gravity, y = Gravity_specificity)) +
    # 重力環境の背景領域
    annotate("rect", xmin = 0.8, xmax = 1.0, ymin = -Inf, ymax = Inf, 
             fill = "#2c7fb8", alpha = 0.1) +
    annotate("rect", xmin = 0.25, xmax = 0.4, ymin = -Inf, ymax = Inf, 
             fill = "#d73027", alpha = 0.1) +
    annotate("rect", xmin = 0.1, xmax = 0.25, ymin = -Inf, ymax = Inf, 
             fill = "#fc8d59", alpha = 0.1) +  
    annotate("rect", xmin = 0.0, xmax = 0.1, ymin = -Inf, ymax = Inf, 
             fill = "#252525", alpha = 0.1) +
    # 環境ラベル
    annotate("text", x = 0.9, y = max(plot_data$Gravity_specificity) * 0.9, 
             label = "Earth", size = 4, fontface = "bold", color = "#2c7fb8") +
    annotate("text", x = 0.33, y = max(plot_data$Gravity_specificity) * 0.9, 
             label = "Mars", size = 4, fontface = "bold", color = "#d73027") +
    annotate("text", x = 0.17, y = max(plot_data$Gravity_specificity) * 0.9, 
             label = "Moon", size = 4, fontface = "bold", color = "#fc8d59") +
    annotate("text", x = 0.05, y = max(plot_data$Gravity_specificity) * 0.9, 
             label = "Space", size = 4, fontface = "bold", color = "#252525") +
    # データ点
    geom_point(aes(size = Point_size, color = Exercise, alpha = Point_alpha, 
                   shape = Response_pattern), stroke = 1) +
    scale_size_identity() +
    scale_color_manual(values = c("UAC" = "#e31a1c", "MAC" = "#1f78b4", "AC" = "#33a02c"),
                       name = "Exercise Type") +
    scale_alpha_identity() +
    scale_shape_manual(values = c("Low-gravity preferring" = 16, 
                                  "High-gravity preferring" = 17,
                                  "Intermediate optimum" = 15,
                                  "Stable across gradient" = 18),
                       name = "Response Pattern") +
    # ラベル
    geom_text_repel(aes(label = Bacteria_clean, color = Exercise), 
                    size = 2.5, max.overlaps = 15, 
                    point.padding = unit(0.3, "lines")) +
    # テーマ
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "Bacterial Growth Specificity in Gravity Gradient",
      subtitle = "2D mapping of optimal gravity conditions and environmental specificity",
      x = "Optimal Gravity Level (g)",
      y = "Gravity Specificity Index",
      caption = "Point size: Specificity strength | Transparency: Growth efficiency | Background: Gravity environments"
    )
  
  return(p1)
}

# オプション2: 重力勾配ヒートマップ
plot_gravity_gradient_heatmap <- function(growth_data) {
  
  # ヒートマップ用データ準備
  heatmap_data <- growth_data %>%
    group_by(Bacteria, Gravity_numeric) %>%
    summarise(Mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
    # 各細菌の存在量を0-1に正規化
    group_by(Bacteria) %>%
    mutate(
      Normalized_abundance = (Mean_abundance - min(Mean_abundance)) / 
        (max(Mean_abundance) - min(Mean_abundance) + 1e-10),
      Bacteria_clean = str_replace_all(Bacteria, "_", " ")
    ) %>%
    ungroup()
  
  p2 <- ggplot(heatmap_data, aes(x = Gravity_numeric, y = reorder(Bacteria_clean, Normalized_abundance), 
                                 fill = Normalized_abundance)) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_viridis_c(name = "Normalized\nAbundance", option = "plasma", 
                         labels = c("Low", "", "", "", "High")) +
    scale_x_continuous(breaks = c(0, 0.17, 0.33, 1.0), 
                       labels = c("0g", "1/6g", "1/3g", "1g"),
                       expand = c(0, 0)) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    labs(
      title = "Bacterial Growth Response Across Gravity Gradient",
      subtitle = "Heatmap showing normalized abundance at different gravity levels",
      x = "Gravity Level",
      y = "Bacterial Species",
      caption = "Each row normalized to 0-1 scale for comparative visualization"
    )
  
  return(p2)
}

# オプション3: 3D風等高線プロット
plot_gravity_contour_map <- function(growth_data, bacteria_optima) {
  
  # 主要細菌（特異性の高い上位10種）を選択
  top_bacteria <- bacteria_optima %>%
    group_by(Bacteria) %>%
    summarise(Mean_specificity = mean(Gravity_specificity), .groups = "drop") %>%
    slice_max(Mean_specificity, n = 10) %>%
    pull(Bacteria)
  
  # 等高線用データ準備
  contour_data <- growth_data %>%
    filter(Bacteria %in% top_bacteria) %>%
    group_by(Bacteria, Gravity_numeric) %>%
    summarise(
      Mean_abundance = mean(Abundance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Bacteria_clean = str_replace_all(Bacteria, "_", " "))
  
  p3 <- ggplot(contour_data, aes(x = Gravity_numeric, y = Mean_abundance)) +
    geom_smooth(aes(color = Bacteria_clean), method = "loess", se = TRUE, alpha = 0.2) +
    geom_point(aes(color = Bacteria_clean), size = 2, alpha = 0.7) +
    scale_color_viridis_d(name = "Bacterial Species", option = "turbo") +
    scale_x_continuous(breaks = c(0, 0.17, 0.33, 1.0), 
                       labels = c("0g", "1/6g", "1/3g", "1g")) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 12, face = "bold")
    ) +
    labs(
      title = "Growth Response Curves Along Gravity Gradient",
      subtitle = "Smooth curves showing abundance trends for high-specificity bacteria",
      x = "Gravity Level",
      y = "Relative Abundance",
      caption = "Curves fitted with LOESS smoothing; ribbons show confidence intervals"
    )
  
  return(p3)
}

# オプション4: インタラクティブ3Dプロット
plot_interactive_3d_map <- function(bacteria_optima) {
  
  # 3D用データ準備
  plot_3d_data <- bacteria_optima %>%
    mutate(
      Bacteria_clean = str_replace_all(Bacteria, "_", " "),
      hover_text = paste0(
        "Bacteria: ", Bacteria_clean, "<br>",
        "Exercise: ", Exercise, "<br>", 
        "Optimal Gravity: ", round(Optimal_gravity, 2), "g<br>",
        "Specificity: ", round(Gravity_specificity, 2), "<br>",
        "Growth Efficiency: ", round(Growth_efficiency, 2)
      )
    )
  
  p4 <- plot_ly(plot_3d_data, 
                x = ~Optimal_gravity, 
                y = ~Gravity_specificity, 
                z = ~Growth_efficiency,
                color = ~Exercise,
                colors = c("#e31a1c", "#1f78b4", "#33a02c"),
                size = ~Growth_range,
                sizes = c(5, 15),
                text = ~hover_text,
                hovertemplate = "%{text}<extra></extra>",
                type = "scatter3d",
                mode = "markers") %>%
    layout(
      title = list(text = "3D Bacterial Growth Specificity Map", font = list(size = 16)),
      scene = list(
        xaxis = list(title = "Optimal Gravity Level (g)"),
        yaxis = list(title = "Gravity Specificity Index"), 
        zaxis = list(title = "Growth Efficiency"),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
      ),
      legend = list(title = list(text = "Exercise Type"))
    )
  
  return(p4)
}

# 4. 統合解析の実行
cat("=== Gravity Gradient Bacterial Specificity Analysis ===\n")

# 1. 重力勾配増殖データの計算
cat("\n1. Calculating bacterial growth profiles along gravity gradient...\n")
growth_analysis <- calculate_gravity_growth_profile(microbiome_data, metadata)

# 2. 重力応答曲線の解析
cat("2. Analyzing gravity response curves...\n")
response_analysis <- calculate_gravity_response_curves(growth_analysis$growth_data)

# 3. 結果サマリー
cat("\n=== Gravity Specificity Summary ===\n")
specificity_summary <- growth_analysis$optima_data %>%
  group_by(Adaptation_type) %>%
  summarise(
    Count = n(),
    Mean_specificity = mean(Gravity_specificity),
    Mean_optimal_gravity = mean(Optimal_gravity),
    .groups = "drop"
  )

print(specificity_summary)

cat("\nResponse pattern distribution:\n")
pattern_summary <- response_analysis$patterns %>%
  count(Response_pattern, name = "Count") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

print(pattern_summary)

# 4. 可視化の生成
cat("\n4. Generating gravity gradient visualizations...\n")

# オプション1: 特異性マップ
cat("Creating specificity map...\n")
tryCatch({
  p1 <- plot_gravity_specificity_map(growth_analysis$optima_data, response_analysis$patterns)
  print(p1)
  cat("Specificity map created successfully!\n")
}, error = function(e) {
  cat("Error in specificity map:", e$message, "\n")
})

# オプション2: ヒートマップ
cat("\nCreating gradient heatmap...\n")
tryCatch({
  p2 <- plot_gravity_gradient_heatmap(growth_analysis$growth_data)
  print(p2)
  cat("Gradient heatmap created successfully!\n")
}, error = function(e) {
  cat("Error in gradient heatmap:", e$message, "\n")
})

# オプション3: 応答曲線
cat("\nCreating response curves...\n")
tryCatch({
  p3 <- plot_gravity_contour_map(growth_analysis$growth_data, growth_analysis$optima_data)
  print(p3)
  cat("Response curves created successfully!\n")
}, error = function(e) {
  cat("Error in response curves:", e$message, "\n")
})

# オプション4: 3Dインタラクティブ（plotlyが利用可能な場合）
cat("\nCreating interactive 3D map...\n")
if (require("plotly", quietly = TRUE)) {
  tryCatch({
    p4 <- plot_interactive_3d_map(growth_analysis$optima_data)
    print(p4)
    cat("Interactive 3D map created successfully!\n")
  }, error = function(e) {
    cat("Error in 3D map:", e$message, "\n")
  })
} else {
  cat("plotly package not available - skipping 3D visualization\n")
}

# 5. 細菌種動態の抽出
cat("\n=== Key Findings ===\n")

# 最も特異性の高い細菌
top_specific <- growth_analysis$optima_data %>%
  group_by(Bacteria) %>%
  summarise(Mean_specificity = mean(Gravity_specificity), .groups = "drop") %>%
  slice_max(Mean_specificity, n = 5)

cat("Top 5 most gravity-specific bacteria:\n")
print(top_specific)

# 各重力環境のトピック解析
gravity_specialists <- growth_analysis$optima_data %>%
  group_by(Adaptation_type) %>%
  slice_max(Gravity_specificity, n = 2) %>%
  select(Bacteria, Adaptation_type, Optimal_gravity, Gravity_specificity)

cat("\nGravity environment specialists:\n")
print(gravity_specialists)

cat("\n=== Analysis Completed Successfully! ===\n")
cat("Gravity gradient specificity analysis provides insights for:\n")
cat("1. Bacterial adaptation strategies across gravity environments\n")
cat("2. Optimal growth conditions for each species\n") 
cat("3. Exercise-dependent responses to gravity changes\n")
cat("4. Risk assessment for different space mission profiles\n")
cat("5. Targeted interventions based on gravity-specific patterns\n")





























# 重力濃度勾配二次元平面における細菌遺伝子発現可視化（斜め軸評価付き）
# Bacterial Gene Expression Visualization on 2D Gravity Gradient Plane with Diagonal Axis Evaluation

# 必要なライブラリ
library(tidyverse)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(scales)
library(ggrepel)

# =============================================================================
# 1. 模擬データの生成
# =============================================================================

set.seed(123)  # 再現可能性のため

# 細菌リストの定義（様々な特性を持つ細菌を想定）
create_mock_bacteria_data <- function(n_bacteria = 25) {
  
  # 細菌名を生成
  bacteria_names <- c(
    "Lactobacillus_acidophilus", "Bifidobacterium_longum", "Escherichia_coli",
    "Bacteroides_fragilis", "Clostridium_difficile", "Enterococcus_faecalis",
    "Staphylococcus_epidermidis", "Pseudomonas_aeruginosa", "Streptococcus_thermophilus",
    "Bacillus_subtilis", "Akkermansia_muciniphila", "Faecalibacterium_prausnitzii",
    "Prevotella_copri", "Roseburia_intestinalis", "Eubacterium_rectale",
    "Methanobrevibacter_smithii", "Helicobacter_pylori", "Campylobacter_jejuni",
    "Salmonella_enterica", "Mycobacterium_tuberculosis", "Corynebacterium_glutamicum",
    "Rhizobium_leguminosarum", "Azotobacter_vinelandii", "Nitrosomonas_europaea",
    "Thiobacillus_ferrooxidans", "Geobacter_sulfurreducens", "Shewanella_oneidensis",
    "Deinococcus_radiodurans", "Pyrococcus_furiosus", "Thermotoga_maritima"
  )[1:n_bacteria]
  
  # 重力レベルの定義
  gravity_levels <- c(0.0, 0.167, 0.333, 1.0)  # 0g, 1/6g, 1/3g, 1g
  gravity_labels <- c("0g", "1/6g", "1/3g", "1g")
  
  # 各細菌の基本特性をランダムに設定
  bacteria_characteristics <- tibble(
    Bacteria = bacteria_names,
    # 基本発現レベル（細菌ごとの基本活性）
    Base_expression = runif(n_bacteria, 50, 200),
    # 重力感受性（重力変化への応答の強さ）
    Gravity_sensitivity = runif(n_bacteria, 0.2, 2.0),
    # 最適重力傾向（どの重力レベルを好むか）
    Optimal_gravity_tendency = sample(1:4, n_bacteria, replace = TRUE,
                                      prob = c(0.2, 0.2, 0.3, 0.3)),  # Earth/Mars寄り
    # 細菌の生理学的分類
    Physiological_type = sample(c("Aerobic", "Anaerobic", "Facultative", "Extremophile"), 
                                n_bacteria, replace = TRUE,
                                prob = c(0.3, 0.3, 0.3, 0.1)),
    # ストレス耐性
    Stress_tolerance = runif(n_bacteria, 0.5, 1.5)
  )
  
  # 各重力条件での遺伝子発現データを生成
  expression_data <- map_dfr(1:n_bacteria, function(i) {
    bacteria_info <- bacteria_characteristics[i, ]
    
    # 各重力レベルでの発現量を計算
    expressions <- map_dbl(1:4, function(g) {
      gravity_val <- gravity_levels[g]
      optimal_gravity <- gravity_levels[bacteria_info$Optimal_gravity_tendency]
      
      # 最適重力からの距離に基づく発現量の計算
      distance_from_optimal <- abs(gravity_val - optimal_gravity)
      
      # 基本発現量 × 重力応答関数 × ランダムノイズ
      base_expr <- bacteria_info$Base_expression
      
      # 重力応答関数（最適重力で最大、離れるほど減少）
      gravity_response <- exp(-distance_from_optimal * bacteria_info$Gravity_sensitivity)
      
      # ストレス応答（極端な重力でのストレス効果）
      stress_effect <- 1 - (abs(gravity_val - 0.5) * (2 - bacteria_info$Stress_tolerance))
      stress_effect <- max(0.3, stress_effect)  # 最低30%の活性は保持
      
      # 最終発現量
      final_expression <- base_expr * gravity_response * stress_effect * 
        rnorm(1, mean = 1, sd = 0.1)  # ランダムノイズ
      
      return(max(10, final_expression))  # 最低発現量を保証
    })
    
    # データフレームとして返す
    tibble(
      Bacteria = bacteria_info$Bacteria,
      Physiological_type = bacteria_info$Physiological_type,
      `0g` = expressions[1],
      `1/6g` = expressions[2],
      `1/3g` = expressions[3],
      `1g` = expressions[4]
    )
  })
  
  return(expression_data)
}

# 模擬データを生成
cat("Generating mock bacterial gene expression data...\n")
mock_data <- create_mock_bacteria_data(25)

cat("Mock data generated successfully!\n")
cat("Bacterial species:", nrow(mock_data), "\n")
cat("Gravity conditions: 4 (0g, 1/6g, 1/3g, 1g)\n")

# データの確認
cat("\nFirst 5 bacteria:\n")
print(head(mock_data, 5))

# =============================================================================
# 2. データ処理と斜め軸（主成分ベクトル）の計算
# =============================================================================

# 各細菌の最適重力環境と斜め軸評価を計算
process_gravity_dependency_with_axes <- function(data) {
  
  gravity_levels <- c(0.0, 0.167, 0.333, 1.0)
  gravity_labels <- c("0g", "1/6g", "1/3g", "1g")
  environment_names <- c("Space", "Moon", "Mars", "Earth")
  
  # 長形式に変換
  long_data <- data %>%
    pivot_longer(cols = c(`0g`, `1/6g`, `1/3g`, `1g`),
                 names_to = "Gravity_condition",
                 values_to = "Expression") %>%
    mutate(
      Gravity_numeric = case_when(
        Gravity_condition == "0g" ~ 0.0,
        Gravity_condition == "1/6g" ~ 0.167,
        Gravity_condition == "1/3g" ~ 0.333,
        Gravity_condition == "1g" ~ 1.0
      ),
      Environment = case_when(
        Gravity_condition == "0g" ~ "Space",
        Gravity_condition == "1/6g" ~ "Moon", 
        Gravity_condition == "1/3g" ~ "Mars",
        Gravity_condition == "1g" ~ "Earth"
      )
    )
  
  # 各細菌の最適条件を計算
  optimal_conditions <- long_data %>%
    group_by(Bacteria, Physiological_type) %>%
    summarise(
      # 最大発現を示す条件
      Max_expression = max(Expression),
      Optimal_gravity = Gravity_numeric[which.max(Expression)],
      Optimal_environment = Environment[which.max(Expression)],
      Optimal_condition = Gravity_condition[which.max(Expression)],
      
      # 重力依存性の計算
      Mean_expression = mean(Expression),
      Expression_CV = sd(Expression) / mean(Expression),  # 変動係数
      Expression_range = max(Expression) - min(Expression),
      
      # 重力勾配応答の計算
      Gravity_correlation = cor(Gravity_numeric, Expression),
      
      .groups = "drop"
    ) %>%
    mutate(
      # 細菌名をきれいにする
      Bacteria_clean = str_replace_all(Bacteria, "_", " "),
      
      # プロット座標
      X_gravity = Optimal_gravity,
      Y_expression = Max_expression,
      
      # =============================================================
      # 斜め軸（主成分ベクトル風）の計算
      # =============================================================
      
      # 軸1: 重力適応軸（45度回転）- 低重力適応 ↔ 高重力適応
      # この軸は右上に行くほど「高重力・高発現」を示す
      Axis1_gravity_adaptation = (X_gravity - min(X_gravity)) / (max(X_gravity) - min(X_gravity)) * 0.707 + 
        (Y_expression - min(Y_expression)) / (max(Y_expression) - min(Y_expression)) * 0.707,
      
      # 軸2: 発現効率軸（-45度回転）- 低効率 ↔ 高効率
      # この軸は左上に行くほど「低重力・高発現」を示す  
      Axis2_expression_efficiency = -(X_gravity - min(X_gravity)) / (max(X_gravity) - min(X_gravity)) * 0.707 + 
        (Y_expression - min(Y_expression)) / (max(Y_expression) - min(Y_expression)) * 0.707,
      
      # 正規化した斜め軸スコア
      Axis1_normalized = (Axis1_gravity_adaptation - min(Axis1_gravity_adaptation)) / 
        (max(Axis1_gravity_adaptation) - min(Axis1_gravity_adaptation)),
      Axis2_normalized = (Axis2_expression_efficiency - min(Axis2_expression_efficiency)) / 
        (max(Axis2_expression_efficiency) - min(Axis2_expression_efficiency)),
      
      # 重力依存性のカテゴリ化
      Dependency_level = case_when(
        Expression_CV > 0.4 ~ "High Dependency",
        Expression_CV > 0.2 ~ "Medium Dependency", 
        TRUE ~ "Low Dependency"
      ),
      
      # 重力応答パターンの分類
      Response_pattern = case_when(
        Gravity_correlation > 0.5 ~ "High-gravity preferring",
        Gravity_correlation < -0.5 ~ "Low-gravity preferring",
        TRUE ~ "Gravity-independent"
      ),
      
      # 適応タイプ
      Adaptation_type = case_when(
        Optimal_gravity >= 0.8 ~ "Earth-adapted",
        Optimal_gravity >= 0.3 ~ "Mars-adapted",
        Optimal_gravity >= 0.15 ~ "Moon-adapted", 
        TRUE ~ "Space-adapted"
      ),
      
      # 斜め軸に基づく新しい分類
      Diagonal_classification = case_when(
        Axis1_normalized > 0.7 ~ "High-Gravity Specialist",
        Axis2_normalized > 0.7 ~ "Low-Gravity Specialist", 
        Axis1_normalized > 0.5 & Axis2_normalized > 0.5 ~ "Versatile Adapter",
        TRUE ~ "Moderate Performer"
      )
    )
  
  return(list(
    long_data = long_data,
    optimal_conditions = optimal_conditions
  ))
}

# データ処理を実行
cat("\nProcessing gravity dependency analysis with diagonal axes...\n")
processed_data <- process_gravity_dependency_with_axes(mock_data)

cat("Analysis completed!\n")
cat("Diagonal axis evaluation calculated for", nrow(processed_data$optimal_conditions), "bacteria\n")

# 斜め軸による分類結果
cat("\nDiagonal axis classification:\n")
diagonal_summary <- processed_data$optimal_conditions %>%
  count(Diagonal_classification, name = "Count") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))
print(diagonal_summary)

# =============================================================================
# 3. 斜め軸付き重力勾配二次元プロット可視化
# =============================================================================

create_gravity_gradient_plot_with_axes <- function(optimal_data) {
  
  cat("\nCreating 2D gravity gradient visualization with diagonal axes...\n")
  
  # プロット用データの準備
  plot_data <- optimal_data %>%
    mutate(
      # ポイントサイズ（重力依存性に基づく）
      Point_size = rescale(Expression_CV, to = c(3, 12)),
      # 透明度（発現範囲に基づく）
      Point_alpha = rescale(Expression_range, to = c(0.6, 1.0))
    )
  
  # 色パレットの定義
  diagonal_colors <- c(
    "High-Gravity Specialist" = "#d73027",      # 赤：高重力スペシャリスト
    "Low-Gravity Specialist" = "#2c7fb8",       # 青：低重力スペシャリスト
    "Versatile Adapter" = "#33a02c",            # 緑：汎用性適応者
    "Moderate Performer" = "#ff7f00"            # オレンジ：中程度パフォーマー
  )
  
  physiological_shapes <- c(
    "Aerobic" = 16,        # 丸
    "Anaerobic" = 17,      # 三角
    "Facultative" = 15,    # 四角
    "Extremophile" = 18    # ダイヤ
  )
  
  # プロット範囲の計算
  x_range <- range(plot_data$X_gravity)
  y_range <- range(plot_data$Y_expression)
  x_center <- mean(x_range)
  y_center <- mean(y_range)
  
  # 斜め軸の線の計算
  # 軸1: 重力適応軸（左下から右上）
  axis1_start_x <- x_range[1] - 0.1 * diff(x_range)
  axis1_start_y <- y_range[1] - 0.1 * diff(y_range)
  axis1_end_x <- x_range[2] + 0.1 * diff(x_range)
  axis1_end_y <- y_range[2] + 0.1 * diff(y_range)
  
  # 軸2: 発現効率軸（右下から左上）
  axis2_start_x <- x_range[2] + 0.1 * diff(x_range)
  axis2_start_y <- y_range[1] - 0.1 * diff(y_range)
  axis2_end_x <- x_range[1] - 0.1 * diff(x_range)
  axis2_end_y <- y_range[2] + 0.1 * diff(y_range)
  
  # メインプロット
  p <- ggplot(plot_data, aes(x = X_gravity, y = Y_expression)) +
    
    # 背景の重力ゾーン
    annotate("rect", xmin = -0.05, xmax = 0.12, ymin = -Inf, ymax = Inf, 
             fill = "#f0f0f0", alpha = 0.3) +
    annotate("rect", xmin = 0.12, xmax = 0.25, ymin = -Inf, ymax = Inf, 
             fill = "#d9d9d9", alpha = 0.3) +
    annotate("rect", xmin = 0.25, xmax = 0.45, ymin = -Inf, ymax = Inf, 
             fill = "#bdbdbd", alpha = 0.3) +
    annotate("rect", xmin = 0.7, xmax = 1.05, ymin = -Inf, ymax = Inf, 
             fill = "#969696", alpha = 0.3) +
    
    # =============================================================
  # 斜め軸（主成分ベクトル風）の描画
  # =============================================================
  
  # 軸1: 重力適応軸（左下→右上）
  annotate("segment", 
           x = axis1_start_x, y = axis1_start_y,
           xend = axis1_end_x, yend = axis1_end_y,
           color = "#d73027", size = 2, alpha = 0.8,
           arrow = arrow(length = unit(0.4, "cm"), type = "closed")) +
    
    # 軸2: 発現効率軸（右下→左上）
    annotate("segment", 
             x = axis2_start_x, y = axis2_start_y,
             xend = axis2_end_x, yend = axis2_end_y,
             color = "#2c7fb8", size = 2, alpha = 0.8,
             arrow = arrow(length = unit(0.4, "cm"), type = "closed")) +
    
    # 斜め軸のラベル
    annotate("text", 
             x = axis1_end_x - 0.15, y = axis1_end_y - 0.05 * diff(y_range),
             label = "重力適応軸\n(Gravity Adaptation)", 
             color = "#d73027", size = 4, fontface = "bold",
             hjust = 1, vjust = 1, angle = 35) +
    
    annotate("text", 
             x = axis2_end_x + 0.05, y = axis2_end_y - 0.05 * diff(y_range),
             label = "発現効率軸\n(Expression Efficiency)", 
             color = "#2c7fb8", size = 4, fontface = "bold",
             hjust = 0, vjust = 1, angle = -35) +
    
    # 斜め軸の目盛り線（補助線）
    # 軸1の目盛り
    geom_abline(slope = 1, intercept = y_center - x_center, 
                color = "#d73027", alpha = 0.3, linetype = "dotted") +
    geom_abline(slope = 1, intercept = (y_center - x_center) + 0.3 * diff(y_range), 
                color = "#d73027", alpha = 0.2, linetype = "dotted") +
    geom_abline(slope = 1, intercept = (y_center - x_center) - 0.3 * diff(y_range), 
                color = "#d73027", alpha = 0.2, linetype = "dotted") +
    
    # 軸2の目盛り
    geom_abline(slope = -1, intercept = y_center + x_center, 
                color = "#2c7fb8", alpha = 0.3, linetype = "dotted") +
    geom_abline(slope = -1, intercept = (y_center + x_center) + 0.3 * diff(y_range), 
                color = "#2c7fb8", alpha = 0.2, linetype = "dotted") +
    geom_abline(slope = -1, intercept = (y_center + x_center) - 0.3 * diff(y_range), 
                color = "#2c7fb8", alpha = 0.2, linetype = "dotted") +
    
    # 環境ラベル
    annotate("text", x = 0.05, y = max(plot_data$Y_expression) * 1.02, 
             label = "SPACE", size = 3.5, fontface = "bold", color = "#2c2c2c") +
    annotate("text", x = 0.167, y = max(plot_data$Y_expression) * 1.02, 
             label = "MOON", size = 3.5, fontface = "bold", color = "#2c2c2c") +
    annotate("text", x = 0.333, y = max(plot_data$Y_expression) * 1.02, 
             label = "MARS", size = 3.5, fontface = "bold", color = "#2c2c2c") +
    annotate("text", x = 0.8, y = max(plot_data$Y_expression) * 1.02, 
             label = "EARTH", size = 3.5, fontface = "bold", color = "#2c2c2c") +
    
    # 細菌データポイント
    geom_point(aes(color = Diagonal_classification,
                   shape = Physiological_type,
                   size = Point_size,
                   alpha = Point_alpha),
               stroke = 1.5) +
    
    # スケール設定
    scale_color_manual(values = diagonal_colors, name = "Diagonal Classification") +
    scale_shape_manual(values = physiological_shapes, name = "Physiological Type") +
    scale_size_identity() +
    scale_alpha_identity() +
    
    # 細菌名ラベル（重複を避けて配置）
    geom_text_repel(aes(label = Bacteria_clean, color = Diagonal_classification),
                    size = 2.5, 
                    max.overlaps = 20,
                    point.padding = unit(0.3, "lines"),
                    box.padding = unit(0.3, "lines"),
                    force = 2,
                    segment.color = "grey50",
                    segment.size = 0.3) +
    
    # 軸の設定
    scale_x_continuous(
      limits = c(-0.1, 1.1),
      breaks = c(0, 0.167, 0.333, 1.0),
      labels = c("0g", "1/6g", "1/3g", "1g"),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.1, 0.15))
    ) +
    
    # テーマ設定
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30"),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.major = element_line(color = "white", alpha = 0.7),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    
    # ラベル
    labs(
      title = "Bacterial Gene Expression: 2D Gravity Gradient with Diagonal Axes",
      subtitle = "Diagonal axes enable evaluation along principal variation directions",
      x = "Optimal Gravity Level (g) →",
      y = "Maximum Gene Expression Level ↑",
      caption = "Red axis: Gravity adaptation (low→high gravity preference)\nBlue axis: Expression efficiency (high efficiency at low gravity)\nPoint size: Gravity dependency | Shape: Physiological type"
    )
  
  return(p)
}

# =============================================================================
# 4. 斜め軸スコア分析表の作成
# =============================================================================

create_diagonal_score_table <- function(optimal_data) {
  
  cat("\nCreating diagonal axis score analysis table...\n")
  
  score_table <- optimal_data %>%
    select(Bacteria_clean, Diagonal_classification, 
           Axis1_normalized, Axis2_normalized,
           Optimal_environment, Max_expression, Expression_CV) %>%
    mutate(
      Gravity_Adaptation_Score = round(Axis1_normalized * 100, 1),
      Expression_Efficiency_Score = round(Axis2_normalized * 100, 1),
      Max_expression = round(Max_expression, 1),
      Expression_CV = round(Expression_CV, 3)
    ) %>%
    arrange(desc(Gravity_Adaptation_Score)) %>%
    select(
      `Bacteria` = Bacteria_clean,
      `Classification` = Diagonal_classification,
      `Gravity Adaptation Score (0-100)` = Gravity_Adaptation_Score,
      `Expression Efficiency Score (0-100)` = Expression_Efficiency_Score,
      `Optimal Environment` = Optimal_environment,
      `Max Expression` = Max_expression,
      `Gravity Dependency` = Expression_CV
    )
  
  return(score_table)
}

# =============================================================================
# 5. 可視化とレポートの実行
# =============================================================================

# メインプロットを作成
cat("\n=== CREATING 2D GRAVITY GRADIENT VISUALIZATION WITH DIAGONAL AXES ===\n")

tryCatch({
  main_plot <- create_gravity_gradient_plot_with_axes(processed_data$optimal_conditions)
  print(main_plot)
  cat("✓ 2D gravity gradient plot with diagonal axes created successfully!\n")
}, error = function(e) {
  cat("✗ Error creating main plot:", e$message, "\n")
})

# 斜め軸スコア表を作成
cat("\n=== DIAGONAL AXIS SCORE ANALYSIS ===\n")
score_table <- create_diagonal_score_table(processed_data$optimal_conditions)
print(score_table)

# =============================================================================
# 6. 詳細統計とサマリー
# =============================================================================

cat("\n=== COMPREHENSIVE ANALYSIS SUMMARY ===\n")

# 斜め軸による分類統計
cat("\n1. Diagonal Axis Classification Distribution:\n")
diagonal_stats <- processed_data$optimal_conditions %>%
  group_by(Diagonal_classification) %>%
  summarise(
    Count = n(),
    Mean_Gravity_Score = round(mean(Axis1_normalized) * 100, 1),
    Mean_Efficiency_Score = round(mean(Axis2_normalized) * 100, 1),
    Mean_Max_Expression = round(mean(Max_expression), 1),
    .groups = "drop"
  )
print(diagonal_stats)

# 斜め軸の相関分析
cat("\n2. Diagonal Axis Correlation Analysis:\n")
axis_correlation <- cor(processed_data$optimal_conditions$Axis1_normalized, 
                        processed_data$optimal_conditions$Axis2_normalized)
cat("Correlation between Gravity Adaptation and Expression Efficiency axes:", round(axis_correlation, 3), "\n")

# 極端値の特定
cat("\n3. Extreme Values on Diagonal Axes:\n")

# 重力適応軸の極端値
cat("\nTop 3 High-Gravity Specialists:\n")
high_gravity_extreme <- processed_data$optimal_conditions %>%
  slice_max(Axis1_normalized, n = 3) %>%
  select(Bacteria_clean, Diagonal_classification, Axis1_normalized, Optimal_environment)
print(high_gravity_extreme)

cat("\nTop 3 Expression Efficiency Specialists:\n") 
efficiency_extreme <- processed_data$optimal_conditions %>%
  slice_max(Axis2_normalized, n = 3) %>%
  select(Bacteria_clean, Diagonal_classification, Axis2_normalized, Optimal_environment)
print(efficiency_extreme)

cat("\n=== DIAGONAL AXIS INTERPRETATION GUIDE ===\n")
cat("【重力適応軸 (Gravity Adaptation Axis)】\n")
cat("• 高スコア(>70): 高重力環境で高発現する重力依存型細菌\n")
cat("• 低スコア(<30): 低重力環境を好む宇宙適応型細菌\n")
cat("• 軸の方向: 左下(低重力適応) → 右上(高重力適応)\n\n")

cat("【発現効率軸 (Expression Efficiency Axis)】\n")
cat("• 高スコア(>70): 低重力環境で高い発現効率を示す\n") 
cat("• 低スコア(<30): 重力に関係なく低い発現効率\n")
cat("• 軸の方向: 右下(低効率) → 左上(高効率・低重力特化)\n\n")

cat("【斜め軸評価のメリット】\n")
cat("1. 従来の直交軸では見えない細菌の特性が明確化\n")
cat("2. 重力適応と発現効率の独立した評価が可能\n")
cat("3. 宇宙生物学的応用に向けた戦略的分類が実現\n")
cat("4. 主成分分析的な解釈による包括的理解\n")

cat("\n=== ANALYSIS COMPLETED ===\n")













# Gravity Environment Gut Microbiome Network Visualization

library(ggplot2)
library(dplyr)
library(igraph)
library(ggraph)

create_sample_data <- function() {
  set.seed(42)
  bacteria <- paste0("ASV_", 1:50)
  samples <- paste0("S_", 1:48)
  gravity <- rep(c("0g", "1/6g", "1/3g", "1g"), each = 12)
  
  asv_data <- matrix(0, 50, 48, dimnames = list(bacteria, samples))
  
  for(i in 1:50) {
    pref_gravity <- sample(c("0g", "1/6g", "1/3g", "1g"), 1)
    base_count <- sample(1000:5000, 1)
    
    for(j in 1:48) {
      effect <- ifelse(gravity[j] == pref_gravity, runif(1, 2, 4), runif(1, 0.3, 0.8))
      asv_data[i,j] <- round(base_count * effect * runif(1, 0.8, 1.2))
    }
  }
  
  rel_data <- sweep(asv_data, 2, colSums(asv_data), FUN = "/")
  
  gravity_means <- data.frame(bacteria = bacteria)
  for(g in c("0g", "1/6g", "1/3g", "1g")) {
    idx <- which(gravity == g)
    gravity_means[[g]] <- rowMeans(rel_data[, idx])
  }
  
  gravity_means$optimal_gravity <- apply(gravity_means[,2:5], 1, function(x) names(x)[which.max(x)])
  gravity_means$x_coord <- scale(rowMeans(gravity_means[,2:5]))[,1]
  gravity_means$y_coord <- scale(apply(gravity_means[,2:5], 1, sd))[,1]
  gravity_means$abundance <- rowMeans(gravity_means[,2:5])
  
  return(gravity_means)
}

create_network <- function(data, threshold = 0.6) {
  coords <- as.matrix(data[, c("x_coord", "y_coord")])
  dist_matrix <- as.matrix(dist(coords))
  similarity <- 1 / (1 + dist_matrix)
  similarity[similarity < threshold] <- 0
  diag(similarity) <- 0
  
  graph <- graph_from_adjacency_matrix(similarity, mode = "undirected", 
                                       weighted = TRUE, diag = FALSE)
  
  V(graph)$x <- data$x_coord[match(V(graph)$name, data$bacteria)]
  V(graph)$y <- data$y_coord[match(V(graph)$name, data$bacteria)]
  V(graph)$gravity <- data$optimal_gravity[match(V(graph)$name, data$bacteria)]
  V(graph)$abundance <- data$abundance[match(V(graph)$name, data$bacteria)]
  
  return(graph)
}

plot_network <- function(graph) {
  colors <- c("0g" = "#FF6B6B", "1/6g" = "#4ECDC4", "1/3g" = "#45B7D1", "1g" = "#96CEB4")
  
  ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
    geom_edge_link(alpha = 0.3, color = "gray60") +
    geom_node_point(aes(color = gravity, size = abundance), alpha = 0.8) +
    scale_color_manual(values = colors, name = "Optimal Gravity") +
    scale_size_continuous(range = c(2, 8), name = "Abundance") +
    labs(title = "Gut Microbiome Gravity Environment Adaptation Network") +
    theme_graph() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

plot_scatter <- function(data) {
  colors <- c("0g" = "#FF6B6B", "1/6g" = "#4ECDC4", "1/3g" = "#45B7D1", "1g" = "#96CEB4")
  
  ggplot(data, aes(x = x_coord, y = y_coord, color = optimal_gravity, size = abundance)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = colors, name = "Optimal Gravity") +
    scale_size_continuous(range = c(2, 6), name = "Mean Abundance") +
    labs(title = "Bacterial Gravity Environment Characteristics Map", 
         x = "Mean Abundance (scaled)", y = "Gravity Sensitivity (scaled)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

run_analysis <- function() {
  data <- create_sample_data()
  network <- create_network(data)
  network_plot <- plot_network(network)
  scatter_plot <- plot_scatter(data)
  
  return(list(network_plot = network_plot, scatter_plot = scatter_plot, data = data))
}

# Usage example
results <- run_analysis()
print(results$network_plot)
print(results$scatter_plot)














# Gravity Environment Gut Microbiome Network Visualization

library(ggplot2)
library(dplyr)
library(igraph)
library(ggraph)

create_sample_data <- function() {
  set.seed(42)
  bacteria <- c("Bifidobacterium longum", "Lactobacillus acidophilus", "Escherichia coli", 
                "Bacteroides fragilis", "Clostridium butyricum", "Enterococcus faecalis",
                "Akkermansia muciniphila", "Prevotella copri", "Faecalibacterium prausnitzii",
                "Ruminococcus bromii", "Eubacterium rectale", "Roseburia intestinalis",
                "Blautia obeum", "Dorea longicatena", "Coprococcus eutactus",
                "Lachnospira pectinoschiza", "Oscillibacter valericigenes", "Alistipes putredinis",
                "Parabacteroides distasonis", "Sutterella wadsworthensis", "Bilophila wadsworthia",
                "Desulfovibrio piger", "Methanobrevibacter smithii", "Collinsella aerofaciens",
                "Actinomyces odontolyticus", "Streptococcus thermophilus", "Lactococcus lactis",
                "Bifidobacterium breve", "Lactobacillus plantarum", "Lactobacillus rhamnosus",
                "Clostridium perfringens", "Clostridium difficile", "Enterobacter cloacae",
                "Klebsiella pneumoniae", "Proteus mirabilis", "Pseudomonas aeruginosa",
                "Staphylococcus epidermidis", "Bacillus subtilis", "Lactobacillus casei",
                "Bifidobacterium bifidum", "Enterococcus faecium", "Clostridium sporogenes",
                "Bacteroides thetaiotaomicron", "Bacteroides ovatus", "Parabacteroides johnsonii",
                "Odoribacter splanchnicus", "Barnesiella intestinihominis", "Tannerella forsythia",
                "Porphyromonas gingivalis", "Fusobacterium nucleatum")
  samples <- paste0("S_", 1:48)
  gravity <- rep(c("0g", "1/6g", "1/3g", "1g"), each = 12)
  
  asv_data <- matrix(0, 50, 48, dimnames = list(bacteria, samples))
  
  for(i in 1:50) {
    pref_gravity <- sample(c("0g", "1/6g", "1/3g", "1g"), 1)
    base_count <- sample(1000:5000, 1)
    
    for(j in 1:48) {
      effect <- ifelse(gravity[j] == pref_gravity, runif(1, 2, 4), runif(1, 0.3, 0.8))
      asv_data[i,j] <- round(base_count * effect * runif(1, 0.8, 1.2))
    }
  }
  
  rel_data <- sweep(asv_data, 2, colSums(asv_data), FUN = "/")
  
  gravity_means <- data.frame(bacteria = bacteria)
  for(g in c("0g", "1/6g", "1/3g", "1g")) {
    idx <- which(gravity == g)
    gravity_means[[g]] <- rowMeans(rel_data[, idx])
  }
  
  gravity_means$optimal_gravity <- apply(gravity_means[,2:5], 1, function(x) names(x)[which.max(x)])
  gravity_means$x_coord <- scale(rowMeans(gravity_means[,2:5]))[,1]
  gravity_means$y_coord <- scale(apply(gravity_means[,2:5], 1, sd))[,1]
  gravity_means$abundance <- rowMeans(gravity_means[,2:5])
  gravity_means$genus <- sapply(strsplit(bacteria, " "), "[", 1)
  
  return(gravity_means)
}

create_network <- function(data, threshold = 0.6) {
  coords <- as.matrix(data[, c("x_coord", "y_coord")])
  dist_matrix <- as.matrix(dist(coords))
  similarity <- 1 / (1 + dist_matrix)
  similarity[similarity < threshold] <- 0
  diag(similarity) <- 0
  
  graph <- graph_from_adjacency_matrix(similarity, mode = "undirected", 
                                       weighted = TRUE, diag = FALSE)
  
  V(graph)$x <- data$x_coord[match(V(graph)$name, data$bacteria)]
  V(graph)$y <- data$y_coord[match(V(graph)$name, data$bacteria)]
  V(graph)$gravity <- data$optimal_gravity[match(V(graph)$name, data$bacteria)]
  V(graph)$abundance <- data$abundance[match(V(graph)$name, data$bacteria)]
  V(graph)$genus <- sapply(strsplit(V(graph)$name, " "), "[", 1)
  
  return(graph)
}

plot_network <- function(graph) {
  colors <- c("0g" = "#FF6B6B", "1/6g" = "#4ECDC4", "1/3g" = "#45B7D1", "1g" = "#96CEB4")
  
  ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
    geom_edge_link(alpha = 0.3, color = "gray60") +
    geom_node_point(aes(color = gravity, size = abundance), alpha = 0.8) +
    geom_node_text(aes(label = genus), size = 2.5, repel = TRUE, max.overlaps = 20) +
    scale_color_manual(values = colors, name = "Optimal Gravity") +
    scale_size_continuous(range = c(2, 8), name = "Abundance") +
    labs(title = "Gut Microbiome Gravity Environment Adaptation Network") +
    theme_graph() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

plot_scatter <- function(data) {
  colors <- c("0g" = "#FF6B6B", "1/6g" = "#4ECDC4", "1/3g" = "#45B7D1", "1g" = "#96CEB4")
  
  ggplot(data, aes(x = x_coord, y = y_coord, color = optimal_gravity, size = abundance)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = genus), size = 2, hjust = 0, vjust = 0, check_overlap = TRUE) +
    scale_color_manual(values = colors, name = "Optimal Gravity") +
    scale_size_continuous(range = c(2, 6), name = "Mean Abundance") +
    labs(title = "Bacterial Gravity Environment Characteristics Map", 
         x = "Mean Abundance (scaled)", y = "Gravity Sensitivity (scaled)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

run_analysis <- function() {
  data <- create_sample_data()
  network <- create_network(data)
  network_plot <- plot_network(network)
  scatter_plot <- plot_scatter(data)
  
  return(list(network_plot = network_plot, scatter_plot = scatter_plot, data = data))
}

# Usage example
results <- run_analysis()
print(results$network_plot)
print(results$scatter_plot)
