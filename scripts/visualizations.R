library(kableExtra)

Cors <- NAtoMean |>
  cor() |>
  melt() |>
  as.data.frame()


ggplot(Cors, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1)) +
  labs(title = "Correlation between Variables by Strength",
       fill = "Strength")


highCorrelation <- filter(Cors, (value >= 0.05 | value <= -0.05) & 
                            Var1 == "had_coronary_heart_disease")

colnames(highCorrelation) <- c("Variable 1", "Variable 2", "Strength")


corr_table <- kable(highCorrelation, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

ggplot(gather(exerciseActivity, "Key", "Value", -seqn), aes(Value)) +
  geom_histogram() +
  facet_wrap(~Key, scales='free_x') + labs(title = "Exercise Activity Variables' Distribution (Figure 1.0)",
                                           subtitle = "Pre-cleaned")

ggplot(gather(medicalConditions, "Key", "Value", -seqn), aes(Value)) +
  geom_histogram() +
  facet_wrap(~Key, scales='free_x')

ggplot(gather(demographics, "Key", "Value", -seqn), aes(Value)) +
  geom_histogram() +
  facet_wrap(~Key, scales='free_x')



ggplot(exerciseActivity, aes(x= walk_or_bike)) +
  geom_histogram(bins = 2)

ggplot(NAtoMean, aes(x = ldl_cholesterol_mg_d_l)) +
  geom_histogram()

ggplot(NAtoMean, aes(x=  age_at_screening_yrs, 
                     y= had_coronary_heart_disease)) +
  geom_point()


ggplot(NAtoMean, aes(x=bmi, 
                     y= had_coronary_heart_disease)) +
  geom_point()

ggplot(NAtoMean, aes(x= mean_systolic, 
                     fill = as.factor(had_coronary_heart_disease)))   +
  geom_histogram()

systolic_proportion <- NAtoMean
systolic_col <- NAtoMean$mean_systolic

breakpoints <- c(0, 120, 129, 139, 179, max(NAtoMean$mean_systolic))

# Create categories using cut()
categories <- cut(systolic_col, breaks = breakpoints, 
                  labels = c("Normal", "Elevated", "High", "Very High", "Hypertensive"), right = TRUE)

systolic_proportion$systolic_cat <- categories

systolic_proportion$systolic_cat <- as.factor(systolic_proportion$systolic_cat)

category_counts <- systolic_proportion %>%
  group_by(systolic_cat, had_coronary_heart_disease) %>%
  summarise(count = n())

category_counts$systolic_cat <- as.factor(category_counts$systolic_cat)

group_counts <- category_counts %>%
  group_by(systolic_cat) %>%
  dplyr::summarise(total_count = sum(as.numeric(count)))

systolic_proportion <- category_counts %>%
  left_join(group_counts, by = "systolic_cat") %>%
  mutate(percentages = count / total_count * 100)

ggplot(systolic_proportion, aes(x= systolic_cat, 
                     fill = as.factor(had_coronary_heart_disease), y= percentages))   +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentages, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs( x = "Systolic Blood Pressure Categories", 
        y= "Percentages", fill = "Had Coronary Heart Disease (1: yes, 2: No)",
        title = "Systolic Blood Pressure by Proportion of Individuals who Had Coronary Heart Disease")



ggplot(CAD, aes(x= age_at_screening_yrs, fill = had_coronary_heart_disease)) +
  geom_histogram(bins = 60)

ggplot(NAtoMean, aes(x= age_at_screening_yrs, fill = as.factor(had_coronary_heart_disease))) +
  geom_histogram(bins = 40) + labs(x= "Age during Screening (years)",
                                   fill = "Had Coronary Heart Disease? (1: Yes, 2: No)",
                                   title = "Age during Screening (years) by Coronary Heart Disease") +
  theme_minimal()

ggplot(merged_data, aes(x= discomfort_in_chest, 
                        fill= as.factor(had_coronary_heart_disease))) +
  geom_histogram()

ggplot(merged_data, aes(x= shortnes_of_breath_stairs, 
                        fill= as.factor(had_coronary_heart_disease))) +
  geom_histogram()

ggplot(NAtoMean, aes(x= moderate_work_a_day_min )) +
  geom_histogram(bins = 50)

ggplot(CAD, aes(x = age_at_screening_yrs)) +
  geom_histogram()


model_names = c("Boost Tree", "Random Forest", "Support Vector Machine",
                "K-Nearest Neighbor", "Logistic Regression", "Neural Networks")

f1_scores = c(f1_boost, f1_rand_forest, svm_f1, knn_f1, logreg_f1, f1_score_nn)

accuracies <- c(boost_accuracy, rand_forest_accuracy, svm_accuracy, knn_accuracy, logreg_accuracy, nn_accuracy)

model_summary_table <- data.frame(Model = model_names, F1_Score = f1_scores, Accuracy = accuracies)

model_table <- kable(model_summary_table, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

