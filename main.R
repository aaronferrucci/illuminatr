source("utils.R")

file <- "data_2019_03_12.csv"
div_names <- c("Category Breakdown", "Assignments")
divs <- find_divisions(file, div_names)

categories <- read_categories(file, divs[1,])
categories <- clean_categories(categories)
assignments <- read_assignments(file, divs[2,])
assignments <- clean_assignments(assignments, quiz_weight=0.75, drop_min=T)

# To add a hypothetical assignment: include Category, Assignment.Name, points.actual, points.possible, weight
final <- data.frame(Category="Tests", Assignment.Name="Chapter 11 Test", weight=0.8, points.actual=89, points.possible=100)
assignments <- rbind(assignments, final)

g_assignments <- group_by(assignments, Category)
summary <- summarize(g_assignments, actual = sum(points.actual), possible = sum(points.possible), weight=min(weight))

grade <- sum(summary$weight * (summary$actual / summary$possible))
print(summary)
print(sprintf("grade: %2.2f", 100*grade))
