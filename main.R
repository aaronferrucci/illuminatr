library(readr)
library(dplyr)

find_divisions <- function(file, div_names) {
  # read in the raw data
  raw <- read_lines("data.csv")
  divs <- data.frame(names=div_names, skip=match(div_names, raw))

  # do a bit of line-oriented validation and parsing
  if (any(is.na(divs$skip))) {
    print("Unexpected data: didn't find all names")
    print.data.frame(divs)
    stop("fatal error, stopping")
  }

  # sort the data by line number
  divs <- divs[order(divs$skip),]
  
  # rows in this section? <next section start> - <this section start> - 1 (header) - 1 (div name)
  divs$nrows = divs$skip[seq(nrow(divs)) + 1] - divs$skip[seq(nrow(divs))] - 2
  divs$nrows[is.na(divs$nrows)] = -1 # default to -1, for read.delim nrows arg
  
  return(divs)
}

get_categories <- function(file, div)
{
  categories <- read.delim(file, stringsAsFactors=F, skip=div$skip[1], nrows=div$nrows[1])
  
  # tidy the data:
  # - "Weighted" looks like "80% Weight" - use it to compute "weight"
  categories$weight <- parse_number(categories$Weighted) / 100.0

  # - "Points" looks like "412/470" - use it to compute points.actual, points.possible
  pt_data <- unlist(strsplit(categories$Points, "/"))
  # odd index elements are 'actual'
  categories$points.actual <- as.numeric(pt_data[seq(1, length(pt_data), 2)])
  # even index elements are 'possible'
  categories$points.possible <- as.numeric(pt_data[seq(2, length(pt_data), 2)])
  
  return(categories)
}

get_assignments <- function(file, div)
{
  assignments <- read.delim(file, stringsAsFactors=F, skip=div$skip[1], nrows=div$nrows[1])
  # tidy the data:
  # "Category" has the weight embedded in it. Separate them into Category (matching categories$Category), weight
  cats <- gsub("\\(", "", assignments$Category)
  cats <- gsub("%\\)", "", cats)
  assignments$Category <- cats
  cat_data <- unlist(strsplit(cats, " "))
  assignments$Category <- cat_data[seq(1, length(cat_data), 2)]
  assignments$weight <- as.numeric(cat_data[seq(2, length(cat_data), 2)]) / 100.0
  # Remove rows with Pts.Possible="Missing"
  assignments <- assignments[assignments$Pts.Possible != "Missing",]
  
  pt_data <- unlist(strsplit(assignments$Pts.Possible, perl=T, "\\s*/\\s*"))
  assignments$points.actual <- as.numeric(pt_data[seq(1, length(pt_data), 2)])
  # even index elements are 'possible'
  assignments$points.possible <- as.numeric(pt_data[seq(2, length(pt_data), 2)])
  
  # The data lies!? Quizzes are not actually worth 100 points (hypothesis: they're worth 70)
  quizzes <- grep("Quiz", assignments$Assignment.Name)
  assignments[quizzes, c("points.actual", "points.possible")] = 0.7 * assignments[quizzes, c("points.actual", "points.possible")]
  
  # Find a minimum test score (Q: by percentage or points?)
  assignments$ratio <- assignments$points.actual / assignments$points.possible
  # Drop _a_ minimum test score
  min_test_indices <- match(min(assignments$ratio), assignments$ratio)
  min_test_index <- min_test_indices[1]
  assignments <- assignments[-min_test_index,]
  
  return(assignments)
}

file <- "data.csv"
div_names <- c("Category Breakdown", "Assignments")
divs <- find_divisions(file, div_names)

categories <- get_categories(file, divs[1,])
assignments <- get_assignments(file, divs[2,])

g_assignments <- group_by(assignments, Category)
summary <- summarize(g_assignments, actual = sum(points.actual), possible = sum(points.possible), weight=min(weight))

grade <- sum(summary$weight * (summary$actual / summary$possible))
print(summary)
print(sprintf("grade: %2.2f", 100*grade))
