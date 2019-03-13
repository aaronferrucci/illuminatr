library(readr)
library(dplyr)
library(stringr)

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

read_categories <- function(file, div)
{
  categories <- read.delim(file, stringsAsFactors=F, skip=div$skip[1], nrows=div$nrows[1])
  
  return(categories)
}

clean_categories <- function(categories)
{
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

read_assignments <- function(file, div)
{
  assignments <- read.delim(file, stringsAsFactors=F, skip=div$skip[1], nrows=div$nrows[1])
  
  return(assignments)
}

clean_assignments <- function(assignments, quiz_weight=0.7, drop_min=TRUE)
{
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
  # Remove rows with Pts.Possible matching "- / <any number>"
  assignments <- assignments[!str_detect(assignments$Pts.Possible, "^- / \\d+$"),]
  
  pt_data <- unlist(strsplit(assignments$Pts.Possible, perl=T, "\\s*/\\s*"))
  assignments$points.actual <- as.numeric(pt_data[seq(1, length(pt_data), 2)])
  # even index elements are 'possible'
  assignments$points.possible <- as.numeric(pt_data[seq(2, length(pt_data), 2)])
  
  # The data lies!? Quizzes are not actually worth 100 points (hypothesis: they're worth 70)
  quizzes <- grep("Quiz", assignments$Assignment.Name)
  assignments[quizzes, c("points.actual", "points.possible")] = quiz_weight * assignments[quizzes, c("points.actual", "points.possible")]
  
  if (drop_min) {
    # Find a minimum test score (Q: by percentage or points?)
    ratio <- assignments$points.actual / assignments$points.possible
    # Drop _a_ minimum test score
    min_test_indices <- match(min(ratio), ratio)
    min_test_index <- min_test_indices[1]
    print("Dropping min test:")
    print(assignments[min_test_index,])
    assignments <- assignments[-min_test_index,]
  }
  # Drop unused columns
  assignments <- subset(assignments, select=-c(Gradebook, Pts.Possible, Grade, Other.Marks, Due.Date, Notes))
  
  return(assignments)
}
