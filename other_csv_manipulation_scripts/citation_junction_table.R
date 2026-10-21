# Libraries
library(dplyr)

# Load data.
remains <- read.csv("database/camel_remains_cleaned.csv")
sources <- read.csv("database/original_sources.csv")

# Select only citations and reference columns.
remains <- remains |>
  select(id, source_other, reference)

# Initialise empty list for junction output.
junction <- list()

# Split secondary_sources into component character strings
# Then split character strings into component words (aka between spaces)
# It results in a char vector containing all existing citation components used in secondary_sources.
all_tokens <- trimws(
  unlist(
    stringr::str_split(remains$secondary_source, stringr::fixed("),"))
  )
)

# Add back the trimmed parantheses and turn back empty strings into empty strings.
all_tokens <- ifelse(grepl("\\)$", all_tokens), all_tokens, paste0(all_tokens, ")"))
all_tokens <- ifelse(all_tokens == ")", "", all_tokens)

# Then split and flatten components at spaces.
all_tokens <- unlist(
  stringr::str_split(trimws(all_tokens), " ")
)

# Check if any component breaks if used as regex.
bad <- all_tokens[sapply(all_tokens, function(p) inherits(try(grepl(p, ""), silent = TRUE), "try-error"))]
if (length(unique(bad)) > 0) {
  print(paste0("Please fix ", unique(bad), ". It breaks as regex. Maybe a parenthesis was missed."))
}


# Loop through every camel id.
for (i in seq_len(nrow(remains))) {
  # Small subset of the references for the current camel id.
  # Same splitting logic as above.
  references <- stringr::str_split(remains$secondary_source[i], stringr::fixed("),"), simplify = TRUE)
  references <- trimws(references)
  references <- ifelse(grepl("\\)$", references), references, paste0(references, ")"))
  references <- ifelse(references == ")", "", references)
  references <- stringr::str_split(references, " ")

  # Initialise empty dataframe to add to the results list.
  tmp_df <- data.frame(
    camel_id = rep(i, length(references) + 1),
    source = NA
  )

  # Loop through the citations for the current camel id.
  for (j in seq_len(length(references))) {
    to_match <- references[[j]] # extract citation components to use as grepl pattern.
    to_match <- to_match[nzchar(to_match)] # remove non-character characters (like double spaces).
    to_match <- gsub("[()]", "", to_match, fixed = FALSE) # remove parantheses (is cleaner for regex).
    if (length(to_match) == 0) next # skip if there is no pattern.

    # Separate year tokens from author/name tokens, and match years more strictly.
    is_year <- grepl("^[0-9]{4}$", to_match) # which of the available components is numeric of 4 digits?

    # set year pattern (every beginning of source will be like ", year.")
    year_pattern <- paste0(", ", to_match[is_year], "\\.") # e.g. ", 1970."

    # and the author component will be the one which is not a year.
    author_tokens <- to_match[!is_year]

    # Assume first that all sources match.
    match <- rep(TRUE, nrow(sources))
    # If there is an author token match based on author.
    if (length(author_tokens) > 0) {
      match <- match & Reduce(`&`, lapply(author_tokens, grepl, x = sources$publication, fixed = TRUE))
    }
    # if there is a year token match based on year.
    if (length(year_pattern) > 0) {
      match <- match & Reduce(`&`, lapply(year_pattern, grepl, x = sources$publication, fixed = FALSE))
    }

    tmp <- sources$source_id[match]

    if (length(tmp) == 0) {
      message("Reference not found in sources. Skipped.")
      next
    } else if (length(tmp) > 1) {
      print(tmp) # add this line temporarily
      stop(
        "Multiple matches for ", paste(to_match, collapse = " "),
        " (camel_id ", i, "). Check your camel remains again!"
      )
    } else {
      message("Found ", paste(to_match, collapse = " "), " for camel_id ", i, ".")
      tmp_df$source[j] <- tmp
    }
  }

  # add one more entry to the temp_df for the full reference that every observation has
  # in reference column
  tmp <- sources |>
    filter(publication == remains$reference[i]) |>
    pull(source_id)

  # Skip the citation if reference was null.
  if (length(tmp) == 0) {
    message("Full reference not found in sources for camel_id ", i, ". Skipped.")
  } else if (length(tmp) > 1) { # stop everything if there are multiple matches
    stop(
      "Multiple matches for full reference (camel_id ", i, "). Check sources for duplicates."
    )
  } else {
    tmp_df$source[length(references) + 1] <- tmp
  }

  junction[[i]] <- tmp_df # append to results list.
}

# melt results list into dataframe and filter.
junction <- bind_rows(junction) |>
  filter(!is.na(source))

# Write data.
readr::write_tsv(junction, "database/junction_table.csv")
