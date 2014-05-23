#!/usr/bin/Rscript

## Multicolumns ##

output3Cols <- function(poem_id, addTitle=FALSE, stanzaNumeration=FALSE) {
  cat("\\begin{minipage}[t]{0.4\\textwidth}\n")
  outputPoems(poem_id, addTitle=FALSE, stanzaNumeration=FALSE)
  cat("\\end{minipage}\n")
  cat("\\begin{minipage}[t]{0.4\\textwidth}\n")
  cat("\\poemlines{0}\n")
  outputSyllabi(poem_id, addTitle=FALSE, stanzaNumeration=FALSE)
  cat("\\end{minipage}\n")
  cat("\\begin{minipage}[t]{0.2\\textwidth}\n")
  outputVerseType(poem_id, addTitle=FALSE, stanzaNumeration=FALSE)
  cat("\\end{minipage}\n")
}


sqLiteLoadTable <- function(database, table) {
    con <- dbConnect("SQLite", dbname = database)
    query <- dbSendQuery(con, paste("SELECT * FROM ", table, ";", sep="")) 
    result <- fetch(query, n = -1, encoding="utf-8")
    dbClearResult(query)
    dbDisconnect(con)
    return(result)
}

sqLiteAddPoem <- function(database, bodytext, author_id, title="", collection_id="", num_order) {
  con <- dbConnect("SQLite", dbname = database)
  dbSendQuery(con, paste0("INSERT INTO poem (bodytext, author_id, poem_title, 
                                             collection_id, num_order) 
                                   VALUES ('", 
                                   bodytext,
                                   "', '",
                                   author_id,
                                   "', '",
                                   title,
                                   "', '",
                                   collection_id,
                                   "', '",
                                   num_order,
                                   "');")) 
  dbDisconnect(con)
}

appendSurname <- function(name, surname) {
  if (is.na(surname)) {return(name)}
  else {return(paste0(name," ",surname))}
}

# From http://stackoverflow.com/a/6364905/1707938
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

underlineWords <- function (string, words) {
  words <- paste(tolower(words), toupper(words), simpleCap(words), sep=",") 
  words <- strsplit(words, ",")[[1]]
  words <- gsub("^\\s+|\\s+$","",words)
    
  for (word in words) {
    string <- gsub(word,
                   paste0("\\\\underline{",word,"}"),
                   string,
                   ignore.case=FALSE)
  }
  return(string)
}

returnVerseInterval <- function(string, first_line, last_line) {
  library(stringr)
  stanzas <- strsplit(string, "\n\n")[[1]]
  stanza_verses <- strsplit(string, "\n")[[1]]
  empty_lines <- stanza_verses %in% ""
  
  # Count number of empty lines within interval
  offset <- sum(empty_lines[first_line:last_line]==TRUE)
  verse_interval <- stanza_verses[first_line:(last_line+offset)]
  
  # Add new line special character and trim last one
  verse_interval <- paste0(verse_interval,"\n")
  verse_interval <- paste(verse_interval, collapse="")
  verse_interval <- substr(verse_interval, 1, nchar(verse_interval)-1)
  
  return(verse_interval)
}

outputPoemInterval <- function(poem_id, first_line, last_line, words=FALSE) {
  
  poem_number <- merged_df$num_order[match(poem_id, merged_df$poem_id)]
  appendix <- paste0(" (\\textbf{",
                     poem_number,
                     "}, ",
                     first_line,
                     "-",
                     last_line,
                     ")\n")
  bodytext <- merged_df$bodytext[match(poem_id, merged_df$poem_id)]
  bodytext <- returnVerseInterval(bodytext, first_line, last_line)
  bodytext <- paste0(bodytext, appendix)
  bodytext <- formatString4Verse(bodytext, stanzaNumeration=FALSE)
  # Remove //! on last line of peom, which generates LaTeX error
  bodytext <- gsub("\\n\\\\\\\\!\\n$", "!\n", bodytext)
  
  # Underline words
  if (words != FALSE) {bodytext <- underlineWords(bodytext, words)}
                              
  cat("\\begin{verse}\n")
  cat(paste0("\\setverselinenums{",first_line,"}{0} \n"))
  cat(bodytext)
  cat("\\end{verse}\n\n")
}


outputPoems <- function(poem_id="", author_id="", author_name="", collection_code="",
                        authorName=FALSE,yearBirth=FALSE,collectionTitle=FALSE,yearWork=FALSE,
                        stanzaNumeration=TRUE, addTitle=TRUE) {  
  df <- returnPoems(merged_df, poem_id, author_id, author_name, collection_code)
  for (i in 1:nrow(df)) {
    title <- df$poem_title[i]
    bodytext <- formatString4Verse(df$bodytext[i],stanzaNumeration)
    if (authorName==TRUE) {author <- df$author_name[i]} else {author <- ""}
    if (yearBirth==TRUE) {
      year_birth <- df$year_birth[i]
      year_death <- df$year_death[i]
      if (is.na(year_death)) {year_death <- ""} 
    } else {
      year_birth <- ""
      year_death <- ""
    }
    if (collectionTitle==TRUE) {collection <- df$collection_name[i]} else {collection <- ""}
    if ((yearWork==TRUE) && (collectionTitle==FALSE)) {year_work <- df$poem_year[i]} else {year_work <- ""}
    if ((yearWork==TRUE) && (collectionTitle==TRUE)) {year_work <- df$collection_year[i]} else {year_work <- ""}
    
    # Suppress title in case
    if (addTitle==FALSE) {title <- FALSE}
    
    typesetPoem(title, bodytext, author, collection, year_work, year_birth, year_death)
  }
}

outputSyllabi <- function(poem_id="", author_id="", author_name="", collection_code="",
                        authorName=FALSE,yearBirth=FALSE,collectionTitle=FALSE,yearWork=FALSE,
                        stanzaNumeration=TRUE, addTitle=TRUE) {  
  df <- returnPoems(merged_df, poem_id, author_id, author_name, collection_code)
  for (i in 1:nrow(df)) {
    title <- df$poem_title[i]
    bodytext <- formatString4Verse(df$human_syllabi[i], stanzaNumeration)
    
    # Suppress title in case
    if (addTitle==FALSE) {title <- FALSE}
    
    typesetPoem(title, bodytext)
  }
}

outputVerseType <- function(poem_id="", author_id="", author_name="", collection_code="",
                          authorName=FALSE,yearBirth=FALSE,collectionTitle=FALSE,yearWork=FALSE,
                          stanzaNumeration=TRUE, addTitle=TRUE) {  
  df <- returnPoems(merged_df, poem_id, author_id, author_name, collection_code)
  for (i in 1:nrow(df)) {
    title <- df$poem_title[i]
    bodytext <- formatString4Verse(df$verse_type[i],stanzaNumeration)
    
    # Suppress title in case
    if (addTitle==FALSE) {title <- FALSE}
    
    typesetPoem(title, bodytext)
  }
}

returnPoems <- function (df, poem_id="", author_id="", author_name="", collection_code="") {
    if (poem_id != "") {
        if (is.numeric(poem_id)) {
            index <- match(poem_id, df$poem_id)
        } else {
            stop("Poem ids must be numeric (e.g. 1, 3, 7, 9)" )
        }
    } else if(author_id != "") {
        if (is.numeric(author_id)) {
          index <- match(author_id, df$author_id)
        } else {
          stop("Author ids must be numeric (e.g. 1, 3, 7, 9)" )
        }
    } else if((author_name != "") && (collection_code != "")) {
        index1 <- df$author_name %in% author_name
        index2 <- df$collection_code %in% collection_code
        index <- intersect(index1, index2)
    } else if(author_name != "") {
        index <- df$author_name %in% author_name
    } else {
      stop("You didn't provide enough attributes. 
           Provide poem_id or author_id or author_name. 
           collection_code alone is not enough")
    }
    return(data.frame(          
      poem_title = df$poem_title[index],
      bodytext = df$bodytext[index],
      human_syllabi = df$human_syllabi[index],
      verse_type = df$verse_type[index],
      author_name = df$author_name[index],
      collection_name = df$collection_name[index],
      collection_code = df$collection_code[index],
      collection_year = df$collection_year[index],
      poem_year = df$poem_year[index],
      year_birth = df$year_birth[index],
      year_death = df$year_death[index]
    ))
}

typesetPoem <- function (title, bodytext, author="", collection="", year_work="", year_birth="", year_death="") {
    cat("\\setcounter{verseno}{0}\n")
    if (title!=FALSE) {cat(paste0("\\poemtitle{",title,"}\n"))}
    cat("\\begin{verse}\n")
    cat(bodytext)
    cat("\\end{verse}\n\n")
    if (author !="") {
        if (year_birth!="") {
            cat(paste0("\\poemAttrib{",
                       author,
                       " (",
                       year_birth,
                       "-",
                       year_death,
                       ")}\n"
                       ))
        } else {
            if ((collection!="") && (year_work!="")) {
                cat(paste0("\\poemAttrib{",
                           author,
                           ", \\textit{",
                           collection,
                           "}, ",
                           year_work,
                           "}\n"))
            } else {
                if ((collection!="") && (year_work=="")) {
                    cat(paste0("\\poemAttrib{",
                               author,
                               ", \\textit{",
                               collection,
                               "}}\n"
                               ))
                }
                if ((collection=="") && (year_work!="")) {
                    cat(paste0("\\poemAttrib{",
                               author,
                               ", ",
                               year_work,
                               "}\n"
                               ))
                }
                if ((collection=="") && (year_work=="")) {
                    cat(paste0("\\poemAttrib{",
                               author,
                               "}\n"
                               ))
                }
            }
        }
    }
    if ((collection!="") && (year_work!="")) {
        cat(paste0("\\poemAttrib{",
                   "\\textit{",
                   collection,
                   "}, ",
                   year_work,
                   "}\n"))
    } else {
        if ((collection!="") && (year_work=="")) {
            cat(paste0("\\poemAttrib{",
                       "\\textit{",
                       collection,
                       "}}\n"
                       ))
        }
        if ((collection=="") && (year_work!="")) {
            cat(paste0("\\poemAttrib{",
                       year_work,
                       "}\n"
                       ))
        }
    }
}

## String Formatting ## Mare sui viaggi dell'età.

replaceHTMLTag <- function(string) {
  string <- gsub("<i>","\\\\textit{", string)
  string <- gsub("</i>","}", string)
  string <- gsub("<b>","\\\\textbf{", string)
  string <- gsub("</b>","}", string)
  string <- gsub("<center>","{\\\\center ", string)
  string <- gsub("</center>","}", string)

} 

sanitizeString <- function(string) {
  string <- gsub("’", "'", string)
  return(string)
}

keepIndent <- function(string) {
  string <- gsub("\n      ","\n\\\\\\hspace{3em}", string)
  string <- gsub("\n    ","\n\\\\\\hspace{2em}", string)
  string <- gsub("\n  ","\n\\\\\\hspace{1em}", string)
  string <- gsub("\n ","\n\\\\\\hspace{0.5em}", string)
  return(string)
}

formatString4Verse <- function(string, stanzaNumeration=FALSE) {
  string <- sanitizeString(string)
  string <- replaceHTMLTag(string)
  string <- keepIndent(string)
  if (stanzaNumeration==FALSE) {
    string <- gsub("\n\n", "\\\\\\\\!§newline§", string)
    string <- gsub("\n", "\\\\\\\\§newline§", string)
    string <- paste0(string, "\\\\!§newline§")
    string <- gsub("§newline§", "\n", string)
    return(string)
  }
  if (stanzaNumeration==TRUE) {
    string <- gsub("\n\n", "\\\\\\\\!§newline§\\\\stanzaNumber§newline§", string)
    string <- gsub("\n", "\\\\\\\\§newline§", string)
    string <- paste0(string, "\\\\!§newline§")
    string <- paste0("\\stanzaNumber§newline§", string)
    string <- gsub("§newline§", "\n", string)
    return(string)
  }
}
