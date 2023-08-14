

# preliminaries: we need to make sure R can talk to Python

# install the reticulate package and setup:

# ---
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::py_install('youtube-transcript-api')
# IF YOU ARE A MAC USER, YOU MAY NEED THE FOLLOWING SETUP STEPS AS WELL
# reticulate::conda_create('myenv')
# THEN RESTART R
# reticulate::use_condaenv('myenv')
# reticulate::py_install('youtube-transcript-api')
# ---

# now we can do it in three steps

# 1. create an object from the python package
youtubecaption <- reticulate::import('youtube_transcript_api')

  # 2. get the transcript from the video you want
d <- youtubecaption$YouTubeTranscriptApi$get_transcript('5417se8E04E')

# notice that this comes to us as a giant list with timestamps.
# If we just want the entire transcript we can loop it and paste it together

# loop basics:
for(i in 1:10){
  print(i)
}

for(i in c('hamburger', 'pasta', 'pizza')){
  print(i)
}

for(food in c('hamburger', 'pasta', 'pizza')){
  print(food)
}

for(i in 1:10){
  print(d[[i]]$text)
}




# 3. paste together the transcript snippets
transcript <- ''
for(i in 1:length(d)){
  transcript <- paste(transcript, d[[i]]$text)
}
transcript

# how many characters in that five hour long meeting?
nchar(transcript)


# why not make it a function? then we can easily rerun that set of steps for any video id we choose
get_youtube_transcript <- function(video_id, lang = 'en'){

  # 1. create an object from the python package
  youtubecaption <- reticulate::import('youtube_transcript_api')

  # 2. get the transcript from the video you want
  d <- youtubecaption$YouTubeTranscriptApi$get_transcript(video_id,
                                                          languages = c(lang, 'en'))


  # 3. paste together the transcript snippets
  transcript <- ''
  for(i in 1:length(d)){
    transcript <- paste(transcript, d[[i]]$text)
  }

  return(transcript)
}


transcript <- get_youtube_transcript('mLyOj_QD4a4')
transcript

# multi-language support
transcript <- get_youtube_transcript('E3w1AdTm6nc', lang = 'ja')
transcript

# chinese language support??
transcript <- get_youtube_transcript('S2-8S6O9W54', lang = 'zh-Hant')
transcript

# if I have multiple video ids, and want the transcripts for all of them, let's try a loop:
video_ids <- c('mLyOj_QD4a4', '5417se8E04E')

library(tidyverse)
# we can use the 'map_chr()' function to run that function multiple times and return a list
# of character objects.
# first input is the list we want to iterate over,
# and the second input is the function we want to run
transcripts <- map_chr(video_ids, get_youtube_transcript)

# you could also do this same thing with a for loop,
# but it would be more tedious

