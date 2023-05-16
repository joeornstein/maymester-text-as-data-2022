

# preliminaries: we need to make sure R can talk to Python

# install the reticulate package and setup:

# ---
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::py_install('youtube-transcript-api')
# ---

# now we can do it in three steps

# 1. create an object from the python package
youtubecaption <- reticulate::import('youtube_transcript_api')

# 2. get the transcript from the video you want
d <- youtubecaption$YouTubeTranscriptApi$get_transcript('SQRqvfqpPe4')

# notice that this comes to us as a giant list with timestamps.
# If we just want the entire transcript we can loop it and paste it together

# 3. paste together the transcript snippets
transcript <- ''
for(i in 1:length(d)){
  transcript <- paste(transcript, d[[i]]$text)
}
transcript



# why not make it a function?
get_youtube_transcript <- function(video_id){

  # 1. create an object from the python package
  youtubecaption <- reticulate::import('youtube_transcript_api')

  # 2. get the transcript from the video you want
  d <- youtubecaption$YouTubeTranscriptApi$get_transcript(video_id)


  # 3. paste together the transcript snippets
  transcript <- ''
  for(i in 1:length(d)){
    transcript <- paste(transcript, d[[i]]$text)
  }

  return(transcript)
}

get_youtube_transcript('yHRMcwQHicI')

