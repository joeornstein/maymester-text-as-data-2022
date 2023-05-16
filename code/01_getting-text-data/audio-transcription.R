
# remotes::install_github('bnosac/audio.whisper'')
library(audio.whisper)

model <- whisper('tiny')

transcript <- predict(model, 'data/audio/jfk.wav',
                      lang = 'en', n_threads = 4)

transcript$data$text

# for more: https://github.com/bnosac/audio.whisper