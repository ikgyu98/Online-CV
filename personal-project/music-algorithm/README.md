# Some Siri-ous Music Shuffler
Music Recommendation Algorithm that finds you the song you would actually enjoy! 
> Ikgyu Shin (ikgyus2@illinois.edu)
>
> Byung Hoon Kwon (bk7@illinois.edu)
> 
> Sohyun Park (sohyunp3@illinois.edu)

## Table of Contents
* [Background](#background)
* [Program](#program)
* [Possible Improvements](#possible-improvements)
* [Acknowledgements](#acknowledgements)


## Background

- Start & End Date
  - 02/26/2022 ~ 04/09/2022 (7 weeks)

- What problem does it (intend to) solve? | What is the purpose of your project?
  - Music shuffling systems of many major streaming services never had provided me satisfactory results. Here, we have decided to build an algorithm of our own which does not simply recommend songs that of similar genre & artist, but actually analyzes the musical feature of your choice of song **(input)** and recommends you the song **(output)** that shares similar musical characteristics.

- Brief explanation of your approach
  - We have attempted to find similarities among many different songs through their musical chord; however, chord processes were not sufficient in representing complexity of a music. Here, through researches, we were able to find Python libraries that provide sophisticated musical trait: Librosa and Spotipy. With that, after a week of research in scholaric articles, we were able to narrow variables down, then ended up with satisfactory classification result


## Program

**Python (Jupyter Notebook)**
- librosa
- spotipy
- sklearn

## Possible Improvements

**Improvements:**
- Simply averaging the numerical musical feature certainly cannot capture the character of the music. As we used the average value of the data, the song could have clustered/characterized slightly differently.
- More training sets.

**How:**
- To improve the means to capture the musics' characteristics, we could create more variables (the average at 1:00, 2:00, and 3:00 respectively, etc) to better capture the piece in full. 


## Acknowledgements

- *This project borrowed knowledges from __[this precedence post](https://www.kdnuggets.com/2020/02/audio-data-analysis-deep-learning-python-part-1.html)__.*
