## Call of Duty: Modern Warfare II | Throwing knife analysis

### 1. Context
This data was collected during the annual life cycle (i.e., Oct. 2022 - Nov. 2023) of Call of Duty: Modern Warfare II. Throwing knives were thrown from spawn at the start of each round in the game mode 'Search and Destroy.' Every time a knife hit and killed an opposing team member the corresponding data was recorded:
- The map name
- The date (in the format of y-m-d)
- The side the throw was made on (i.e., offense or defense)
- The in-game name of the player who was hit

The data is in the 'tidy' format (refer to https://vita.had.co.nz/papers/tidy-data.html) and consists of 86 observations and four variables:
- map
- date
- side
- name

### 2. Analysis
I wanted to do a basic exploratory analysis on the dataset and create various plots to answer (and further discuss) the following questions:
1. Assess the annual timeframe of the data, are there any patterns or explanations for the hits?
2. Which map and side had the most hits? With fairly good map knowledge, speculate on why this might have this occurred?
3. Check out the counts of players who were hit
5. Discussion
