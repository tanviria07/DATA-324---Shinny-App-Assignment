# DATA 324 - Shiny App Assignment

Homework 5 For Dr. Thatcher's DATA 324 Class (Fall 2026). A Shiny app for comparing players from two squads at the 2026 World Cup.

## What it does

- Pick any two teams and any two stats (goals, assists, minutes played, per 90 stats, etc.)
- Scatter plot of the two stats, colored by team, with point shape showing position
- Filter by position and by minimum minutes played (per 90 stats are misleading for players who only played a few minutes)
- Click a point on the scatter plot to see which player(s) are there
- Boxplots tab compares the two teams on each chosen stat

## How to run

Needs the `shiny` and `tidyverse` packages. Keep `world_cup_stats.csv` in the same folder as `app_tanvir.R`, then run:

    shiny::runApp("app_tanvir.R")

## Data

`world_cup_stats.csv` has player stats from the 2026 World Cup and was provided with the assignment.
