# Getting the AFLW data
library(fitzRoy)
aflw <- fetch_player_stats(2021, comp = "AFLW")

# Clean a little
library(lubridate)
library(tidyverse)
aflw_clean <- aflw %>%
	mutate(utcStartTime = as_datetime(utcStartTime),
		     date = date(utcStartTime),
				 day = wday(utcStartTime, label=TRUE, abbr = TRUE, week_start=1),
				 hour = hour(utcStartTime)) %>%
	select(-c(utcStartTime, status, compSeason.shortName, round.name,
				 player.photoURL, home.team.club.name,
				 away.team.club.name,
				 player.player.player.playerJumperNumber,
				 teamId, inside50s, ratingPoints, lastUpdated,
				 goalEfficiency, shotEfficiency,
				 interchangeCounts, scoreInvolvements,
				 extendedStats, clearances.centreClearances,
				 clearances.stoppageClearances,
				 player.playerJumperNumber,
				 player.player.player.givenName,
				 player.player.player.surname,
				 player.player.player.captain,
				 teamStatus, player.player.player.playerId,
				 superGoals, ranking, dreamTeamPoints)) %>%
	rename(provider = providerId,
		     round = round.roundNumber,
				 venue = venue.name,
				 number = player.jumperNumber,
				 position = player.player.position,
				 home_team = home.team.name,
				 away_team = away.team.name,
				 id = player.playerId,
				 captain = player.captain,
				 given_name = player.givenName,
				 surname = player.surname,
				 num_games = gamesPlayed,
				 time_pct = timeOnGroundPercentage,
				 contested = contestedPossessions,
				 uncontested = uncontestedPossessions,
				 possessions = totalPossessions,
				 marks_in50 = marksInside50,
				 contested_marks = contestedMarks,
				 one_pct = onePercenters,
				 disposal = disposalEfficiency,
				 frees_for = freesFor,
				 frees_against = freesAgainst,
				 rebounds_in50 = rebound50s,
				 assists = goalAssists,
				 accuracy = goalAccuracy,
				 tackles_in50 = tacklesInside50,
				 shots = shotsAtGoal,
         metres = metresGained,
         clearances = clearances.totalClearances,
				 team = team.name) %>%
	select(id, given_name, surname, number, position, team,
				 time_pct:clearances,
				 round, date, day, hour,
				 venue, home_team, away_team,
				 provider)

# Check for duplicates
aflw_clean %>%
	select(id, given_name, surname, number, team) %>%
	distinct() %>%
	count(id, sort=TRUE) %>%
	summary(n)
# Players do play different positions, but all have same
# name and team and jumper number throughout season

# Calculate player averages
aflw_clean2 <- aflw_clean %>%
	group_by(id, given_name, surname, number, team) %>%
	summarise_if(is.numeric, mean) %>%
	ungroup()

# Calculate most common position
mymode <- function(x) {
	t <- table(x)
	names(t)[ which.max(t) ]
}
aflw_clean3 <- aflw_clean %>%
	group_by(id, given_name, surname, number, team) %>%
	summarise(position = mymode(position)) %>%
	ungroup()

aflw_player <- left_join(aflw_clean2, aflw_clean3) %>%
	select(id:team, position, time_pct:clearances)
aflw <- aflw_player
save(aflw, file="data/aflw.rda")
