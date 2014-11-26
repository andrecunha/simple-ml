# Basic functions for evolutive algorithms.

distances <-function(dmin, dmax, ncities) {
	dist = matrix(0, nrow=ncities, ncol=ncities)

	for (i in 1:(ncities-1)) {
		for (j in (i+1):ncities) {
			dist[i,j] = runif(dmin, dmax, n=1)
			dist[j,i] = dist[i,j]
		}
	}

	dist
}

fitness <- function(candidate.solution, distance.matrix) {
	total = 0

	candidate.solution = c(candidate.solution,
			       candidate.solution[1])

	for (i in 1:(length(candidate.solution)-1)) {
		total = total + 
			distance.matrix[candidate.solution[i],
					candidate.solution[i+1]]
	}

	#-total
	#exp(-total)
	1/total
}

mutate = function(child, mutation_rate) {
    if (runif(min=0, max=1, n=1) < mutation_rate) {
        cities_to_swap = sample(1:length(child), size=2)

        aux = child[cities_to_swap[1]]
        child[cities_to_swap[1]] = child[cities_to_swap[2]]
        child[cities_to_swap[2]] = aux
    }

    child
}
