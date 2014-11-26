# The Batch model (model 2)

source(file='evol_common.r')

batch = function(dist_matrix, ncities=70, pop_size=100, ngenerations=1000, mutation_rate=0.02, nchildren=10) {
    # Generate the populations
    population = matrix(0, nrow=pop_size, ncol=ncities)
    fitness = rep(0, pop_size)
    for (i in 1:pop_size) {
        individual = sample(1:ncities, size=ncities)
        population[i,] = individual
        fitness[i] = fitness(individual, dist_matrix)
    }
    children_population = matrix(0, nrow=nchildren, ncol=ncities)
    children_fitness = rep(0, nchildren)

    stats = list()
    stats$mean_fitness = rep(0, ngenerations)
    stats$sd_fitness = rep(0, ngenerations)

    for (gen in 1:ngenerations) {
        # Compute population statistics
        stats$mean_fitness[gen] = mean(fitness)
        stats$sd_fitness[gen] = sd(fitness)

        # For nchildren children.
        for(ch in 1:nchildren) {
            # Select a father.
            father = population[sample(1:pop_size, size=1), ]
            
            # Probabilisticaly alter the father to produce a child.
            child = mutate(father, mutation_rate)

            # Compute child's fitness.
            child_fitness = fitness(child, dist_matrix)

            # Keep the child in it's own population.
            children_population[ch,] = child
            children_fitness[ch] = child_fitness
        }

        # For each child.
        for (ch in 1:nchildren) {
            # Choose a competitor among the parents population.
            competitor_id = sample(1:pop_size, size=1)
            competitor_fitness = fitness(population[competitor_id, ], dist_matrix)

            # Force child 'ch' to compete against the chosen competitor.
            if (children_fitness[ch] > competitor_fitness) {
                population[competitor_id, ] = children_population[ch, ]
                fitness[competitor_id] = children_fitness[ch]
            }
        }
    }

    stats
}
