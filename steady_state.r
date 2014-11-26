# Steady state model (model number 1)

source(file='evol_common.r')

steady_state = function(dist_matrix, ncities=70, pop_size=100, ngenerations = 1000,
                       mutation_rate=0.02) {
    # Generate the population.
    population = NULL
    fitness = rep(0, pop_size)
    for (i in 1:pop_size) {
        individual = sample(1:ncities, size=ncities)
        population = rbind(population, individual)
        fitness[i] = fitness(individual, dist_matrix)
    }

    stats = list()
    stats$mean_fitness = rep(0, ngenerations)
    stats$sd_fitness = rep(0, ngenerations)

    for (gen in 1:ngenerations) {
        # Compute population statistics
        stats$mean_fitness[gen] = mean(fitness)
        stats$sd_fitness[gen] = sd(fitness)

        # Select the father.
        father = population[sample(1:pop_size, size=1),]

        # Probabilisticaly alter the father to create the child.
        child = mutate(father, mutation_rate)
        
        # Compute child's fitness.
        child_fitness = fitness(child, dist_matrix)

        # Select another individual in the population.
        competitor_id = sample(1:pop_size, size=1)
        competitor_fitness = fitness(population[competitor_id, ], dist_matrix)

        # Put the child and that individual to compete.
        if (child_fitness > competitor_fitness) {
            population[competitor_id, ] = child
            fitness[competitor_id] = child_fitness
        }
    }

    stats
}
