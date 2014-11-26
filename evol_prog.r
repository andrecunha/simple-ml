# The Evolutionary Programming algorithm (model 3)

source(file='evol_common.r')

evol_prog = function(dist_matrix, ncities=70, pop_size=100, ngenerations=1000,
                     mutation_rate=0.02) {
    # Generate the populations
    population = matrix(0, nrow=pop_size, ncol=ncities)
    fitness = rep(0, pop_size)
    for (i in 1:pop_size) {
        individual = sample(1:ncities, size=ncities)
        population[i, ] = individual
        fitness[i] = fitness(individual, dist_matrix)
    }
    children_population = matrix(0, nrow=pop_size, ncol=ncities)
    children_fitness = rep(0, pop_size)

    stats = list()
    stats$mean_fitness = rep(0, ngenerations)
    stats$sd_fitness = rep(0, ngenerations)

    for (gen in 1:ngenerations) {
        # Compute population statistics.
        stats$mean_fitness[gen] = mean(fitness)
        stats$sd_fitness[gen] = sd(fitness)

        # For each individual in the population.
        for (f in 1:pop_size) {
            # Use that individual to produce a child.
            father = population[f, ]
            child = mutate(father, mutation_rate)

            # Compute child's fitness.
            child_fitness = fitness(child, dist_matrix)

            # Keep the child in a separate population.
            children_population[f, ] = child
            children_fitness[f] = child_fitness
        }

        # Unify parents and children populations.
        unified_population = rbind(population, children_population)
        unified_fitness = c(fitness, children_fitness)

        # Keep only the 'pop_size' best individuals.
        survivors = sort.list(unified_fitness, decreasing=TRUE)[1:pop_size]

        population = unified_population[survivors, ]
        fitness = unified_fitness[survivors]
    }

    stats
}
