# The Genetic Algorithm

source(file='evol_common.r')

# Partially matched crossover.
pmx = function(mother, father) {
    ncities = length(mother)
    
    single_point = sample(1:(ncities - 1), size=1)

    mp1 = mother[1:single_point]
    mp2 = mother[(single_point + 1):ncities]

    fp1 = father[1:single_point]
    fp2 = father[(single_point + 1):ncities]

    child1 = rep(0, ncities)
    child2 = rep(0, ncities)

    child1[1:single_point] = mp1
    child2[1:single_point] = fp1

    # For the first child.
    intersection = intersect(mp1, fp2)
    blanks = rep(0, length(intersection))
    for (k in 1:length(intersection)) {
        position = which(fp2 == intersection[k]) + length(mp1)
        blanks[k] = position
    }

    child1[blanks] = setdiff(fp1, intersect(mp1, fp1))
    blanks = which(child1 == 0)
    child1[blanks] = setdiff(fp2, intersect(child1, fp2))

    # For the second child.
    intersection = intersect(fp1, mp2)
    blanks = rep(0, length(intersection))
    for (k in 1:length(intersection)) {
        position = which(mp2 == intersection[k]) + length(fp1)
        blanks[k] = position
    }

    child2[blanks] = setdiff(mp1, intersect(fp1, mp1))
    blanks = which(child2 == 0)
    child2[blanks] = setdiff(mp2, intersect(child2, mp2))

    ret = list()

    ret$single_point = single_point
    ret$child1 = child1
    ret$child2 = child2

    ret
}

genetic = function(dist_matrix, ncities=70, pop_size=100, ngenerations=1000, mutation_rate=0.02,
                    selection_method='random', elitism=1, k=2) {
    # Generate the initial chromosomes.
    chromosomes = matrix(0, nrow=pop_size, ncol=ncities)
    fitness = rep(0, pop_size)
    for (i in 1:pop_size) {
        individual = sample(1:ncities, size=ncities)
        chromosomes[i, ] = individual
        fitness[i] = fitness(individual, dist_matrix)
    }
    children_chromosomes = matrix(0, nrow=pop_size, ncol=ncities)
    children_fitness = rep(0, pop_size)

    # Making pop_size even.
    pop_size = pop_size + (pop_size %% 2)

    stats = list()
    stats$mean_fitness = rep(0, ngenerations)
    stats$sd_fitness = rep(0, ngenerations)

    for (gen in 1:ngenerations) {
        # Compute chromosomes statistics
        stats$mean_fitness[gen] = mean(fitness)
        stats$sd_fitness[gen] = sd(fitness)

        # Generate pop_size children.
        i_child = 1
        for (i in 1:(pop_size / 2)) {
            # Select two individuals from the chromosomes.
            if (selection_method == 'random') {
                parents_ids = sample(1:pop_size, size=2)
            } else if (selection_method == 'roulette') {
                parents_ids = sample(1:pop_size, prob=fitness, size=2)
            } else if (selection_method == 'ranking') {
                ranking = rep(0, pop_size)

                ids = sort.list(fitness, decreasing=TRUE)

                value = 1000
                for (j in 1:pop_size) {
                    ranking[ids[j]] = value
                    value = value / 2
                }

                parents_ids = sample(1:pop_size, prob=ranking, size=2)
            } else if (selection_method == 'tournament') {
                k_ids = sample(1:pop_size, size=k)
                best_ids = sort.list(fitness[k_ids], decreasing=TRUE)[1:2]
                parents_ids = k_ids[best_ids]
            }
            parents = chromosomes[parents_ids, ]

            father = parents[1, ]
            mother = parents[2, ]

            # Generate children using crossover.
            children = pmx(mother, father)

            child1 = children$child1
            child2 = children$child2

            # Probabilisticaly mutate each child.
            child1 = mutate(child1, mutation_rate)
            child2 = mutate(child2, mutation_rate)

            # Keep the children in a separate chromosomes.
            children_chromosomes[i_child, ] = child1
            children_chromosomes[i_child + 1, ] = child2

            children_fitness[i_child] = fitness(child1, dist_matrix)
            children_fitness[i_child + 1] = fitness(child2, dist_matrix)

            i_child = i_child + 2
        }

        # Now, we use elitism to keep the best individuals.
        if (elitism > 0) {
            new_chromosomes = matrix(0, nrow=pop_size, ncol=ncities)
            new_fitness = rep(0, pop_size)

            # Join parents and children populations.
            unified_chromosomes = rbind(chromosomes, children_chromosomes)
            unified_fitness = c(fitness, children_fitness)

            # Choose the best 'elitism' individuals.
            bests = sort.list(unified_fitness, decreasing=TRUE)[1:elitism]
             
            # Make them survive to the next generation.
            new_chromosomes[1:elitism, ] = unified_chromosomes[bests, ]
            new_fitness[1:elitism] = unified_fitness[bests]

            # If 'elitism = pop_size', the new population is already ready.
            if (elitism < pop_size) {
                # If an individual from the children chromosomes is among
                #   the bests, we cannot insert it twice in the new
                #   chromosomes.

                # Find the children that are already in the new population.
                bests_in_children = bests[which(bests > pop_size)] - pop_size
                if (length(bests_in_children) == 0) {
                    # There is no children among the bests.
                    bests_in_children = c()
                }
                
                # Find the children that aren't in the new population.
                remaining_children = 
                    children_chromosomes[setdiff(1:pop_size, bests_in_children), ]
                remaining_fitness = 
                    children_fitness[setdiff(1:pop_size, bests_in_children)]

                # Choose a proper number of them to go to the new population.
                children_to_insert = 
                    sample(1:nrow(remaining_children), size=(pop_size - elitism))
                new_chromosomes[(elitism + 1):pop_size, ] = 
                    remaining_children[children_to_insert, ]
                new_fitness[(elitism + 1):pop_size] = 
                    remaining_fitness[children_to_insert]
            }
        } else {
            new_chromosomes = children_chromosomes
            new_fitness = children_fitness
        }

        chromosomes = new_chromosomes
        fitness = new_fitness
    }

    stats
}
