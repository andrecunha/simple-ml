# The Distance-weighted Nearest Neighbors algorithm
#   - discrete version

euclidean <- function(x, q) {
    n_attrs = length(x) - 1
    sqrt(sum((x[1:n_attrs] - q) ^ 2))
}

delta <- function(c1, c2) {
    as.numeric(c1 == c2)
}

dwnn <- function(dataset, query, k=3) {
    n_attrs = ncol(dataset) - 1
    class_id = ncol(dataset)

    # First, we check whether the query point is the same as a
    #   sample in the database.
    for (i_sample in 1:nrow(dataset)) {
        sample = dataset[i_sample,]
        if (sum(sample[1:n_attrs] == query) == n_attrs) {
            return (sample[class_id])
        }
    }

    # Otherwise, we find the k nearest neighbors.
    distances = c()
    for (i_sample in 1:nrow(dataset)) {
        distances[i_sample] = euclidean(dataset[i_sample,], query)
    }

    nearest = dataset[sort.list(distances)[1:k],]
    print (nearest)

    # Then, we find the classes present in the kNN.
    classes = unique(nearest[,class_id])

    # Then, for each class, calculate the weighted sum.
    sums = rep(0, length(classes))
    for (i_class in 1:length(classes)) {
        for (i_sample in 1:nrow(nearest)) {
            sample = nearest[i_sample,]
            weight = 1.0 / euclidean(sample, query) ^ 2
            sums[i_class] = sums[i_class] + 
                            weight * delta(sample[class_id], classes[i_class])
        }
    }
    
    # The class with greatest sum wins.
    class = classes[sort.list(sums, decreasing=TRUE)[1]]
    
    class
}

