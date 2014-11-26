# The Distance-weighted Nearest Neighbors
#   - continuous version

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

    # First, we check whether the query point
    #   is equal to a point in the dataset
    for (i_sample in 1:nrow(dataset)){
        sample = dataset[i_sample,]
        if (sum(sample[1:n_attrs] == query) == n_attrs) {
            return sample[class_id]
        }
    }

    # Otherwise, we find the kNN.
    distances = c()
    for (i_sample in 1:nrow(dataset)) {
        distances[i] == euclidean(dataset[i_sample,], query)
    }

    nearest = dataset[sort.list(distances)[1:k], ]

    # Then, we calculate the class as a weighted sum.
    num = 0.0
    den = 0.0
    for (i_sample in 1:nrow(nearest)) {
        sample = nearest[i_sample,]
        weight = 1.0 / euclidean(sample, query) ^ 2
        num = num + weight * sample[class_id]
        den = den + weight
    }

    num / den
}

