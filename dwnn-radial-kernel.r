# The Distance-weighted Nearest Neighbors
#   radial kernel function version

euclidean = function(x, q) {
    sqrt(sum((x - q) ^ 2))
}

radial = function (x, q, sigma) {
    exp(-euclidean(x, q) ^ 2 / 2 * sigma ^ 2)
}

dwnn = function(dataset, query, k=3, sigma=1) {
    n_attrs = ncol(dataset)
    class_id = ncol(dataset)

    # First, we find the k nearest neighbors.
    distances = c()
    for (i_sample in 1:nrow(dataset)) {
        sample = dataset[i_sample,]
        distances[i_sample] = euclidean(sample[1:n_attrs], query)
    }

    nearest = dataset[sort.list(distances)[1:k],]

    # Then, we calculate the class as a weighted sum.
    num = 0.0
    den = 0.0
    for (i_sample in 1:nrow(nearest)) {
        sample = nearest[i_sample,]
        weight = radial(sample[1:n_attrs], query, sigma)
        num = num + weight * sample[class_id]
        den = den + weight
    }

    class = num / den

    class
}

