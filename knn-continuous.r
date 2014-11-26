# The basic KNN algoritm - continuous version

euclidean <- function(x, q) {
    n_attrs = length(x) - 1
    sqrt(sum((x[1:n_attrs] - q) ^ 2))
}

knn <- function(dataset, query, k=3) {
    class_id = ncol(dataset)

    # First, find the k nearest neighbors.
    distances = c()
    for (i_sample in 1:nrow(dataset)) {
        distances[i_sample] = euclidean(dataset[i_sample,], query)
    }

    nearest = dataset[sort.list(distances)[1:k],]

    # Then, we calculate the mean output of the kNN.
    output = mean(nearest[,class_id])

    output
}
