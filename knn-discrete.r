# The basic KNN - discrete version

euclidean <- function(x, q) {
    n_attrs = length(x) - 1
    sqrt(sum((x[1:n_attrs] - q) ^ 2))
}

knn <- function(dataset, query, k=3) {
    class_id = ncol(dataset)
    
    # First, we find the distances between
    #   the query point and each data point.
    distances = c()
    for (i_sample in 1:nrow(dataset)) {
        distances[i_sample] = euclidean(dataset[i_sample,], query)
    }

    # Now, we need to find the k nearest neighbors
    nearests = dataset[sort.list(distances)[1:k],]

    # Then, we need to find out the class
    #   the occurs the most in the nearest neighbors.
    classes_nearest = unique(nearests[,class_id])

    count = rep(0, length(classes_nearest))
    for (i in 1:length(classes_nearest)) {
        count[i] = sum(nearests[,class_id] == classes_nearest[i])
    }

    class = classes_nearest[sort.list(count, decreasing=TRUE)[1]]

    class
}

