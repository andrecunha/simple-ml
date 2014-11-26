# The k-means clustering algorithm.

euclidean = function (obj1, obj2) {
    sqrt(sum((obj1 - obj2) ^ 2))
}

select_prototype = function(object, prototypes) {
    distances = rep(0, nrow(prototypes))
    for (p in 1:nrow(prototypes)) {
        distances[p] = euclidean(object, prototypes[p, ])
    }

    which.min(distances)
}

kmeans = function (dataset, k, epsilon = 0.01) {
    dataset = as.data.frame(dataset)

    # Initializing the prototypes.
    prototypes = dataset[sample(1:nrow(dataset), size=k), ]

    deviation = epsilon + 1
    while (deviation > epsilon) {
        deviation = 0
        
        # Selecting the closest prototype of each object.
        closest = rep(0, nrow(dataset))
        for (obj_id in 1:nrow(dataset)) {
            closest[obj_id] = select_prototype(dataset[obj_id, ], prototypes)
        }

        # Updating the prototypes.
        for (p in 1:nrow(prototypes)) {
            objects_in_cluster = which(closest == p)

            if (length(objects_in_cluster) == 0) {
                prototypes[p, ] = 
                    dataset[sample(1:nrow(dataset), size=1), ]
            } else {
                new_prototype = colSums(dataset[objects_in_cluster, ]) / length(objects_in_cluster)
                deviation = deviation + euclidean(new_prototype, prototypes[p, ])
                prototypes[p, ] = new_prototype
            }
        }
    }
    
    ret = list()
    ret$prototypes = prototypes
    ret$closest = closest

    ret
}
