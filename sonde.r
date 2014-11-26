# SONDE

euclidean = function(object1, object2) {
    sqrt(sum((object1 - object2) ^ 2))
}

select_center = function(object, centers, n_centers, sigma, threshold) {
    if (n_centers == 0) {
        return (-1)
    }

    #print('###')
    #print(centers)

    distances = rep(0, n_centers)

    for(c in 1:n_centers) {
        distances[c] = euclidean(object, centers[c, ])
    }

    closest = which.min(distances)[1]

    activation = exp(-(distances[closest] ^ 2) / (2 * sigma ^ 2))

    if (activation < threshold) {
        # Create a new center
        return (-1)
    } else {
        return (closest)
    }
}

sonde = function(dataset, alpha, sigma, threshold) {
    dataset = as.matrix(dataset)

    closest = rep(0, nrow(dataset))
    rows = nrow(dataset)

    centers = matrix(0, nrow=nrow(dataset), ncol=ncol(dataset))
    n_centers = 0

    for (obj_id in 1:nrow(dataset)) {
        cat(paste('Processing object ', obj_id, ' out_of ', nrow(dataset), "\n"))

        object = dataset[obj_id, ]
        center = select_center(object, centers, n_centers,  sigma, threshold)

        if (center == -1) {
            # We should create another center.
            n_centers = n_centers + 1
            centers[n_centers, ] = object
            id = n_centers
        } else {
            # Insert the object under an existing radial.
            centers[center, ] = (1 - alpha) * centers[center, ] + alpha * object
            id = center
        }

        closest[obj_id] = id
    }

    ret = list()
    ret$n_centers = n_centers
    ret$centers = centers[1:n_centers, ]
    ret$closest = closest

    ret
}
