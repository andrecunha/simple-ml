# The basic perceptron ANN

# The activation function (here, an unitary step function)
f <- function(net, threshold=0.5) {
    if (net >= threshold) 1 else 0
}

# Applies 'f' to multiple values
f_multiple_values <- function(nets, threshold=0.5) {
    sapply(nets, f)
}

# Auxiliary function that splits a matrix in a list containing it's rows.
row_split <- function(dataset) {
    if (is.vector(dataset)) {
        nrows = 1
        ncols = length(dataset)
    } else {
        nrows = nrow(dataset)
        ncols = ncol(dataset)
    }
    split(as.vector(t(dataset)), rep(1:nrows, each=ncols))
}

# Calculates the output of the perceptron for a list of inputs.
apply_perceptron <- function(inputs, weights) {
    # Split the input in rows
    inputs_list = row_split(inputs)

    # Compute the net of each row (input)
    nets = sapply(inputs_list, function(input_item){ sum(c(input_item, 1) * weights) })

    # Apply 'f' to each net value
    f_multiple_values(nets)
}

# Trains the perceptron
train_perceptron <- function(dataset, eta=0.1, epsilon=0.0001) {
    dataset = as.matrix(dataset)

    answer = ncol(dataset)
    n_attrs = ncol(dataset) - 1

    weights = runif(n_attrs + 1, min=-1, max=1)
    samples = row_split(dataset)

    error = epsilon + 1
    while (error > epsilon) {
        error = 0

        for (sample in samples) {
            input = sample[1:n_attrs]
            expected_outcome = sample[answer]

            # Calculate the outcome using current weights.
            outcome = apply_perceptron(input, weights)
            
            # Calculate the current error
            error = error + (expected_outcome - outcome) ^ 2

            # Update the weights of the inputs
            for (attr in 1:n_attrs) {
                derivative = (expected_outcome - outcome) * -input[attr]
                weights[attr] = weights[attr] - eta * derivative
            }

            # Update the weight of the bias
            derivative = (expected_outcome - outcome) * -1
            weights[length(weights)] = weights[length(weights)] - eta * derivative
        }
    }

    print(apply_perceptron(dataset[, 1:n_attrs], weights))
    weights
}

plot.perceptron <- function(weights) {
	x = seq(0,1,len=100)

    f_multiple_values2 <- function(nets, threshold = 0.5) {
        ret = nets
        ret[nets > threshold] = 1
        ret[nets <= threshold] = 0

        ret
    }

	result = f_multiple_values2(outer(x, x, function(x,y) { 
		       cbind(x, y, rep(1,100)) %*% weights; } ))
	filled.contour(x, x, result)
}
