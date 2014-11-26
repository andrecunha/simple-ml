# The ID3 algorithm

# Calculates the entropy of a dataset
entropy <- function(dataset) {
    # If there is only one instance, entropy is zero
    if (is.vector(dataset)) {
        return (0)
    }

    answer = ncol(dataset)
    options = unique(dataset[, answer])

    counting = c()
    for (opt in options) {
        counting = c(counting, sum(dataset[, answer] == opt))
    }

    probabilities = counting / sum(counting)
    H = -sum(probabilities[!is.nan(probabilities)] * log2(probabilities[!is.nan(probabilities)]))

    H
}

# Calculates the information gain of an attribute
information_gain <- function(dataset, attr) {
    S = entropy(dataset)

    options = unique(dataset[,attr])

    inf_gain = S
    for (opt in options) {
        subset = dataset[dataset[,attr] == opt, ]
        factor = nrow(subset) / nrow(dataset)
        entropy_subset = entropy(subset)
        inf_gain = inf_gain - factor * entropy_subset
    }

    inf_gain
}

# The ID3 algorithm
id3 <- function(dataset) {
    root = list()
    
    root$data= dataset
    root$eligible_attrs = 1:(ncol(dataset) - 1)
    
    root = id3_expand(root)

    root
}

# Expands a tree node
id3_expand <- function(node) {
    # For each attribute, calculate the information gain.
    max_gain = 0
    attr_max_gain = -1
    for (attr in node$eligible_attrs) {
        inf_gain = information_gain(node$data, attr)
        
        # Choose the attribute with higher information gain.
        if (inf_gain > max_gain) {
            max_gain = inf_gain
            attr_max_gain = attr
        }
    }
    node$selected_attr = attr_max_gain

    # If the information gain is 0, we don't need further expansion.
    if (max_gain == 0) {
        node$answer = node$data[1, ncol(node$data)]
        return (node)
    }

    # Otherwise, expand the tree using the attribute with higher information gain.
    node$children = list()
    options = unique(node$data[, attr_max_gain])
    for (opt in 1:length(options)) {
        option = options[opt]
        
        child = list()

        child$data = node$data[node$data[, attr_max_gain] == option, ]
        child$eligible_attrs = setdiff(node$eligible_attrs, attr_max_gain)
        
        node$children[[opt]] = child
    }
    
    # Call recursively to all the generated children.
    for (child in 1:length(node$children)) {
        node$children[[child]] = id3_expand(node$children[[child]])
    }
    
    node
}

