# The ID3 algorithm.

entropy = function(dataset) {
    answer = ncol(dataset)

    options = unique(dataset[,answer])
    counts = rep(0, length(options))

    for (i_option in 1:length(options)) {
        counts[i_option] = sum(dataset[,answer] == options[i_option])
    }

    probabilities = counts / sum(counts)

    entropy = -sum(probabilities * log2(probabilities))

    entropy
}

information_gain = function(dataset, attr_id) {
    entropy_set = entropy(dataset)
    count_set = nrow(dataset)

    options = unique(dataset[,attr_id])
    entropy_subsets = rep(0, length(options))
    count_subsets = rep(0, length(options))

    for(i_option in 1:length(options)) {
        subset_row_ids = which(dataset[,attr_id] == options[i_option])
        subset = dataset[subset_row_ids,]
        entropy_subsets[i_option] = entropy(subset)
        count_subsets[i_option] = nrow(subset)
    }

    information_gain = entropy_set - sum(count_subsets / count_set * entropy_subsets)
    
    information_gain
}

id3 = function(dataset) {
    n_attrs = ncol(dataset) - 1
    root = list()

    root$dataset = dataset
    root$eligible_attrs = 1:n_attrs

    root = expand_tree(root)

    root
}

expand_tree = function(node) {
    # If the entropy of the node's dataset is 0, we already have an answer.
    if (entropy(node$dataset) == 0) {
        answer_col = ncol(node$dataset)
        node$answer = unique(node$dataset[,answer_col])[1]
        return (node)
    }

    # Otherwise, we need to keep expanding the tree.

    # First, we choose the eligible attribute with greater information gain.
    information_gains = rep(0, length(node$eligible_attrs))

    for (i_attr in 1:length(node$eligible_attrs)) {
        information_gains[i_attr] = information_gain(node$dataset, node$eligible_attr[i_attr])
    }

    chosen_attr = node$eligible_attrs[sort.list(information_gains, decreasing=TRUE)[1]]
    node$chosen_attr = chosen_attr

    # Then, we create the children nodes.
    node$children = list()
    options = unique(node$dataset[,chosen_attr])

    for (i_option in 1:length(options)) {
        child = list()

        child_dataset_row_ids = which(node$dataset[,chosen_attr] == options[i_option])
        child$dataset = node$dataset[child_dataset_row_ids,]

        child$eligible_attrs = setdiff(node$eligible_attrs, c(chosen_attr))

        node$children[[i_option]] = child
    }

    # Finally, we recursivelly expand the children.
    for (i_child in 1:length(node$children)) {
        node$children[[i_child]] = expand_tree(node$children[[i_child]])
    }

    node
}

