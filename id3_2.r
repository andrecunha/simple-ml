# The ID3 algorithm

entropy = function(dataset) {
	answer = ncol(dataset)
	options = unique(dataset[,answer])

	counting = rep(0, length(options))
	for (i_option in 1:length(options)) {
		counting[i_option] = sum(dataset[,answer] == options[i_option])
	}

	probabilities = counting / sum(counting)
	entropy = -sum(probabilities * log2(probabilities))

	entropy
}

information_gain = function(dataset, attr_id) {
	entropy_set = entropy(dataset)

	options = unique(dataset[,attr_id])

	counting = rep(0, length(options))
	entropy_subsets = rep(0, length(options))
	for (i_option in 1:length(options)) {
		counting[i_option] = sum(dataset[,attr_id] == options[i_option])
		subset_row_ids = which(dataset[,attr_id] == options[i_option])
		entropy_subsets[i_option] = entropy(dataset[subset_row_ids,])
	}

	proportions = counting / sum(counting)

	information_gain = entropy_set - sum(proportions * entropy_subsets)
	
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
	answer = ncol(node$dataset)
	n_attrs = length(node$eligible_attrs)
	
	# If the entropy is zero, we already have an answer.
	if(entropy(node$dataset) == 0) {
		node$answer = unique(node$dataset[,answer])[1]
		return (node)
	}

	# Otherwise, we need to keep expanding the tree.
	
	# First, we choose the attribute with greatest information gain.
	information_gains = rep(0, n_attrs)
	for (i_attr in 1:n_attrs) {
		information_gains[i_attr] = 
			information_gain(node$dataset, node$eligible_attrs[i_attr])
	}

	attr_chosen = 
		node$eligible_attrs[sort.list(information_gains, decreasing=TRUE)[1]]
	node$attr_chosen = attr_chosen

	# Then, we create a child for each value of the chosen attribute.
	node$children = list()
	options = unique(node$dataset[,attr_chosen])

	for (i_option in 1:length(options)) {
		child_node = list()
		
		child_node_row_ids = 
			which(node$dataset[,attr_chosen] == options[i_option])
		child_node$dataset = node$dataset[child_node_row_ids,]
		child_node$eligible_attrs = 
			setdiff(node$eligible_attrs, c(attr_chosen))

		node$children[[i_option]] = child_node
	}

	# Finaly, we recursively expand each child node.
	for (i_child in 1:length(node$children)) {
		node$children[[i_child]] = expand_tree(node$children[[i_child]])
	}

	node
}

classify = function(tree, query) {
	if (!is.null(tree$answer)) {
		return (tree$answer)
	}

	attr_chosen_value_query = query[tree$attr_chosen]

	for (child in tree$children) {
		attr_chosen_value_child = unique(child$dataset[,tree$attr_chosen])[1]
		if (attr_chosen_value_query == attr_chosen_value_child) {
			return (classify(child, query))
		}
	}
}

