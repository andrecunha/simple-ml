# The Naïve Bayes classifier.

naive_bayes = function(dataset, sample) {
    answer = ncol(dataset)
    hypotheses = unique(dataset[, answer])

    prob_hypotheses = rep(0, length(hypotheses))
    for (i_hyp in 1:length(hypotheses)) {
        hyp_subset = dataset[which(dataset[, answer] == hypotheses[i_hyp]),]
        
        prob_hypothesis = nrow(hyp_subset) / nrow(dataset)

        for (i_attr in 1:length(sample)) {
            value_subset = 
                hyp_subset[which(hyp_subset[, i_attr] == sample[i_attr]),]

            prob_attr_given_hyp = nrow(value_subset) / nrow(hyp_subset)
             
            prob_hypothesis = prob_hypothesis * prob_attr_given_hyp
        }

        prob_hypotheses[i_hyp] = prob_hypothesis
    }

    hypotheses[which.max(prob_hypotheses)]
}
