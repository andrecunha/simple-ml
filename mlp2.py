#!/usr/bin/env python
from random import uniform
from math import exp


class Neuron:
    def __init__(self, n_inputs):
        self.n_inputs = n_inputs
        self.weights = [uniform(-1, 1) for i in range(n_inputs)]
        self.bias = uniform(-1, 1)

    def get_net(self, inputs):
        net = sum([w * x for w, x in zip(self.weights, inputs)]) + self.bias
        return net

    def get_output(self, inputs):
        sigmoid = lambda x: 1.0 / (1.0 + exp(-x))
        return sigmoid(self.get_net(inputs))


class MLP:
    def __init__(self, n_inputs, n_hidden, n_output):
        self.n_inputs = n_inputs
        self.hidden_layer = [Neuron(n_inputs) for i in range(n_hidden)]
        self.output_layer = [Neuron(n_hidden) for i in range(n_output)]

    def forward(self, dataset):
        for sample in dataset:
            inputs = sample[0:self.n_inputs]
            hiddens = [n.get_output(inputs) for n in self.hidden_layer]
            outputs = [n.get_output(hiddens) for n in self.output_layer]
            print sample + outputs

    def backward(self, dataset, eta=0.01, epsilon=0.01):
        error = epsilon + 1.0
        while error > epsilon:
            error = 0.0
            for sample in dataset:
                inputs = sample[0:self.n_inputs]
                expecteds = sample[self.n_inputs::]

                # First, we do the forward step.
                hiddens = [n.get_output(inputs) for n in self.hidden_layer]
                outputs = [n.get_output(hiddens) for n in self.output_layer]

                # Then, we compute the squared error for this sample.
                error += \
                    sum([(y - o) ** 2 for y, o in zip(expecteds, outputs)])

                # Then, we compute the deltas.
                comp_delta_output = lambda (y, o): (y - o) * o * (1.0 - o)
                deltas_output = map(comp_delta_output, zip(expecteds, outputs))

                weights_j = lambda j:\
                    [neuron.weights[j] for neuron in self.output_layer]
                propagated_error = lambda j:\
                    sum([d * w for d, w in zip(deltas_output, weights_j(j))])
                comp_delta_hidden = lambda j:\
                    hiddens[j] * (1.0 - hiddens[j]) * propagated_error(j)
                deltas_hidden = map(comp_delta_hidden, range(len(hiddens)))

                # Finally, we update the weights.
                for k in range(len(self.output_layer)):
                    neuron = self.output_layer[k]
                    updates = [eta * deltas_output[k] * h for h in hiddens]
                    neuron.weights = \
                        [weight + update
                            for weight, update in zip(neuron.weights, updates)]

                for j in range(len(self.hidden_layer)):
                    neuron = self.hidden_layer[j]
                    updates = [eta * deltas_hidden[j] * i for i in inputs]
                    neuron.weights = \
                        [weight + update
                            for weight, update in zip(neuron.weights, updates)]

            print "Error: ", error


if __name__ == "__main__":
    or_dataset = [[0, 0, 0], [0, 1, 1], [1, 0, 1], [1, 1, 1]]
    mlp = MLP(2, 3, 1)
    mlp.forward(or_dataset)
    mlp.backward(or_dataset)
    mlp.forward(or_dataset)
