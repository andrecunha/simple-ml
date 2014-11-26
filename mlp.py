#!/usr/bin/env python
from random import uniform
from math import exp


class Neuron:
    def __init__(self, n_inputs):
        self.n_inputs = n_inputs
        self.weights = [uniform(-1, 1) for i in range(n_inputs)]
        self.bias = uniform(-1, 1)

    def get_net(self, inputs):
        net = sum([self.weights[i] * inputs[i] for i in range(self.n_inputs)])
        net += self.bias
        return net

    def get_output(self, inputs):
        sigmoid = lambda x: 1.0 / (1 + exp(-x))
        return sigmoid(self.get_net(inputs))


class MLP:
    def __init__(self, n_inputs, n_hidden, n_output):
        self.n_inputs = n_inputs
        self.hidden_layer = [Neuron(n_inputs) for i in range(n_hidden)]
        self.output_layer = [Neuron(n_hidden) for i in range(n_output)]

    def forward(self, dataset):
        n_inputs = self.n_inputs
        inputs_vector = [line[0:n_inputs] for line in dataset]

        for inputs in inputs_vector:
            hiddens = [n.get_output(inputs) for n in self.hidden_layer]
            outputs = [n.get_output(hiddens) for n in self.output_layer]

            print inputs + outputs

    def backward(self, dataset, epsilon=0.01, eta=0.01):
        n_inputs = self.n_inputs
        i_output = len(or_dataset[0]) - 1

        error = epsilon + 1.0
        while error > epsilon:
            error = 0.0
            for sample in dataset:
                inputs = sample[0:n_inputs]
                expecteds = sample[i_output::]

                # For each input, do the 'forward' step
                hiddens = [n.get_output(inputs) for n in self.hidden_layer]
                outputs = [n.get_output(hiddens) for n in self.output_layer]

                # Calculate the error for this sample
                errors = [(expecteds[k] - outputs[k]) ** 2
                          for k in range(len(self.output_layer))]
                error += sum(errors)

                # Before updating the weights, we calculate the deltas
                compute_delta_output = lambda y, o: (y - o) * o * (1.0 - o)
                deltas_output = [compute_delta_output(expecteds[k], outputs[k])
                                 for k in range(len(self.output_layer))]

                out_sum = lambda j: sum(
                    [deltas_output[k] * self.output_layer[k].weights[j]
                        for k in range(len(self.output_layer))])
                compute_delta_hidden = lambda j: \
                    hiddens[j] * (1.0 - hiddens[j]) * out_sum(j)
                deltas_hidden = [compute_delta_hidden(j)
                                 for j in range(len(self.hidden_layer))]

                # Now, we update the weights
                for k in range(len(self.output_layer)):
                    neuron = self.output_layer[k]
                    updates = [eta * deltas_output[k] * hiddens[j]
                               for j in range(len(self.hidden_layer))]
                    neuron.weights = [neuron.weights[j] + updates[j]
                                      for j in range(len(self.hidden_layer))]

                for j in range(len(self.hidden_layer)):
                    neuron = self.hidden_layer[j]
                    updates = [eta * deltas_hidden[j] * inputs[i]
                               for i in range(n_inputs)]
                    neuron.weights = [neuron.weights[i] + updates[i]
                                      for i in range(n_inputs)]

            print 'Error: ', error

if __name__ == '__main__':
    or_dataset = [[0, 0, 0], [0, 1, 1], [1, 0, 1], [1, 1, 1]]
    mlp = MLP(2, 3, 1)
    mlp.forward(or_dataset)
    mlp.backward(or_dataset)
    mlp.forward(or_dataset)
