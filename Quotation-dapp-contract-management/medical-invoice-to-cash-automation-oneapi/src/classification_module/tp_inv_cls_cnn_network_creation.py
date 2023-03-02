# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
# pylint: disable=E121,E231,E501
import tensorflow.compat.v1 as tf

# Model Hyperparameters
embedding_dim = 50
filter_sizes = "3,4,5"
num_filters = 100
l2_reg_lambda = 0.0


def cnn_network_creation_invoker(training_x, training_y, vocabulary_size):
    class TextCNN(object):
        """
        A CNN for text classification.
        Uses an embedding layer, followed by a convolutional, max-pooling and softmax layer.
        """
        def __init__(
          self, sequence_length, num_classes, vocab_size,
          embedding_size, filter_sizes, num_filters, l2_reg_lambda=0.0):

            # Placeholders for input, output and dropout
            self.input_x = tf.placeholder(tf.int32, [None, sequence_length], name="input_x")
            self.input_y = tf.placeholder(tf.float32, [None, num_classes], name="input_y")
            self.dropout_keep_prob = tf.placeholder(tf.float32, name="dropout_keep_prob")

            # Keeping track of l2 regularization loss (optional)
            l2_loss = tf.constant(0.0)

            # Embedding layer
            with tf.device('/cpu:0'), tf.name_scope("embedding"):
                self.W = tf.Variable(tf.random_uniform([vocab_size, embedding_size], -1.0, 1.0), name="W")  # We initialize embedding matrix using a random uniform distribution.
                self.embedded_chars = tf.nn.embedding_lookup(self.W, self.input_x)
                self.embedded_chars_expanded = tf.expand_dims(self.embedded_chars, -1)

            # Create a convolution + maxpool layer for each filter size
            pooled_outputs = []
            for i, filter_size in enumerate(filter_sizes):
                with tf.name_scope("conv-maxpool-%s" % filter_size):
                    # Convolution Layer
                    filter_shape = [filter_size, embedding_size, 1, num_filters]
                    W = tf.Variable(tf.truncated_normal(filter_shape, stddev=0.1), name="W")
                    b = tf.Variable(tf.constant(0.1, shape=[num_filters]), name="b")
                    # Each filter slides over the whole embedding, but varies in how many words it covers
                    conv = tf.nn.conv2d(
                        self.embedded_chars_expanded,
                        W,
                        strides=[1, 1, 1, 1],
                        padding="VALID",
                        name="conv")
                    # h is the result of applying the nonlinearity to the convolution output.
                    h = tf.nn.relu(tf.nn.bias_add(conv, b), name="relu")
                    # Maxpooling over the outputs
                    pooled = tf.nn.max_pool(
                        h,
                        ksize=[1, sequence_length - filter_size + 1, 1, 1],
                        strides=[1, 1, 1, 1],
                        padding='VALID',
                        name="pool")
                    pooled_outputs.append(pooled)

            # Combine all the pooled features
            num_filters_total = num_filters * len(filter_sizes)
            #  Once we have all the pooled output tensors from each filter size we combine them into one long feature vector of shape [batch_size, num_filters_total].
            self.h_pool = tf.concat(pooled_outputs, 3)
            self.h_pool_flat = tf.reshape(self.h_pool, [-1, num_filters_total])

            # Add dropout
            with tf.name_scope("dropout"):
                self.h_drop = tf.nn.dropout(self.h_pool_flat, self.dropout_keep_prob)

            # Final (unnormalized) scores and predictions
            with tf.name_scope("output"):
                W = tf.get_variable(
                    "W",
                    shape=[num_filters_total, num_classes])
                b = tf.Variable(tf.constant(0.1, shape=[num_classes]), name="b")
                l2_loss += tf.nn.l2_loss(W)
                l2_loss += tf.nn.l2_loss(b)
                self.scores = tf.nn.xw_plus_b(self.h_drop, W, b, name="scores")
                self.predictions = tf.argmax(self.scores, 1, name="predictions")

            # Calculate mean cross-entropy loss
            with tf.name_scope("loss"):
                losses = tf.nn.softmax_cross_entropy_with_logits(logits=self.scores, labels=self.input_y)
                self.loss = tf.reduce_mean(losses) + l2_reg_lambda * l2_loss

            # Accuracy
            with tf.name_scope("accuracy"):
                correct_predictions = tf.equal(self.predictions, tf.argmax(self.input_y, 1))
                self.accuracy = tf.reduce_mean(tf.cast(correct_predictions, "float"), name="accuracy")

    sess = tf.Session()
    with sess.as_default():
        cnn = TextCNN(
            sequence_length=training_x.shape[1],
            num_classes=training_y.shape[1],
            vocab_size=vocabulary_size,
            embedding_size=embedding_dim,
            filter_sizes=list(map(int, filter_sizes.split(","))),
            num_filters=num_filters,
            l2_reg_lambda=l2_reg_lambda)
        global_step = tf.Variable(0, name="global_step", trainable=False)
        optimizer = tf.train.AdamOptimizer(1e-3)
        grads_and_vars = optimizer.compute_gradients(cnn.loss)
        train_op = optimizer.apply_gradients(grads_and_vars, global_step=global_step)
        return sess, cnn, train_op, global_step
