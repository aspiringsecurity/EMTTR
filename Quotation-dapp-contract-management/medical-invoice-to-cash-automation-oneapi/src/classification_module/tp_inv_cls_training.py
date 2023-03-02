# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import pandas as pd
import numpy as np
import os
import logging
import tensorflow.compat.v1 as tf
import sys
import warnings
import nltk
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')
nltk.download('omw-1.4')


from tensorflow.python.framework import graph_util
import time
import tp_inv_cls_data_preprocessing as inv_data_preprocessor
import tp_inv_cls_cnn_network_creation as inv_cnn_network_creator
import argparse

parser = argparse.ArgumentParser()

parser.add_argument('-hp',
                    '--hyperparameter', default=0,
                    help='train with hyperparameter tuning or not (0|1)')

parser.add_argument('-l',
                    '--logfile',
                    type=str,
                    default='./logs/classification_model_training.txt',
                    help="log file to output benchmarking results to")
parser.add_argument('-p',
                    '--data_path',
                    required=True,
                    type=str,
                    help="Training dataset csv path")
parser.add_argument('-o',
                    '--model_output_path',
                    required=True,
                    type=str,
                    help="Model output path")

FLAGS = parser.parse_args()
hp_tuning_flag = int(FLAGS.hyperparameter)

if FLAGS.logfile == "":
     logging.basicConfig(level=logging.DEBUG)
else:
     logging.basicConfig(filename=FLAGS.logfile, level=logging.DEBUG)
logger = logging.getLogger()

if not sys.warnoptions:
    warnings.simplefilter("ignore")
tf.disable_v2_behavior()

train_dataset_path=FLAGS.data_path
model_save_path=FLAGS.model_output_path

if not os.path.exists(model_save_path):
    os.makedirs(model_save_path)

df_csv = pd.read_csv(train_dataset_path)


def print_logger(log_str):
    print(log_str)
    logging.info(log_str)


def model_saver(model_type_name):
    """
        Model save function.
        Will save the model graph as Frozen Graph.
        """
    model_path = os.path.join(model_save_path, model_type_name)
    if not os.path.exists(model_path):
        os.makedirs(model_path)
    # =====================Frozen Graph ==============================
    # This will only save the graph but the variables will not be saved.
    # You have to freeze your model first
    tf.train.write_graph(graph_or_graph_def=sess.graph_def, logdir=model_path,
                         name=model_type_name, as_text=True)

    # Freeze graph
    graph = tf.get_default_graph()
    input_graph_def = graph.as_graph_def()
    output_node_names = ['output/predictions']

    output_graph_def = graph_util.convert_variables_to_constants(sess, input_graph_def, output_node_names)
    # For some models, we would like to remove training nodes
    # output_graph_def = graph_util.remove_training_nodes(output_graph_def, protected_nodes=None)
    pb_filepath = os.path.join(model_path, model_type_name + '.pb')
    with tf.gfile.GFile(pb_filepath, 'wb') as f:
        f.write(output_graph_def.SerializeToString())
    # =====================Frozen Graph ==============================
    # To save model check points, meta data etc.,
    saver = tf.train.Saver()
    saver.save(sess, os.path.join(model_path, model_type_name))

# ============================ Data Preprocessing - Start ========================
# Data Preprocessing
print_logger('Data Preprocessing Started')
train_ratio = 0.7
df_text_cate, labels, cat_sample_max, vocabulary_size = inv_data_preprocessor.data_preprocessing_invoker(df_csv)
training_x, training_y, validation_x, validation_y, testing_x, testing_y = inv_data_preprocessor.split_dataset(
    df_text_cate, train_ratio)
print_logger('Data Preprocessing Completed')
# ============================ Data Preprocessing - End ==========================


# ============================ CNN - Network Creation - Start ====================
print_logger('CNN - Network Creation Started')
dropout_keep_prob = 0.5
sess, cnn, train_op, global_step = inv_cnn_network_creator.cnn_network_creation_invoker(training_x, training_y, vocabulary_size)
print_logger('CNN - Network Creation Completed')
# ============================ CNN - Network Creation - End ======================


def invoke_train(batch_size, i, train_epoch_time):

    off_1 = i * batch_size
    off_2 = i * batch_size + batch_size
    batch_x = training_x[off_1:off_2]
    batch_y = training_y[off_1:off_2]
    batch_x = np.asarray(batch_x)
    batch_y = np.asarray(batch_y)

    batch_val_x = np.asarray(validation_x)
    batch_val_y = np.asarray(validation_y)

    feed_dict_train = {
        cnn.input_x: batch_x,
        cnn.input_y: batch_y,
        cnn.dropout_keep_prob: dropout_keep_prob
    }

    feed_dict_valid = {
        cnn.input_x: batch_val_x,
        cnn.input_y: batch_val_y,
        cnn.dropout_keep_prob: 1.0
    }

    # ================== Training Part - Start ===================
    start_time = time.time()
    _, step, train_loss, train_accuracy = sess.run([train_op, global_step, cnn.loss, cnn.accuracy], feed_dict_train)
    train_epoch_time += time.time() - start_time
    # ================== Training Part - End ===================

    valid_loss, valid_accuracy = sess.run([cnn.loss, cnn.accuracy], feed_dict_valid)

    return train_loss, train_accuracy, valid_loss, valid_accuracy, train_epoch_time


def normal_training(batch_size, num_epochs, train_epoch_time):
    epoch, train_loss, train_accuracy, valid_loss, valid_accuracy = 0, 0, 0, 0, 0
    num_batches = len(training_x) / 64

    # Initialize all variables
    sess.run(tf.global_variables_initializer())

    for epoch in range(int(num_epochs)):
        for i in range(int(num_batches)):
            train_loss, train_accuracy, valid_loss, valid_accuracy, train_epoch_time = invoke_train(batch_size, i, train_epoch_time)

        print_logger("Epoch{}: train_loss {:g}, train_acc {:g}, valid_loss {:g}, valid_acc {:g}".format(epoch, train_loss,
                                                                                                 train_accuracy,
                                                                                                 valid_loss,
                                                                                                 valid_accuracy))
    return train_epoch_time


# =============================== Train without hyper parameter - Start ========================
if hp_tuning_flag == 0:
    print_logger("Training started for without hyper parameter tuning")
    batch_size = 64
    epoch = 40
    train_epoch_time = 0
    
    sess.run(tf.global_variables_initializer())
    print_logger("Running for Epoch: " + str(epoch) + " Batch Size: " + str(batch_size))
    train_epoch_time = normal_training(batch_size, epoch, train_epoch_time)
    print_logger("Training execution time (Without Hyper parameter): " + str(train_epoch_time))

    model_saver("tp_inv_cls_model_without_HP")
    print_logger("Model without hyper parameter saved successfully!!")
# =============================== Train without hyper parameter - End ==========================
# =============================== Train with hyper parameter - Start ===========================
elif hp_tuning_flag == 1:
    print_logger("Training with hyper parameter tuning")
    best_hp = ()
    train_epoch_time = 0
    best_accuracy = 0.9
    epochs_vals = [50, 80]
    batchs = [32, 64]

    epoch, train_loss, train_accuracy, valid_loss, valid_accuracy = 0, 0, 0, 0, 0

    sess.run(tf.global_variables_initializer())

    for epoch_val in epochs_vals:
        for epoch in range(int(epoch_val)):
            for batch_size in batchs:
                num_batches = len(training_x) / batch_size
                for i in range(int(num_batches)):
                    train_loss, train_accuracy, valid_loss, valid_accuracy, train_epoch_time = invoke_train(batch_size, i, train_epoch_time)
            print_logger("Epoch{}: train_loss {:g}, train_acc {:g}, valid_loss {:g}, valid_acc {:g}".format(epoch, train_loss,
                train_accuracy, valid_loss, valid_accuracy))

        if valid_accuracy > best_accuracy:
            model_saver("tp_inv_cls_model_with_HP")
            best_accuracy = valid_accuracy
            best_hp = (epoch, batch_size)
            print_logger("Best accuracy model saved successfully!!")
            print_logger("Best Accuracy --->" + str(best_accuracy))
            print_logger("Best hyper parameters ----> Epoch: " + str(best_hp[0]) + " Batch Size: " + str(best_hp[1]))

    print_logger("Training execution time (With Hyper parameter): " + str(train_epoch_time))

    print_logger("Training model with new best hyper parameters ----> Epoch: " + str(best_hp[0]) + " Batch Size: " + str(best_hp[1]))
    train_epoch_time = normal_training(best_hp[1], best_hp[0], train_epoch_time)

    model_saver('cls_model')
    print_logger("Model with hyper parameter retained model saved successfully!!")

    print_logger("Training execution time (Model retraining): " + str(train_epoch_time))

# =============================== Train with hyper parameter - End ===========================

feed_dict_test = {
    cnn.input_x: testing_x,
    cnn.input_y: testing_y,
    cnn.dropout_keep_prob: 1.0
}

accuracy = sess.run([cnn.accuracy], feed_dict_test)
print_logger("The final inference accuracy is: " + str(accuracy))



