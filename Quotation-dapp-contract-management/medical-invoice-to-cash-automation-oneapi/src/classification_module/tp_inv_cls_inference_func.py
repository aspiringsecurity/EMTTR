# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import os.path
import pandas as pd
import tensorflow.compat.v1 as tf
import tp_inv_cls_data_preprocessing as inv_data_preprocessor
import time
import logging


def print_logger(log_str):
    logger = logging.getLogger(__name__)
    print(log_str)
    logger.info(log_str)


def invoke_inv_classification_inference(test_dataset_path, model_saved_path, inference_for_single_batch, inference_count):

    tf.compat.v1.disable_v2_behavior()
    df_csv = pd.read_csv(test_dataset_path)
    inference_count = -inference_count

    # ============================ Load Frozen graph - Start ========================
    print_logger('Loading model from Frozen graph')
    with tf.gfile.GFile(os.path.join(model_saved_path, os.path.basename(model_saved_path) + '.pb'), "rb") as f:
        graph_def = tf.GraphDef()
        graph_def.ParseFromString(f.read())

    with tf.Graph().as_default() as graph:
        tf.import_graph_def(graph_def,
                            input_map=None,
                            return_elements=None,
                            name="")
    sess = tf.Session(graph=graph)
    # ============================ Load Frozen graph - End ========================

    input_x = graph.get_tensor_by_name("input_x:0")
    dropout_keep_prob = graph.get_tensor_by_name("dropout_keep_prob:0")
    prediction_variable_on_graph = graph.get_tensor_by_name("output/predictions:0")
    vocabulary_size_from_graph = graph.get_tensor_by_name("embedding/W:0")
    vocabulary_size_built = vocabulary_size_from_graph.shape.dims[0].value

    # =========================== Data Preprocessing - Start ========================
    print_logger('Data Preprocessing Started')
    train_ratio = 0.7
    df_text_cate, labels, cat_sample_max, vocabulary_size = inv_data_preprocessor.data_preprocessing_invoker(df_csv, vocabulary_size_built)
    training_x, training_y, validation_x, validation_y, testing_x, testing_y = inv_data_preprocessor.split_dataset(df_text_cate, train_ratio)
    print_logger('Data Preprocessing Completed')
    # ============================ Data Preprocessing - End ========================

    if inference_for_single_batch == 1:
        feed_dict_test = {
            input_x: testing_x,
            dropout_keep_prob: 1.0
        }
        print_logger("Running batch inference")
    else:
        feed_dict_test = {
            input_x: testing_x.iloc[inference_count:],
            dropout_keep_prob: 1.0
        }
        print_logger("Running single inference")

    if testing_y.columns[0] in 'CLASS-1963':
        label_dict = {0: 'CLASS-1963', 1: 'CLASS-1250', 2: 'CLASS-1274', 3: 'CLASS-1522', 4: 'CLASS-1376',
                  5: 'CLASS-1758', 6: 'CLASS-2141', 7: 'CLASS-1249', 8: 'CLASS-1721', 9: 'CLASS-1828'}
    else:
        label_dict = {0: 'CL_001', 1: 'CL_003', 2: 'CL_005', 3: 'CL_006', 4: 'CL_004',
                  5: 'CL_007', 6: 'CL_008', 7: 'CL_002', 8: 'CL_0010', 9: 'CL_009'}

    start_time = time.time()
    predicted_labels = sess.run([prediction_variable_on_graph], feed_dict_test)
    end_time = time.time() - start_time

    for idx, label in enumerate(list(predicted_labels[0])):
        print(str(idx + 1) + '. ' + str(label_dict.get(int(label))))

    # =========== Reversing One Hot encoding labels ============
    y_labels = pd.get_dummies(testing_y.iloc[inference_count:]).idxmax(1)
    y_labels_list = []
    for label in y_labels:
        y_labels_list.append(list(label_dict.keys())[list(label_dict.values()).index(label)])
    # =========== Reversing One Hot encoding labels ============

    # ========== Accuracy calculation using TF ==========
    g = tf.Graph()
    with g.as_default():
        acc = tf.compat.v1.metrics.accuracy(y_labels_list, list(predicted_labels[0]))
        global_init = tf.compat.v1.global_variables_initializer()
        local_init = tf.compat.v1.local_variables_initializer()
    sess = tf.compat.v1.Session(graph=g)
    sess.run([global_init, local_init])
    print_logger("Predicted accuracy is:" + str(sess.run([acc])))
    # ========== Accuracy calculation using TF ==========

    print_logger("Inference Execution time : " + str(end_time))

