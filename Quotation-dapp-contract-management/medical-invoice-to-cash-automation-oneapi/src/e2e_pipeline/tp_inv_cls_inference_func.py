# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import os.path
import pandas as pd
import tensorflow.compat.v1 as tf
import tp_inv_cls_data_preprocessing as inv_data_preprocessor
import time
import logging
import config

cat_discount_mapping = config.cat_discount_map
categories = []
discs=[]

#Reading Catagory-Discount mapping file
with open(cat_discount_mapping, encoding="utf-8") as f:
    content = f.readlines()
    i = 0
    for line in content:
        if i==0:
            i=i+1
            continue
        if '\t' in line:
            name, discount = line.split('\t')
        else:
            name= line
            discount = ""
        categories.append(name.strip())
        discs.append(discount.strip())

print(categories)
print(discs)

def print_logger(log_str):
    logger = logging.getLogger(__name__)
    print(log_str)
    logger.info(log_str)

def isfloat(num):
    try:
        float(num)
        return True
    except ValueError:
        return False

#Validate claims for the given invoice based on catagory & discount value
def validate_claim(claim_category, inv_discount):
    print(inv_discount)
    if inv_discount == "":
        return False
    if isfloat(inv_discount):
        VALID_CAT=claim_category in categories

        if VALID_CAT:
            index = categories.index(claim_category)
            cat_discount = float(discs[index])
            if float(inv_discount) == cat_discount:
                print("Invoice Discount Amount is matching with Catagory discout amount")
                return True
            else:
                return False
        else:
            return False
    else:
        return False

def invoke_inv_classification_inference(test_dataset_path, model_saved_path, inference_for_single_batch, inference_count, quantized_model=False):

    tf.compat.v1.disable_v2_behavior()
    df_csv = pd.read_csv(test_dataset_path)
    inference_count = -inference_count

    # ============================ Load Frozen graph - Start ========================
    print_logger('Loading model from Frozen graph')
    #with tf.gfile.GFile(os.path.join(model_saved_path, os.path.basename(model_saved_path)), "rb") as f:
    with tf.gfile.GFile(model_saved_path, "rb") as f:
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
    df_text_cate, labels, vocabulary_size = inv_data_preprocessor.data_preprocessing_invoker(df_csv, vocabulary_size_built)
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
    #warmup runs
    #if quantized_model==True:
    #    for i in range(0,5):
    #        sess.run([prediction_variable_on_graph], feed_dict_test)

    start_time = time.time()
    predicted_labels = sess.run([prediction_variable_on_graph], feed_dict_test)
    end_time = time.time() - start_time
    predicted_cats = []
    for idx, label in enumerate(list(predicted_labels[0])):
        print(categories[int(label)])
        predicted_cats.append(categories[int(label)])
    
    print("Predicted claim catagory is: "+predicted_cats[0])
    inv_dsc_amt = df_csv.iloc[inference_count:]["DiscountAmt"].tolist()
    
    for claim in inv_dsc_amt:
        is_valid_claim = validate_claim(predicted_cats[0], claim)
        print('Claim is Valid: '+str(is_valid_claim))
    print_logger("Inference Execution time for classification model: " + str(end_time))

    return end_time
