# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import pandas as pd
import tensorflow.compat.v1 as tf
import logging
import tp_inv_cls_inference_func as inv_cls_inference_func

import argparse

parser = argparse.ArgumentParser()

parser.add_argument('-t',
                    '--inference_type',
                    default=0,
                    help='inference run for single or batch (0|1)')
parser.add_argument('-p',
                    '--test_file_path',
                    default=r'./data/classification_module/Train_10k.csv',
                    help='test file path')
parser.add_argument('-m',
                    '--model_path',
                    default=r'./models/classification_module/sept29_tf28/tp_inv_cls_model_without_HP',
                    help='saved model path')
parser.add_argument('-c',
                    '--inference_count',
                    default=1,
                    help='inference run for single or batch (0|1)')
parser.add_argument('-l',
                    '--logfile',
                    type=str,
                    default='/home/azureuser/Validation/TradePromo_220922/log/sept29_tf28.txt',
                    help="log file to output benchmarking results to")

FLAGS = parser.parse_args()
single_or_batch_inference = int(FLAGS.inference_type)
model_saved_path = str(FLAGS.model_path)
test_dataset_path = str(FLAGS.test_file_path)
inference_count = int(FLAGS.inference_count)

if FLAGS.logfile == "":
    logging.basicConfig(level=logging.DEBUG)
else:
    logging.basicConfig(filename=FLAGS.logfile, level=logging.DEBUG)
logger = logging.getLogger()

tf.compat.v1.disable_v2_behavior()
sess = tf.Session()

df_csv = pd.read_csv(test_dataset_path)

inv_cls_inference_func.invoke_inv_classification_inference(test_dataset_path,
                                                           model_saved_path,
                                                           single_or_batch_inference, inference_count)



