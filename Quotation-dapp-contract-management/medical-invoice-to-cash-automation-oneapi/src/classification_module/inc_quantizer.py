# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import pandas as pd
import tensorflow.compat.v1 as tf
import logging
import argparse
from neural_compressor.experimental import Quantization,  common
from neural_compressor.metric import BaseMetric
import tp_inv_cls_data_preprocessing as inv_data_preprocessor
import numpy as np

def print_logger(log_str):
    logger = logging.getLogger(__name__)
    print(log_str)
    logger.info(log_str)

def quantize_model(test_dataset_path, model_saved_path, output_model_path):
    tf.compat.v1.disable_v2_behavior()
    df_csv = pd.read_csv(test_dataset_path)

    # ============================ Load Frozen graph - Start ========================
    print_logger('Loading model from Frozen graph')
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
    #import pdb;pdb.set_trace()
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

    feed_dict_test = {
        input_x: testing_x,
        dropout_keep_prob: 1.0
    }

    if testing_y.columns[0] in 'CLASS-1963':
        label_dict = {0: 'CLASS-1963', 1: 'CLASS-1250', 2: 'CLASS-1274', 3: 'CLASS-1522', 4: 'CLASS-1376',
                  5: 'CLASS-1758', 6: 'CLASS-2141', 7: 'CLASS-1249', 8: 'CLASS-1721', 9: 'CLASS-1828'}
    else:
        label_dict = {0: 'CL_001', 1: 'CL_003', 2: 'CL_005', 3: 'CL_006', 4: 'CL_004',
                  5: 'CL_007', 6: 'CL_008', 7: 'CL_002', 8: 'CL_0010', 9: 'CL_009'}
    
    class Dataset:
        """Creating Dataset class for getting Image and labels"""
        def __init__(self,data,input_tens,input_tens2):
            self.input_x= tf.convert_to_tensor(testing_x)
            self.labels = tf.convert_to_tensor(testing_y)
            self.data = data
            self.input_tensor = input_tens
            self.input_tensor2 = input_tens2
                        
        def __getitem__(self, index):
            return {"input_x":np.random.rand(40),"dropout_keep_prob":self.data[self.input_tensor2]}, [tf.slice(self.labels,[index,0],[1,-1])]
           
        def __len__(self):
            return 1# self.input_x.shape[0]
    
    # ============================ Quantize the model ========================
      
    dataset = Dataset(feed_dict_test,input_x,dropout_keep_prob)
    quantizer = Quantization("./src/classification_module/conf.yaml")
    print("Quantizing")
    quantizer.model = common.Model(model_saved_path) #'test_model.onnx'
    quantizer.calib_dataloader = common.DataLoader(dataset)
    q_model = quantizer.fit()
    q_model.save(output_model_path)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument('-p',
                        '--test_dataset_path',
                        help='test file path')
    parser.add_argument('-m',
                        '--fp32_model_path',
                        help='FP32 model path to load')
    parser.add_argument('-o',
                        '--output_model_path',
                        help='Int8 model path to load')
    parser.add_argument('-l',
                        '--logfile',
                        type=str,
                        default='log.txt',
                        help="log file to output benchmarking results to")

    FLAGS = parser.parse_args()
    fp32_model_path = str(FLAGS.fp32_model_path)
    test_dataset_path = str(FLAGS.test_dataset_path)
    output_model_path = str(FLAGS.output_model_path)
  
    if FLAGS.logfile == "":
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(filename=FLAGS.logfile, level=logging.DEBUG)
    logger = logging.getLogger()

    quantize_model(test_dataset_path, fp32_model_path, output_model_path)

