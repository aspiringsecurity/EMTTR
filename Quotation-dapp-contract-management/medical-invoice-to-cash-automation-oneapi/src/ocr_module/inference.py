# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
# pylint: disable=missing-module-docstring
# pylint: disable=useless-import-alias
# pylint: disable=import-error
import argparse
import string
import time
import numpy as np
import cv2
from PIL import Image
import torch
from torch.autograd import Variable
import torch.nn.functional as F
import crnn as crnn
import config
import mydataset
import keys
import utils
import crnn
import logging
import os 



alphabet = keys.alphabet_v2
converter = utils.strLabelConverter(alphabet.copy())

#assert test_dataset
sampler = None
images_list = []
annot_list = []
cntr = 0


with open(config.val_infofile, encoding="utf-8") as f:
    content = f.readlines()
    num_all = 0
    num_correct = 0
    for line in content:
        if '\t' in line:
            fname, label = line.split('\t')
        else:
            fname, label = line.split('g:')
            fname += 'g'
        label = label.replace('\r', '').replace('\n', '')
        img = cv2.imread(fname)
        w = 282
        h = 32
        imgH = config.imgH
        h, w = img.shape[:2]
        imgW = imgH * w//h
        transformer = mydataset.resizeNormalize((282, 32), is_test=True)
        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        img = Image.fromarray(np.uint8(img)).convert('L')
        img = transformer(img)
        img = img.view(1, *img.size())
        img = Variable(img)
        if cntr < 1:
            image = img
        else:
            image = torch.cat((image, img), 0)
        images_list.append(img)
        annot_list.append(label)
        cntr = cntr + 1

def evaluate_model(model):
    '''This module evaluates the model performance of the model and calculates the accuracy
       @input params: model
       @output :accuracy
    '''
    number_of_correct = 0
    total = len(images_list)
    for i in range(0, len(images_list)):
        sim_pred =""
        preds = model(images_list[i])
        preds = F.log_softmax(preds, 2)
        conf, preds = preds.max(2)
        #print(conf)
        preds = preds.transpose(1, 0).contiguous().view(-1)
        preds_size = Variable(torch.IntTensor([preds.size(0)]))
        sim_pred = converter.decode(preds.data, preds_size.data, raw=False)
        annot_list[i] = annot_list[i].replace('"', ' ')
        sim_pred = sim_pred.replace('"', ' ')
        print("predicted:", sim_pred)
        print("targeted:", annot_list[i])
        if sim_pred.strip() == annot_list[i].strip():
            number_of_correct = number_of_correct+1
    accuracy = number_of_correct/total
    return accuracy

if __name__ == "__main__":
    # Parameters
    parser = argparse.ArgumentParser()
    parser.add_argument('-i',
                        '--intel',
                        type=int,
                        required=True,
                        help='use 1 for enabling intel pytorch optimizations, default is 0')
                        
    parser.add_argument('-m',
                        '--model_path',
                        type=str,
                        required=True,
                        help='path for ocr model')                    

                        
    FLAGS = parser.parse_args()
    intel_flag = FLAGS.intel
    model_path= FLAGS.model_path
    crnn = crnn.CRNN(config.imgH, config.nc, config.nclass, config.nh)
    crnn.load_state_dict(torch.load(model_path, map_location="cpu"))
    crnn.eval()

    if intel_flag:
        import intel_extension_for_pytorch as ipex  # pylint: disable=E0401
        crnn = ipex.optimize(crnn)
        log_filename = os.path.join('./log/', 'Intel_OCR_Inference' + '.txt')
        logging.basicConfig(filename=log_filename, level=logging.DEBUG,force=True,filemode='w')
        logger = logging.getLogger()
        print("Intel Pytorch Optimizations has been Enabled!")
        logger.debug("Intel Pytorch Optimizations has been Enabled!")
    else:
        log_filename = os.path.join('./log/', 'Stock_OCR_Inference' + '.txt')
        logging.basicConfig(filename=log_filename, level=logging.DEBUG,force=True,filemode='w')
        logger = logging.getLogger()
        device = torch.device('cpu')
    print(image.shape)
    time2 = 0
    for i in range(20):
        start = time.time()
        crnn(image)
        time2 += time.time()-start
        print("Batch inference time --> ", time.time()-start)
        logger.debug("Batch inference time --> :  %f" % (time.time()-start))
    #Evaluate model accuracy
    accuracy = evaluate_model(crnn)
    print("Accuracy is --->",accuracy)
    logger.debug("Accuracy is ---> :  %f"% (accuracy))
    print("Average Batch Inference time taken for in seconds ---> ", time2/20)
    logger.debug("Average Batch Inference time taken for in seconds : %f" % (time2/20))
