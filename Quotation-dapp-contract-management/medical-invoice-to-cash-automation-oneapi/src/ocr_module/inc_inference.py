# pylint: disable=missing-docstring
# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import argparse
import string
import time
import torch
import numpy as np
import cv2
from PIL import Image
from neural_compressor.utils.pytorch import load 
import intel_extension_for_pytorch as ipex 
from torch.autograd import Variable
import mydataset
import config
import crnn

sampler = None
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
        imgW = imgH * w // h
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
        annot_list.append(label)
        cntr = cntr + 1

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-m',
                        '--model_path_fp32',
                        type=str,
                        required=True,
                        help='path for fp32 model')
   
    parser.add_argument('-q',
                        '--model_path_int8',
                        type=str,
                        required=True,
                        help='path for int8 model')
                        
    FLAGS = parser.parse_args()
    model_path = FLAGS.model_path_fp32
    crnn = crnn.CRNN(config.imgH, config.nc, config.nclass, config.nh).to("cpu")
    crnn.load_state_dict(torch.load(model_path, map_location="cpu"))
    quantized_model_path = FLAGS.model_path_int8
    int8_model = load(quantized_model_path, crnn)
    print(image.shape)
    
    #creating dataset time analysis
    crnn.eval()
    int8_model .eval()
    crnn = ipex.optimize(crnn)
    int8_model = ipex.optimize(int8_model)
    time2, time1 = 0, 0
    for i in range(20):
        start = time.time()
        crnn(image)
        time2 += time.time()-start
        print("FP32Model --> ", time.time()-start)
        start1 = time.time()
        int8_model(image)
        time1 += time.time()-start1
        print("INT8Model --> ", time.time()-start1)
    print("Average Inference time taken for FP32 Model in seconds ---> ", time2/20)
    print("Average Inference time taken for INT8 Model in seconds ---> ", time1/20)
