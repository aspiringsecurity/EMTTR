# pylint: disable=missing-module-docstring
# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import argparse
import string
import numpy as np
import cv2
from PIL import Image
import torch
from torch.autograd import Variable
from neural_compressor.experimental import Quantization, common 
import crnn
import config
import mydataset


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

class Dataset:
    """Creating Dataset class for getting Image and labels"""
    def __init__(self):
        test_images, test_labels = image, annot_list
        self.test_images = test_images
        self.labels = test_labels

    def __getitem__(self, index):
        return self.test_images[index], self.labels[index]

    def __len__(self):
        return len(self.test_images)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    
    parser.add_argument('-m',
                        '--model_path_fp32',
                        type=str,
                        required=True,
                        help='path for fp32 model')
  
    parser.add_argument('-o',
                        '--output_path',
                        type=str,
                        required=True,
                        help='output path to save quantized model')
    
    FLAGS = parser.parse_args()
    model_path = FLAGS.model_path_fp32
    config_path =config.yml_quantized
    
    crnn = crnn.CRNN(config.imgH, config.nc, config.nclass, config.nh).to("cpu")
    crnn.load_state_dict(torch.load(model_path, map_location="cpu"))
    dataset = Dataset()
    quantizer = Quantization(config_path)
    quantizer.model = common.Model(crnn)
    quantizer.calib_dataloader = common.DataLoader(dataset)
    q_model = quantizer.fit()
    q_model.save(FLAGS.output_path)
