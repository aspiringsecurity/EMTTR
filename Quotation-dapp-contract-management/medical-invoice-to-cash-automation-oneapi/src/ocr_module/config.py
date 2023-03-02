# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause

# pylint: disable=missing-docstring
import keys
train_infofile_fullimg = ''
alphabet = keys.alphabet
alphabet_v2 = keys.alphabet_v2
workers = 4
batchSize = 50
imgH = 32
imgW = 280
nc = 1
nclass = len(alphabet)+1
nh = 256
niter = 100
lr = 0.0003
beta1 = 0.5
cuda = True
ngpu = 1
pretrained_model = ''
saved_model_dir = 'models/ocr_module/'
saved_model_prefix = 'CRNN-'
use_log = False
remove_blank = False
experiment = None
displayInterval = 500
n_test_disp = 10
valInterval = 500
saveInterval = 500
adam = False
adadelta = False
keep_ratio = False
random_sample = True
pretrained_model = 'models/ocr_module/CRNN-1010.pth'
intel_model='./models/ocr_module/intel_pt_whp/CRNN-1010_HP.pth'
stock_model='./models/ocr_module/stock_pt_whp/CRNN-1010_HP.pth'
train_infofile = './data/ocr_module/train.txt'
val_infofile = './data/ocr_module/test.txt'
yml_quantized='./src/ocr_module/conf.yaml'
intel_fp32_model='./models/ocr_module/CRNN-1010_HP.pth'
int8_model="./output_ocr_quantized/best_model.pt"
