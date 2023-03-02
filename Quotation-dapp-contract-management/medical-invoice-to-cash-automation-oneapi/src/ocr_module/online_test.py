# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
# pylint: disable=missing-docstring
# pylint: disable=consider-using-with
from PIL import Image
import numpy as np
import cv2
import torch.nn.functional as F
import torch
from torch.autograd import Variable
import utils  # pylint: disable=no-name-in-module
import keys
import config
import mydataset
alphabet = keys.alphabet_v2
converter = utils.strLabelConverter(alphabet.copy())

def val_model(infofile, model, gpu, log_file='0625.log'):
    '''This module validates the trained model '''
    with open(infofile, encoding="utf-8") as f:
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
            res = val_on_image(img, model, gpu)
            res = res.strip()
            label = label.strip()
            label = label.replace('"', '')
            if res == label:
                num_correct += 1
            #else:
                #print('filename:{}\npred  :{}\ntarget:{}'.format(fname, res, label))
            num_all += 1
    f.close()
    return num_correct, num_all

def val_on_image(img, model, gpu):
    '''This modele performs the validation on single image'''
    imgH = config.imgH
    h, w = img.shape[:2]
    imgW = imgH * w // h

    transformer = mydataset.resizeNormalize((imgW, imgH), is_test=True)
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    image = Image.fromarray(np.uint8(img)).convert('L')
    image = transformer(image)
    if gpu:
        image = image.cuda()
    image = image.view(1, *image.size())
    image = Variable(image)

    model.eval()
    preds = model(image)

    preds = F.log_softmax(preds, 2)
    conf, preds = preds.max(2)
    #print(conf)
    preds = preds.transpose(1, 0).contiguous().view(-1)
    preds_size = Variable(torch.IntTensor([preds.size(0)]))
    # raw_pred = converter.decode(preds.data, preds_size.data, raw=True)
    sim_pred = converter.decode(preds.data, preds_size.data, raw=False)
    return sim_pred

# if __name__ == '__main__':
#     import sys
#     model_path = './crnn_models/CRNN-0627-crop_48_901.pth'
#     gpu = True
#     if not torch.cuda.is_available():
#         gpu = False

#     model = crnn.CRNN(config.imgH, 1, len(alphabet) + 1, 256)
#     if gpu:
#         model = model.cuda()
#     print('loading pretrained model from %s' % model_path)
#     if gpu:
#         model.load_state_dict(torch.load(model_path))
#     else:
#         model.load_state_dict(torch.load(model_path, map_location = lambda storage, loc: storage))

#     if len(sys.argv) > 1 and 'train' in sys.argv[1]:
#         infofile = 'data_set/infofile_updated_0627_train.txt'
#         print(val_model(infofile, model, gpu, '0627_train.log'))
#     elif len(sys.argv)>1 and 'gen' in sys.argv[1]:
#         infofile = 'data_set/infofile_0627_gen_test.txt'
#         print(val_model(infofile, model, gpu, '0627_gen.log'))
#     else:
#         infofile = 'data_set/infofile_updated_0627_test.txt'
#         print(val_model(infofile, model, gpu, '0627_test.log'))
