# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
# pylint: disable=missing-module-docstring
# pylint: disable=useless-import-alias
# pylint: disable=import-error
import argparse
import time
import numpy as np
import cv2
from PIL import Image
import torch
from torch.autograd import Variable
import torch.nn.functional as F
import config
import mydataset
import keys
import utils
import crnn
import pandas as pd
import easyocr
import cv2
import tp_inv_cls_inference_func as inv_cls_inference_func
import os
import logging
from datetime import datetime
import entity_extractor
import glob

temp_test_file=config.test
temp_images_files=config.croppedimg
tempCSVFile=config.temp_csv_file

dataset=config.dataset
alphabet = keys.alphabet_v2
converter = utils.strLabelConverter(alphabet.copy())

temp_input_image=None
temp_images_list = []
temp_bbox_list = []
date=datetime.now().strftime("%Y_%m_%d-%I-%M-%S")
df_train=pd.read_csv(dataset)

reader = easyocr.Reader(['en'],gpu=False)

#Deleting Existing TrainNew Csv File from data folder
if os.path.exists(tempCSVFile):
    os.remove(tempCSVFile)
    print("%s file has been deleted",tempCSVFile)
else:
    print("Existing TrainNew Csv File not present")

#Deleting Existing images from croppedImage folder
def removeCroppedImage():
    for file in os.listdir(temp_images_files):
        imgFile=os.path.join(temp_images_files,file)
        if os.path.exists(imgFile):
            os.remove(imgFile)


def processCroppedImages():
    ''' Reading Images by taking path from test txt file
        '''
    cntr = 0
    global temp_input_image
    global temp_images_list

    with open(temp_test_file, encoding="utf-8") as f:
        content = f.readlines()
        for line in content:
            #try:
            if '\t' in line:
                fname, label = line.split('\t')
            else:
                fname= line
                label = ""
            
            #label = label.replace('\r', '').replace('\n', '')
            #print(fname)
            img = cv2.imread(fname.strip())
            imgW = int(config.imgW)
            imgH = int(config.imgH)
            transformer = mydataset.resizeNormalize((imgW, imgH), is_test=True)
            img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
            img = Image.fromarray(np.uint8(img)).convert('L')
            img = transformer(img)
            img = img.view(1, *img.size())
            img = Variable(img)
            if cntr < 1:
                temp_input_image = img
            else:
                temp_input_image = torch.cat((temp_input_image, img), 0)
            temp_images_list.append(img)
            #temp_annot_list.append(label)
            cntr = cntr + 1
            # except:
            #     cntr = cntr + 1
            #     continue

#Run Text Detection CRNN model and predict text on cropped images
def recognize_text(model, gt_text):
    '''This module runs the trained text recognition CRNN model on cropped images and predict the text
       @input params: model
       @output :text
    '''
    word =""
    number_of_correct = 0
    total = len(temp_images_list)
    total_time = 0
    for i in range(0, len(temp_images_list)):
        sim_pred =""
        start_time = time.time()
        preds = model(temp_images_list[i])
        pred_time = time.time()-start_time
        total_time+=pred_time
        preds = F.log_softmax(preds, 2)
        conf, preds = preds.max(2)
        preds = preds.transpose(1, 0).contiguous().view(-1)
        preds_size = Variable(torch.IntTensor([preds.size(0)]))
        sim_pred = converter.decode(preds.data, preds_size.data, raw=False)
        #temp_annot_list[i] = temp_annot_list[i].replace('"', ' ')
        sim_pred = sim_pred.replace('"', ' ')
        word=word+" "+sim_pred.strip()
        # if sim_pred.strip() == temp_annot_list[i].strip():
        #     number_of_correct = number_of_correct+1
    words=word.strip()
    print(words)
    wer_metrics = wer(gt_text.strip(),words)
    print("WER is: "+str(wer_metrics))
    #accuracy = number_of_correct/total
    return words, total_time

def wer(r, h):
    """
    Calculation of WER with Levenshtein distance.

    Works only for iterables up to 254 elements (uint8).
    O(nm) time ans space complexity.
    """
    d = np.zeros((len(r) + 1) * (len(h) + 1), dtype=np.uint8)
    d = d.reshape((len(r) + 1, len(h) + 1))
    for i in range(len(r) + 1):
        for j in range(len(h) + 1):
            if i == 0:
                d[0][j] = j
            elif j == 0:
                d[i][0] = i

    # computation
    for i in range(1, len(r) + 1):
        for j in range(1, len(h) + 1):
            if r[i - 1] == h[j - 1]:
                d[i][j] = d[i - 1][j - 1]
            else:
                substitution = d[i - 1][j - 1] + 1
                insertion = d[i][j - 1] + 1
                deletion = d[i - 1][j] + 1
                d[i][j] = min(substitution, insertion, deletion)

    return d[len(r)][len(h)]

#Run Text Detection CRNN model and predict text on cropped images
def recognize_text_batch(model, gt_text):
    '''This module runs the trained text recognition CRNN model on cropped images and predict the text
       @input params: model
       @output :text
    '''
    OverallTime = 0
    j = 0
    print("predicting from text recognition model")
    for j in range(2):
        start=time.time()
        preds = model(temp_input_image)
        OverallTime+=time.time()-start

    #Postprocess the predictions to decode text
    preds = F.log_softmax(preds, 2)
    conf, preds = preds.max(2)
    preds = preds.transpose(1, 0).contiguous().view(-1)
    preds_size = Variable(torch.IntTensor([preds.size(0)]))
    sim_pred = converter.decode(preds.data, preds_size.data, raw=False)
    sim_pred = sim_pred.replace('"', ' ')

    print(sim_pred)
    wer_metrics = wer(gt_text.strip(),sim_pred.strip())
    print("WER is: "+str(wer_metrics))
    #print("OCR Average Inference time taken for 1 Invoice image is {} ".format(OverallTime / 2))
    return sim_pred.strip(), OverallTime/2

def load_model(model_path, intel_flag=False):
    crnn_model = crnn.CRNN(config.imgH, config.nc, config.nclass, config.nh)
    print(crnn_model)
    crnn_model.load_state_dict(torch.load(model_path, map_location="cpu"))
    crnn_model.eval()

    if intel_flag:
        import intel_extension_for_pytorch as ipex
        crnn_model = ipex.optimize(crnn_model)
        log_filename = os.path.join('./logs/', 'Intel_E2E_inference_'+str(date)+ '.txt')
        logging.basicConfig(filename=log_filename, level=logging.DEBUG,force=True,filemode='w')
        logger = logging.getLogger()
        print("Intel Pytorch Optimizations has been Enabled!")
        logger.debug("Intel Pytorch Optimizations has been Enabled!")
    else:
        log_filename = os.path.join('./logs/', 'Stock_E2E_inference_'+str(date)+ '.txt')
        logging.basicConfig(filename=log_filename, level=logging.DEBUG,force=True,filemode='w')
        logger = logging.getLogger()
        device = torch.device('cpu')

    return crnn_model

#Classify Invoice using classification model
def classifyInvoice(tempCSVFile,cls_model, num_files):
    cls_time=inv_cls_inference_func.invoke_inv_classification_inference(tempCSVFile,cls_model,
                                             0,batch_size,quantized_model=False)
    return cls_time

#Process Input Invoice document images
def processInvoices(input_invoices_path, batch_size, crnn_model, cls_model):
    global temp_images_list
    global temp_input_image
    #global temp_annot_list
    total_time_invoices = 0
    total_time_invoices_ocr = 0

    invoices = glob.glob(input_invoices_path+"/*.png")
    invoices = invoices[:batch_size]

    OverallTime=0
    for img_path in invoices:
        #img_path, category=file.split('\t')
        #img_path = os.path.join(input_invoices_path,file)
        print(img_path)
        temp_input_image = None
        temp_images_list = []
        temp_bbox_list = []
        
        if os.path.exists(temp_test_file):
            os.remove(temp_test_file)

        if os.path.exists(temp_images_files):
            removeCroppedImage()

        #Crop text ROI images from input invoice document image
        bboxes = cropImages(str(img_path.strip()))

        #Process all the cropped images
        processCroppedImages()

        gt_path = str(img_path.strip()).replace("img_","gt_ocr_")
        gt_path = gt_path.replace(".png",".txt")
        print(gt_path)
        with open(gt_path,'r') as f:
            gt_text = f.read()
        f.close()
        print(gt_text)
        #Run Text Recognition model on cropped images and predict text
        words, total_ocr_time = recognize_text_batch(crnn_model, gt_text)
        print('Time taken for OCR :',total_ocr_time)

        #identify entities from from model predictions
        entities = entity_extractor.extract(words, bboxes)

        gt_cls_path = str(img_path.strip()).replace("img_","gt_cls_")
        gt_cls_path = gt_cls_path.replace(".png",".txt")
        print(gt_cls_path)
        with open(gt_cls_path,'r') as f:
            gt_cat = f.read()
        f.close()

        print(gt_cat)
        entities.append(gt_cat.strip())
        print(entities)

        df_train.loc[len(df_train)]=entities
        df_train.to_csv(tempCSVFile,index=False)
        
        total_time_invoices_ocr+=total_ocr_time

    #Classify Invoice using classification model
    cls_time = classifyInvoice(tempCSVFile,cls_model, batch_size)

    total_time_invoices=total_time_invoices_ocr+cls_time

    print('Total time taken by e2e pipeline for all the invoices :',total_time_invoices)
    print('Total OCR time taken by e2e pipeline for all the invoices :',total_time_invoices_ocr)
    print('Total CLassification time taken by e2e pipeline for all the invoices :',cls_time)

#Run text detection model to predict text ROIS & Crop text ROIs from input Invoice images
def cropImages(image_path):
    ''' Cropping text part from Invoice and
             saving it in croppedImage folder,and also writing the image path in
             temp test text file '''
    #Run easyOCR text detection model to predict text ROIs
    global temp_bbox_list

    img = cv2.imread(image_path)
    bbox,free_pts = reader.detect(img, width_ths=0.5, min_size=5)
    os.makedirs(temp_images_files, exist_ok=True)
    index = 0
    temp_bbox_list = []
    for roi in (bbox[0]):
        temp_bbox_list.append(roi)
        crop_filename = temp_images_files+str(index) + ".png"
        index+=1
        x_min,x_max,y_min,y_max = roi
        cropped_image = img[y_min:y_max, x_min:x_max]
        cv2.imwrite(crop_filename, cropped_image)

        with open(temp_test_file,'a') as f:
            f.write(crop_filename)
            f.write('\n')
    return temp_bbox_list

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i',
                        '--intel',
                        type=int,
                        required=False,
                        default=0,
                        help='use 1 for enabling intel pytorch optimizations, default is 0')
                        
    parser.add_argument('-m',
                        '--ocr_model_path',
                        type=str,
                        required=True,
                        help='path for ocr model')
                        
    parser.add_argument('-l',
                        '--cls_model_path',
                        type=str,
                        required=True,
                        help='path for classification model')

    parser.add_argument('-p',
                        '--test_data_path',
                        type=str,
                        required=True,
                        help='Test data path')            

    parser.add_argument('-b',
                        '--batch_size',
                        type=int,
                        required=True,
                        help='Batch Size')        

    FLAGS = parser.parse_args()
    intel_flag = FLAGS.intel
    model_path=FLAGS.ocr_model_path
    classification_model=FLAGS.cls_model_path
    test_data_path = FLAGS.test_data_path
    batch_size = FLAGS.batch_size
    
    #Load CRNN model
    crnn_model = load_model(model_path, intel_flag)

    #Process Invoices
    processInvoices(test_data_path, batch_size,crnn_model, classification_model)

    
