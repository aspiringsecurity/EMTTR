# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause

# pylint: disable=missing-docstring
import argparse
import pandas as pd
import numpy as np
import cv2
import os
import time
import classification_data_generator as cdg

number_of_crops = 0

def process_crop(template_image, crop_filename, xmin, ymax, text_width, text_height, label):
    global number_of_crops
    tf = open(os.path.join(ocr_dir,'test.txt'),'a')
    tt =  open(os.path.join(ocr_dir,'train.txt'),'a')

    if number_of_crops<=100: # generate 100 test image path
        tf.write(crop_filename+"\t"+label)
        tf.write('\n')
        tf.close()
    else:
        tt.write(crop_filename+"\t"+label)
        tt.write('\n')
        tt.close()

    img_crop = template_image[ymax-text_height:ymax+6,xmin-2:xmin+text_width+2]
    cv2.imwrite(crop_filename, img_crop)
    number_of_crops = number_of_crops+1

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
                        
    parser.add_argument('-i',
                        '--input_csv_path',
                        type=str,
                        required=True,
                        help='path for input csv')

    parser.add_argument('-d',
                        '--output_dir_path',
                        type=str,
                        required=True,
                        help='path for output csv')

    parser.add_argument('-n',
                        '--number_of_invoices',
                        type=int,
                        default=1000,
                        help='number of invoices to be generated')    

    parser.add_argument('-pc',
                        '--per_class_records',
                        type=int,
                        default=1000,
                        help='per class records')
    
    parser.add_argument('-t',
                        '--total_cls_records',
                        type=int,
                        default=10000,
                        help='total records')
                    
    FLAGS = parser.parse_args()
    input_csv_path = FLAGS.input_csv_path
    output_dir_path=FLAGS.output_dir_path
    number_of_invoices=FLAGS.number_of_invoices
    total_cls_records = FLAGS.total_cls_records
    per_class_records = FLAGS.per_class_records

    os.makedirs(output_dir_path,exist_ok=True)

    e2e_pipeline_dir = os.path.join(output_dir_path,'e2e_pipeline')
    ocr_dir = os.path.join(output_dir_path,'ocr_module')
    cls_dir = os.path.join(output_dir_path,'classification_module')

    os.makedirs(e2e_pipeline_dir,exist_ok=True)
    os.makedirs(ocr_dir,exist_ok=True)
    os.makedirs(cls_dir,exist_ok=True)

    invoice_image_dir = os.path.join(e2e_pipeline_dir,'invoice_dataset')
    os.makedirs(invoice_image_dir,exist_ok=True)
    ocr_dataset = os.path.join(ocr_dir,'ocr_dataset')
    os.makedirs(ocr_dataset,exist_ok=True)
    
    #Generate Classification Data
    cls_csv_path = os.path.join(cls_dir,'classification_data.csv')
    cdg.gen_cls_data(input_csv_path, cls_dir, per_class_records, total_cls_records)

    #Generate Invoice image dataset
    df = pd.read_csv(cls_csv_path)

    font = cv2.FONT_HERSHEY_SIMPLEX
    font_color = (3,0,0)
    thick = 1
    font_size = 0.5
    
    for i in range (0,number_of_invoices):
        start=time.time()
        row=df.iloc[i,]
        inv_id=row[0]
        vendor_code=row[1]
        Gl_code=row[2]
        inv_amt=row[3]
        desc_amt = row[4]
        Item_desc=row[5]
        prod_cat=row[6]

        crop_text = []
        crop_p1 = []
        gt_text = ""

        gt_ocr_filename="gt_ocr_"+str(inv_id)+".txt"
        gt_cls_filename="gt_cls_"+str(inv_id)+".txt"
        image_file_name = "img_"+str(inv_id)+".png"

        image_path = os.path.join(invoice_image_dir, image_file_name)
        template_image =255-np.zeros((715,1800,3), np.uint8)
        j=0

        text = "INVOICE"
        cv2.putText(template_image,text,(210,48), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((210,48))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j) +".png"
        j+=1
        process_crop(template_image, crop_filename, 210, 48, text_width, text_height+baseline, text)

        text = "InvoiceId:"
        cv2.putText(template_image,text,(22,123), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((22,123))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 22, 123, text_width, text_height+baseline, text)
        
        text = str(inv_id)
        cv2.putText(template_image,text,(130,123), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((130,123))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j) +".png"
        j+=1
        process_crop(template_image, crop_filename, 130, 123, text_width, text_height+baseline, text)

        text = "VendorName:"
        cv2.putText(template_image,text,(315,123), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((315,123))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j) +".png"
        j+=1
        process_crop(template_image, crop_filename, 315, 123, text_width, text_height+baseline, text)

        text = str(vendor_code)
        cv2.putText(template_image,text,(430,123), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((430,123))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j) +".png"
        j+=1
        process_crop(template_image, crop_filename, 430, 123, text_width, text_height+baseline, text)

        text = "GlCode:"
        cv2.putText(template_image,text,(22,150), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((22,150))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" +str(j)+ ".png"
        j+=1
        process_crop(template_image, crop_filename, 22, 150, text_width, text_height+baseline, text)

        text = str(Gl_code)
        cv2.putText(template_image,text,(130,150), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((130,150))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" +str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 130, 150, text_width, text_height+baseline, text)

        text = "Date:"
        cv2.putText(template_image,text,(315,150), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((315,150))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j) +".png"
        j+=1
        process_crop(template_image, crop_filename, 315, 150, text_width, text_height+baseline, text)

        text = "13/7/2022"
        cv2.putText(template_image,text,(430,150), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((430,150))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" +str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 430, 150, text_width, text_height+baseline, text)
        
        text = "InvAmt:"
        cv2.putText(template_image,text,(22,212), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((22,212))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 22, 212, text_width, text_height+baseline, text)

        text = str(inv_amt)
        cv2.putText(template_image,text,(195,212), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((195,212))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 195, 212, text_width, text_height+baseline, text)

        text = "DiscountAmt:"
        cv2.putText(template_image, text, (22, 238), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((22,238))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 22, 238, text_width, text_height+baseline, text)

        text = str(desc_amt)
        cv2.putText(template_image, text, (195, 238), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((195,238))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 195, 238, text_width, text_height+baseline, text)

        text = "Description:"
        cv2.putText(template_image,text,(22,265), font,font_size,font_color,thick)
        crop_text.append(text)
        crop_p1.append((22,265))
        (text_width, text_height), baseline = cv2.getTextSize(text, font, font_size, thick)
        gt_text = gt_text+text+" "
        crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j)+".png"
        j+=1
        process_crop(template_image, crop_filename, 22, 265, text_width, text_height+baseline, text)

        text = str(Item_desc)
        text_splits = text.split(" ")
        x_offset = 0
        for word in text_splits:
            word=word.strip()
            #word=str(word.encode(encoding="ascii",errors="ignore"))
            cv2.putText(template_image,word+" ",(195+x_offset,265), font,font_size,font_color,thick)
            crop_text.append(word)
            crop_p1.append((195+x_offset,265))
            (text_width, text_height), baseline = cv2.getTextSize(word+" ", font, font_size, thick)
            gt_text = gt_text+word+" "
            crop_filename = ocr_dataset + "/" +image_file_name.split('.')[0]+"_" + str(j)+".png"
            j = j+1
            process_crop(template_image, crop_filename, 195+x_offset, 265, text_width, text_height+baseline, word)
            x_offset = x_offset+text_width
            
        cv2.imwrite(image_path,template_image)
        
        #gt_text = cropImage(image_file_name, template_image, number_files, crop_text, crop_p1, ocr_dir)

        out_gt_text_file = open(os.path.join(invoice_image_dir, gt_ocr_filename), "w+")
        out_gt_cls_file = open(os.path.join(invoice_image_dir, gt_cls_filename), "w+")

        out_gt_text_file.write(gt_text)
        out_gt_cls_file.write(prod_cat)

        out_gt_text_file.close()   
        out_gt_cls_file.close() 

        #print(time.time()-start)
