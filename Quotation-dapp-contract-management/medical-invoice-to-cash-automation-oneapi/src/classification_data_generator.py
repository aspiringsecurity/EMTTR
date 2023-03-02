# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause

# pylint: disable=missing-docstring
import pandas as pd 
import random
import numpy as np
import argparse
import os

inv_id_list=[]
vendor_code_list=[]
gl_code_list=[]
inv_amt_list=[]
discount_amt_list=[]
item_description_list=[]
prod_category_list=[]
input_category_list=[]
discount_table_list=[]

def gen_cls_data(input_csv_path, cls_dir, per_class_records, total_records):
    '''data_generator function generates the claim category csv file from the input csv file
       input:input csv file
       output:generated csv file'''

    df_original = pd.read_csv(input_csv_path)
    df=df_original[['product_name','product_category_tree']]
    
    # Finding the categories in the data
    for index, row in df.iterrows():
        old_val=row['product_category_tree']
        if len(old_val.split(">>"))==1:
            row['product_category_tree']='NULL'
        else:
            new_val=old_val.split(">>")[0]
            row['product_category_tree']=new_val.strip('["')
            #print(row['product_category_tree'])
    prod_category_list=df[['product_category_tree']].values.tolist()
    cat_list=np.unique(prod_category_list)

    # Find the top 10 list of classes 
    df_temp=df
    class_count_list=[]
    prod_category_list=[]
    # Find the count for all categories
    for category in cat_list:
        df=df_temp
        df = df.set_index(df_temp.columns[1])
        df=df.loc[category]
        length_of_records=df.shape[0]
        class_count_list.append([category,length_of_records])
    # Save count of each record    
    # with open("count.csv",'w') as f:
    #     write = csv.writer(f)
    #     write.writerows(class_count_list)
    
    # Sort the count list to get the top 10 values
    sorted_lst=sorted(class_count_list, key = lambda x: x[1],reverse=True)
    category_list=[]
    for i in range(0,10):
        category_list.append(sorted_lst[i][0])
    #print(category_list)

    #Generate output csv file 
    df=df_temp
    #df_temp.to_csv("temp.csv", index=False)
    df_original=df
    per_class_record_usr=per_class_records    
    for category in category_list:
        df=df_original
        per_class_records=per_class_record_usr # original value sent by the user.
        df = df.set_index(df_original.columns[1])
        df=df.loc[category]
        length_of_records=df.shape[0]
        # Initial fix applied if the no of records per class is less than the demanded no of records from the user
        # if length_of_records<per_class_records:
        #     print("The number of per class records for class %s is more than the records available currently...Setting the per class records to the max number available!!!!!" % category)
        #     per_class_records=length_of_records # Value changed to max records available
        #import pdb;pdb.set_trace()
        per_class_records=length_of_records
        temp_list=df.values.tolist()

        #Adding item descriptions and claim category
        discount_amt=str(random.randint(10,80))
        discount_table_list.append(['CL_'+str(category_list.index(category)),discount_amt])

        for iter in range(0,per_class_records):
            item=temp_list[iter][0]
            item_description_list.append(temp_list[iter][0])
            prod_category_list.append('CL_'+str(category_list.index(category)))
            discount_amt_list.append(discount_amt)

    #Adding Random values for fields
    total_records=len(prod_category_list)
    for i in range(total_records):
        inv_id_list.append(random.randint(0,total_records))
        vendor_code_list.append(str(random.randint(500,total_records)))
        gl_code_list.append(str(random.randint(500,10000)))
        inv_amt_list.append(random.randint(1000,50000))
    
    #Write data to new csv
    output_csv_path = os.path.join(cls_dir,'classification_data.csv')
    list_dict = {'InvId':inv_id_list, 'VendorCode':vendor_code_list,'GlCode':gl_code_list,'InvAmt':inv_amt_list,'DiscountAmt':discount_amt_list,'Description':item_description_list,'Claim_Category':prod_category_list} 
    df = pd.DataFrame(list_dict)
    df_shuffled=df.sample(frac=1) 
    df_shuffled.to_csv(output_csv_path, index=False)

    # Writing discounts to txt file 
    with open(os.path.join(cls_dir, "mapping.txt"), 'w+') as fp:
        fp.write("claim_category")
        fp.write('\t')
        fp.write("discount")
        fp.write('\n')
        for item in discount_table_list:
        # write each item on a new line
            fp.write(item[0])
            fp.write('\t')
            fp.write(item[1])
            fp.write('\n')
    fp.close()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
                        
    parser.add_argument('-i',
                        '--input_csv_path',
                        type=str,
                        required=True,
                        help='path for input csv')

    parser.add_argument('-o',
                        '--output_csv_path',
                        type=str,
                        required=True,
                        help='path for output csv')

    parser.add_argument('-pc',
                        '--per_class_records',
                        type=int,
                        default=1000,
                        help='per class records')
    
    parser.add_argument('-t',
                        '--totalrecords',
                        type=str,
                        default=10000,
                        help='total records')                     

    FLAGS = parser.parse_args()
    input_csv_path = FLAGS.input_csv_path
    output_csv_path=FLAGS.output_csv_path
    per_class_records=FLAGS.per_class_records
    total_records=FLAGS.totalrecords

    input_csv_path = "flipkart_com-ecommerce_sample.csv"
    output_csv_path="data.csv"
    per_class_records=1000
    total_records=10000
    gen_cls_data(input_csv_path, output_csv_path, per_class_records, total_records)


