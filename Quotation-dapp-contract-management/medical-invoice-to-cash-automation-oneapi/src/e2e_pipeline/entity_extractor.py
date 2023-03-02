# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
def extract(txt,bboxes):
    words = txt.strip().split(' ')
    print(words)
    print(len(words))
    #print(bboxes)
    # inv_number = words[2]
    # vendor_name = words[4]
    # gl_code = words[6]
    # inv_amt = words[10]
    # discount_amt = words[12]
    # description = ""
    # for i in range(14,len(words)):
    #     description = description+words[i]+" "
    index = -1
    inv_amt = ""
    discount_amt = ""
    gl_code = ""
    vendor_name = ""
    inv_number = ""
    description_index = 0

    for bbox in bboxes:
        index += 1
        xmin,xmax,ymin,ymax = bbox
        if xmin in range(120,140) and ymin in range(105,125):
            inv_number = words[index]
        
        if xmin in range(420,440) and ymin in range(105,125):
            vendor_name = words[index]

        if xmin in range(120,140) and ymin in range(130,155):
            gl_code = words[index]

        if xmin in range(185,205) and ymin in range(190,215):
            inv_amt = words[index]

        if xmin in range(185,210) and ymin in range(220,245):
            discount_amt = words[index]

        if xmin in range(10,30) and ymin in range(240,270):
            description_index = index

    description = ""
    for i in range(description_index+1,len(words)):
        description = description+words[i]+" "
    entities = [inv_number,vendor_name,gl_code,inv_amt,discount_amt,description]
    #print(entities)
    return entities

