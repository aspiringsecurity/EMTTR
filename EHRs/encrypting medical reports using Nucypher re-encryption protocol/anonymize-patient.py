# -*- coding: utf-8 -*-
import os
import glob
import re

inputFileList=glob.glob('inputFiles/*.txt')

outputDir="anonymizedFiles"
os.makedirs(outputDir,exist_ok=True)

for filepath in inputFileList:
    outFile=open(os.path.join(outputDir,os.path.basename(filepath)),"w", encoding='utf-8')
    with open(filepath,encoding='utf-8') as content_file:
        for line in content_file:
            original_content = line
            anonymized = original_content
            content = re.sub(r'(?i)<[^>]*>', '', original_content)
            no_html = re.sub(r'(?i)&nbsp;', '', content)

            field_list = ["ΟΝΟΜΑ ΑΣΘΕΝΗ:", "Όνομα Ασθενούς:", "Όνομα:", "όνομα:", "ΟΝΟΜΑ:", "Name:", "NAME:"]

            for field in field_list:
                pattern = re.compile(r'(?:' + field + ')(?:\s*)(\S*)(?:\s)(\S*)', re.I|re.S|re.U)

                for m in re.finditer(pattern, no_html):
                    print("Replacing " + m.group())
                    try:
                        anonymized = re.sub(r"("+m.group(1)+")", '__ANONYMOUS__', anonymized)
                        anonymized = re.sub(r"("+m.group(2)+")", '__ANONYMOUS__', anonymized)
                    except  re.error as re_error:
                        print("Skipping: " +  m.group() + "Text\n\n" + line)
            outFile.write(anonymized)
