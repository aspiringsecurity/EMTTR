# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import pandas as pd
import numpy as np
import re

from nltk.stem import WordNetLemmatizer
from nltk.tokenize import word_tokenize
from nltk import pos_tag
from nltk.corpus import wordnet


def data_preprocessing_invoker(df_csv, trained_voc_size = 0):
    cat_sample_max = max(df_csv['Claim_Category'].value_counts())

    # preprocessing
    all_text = list(df_csv['Description'][...])

    text = list()
    word_count = list()  # statistics how many words in each sample
    lemmatizer = WordNetLemmatizer()  # model used to lemmatize word (defined by package nltk)
    text_lemmatized, word_count = text_lemmatization(text, lemmatizer, all_text, word_count)

    class Indexer:
        # Tokenizer
        def __init__(self):
            self.counter = 1
            self.d = {"<unk>": 0}
            self.rev = {}
            self._lock = False
            self.word_count = {}
            self.rev_d = {}
            self.rev[0] = "unk"

        def convert(self, w):
            if w not in self.d:
                if self._lock:
                    return self.d["<unk>"]
                self.d[w] = self.counter
                self.rev[self.counter] = w
                self.counter += 1
                self.word_count[self.d[w]] = 0
            self.word_count[self.d[w]] = self.word_count[self.d[w]] + 1
            return self.d[w]

        def convertback(self, w):
            return self.rev[w]

        def lock(self):
            self._lock = True

    all_data = []
    split_data = []
    tokenizer = Indexer()
    max_len_sent = 0
    max_index = 0
    for i, t in enumerate(text_lemmatized):
        current_convert = [tokenizer.convert(w) for w in t.split()]
        # # ===========================
        # # Priyanga - Remove list of words not trained in vocabulary list
        if trained_voc_size > 0:
            for word_num in current_convert[:]:
                if word_num >= trained_voc_size:
                    current_convert.remove(word_num)
        # # ===========================
        max_index = max(max_index, max(current_convert))
        max_len_sent = max(max_len_sent, len(current_convert))
        split_data.append(current_convert)
    vocabulary_size = max_index + 1

    for i, t in enumerate(split_data):
        if (len(t) < max_len_sent):
            num_of_padding = max_len_sent - len(t)
            for n in range(0, num_of_padding):
                t.append(0)
        all_data.append(t)

    text_df = pd.DataFrame(all_data)
    cate_df = pd.DataFrame(df_csv['Claim_Category'])

    df_text_cate = text_df
    df_text_cate['category'] = 'cat'
    for i in range(0, len(cate_df)):
        df_text_cate.at[i, 'category'] = cate_df.iloc[i]['Claim_Category']

    labels = list(df_text_cate.category.unique())
    for l in labels:
        df_text_cate[l] = 0
    for i in range(0, len(df_text_cate)):
        cat = df_text_cate.iloc[i]['category']
        df_text_cate.at[i, cat] = 1

    return df_text_cate, labels, cat_sample_max, vocabulary_size


def clean_str(string):
    """
    String cleaning .
    """
    string = re.sub(r"[^A-Za-z0-9]", " ", string) # remove unused charactor other than english letter and number, use space to replace
    return string.strip()                         # delete the first and last space


def get_wordnet_pos(treebank_tag):
    """
    Return the POS of each word for later usage .
    """
    if treebank_tag.startswith('J'):
        return wordnet.ADJ
    elif treebank_tag.startswith('V'):
        return wordnet.VERB
    elif treebank_tag.startswith('N'):
        return wordnet.NOUN
    elif treebank_tag.startswith('R'):
        return wordnet.ADV
    else:
        return wordnet.NOUN   # no need to change


def text_lemmatization(l,lemmatizer,t_text, word_count):
    """
    Tokenization: Split the text into words.
    Lemmatize the text .
    """
    for i in range(len(t_text)):
        text_clean = clean_str(t_text[i])     # clean texts, remove useless symbols
        text_word = word_tokenize(text_clean) # set each individual token
        text_pos = pos_tag(text_word)         # pos tagging each token [word,POS]
        text_lemma = ""
        for item in text_pos:                 # lemmatizing each token
            # Put each word after lemmatization into the list
            text_lemma = text_lemma + " " + (lemmatizer.lemmatize(item[0],get_wordnet_pos(item[1])))
        l.append(text_lemma.strip())          # append the preprocessed sample to x_train list, remove the space
        word_count.append(len(text_pos))
    return l, word_count


def split_dataset(df_text_cate, train_ratio):
    validation_ratio = (1-train_ratio) / 2

    train_spilt_ratio = round((len(df_text_cate) * train_ratio))
    valid_spilt_ratio = round(len(df_text_cate) * validation_ratio)
    column_index = df_text_cate.columns.get_loc('category')
    train_y = df_text_cate.iloc[0:train_spilt_ratio, column_index + 1:57]
    train_x = df_text_cate.iloc[0:train_spilt_ratio, 0:column_index]

    valid_y = df_text_cate.iloc[train_spilt_ratio:train_spilt_ratio + valid_spilt_ratio, column_index + 1:57]
    valid_x = df_text_cate.iloc[train_spilt_ratio:train_spilt_ratio + valid_spilt_ratio, 0:column_index]

    test_y = df_text_cate.iloc[train_spilt_ratio + valid_spilt_ratio:, column_index + 1:57]
    test_x = df_text_cate.iloc[train_spilt_ratio + valid_spilt_ratio:, 0:column_index]

    return train_x, train_y, valid_x, valid_y, test_x, test_y

