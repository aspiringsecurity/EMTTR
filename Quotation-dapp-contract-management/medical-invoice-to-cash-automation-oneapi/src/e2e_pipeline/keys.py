# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
# pylint: disable=missing-docstring
# pylint: disable=consider-using-with
import pickle as pkl  # nosec
alphabet_list = pkl.load(open('./src/e2e_pipeline/alphabet.pkl', 'rb'))  # nosec
alphabet = [ord(ch) for ch in alphabet_list]
alphabet_v2 = alphabet
