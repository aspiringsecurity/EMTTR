# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
import torch
import torch.onnx
import crnn
import config
from torch.autograd import Variable

crnn = crnn.CRNN(config.imgH, config.nc, config.nclass, config.nh)
# model = torch.load("crnn_models/CRNN-1010_best_920.pth")
#print('loading pretrained model from %s' % model_path)
crnn.load_state_dict(torch.load("src/crnn_models/CRNN-1010_best_Intel_WHP_1000.pth",map_location="cpu"))
model = crnn
model.eval()
input_torch_tensor = Variable(torch.randn(1, 1, 32, 120))
onnx_model_name = "model/test_model_new.onnx"
torch.onnx.export(model, input_torch_tensor, onnx_model_name)