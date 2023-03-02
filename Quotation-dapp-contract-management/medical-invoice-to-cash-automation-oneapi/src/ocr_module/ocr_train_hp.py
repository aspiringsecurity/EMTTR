# pylint: disable=missing-docstring
# Copyright (C) 2022 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
from __future__ import print_function
import argparse
import random
import os
import datetime
import time
import torch
from torch import optim
import torch.utils.data
from torch.autograd import Variable
from torch.nn import CTCLoss
import numpy as np
import utils  # pylint : disable=no-name-in-module
import mydataset
import crnn
import logging
import config
from online_test import val_model

torch.autograd.set_detect_anomaly(True)

config.imgW = 128
config.imgH = 32
config.alphabet = config.alphabet_v2
config.nclass = len(config.alphabet) + 1
config.saved_model_prefix = 'CRNN-1010_HP'
config.keep_ratio = True
config.use_log = True
config.batchSize = 32
config.workers = 4
config.adam = True
config.cuda = False
config.ngpu = 0
config.lr = 1e-4
config.niter = 1

os.environ["CUDA_VISIBLE_DEVICES"] = "0"
if not os.path.exists('debug_files'):
    os.mkdir('debug_files')
if not os.path.exists(config.saved_model_dir):
    os.mkdir(config.saved_model_dir)
if config.experiment is None:
    config.experiment = 'expr'
if not os.path.exists(config.experiment):
    os.mkdir(config.experiment)

config.manualSeed = random.randint(1, 10000) 
random.seed(config.manualSeed)
np.random.seed(config.manualSeed)
torch.manual_seed(config.manualSeed)

#Generating the train dataset from the training info file
train_dataset = mydataset.MyDataset(info_filename=config.train_infofile)
assert train_dataset  # nosec
if not config.random_sample:
    sampler = mydataset.randomSequentialSampler(train_dataset, config.batchSize)
else:
    sampler = None
    
#Creating the train loader
train_loader = torch.utils.data.DataLoader(
    train_dataset, batch_size=config.batchSize,
    shuffle=True, sampler=sampler,
    num_workers=int(config.workers),
    collate_fn=mydataset.alignCollate(imgH=config.imgH, imgW=config.imgW, keep_ratio=config.keep_ratio))

#Generating the test dataset from the test info file
test_dataset = mydataset.MyDataset(
    info_filename=config.val_infofile, transform=mydataset.resizeNormalize((config.imgW, config.imgH), is_test=True))

converter = utils.strLabelConverter(config.alphabet)
criterion = CTCLoss(reduction='sum', zero_infinity=True)

# custom weights initialization called on crnn
def weights_init(m):
    '''Initialization of weights'''
    classname = m.__class__.__name__
    if classname.find('Conv') != -1:
        m.weight.data.normal_(0.0, 0.02)
    elif classname.find('BatchNorm') != -1:
        m.weight.data.normal_(1.0, 0.02)
        m.bias.data.fill_(0)

if config.cuda:
    crnn.cuda()
    device = torch.device('cuda:0')
    criterion = criterion.cuda()

#Creating object of class CRNN
crnn = crnn.CRNN(config.imgH, config.nc, config.nclass, config.nh)

#If pretrained weights available loading weights from pretrained model else apply the initial weights
if config.pretrained_model != '' and os.path.exists(config.pretrained_model):
    print('loading pretrained model from %s' % config.pretrained_model)
    crnn.load_state_dict(torch.load(config.pretrained_model, map_location="cpu"))
else:
    crnn.apply(weights_init)

# loss averager
loss_avg = utils.averager()

# setup optimizer
if config.adam:
    optimizer = optim.Adam(crnn.parameters(), lr=config.lr, betas=(config.beta1, 0.999))
elif config.adadelta:
    optimizer = optim.Adadelta(crnn.parameters(), lr=config.lr)
else:
    optimizer = optim.RMSprop(crnn.parameters(), lr=config.lr)


def val(net, dataset, criterion, max_iter=100, intel=False):
    '''val function Validates the model to determine the accuracy and saves the best model
       @input params :model,dataset,criterion
    '''
    best_acc = 0.1
    print('Start val')
    logger.debug('Start val')
    for p in net.parameters():
        p.requires_grad = False

    num_correct, num_all = val_model(config.val_infofile, net, False)
    print(num_correct)
    print(num_all)
    accuracy = num_correct / num_all
    print('ocr_acc: %f' % (accuracy))
    logger.debug('ocr_acc: %f' % (accuracy))
    if intel:
        env_str="intel"
    else:
        env_str = "stock"
    
    if accuracy > best_acc:
        best_acc = accuracy
        torch.save(net.state_dict(), '{}/{}_{}.pth'.format(config.saved_model_dir, config.saved_model_prefix,env_str))
    torch.save(net.state_dict(), '{}/{}_{}.pth'.format(config.saved_model_dir, config.saved_model_prefix,env_str))
    return accuracy

def trainBatch(net, criterion, optimizer):
    '''trainBatch function trains the crnn model and returns the cost
       @input params:net,criterion,optimizer
       @output param:cost '''
    data = train_iter.next()
    cpu_images, cpu_texts = data
    batch_size = cpu_images.size(0)
    print("batch size: " + str(batch_size))
    logger.debug("batch size: "+str(batch_size))
    image = cpu_images

    text, length = converter.encode(cpu_texts)

    preds = net(image)  # seqLength x batchSize x alphabet_size
    preds_size = Variable(torch.IntTensor([preds.size(0)] * batch_size).cpu())  # seqLength x batchSize
    cost = criterion(preds.log_softmax(2).cpu(), text.cpu(), preds_size, length.cpu())/ batch_size
    if torch.isnan(cost):
        print(batch_size, cpu_texts)
        logger.debug(batch_size, cpu_texts)
    else:
        net.zero_grad()
        cost.backward()
        torch.nn.utils.clip_grad_norm_(net.parameters(), 5)
        optimizer.step()
    return cost

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-i',
                        '--intel',
                        type=int,
                        required=True,
                        default=0,
                        help='use 1 for enabling intel pytorch optimizations, default is 0')
                        
    FLAGS = parser.parse_args()
    intel_flag = FLAGS.intel
    if intel_flag:
        import intel_extension_for_pytorch as ipex  # pylint: disable=E0401
        crnn = ipex.optimize(crnn, optimizer=optimizer)
        crnn_net = crnn[0]
        optimizer_intel = crnn[1]
        log_filename = os.path.join('./logs/', 'Intel_'+config.saved_model_prefix + '.txt')
        logging.basicConfig(filename=log_filename, level=logging.DEBUG,force=True)
        logger = logging.getLogger()
        print("Intel Pytorch Optimizations has been Enabled!")
        logger.debug("Intel Pytorch Optimizations has been Enabled!")
    else:
        log_filename = os.path.join('./logs/', 'Stock_'+config.saved_model_prefix + '.txt')
        logging.basicConfig(filename=log_filename, level=logging.DEBUG,force=True)
        logger = logging.getLogger()
        device = torch.device('cpu')
        criterion = criterion.to(device)
    parameters_list = []
    accuracy_list = []
    print("Start of hp tuning ")
    logger.debug("Start of hp tuning")
    train_time = time.time()
    batch_size = [32]
    epochs_vals = [5,10]
    lr_vals = [1e-4]
    for bt_sz in batch_size:  # iterating through the batch_size list
        config.batchSize = bt_sz
        for epoch_val in epochs_vals:  # iterating through epoch values list
            config.niter = epoch_val
            for lrng_rate in lr_vals:  # terating through the learning rate list
                config.lr = lrng_rate

                for epoch in range(config.niter):
                    loss_avg.reset()
                    print('epoch {}....'.format(epoch))
                    logger.debug('epoch {}....'.format(epoch))
                    train_iter = iter(train_loader)
                    i = 0
                    n_batch = len(train_loader)
                    if intel_flag:
                        while i < len(train_loader):
                            for p in crnn_net.parameters():
                                p.requires_grad = True
                            crnn_net.train()
                            cost = trainBatch(crnn_net, criterion, optimizer_intel)
                            print('epoch: {} iter: {}/{} Train loss: {:.3f}'.format(epoch, i, n_batch, cost.item()))
                            logger.debug('epoch: {} iter: {}/{} Train loss: {:.3f}'.format(epoch, i, n_batch, cost.item()))
                            loss_avg.add(cost)
                            #loss_avg.add(cost)
                            i += 1
                        print('Train loss: %f' % (loss_avg.val()))
                        logger.debug('Train loss: %f' % (loss_avg.val()))
                    else:
                        while i < len(train_loader):
                            for p in crnn.parameters():
                                p.requires_grad = True
                            crnn.train()
                            cost = trainBatch(crnn, criterion, optimizer)
                            print('epoch: {} iter: {}/{} Train loss: {:.3f}'.format(epoch, i, n_batch, cost.item()))
                            logger.debug('epoch: {} iter: {}/{} Train loss: {:.3f}'.format(epoch, i, n_batch, cost.item()))
                            loss_avg.add(cost)
                            #loss_avg.add(cost)
                            i += 1
                        print('Train loss: %f' % (loss_avg.val()))
                        logger.debug('Train loss: %f' % (loss_avg.val()))
                print("Inferencing.................")
                logger.debug("Inferencing.................")
                if intel_flag:
                    valid_accuracy = val(crnn_net, test_dataset, criterion, intel=True)
                else:
                    valid_accuracy = val(crnn, test_dataset, criterion, intel=False)
                accuracy_list.append(valid_accuracy)
                parameters_list.append((config.batchSize, config.lr, config.niter))
    print("Hyperparameter tuning time is", time.time()-train_time)
    logger.debug("Hyperparameter tuning time is : %f" % (time.time()-train_time))

    max_value = max(accuracy_list)
    max_index = accuracy_list.index(max_value)
    print("accuracy list")
    logger.debug("accuracy list")
    print(accuracy_list)
    logger.debug(accuracy_list)
    print("parameters list")
    logger.debug("parameters list")
    print(parameters_list)
    logger.debug(parameters_list)
    print("the best parameters are")
    logger.debug("the best parameters are")
    print(parameters_list[max_index])
    logger.debug(parameters_list[max_index])
