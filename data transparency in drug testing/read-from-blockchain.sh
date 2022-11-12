#!/bin/bash

pushd .
cd src

BASE="node workflow.js"
$BASE readFromRegulator
$BASE readFromTrial

popd