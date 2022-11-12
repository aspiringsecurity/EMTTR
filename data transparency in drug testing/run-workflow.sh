#!/bin/bash

pushd .
cd src

BASE="node workflow.js"
$BASE deployRegulator
$BASE addCroForApproval
$BASE approveCroWithId0
$BASE deployClinicalTrial
$BASE addSubjects
$BASE getSubjectById
$BASE addDataPoints

popd