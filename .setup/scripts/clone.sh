#!/bin/bash

git clone git@github.com:ijapesigan/longMI.git
rm -rf "$PWD.git"
mv longMI/.git "$PWD"
rm -rf longMI
