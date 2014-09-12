#!/usr/bin/env bash

if [ ! -f /etc/apt/sources.list.old ]
then
    sudo mv /etc/apt/sources.list /etc/apt/sources.list.old
    sudo cp /vagrant/conf/sources.list /etc/apt/sources.list
fi

sudo apt-get update
sudo apt-get dist-upgrade -y
sudo apt-get install -y devscripts debhelper debianutils dh-lisp  \
     gnupg rsync

# build deps
sudo apt-get install sbcl buildapp cl-asdf cl-postmodern cl-esrap \
     cl-py-configparser cl-split-sequence cl-ppcre  cl-alexandria \
     cl-hunchentoot cl-yason cl-closer-mop cl-daemon cl-who       \
     cl-bordeaux-threads cl-graph cl-trivial-backtrace            \
     ruby-ronn

cat /vagrant/conf/bashrc.sh >> ~/.bashrc
