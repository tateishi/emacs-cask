@echo off

emacs -Q --script ~/.cask/cask-cli.el -- update
emacs -Q --script ~/.cask/cask-cli.el -- install
emacs -Q --script ~/.cask/cask-cli.el -- upgrad
