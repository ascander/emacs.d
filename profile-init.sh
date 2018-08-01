#!/bin/sh

EMACS_CMD=/Applications/Emacs.app/Contents/MacOS/Emacs

$EMACS_CMD -Q -l ./site-lisp/profile-dotemacs.el -f profile-dotemacs
