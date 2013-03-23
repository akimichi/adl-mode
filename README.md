adl-mode
========

An Archetype Definition Language(ADL) mode for GNU emacs.

![Screenshot](https://raw.github.com/akimichi/adl-mode/master/images/screenshot01.png)

Installation
======

    $ git clone git://github.com/akimichi/adl-mode.git

Then put adl-mode.el into your site-lisp directory.

In your emacs config:

    (autoload 'adl-mode "adl-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.adl\\'" . adl-mode))


Known Bugs
====

Indentations invoked by carriage return are broken.

