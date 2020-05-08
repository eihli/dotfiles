;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-

(map! :leader
      "lh" #'scroll-left
      "ll" #'scroll-right)
(map!
 :ni "C-S-d" #'delete-surrounding-whitespace
 :nm  "C-S-o" #'better-jumper-jump-forward)
