;;;; package.lisp

(uiop:define-package #:tiling-viewport-manager
    (:use #:cl #:cepl #:vari)
  (:export
   :color-target
   :default-tvm-system
   :draw
   :frame
   :frame-at-point
   :focus-frame-at-point
   :make-color-target
   :pop-frame
   :register-target
   :target
   :target
   :target-names
   :tvm-draw
   :tvm-layout
   :tvm-reinit
   :split-horizontally
   :split-vertically
   :switch-to-target))
