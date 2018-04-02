;;;; package.lisp

(uiop:define-package #:tiling-viewport-manager
    (:use #:cl #:cepl #:vari)
  (:export
   :default-tvm-system
   :draw
   :frame
   :frame-at-point
   :focus-frame-at-point
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
   :switch-to-target

   ;; color target
   :color-target
   :make-color-target

   ;; fbo target
   :fbo-target
   :make-fbo-target
   :change-fbo
   :update-sample-params
   :target-fbo
   :target-attachment

   ;; tvm owned fbo target
   :tvm-owned-fbo-target
   :make-tvm-owned-fbo-target))
