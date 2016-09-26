(in-package #:tiling-viewport-manager)
(in-readtable :fn.reader)

(defparameter *minimum-frame-size* 5s0)
(defparameter *default-clear-color* (v! 0.5 0.5 0.5 0))

(deftclass (frame (:conc-name nil))
  split-type
  (children (make-viewport)))

(deftclass (buffer (:conc-name nil))
  (clear-color *default-clear-color*)
  (viewport (make-viewport))
  (content nil :type (or null sampler)))

(defun parent (frame)
  (unless (find frame *groups*)
    (find-if #'identity *groups* :key λ(%find-parent _))))

(defun %find-parent (frame)
  (with-slots (children) frame
    (when (typep children 'list)
      (or (when (find frame children) frame)
          (find-if #'identity children :key λ(%find-parent _))))))

(defparameter *groups* (list (make-frame)))
(defvar *current-group* 0)
(defvar *current-frame* nil)

(defun %set-current-frame (frame)
  (setf *current-frame* frame))

;;------------------------------------------------------------

(defun pick-frame (x-pix y-pix &optional (group-id *current-group*))
  (destructuring-bind (x y)
      (cepl.internals:window-size cepl.internals:*gl-window*)
    (let* ((n-x (/ x-pix x))
           (n-y (/ y-pix y)))
      (pick-frame-normalize-coords n-x n-y group-id))))

(defun pick-frame-normalize-coords (x y &optional (group-id *current-group*))
  (let ((frame (elt *groups* group-id)))
    (%pick-frame frame x y)))

(defun %pick-frame (frame x y)
  (let ((rx x)
        (ry y))
    (with-slots (children split-type) frame
      (if (listp children)
          (loop :for (c %x %y) :in children
             :if (and (<= rx %x) (<= ry %y))
             :return (cond
                       ((not (typep x 'frame)) c)
                       ((eq split-type :horizontal) (%pick-frame c (/ rx %x) y))
                       (t (%pick-frame c x (/ ry %y))))
             :else :do (if (eq split-type :horizontal)
                           (decf rx %x)
                           (decf ry %y))
             :finally (error "walked the wrong path. This is a bug"))
          frame))))

;;------------------------------------------------------------

(defun swap-group (&optional (group-id *current-group*))
  (render-buffers (elt *groups* group-id))
  (swap))

(defun render-buffers (frame)
  (with-slots (children) frame
    (typecase children
      (list (map nil #'render-buffers children))
      (buffer (render-buffer children)))))

(defun render-buffer (buffer)
  (format t "rendering ~s" buffer))

;;------------------------------------------------------------

(defun split-frame (frame &optional (direction :horizontal))
  (assert (find direction '(:horizontal :vertical)))
  (let ((parent (parent frame)))
    (if (and parent (eq direction (split-type parent)))
        (%split-parent parent frame)
        (%split-frame-children frame direction)))
  (update-frame-sizes)
  frame)

(defun %split-frame-children (frame direction)
  (with-slots (children split-type) frame
    (assert (not (listp children)))
    (setf children (list (list children 0.5s0 0.5s0)
                         (list (make-viewport) 0.5s0 0.5s0))
          split-type direction)
    (%set-current-frame (first children))))

(defun %split-parent (frame child)
  (with-slots (children split-type) frame
    (let ((pos (position child children :key #'first)))
      (destructuring-bind (child w h) (elt children pos)
        (let ((new-w (if (eq split-type :horizontal) (/ w 2s0) w))
              (new-h (if (eq split-type :vertical) (/ h 2s0) h)))
          (append (subseq children 0 (max 0 (- pos 1)))
                  (list (list child new-w new-h)
                        (list (make-frame) new-w new-h))
                  (subseq children pos)))))))

;;------------------------------------------------------------

(defun update-frame-sizes ()
  (let ((win cepl.context::*gl-window*))
    (when win
      (destructuring-bind (w h) (cepl.internals:window-size)
        (mapcar λ(update-frame _ w h 0 0) *groups*))))
  nil)

(defun update-frame (frame w h x y)
  (etypecase frame
    (viewport
     (setf (viewport-resolution frame) (v! w h)
           (viewport-origin frame) (v! x y)))
    (frame
     (with-slots (%-w %-h children) frame
       (etypecase children
         (viewport (setf (viewport-resolution children) (v! w h)
                         (viewport-origin children) (v! x y)))
         (list (update-children frame w h x y)))))))

(defun update-children (frame w h x y)
  (with-slots (%-w %-h split-type children) frame
    (loop :for (c-frame w% h%) :in children :do
       (let ((fw (if (eq split-type :horizontal) (* w% w) w))
             (fh (if (eq split-type :vertical) (* h% h) h)))
         (update-frame c-frame fw fh x y)
         (ecase split-type
           (:vertical (incf y fh))
           (:horizontal (incf x fw)))))))

;;------------------------------------------------------------

(defun change-frame-size (frame new-w new-h)
  (when (parent frame)
    (let* ((children (loop for c :in (children (parent frame)) :for i :from 0
                        :collect (cons i c)))
           (to-index (position frame children :key #'first))
           (is-last (= to-index (1- (length children))))
           (from-index (if is-last (1- to-index) (1+ to-index))))
      (assert (= 1 (abs (- to-index from-index))))
      (let ((res (sort (append (subseq children 0 (min to-index from-index))
                               (subseq children (max to-index from-index))
                               (give-size new-w new-h
                                          (elt children to-index)
                                          (elt children from-index)))
                       #'< :key #'car)))
        (setf children (mapcar #'rest res)))
      (assert (< (abs (- 100s0 (reduce #'+ children :key #'second))) 1))
      (assert (< (abs (- 100s0 (reduce #'+ children :key #'third))) 1))))
  (update-frame-sizes))

(defun give-size (w h to from)
  (destructuring-bind (to-id to to-w to-h) to
    (destructuring-bind (from-id from from-w from-h) from
      (let* ((to-w-dif (- w (max to-w *minimum-frame-size*)))
             (to-h-dif (- h (max to-h *minimum-frame-size*)))
             (proposed-to-w (- from-w to-w-dif))
             (proposed-to-h (- from-h to-h-dif))
             (to-w-dif (- to-w-dif (max proposed-to-w *minimum-frame-size*)))
             (to-h-dif (- to-h-dif (max proposed-to-h *minimum-frame-size*))))
        (list (list to-id to (+ to-w to-w-dif) (+ to-h to-h-dif))
              (list from-id from (- from-w to-w-dif) (+ from-h to-h-dif)))))))

;;------------------------------------------------------------
