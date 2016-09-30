(in-package #:tiling-viewport-manager)
(in-readtable :fn.reader)

(defparameter *minimum-frame-size* 5s0)
(defparameter *frame-size-px* 5s0)
(defparameter *last-highlight-time* 0s0)
(defparameter *highlight-duration-ms* 64s0)
(defparameter *highlight-color* (v! 1 0 0 0))

;;------------------------------------------------------------

(defparameter *colors* (list (v! 0.46 0.46 0.46 0)
                             (v! 0.26 0.26 0.26 0)
                             (v! 0.43 0.43 0.43 0)
                             (v! 0.37 0.37 0.37 0)
                             (v! 0.17 0.17 0.17 0)
                             (v! 0.2 0.2 0.2 0)
                             (v! 0.29 0.29 0.29 0)
                             (v! 0.31 0.31 0.31 0)
                             (v! 0.4 0.4 0.4 0)
                             (v! 0.34 0.34 0.34 0)
                             (v! 0.23 0.23 0.23 0)))

(let ((i 0))
  (defun get-col ()
    (setf i (mod (1+ i) (length *colors*)))
    (elt *colors* i)))

;;------------------------------------------------------------

(deftclass (buffer (:conc-name nil) (:constructor %%make-buffer))
  (name (error "A buffer needs a name"))
  (clear-color (get-col))
  (viewport (make-viewport))
  (bordered-viewport (make-viewport))
  (content nil :type (or null sampler))
  (frame nil :type (or null frame)))

(defvar *buffers* (make-hash-table))
(defvar *buffer-id* -1)

(defun gen-null-buffer-name ()
  (intern (format nil "NULL-BUFFER-~a" (incf *buffer-id*)) :keyword))

(defun make-buffer (&optional name)
  (second (multiple-value-list (%make-buffer name))))

(defun %make-buffer (&optional name frame)
  (assert (or (null name) (keywordp name)))
  (let ((name (or name (gen-null-buffer-name))))
    (assert (not (buffer-exists-p name)))
    (assert (keywordp name))
    (values (setf (gethash name *buffers*)
                  (%%make-buffer :name name :frame frame))
            name)))

(defun kill-buffer (name)
  (assert (buffer-exists-p name))
  (let ((buffer (gethash name *buffers*)))
    (%switch-to-buffer (frame buffer) nil)
    (setf (viewport buffer) nil
          (content buffer) nil
          (frame buffer) nil)
    nil))

(defun buffer-exists-p (name)
  (assert (keywordp name))
  (not (null (gethash name *buffers*))))

(defun find-or-make-suitable-buffer ()
  (block nil
    (maphash (lambda (name buffer)
               (when (not (frame buffer))
                 (return name)))
             *buffers*)
    (make-buffer)))

(defmethod resolution ((buffer buffer))
  (with-slots (viewport) buffer
    (when viewport
      (viewport-resolution viewport))))

(defmethod (setf resolution) (value (buffer buffer))
  (with-slots (viewport bordered-viewport) buffer
    (when viewport
      (setf (viewport-resolution viewport) value)
      (setf (viewport-resolution bordered-viewport)
            (v2:- value (v! *frame-size-px* *frame-size-px* ))))))

(defmethod origin ((buffer buffer))
  (with-slots (viewport) buffer
    (when viewport
      (viewport-origin viewport))))

(defmethod (setf origin) (value (buffer buffer))
  (with-slots (viewport bordered-viewport) buffer
    (when viewport
      (setf (viewport-origin viewport) value)
      (setf (viewport-origin bordered-viewport)
            (v2:+ value (v! *frame-size-px* *frame-size-px*))))))

(defun pick-buffer-left (&optional buffer)
  (let* ((of (or buffer (%current-buffer)))
         (origin (origin of)))
    (or (pick-buffer (- (v:x origin) (* 4 *frame-size-px*))
                    (+ (v:y origin) (* 4 *frame-size-px*)))
        buffer)))

(defun pick-buffer-right (&optional buffer)
  (let* ((of (or buffer (%current-buffer)))
         (origin (origin of))
         (res (resolution of)))
    (or (pick-buffer (- (v:x origin) (v:x res) (* 4 *frame-size-px*))
                    (+ (v:y origin) (* 4 *frame-size-px*)))
        buffer)))

(defun pick-buffer-down (&optional buffer)
  (let* ((of (or buffer (%current-buffer)))
         (origin (origin of)))
    (or (pick-buffer (+ (v:x origin) (* 4 *frame-size-px*))
                    (- (v:y origin) (* 4 *frame-size-px*)))
        buffer)))

(defun pick-buffer-up (&optional buffer)
  (let* ((of (or buffer (%current-buffer)))
         (origin (origin of))
         (res (resolution of)))
    (or (pick-buffer (+ (v:x origin) (* 4 *frame-size-px*))
                    (+ (v:y origin) (v:y res) (* 4 *frame-size-px*)))
        buffer)))

(defun switch-to-buffer (buffer-name)
  (assert (buffer-exists-p buffer-name))
  (%switch-to-buffer (%current-frame) buffer-name))

(defun %switch-to-buffer (frame &optional buffer-name)
  (with-slots (children) frame
    (assert (buffer-p children))
    (let* ((buffer-name (or buffer-name (find-or-make-suitable-buffer)))
           (buffer (gethash buffer-name *buffers*)))
      (if (frame buffer)
          (setf *current-buffer* buffer)
          (%switch-to-buffer-obj frame buffer))
      buffer-name)))

(defun %switch-to-buffer-obj (frame buffer)
  ;; duplicate above asserts for the case when this is called directly
  (assert buffer)
  (unless (frame buffer) ;; if buffer already has a frame then just focus
    (with-slots (children) frame
      (assert (buffer-p children))
      (setf children buffer)))
  (setf *current-buffer* buffer))

(defvar *current-buffer*)

(defun current-buffer ()
  (name (%current-buffer)))

(defun %current-buffer ()
  *current-buffer*)

(defun buffer-contents (&optional buffer-name)
  (if buffer-name
      (content (gethash buffer-name *buffers*))
      (content (%current-buffer))))

(defun (setf buffer-contents) (value &optional buffer-name)
  (assert (or (typep value 'texture)
              (typep value 'sampler)
              (null value)))
  (if buffer-name
      (setf (content (gethash buffer-name *buffers*)) value)
      (setf (content (%current-buffer)) value)))

(defun list-all-buffers ()
  (cepl-utils:map-hash
   (lambda (k v) (declare (ignore v)) k)
   *buffers*))

;;------------------------------------------------------------

(deftclass (frame (:conc-name nil))
  split-type
  group-id
  (children (%make-buffer)))

(defun parent (frame)
  (let* ((gid (group-id frame))
         (root-frame (or (gethash gid *groups*) frame)))
    (unless (eq frame root-frame)
      (%find-parent root-frame))))

(defun %find-parent (frame)
  (with-slots (children) frame
    (when (typep children 'list)
      (or (when (find frame children) frame)
          (find-if #'identity children :key λ(%find-parent (first _)))))))

(defun %current-frame ()
  (frame (gethash (current-buffer) *buffers*)))

;;------------------------------------------------------------


(defun pick-buffer (x-pix y-pix &optional (group-id *current-group*))
  (destructuring-bind (x y)
      (cepl.internals:window-dimensions cepl.internals:*gl-window*)
    (let* ((n-x (/ x-pix x))
           (n-y (/ y-pix y)))
      (children (pick-frame-normalize-coords n-x n-y group-id)))))

(defun pick-buffer-normalize-coords (x y &optional (group-id *current-group*))
  (let ((frame (gethash group-id *groups*)))
    (children (%pick-frame frame x y))))

(defun %pick-frame (frame x y)
  (let ((rx x)
        (ry y))
    (with-slots (children split-type) frame
      (if (listp children)
          (loop :for (c %x %y) :in children
             :if (and (<= rx %x) (<= ry %y))
             :return (cond
                       ((not (typep c 'frame)) c)
                       ((eq split-type :horizontal) (%pick-frame c (/ rx %x) y))
                       (t (%pick-frame c x (/ ry %y))))
             :else :do (if (eq split-type :horizontal)
                           (decf rx %x)
                           (decf ry %y))
             :finally (error "walked the wrong path. This is a bug"))
          frame))))

;;------------------------------------------------------------

(defun draw-&-swap (&optional (group-id (current-group)))
  (unless (= *last-highlight-time* 0s0)
    (%render-highlight (%current-buffer)))
  (%render-buffers (gethash group-id *groups*))
  (swap))

(defun %render-buffers (frame)
  (with-slots (children) frame
    (typecase children
      (list (map nil λ(%render-buffers (first _)) children))
      (buffer (%render-buffer children)))))

(defun %render-highlight (buffer)
  (let ((time (get-internal-real-time)))
    (with-viewport (viewport buffer)
          (cepl.misc:draw-colored-quad
           *highlight-color* :swap nil :depth 0.8s0))
    (when (> (- time *last-highlight-time*) *highlight-duration-ms*)
      (setf *last-highlight-time* 0s0))))

(defun %render-buffer (buffer)
  (with-viewport (bordered-viewport buffer)
    (with-slots (content clear-color) buffer
      (typecase content
        (texture (cepl.misc:draw-texture
                  content :swap nil :depth 0.9s0))
        (sampler (cepl.misc:draw-texture
                  content :swap nil :depth 0.9s0))
        (null (when clear-color
                (cepl.misc:draw-colored-quad
                 clear-color :swap nil :depth 0.9s0)))))))

;;------------------------------------------------------------

(defun split-frame (&optional (direction :horizontal))
  (%split-frame (%current-frame) direction))

(defun %split-frame (frame &optional (direction :horizontal))
  (assert (find direction '(:horizontal :vertical)))
  (let ((parent (parent frame)))
    (if (and parent (eq direction (split-type parent)))
        (%split-parent parent frame)
        (%split-frame-children frame direction)))
  (update-frame-sizes)
  frame)

(defun %split-frame-children (frame direction)
  (with-slots (children split-type) frame
    (assert (buffer-p children))
    (let* ((top-buffer children)
           (top-frame (make-frame :group-id (group-id frame)
                                  :children children))
           (bottom-buffer (%make-buffer))
           (bottom-frame (make-frame :group-id (group-id frame)
                                     :children bottom-buffer)))
      (setf (frame top-buffer) top-frame
            (frame bottom-buffer) bottom-frame)
      (setf children (list (list top-frame 0.5s0 0.5s0)
                           (list bottom-frame 0.5s0 0.5s0))
            split-type direction)
      (%switch-to-buffer-obj top-frame top-buffer))))

(defun %split-parent (frame child-to-split)
  (with-slots (children split-type) frame
    (let ((pos (position child-to-split children :key #'first)))
      (destructuring-bind (child w h) (elt children pos)
        (let ((new-w (if (eq split-type :horizontal) (/ w 2s0) w))
              (new-h (if (eq split-type :vertical) (/ h 2s0) h)))
          (setf children
                (append (subseq children 0 (max 0 (- pos 1)))
                        (list (list child new-w new-h)
                              (list (make-frame :group-id (group-id frame))
                                    new-w new-h))
                        (subseq children pos))))))))

;;------------------------------------------------------------

(defun update-frame-sizes ()
  (let ((win cepl.context::*gl-window*))
    (when win
      (destructuring-bind (w h) (cepl.internals:window-dimensions)
        (update-frame (gethash (current-group) *groups*) w h 0 h))))
  nil)

(defun update-frame (frame w h x y)
  (etypecase frame
    (buffer
     (setf (resolution frame) (v! w h)
           (origin frame) (v! x y)))
    (frame
     (with-slots (%-w %-h children) frame
       (etypecase children
         (buffer (setf (resolution children) (v! w h)
                       (origin children) (v! x (- y h))))
         (list (update-children frame w h x y)))))))

(defun update-children (frame w h x y)
  (with-slots (%-w %-h split-type children) frame
    (loop :for (c-frame w% h%) :in children :do
       (let ((fw (if (eq split-type :horizontal) (* w% w) w))
             (fh (if (eq split-type :vertical) (* h% h) h)))
         (update-frame c-frame fw fh x y)
         (ecase split-type
           (:vertical (decf y fh))
           (:horizontal (incf x fw)))))))

;;------------------------------------------------------------

(defun change-frame-size (frame new-w new-h)
  (when (parent frame)
    (with-slots (children) (parent frame)
      (let* ((to-pos (position frame children :key #'first))
             (is-last (= to-pos (1- (length children))))
             (from-pos (if is-last (1- to-pos) (1+ to-pos))))
        (assert (= 1 (abs (- to-pos from-pos))))
        (setf children
              (append (subseq children 0 (min to-pos from-pos))
                      (give-size new-w new-h
                                 to-pos (elt children to-pos)
                                 from-pos (elt children from-pos))
                      (subseq children (max to-pos from-pos))))
        (assert (< (abs (- 100s0 (reduce #'+ children :key #'second))) 1))
        (assert (< (abs (- 100s0 (reduce #'+ children :key #'third))) 1)))))
  (update-frame-sizes))

(defun give-size (w h to-pos to from-pos from)
  (destructuring-bind (to to-w to-h) to
    (destructuring-bind (from from-w from-h) from
      (let* ((to-w-dif (- w (max to-w *minimum-frame-size*)))
             (to-h-dif (- h (max to-h *minimum-frame-size*)))
             (proposed-to-w (- from-w to-w-dif))
             (proposed-to-h (- from-h to-h-dif))
             (to-w-dif (- to-w-dif (max proposed-to-w *minimum-frame-size*)))
             (to-h-dif (- to-h-dif (max proposed-to-h *minimum-frame-size*))))
        (mapcar #'first
                (sort (list (list to-pos to (+ to-w to-w-dif) (+ to-h to-h-dif))
                            (list from-pos from (- from-w to-w-dif) (+ from-h to-h-dif)))
                      #'< :key #'car))))))

;;------------------------------------------------------------

(defparameter *group-count* 0)
(defparameter *last-group* nil)
(defparameter *groups* nil)
(defvar *current-group* nil)

(defun current-group ()
  *current-group*)

(defun add-group (&optional switch-to-group)
  (let* ((name (intern (format nil "GROUP-~s" (incf *group-count*))
                      :keyword))
         (frame (make-frame :group-id name)))
    (setf (frame (children frame)) frame)
    (setf (gethash name *groups*) frame)
    (when switch-to-group
      (switch-to-group name))
    name))

(defun switch-to-group (id)
  (assert (group-exists-p id))
  (unless (eq id *current-group*)
    (when *current-group*
      (push *current-group* *last-group*))
    (setf *current-group* id)
    (%focus-some-buffer-in-frame (gethash id *groups*)))
  id)

(defun %focus-some-buffer-in-frame (frame)
  "this is a hack but will work for now, ideally I guess we should
store the focused buffer for each group. I will come back to this
though as sooner or later it will start to annoy me"
  (let ((buffer (%find-some-buffer frame)))
    (%switch-to-buffer-obj (frame buffer) buffer)))

(defun %find-some-buffer (frame)
  (etypecase frame
    (frame (with-slots (children) frame
             (etypecase children
               (list (find #'%find-some-buffer children))
               (buffer children))))
    (buffer frame)))

(defun kill-group (id)
  (assert (group-exists-p id))
  (when *last-group*
    (switch-to-group
     (or (pop *last-group*) (first (hash-table-keys *groups*)))))
  (when (= (hash-table-count *groups*) 0)
    (add-group t))
  nil)

(defun group-exists-p (id)
  (not (null (gethash id *groups*))))

(defun init ()
  (setf *group-count* 0)
  (setf *groups* (make-hash-table))
  (add-group t))

(init)

;;------------------------------------------------------------
