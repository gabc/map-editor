(defpackage "MAP-EDITOR"
  (:use :clim :clim-lisp)
  (:export "MAP-EDITOR-MAIN"))

(in-package :map-editor)

(defvar *taille-carre* 30)
(defvar *click-val* 0)
(defvar *save-file*)
(defvar *save-format*)

(defclass node ()
    ((pos-x
      :initarg :pos-x
      :initform 0)
     (pos-y
      :initarg :pos-y
      :initform 0)
     (value
      :initarg :value
      :initform 0)))

(defvar *map* '())
(defvar *max-x* 5)
(defvar *max-y* 5)

(defun init-map ()
  (loop for i upto *max-x*
     do (loop for j upto *max-y*
	   do (push (make-instance 'node :pos-x i :pos-y j :value 0) *map*))))

(defclass canvas-pane (application-pane)
  ((first-point-x :initform nil)
   (first-point-y :initform nil)))

(define-application-frame map-editor ()
  ()
  ;; (:command-table (map-editor :inherit-from 
  ;; 			      (clim::accept-values-pane)))
  ;; (:command-definer t)
  (:panes
   (canvas (make-pane 'canvas-pane
		      ;; :accept-values 
		      :name 'canvas
		      :incremental-redisplay t
		      :display-function 'display-canvas
		      :display-after-commands t))
   (int :interactor
	:width 100
	:height 50))
  (:layouts
   (default canvas int)))

(defun display-canvas (frame pane)
  (draw)
  (fill-canvas))
  
(defun draw ()
  (loop for i from 0 upto 100
     do (draw-line* (find-pane-named *application-frame* 'canvas) (* i *taille-carre*) 0
		    (* i *taille-carre*) (* 200 *taille-carre*)))
  (loop for i from 0 upto 100
     do (draw-line* (find-pane-named *application-frame* 'canvas) 0 (* i *taille-carre*)
		    (* 500 *taille-carre*) (* i *taille-carre*))))

(defun draw-colored-square (x y color)
  (draw-rectangle* (find-pane-named *application-frame* 'canvas)
					    (+ (* x *taille-carre*) 1) (+ (* y *taille-carre*) 1)
					    (- (+ *taille-carre* (* x *taille-carre*)) 1) (- (+ *taille-carre* (* y *taille-carre*)) 1)
					    :ink color))

(defun fill-canvas ()
  (dolist (n *map*)
    (case (slot-value n 'value)
      (0 (draw-colored-square (slot-value n 'pos-x) (slot-value n 'pos-y) +blue+))
      (1 (draw-colored-square (slot-value n 'pos-x) (slot-value n 'pos-y) +green+))
      (2 (draw-colored-square (slot-value n 'pos-x) (slot-value n 'pos-y) +red+))
      (3 (draw-colored-square (slot-value n 'pos-x) (slot-value n 'pos-y) +grey+))
      (t (draw-colored-square (slot-value n 'pos-x) (slot-value n 'pos-y) +white+)))))

(define-map-editor-command (com-refresh :name t) ()
  ())

(define-map-editor-command (com-set-value :name t) ((n 'integer))
  (setf *click-val* n))

(define-map-editor-command (com-touche :name t) ()
  (handle-pointer (find-pane-named *application-frame* 'canvas) *click-val*))

;; (define-map-editor-command (com-set-size :name t) ((x 'integer) (y 'integer))
;;   (let ((dx (- x *max-x*))		;Changes we're asked to make
;; 	(dy (- y *max-y*)))
;;     (if (> number more-numbers...dx 0)				;Append x
;;        (dotimes (i dx)
;; 	 (incf *max-x*)
;; 	 (push (make-instance 'node :pos-x *max-x* :pos-y *max-y* :value 0) *map*)))
;;     (if (> dy 0)				;Append y
;;        (dotimes (i dy)
;; 	 (incf *max-y*)
;; 	 (push (make-instance 'node :pos-x *max-x* :pos-y *max-y* :value 0) *map*)))
;;     (if (and (< dx 0) (< dy 0)
;; 	(remove-if (lambda (n) (< (slot-value n 'pos-x) x)
;;   ))))))
  
(define-map-editor-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-map-editor-command (com-set-save :name t) ((s 'string))
  (setf *save-file* (merge-pathnames s)))

(define-map-editor-command (com-save :name t) ()
  (with-open-file (s *save-file* :direction :output :if-exists :supersede); :if-does-not-exists :create)
    (dolist (n *map*)
      (format s (or *save-format* "~a ~a ~a~%") (slot-value n 'pos-x) (slot-value n 'pos-y) (slot-value n 'value)))))

(define-map-editor-command (com-set-save-format :name t) ((s 'string))
  (setf *save-format* s))

(defun handle-pointer (pane value)
  (tracking-pointer (pane)
    (:pointer-button-release (&key event x y)
			     (change-map (truncate (/ x *taille-carre*)) (truncate (/ y *taille-carre*)) value)
			     (return-from handle-pointer))))

(defun resize-map (set n)
  (cond 
    ((= (length set) n) nil)
    ((> (length set) n)			; Plus grand
     (setf set (subseq set 0 n)))
    ((< (length set) n)			;Plus petit
     (dotimes (i (- n (length set)))
       (setf set (cons nil set)))))
  set)

(defun change-map (x y &optional value)
  (dolist (n *map*)
    (if (and
	 (= (slot-value n 'pos-x) x)
	 (= (slot-value n 'pos-y) y))
	(setf (slot-value n 'value) (or value (if (= 0 (nth x (nth y *map*))) 1 0))))))
  ;; (setf (nth x (nth y *map*)) (or value (if (= 0 (nth x (nth y *map*))) 1 0))))

(defun map-editor-main ()
  (run-frame-top-level (make-application-frame 'map-editor)))
