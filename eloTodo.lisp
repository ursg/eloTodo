(ql:quickload 'infix-math)
(use-package 'infix-math)
(ql:quickload '#:com.inuoe.jzon)
(sb-ext:add-package-local-nickname '#:jzon '#:com.inuoe.jzon) 
(ql:quickload 'serapeum)
(ql:quickload 'uiop)

(defparameter *filename* "./todo.json")

(defclass item ()
  ((name 
     :initarg :name 
     :accessor item-name)
   (rating 
     :initarg :rating 
     :accessor item-rating :initform 1200)
   (matches 
     :initarg :matches 
     :accessor item-matches 
     :initform 0)
   (done 
     :initarg :done 
     :type boolean 
     :accessor item-done 
     :initform nil)))

(defmethod print-object ((foo item) out)
  (format out "[~A] ~A (~A): ~A" (if (item-done foo) "X" " ") (item-rating foo) (item-matches foo) (item-name foo)))

; Read json input file
(defun load-json ()
  (with-open-file (in *filename*)
   (map 'list
        (lambda (i) 
         (make-instance 'item
           :name (gethash "name" i)
           :rating (gethash "rating" i)
           :matches (gethash "matches" i)
           :done (gethash "done" i)))
        (gethash "players" (jzon:parse in)))))

(defvar *ranking* (remove-if (lambda (i) (item-done i)) (load-json))) 

; Write json output file
(defun write-json () 
  (with-open-file (out *filename* :direction :output :if-exists :supersede)
    (jzon:stringify (serapeum:dict "players" *ranking*) :stream out :pretty t)))

(defun sort-ranking ()
  "Sort the TODO-list by elo rating"
  (setf *ranking* (sort *ranking* #'(lambda (a b) (> (item-rating a) (item-rating b))))))

(defun clamp (v min max)
  (cond ((< v min) min)
        ((>= v max) (- max 1))
        (t v)))

; --------------- Competition handling ----------------
(defvar *current-compo* (list (first *ranking*) (second *ranking*)))

; TODO: Bail out if only one entry in the ranking
(defun new-compo ()
  (let ((i (random (length *ranking*)))
        (j (random (length *ranking*))))
    (if (eq i j) 
      (new-compo)
      (setf *current-compo* 
            (list (elt *ranking* i) (elt *ranking* j))))))

; Adjust elo score rating of two items after a match
(defun score (winner loser)
   (let* ((R_a (item-rating winner))
          (R_b (item-rating loser))
          (K 32)
          (E_a ($ 1.0 / (1.0 + 10.0 ^ ((R_a - R_b) / 400.0))))
          (E_b ($ 1.0 / (1.0 + 10.0 ^ ((R_b - R_a) / 400.0)))))
      (setf (item-rating winner) (floor ($ R_a + K * (1. - E_a))))
      (incf (item-matches winner))
      (setf (item-rating loser)  (floor ($ R_b + K * (0. - E_b))))
      (incf (item-matches loser))))

(defun score-compo (winner)
  (cond
     ((eq winner 0) (new-compo))
     ((> winner 0) (progn
                     (score (second *current-compo*) (first *current-compo*))
                     (new-compo)))
     ((< winner 0) (progn
                     (score (first *current-compo*) (second *current-compo*))
                     (new-compo)))
   (sort-ranking)
   (write-json)))


; --------------------- TUI --------------------------
(ql:quickload 'cl-tui)
(use-package 'cl-tui)

(defvar *cursor-index* 0)

; Colors
(defvar normal (color-pair (color 750 750 750) (color 0 0 0)))
(defvar inverse (color-pair (color 0 0 0) (color 1000 1000 1000)))
(defvar selection (color-pair (color 0 0 0) (color 1000 1000 0)))
(defvar done-color (color-pair (color 750 750 000) (color 0 0 0)))
(defvar winner (color-pair (color 500 1000 500) (color 0 0 0)))
(defvar loser (color-pair (color 1000 500 500) (color 0 0 0)))

(defmacro with-winnerloser (check &body body)
  `(cond 
     ((< 0 ,check) (with-attributes ((:color winner)) frame (progn ,@body)))
     ((> 0 ,check) (with-attributes ((:color loser)) frame (progn ,@body)))
     ((eq 0 ,check) (progn ,@body))))


; ------------- Choice dialog (current compo) ---------------
(defun choice-render (&key frame h w)
  (with-attributes ((:color normal)) frame
    (draw-box frame)
    (put-text frame (- h 1) 3 "[l/left/r/right: Choose winner, space: draw]"))
  (with-attributes ((:color inverse)) frame
    (put-text frame 0 3 "[Current competition:]"))
  (let ((name1 (item-name (first *current-compo*)))
        (name2 (item-name (second *current-compo*)))
        (points1 (item-rating (first *current-compo*)))
        (points2 (item-rating (second *current-compo*))))
    (with-winnerloser (- points1 points2)
      (put-text frame 
                  (floor (/ h 2))
                  (floor ($ (w / 4) - (length name1) / 2 - 3)) 
                  (format nil "[~4A] ~A" points1 name1)))
    (put-text frame
                (floor (/ h 2)) 
                (floor (/ w 2))
                "<=>")
    (with-winnerloser (- points2 points1)
      (put-text frame 
                  (floor (/ h 2)) 
                  (floor ($ (w / 2) + (w / 4) - (length name2) / 2 - 3))
                  (format nil "[~4A] ~A" points2 name2)))))

; ------------- Ranking dialog ---------------
; TODO: limit string width
(defvar *list-scroll* 0)
(defun ranking-render (&key frame h w)

  ; Frame and title
  (with-attributes ((:color normal)) frame
    (draw-box frame)
    (put-text frame (- h 1) 3 "[up/down: cycle list, d: done, u: undone, n: new item]"))
  (with-attributes ((:color inverse)) frame
    (put-text frame 0 3 "[TODO list items]"))

  ; Make sure the current selection is within the window
  (if (> (- *cursor-index* *list-scroll*) (- h 3))
    (setf *list-scroll* (clamp (- *cursor-index* h -3) 0 (length *ranking*))))
  (if (< (- *cursor-index* *list-scroll*) 0)
    (setf *list-scroll* (clamp *cursor-index* 0 (length *ranking*))))

  ; Put all items
  (loop for i from *list-scroll* below (length *ranking*)
        and row from 1 below (- h 1) do
      (let* ((item (elt *ranking* i))
             (done (item-done item))
             (color (cond
                      ((eq *cursor-index* i) selection)
                      (done done-color)
                      (t normal))))
          (with-attributes ((:color color)) frame
                             (put-text frame row 1
                                       (format nil "~3A: ~A" i item))))))
  

; ----------------- New item dialog ----------------
(defun new-item-render (&key frame h w)
  (draw-box frame))

(define-frame container (container-frame) :on :root)
(define-frame choice (simple-frame :render 'choice-render) :on container :h 7)
(define-frame ranking (simple-frame :render 'ranking-render) :on container)
(define-frame new-item (container-frame))
(define-frame input (edit-frame :prompt "New Item> ") :on new-item :h 1)

(defvar *keys* (list))

; -------------------- Initialization ---------------
(sort-ranking)
(new-compo)

; --------------------- Main loop ------------------
(with-screen (:colors)
    (loop 
       (refresh)
       (let ((key (read-key)))
          (case key
            (#\q (return))
            (#\Esc (return))
            (#\L (progn
                   (load-json)
                   (sort-ranking)))

            ; Cursor selection
            (:KEY-UP (setf *cursor-index* (clamp (- *cursor-index* 1) 0 (length *ranking*))))
            (:KEY-DOWN (setf *cursor-index* (clamp (+ *cursor-index* 1) 0 (length *ranking*))))
            (#\Newline (setf (item-done (elt *ranking* *cursor-index*)) t))

            ; Scroll list
            (:KEY-NPAGE (setf *cursor-index* (clamp (+ *cursor-index* 20) 0 (length *ranking*))))
            (:KEY-PPAGE (setf *cursor-index* (clamp (- *cursor-index* 20) 0 (length *ranking*))))

            ; Done and undone
            (#\d (setf (item-done (elt *ranking* *cursor-index*)) t))
            (#\u (setf (item-done (elt *ranking* *cursor-index*)) nil))
            (#\Newline (setf (item-done (elt *ranking* *cursor-index*)) (not (item-done (elt *ranking* *cursor-index*)))))

            ; New item
            (#\n (progn
                   (display 'new-item)
                   (loop
                     (refresh)
                     (let ((key (read-key)))
                       (case key
                         (#\Esc (return))
                         (#\Newline (progn
                                      (push (make-instance 'item :name (get-text 'input)) *ranking*)
                                      (sort-ranking)
                                      ;(write-json)
                                      (return)))
                         (t (handle-key 'input key)))))
                   (display :root)))

            ; Compo time
            ((#\l :KEY-LEFT) (score-compo -1))
            ((#\r :KEY-RIGHT) (score-compo 1))
            (#\Space (score-compo 0))

            (otherwise (push (format nil "Key pressed: ~A" key) *keys*))))))

(loop for i in *keys* do
  (format t "~A~%" i))

(write-json)
(uiop:quit)
