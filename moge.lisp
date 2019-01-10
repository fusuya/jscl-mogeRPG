;; (defun moge ()
;;   (labels ((rgb (r g b)
;;              (format nil "rgb(~A,~A,~A)"
;;                      (truncate (* 255 r))
;;                      (truncate (* 255 g))
;;                      (truncate (* 255 b)))))
;;     (let* ((canvas (#j:document:getElementById "screen"))
;; 	   (ctx ((jscl::oget canvas "getContext") "2d")))
;;       ((jscl::oget ctx "fillRect") 420 440 50 100)
;;       (setf (jscl::oget ctx "strokeStyle") (apply #'rgb (list 0 0 255))
;; 	    (jscl::oget ctx "fillStyle") (rgb 255 0 0))
;;       ((jscl::oget ctx "strokeRect") 500 480 100 50)
;;       ((jscl::oget ctx "arc") 150 75 60 pi (* pi 2) t)
;;       ((jscl::oget ctx "fill"))))
;;   (#j:window:requestAnimationFrame #'moge))

(defun test ()
  (#j:alert "hoge"))

(defstruct moge
  (x 3)
  (y 3))

(defparameter *map-w* 640)
(defparameter *map-h* 432)

(defparameter *tate* 9)
(defparameter *yoko* 13)

(defparameter *yn-waku-x* 480)
(defstruct donjon
  (map (make-array (* *tate* *yoko*) :initial-element 1))
  (stop-list nil)
  (tate 10)
  (yoko 13))

(defstruct keystate
  (z nil)
  (x nil)
  (up nil)
  (down nil)
  (right nil)
  (left nil))

(defparameter *keystate* (make-keystate))
(defparameter *kabe* nil)
(defparameter *map* (make-donjon))

(defun rnd (min max)
  (let ((n (#j:Math:ceil min))
	(x (#j:Math:floor max)))
    (+ (#j:Math:floor (* (#j:Math:random) (- x n))) n)))

(defun rand1234 ()
  (let ((hoge nil))
    (loop until (>= (length hoge) 4)
	  do (let ((n (rnd 0 4)))
	       (when (null (find n hoge))
		 (push n hoge))))
    hoge))

(defun recursion (y x map)
  (let ((lst (rand1234))
	(stop? t))
    (loop for i in lst do
      (case i
	(1 ;;上
	 (when (and (< 0 (- y 2)) ;;2マス先が迷路の外か
		    (= (aref (donjon-map map) (+ (* (donjon-yoko map) (- y 2)) x)) 1)) ;;2マス先が通路か
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) (- y 2)) x)) 0)
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) (- y 1)) x)) 0)
	   (setf stop? nil)
	   (recursion (- y 2) x map)))
	;;(return))
	(2 ;;下
	 (when (and (> (1- (donjon-tate map)) (+ y 2)) ;;2マス先が迷路の外か 
		    (= (aref (donjon-map map) (+ (* (donjon-yoko map) (+ y 2)) x)) 1))
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) (+ y 2)) x)) 0)
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) (+ y 1)) x)) 0)
	   (setf stop? nil)
	   (recursion (+ y 2) x map)))
	;;(return))
	(3 ;;右
	 (when (and (> (1- (donjon-yoko map)) (+ x 2)) ;;2マス先が迷路の外か 
		    (= (aref (donjon-map map) (+ (* (donjon-yoko map) y) (+ x 2))) 1))
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) y) (+ x 2))) 0)
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) y) (+ x 1))) 0)
	   (setf stop? nil)
	   (recursion y (+ x 2) map)))
	;;(return))
	(0 ;;左
	 (when (and (< 0 (- x 2)) ;;2マス先が迷路の外か
		    (= (aref (donjon-map map) (+ (* (donjon-yoko map) y) (- x 2))) 1))
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) y) (- x 2))) 0)
	   (setf (aref (donjon-map map) (+ (* (donjon-yoko map) y) (- x 1))) 0)
	   (setf stop? nil)
	   (recursion y (- x 2) map)))))
    (when stop? ;;行き止まりだったら 
      ;;(scr-format "y=~d x=~d~%" y x);;テスト用
      (push (list y x) (donjon-stop-list map)) ;;行き止まりの座標リスト
      (setf (aref (donjon-map map) (+ (* (donjon-yoko map) y) x)) 3))))

(defparameter *moge* (make-moge))

(recursion (moge-y *moge*) (moge-x *moge*) *map*)

(defun render-clock (cr w h kabe-img moge-img)
  (labels (
           (rgb (r g b)
             (format nil "rgb(~A,~A,~A)"
                     (truncate (* 255 r))
                     (truncate (* 255 g))
                     (truncate (* 255 b))))
           (draw-bg ()
             (setf (jscl::oget cr "fillStyle")
                   (rgb 0 0 0))
             ((jscl::oget cr "fillRect") 0 0 w h))
	   (draw-msg-waku ()
	     (setf (jscl::oget cr "strokeStyle")
		   (rgb 1 1 1))
	     ((jscl::oget cr "strokeRect") 0 (+ 10 *map-h*) 470 60))
	   (yesno-waku ()
	     (setf (jscl::oget cr "strokeStyle")
		   (rgb 1 1 1))
	     ((jscl::oget cr "strokeRect") 480 (+ 10 *map-h*) 100 80))
	   (draw-text ()
	     (setf (jscl::oget cr "font")
		   (format nil "italic 40px Arial")
		   (jscl::oget cr "fillStyle") (format nil "green"))
	     ((jscl::oget cr "fillText") "test" 10 50))
	   (draw-moge ()
	     ((jscl::oget cr "drawImage") moge-img (* 48 (moge-x *moge*))
	      (* 48 (moge-y *moge*)) 48 48))
	   (draw-map ()
	     (dotimes (i *tate*)
	       (dotimes (j *yoko*)
		 (if (= (aref (donjon-map *map*) (+ (* i *yoko*) j)) 1)
		     ((jscl::oget cr "drawImage") kabe-img (* j 48)
		      (* i 48) 48 48)))))
		   
	   (draw-rect ()
	     (setf (jscl::oget cr "fillStyle") (rgb 0 0.3 1)
		   (jscl::oget cr "strokeStyle") (rgb 1 0 0))
	     ((jscl::oget cr "fillRect") (rect-x *rect*) (rect-y *rect*)
	      (rect-width *rect*) (rect-height *rect*))))
    (draw-bg)
    (draw-map)
    (draw-msg-waku)
    (yesno-waku)
    (draw-moge)))



(defun ido ()
  (cond
    ((keystate-up *keystate*)
     (when (null (kabe? (moge-x *moge*) (1- (moge-y *moge*))))
       (decf (moge-y *moge*))))
    ((keystate-down *keystate*)
     (when (null (kabe? (moge-x *moge*) (1+ (moge-y *moge*))))
       (incf (moge-y *moge*))))
    ((keystate-left *keystate*)
     (when (null (kabe? (1- (moge-x *moge*)) (moge-y *moge*)))
       (decf (moge-x *moge*))))
    ((keystate-right *keystate*)
     (when (null (kabe? (1+ (moge-x *moge*)) (moge-y *moge*)))
       (incf (moge-x *moge*))))))

(defun render-kabe-msg (ctx)
  (setf (jscl::oget ctx "font")
	(format nil "italic 20px Arial")
	(jscl::oget ctx "fillStyle") (format nil "green"))
  ((jscl::oget ctx "fillText") "壁を壊しますか？" 5 (+ 35 *map-h*)))

(defparameter *cursor* 0)
(defun render-yesno (ctx cursor)
    (let ((cursor-y (if (= cursor 0) 35 65)))
      (setf (jscl::oget ctx "font")
	    (format nil "italic 20px Arial")
	    
	    (jscl::oget ctx "fillStyle") (format nil "green"))
      ((jscl::oget ctx "fillText") "▶" (+ *yn-waku-x* 5) (+ cursor-y *map-h*))
      ((jscl::oget ctx "fillText") "はい" (+ *yn-waku-x* 30) (+ 35 *map-h*))
      ((jscl::oget ctx "fillText") "いいえ" (+ *yn-waku-x* 30) (+ 65 *map-h*))))


(defparameter *counter* 0)

(let ((cursor 0))
  (defun select-yn ()
    (cond
      ((keystate-up *keystate*)
       (if (= cursor 0)
	   (setf cursor 1)
	   (setf cursor 0)))
      ((keystate-down *keystate*)
       (if (= cursor 0)
	   (setf cursor 1)
	   (setf cursor 0)))
      ((keystate-z *keystate*)
       (setf *kabe* nil))
      ((keystate-x *keystate*)
       (setf *kabe* nil))))
  (defun iter (timestamp)
    (let* ((canvas (#j:document:getElementById "screen"))
	   (kabe-img (#j:document:getElementById "kabe"))
	   (moge-img (#j:document:getElementById "moge"))
	   (ctx ((jscl::oget canvas "getContext") "2d"))
	   (w (jscl::oget canvas "width"))
	   (h (jscl::oget canvas "height")))
      (incf *counter*)
      (render-clock ctx w h kabe-img moge-img)
      (cond
	(*kabe*
	 (render-kabe-msg ctx)
	 (render-yesno ctx cursor)
	 (when (zerop (mod *counter* 6))
	   (select-yn)))
	(t
	 (when (zerop (mod *counter* 6))
	   (ido))))
      )
    (#j:window:requestAnimationFrame #'iter)))

(#j:window:requestAnimationFrame #'iter)


(defparameter *canvas* (#j:document:getElementById "dom"))
(defparameter *btn* (#j:document:getElementById "ev"))
(defparameter *key* (#j:document:getElementById "canvas"))


(defun kabe? (x y)
  (if (= (aref (donjon-map *map*) (+ (* *yoko* y) x)) 1)
      (setf *kabe* t) nil))



(defun keydown-ev (event)
  ((jscl::oget event "preventDefault"))
  (cond
    ((equal (jscl::oget event "key") "ArrowUp")
     (setf (keystate-up *keystate*) t))
    ((equal (jscl::oget event "key") "ArrowDown")
     (setf (keystate-down *keystate*) t))
    ((equal (jscl::oget event "key") "ArrowLeft")
     (setf (keystate-left *keystate*) t))
    ((equal (jscl::oget event "key") "ArrowRight")
     (setf (keystate-right *keystate*) t))
    ((equal (jscl::oget event "key") "z")
     (setf (keystate-z *keystate*) t))
    ((equal (jscl::oget event "key") "x")
     (setf (keystate-x *keystate*) t)))
  )

(defun keyup-ev (event)
  ((jscl::oget event "preventDefault"))
  (cond
    ((equal (jscl::oget event "key") "ArrowUp")
     (setf (keystate-up *keystate*) nil))
    ((equal (jscl::oget event "key") "ArrowDown")
     (setf (keystate-down *keystate*) nil))
    ((equal (jscl::oget event "key") "ArrowLeft")
     (setf (keystate-left *keystate*) nil))
    ((equal (jscl::oget event "key") "ArrowRight")
     (setf (keystate-right *keystate*) nil))
    ((equal (jscl::oget event "key") "z")
     (setf (keystate-z *keystate*) nil))
    ((equal (jscl::oget event "key") "x")
     (setf (keystate-x *keystate*) nil))))

(setf #j:window:document:onkeydown
      #'keydown-ev)

(setf #j:window:document:onkeyup
      #'keyup-ev)

#|
(defun btn-func (event)
  (#j:alert "hoge"))

(setf (jscl::oget *btn* "onclick") #'btn-func)

(defun fnc1 (event)
  (setf (jscl::oget *canvas* "innerHTML") (format nil "HOGEGE")))

(defun fnc2 (event)
  (setf (jscl::oget *canvas* "innerHTML") (format nil "moge")))

(setf (jscl::oget *canvas* "onmouseover") #'fnc1
      (jscl::oget *canvas* "onmouseout") #'fnc2)
|#

