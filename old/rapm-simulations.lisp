;;; ================================================================
;;; RAPM SIMULATIONS
;;; ----------------------------------------------------------------
;;; Contains code for running simulations of the RAPM model
;;; ----------------------------------------------------------------

(defun simulate (n &optional (res-file "results.csv"))
  (with-open-file (file res-file
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
    (let ((names (list 'd1 'ticks 'alpha 'egs 'accuracy 'problem 'choice)))
      (format file "~{~a~^, ~}~%" names))

    (dolist (d1 '(1/10 1/5 1 5 10))
      (dolist (ticks '(10 15 20))
	(dolist (alpha '(2/10 4/10 6/10 8/10 1))
	  (dolist (egs '(1/10 2/10 3/10 4/10 5/10))
	    (format t "~{~a~^, ~}~%" (list 'd1 d1 'ticks ticks 'alpha alpha 'egs egs))
	    (dotimes (j n)
	      (rapm-reload)  ; Reload
	      (setf *d1* d1)
	      (setf *ticks* ticks)
	      (sgp-fct `(:egs ,egs :alpha ,alpha :v nil)) ; Sets the params
	      (run 1000 :real-time nil)
	      (let* ((trial (first (experiment-log (current-device))))
		     (res (list d1 ticks alpha egs
				(trial-accuracy trial)
				(trial-problem-rt trial)
				(trial-choice-rt trial))))
		(format file "~{~a~^, ~}~%" (mapcar #'float res))))))))))


(defun simulate-d2 (n &key (vals '(1/2 1 3/2 2 5/2 3 7/2 4)) (v nil))
  (let ((results nil))
    (dolist (d2 vals (reverse results))
      (setf *d2* d2)
      (let ((partial nil))
	(dotimes (j n)
	  (rapm-reload nil)  ; Reload
	  (sgp :v nil)
	  (no-output (run 10000 :real-time nil))
	  (when v
	    (format t "Time = ~A, N=~A~%" (mp-time) (length (experiment-log (current-device)))))
	  (push (apply #'mean
		       (mapcar #'trial-accuracy (experiment-log (current-device)))
		       )
		partial))
	(push (cons (float d2) (float (apply #'mean partial))) results)))))
    

(defun simulate-d1 (n &key (vals '(1/2 1 3/2 2 5/2 3 7/2 4)))
  (let ((results nil))
    (dolist (d1 vals (reverse results))
      (setf *d1* d1)
      (let ((partial nil))
	(dotimes (j n)
	  (rapm-reload nil)  ; Reload
	  (sgp :v nil)
	  (no-output (run 10000 :real-time nil))
	  ;(print (list *d1* j (index (current-device)) (mp-time)))
	  (push (apply #'mean
		       (mapcar #'trial-accuracy (experiment-log (current-device)))
		       )
		partial))
	(push (cons (float d1) (float (apply #'mean partial))) results)))))

