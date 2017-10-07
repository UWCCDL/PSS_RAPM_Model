;;; ================================================================
;;; RAPM SIMULATIONS
;;; ----------------------------------------------------------------
;;; Contains code for running simulations of the RAPM model
;;; ----------------------------------------------------------------
;;; Author: Andrea Stocco,
;;;       : University of Washington
;;;       : Seattle, WA, 98195
;;;       : stocco@uw.edu
;;; ------------------------------------------------------------------

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


(defun simulate-d2 (n &key
			(vals '(1/2 1 3/2 2 5/2 3 7/2 4))
			(verbose nil))
  "Quickly simulates the effects of different D2 values"
  (let ((results nil))
    (dolist (d2 vals (reverse results))
      (setf *d2* d2)
      (let ((partial nil))
	(dotimes (j n)
	  (rapm-reload nil)  ; Reload
	  (sgp :v nil)
	  (no-output (run 10000 :real-time nil))
	  (when verbose
	    (format t "~A problems attempted at time ~a~%"
		    (length (experiment-log (current-device)))
		    (mp-time)))
	  (push (apply #'mean
		       (mapcar #'trial-accuracy (experiment-log (current-device))))
		partial))
	(push (cons (float d2)
		    (float (apply #'mean partial)))
	      results)))))
    

(defun simulate-d1 (n &key
			(vals '(1/2 1 3/2 2 5/2 3 7/2 4))
			(verbose nil))
  "Quickly simulates the effects of different D1 values"
  (let ((results nil))
    (dolist (d1 vals (reverse results))
      (setf *d1* d1)
      (let ((partial nil))
	(dotimes (j n)
	  (rapm-reload nil)  ; Reload
	  (sgp :v nil)
	  (no-output (run 10000 :real-time nil))
	  (when verbose
	    (format t "~A problems attempted at time ~a~%"
		    (length (experiment-log (current-device)))
		    (mp-time)))
	  (push (apply #'mean
		       (mapcar #'trial-accuracy (experiment-log (current-device))))
		partial))
	(push (cons (float d1)
		    (float (apply #'mean partial)))
	      results)))))


(defun general-simulations (n &key
				(fname "simulations-devel4-model2.txt")
				(tickvals '(20 25 30 35)))
  "General simulations across all parameters of interest"
  (with-open-file (out fname
		       :direction :output
		       :if-exists :overwrite
		       :if-does-not-exist :create)
    (let ((names '(ticks #|pos-reward neg-reward|# alpha init-value-uppr-bound d1 d2 accuracy problem-rt reward-bold rpe-bold)))
      (format out "~{~a~^, ~}~%" names)
      (dolist (ticks tickvals) 
	;;	(dolist (pos-rwrd '(4 6 8 10)) ;;'(2 4 6 8 10))
	;;	(dolist (neg-rwrd '(-0.5 -1 -1.5 -2))
	(dolist (alpha '(0.025 0.050 0.075 0.100))
	  (dolist (uppr-bnd '(1.0 2.0 3.0 4.0)) ;;'(1 2 3 4))
	    (dolist (d1 '(0.1 0.5 1 2 5 10))
	      (dolist (d2 '(0.1 0.5 1 2 5 10))
		(setf *d1* d1)
		(setf *d2* d2)
		(setf *initial-value-upper-bound* uppr-bnd)
		;;(setf *negative-reward* neg-rwrd)
		;;  (setf *positive-reward* pos-rwrd)
		(setf *ticks* ticks)
		(dotimes (j n)
		  (rapm-reload nil)  ; Reload
		  (sgp :v nil)
		  (reset-striatal-activity)
		  (no-output (run 10000 :real-time nil))		  
		  (let* ((accuracy (float (apply #'mean
						 (mapcar #'trial-accuracy
							 (experiment-log
							  (current-device))))))
			 (problem-rt (float (apply #'mean
						   (mapcar #'trial-problem-rt
							   (experiment-log
							    (current-device))))))
			 (reward-bold (cdr (assoc 'problem (cdr (assoc 'reward *striatal-activity*)))))
			 (rpe-bold (cdr (assoc 'problem (cdr (assoc 'reward *striatal-activity*)))))
			 (vals (list ticks #|pos-rwrd neg-rwrd |# alpha uppr-bnd d1 d2 accuracy problem-rt reward-bold rpe-bold)))
		    (format out "~{~a~^, ~}~%" (mapcar #'float vals))))))))))))
