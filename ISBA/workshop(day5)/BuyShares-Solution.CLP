;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is written by Charles Pang, 2018, ISS NUS (Copyright)
;; This is the solution to the workshop
;; (1) It incorporates codes for negative-negative and postive-negative CF combinations
;; (2) It also prompts the user for input instead of using the passive deffacts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to store the current goal - buy-shares
(deftemplate current_goal (slot goal) (slot cf))

;; to store the current facts – 
;; pays-dividends, good-mgmnt, positive-earnings, signing-large-contract
(deftemplate current_fact (slot fact) (slot cf))

(deftemplate new_goal (slot goal) (slot cf))

;; initialise current goal when a new_goal is asserted
(defrule initialise-current-goal
	(not (current_goal (goal ?cg) (cf ?cfg)))
	?newg <- (new_goal (goal ?ng) (cf ?cfng))
=> 	(assert (current_goal (goal ?ng) (cf ?cfng)))
	(retract ?newg)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine POSITIVE (or ZERO) certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1- cf1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-positive-cf
	?f1 <- (current_goal (goal ?g)(cf ?cf1&:(>= ?cf1 0)))
	?f2 <- (new_goal (goal ?g)(cf ?cf2&:(>= ?cf2 0)))
	(test (neq ?f1 ?f2)) ;; compares pointers not value
  =>
 	(retract ?f2)
  	(modify ?f1 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
	(printout t "B-1= " ?cf1 crlf) ;;; for debugging
	(printout t "B-2= " ?cf2 crlf) ;;; for debugging
	(printout t "B1-B2 combined = " (+ ?cf1 (* ?cf2 (- 1 ?cf1))) crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine NEGATIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1+cf1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-negative-cf
 	(declare (salience -1))
	?f1 <- (current_goal (goal ?g)(cf ?cf1&:(< ?cf1 0)))
  	?f2 <- (new_goal (goal ?g)(cf ?cf2&:(< ?cf2 0)))
  	(test (neq ?f1 ?f2))
  =>
  	(retract ?f2)
  	(modify ?f1 (cf =(+ ?cf1 (* ?cf2 (+ 1 ?cf1)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine POSITIVE & NEGATIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = (cf1 + cf2)/ 1- MIN(|cf1|, |cf1|)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-pos-neg-cf
 	(declare (salience -1))
  	?f1 <- (current_goal (goal ?g) (cf ?cf1))
  	?f2 <- (new_goal (goal ?g) (cf ?cf2))
  	(test (neq ?f1 ?f2))
  	(test (< (* ?cf1 ?cf2) 0))
  =>
  	(retract ?f2)
  	(modify ?f1 (cf =(/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is an extra rule to get user input for the system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule rule-0
=>
	(printout t crlf "How certain are you that the company will pay good dividends :")
	(bind ?cf-c (read))
	(assert (current_fact (fact pays-dividends) (cf ?cf-c)))

	(printout t crlf crlf "How certain are you that the company has a good management team :")
	(bind ?cf-d (read))
	(assert (current_fact (fact good-mgmnt) (cf ?cf-d)))

	(printout t crlf crlf "How certain are you that the company will announce positive earnings :")
	(bind ?cf-e (read))
	(assert (current_fact (fact positive-earnings) (cf ?cf-e)))

	(printout t crlf crlf "How certain are you that company is securing a large contract :")
	(bind ?cf-a (read))
	(assert (current_fact (fact signing-large-contract) (cf ?cf-a)))
)

;; **********************    Business Rules    ******************* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IF pays-dividends & good-mgmnt & positive-earnings THEN buy-shares (0.6)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule rule-1
	(current_fact (fact pays-dividends) (cf ?cf-c))
	(current_fact (fact good-mgmnt) (cf ?cf-d))
	(current_fact (fact positive-earnings) (cf ?cf-e))
=>	(assert (new_goal (goal buy-shares) (cf (* (min ?cf-c ?cf-d ?cf-e) 0.6))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if signing-large-contract THEN buy-shares (0.9)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule rule-2
	(current_fact (fact signing-large-contract) (cf ?cf-A)) 
=> 	(assert (new_goal (goal buy-shares) (cf (* ?cf-A 0.9))))
)


