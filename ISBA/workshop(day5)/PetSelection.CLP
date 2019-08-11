;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is written by Charles Pang, 2018, ISS NUS (Copyright)
;; This is a personalized knowledge-based recommender system to help users select a good pet: 
;; rabbit, mice, fish, cat, dog. It is an interactive system that makes use of certainty factors. 
;; 
;; Progamming is an art more than a science. The solution here is not the most elegant and may
;; not be the most efficient ... but I have traded those for readability and simplicity in design
;; so that you can follow the english rules closely
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate current_fact (slot fact) (slot cf))

(deftemplate current_goal (slot goal) (slot cf)) ;; the top-level

(deftemplate working_goal (slot goal) (slot cf)) ;; 

(deftemplate recomendation 
	(slot rabbit) 
	(slot mice) 
	(slot fish) 
	(slot cat) 
	(slot dog))

;;; by default (initially) all pets have equal chance of being recommended 
;;; however, if you have knowledge that some pets are less popular
;;; you can give it a lower CF to begin with
(deffacts load-facts
	(current_fact (fact rabbit) (cf 0.5))
	(current_fact (fact mice) (cf 0.5))
	(current_fact (fact fish) (cf 0.5))
	(current_fact (fact cat) (cf 0.5))
	(current_fact (fact dog) (cf 0.5))

	(current_goal (goal rabbit) (cf 0.5))
	(current_goal (goal mice) (cf 0.5))
	(current_goal (goal fish) (cf 0.5))
	(current_goal (goal cat) (cf 0.5))
	(current_goal (goal dog) (cf 0.5))
)

;;; some global variables used in rules 4&5
(defglobal
	?*time-cf* = 0.0
	?*money-cf* = 0.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initialise current goal when a new_goal is asserted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defrule initialise-current-goal
;	(working_goal (goal ?ng) (cf ?cfng))
;	(not (current_goal (goal ?cg) (cf ?cfg)))
;	?newg <- (working_goal (goal ?ng) (cf ?cfng))
;=> 	(assert (current_goal (goal ?ng) (cf ?cfng)))
;	(retract ?newg)
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CF combination rules
;;; understand how it works - and then treat this as a black-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;combine POSITIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1- cf1)

(defrule combine-positive-cf
  ?f1 <- (current_goal (goal ?g)(cf ?cf1&:(>= ?cf1 0)))
  ?f2 <- (working_goal (goal ?g)(cf ?cf2&:(>= ?cf2 0)))
  (test (neq ?f1 ?f2)) ; test pointers and not value
  =>
  (retract ?f2)
  (modify ?f1 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
)

;combine NEGATIVE cf
;cf(cf1,cf2) = cf1 + cf2 * (1 + cf1)

(defrule combine-neg-cf
 (declare (salience -1))
  ?f1 <- (current_goal   (goal ?g)(cf ?cf1&:(< ?cf1 0)))
  ?f2 <- (working_goal (goal ?g)(cf ?cf2&:(< ?cf2 0)))
  (test (neq ?f1 ?f2))
  =>
  (retract ?f2)
  (modify ?f1 (cf =(+ ?cf1 (* ?cf2 (+ 1 ?cf1)))))
)

;combine one POSITIVE and one NEGATIVE
;cf(cf1,cf2) = (cf1 + cf2) / 1 - MIN[abs(cf1),abs(cf2)]

(defrule neg-pos-cf
 (declare (salience -1))
  ?f1 <- (current_goal (goal ?g) (cf ?cf1))
  ?f2 <- (working_goal (goal ?g) (cf ?cf2))
  (test (neq ?f1 ?f2))
  (test (< (* ?cf1 ?cf2) 0))
  =>
  (retract ?f2)
  (modify ?f1 (cf =(/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BUSINESS RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;**** Rule 1: Ask user preference for size of pets.
;**** this will increase the CF of your preferred pet (based on size)
(defrule small_pet
	(declare (salience 10))
	(current_fact (fact rabbit) (cf ?cf-rabbit))
	(current_fact (fact mice) (cf ?cf-mice))
	(current_fact (fact fish) (cf ?cf-fish))
	(current_fact (fact cat) (cf ?cf-cat))
	(current_fact (fact dog) (cf ?cf-dog))
=>	(printout t crlf "Do you like small, midsize or large pets? (s/m/l)") 
	(bind ?response (read))
	(switch ?response
		(case s then 	(assert (working_goal (goal rabbit) (cf (* ?cf-rabbit 0.5))))
				(assert (working_goal (goal mice) (cf (* ?cf-mice 0.8))))
				(assert (working_goal (goal fish) (cf (* ?cf-fish 1.0)))))
		(case m then 	(assert (working_goal (goal rabbit) (cf (* ?cf-rabbit 0.8))))	
				(assert (working_goal (goal cat) (cf (* ?cf-cat 0.6))))
				(assert (working_goal (goal dog) (cf (* ?cf-dog 0.2)))))
		(case l then 	(assert (working_goal (goal cat) (cf (* ?cf-cat 0.5))))
				(assert (working_goal (goal dog) (cf (* ?cf-dog 0.9)))))
	)
)

;**** Rule2:
(defrule good_company
	(current_fact (fact cat) (cf ?cf-cat))
	(current_fact (fact dog) (cf ?cf-dog))
	(current_fact (fact rabbit) (cf ?cf-rabbit))
	(current_fact (fact fish) (cf ?cf-fish))
	(current_fact (fact mice) (cf ?cf-mice))
=>	(printout t crlf "Do you like to pets that follow you around? (y/n)") 
	(bind ?response (read))
	(if (eq ?response y) then 
		(assert (working_goal (goal cat) (cf (* ?cf-cat 0.7))))
		(assert (working_goal (goal dog) (cf (* ?cf-dog 1.0))))
		(assert (working_goal (goal rabbit) (cf (* ?cf-rabbit 0.3))))
	else 	(assert (working_goal (goal mice) (cf (* ?cf-mice 0.9))))
		(assert (working_goal (goal fish) (cf (* ?cf-fish 0.9))))
	)
)

;**** Rule3:

;**** Rule4: 

;**** Rule5: 

;**** Rule6: 

;**** Print out the final results
; this is not an elegant way to program - imagine if you have 30 current_goals!
; also note the output is not sorted by CF
(defrule compile_recommendations
	(declare (salience -10))
	(current_goal (goal rabbit) (cf ?cf-r))
	(current_goal (goal mice) (cf ?cf-m))
	(current_goal (goal fish) (cf ?cf-f))
	(current_goal (goal cat) (cf ?cf-c))
	(current_goal (goal dog) (cf ?cf-d))
=>	(assert (recomendation (rabbit ?cf-r) (mice ?cf-m) (fish ?cf-f) (cat ?cf-c) (dog ?cf-d)))
	(printout t crlf "Our recommendation is as follows :")
	(printout t crlf "Rabbits: " (integer (* ?cf-r 100)) "%")
	(printout t crlf "Mice   : " (integer (* ?cf-m 100)) "%")
	(printout t crlf "Fish   : " (integer (* ?cf-f 100)) "%")
	(printout t crlf "Cat    : " (integer (* ?cf-c 100)) "%")
	(printout t crlf "Dog    : " (integer (* ?cf-d 100)) "%" crlf)
)

