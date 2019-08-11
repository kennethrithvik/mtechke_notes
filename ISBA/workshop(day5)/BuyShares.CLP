;;This program is written by Charles Pang, 2018, ISS NUS (Copyright)

;; to store the current goal - "buy-shares"
(deftemplate current_goal 
	(slot goal) (slot cf))

;; to store the current facts
;; pays-dividends, good-mgmnt, positive-earnings, signing-large-contract, Penny-stocks
(deftemplate current_fact 
	(slot fact) (slot cf))

;; to load all the facts so that you dont have to type them in
;; For Task 2, this part will be removed - the user will be prompted to enter them
;; and you will assert the current_fact as you read them from the user
(deffacts load-facts
	(current_fact (fact signing-large-contract) (cf 0.8))
	(current_fact (fact pays-dividends) (cf 0.9))
	(current_fact (fact good-mgmnt) (cf 0.7))
	(current_fact (fact positive-earnings) (cf 0.5))
	(current_fact (fact penny-stocks) (cf 1.0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Rule-combining - refer to coursenotes (slide44)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-positive-cf  ;; formula 1
	?f1 <- (current_goal (goal ?g)(cf ?cf1&:(>= ?cf1 0)))
  	?f2 <- (current_goal (goal ?g)(cf ?cf2&:(>= ?cf2 0)))
	(test (neq ?f1 ?f2)) ;; compares pointers not value
  =>
  	(retract ?f1)
  	(modify ?f2 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
)
;;;;;;;;;;;;;;;;;;;; BUSINESS RULES (Slide43) ;;;;;;;;;;;;;;;;

;(defrule RULE-1 ..... )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if signing-large-contract THEN buy-shares (1.0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule RULE-2
  	(current_fact (fact signing-large-contract) (cf ?cf-A)) 
=>	(assert (current_goal (goal buy-shares) (cf (* ?cf-A 1.0))))) ; study this line carefully

;(defrule RULE-3 ..... )
