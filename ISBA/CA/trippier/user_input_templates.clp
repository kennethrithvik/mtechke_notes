; Templates, Functions and Rules based on the list of questions targeted towards user
;;------------------------------------------------------------------


;;;*****************
;;;* Configuration *
;;;*****************
   
; console, cgi, or gui
(defglobal ?*target* = gui) 

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate MAIN::text-for-id
   (slot id)
   (slot text))

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (multislot display-answers)
   (multislot display-results)
   (slot state (default middle)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is written by Team Trippier, 2018, ISS NUS (Copyright)
;; This code contains Templates, Functions and Rules based on the list of questions targeted towards user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(deftemplate current_goal (slot fact) (slot cf))
(deftemplate new_goal (slot fact) (slot cf))



   
;;;***************************
;;;* DEFFUNCTION DEFINITIONS *
;;;***************************

;; Search for the text-for-id fact
;; with the same id as ?id
(deffunction MAIN::find-text-for-id (?id)   
   (bind ?fact
      (find-fact ((?f text-for-id))
                  (eq ?f:id ?id)))
   (if ?fact
      then
      (fact-slot-value (nth$ 1 ?fact) text)
      else
      ?id))
      
(deffunction MAIN::translate-av (?values)
   ;; Create the return value
   (bind ?result (create$))
   ;; Iterate over each of the allowed-values
   (progn$ (?v ?values)
      ;; Find the associated text-for-id fact
      (bind ?nv
         (find-text-for-id ?v))
      ;; Add the text to the return value
      (bind ?result (create$ ?result ?nv)))
   ;; Return the return value
   ?result)

(deffunction MAIN::replace-spaces (?str)
   (bind ?len (str-length ?str))
   (bind ?i (str-index " " ?str))
   (while (neq ?i FALSE)
      (bind ?str (str-cat (sub-string 1 (- ?i 1) ?str) "-" (sub-string (+ ?i 1) ?len ?str)))
      (bind ?i (str-index " " ?str)))
   ?str)

(deffunction MAIN::sym-cat-multifield (?values)
   (bind ?rv (create$))
   (progn$ (?v ?values)
      (bind ?rv (create$ ?rv (sym-cat (replace-spaces ?v)))))
   ?rv)

(deffunction MAIN::multifield-to-delimited-string (?mv ?delimiter)
   (bind ?rv "")
   (bind ?first TRUE)
   (progn$ (?v ?mv)
      (if ?first
         then
         (bind ?first FALSE)
         (bind ?rv (str-cat ?v))
         else
         (bind ?rv (str-cat ?rv ?delimiter ?v))))
   ?rv)

; Generic function to get single slot integer response from a question
(deffunction q-and-a-integer (?question ?minimum)
	(printout t ?question)
	(bind ?answer (read))
	(while (or (not (integerp ?answer)) (< ?answer ?minimum))
		(printout "Please enter a number >= " ?minimum crlf)
		(printout t ?question)
		(bind ?answer (read))
	)
	return ?answer
)

; Generic function to get single slot symbol response from a question
(deffunction q-and-a-symbol (?question ?option)
	(printout t ?question)
	(bind ?answer (read))
	(while (not(member$ ?answer ?option))
		(printout "Incorrect choice! Please enter again." crlf)
		(printout t ?question)
		(bind ?answer (read))
	)
	return ?answer
)

; Generic function to get multislot symbol responses from a question
(deffunction q-and-a-multislot (?question ?option)
	(printout t ?question)
	(bind ?answer (explode$ (readline)))
	(while (not(member$ ?answer ?option))
		(printout "Incorrect choice! Please enter again." crlf)
		(printout t ?question)
		(bind ?answer (read))
	)
	return ?answer
)


;;;*****************
;;;* STATE METHODS *
;;;*****************
     

;;; GUI target (iOS and JNI)

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted greeting)
                     (state ?state)
                     (valid-answers yes)
                     (display-answers yes)
					 ))
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (response ?response)
                     (valid-answers ?valid-answers)
                     (display-answers ?display-answers)))
   (halt))
 
(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target gui))
                         (?display LEXEME))
   (assert (UI-state (display ?display)
                     (state ?state)
                     (valid-answers)
                     (display-answers)))
   (assert (conclusion))
   (halt))

;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""
  (not (greeting yes))
  =>
  (handle-state greeting
                ?*target*
                (find-text-for-id WelcomeMessage)
                greeting
                (create$)))
  
;;;***************
;;;* QUERY RULES *
;;;***************


; Q1 - Attire to airport
(defrule q1 ""
   (greeting yes)
   (not (q1 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q1a1 q1a2 q1a3 q1a4 q1a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que1)
                 q1
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)

; Q2 - Choice of movie on a long flight
(defrule q2 ""   
   (not (q2 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q2a1 q2a2 q2a3 q2a4 q2a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que2)
                 q2
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)

; Q3 - Choice for First Date
(defrule q3 ""   
   (not (q3 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q3a1 q3a2 q3a3 q3a4 q3a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que3)
                 q3
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)

; Q4 - Nature makes me _______
(defrule q4 ""   
   (not (q4 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q4a1 q4a2 q4a3 q4a4 q4a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que4)
                 q4
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)
   
; Q5 - Number - One item in the bucket list :
(defrule q5 ""   
   (not (q5 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q5a1 q5a2 q5a3 q5a4 q5a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que5)
                 q5
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)


; Q6 - Saturdays are for _______ :
(defrule q6 ""   
   (not (q6 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q6a1 q6a2 q6a3 q6a4 q6a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que6)
                 q6
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)

; Q7 - What do you never leave home without _______ :
(defrule q7 ""   
   (not (q7 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q7a1 q7a2 q7a3 q7a4 q7a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que7)
                 q7
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)

; Q8 - If you could have dinner with one historical figure, who would it be? :
(defrule q8 ""   
   (not (q8 ?))
   (not (conclusion))
   =>
   (bind ?options (create$ q8a1 q8a2 q8a3 q8a4 q8a5))
   (handle-state interview
                 ?*target*
                 (find-text-for-id Que8)
                 q8
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)

; Get travel type base
(defrule travel-type-base ""
   ;(greeting yes)
   (not (travel_type_base ?))
   (not (conclusion))
   =>
   (bind ?options (create$ solo group))
   (handle-state interview
                 ?*target*
                 (find-text-for-id StartQuestion)
                 travel_type_base
                 (nth$ 1 ?options)
                 ?options
                 (translate-av ?options))
)

;(assert (traveller_count (travel_type_base ?travel_type_base)))

; Get solo age group
(defrule get-solo-age-group ""
	(travel_type_base solo)
	(not (age ?))
	(not (conclusion))
    =>
	(bind ?options (create$ 18-25 26-30 31-45 46-60 >60))
	(handle-state interview
                  ?*target*
                  (find-text-for-id AgeQuestion)
                  age
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
)
    ;(bind ?age (q-and-a-symbol "What is your age?: " ?options))
    ;(assert (average_adult_age_group (adult_age ?age)))
	;(assert (num_adults (adult_count 1)))


; Get travel type as a group
(defrule get-travel-type-group ""
	(travel_type_base group)
	(not (travel_type_group ?))
	(not (conclusion))
    =>
	(bind ?options (create$ family friends colleagues))
	(handle-state interview
                  ?*target*
                  (find-text-for-id GroupType)
                  travel_type_group
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
    ;(bind ?travel_type_group (q-and-a-symbol "What will your group comprise of?: " ?options))
    ;(assert (travel_group (travel_type_group ?travel_type_group)))
)

; Get adult count
(defrule get-adult-count ""
	(travel_type_base group)
	(travel_type_group ?)
	(not (adult_count ?))
	(not (conclusion))
    =>
	(bind ?options (create$ Num1 Num2 Num3 Num4 Num5 Num6 Num7 Num8 Num9 Num10))
	(handle-state interview
                  ?*target*
                  (find-text-for-id AdultCount)
                  adult_count
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
    ;(bind ?adult_count (q-and-a-integer "How many adults?: " 1))
    ;(assert (num_adults (adult_count ?adult_count)))
)

; Get kids count
(defrule get-kids-count ""
	(travel_type_base group)
	(travel_type_group family)
	(not (kids_count ?))
	(not (conclusion))
    =>
	(bind ?options (create$ Num0 1to2 3to4 5to6))
	(handle-state interview
                  ?*target*
                  (find-text-for-id KidsCount)
                  kids_count
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
    ;(bind ?kids_count (q-and-a-integer "Are there kids coming along? If yes, how many?: " 0))
    ;(assert (num_kids (kids_count ?kids_count)))
)

; Any kids below age of 7
(defrule get-kids-below-7
    (travel_type_base group)
	(travel_type_group family)
	(not (kids_count Num0))
	(not (kid_accepted ?))
	(not (conclusion))
    =>
	(bind ?options (create$ yes no))
	(handle-state interview
                  ?*target*
                  (find-text-for-id KidAccepted)
                  kid_accepted
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
;   (bind ?kid_accepted (q-and-a-symbol "Is kid below 7 years of age?: " ?options))
;    (assert (kid_non_acceptable_age (kid_age ?kid_accepted)))
)

; Get adult age group
(defrule get-average-adult-age-group ""
	(travel_type_base group)
	(not (adult_age ?))
	(not (conclusion))
    =>
	(bind ?options (create$ 18-25 26-30 31-45 46-60 >60))
	(handle-state interview
                  ?*target*
                  (find-text-for-id AdultAge)
                  adult_age
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
    ;(bind ?age (q-and-a-symbol "What is the average age of adults in the group?: " ?options))
    ;(assert (average_adult_age_group (adult_age ?age)))
)





; Get Themes
(defrule get-themes ""
(not (theme ?))
(not (conclusion))
    =>
	(bind ?options (create$ sports non_sports))
	(handle-state interview
                  ?*target*
                  (find-text-for-id Theme)
                  theme
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
    ;(bind ?theme (q-and-a-symbol "You won a ticket to a sports event?: " ?options))
    ;(assert (themes (theme ?theme)))
)

; Get vacation span
(defrule get-vacation-span ""
(not (travel_days ?))
(not (conclusion))
    =>
	(bind ?options (create$ 1to3 4to7 7to10 11to14))
	(handle-state interview
                  ?*target*
                  (find-text-for-id TravelDays)
                  travel_days
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
    ;(bind ?travel_days (q-and-a-symbol "How many days will you be on vacation?: " ?options))
    ;(assert (num_travel_days (travel_day ?travel_days)))
)


; Get vacation span
(defrule get-cuisine ""
(not (cuisine ?))
(not (conclusion))
    =>
	(bind ?options (create$ indian continental chinese japanese malay seafood singaporean cross_cultural))
	(handle-state interview
                  ?*target*
                  (find-text-for-id Cuisine)
                  cuisine
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
    ;(bind ?cuisine (q-and-a-symbol "What is your preferred cuisine?: " ?options))
    ;(assert (food_type (cuisine ?cuisine)))
)

; Get Insurance
(defrule rule-get-insurance ""
(not (get-insurance ?))
(current_goal (fact INSURANCE_TRUE) (cf ?x))
(current_goal (fact INSURANCE_MAYBE) (cf ?y))
(test (>= ?x ?y))
(not (conclusion))
    =>
	(bind ?options (create$ true maybe))
	(handle-state interview
                  ?*target*
                  (find-text-for-id getInsurance)
                  get-insurance
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))	
)

; Get Budget
(defrule get-budget ""
(not (get-budget ?))
(not (conclusion))
    =>
	(bind ?options (create$ economy mid_tier upscale luxury))
	(handle-state interview
                  ?*target*
                  (find-text-for-id getBudget)
                  get-budget
                  (nth$ 1 ?options)
                  ?options
                  (translate-av ?options))
	;(bind ?options (create$ <500 500-750 750-1000 1000-1500 1500-2000 2000-3000 3000-4000 4000-5000 >5000))
    ;(bind ?budget (q-and-a-symbol "What is your expected travel spend?: " ?options))
    ;(assert (travel_spend (budget ?budget)))
)

(defrule recommendation ""   
   (not (get-budget))
   =>
   (handle-state conclusion ?*target* (find-text-for-id recommendation)))




;;------------------------------------------------------------------


;; initialise current goal when a new_goal is asserted
(defrule initialise-current-goal
 	(declare (salience 50))
	?newg <- (new_goal (fact ?ng) (cf ?cfng))
=> 	(assert (current_goal (fact ?ng) (cf ?cfng)))
	(retract ?newg)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine POSITIVE (or ZERO) certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1- cf1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-positive-cf
 	(declare (salience 100))
	?f1 <- (current_goal (fact ?g)(cf ?cf1&:(>= ?cf1 0)))
	?f2 <- (new_goal (fact ?g)(cf ?cf2&:(>= ?cf2 0)))
	(test (neq ?f1 ?f2)) ;; compares pointers not value
  =>
  	(modify ?f1 (cf =(+ ?cf1 (* ?cf2 (- 1 ?cf1)))))
	(retract ?f2)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine NEGATIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = cf1 + cf2 * (1+cf1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-negative-cf
 	(declare (salience 100))
	?f1 <- (current_goal (fact ?g)(cf ?cf1&:(< ?cf1 0)))
  	?f2 <- (new_goal (fact ?g)(cf ?cf2&:(< ?cf2 0)))
  	(test (neq ?f1 ?f2))
  =>
  	(modify ?f1 (cf =(+ ?cf1 (* ?cf2 (+ 1 ?cf1)))))
	(retract ?f2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;combine POSITIVE & NEGATIVE certainty factors for multiple conclusions
;cf(cf1,cf2) = (cf1 + cf2)/ 1- MIN(|cf1|, |cf1|)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule combine-pos-neg-cf
  	(declare (salience 100))
  	?f1 <- (current_goal (fact ?g) (cf ?cf1))
  	?f2 <- (new_goal (fact ?g) (cf ?cf2))
  	(test (neq ?f1 ?f2))
  	(test (< (* ?cf1 ?cf2) 0))
  =>
  	(modify ?f1 (cf =(/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2))))))
	(retract ?f2)
)

;
; Rules for user input responses
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is written by Team Trippier, 2018, ISS NUS (Copyright)
;; CLIPS rules file containing list of rules for the following:
;; 1. Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; SOLO CHOICES
;

;IF TYPE = SOLO AND SPORTS != NULL THEN REVISED_SPORTS_WEIGHT = SPORTS + (0.0833*0.8 + 0.0833*0.7 + 0.1667*0.6 + 0.0833*0.5 + 0.01667*0.4 + 0*0.3 + 0.01667*0.2 + 0.25*0.1)
(defrule solo-sports "CF of solo player interested in sports"
	(travel_type_base solo)
	;(theme sports)
=>
	(bind ?sports_image_value 0.5)
 	(assert (new_goal (fact REVISED_SPORTS_WEIGHT) (cf (* ?sports_image_value 0.301622))))
)


;IF TYPE = SOLO AND NATURE != NULL THEN REVISED_NATURE_WEIGHT = 2*NATURE + (0.5833*0.8 + 0.25*0.7 + 0.0833*0.6 + 0*0.5 + 0.0833*0.4 + 0*0.3 + 0*0.2 + 0*0.1)
(defrule solo-nature "CF of solo player interested in nature"
	(travel_type_base solo)
	;(theme nature)
=>
	(bind ?nature_image_value 0.5)
 	(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf (* (* 1 ?nature_image_value) 0.72494))))
)


;IF TYPE = SOLO AND CULTURE != NULL THEN REVISED_CULTURE_WEIGHT = 2*CULTURE + (0*0.8 + 0.1667*0.7 + 0.1667*0.6 + 0.25*0.5 + 0*0.4 + 0.33*0.3 + 0.833*0.2 + 0*0.1)
(defrule solo-culture "CF of solo player interested in culture"
	(travel_type_base solo)
	;(theme culture)
=>
	(bind ?culture_image_value 0.5)
 	(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf (* (* 1 ?culture_image_value) 0.60731))))
)


;IF TYPE = SOLO AND HISTORY != NULL THEN REVISED_HISTORY _WEIGHT = 2*HISTORY + (0*0.8 + 0.833*0.7 + 0.1667*0.6 + 0.333*0.5 + 0.0833*0.4 + 0.0833*0.3 + 0.1667*0.2 + 0.0833*0.1)
(defrule solo-history "CF of solo player interested in history"
	(travel_type_base solo)
	;(theme history)
=> 
	(bind ?history_image_value 0.5)
 	(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf (* (* 1 ?history_image_value) 0.9496))))
)


;IF TYPE = SOLO AND SHOPPING != NULL THEN REVISED_SHOPPING_WEIGHT = 2*SHOPPING + (0.0833*0.8 + 0.0*0.7 + 0.0*0.6 + 0.0833*0.5 + 0.1667*0.4 + 0.0833*0.3 + 0.25*0.2 + 0.33*0.1)
(defrule solo-shopping "CF of solo player interested in shopping"
	(travel_type_base solo)
	;(theme lifestyle)
=> 
	(bind ?shop_image_value 0.5)
 	(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf (* (* 1 ?shop_image_value) 0.28296))))
)


;IF TYPE = SOLO AND THEME_PARK != NULL THEN REVISED_THEME_PARK_WEIGHT = 2*THEME_PARK + (0.0833*0.8 + 0.0*0.7 + 0.1667*0.6 + 0.0833*0.5 + 0.1667*0.4 + 0.0833*0.3 + 0.1667*0.2 + 0.25*0.1)
(defrule solo-theme-park "CF of solo player interested in theme park"
	(travel_type_base solo)
	;(theme adventure)
=> 
	(bind ?theme_park_image_value 0.5)
 	(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf (* (* 1 ?theme_park_image_value) 0.35832))))
)


;
; FAMILY CHOICES
;

;IF TYPE = FAMILY AND SPORTS != NULL THEN REVISED_SPORTS_WEIGHT = 2*SPORTS + (0.0556*0.8 + 0.0*0.7 + 0.0556*0.6 + 0.0*0.5 + 0.01667*0.4 + 0.056*0.3 + 0.01667*0.2 + 0.50*0.1)
(defrule family-sports "CF of family interested in sports"
	(travel_type_base group)
	(travel_type_group family)
	;(theme sports)
=>
	(bind ?sports_image_value 0.5)
 	(assert (new_goal (fact REVISED_SPORTS_WEIGHT) (cf (* (* 1 ?sports_image_value) 0.154642))))
)


;IF TYPE = FAMILY AND NATURE != NULL THEN REVISED_NATURE_WEIGHT = 2*NATURE + (0.111*0.8 + 0.5556*0.7 + 0.111*0.6 + 0*0.5 + 0.111*0.4 + 0.556*0.3 + 0.556*0.2 + 0*0.1)
(defrule family-nature "CF of family interested in nature"
	(travel_type_base group)
	(travel_type_group family)
	;(theme nature)
=>
	(bind ?nature_image_value 0.5)
 	(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf (* (* 1 ?nature_image_value) 0.86672))))
)


;IF TYPE = FAMILY AND CULTURE != NULL THEN REVISED_CULTURE_WEIGHT = 2*CULTURE + (0.0556*0.8 + 0.0556*0.7 + 0.0*0.6 + 0.2778*0.5 + 0.2778*0.4 + 0.1667*0.3 + 0.111*0.2 + 0.056*0.1)
(defrule family-culture "CF of family interested in culture"
	(travel_type_base group)
	(travel_type_group family)
	;(theme culture)
=>
	(bind ?culture_image_value 0.5)
 	(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf (* (* 1 ?culture_image_value) 0.41123))))
)


;IF TYPE = FAMILY AND HISTORY != NULL THEN REVISED_HISTORY _WEIGHT = 2*HISTORY + (0.111*0.8 + 0.0556*0.7 + 0.1667*0.6 + 0.111*0.5 + 0.0556*0.4 + 0.2778*0.3 + 0.1667*0.2 + 0.0556*0.1)
(defrule family-history "CF of family interested in history"
	(travel_type_base group)
	(travel_type_group family)
	;(theme history)
=> 
	(bind ?history_image_value 0.5)
 	(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf (* (* 1 ?history_image_value) 0.42772))))
)


;IF TYPE = FAMILY AND SHOPPING != NULL THEN REVISED_SHOPPING_WEIGHT = 2*SHOPPING + (0.0556*0.8 + 0.0*0.7 + 0.0556*0.6 + 0.2222*0.5 + 0.0556*0.4 + 0.2222*0.3 + 0.2222*0.2 + 0.1667*0.1)
(defrule family-shopping "CF of family interested in shopping"
	(travel_type_base group)
	(travel_type_group family)
	;(theme lifestyle)
=> 
	(bind ?shop_image_value 0.5)
 	(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf (* (* 1 ?shop_image_value) 0.33895))))
)


;IF TYPE = FAMILY AND THEME_PARK != NULL THEN REVISED_THEME_PARK_WEIGHT = 2*THEME_PARK + (0.0556*0.8 + 0.1667*0.7 + 0.1667*0.6 + 0.0556*0.5 + 0.1111*0.4 + 0.1111*0.3 + 0.1111*0.2 + 0.2222*0.1)
(defrule family-theme-park "CF of family interested in theme park"
	(travel_type_base group)
	(travel_type_group family)
	;(theme adventure)
=> 
	(bind ?theme_park_image_value 0.5)
 	(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf (* (* 1 ?theme_park_image_value) 0.4112))))
)


;
; FRIENDS CHOICES
;

;IF TYPE = FRIEND AND SPORTS != NULL THEN REVISED_SPORTS_WEIGHT = 2*SPORTS + (0.0714*0.8 + 0.0714*0.7 + 0.0714*0.6 + 0.1429*0.5 + 0.2143*0.4 + 0*0.3 + 0.0*0.2 + 0.0*0.1)
(defrule friends-sports "CF of friends interested in sports"
	(travel_type_base group)
	(travel_type_group friends)
	;(theme sports)
=>
	(bind ?sports_image_value 0.5)
 	(assert (new_goal (fact REVISED_SPORTS_WEIGHT) (cf (* (* 1 ?sports_image_value) 0.30711))))
)


;IF TYPE = FRIEND AND NATURE != NULL THEN REVISED_NATURE_WEIGHT = 2*NATURE + (0.4286*0.8 + 0.1429*0.7 + 0.2143*0.6 + 0.0714*0.5 + 0.0714*0.4 + 0*0.3 + 0*0.2 + 0.0714*0.1)
(defrule friends-nature "CF of friends interested in nature"
	(travel_type_base group)
	(travel_type_group friends)
	;(theme nature)
=>
	(bind ?nature_image_value 0.5)
 	(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf (* (* 1 ?nature_image_value) 0.64289))))
)


;IF TYPE = FRIEND AND CULTURE != NULL THEN REVISED_CULTURE_WEIGHT = 2*CULTURE + (0714*0.8 + 0.0*0.7 + 0.1429*0.6 + 0.2857*0.5 + 0.1429*0.4 + 0.2143*0.3 + 0.0714*0.2 + 0.0714*0.1)
(defrule friends-culture "CF of friends interested in culture"
	(travel_type_base group)
	(travel_type_group friends)
	;(theme culture)
=>
	(bind ?culture_image_value 0.5)
 	(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf (* (* 1 ?culture_image_value) 0.42858))))
)


;IF TYPE = FRIEND AND HISTORY != NULL THEN REVISED_HISTORY _WEIGHT = 2*HISTORY + (0.1429*0.8 + 0.0*0.7 + 0.0714*0.6 + 0.2143*0.5 + 0.1429*0.4 + 0.2143*0.3 + 0.0714*0.2 + 0.1429*0.1)
(defrule friends-history "CF of friends interested in history"
	(travel_type_base group)
	(travel_type_group friends)
	;(theme history)
=> 
	(bind ?history_image_value 0.5)
 	(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf (* (* 1 ?history_image_value) 0.41433))))
)


;IF TYPE = FRIEND AND SHOPPING != NULL THEN REVISED_SHOPPING_WEIGHT = 2*SHOPPING + (0.0*0.8 + 0.0*0.7 + 0.0714*0.6 + 0.0*0.5 + 0.0714*0.4 + 0.0714*0.3 + 0.2857*0.2 + 0.50*0.1)
(defrule friends-shopping "CF of friends interested in shopping"
	(travel_type_base group)
	(travel_type_group friends)
	;(theme lifestyle)
=> 
	(bind ?shop_image_value 0.5)
 	(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf (* (* 1 ?shop_image_value) 0.19996))))
)

;IF TYPE = FRIEND AND THEME_PARK != NULL THEN REVISED_THEME_PARK_WEIGHT = 2*THEME_PARK + (0.0714*0.8 + 0.1429*0.7 + 0.1429*0.6 + 0.0*0.5 + 0.1429*0.4 + 0.2857*0.3 + 0.1429*0.2 + 0.0714*0.1)
(defrule friends-theme-park "CF of friends interested in theme park"
	(travel_type_base group)
	(travel_type_group friends)
	;(theme adventure)
=> 
	(bind ?theme_park_image_value 0.5)
 	(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf (* (* 1 ?theme_park_image_value) 0.42148))))
)


;
; COLLEAGUES CHOICES
;

;IF TYPE = COLLEAGUES AND SPORTS != NULL THEN REVISED_SPORTS_WEIGHT = 2*SPORTS + (0.0714*0.8 + 0.0714*0.7 + 0.0714*0.6 + 0.1429*0.5 + 0.2143*0.4 + 0*0.3 + 0.0*0.2 + 0.0*0.1)
(defrule colleagues-sports "CF of colleagues interested in sports"
	(travel_type_base group)
	(travel_type_group colleagues)
	;(theme sports)
=>
	(bind ?sports_image_value 0.5)
 	(assert (new_goal (fact REVISED_SPORTS_WEIGHT) (cf (* (* 1 ?sports_image_value) 0.30711))))
)


;IF TYPE = COLLEAGUES AND NATURE != NULL THEN REVISED_NATURE_WEIGHT = 2*NATURE + (0.4286*0.8 + 0.1429*0.7 + 0.2143*0.6 + 0.0714*0.5 + 0.0714*0.4 + 0*0.3 + 0*0.2 + 0.0714*0.1)
(defrule colleagues-nature "CF of colleagues interested in nature"
	(travel_type_base group)
	(travel_type_group colleagues)
	;(theme nature)
=>
	(bind ?nature_image_value 0.5)
 	(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf (* (* 1 ?nature_image_value) 0.64289))))
)


;IF TYPE = COLLEAGUES AND CULTURE != NULL THEN REVISED_CULTURE_WEIGHT = 2*CULTURE + (0714*0.8 + 0.0*0.7 + 0.1429*0.6 + 0.2857*0.5 + 0.1429*0.4 + 0.2143*0.3 + 0.0714*0.2 + 0.0714*0.1)
(defrule colleagues-culture "CF of colleagues interested in culture"
	(travel_type_base group)
	(travel_type_group colleagues)
	;(theme culture)
=>
	(bind ?culture_image_value 0.5)
 	(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf (* (* 1 ?culture_image_value) 0.42858))))
)


;IF TYPE = COLLEAGUES AND HISTORY != NULL THEN REVISED_HISTORY _WEIGHT = 2*HISTORY + (0.1429*0.8 + 0.0*0.7 + 0.0714*0.6 + 0.2143*0.5 + 0.1429*0.4 + 0.2143*0.3 + 0.0714*0.2 + 0.1429*0.1)
(defrule colleagues-history "CF of colleagues interested in history"
	(travel_type_base group)
	(travel_type_group colleagues)
	;(theme history)
=> 
	(bind ?history_image_value 0.5)
 	(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf (* (* 1 ?history_image_value) 0.41433))))
)


;IF TYPE = COLLEAGUES AND SHOPPING != NULL THEN REVISED_SHOPPING_WEIGHT = 2*SHOPPING + (0.0*0.8 + 0.0*0.7 + 0.0714*0.6 + 0.0*0.5 + 0.0714*0.4 + 0.0714*0.3 + 0.2857*0.2 + 0.50*0.1)
(defrule colleagues-shopping "CF of colleagues interested in shopping"
	(travel_type_base group)
	(travel_type_group colleagues)
	;(theme lifestyle)
=> 
	(bind ?shop_image_value 0.5)
 	(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf (* (* 1 ?shop_image_value) 0.19996))))
)

;IF TYPE = COLLEAGUES AND THEME_PARK != NULL THEN REVISED_THEME_PARK_WEIGHT = 2*THEME_PARK + (0.0714*0.8 + 0.1429*0.7 + 0.1429*0.6 + 0.0*0.5 + 0.1429*0.4 + 0.2857*0.3 + 0.1429*0.2 + 0.0714*0.1)

(defrule colleagues-theme-park "CF of colleagues interested in theme park"
	(travel_type_base group)
	(travel_type_group colleagues)
	;(theme adventure)
=> 
	(bind ?theme_park_image_value 0.5)
 	(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf (* (* 1 ?theme_park_image_value) 0.42148))))
)


;
; Kid choices
;

; If kid is below 7 years of age then low priority to theme parks
(defrule kid-accepted-theme-parks "CF of kid below 7 years impacting theme parks"
	(kid_age yes)
=>
 	(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf -5.00)))
)


;
; AGE
;


;IF AGE = 18 - 25 AND SPORTS != NULL THEN REVISED_SPORTS_WEIGHT = 2*SPORTS + (0.0833*0.8 + 0.0833*0.7 + 0.1667*0.6 + 0.0833*0.5 + 0.01667*0.4 + 0*0.3 + 0.01667*0.2 + 0.25*0.1)
(defrule age-young-sports "CF of 18-25 interested in sports"
	(adult_age 18-25)
	;(theme sports)
=> 
	(bind ?sports_image_value 0.5)
 	(assert (new_goal (fact REVISED_SPORTS_WEIGHT) (cf (* (* 1 ?sports_image_value) 0.301622))))
)

;IF AGE = 26 - 30 AND NATURE != NULL THEN REVISED_NATURE_WEIGHT = 2*NATURE + (0.5833*0.8 + 0.25*0.7 + 0.0833*0.6 + 0*0.5 + 0.0833*0.4 + 0*0.3 + 0*0.2 + 0*0.1)
(defrule age-midyouth-nature "CF of 26-30 interested in nature"
	(adult_age 26-30)
	;(theme nature)
=> 
	(bind ?nature_image_value 0.5)
 	(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf (* (* 1 ?nature_image_value) 0.72494))))
)

;IF AGE = 31 - 45 AND CULTURE != NULL THEN REVISED_CULTURE_WEIGHT = 2*CULTURE + (0*0.8 + 0.1667*0.7 + 0.1667*0.6 + 0.25*0.5 + 0*0.4 + 0.33*0.3 + 0.833*0.2 + 0*0.1)
(defrule age-mid-culture "CF of 31-45 interested in culture"
	(adult_age 31-45)
	;(theme culture)
=> 
	(bind ?culture_image_value 0.5)
 	(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf (* (* 1 ?culture_image_value) 0.60731))))
)

;IF AGE = 46 - 60 AND HISTORY != NULL THEN REVISED_HISTORY _WEIGHT = 2*HISTORY + (0*0.8 + 0.833*0.7 + 0.1667*0.6 + 0.333*0.5 + 0.0833*0.4 + 0.0833*0.3 + 0.1667*0.2 + 0.0833*0.1)
(defrule age-old-history "CF of 46-60 interested in history"
	(adult_age 46-60)
	;(theme history)
=> 
	(bind ?history_image_value 0.5)
 	(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf (* (* 1 ?history_image_value) 0.9496))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is written by Team Trippier, 2018, ISS NUS (Copyright)
;; CLIPS rules file containing list of rules for the following:
;; 3. Number of attractions per day
;; 4. Number of restaurants per day
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; Based on AGE
;
;IF AGE = 18 – 25 THEN NUM_ATTRACTIONS = 2 (cf=0.4286)
;IF AGE = 18 – 25 THEN NUM_RESTAURANTS = 2 (cf=0.3571)
;IF AGE = 18 – 25 THEN NUM_ATTRACTIONS = 3 (cf=0.3571)
;IF AGE = 18 – 25 THEN NUM_RESTAURANTS = 3 (cf=0.4286)
(defrule age-num-attractions-restaurants-young "CF of 18-25 influencing number of attractions and restaurants per day"
	(adult_age 18-25)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.4286)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.3571)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.3571)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.4286)))
)

;IF AGE = 26– 30 THEN NUM_ATTRACTIONS = 2 (cf=0.3333)
;IF AGE = 26– 30 THEN NUM_RESTAURANTS = 2 (cf=0.7778)
;IF AGE = 26– 30 THEN NUM_ATTRACTIONS = 3 (cf=0.3333)
;IF AGE = 26– 30 THEN NUM_RESTAURANTS = 3 (cf=0.1111)
(defrule age-num-attractions-restaurants-midyouth "CF of 26-30 influencing number of attractions and restaurants per day"
	(adult_age 26-30)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.3333)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.7778)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.3333)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.1111)))
)

;IF AGE = 31- 45 THEN NUM_ATTRACTIONS = 2 (cf=0.1429)
;IF AGE = 31- 45 THEN NUM_RESTAURANTS = 2 (cf=0.50)
;IF AGE = 31 – 45 THEN NUM_ATTRACTIONS = 3 (cf=0.5714)
;IF AGE = 31 – 45 THEN NUM_RESTAURANTS = 3 (cf=0.2857)
(defrule age-num-attractions-restaurants-mid "CF of 31-45 influencing number of attractions and restaurants per day"
	(adult_age 31-45)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.1429)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.50)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.5714)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.2857)))
)

;IF AGE = 46-60 THEN NUM_ATTRACTIONS = 2 (cf=0.70)
;IF AGE = 46-60 THEN NUM_RESTAURANTS = 2 (cf=0.50)
;IF AGE = 46-60 THEN NUM_ATTRACTIONS = 3 (cf=0.20)
;IF AGE = 46-60 THEN NUM_RESTAURANTS = 3 (cf=0.50)
(defrule age-num-attractions-restaurants-old "CF of 46-60 influencing number of attractions and restaurants per day"
	(adult_age 46-60)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.70)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.50)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.20)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.50)))
)


;
; Based on number of days in vacation
;

;IF VACATION = 1-3 THEN NUM_ATTRACTIONS = 2 (cf=0.2857)
;IF VACATION = 1-3 THEN NUM_ATTRACTIONS = 3 (cf=0.4286)
;IF VACATION = 1-3 THEN NUM_RESTAURANTS = 1 (cf=0.3286)
;IF VACATION = 1-3 THEN NUM_RESTAURANTS = 2 (cf=0.2857)
;IF VACATION = 1-3 THEN NUM_RESTAURANTS = 3 (cf=0.2857)
(defrule num-attractions-restaurants-vacation-1 "CF of number of vacation days influencing number of attractions and restaurants per day"
	(travel_day 1-3)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.2857)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.2857)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.4286)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.2857)))
	(assert (new_goal (fact NUM_RESTAURANTS_1) (cf 0.3286)))
)

;IF VACATION = 4-6 THEN NUM_ATTRACTIONS = 2 (cf=0.1805)
;IF VACATION = 4-6 THEN NUM_ATTRACTIONS = 3 (cf=0.65)
;IF VACATION = 4-6 THEN NUM_RESTAURANTS = 2 (cf=0.45)
;IF VACATION = 4-6 THEN NUM_RESTAURANTS = 3 (cf=0.4013)
(defrule num-attractions-restaurants-vacation-2 "CF of number of vacation days influencing number of attractions and restaurants per day"
	(travel_day 4-6)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.1805)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.45)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.65)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.4013)))
)

;IF VACATION = 7-10 THEN NUM_ATTRACTIONS = 2 (cf=0.3684)
;IF VACATION = 7-10 THEN NUM_ATTRACTIONS = 3 (cf=0.3158)
;IF VACATION = 7-10 THEN NUM_RESTAURANTS = 2 (cf=0.5263)
;IF VACATION = 7-10 THEN NUM_RESTAURANTS = 3 (cf=0.2105)
(defrule num-attractions-restaurants-vacation-3 "CF of number of vacation days influencing number of attractions and restaurants per day"
	(travel_day 7-10)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.3684)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.5263)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.3158)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.2105)))
)

;IF VACATION = 11-14 THEN NUM_ATTRACTIONS = 2 (cf=0.3684)
;IF VACATION = 11-14 THEN NUM_ATTRACTIONS = 3 (cf=0.2533)
;IF VACATION = 11-14 THEN NUM_RESTAURANTS = 2 (cf=0.4141)
;IF VACATION = 11-14 THEN NUM_RESTAURANTS = 3 (cf=0.2105)
(defrule num-attractions-restaurants-vacation-4 "CF of number of vacation days influencing number of attractions and restaurants per day"
	(travel_day 11-14)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.3684)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.4141)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.2533)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.2105)))
)

;IF VACATION = 15-30 THEN NUM_RESTAURANTS = 1 (cf=0.3286)
;IF VACATION = 15-30 THEN NUM_ATTRACTIONS = 1 (cf=0.1342)
;IF VACATION = 15-30 THEN NUM_ATTRACTIONS = 2 (cf=0.3684)
;IF VACATION = 15-30 THEN NUM_ATTRACTIONS = 3 (cf=0.2533)
;IF VACATION = 15-30 THEN NUM_RESTAURANTS = 2 (cf=0.4141)
;IF VACATION = 15-30 THEN NUM_RESTAURANTS = 3 (cf=0.2105)
(defrule num-attractions-restaurants-vacation-5 "CF of number of vacation days influencing number of attractions and restaurants per day"
	(travel_day 15-30)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_1) (cf 0.1342)))
	(assert (new_goal (fact NUM_RESTAURANTS_1) (cf 0.3286)))
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.3684)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.4141)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.2533)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.2105)))
)


;
; Based on the cuisine
;
;IF CUISINE = chinese THEN NUM_ATTRACTIONS = 2 (cf=0.40)
;IF CUISINE = chinese THEN NUM_ATTRACTIONS = 3 (cf=0.60)
;IF CUISINE = chinese THEN NUM_RESTAURANTS= 2 (cf=0.40)
;IF CUISINE = chinese THEN NUM_RESTAURANTS = 3 (cf=0.40)
;IF CUISINE = chinese THEN NUM_RESTAURANTS = 4 (cf=0.75)
(defrule num-attractions-restaurants-cuisine-chinese "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine chinese)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.40)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.40)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.60)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.40)))
	(assert (new_goal (fact NUM_RESTAURANTS_4) (cf 0.75)))
)

;IF CUISINE = indian THEN NUM_ATTRACTIONS = 2 (cf=0.4545)
;IF CUISINE = indian THEN NUM_ATTRACTIONS = 3 (cf=0.3636)
;IF CUISINE = indian THEN NUM_RESTAURANTS = 2 (cf=0.2727)
;IF CUISINE = indian THEN NUM_RESTAURANTS = 3 (cf=0.4545)
(defrule num-attractions-restaurants-cuisine-indian "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine indian)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.4545)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.2727)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.3636)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.4545)))
)

;IF CUISINE = MALAY THEN NUM_ATTRACTIONS = 2 (cf=0.60)
;IF CUISINE = MALAY THEN NUM_ATTRACTIONS = 3 (cf=0.40)
;IF CUISINE = MALAY THEN NUM_RESTAURANTS= 1 (cf=0.3333)
;IF CUISINE = MALAY THEN NUM_RESTAURANTS= 2 (cf=0.3333)
;IF CUISINE = MALAY THEN NUM_RESTAURANTS= 3 (cf=0.3333)
(defrule num-attractions-restaurants-cuisine-malay "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine malay)
=> 
	(assert (new_goal (fact NUM_RESTAURANTS_1) (cf 0.3333)))
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.60)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.3333)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.40)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.3333)))
)

;IF CUISINE = japanese THEN NUM_ATTRACTIONS = 2 (cf=0.4167)
;IF CUISINE = japanese THEN NUM_ATTRACTIONS = 3 (cf=0.50)
;IF CUISINE = japanese THEN NUM_RESTAURANTS= 2 (cf=0.4167)
;IF CUISINE = japanese THEN NUM_RESTAURANTS= 3 (cf=0.25)
;IF CUISINE = japanese THEN NUM_RESTAURANTS= 4 (cf=0.60)
(defrule num-attractions-restaurants-cuisine-japanese "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine japanese)
=> 
	(assert (new_goal (fact NUM_RESTAURANTS_4) (cf 0.60)))
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.4167)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.4167)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.50)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.25)))
)

;IF CUISINE = continental THEN NUM_ATTRACTIONS = 2 (cf=0.25)
;IF CUISINE = continental THEN NUM_ATTRACTIONS = 3 (cf=0.50)
;IF CUISINE = continental THEN NUM_RESTAURANTS = 2 (cf=0.5833)
;IF CUISINE = continental THEN NUM_RESTAURANTS = 3 (cf=0.25)
(defrule num-attractions-restaurants-cuisine-continental "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine continental)
=> 
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.25)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.5833)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.50)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.25)))
)

;IF CUISINE = SEAFOOD THEN NUM_ATTRACTIONS = 2 (cf=0.2105)
;IF CUISINE = SEAFOOD THEN NUM_ATTRACTIONS = 3 (cf=0.4737)
;IF CUISINE = SEAFOOD THEN NUM_RESTAURANTS = 1 (cf=0.20)
;IF CUISINE = SEAFOOD THEN NUM_RESTAURANTS = 2 (cf=0.5263)
;IF CUISINE = SEAFOOD THEN NUM_RESTAURANTS = 3 (cf=0.1579)
(defrule num-attractions-restaurants-cuisine-seafood "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine seafood)
=> 
	(assert (new_goal (fact NUM_RESTAURANTS_1) (cf 0.20)))
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.2015)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.5263)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.4737)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.1579)))
)

;IF CUISINE = singaporean THEN NUM_ATTRACTIONS = 2 (cf=0.1364)
;IF CUISINE = singaporean THEN NUM_ATTRACTIONS = 3 (cf=0.3182)
;IF CUISINE = singaporean THEN NUM_RESTAURANTS = 2 (cf=0.5909)
;IF CUISINE = singaporean THEN NUM_RESTAURANTS = 3 (cf=0.2273)
;IF CUISINE = singaporean THEN NUM_RESTAURANTS = 4 (cf=0.4091)
(defrule num-attractions-restaurants-cuisine-singaporean "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine singaporean)
=> 
	(assert (new_goal (fact NUM_RESTAURANTS_4) (cf 0.4091)))
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.1364)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.5909)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.3182)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.2273)))
)

;IF CUISINE = cross_cultural THEN NUM_ATTRACTIONS = 2 (cf=0.4091)
;IF CUISINE = cross_cultural THEN NUM_ATTRACTIONS = 3 (cf=0.3182)
;IF CUISINE = cross_cultural THEN NUM_RESTAURANTS = 2 (cf=0.5909)
;IF CUISINE = cross_cultural THEN NUM_RESTAURANTS = 3 (cf=0.2273)
;IF CUISINE = cross_cultural THEN NUM_RESTAURANTS = 4 (cf=0.1364)
(defrule num-attractions-restaurants-cuisine-cross-cultural "CF of cuisine influencing number of attractions and restaurants per day"
	(cuisine cross_cultural)
=> 
	(assert (new_goal (fact NUM_RESTAURANTS_4) (cf 0.1364)))
	(assert (new_goal (fact NUM_ATTRACTIONS_2) (cf 0.4091)))
	(assert (new_goal (fact NUM_RESTAURANTS_2) (cf 0.5909)))
	(assert (new_goal (fact NUM_ATTRACTIONS_3) (cf 0.3182)))
	(assert (new_goal (fact NUM_RESTAURANTS_3) (cf 0.2273)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is written by Team Trippier, 2018, ISS NUS (Copyright)
;; CLIPS rules file containing list of rules for the following:
;; 2. Insurance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; Based on AGE
;

; IF AGE = 18 – 25 THEN INSURANCE = TRUE (cf=0.1429)
; IF AGE = 18 – 25 THEN INSURANCE = MAYBE (cf=0.2143)
(defrule age-insurance-young "CF of 18-25 opting insurance"
	(adult_age 18-25)
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.1429)))
	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.2143)))
)

;IF AGE = 26– 30 THEN INSURANCE = TRUE (cf=0.3333)
;IF AGE = 26– 30 THEN INSURANCE = MAYBE (cf=0.4444)
(defrule age-insurance-midyouth "CF of 26-30 opting insurance"
	(adult_age 26-30)
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.3333)))
	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.4444)))
)

;IF AGE = 31- 45 THEN INSURANCE = TRUE (cf=0.4543)
;IF AGE = 31 – 45 THEN INSURANCE = MAYBE (cf=0.6143)
(defrule age-insurance-mid "CF of 31-45 opting insurance"
	(adult_age 31-45)
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.4543)))
	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.6143)))
)

;IF AGE = 46-60 THEN INSURANCE = TRUE (cf=0.65)
;IF AGE = 46-60 THEN INSURANCE = MAYBE (cf=0.70)
(defrule age-insurance-old "CF of 46=60 opting insurance"
	(adult_age 46-60)
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.65)))
	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.70)))
)

;IF AGE = >60 THEN MEDICAL_INSURANCE = TRUE (cf=0.80)
;IF AGE = >60 THEN MEDICAL_INSURANCE = MAYBE (cf=0.79)
(defrule age-insurance-veryold "CF of >60 opting insurance"
	(adult_age >60)
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.80)))
	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.79)))
)


;
; Based on Theme
;

;IF REVISED_SPORTS_WEIGHT >= 0.8 THEN INSURANCE = TRUE (cf = 0.45)
(defrule theme-insurance-sports-true "CF of sports weight and opting insurance"
	(REVISED_SPORTS_WEIGHT ?x)
	(test (>= ?x 0.8))
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.45)))
)

;IF REVISED_SPORTS_WEIGHT >= 0.5 THEN INSURANCE = MAYBE (cf = 0.45)
(defrule theme-insurance-sports-maybe "CF of sports weight and opting insurance"
	(REVISED_SPORTS_WEIGHT ?x)
	(test (>= ?x 0.5))
=> 
 	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.45)))
)


;IF REVISED_NATURE_WEIGHT >= 0.8 THEN INSURANCE = TRUE (cf = 0.20)
(defrule theme-insurance-nature-true "CF of nature weight and opting insurance"
	(REVISED_NATURE_WEIGHT ?x)
	(test (>= ?x 0.8))
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.20)))
)

;IF REVISED_NATURE_WEIGHT >= 0.5 THEN INSURANCE = MAYBE (cf = 0.20)
(defrule theme-insurance-nature-maybe "CF of nature weight and opting insurance"
	(REVISED_NATURE_WEIGHT ?x)
	(test (>= ?x 0.5))
=> 
 	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.20)))
)


;IF REVISED_CULTURE_WEIGHT >= 0.8 THEN INSURANCE = TRUE (cf = 0.10)
(defrule theme-insurance-culture-true "CF of culture weight and opting insurance"
	(REVISED_CULTURE_WEIGHT ?x)
	(test (>= ?x 0.8))
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.10)))
)

;IF REVISED_CULTURE_WEIGHT >= 0.5 THEN INSURANCE = MAYBE (cf = 0.10)
(defrule theme-insurance-culture-maybe "CF of culture weight and opting insurance"
	(REVISED_CULTURE_WEIGHT ?x)
	(test (>= ?x 0.5))
=> 
 	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.10)))
)


;IF REVISED_HISTORY_WEIGHT >= 0.8 THEN INSURANCE = TRUE (cf = 0.15)
(defrule theme-insurance-history-true "CF of history weight and opting insurance"
	(REVISED_HISTORY_WEIGHT ?x)
	(test (>= ?x 0.8))
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.15)))
)

;IF REVISED_HISTORY_WEIGHT >= 0.5 THEN INSURANCE = MAYBE (cf = 0.15)
(defrule theme-insurance-history-maybe "CF of history weight and opting insurance"
	(REVISED_HISTORY_WEIGHT ?x)
	(test (>= ?x 0.5))
=> 
 	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.10)))
)


;IF REVISED_SHOPPING_WEIGHT >= 0.8 THEN INSURANCE = TRUE (cf = 0.10)
(defrule theme-insurance-shopping-true "CF of shopping weight and opting insurance"
	(REVISED_LIFESTYLE_WEIGHT ?x)
	(test (>= ?x 0.8))
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.10)))
)

;IF REVISED_SHOPPING_WEIGHT >= 0.5 THEN INSURANCE = MAYBE (cf = 0.10)
(defrule theme-insurance-shopping-maybe "CF of shopping weight and opting insurance"
	(REVISED_LIFESTYLE_WEIGHT ?x)
	(test (>= ?x 0.5))
=> 
 	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.10)))
)


;IF REVISED_THEME_PARK_WEIGHT >= 0.8 THEN INSURANCE = TRUE (cf = 0.40)
(defrule theme-insurance-theme-park-true "CF of theme-park weight and opting insurance"
	(REVISED_ADVENTURE_WEIGHT ?x)
	(test (>= ?x 0.8))
=> 
 	(assert (new_goal (fact INSURANCE_TRUE) (cf 0.40)))
)

;IF REVISED_THEME_PARK_WEIGHT >= 0.5 THEN INSURANCE = MAYBE (cf = 0.40)
(defrule theme-insurance-theme-park-maybe "CF of theme-park weight and opting insurance"
	(REVISED_ADVENTURE_WEIGHT ?x)
	(test (>= ?x 0.5))
=> 
 	(assert (new_goal (fact INSURANCE_MAYBE) (cf 0.40)))
)
;;;---------------------------------------------------------------------------------------------------

(defrule REVISED_ADVENTURE_WEIGHT5450 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q7 q7a4) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.952)))) 
 
 
(defrule REVISED_NATURE_WEIGHT4725 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q3 q3a2) 
(q7 q7a4) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.973)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT3256 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a4) 
(q3 q3a4) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT5729 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a5) 
(q3 q3a4) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT308 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q3 q3a4) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.826)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT4566 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.977)))) 
 
 
(defrule REVISED_NATURE_WEIGHT6562 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q3 q3a2) 
(q8 q8a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT186 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a2) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.867)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT5735 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q6 q6a5) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.9)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT7918 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a1) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.9)))) 
 
 
(defrule REVISED_NATURE_WEIGHT370 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a1) 
(q3 q3a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.75)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT1062 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q6 q6a2) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.838)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT5863 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a5) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.857)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT4612 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a1) 
(q3 q3a4) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.833)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT5496 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q5 q5a2) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_NATURE_WEIGHT5624 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a1) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_NATURE_WEIGHT9804 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a5) 
(q8 q8a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.9)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT3332 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q4 q4a3) 
(q7 q7a5) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.909)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT8957 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q7 q8a2) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.889)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT9395 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q3 q3a2) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.692)))) 
 
 
(defrule REVISED_NATURE_WEIGHT3407 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.692)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT9239 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q3 q3a4)
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.889)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT6073 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.769)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT898 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q6 q6a2) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_NATURE_WEIGHT161 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a2) 
(q4 q4a1) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.75)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT5700 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q4 q4a1) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.824)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT1916 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q6 q6a2) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.875)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT4970 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q3 q3a4) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.7)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT1030 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q7 q7a4) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.952)))) 
 
 
(defrule REVISED_NATURE_WEIGHT3892 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q3 q3a2) 
(q7 q7a4) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.973)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT2085 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a4) 
(q3 q3a4) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT3487 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a5) 
(q3 q3a4) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT8394 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q3 q3a4) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.826)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT9855 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.977)))) 
 
 
(defrule REVISED_NATURE_WEIGHT2908 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q3 q3a2) 
(q8 q8a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT889 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a2) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.867)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT77 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q6 q6a5) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.9)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT9161 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a1) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.9)))) 
 
 
(defrule REVISED_NATURE_WEIGHT4670 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a1) 
(q3 q3a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.75)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT3303 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q6 q6a2) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.838)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT6335 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a5) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.857)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT527 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q2 q2a1) 
(q3 q3a4) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.833)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT1300 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q5 q5a2) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_NATURE_WEIGHT8946 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a1) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_NATURE_WEIGHT589 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a5) 
(q8 q8a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.9)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT235 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q4 q4a3) 
(q7 q7a5) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.909)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT2589 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q7 q8a2) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.889)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT2795 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q3 q3a2) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.692)))) 
 
 
(defrule REVISED_NATURE_WEIGHT190 "Rule for people with type:REVISED_NATURE_WEIGHT" 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.692)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT9201 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q3 q3a4) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.889)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT7687 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.769)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT5899 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q6 q6a2) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_NATURE_WEIGHT6914 "Rule for people with type: REVISED_NATURE_WEIGHT" 
(q2 q2a2) 
(q4 q4a1) 
=> 
(assert (new_goal (fact REVISED_NATURE_WEIGHT) (cf 0.75)))) 
(defrule REVISED_LIFESTYLE_WEIGHT33 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q6 q6a2) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.825)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT5760 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q8 q8a4) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.737)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT3085 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q5 q5a5) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.75)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT4990 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q5 q5a5) 
(q6 q6a2) 
(q8 q8a4) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.714)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT1546 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.977)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT6610 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q3 q3a5) 
(q6 q6a2) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT4799 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q3 q3a3) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT2559 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q5 q5a1) 
(q6 q6a2) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.833)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT3123 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q4 Itchy) 
(q6 q6a2) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT5466 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.981)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT5722 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q3 q3a4) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.985)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT5662 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q5 q5a1) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.933)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT7138 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q4 q4a3) 
(q7 q8a2) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT7366 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q6 q6a2) 
(q8 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.825)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT1367 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q8 q8a4) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.737)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT9311 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q5 q5a5) 
(q7 q7a2) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.75)))) 
 
 
(defrule REVISED_LIFESTYLE_WEIGHT8402 "Rule for people with type: REVISED_LIFESTYLE_WEIGHT" 
(q5 q5a5) 
(q6 q6a2) 
(q8 q8a4) 
=> 
(assert (new_goal (fact REVISED_LIFESTYLE_WEIGHT) (cf 0.714)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT6893 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q8 q7a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.977)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT2673 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q6 q6a2) 
(q7 q7a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.7)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT4882 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q5 q5a1) 
(q6 q6a2) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.857)))) 
 
 
(defrule REVISED_HISTORY_WEIGHT2725 "Rule for people with type: REVISED_HISTORY_WEIGHT" 
(q2 q2a4) 
(q4 Hungry!) 
(q5 q5a1) 
=> 
(assert (new_goal (fact REVISED_HISTORY_WEIGHT) (cf 0.75)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT81 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q7 q7a4) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.952)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT1031 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q5 q5a2) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.853)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT3107 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q2 q2a3) 
(q4 q4a1) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_ADVENTURE_WEIGHT8545 "Rule for people with type: REVISED_ADVENTURE_WEIGHT" 
(q3 q3a4) 
(q5 q5a2) 
(q7 q7a4) 
=> 
(assert (new_goal (fact REVISED_ADVENTURE_WEIGHT) (cf 0.8)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT1210 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q3 q3a4) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 0.897)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT5843 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q5 q5a1) 
(q8 q8a2) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT3850 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q5 q5a1) 
(q7 q7a3) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 
 
 
(defrule REVISED_CULTURE_WEIGHT7852 "Rule for people with type: REVISED_CULTURE_WEIGHT" 
(q4 q4a3) 
(q8 q8a2) 
=> 
(assert (new_goal (fact REVISED_CULTURE_WEIGHT) (cf 1.0)))) 


