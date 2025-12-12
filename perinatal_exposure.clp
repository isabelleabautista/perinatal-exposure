;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perinatal Environmental Exposure Advisor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts start
  (start-questions))

(deftemplate patient
  (slot pregnant)          ;; yes / no
  (slot exposure-type)     ;; retinoid / lead / solvent / none
  (slot home-year)         ;; number year
  (slot water-source)      ;; well / municipal / nil
  (slot symptoms))         ;; none / dizziness / headache / confusion

(deftemplate recommendation
  (slot severity)          ;; low / medium / high
  (slot text))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pregnancy Question
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule ask-pregnancy
   ?s <- (start-questions)
   =>
   (retract ?s)
   (printout t "Is the patient pregnant? (yes/no): ")
   (bind ?p (read))
   (assert (patient (pregnant ?p)
                    (exposure-type nil)
                    (home-year nil)
                    (water-source nil)
                    (symptoms none))))

;; If no pregnancy, stop with low severity
(defrule pregnancy-no-stop
   (patient (pregnant no))
   =>
   (assert (recommendation
      (severity low)
      (text "Patient is not pregnant. Perinatal environmental risk is not a concern right now; no further advisor questions needed."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exposure Type Question
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule ask-exposure
   ?pFact <- (patient (pregnant yes)
                      (exposure-type nil))
   =>
   (printout t crlf "Enter exposure type (retinoid / lead / solvent / none): ")
   (bind ?e (read))
   (modify ?pFact (exposure-type ?e)))

;; If exposure type is 'none' stop with low severity
(defrule exposure-none-exit
   (patient (pregnant yes)
            (exposure-type none))
   =>
   (assert (recommendation
      (severity low)
      (text "No specific exposure reported. No further advisor questions are needed."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lead Followup Qs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule ask-lead-details
   ?pFact <- (patient (pregnant yes)
                      (exposure-type lead)
                      (home-year nil))
   =>
   (printout t "Enter home construction year (e.g., 1965): ")
   (bind ?y (read))
   (printout t "Water source (well / municipal): ")
   (bind ?w (read))
   (modify ?pFact (home-year ?y) (water-source ?w)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solvent Followup Q
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule ask-solvent-symptoms
   ?pFact <- (patient (pregnant yes)
                      (exposure-type solvent)
                      (symptoms none))
   =>
   (printout t "List symptoms (none / dizziness / headache / confusion): ")
   (bind ?s (read))
   (modify ?pFact (symptoms ?s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retinoid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule retinoid-high
   (patient (pregnant yes)
            (exposure-type retinoid))
   =>
   (assert (recommendation
      (severity high)
      (text "Avoid topical retinoids during pregnancy. Consider safer alternatives such as azelaic acid or glycolic acid as tolerated."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lead!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; High is BOTHHHHH pre-1978 home AND private well
(defrule lead-well-high
   (patient (pregnant yes)
            (exposure-type lead)
            (home-year ?y&:(and (integerp ?y)
                                (< ?y 1978)))
            (water-source well))
   =>
   (assert (recommendation
      (severity high)
      (text "A pre-1978 home with a private well is a higher lead risk. Prioritize maternal blood lead testing and consider testing the well water for lead."))))

;; Medium is pre-1978 home, but water source NOT well
(defrule lead-pre1978-medium
   (patient (pregnant yes)
            (exposure-type lead)
            (home-year ?y&:(and (integerp ?y)
                                (< ?y 1978)))
            (water-source ?w&~well))
   =>
   (assert (recommendation
      (severity medium)
      (text "A pre-1978 home is a potential lead hazard. Consider maternal blood lead testing and lead-safe cleaning and renovation practices."))))

;; Low is 1978 or newer home, water source whatever
(defrule lead-newer-low
   (patient (pregnant yes)
            (exposure-type lead)
            (home-year ?y&:(and (integerp ?y)
                                (>= ?y 1978))))
   =>
   (assert (recommendation
      (severity low)
      (text "Home construction year is 1978 or newer, so paint-based lead risk is lower. Follow general environmental health advice as needed."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solvents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; High is any one specific neurologic symptoms
(defrule solvent-neuro-high
   (patient (pregnant yes)
            (exposure-type solvent)
            (symptoms ?s&:(or (eq ?s dizziness)
                              (eq ?s headache)
                              (eq ?s confusion))))
   =>
   (assert (recommendation
      (severity high)
      (text "Any one neurologic symptoms with solvent inhalation are concerning in pregnancy. Leave the area immediately and seek medical attention if symptoms persist or worsen."))))

;; Low solvent exposure but symptoms = none
(defrule solvent-none-low
   (patient (pregnant yes)
            (exposure-type solvent)
            (symptoms none))
   =>
   (assert (recommendation
      (severity low)
      (text "Solvent exposure  but no neurologic symptoms reported. Improve ventilation, reduce exposure, and monitor for any new symptoms."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level exclusivity checks!!!! so that they dont print more than one!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If high exists, print high only
(defrule print-high
   (recommendation (severity high) (text ?t))
   =>
   (printout t crlf "[HIGH] " ?t crlf))

;; If no high but a medium, print the medium medium
(defrule print-medium
   (not (recommendation (severity high)))
   (recommendation (severity medium) (text ?t))
   =>
   (printout t crlf "[MEDIUM] " ?t crlf))

;; Low low if low. No hi or med
(defrule print-low
   (not (recommendation (severity high)))
   (not (recommendation (severity medium)))
   (recommendation (severity low) (text ?t))
   =>
   (printout t crlf "[LOW] " ?t crlf))
