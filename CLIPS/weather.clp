; -------------------------------
; Templates
; -------------------------------

(deftemplate sky
  (slot color))
  
  ; This is a schema for facts specifying slot(field) that can have facts in them.  In this case it represents the color of the sky.
    ;Example fact will be (sky (color blue))

(deftemplate weather
  (slot condition))

;This is  a schema for weather and has only 1 field called condition

(deftemplate set-sky
  (slot to-color))

;Another schema set-sky with a  field to-color


(deftemplate prompt-next
  (slot value))

;Another schema prompt-next with a field value

(deftemplate exit-flag)

; This is just an action so no fields associated with it. It acts like a flag, either present or absent


; -------------------------------
; Ask user for sky color
; It will printout to t (terminal) and move to the next line (crlf)
; -------------------------------

(defrule ask-user
  ?prompt <- (prompt-next (value ready))
  =>
  (retract ?prompt)
  (printout t crlf "What is the sky color? (blue or grey, or type exit): ")
  (bind ?c (read))   ; ? indicates a local variable named c short for color or choice you name it as ?input and read captures user input from keyboard
  (if (eq ?c exit) then  ;eq stand for equal so asking whether it is equal to variable exit
    (assert (exit-flag))
    (printout t "Exiting." crlf)
  else
    (assert (set-sky (to-color ?c)))))  ;if not exit it will change variable c to the color set

; -------------------------------
; Set or update sky fact
; -------------------------------

(defrule init-sky   ;ensures sky is only set once unless later removed
  ?cmd <- (set-sky (to-color ?newColor))  ; here it is looking for a fact which matches
  (not (sky))  ;avoids duplicates-  ensures there is no existing sky fact in the system - this only triggers if sky hasn't been initialised yet
  =>  ; seperates conditions from actions
  (retract ?cmd)  ;cleanup - deletes fact stored in ?cmd from the system - prevents the rule from firing repeatedly for the same command
  (assert (sky (color ?newColor))))

(defrule update-sky
  ?cmd <- (set-sky (to-color ?newColor)) ;looks for fact in form and binds it to variable ?cmd so it can be removed later - binds new colour to ?newColor
  ?old <- (sky (color ?oldColor&:(neq ?oldColor ?newColor))) ? checks sky fact and puts it in ?old to be removed later. current to ?oldColor neq ensures new and old colors are different (rule only triggers if change in sky color)
  => ;above were conditions now actions are coming
  (retract ?old) ; deletes color which was inserted in old
  (retract ?cmd) ; delets set-sky(to-color ?newColor) fact 
  (assert (sky (color ?newColor))))  ; creates new sky fact with updated color

(defrule discard-duplicate-sky-command  ; detects and removes redundant set-sky when not changing anything
  ?cmd <- (set-sky (to-color ?newColor)) ; looks for any existing set-sky command, binds whole fact to variable ?cmd and extracts the requested new color into variable?newColor
  (sky (color ?newColor)) ; checks if there is already a sky fact with the same color as requested
  =>
  (retract ?cmd)) ;removes redundant set-sky from fact list - prevents unnecessary processing by other rules

; -------------------------------
; Weather updates
; -------------------------------

(defrule weather-nice
  (sky (color blue))
  ?w <- (weather (condition ?c&:(neq ?c nice)))
  =>
  (retract ?w)
  (assert (weather (condition nice))))

(defrule weather-ugly
  (sky (color grey))
  ?w <- (weather (condition ?c&:(neq ?c ugly)))
  =>
  (retract ?w)
  (assert (weather (condition ugly))))

(defrule init-weather-nice
  (sky (color blue))
  (not (weather))
  =>
  (assert (weather (condition nice))))

(defrule init-weather-ugly
  (sky (color grey))
  (not (weather))
  =>
  (assert (weather (condition ugly))))

; -------------------------------
; Loop to ask again unless exiting
; -------------------------------

(defrule repeat-loop
  (sky)
  (weather)
  (not (exit-flag))
  (not (prompt-next))
  =>
  (assert (prompt-next (value ready))))
