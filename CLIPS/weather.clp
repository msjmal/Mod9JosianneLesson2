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
  (if (eq ?c exit) then
    (assert (exit-flag))
    (printout t "Exiting." crlf)
  else
    (assert (set-sky (to-color ?c)))))

; -------------------------------
; Set or update sky fact
; -------------------------------

(defrule init-sky
  ?cmd <- (set-sky (to-color ?newColor))
  (not (sky))
  =>
  (retract ?cmd)
  (assert (sky (color ?newColor))))

(defrule update-sky
  ?cmd <- (set-sky (to-color ?newColor))
  ?old <- (sky (color ?oldColor&:(neq ?oldColor ?newColor)))
  =>
  (retract ?old)
  (retract ?cmd)
  (assert (sky (color ?newColor))))

(defrule discard-duplicate-sky-command
  ?cmd <- (set-sky (to-color ?newColor))
  (sky (color ?newColor)) ; already correct, no update needed
  =>
  (retract ?cmd))

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
