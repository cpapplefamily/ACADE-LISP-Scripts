(defun c:ReplaceAttdefTags ( / ss i ent tag newtag entData searchStr replaceStr )
  (vl-load-com)

  ;; Prompt for search pattern (wildcards allowed), default to *01
  (initget 0 "01")
  (setq searchStr (getstring t "\nEnter search pattern for TAGs [default: *01]: "))
  (if (= searchStr "") (setq searchStr "*01"))

  ;; Prompt for replacement string (no default)
  (setq replaceStr (getstring t "\nEnter string to replace matching part: "))

  ;; Select ATTDEFs only
  (prompt "\nSelect attribute definitions (ATTDEFs) to process:")
  (setq ss (ssget '((0 . "ATTDEF"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq tag (cdr (assoc 2 entData))) ; Group code 2 = TAG
        (if (and tag (wcmatch tag searchStr))
          (progn
            ;; Replace matching part with replacement string
            (setq newtag (vl-string-subst replaceStr (substr searchStr 2) tag)) ; crude match logic
            (setq entData (subst (cons 2 newtag) (assoc 2 entData) entData))
            (entmod entData)
            (princ (strcat "\nChanged TAG: " tag " → " newtag))
          )
        )
        (setq i (1+ i))
      )
      (princ "\nDone. REGEN if needed.")
    )
    (prompt "\nNo ATTDEFs selected.")
  )
  (princ)
)
