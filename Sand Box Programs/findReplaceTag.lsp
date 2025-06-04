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

(defun c:ReplaceAttdefTagsLoop ( / ss i ent tag newtag entData searchStr replaceStr num numWidth exitFlag padded )
  (vl-load-com)

  ;; Prompt for search pattern
  (initget 0 "01")
  (setq searchStr (getstring t "\nEnter search pattern for TAGs [default: *01]: "))
  (if (= searchStr "") (setq searchStr "*01"))

  ;; Prompt for starting replace string (e.g., 02)
  (setq replaceStr (getstring t "\nEnter starting replace string (e.g. 02): "))
  (if (= replaceStr "") (setq replaceStr "02"))

  ;; Extract numeric part and width for zero-padding
  (setq num (atoi replaceStr))                      ; Start number
  (setq numWidth (strlen replaceStr))               ; Digits to preserve

  (setq exitFlag nil)

  (while (not exitFlag)
    ;; Build padded number string
    (setq padded (rtos num 2 0)) ; Convert to string, no decimal
    (while (< (strlen padded) numWidth)
      (setq padded (strcat "0" padded))
    )

    ;; Prompt user to select ATTDEFs
    (prompt (strcat "\nSelect ATTDEFs to change " searchStr " → *" padded ": "))
    (setq ss (ssget '((0 . "ATTDEF"))))

    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq ent (ssname ss i))
          (setq entData (entget ent))
          (setq tag (cdr (assoc 2 entData)))
          (if (and tag (wcmatch tag searchStr))
            (progn
              (setq newtag (vl-string-subst padded (substr searchStr 2) tag))
              (setq entData (subst (cons 2 newtag) (assoc 2 entData) entData))
              (entmod entData)
              (princ (strcat "\nChanged TAG: " tag " → " newtag))
            )
          )
          (setq i (1+ i))
        )
        ;; Increment number for next pass
        (setq num (1+ num))
      )
      (progn
        (prompt "\nNothing selected. Exiting loop.")
        (setq exitFlag T)
      )
    )
  )

  (prompt "\nAll done. Use REGEN if changes aren't visible.")
  (princ)
)

(defun c:InsertXrefSymbol ( / blkName insPt blkDef )
  (vl-load-com)
  (setq blkName "ha5s1_ref") ; Block name for the cross-reference symbol

  ;; Check if block is defined in drawing
  (setq blkDef (tblsearch "BLOCK" blkName))
  (if (not blkDef)
    (progn
      (prompt (strcat "\nBlock \"" blkName "\" not found in drawing."))
      (prompt "\nMake sure the block is defined or insert it once manually.")
    )
    (progn
      ;; Get insertion point
      (prompt "\nPick insertion point for stand-alone cross-reference:")
      (setq insPt (getpoint "\nSpecify insertion point: "))
      (if insPt
        (command "_.-INSERT" blkName insPt "1" "1" "0") ; scaleX, scaleY, rotation
        (prompt "\nNo point selected. Cancelled.")
      )
    )
  )
  (princ)
)
(defun c:InsertXrefSymbol2 ( / blkName insPt blkDef )
  (vl-load-com)

  (setq blkName "ha5s1_ref") ; Block name for the cross-reference symbol
  
  ;; Save current layer
  (setq oldLayer (getvar "CLAYER"))

  ;; Switch to layer 0
  (setvar "CLAYER" "0")

  ;; Check if the block is already defined
  (setq blkDef (tblsearch "BLOCK" blkName))

  ;; If not found, insert from default path
  (if (not blkDef)
    (progn
      (prompt (strcat "\nBlock \"" blkName "\" not found. Attempting to load from default path..."))
      ;; Attempt to insert from library path (assumes it's in support path)
      (command "_.-INSERT" blkName "0,0" "1" "1" "0")
      (setq blkDef (tblsearch "BLOCK" blkName)) ; Check again
      (if (not blkDef)
        (progn
          (prompt (strcat "\nFailed to load block \"" blkName "\". Make sure it's in your support path or symbol library."))
          (princ)
          (exit)
        )
      )
    )
  )

  ;; Ask user for insertion point
  (prompt "\nPick insertion point for stand-alone cross-reference:")
  (setq insPt (getpoint "\nSpecify insertion point: "))

  (if insPt
    (command "_.-INSERT" blkName insPt "1" "1" "0")
    (prompt "\nNo point selected. Cancelled.")
  )
  
    ;; Restore previous layer
  (setvar "CLAYER" oldLayer)

  (princ)
)

(defun c:InsertXrefSymbol3 ( / blkName insPt blkDef oldLayer ent xrefCode entData attList )
  (vl-load-com)

  (setq blkName "ha5s1_ref")

  ;; Check if block is already in drawing
  (setq blkDef (tblsearch "BLOCK" blkName))
  (if (not blkDef)
    (command "_.-INSERT" blkName "0,0" "1" "1" "0") ; attempt to load block
  )

  ;; Save current layer and switch to 0
  (setq oldLayer (getvar "CLAYER"))
  (setvar "CLAYER" "0")

  ;; Prompt for insertion point
  (prompt "\nPick insertion point for stand-alone cross-reference:")
  (setq insPt (getpoint "\nSpecify insertion point: "))

  (if insPt
    (progn
      ;; Insert block
      (command "_.-INSERT" blkName insPt "1" "1" "0")
      (setq ent (entlast))

      ;; Prompt for cross-reference CODE
      (setq xrefCode (getstring t "\nEnter cross-reference SIGCODE: "))

      ;; Find and set CODE attribute
      (if (and ent xrefCode)
        (progn
          (setq entData (entnext ent))
          (while entData
            (if (= "ATTRIB" (cdr (assoc 0 (entget entData))))
              (progn
                (setq attList (entget entData))
                (if (= (strcase (cdr (assoc 2 attList))) "SIGCODE")
                  (progn
                    (setq attList (subst (cons 1 xrefCode) (assoc 1 attList) attList))
                    (entmod attList)
                    (entupd entData)
                  )
                )
              )
            )
            (setq entData (entnext entData))
          )
        )
      )
    )
    (prompt "\nNo point selected. Cancelled.")
  )

  ;; Restore previous layer
  (setvar "CLAYER" oldLayer)

  (princ)
)
