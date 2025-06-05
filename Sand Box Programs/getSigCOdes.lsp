(defun GetAvailableSigCodes ( / ss i ent code codes)
  ;; Search the drawing for SIGCODE attributes
  (setq codes '())
  (setq ss (ssget "_X" '((0 . "INSERT")))) ; all blocks

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (foreach att (vlax-invoke (vlax-ename->vla-object ent) 'GetAttributes)
          (if (= (strcase (vla-get-TagString att)) "SIGCODE")
            (setq code (vla-get-TextString att))
          )
          (if (and code (not (member code codes)))
            (setq codes (cons code codes))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  codes
)

(defun SelectSigCodeFromList (codes / sel)
  ;; Present a selection list using command line keywords
  (if codes
    (progn
      (initget (apply 'strcat (mapcar (function (lambda (c) (strcat c " "))) codes)))
      (setq sel (getkword (strcat "\nSelect SIGCODE: [" (apply 'strcat (mapcar (function (lambda (c) (strcat c "/"))) codes)) "\b]: ")))
      sel
    )
    (progn (prompt "\nNo SIGCODEs found.") nil)
  )
)

(defun c:MinMationInsertBusDestSymbol ( / blkName insPt blkDef oldLayer ent selCode entData attList )
  (vl-load-com)

  (setq blkName "ha5d1_ref")

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

      ;; Get SIGCODEs from the drawing and ask user to pick
      (setq selCode (SelectSigCodeFromList (GetAvailableSigCodes)))

      ;; Assign selected SIGCODE
      (if (and ent selCode)
        (progn
          (setq entData (entnext ent))
          (while entData
            (if (= "ATTRIB" (cdr (assoc 0 (entget entData))))
              (progn
                (setq attList (entget entData))
                (if (= (strcase (cdr (assoc 2 attList))) "SIGCODE")
                  (progn
                    (setq attList (subst (cons 1 selCode) (assoc 1 attList) attList))
                    (entmod attList)
                    (setq attList (subst (cons 1 selCode) (assoc 1 attList) attList))
                      (entmod attList)
                      (entupd entData)
                    ;; Ensure Electrical reactors are loaded
                    (if (not (member "wdreactor.arx" (arx)))
                      (arxload "wdreactor.arx")
                    )
                    ;; Refresh cross-reference using command-line
                    (command "_.WD_RETAG_UPDATE_NOPROMPTS" "S" ent "")

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
