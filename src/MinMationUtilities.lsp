;|
      Steps to Add a Path to AutoCAD's Search Path
      Open AutoCAD (or AutoCAD Electrical).

      Type OPTIONS and press Enter.

      In the Options dialog, go to the Files tab.

      Expand the entry: Support File Search Path.

      Click Add.

      Click Browse, and navigate to the folder containing your block:

      e.g. C:\MyBlocks\Symbols or C:\Users\Public\Documents\Autodesk\Acade 2023\Libs

      Click OK.

      Use the Move Up button to bring your custom folder higher in priority (optional but helpful).

      Click OK to close the dialog. 
|;


(defun c:MinMationInsertBusSourceSymbol ( / blkName insPt blkDef oldLayer ent xrefCode entData attList )
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
          ;; Call updater
          ;(UpdateSigCode ent)
        )
      )
    )
    (prompt "\nNo point selected. Cancelled.")
  )

  ;; Restore previous layer
  (setvar "CLAYER" oldLayer)

  (princ)
)

(defun c:MinMationInsertBusDestSymbol ( / blkName insPt blkDef oldLayer ent xrefCode entData attList )
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
           ;; Call updater
          ;(UpdateSigCode ent)
        )
      )
    )
    (prompt "\nNo point selected. Cancelled.")
  )

  ;; Restore previous layer
  (setvar "CLAYER" oldLayer)

  (princ)
)
;| (defun UpdateSigCode (ent / ss)
  (if (and ent (entget ent))
    (progn
      ;; Create a selection set from the entity
      (setq ss (ssadd ent))
      (if ss
        (progn
          ;; React to retag/update the component using ACADE command
          (command "_.WD_REACT_DO_RETAG_OR_UPD" ss "")
        )
      )
    )
  )
)
 |;
(defun c:RetagComponent ( / ent blkObj )
  (vl-load-com)

  ;; Prompt user to select a block
  (setq ent (car (entsel "\nSelect a component to retag/update: ")))

  ;; Check if it's a block reference
  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      ;; Convert to VLA object
      (setq blkObj (vlax-ename->vla-object ent))

      ;; Call the AutoCAD Electrical retag function
      (wd_react_do_retag_or_upd blkObj nil T)
    )
    (prompt "\nInvalid selection. Please select a block.")
  )

  (princ)
)

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

