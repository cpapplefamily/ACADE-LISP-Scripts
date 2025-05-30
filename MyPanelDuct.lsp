;| ====================================================================================================================================================================

DinRail.lsp

This program automates use of AutoCAD Electrical's Insert Din rail command with user presets.

Environment: AutoCAD 2025
Author: Arshdeep Singh

Version 1.0 - July 27, 2024
Initial Release

====================================================================================================================================================================|;

;START OF THE PROGRAM

(defun MyPanelDuct (Manufacturer Catalog Assycode / OldOrthomode pt1 pt2 tempPt dx dy ang len par1 par2 par3 par4 par5 par6 par7 par8 parameters LastEntity EntData)

  
  
;= Snapshot System Variables ======================================================================================================================================
  
  (setq OldOrthomode (getvar "ORTHOMODE"))                                    ; Get the current ORTHOMODE and save it as oldOrtho
  (setvar "ORTHOMODE" 1)                                                      ; Set ORTHOMODE to 1 (on)

  
;= Get Insertion information from user ============================================================================================================================
  
  (setq pt1 (getpoint "\nSpecify base point: "))                              ; Prompt the user for the base point
  (setq tempPt (polar pt1 0 0.01))                                            ; Generate a temporary point 0.01 units away from pt1 in the direction of 0 radians (right)
  (command "LINE" pt1 tempPt "")                                              ; Draw a line from pt1 to tempPt (used as a reference for visual aid)
  (setq pt2 (getpoint pt1 "\nSpecify second point: "))                        ; Prompt the user for the second point
  (command "ERASE" tempPt "")                                                 ; Erase the temporary reference point (tempPt) to clean up
  
  (setq dx (- (car pt2) (car pt1)))                                           ; Calculate the horizontal distance between pt1 and pt2
  (setq dy (- (cadr pt2) (cadr pt1)))                                         ; Calculate the vertical distance between pt1 and pt2
  (setq ang (atan dy dx))                                                     ; Calculate angle in radians
  (setq len (distance pt1 pt2))                                               ; Calculate length
  
  
;= Insert Din Rail =============================================================================================================================================

  (setq par2 len)                                                             ; Length                                                            
  (setq par3 (cond                                                            ; Orientation, horiz or vertical insert "H" or "V"
    ((or (< (abs ang) 1e-6) (< (abs (- ang pi)) 1e-6)) "H")                     ; Horizontal if angle ≈ 0 or π
    ((or (< (abs (- ang (/ pi 2))) 1e-6) (< (abs (+ ang (/ pi 2))) 1e-6)) "V")  ; Vertical if angle ≈ π/2 or -π/2
    (T "H")                                                                   ; Default to Horizontal
  ))
  (setq par4 1.0)                                                             ; Scale, nil = 1.0
  (setq par5 3)                                                               ; Panel Mounting, 1 = holes, 2 = stand-offs, 3 = none
  
  (setq par1 pt1)                                                             ; Base Point
  (cond
    ((equal par3 "H")
      (if (> (car pt1) (car pt2))
        (progn
          (setq par1 pt2)
          (setq pt2 pt1)
          (setq pt1 par1)
        )
      )
    )
    ((equal par3 "V")
      (if (> (cadr pt1) (cadr pt2))
        (progn
          (setq par1 pt2)
          
          (setq pt2 pt1)
          (setq pt1 par1)
        )
      )
    )
  )
  
  (setq parameters (list par1 par2 par3 par4 par5))                           ; Generate parameters list
  
  (print (strcat "Inserting Din Rail with parameters: " Manufacturer ", " Catalog ", " Assycode)) ; Print parameters for debugging()

  (c:ace_ins_dinrail Manufacturer Catalog Assycode parameters)                      ; Run AutoCAD Electrical's ace_ins_dinrail command
  
;= Rotate Din Rail =============================================================================================================================================
  
  (setq LastEntity (entlast))                                                 ; Get the last drawn entity that was drawn by ace_ins_dinrail command
 ;|  (setq EntData (entget LastEntity))                                          ; Get the entity data
  
  (setq EntData (subst (cons 50 ang) (assoc 50 EntData) EntData))             ; Change rotation using radian value calculated from user specified points
  
  (entmod EntData)                                                            ; Apply changes
  (entupd LastEntity)                                                         ; Update entity
 |;
  
;= Restore System Variables ====================================================================================================================================
  
  (c:wd_modattrval LastEntity "DESC1" Catalog nil)                           ; Modify the attribute value of DESC1 to "newval" (you can change this to any value you want)
  (c:wd_modattrval LastEntity "DESC2" (strcat (rtos len 2 2) "\"" ) nil)                           ; Modify the attribute value of DESC1 to "newval" (you can change this to any value you want)
  
;= Restore System Variables ====================================================================================================================================
  
  (setvar "ORTHOMODE" OldOrthomode)                                           ; Restore the original ORTHOMODE
  (princ)                                                                     ; Exit quietly
  
)

(princ "Type \"MyPanelDuct\" in command prompt to run the lisp.")

;END OF THE PROGRAM