(defun c:RotateAtts ( / ent ename angle blkRef atts att)
  (prompt "\nSelect block with attributes: ")
  (setq ent (entsel))
  (if ent
    (progn
      (setq ename (car ent))
      (setq blkRef (vlax-ename->vla-object ename))
      (if (and (eq (vla-get-HasAttributes blkRef) :vlax-true)
               (setq angle (getangle "\nEnter rotation angle in degrees: ")))
        (progn
         ; (setq angle (degrees-to-radians angle))
          (setq atts (vlax-invoke blkRef 'GetAttributes))
          (foreach att atts
            (princ (strcat "\nRotating attribute: " (vla-get-TagString att)))
            (vla-put-Rotation att angle)
          )
          (vla-update blkRef)
          (princ "\nAttributes rotated successfully.")
        )
        (prompt "\nSelected block has no attributes or invalid input.")
      )
    )
    (prompt "\nNothing selected.")
  )
  (princ)
)

(defun degrees-to-radians (deg)
  (* pi (/ deg 180.0))
)

(princ "Type \"RotateAtts\" in command prompt to run the lisp.")