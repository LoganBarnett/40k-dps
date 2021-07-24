;;; 40k-dps.el --- Calculate 40k attack/defense averages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Logan Barnett-Hoy
;;
;; Author: Logan Barnett-Hoy <https://github.com/logan>
;; Maintainer: Logan Barnett-Hoy <logustus@gmail.com>
;; Created: June 26, 2021
;; Modified: June 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/logan/40k-dps
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Computes average damage from a given salvo of weapons fire or attacks in the
;; grimdark game of Warhammer 40,000 by Games Workshop.
;;
;;; Code:

(defun wh40k/org-table-lookup (column-name table row)
  "Get field value from ROW using COLUMN-NAME found in TABLE."
  (let (
        (header-index
         (-elem-index
          column-name
          (car table)
          ))
        )
    (message "index at %s" header-index )
    (message "row %s" row)
    (message "output at %s: %s" header-index (nth header-index row))
    (nth header-index row)
    )
  )
(defun wh40k/org-table-lookup-n (&rest args)
  "Get numberic table value using `wh40k/org-table-lookup' ARGS."
  (string-to-number (apply #'wh40k/org-table-lookup args))
  )
;; We have some special rows:
;; Row 2: header row - I need this.
;; Row 3: hline - drop it.
(defun wh40k/org-table-keep-row (row)
  "Indicate if ROW is not an hline."
  (cond
   ((equal 'hline row) nil)
   (t t)
   )
  )
(defun wh40k/org-table-lisp-from-name (tblname)
  "Find an org-table named TBLNAME in the current buffer."
  (save-excursion
    (let* (
           (table
            (org-element-map (org-element-parse-buffer) 'table
              (lambda (element)
                (when (string= tblname (org-element-property :name element))
                  element))
              nil ;info
              t )) ; first-match
           )
      (goto-char (org-element-property :contents-begin table))
      (-filter #'wh40k/org-table-keep-row (org-table-to-lisp))
      )
    )
  )
(defun wh40k/d6-from-plus (n)
  "Calculates the percentage probability of a d6 roll of N or greater."
  (cond
    ((> n 6) 0)
    (t (/ (- 7 (max 2 n)) 6.0))
  ))
(defun wh40k/d6-from-plus-negate (n)
  "Calculates the percentage chances of rolling N or lower on a d6."
  (cond
    ((> n 6) 1.0)
    (t (/ (max 1.0 (- n 1)) 6.0))
  ))
(defun wh40k/d6-with-reroll (n r)
  "Compute success chance roll of N+ on a d6, with a reroll of R or below."
  ;; If r + 1 >= n, we reroll everything. So it's %n + %n * %n.
  ;; If r < n, we reroll r and lower. %n + %n * %(r + n).
  (cond
   ;; ((>= r n) (wh40k/d6-from-plus n))
   ((>= (+ r 1) n) (-
                    (* 2 (wh40k/d6-from-plus n))
                    (expt (wh40k/d6-from-plus n) 2)
                    ))
   (t (-
       (* 2 (wh40k/d6-from-plus n))
       ;; 3+ reroll 1
       ;; 4/6 succeed plus half of 2/6 * 4/6.
       ;; 4/6 + 4/6 * 4/6 * 0.5 = 0.88888889 <- wrong
       ;; (4/6 * 2) - (4/6)^2 * 1/6 = 0.88888889
       ;; 3+ reroll all
       ;; 4/6 succeed plus all of 2/6 * 4/6.
       ;; 4/6 + 4/6 * 4/6 * 1 = 1.11111111  <- wrong
       ;; (4/6 * 2) - (4/6)^2 = 0.88888889
       ;; (/
        (* (wh40k/d6-from-plus r) (wh40k/d6-from-plus n))
        ;; (/ r 6)
        ;; (- 1 (/ r 6))
        ;; )
       )
      )
   )
  )
(defun wh40k/strength-vs-toughness (strength toughness)
  "The d6 roll to wound with STRENGTH against TOUGHNESS."
  (cond
      ((>= strength (* toughness 2)) 2)
      ((<= (* strength 2) toughness) 6)
      ((> strength toughness) 3)
      ((< strength toughness) 5)
      (t 4)
      )
  )
;; Apply armor penetration to the save. If the save is worse than the
;; invulnerable, use the invulnerable. Returns a percentage reduced by saves.
(defun wh40k/ap-vs-save (ap save invulnerable)
  "The d6 roll needed to ignore wound using SAVE or INVULNERABLE based on AP.

If no save or invulnerable, use 7 or higher. AP should be a negative value
generally, but could be positive to reflect cover or other factors."
  (min
   7
   (cond
    ((> (- save ap) invulnerable) invulnerable)
    (t (- save ap))
    )
   )
  )
(defun wh40k/damage-die (damage-notation)
  "The rolled segment of DAMAGE-NOTATION."
  (string-to-number
   (save-match-data
     (if (string-match "d\\([\\[:digit:]]\\)" damage-notation)
         (match-string 1 damage-notation)
       "0"
       )
     )
   )
  )
(defun wh40k/damage-parse (damage)
  "Parse a DAMAGE notation into a tuple of random and constant values.

Examples:
(wh40k/damage-parse \"d3\")   ; '(3 0)
(wh40k/damage-parse \"d3+3\") ; '(3 3)
(wh40k/damage-parse \"2\")    ; '(0 2)
"
  (let* (
         ;; Make the notation uniform. "D3 + 3" becomes "d3+3", etc.
         (dmg-clean (replace-regexp-in-string
                     " "
                     ""
                     (downcase damage)))
         (variable (wh40k/damage-die dmg-clean))
         (die-text (concat "d" (number-to-string variable)))
         (dmg-sans-die
          ;; We cannot do negative lookbehinds in Emacs Lisp, so we must instead
          ;; extract the die. Fortunately we just got it, so it's a replace
          ;; away.
          (replace-regexp-in-string
           (regexp-quote die-text)
           ""
           dmg-clean
           )
          )
         (constant (save-match-data
                     (if (string-match
                          "\\([\\[:digit:]]+\\)"
                          dmg-sans-die
                          )
                         (match-string 1 dmg-sans-die)
                       "0"
                       )
                     ))
         )
    (list
     (cond
      ((= variable 3) (/ 3.0 2.0))
      ((= variable 6) (/ 6.0 2.0))
      (t 0.0)
      )
     (string-to-number constant)
     )
    )
  )
;; This only works with a resistance of 1. Need to refine formula for more when
;; the need arises.
(defun wh40k/damage-resist-from-die (die)
  "Average the reduction of DIE damage amount by 1, to a minimum of one."
  (/ (+ 2.0 (* 4.0 (- (/ (+ 2.0 die) 2.0) 1.0))) 6.0)
  )
(defun wh40k/damage-multiplier (damage-notation resist)
  (let* (
         (damage-pair (wh40k/damage-parse damage-notation))
         ;; Keep in mind this is not the die itself, but the variable average
         ;; from the die.
         (variable (car damage-pair))
         (constant (car (cdr damage-pair)))
         )
    (message "constant %s" constant)
    (message "variable %s" variable)
    (cond
     ;; Nothing to do here if we resist nothing.
     ((= resist 0) (+ constant variable))
     ;; Do not deduct damage if it is just 1.
     ((and (= constant 1) (= variable 0.0)) 1.0)
     ;; If the constant is > 1, we can safely deduct 1 always.
     ((> constant 1) (+ variable (- constant 1)))
     ;; If we just have a die, we have to do some computation.
     ((> variable 0.0) (wh40k/damage-resist-from-die
                        (wh40k/damage-die damage-notation)))
     ;; That _should_ be all the cases.
     (t nil)
      )
    )
  )
(defun wh40k/damage-probability-all-weapons-to-defender (
                                 weapon-profile-table-name
                                 defense-profile-table-name
                                 defender
                                 )
  "Calculate damage statistics WEAPON-PROFILE-TABLE-NAME against DEFENDER using DEFENSE-PROFILE-TABLE-NAME."
  (let* (
         (defense-profiles (wh40k/org-table-lisp-from-name
                            defense-profile-table-name))
         (weapon-profiles (wh40k/org-table-lisp-from-name
                           weapon-profile-table-name))
         (defender-row (-find
                        (lambda (d)
                          (string-equal
                           (wh40k/org-table-lookup "name" defense-profiles d)
                           defender))
                        defense-profiles))
         )
    (message "defense-profles %s" defense-profiles)
    (message "weapon-profles %s" weapon-profiles)
    (-map
     (lambda (row)
       (message "row %s" row)
       (let*
           (
            (weapon-name (wh40k/org-table-lookup "name" weapon-profiles row))
            (defender-name (wh40k/org-table-lookup "name" defense-profiles defender-row))
            ;; Defensive traits.
            (hit-mod (wh40k/org-table-lookup-n "hit" defense-profiles defender-row))
            (toughness (wh40k/org-table-lookup-n "t" defense-profiles defender-row))
            (save (wh40k/org-table-lookup-n "v" defense-profiles defender-row))
            (invulnerable (wh40k/org-table-lookup-n "i" defense-profiles defender-row))
            (resist-damage (wh40k/org-table-lookup-n "res" defense-profiles defender-row))
            (feel-no-pain (wh40k/org-table-lookup-n "fnp" defense-profiles defender-row))
            ;; Offensive traits.
            (ballistic-skill (wh40k/org-table-lookup-n "bs" weapon-profiles row))
            (attacks (wh40k/org-table-lookup "a" weapon-profiles row))
            (strength (wh40k/org-table-lookup-n "s" weapon-profiles row))
            (armor-penetration (wh40k/org-table-lookup-n "ap" weapon-profiles row))
            (damage (wh40k/org-table-lookup "dmg" weapon-profiles row))
            ;; Computed.
            ;; TODO: name this better.
            (attacks-multiplier (wh40k/damage-multiplier attacks 0))
            (to-hit-multiplier (wh40k/d6-from-plus (- ballistic-skill hit-mod)))
            (to-wound-multiplier (wh40k/d6-from-plus
                                  (wh40k/strength-vs-toughness
                                   strength
                                   toughness)))
            (save-multiplier
             (wh40k/d6-from-plus-negate
              (wh40k/ap-vs-save armor-penetration
                                save
                                invulnerable)
              )
             )
            (damage-multiplier (wh40k/damage-multiplier damage resist-damage))
            (feel-no-pain-multiplier (wh40k/d6-from-plus-negate feel-no-pain))
            )
         (message "row:
weapon-name:             %s
defender-name:           %s
attacks-multiplier:      %s
to-hit-multiplier:       %s
to-wound-multiplier:     %s
save-multiplier:         %s
damage-multiplier:       %s
feel-no-pain-multiplier: %s
"
                  weapon-name
                  defender-name
                  attacks-multiplier
                  to-hit-multiplier
                  to-wound-multiplier
                  save-multiplier
                  damage-multiplier
                  feel-no-pain-multiplier
                  )
         ;; (message "processing row...")
         ;; (message "damage %s" damage)
         ;; (message "resist-damage %s" resist-damage)
         ;; (message "feel-no-pain %s" feel-no-pain)
         ;; (message "save: %s" (wh40k/ap-vs-save armor-penetration
         ;;                                       save
         ;;                                       invulnerable))
         ;; (message "computed save: %s" (wh40k/d6-from-plus (wh40k/ap-vs-save armor-penetration
         ;;                                                                    save
         ;;                                                                    invulnerable)))
         (list
          weapon-name
          defender-name
          (*
           attacks-multiplier
           to-hit-multiplier
           to-wound-multiplier
           save-multiplier
           damage-multiplier
           feel-no-pain-multiplier
           )
          )
         )
       )
     (-drop 1 weapon-profiles)
     )
    )
  )
(provide '40k-dps)
;;; 40k-dps.el ends here
