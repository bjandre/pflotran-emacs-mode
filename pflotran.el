;;; pflotran.el --- emacs mode for editing pflotran input files
;;
;;; Copyright (c) 2013 Ben Andre <bjandre@gmail.com>
;;
;; Author: Ben Andre
;; Version: 0.1
;; Keywords: pflotran
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;; Usage:
;;
;;  TAB : indent line
;;  M-TAB : keyword completion
;;
;;; Commentary:
;;
;;   * copy pflotran.el into ~/.emacs.d/
;;
;;   * edit your init.el and add: (load pflotran.el)
;;
;;   * PFloTran's ".in" suffix maybe too generic to be useful. You can
;; disable automatic pflotran mode by commenting the "auto-mode" lines
;; below, then enable pflotran mode manually by opening a pflotran
;; input file and typing:
;;
;;    M-x pflotran-mode
;;
;; Or by adding a mode line comment to the first line of your inputfile:
;;
;; : -*- mode: pflotran -*-
;;
;; Notes:
;;
;;   Inspired by:
;;     http://www.emacswiki.org/emacs/ModeTutorial
;;     http://www.emacswiki.org/emacs/DerivedMode
;;     http://ergoemacs.org/emacs/elisp_keyword_completion.html

;;; Code:

(defvar pflotran-mode-hook nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Open all ".in" files and pflotran input files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.in\\'" . pflotran-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Special key bindings for the mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pflotran-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for pflotran major mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define the pflotran sections and keywords for syntax highlighting
;; and indenting
;;
;; Notes:
;;
;;  * sections are keywords that also control indentation. keywords
;; don't effect indentation. we highlight both sections and keywords
;;
;;  * if a word is both a keyword and a section name, then the special
;;  section name rules need to go first (top level sections are the
;;  first word on a line)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar end-section '("END" "/"))

(defvar toplevel-keywords
  '("MODE"
    "CHECKPOINT"
    "RESTART"
    "WALLCLOCK_STOP"
    "PROC"
    "UNIFORM_VELOCITY"
    "REFERENCE_POROSITY"
    ))

(defvar toplevel-sections
  '("GRID"
    "CHEMISTRY"
    "REGION"
    "BOUNDARY_CONDITION"
    "INITIAL_CONDITION"
    "SOURCE_SINK"
    "FLOW_CONDITION"
    "TRANSPORT_CONDITION"
    "CONSTRAINT"
    "TIMESTEPPER"
    "TIME"
    "LINEAR_SOLVER"
    "NEWTON_SOLVER"
    "FLUID_PROPERTY"
    "SATURATION_FUNCTION"
    "MATERIAL_PROPERTY"
    "STRATA"
    "DATASET"
    "OUTPUT"
    "OBSERVATION"
    "DEBUG"
    ))

(defvar toplevel-all
  (append toplevel-keywords toplevel-sections end-section))

(defvar toplevel-sections-re "")

(loop for s in toplevel-sections do
      (setq toplevel-sections-re (concat toplevel-sections-re "^" s "\\|")))
;;
;; chemistry
;;
(defvar chemistry-keywords
  '("DATABASE"
    "LOG_FORMULATION"
    "ACTIVITY_COEFFICIENTS" "LAG" "NEWTON" "TIMESTEP" "NEWTON_ITERATION"
    "NO_CHECKPOINT_ACT_COEFS"
    "NO_BDOT"
    "UPDATE_POROSITY"
    "UPDATE_PERMEABILITY"
    "UPDATE_TORTUOSITY"
    "UPDATE_MINERAL_SURFACE_AREA"
    "ACTIVITY_H2O" "ACTIVITY_WATER"
    "MOLAL" "MOLARITY" "MOLALITY"
    "MAX_DLNC"
    "MAX_RELATIVE_CHANGE_TOLERANCE"
    "MAX_RESIDUAL_TOLERANCE"
    "REACTION" "FORWARD_RATE" "BACKWARD_RATE"
    "RATE_CONSTANT" "ACTIVIATION_ENERGY" "AFFINITY_THRESHOLD"
    "RATE_LIMITER" "IRREVERSIBLE" "SURFACE_AREA_POROSITY_POWER"
    "SURFACE_AREA_VOL_FRAC_POWER"
    "PREFACTOR_SPECIES" "ALPHA" "BETA" "ATTENUATION_COEF"
    "MINERAL" "CEC" "DISTRIBUTION_COEFFICIENT" "TYPE" "LINEAR"
    "LANGMUIR" "FREUNDLICH" "LANGMUIR_B" "FREUNDLICH_N"
    "EQUILIBRIUM" "MULTIRATE_KINETIC" "KINETIC" "COMPLEX_KINETICS"
    "MULTIRATE_SCALE_FACTOR" "COLLOID" "SITE"
    "OFF" "ALL" "GASES" "PH" "KD" "COLLOIDS" "TOTAL_SORBED_MOBILE" "TOTAL_SORBED"
    "FREE_ION" "SITE_DENSITY" "AGE"
    ))

(defvar chemistry-sections
  '("PRIMARY_SPECIES"
    "SECONDARY_SPECIES"
    "GAS_SPECIES"
    "GENERAL_REACTION"
    "MINERALS"
    "MINERAL_KINETICS" "PREFACTOR"
    "SORPTION"
    "ION_EXCHANGE_RXN" "CATIONS"
    "ISOTHERM_REACTIONS"
    "SURFACE_COMPLEXATION_RXN" "COMPLEXES" "SITE_FRACTION" "RATES"
    "REACTION_SANDBOX"
    "OUTPUT"
    ))

(defvar chemistry-all
  (append chemistry-keywords chemistry-sections))

;;
;; constraint
;;
(defvar constraint-keywords
  '(""
    ))

(defvar constraint-sections
  '("CONCENTRATIONS"
    "MINERALS"
    ))

(defvar constraint-all
  (append constraint-keywords constraint-sections))

;;
;; fluid properties
;;
(defvar fluid-property-keywords
  '("DIFFUSION_COEFFICIENT"
    ))

(defvar fluid-property-sections
  '(""
    ))

(defvar fluid-property-all
  (append fluid-property-keywords fluid-property-sections))


;;
;; grid
;;
(defvar grid-keywords
  '("TYPE"
    "NXYZ"
    "GRAVITY"
    "ORIGIN"
    "INVERT_Z"
    "FILENAME"))

(defvar grid-sections
  '("BOUNDS"
    "DXYZ"))

(defvar grid-all
  (append grid-keywords grid-sections))

;;
;; initial/boundary conditions
;;
(defvar ic-bc-keywords
  '("TRANSPORT_CONDITION"
    "FLOW_CONDITION"
    "REGION"
    ))

(defvar ic-bc-sections
  '(""
    ))

(defvar ic-bc-all
  (append ic-bc-keywords ic-bc-sections))

;;
;; material properties
;;
(defvar material-property-keywords
  '("ID"
    "NAME"
    "POROSITY"
    "POROSITY DATASET"
    "ANISOTROPIC"
    "ISOTROPIC"
    "PERM_ISO"
    "PERM_X"
    "PERM_Y"
    "PERM_Z"
    "VERTICAL_ANISOTROPY_FACTOR"
    "DATASET"
    "PERMEABILITY_POWER"
    "TORTUOSITY"
    "TORTUOSITY_POWER"
    "SATURATION_FUNCTION"
    "ROCK_DENSITY"
    "SPECIFIC_HEAT"
    "LONGITUDINAL_DISPERSIVITY"
    "THERMAL_CONDUCTIVITY_DRY"
    "THERMAL_CONDUCTIVITY_WET"
    "PORE_COMPRESSIBILITY"
    "THERMAL_EXPANSITIVITY"
    ))

(defvar material-property-sections
  '("PERMEABILITY"
    ))

(defvar material-property-all
  (append material-property-keywords material-property-sections))

;;
;; observation
;;
(defvar observation-keywords
  '("REGION"
    "VELOCITY"
    "AT_CELL_CENTER"
    ))

(defvar observation-sections
  '(""
    ))

(defvar observation-all
  (append observation-keywords observation-sections))

;;
;; output
;;
(defvar output-keywords
  '("NO_PRINT_INITIAL"
    "NO_PRINT_FINAL"
    "SCREEN PERIODIC"
    "PERIODIC_OBSERVATION"
    "PERIODIC"
    "TIME"
    "TIMESTEP"
    "FORMAT"
    "MASS_BALANCE"
    "FORMAT"
    "TECPLOT"
    "HDF5"
    "MULTIPLE_FILES"
    "POINT"
    "PROCESSOR_ID"
    ))

(defvar output-sections
  '(""
    ))

(defvar output-all
  (append output-keywords output-sections))

;;
;; region
;;
(defvar region-keywords
  '("COORDINATE"
    "BLOCK"
    "FILE"
    "FACE"
    ))

(defvar region-sections
  '("COORDINATES"
    ))

(defvar region-all
  (append region-keywords region-sections))

;;
;; saturation function
;;
(defvar saturation-function-keywords
  '("SATURATION_FUNCTION_TYPE"
    "VAN_GENUCHTEN"
    "RESIDUAL_SATURATION"
    "LAMBDA"
    "ALPHA"
    ))

(defvar saturation-function-sections
  '(""
    ))

(defvar saturation-function-all
  (append saturation-function-keywords saturation-function-sections))

;;
;; newton and linear solver
;;
(defvar solver-keywords
  '("PRECONDITIONER_MATRIX_TYPE"
    "AIJ"
    "RTOL"
    "ATOL"
    "STOL"
    "NO_INFINITY_NORM"
    "NO_PRINT_CONVERGENCE"
    "PRINT_DETAILED_CONVERGENCE"
    "SOLVER DIRECT"
    ))

(defvar solver-sections
  '(""
    ))

(defvar solver-all
  (append solver-keywords solver-sections))

;;
;; strata
;;

(defvar strata-keywords
  '("REGION"
    "MATERIAL"
    ))

(defvar strata-sections
  '(""
    ))

(defvar strata-all
  (append strata-keywords strata-sections))

;;
;; time
;;
(defvar time-keywords
  '("FINAL_TIME"
    "INITIAL_TIMESTEP_SIZE"
    "MAXIMUM_TIMESTEP_SIZE"
    "STEADY_STATE"))

(defvar time-sections
  '(""
    ))

(defvar time-all
  (append time-keywords time-sections))

;;
;; time stepper
;;
(defvar timestepper-keywords
  '("FLOW"
    "TRANSPORT"
    "NUM_STEPS_AFTER_CUT"
    "MAX_STEPS"
    "TS_ACCELERATION"
    "MAX_TS_CUTS"
    "CFL_LIMITER"
    "DT_FACTOR"
    "INITIALIZE_TO_STEADY_STATE"
    "RUN_AS_STEADY_STATE"
    "MAX_PRESSURE_CHANGE"
    "MAX_TEMPERATURE_CHANGE"
    "MAKE_CONCENTRATION_CHANGE"
    "MAX_SATURATION_CHANGE"
    ))

(defvar timestepper-sections
  '(""
    ))

(defvar timestepper-all
  (append timestepper-keywords timestepper-sections))

;;
;; transport conditions
;;
(defvar transport-condition-keywords
  '("TYPE"
    "dirchlet" "dirchlet_zero_gradient" "zero_gradient" "neumann" "equilibrium"
    ))

(defvar transport-condition-sections
  '("CONSTRAINT_LIST"
    ))

(defvar transport-condition-all
  (append transport-condition-keywords transport-condition-sections))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create some combined sections and regular expressions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar pflotran-all
  (append toplevel-all
          chemistry-all
          constraint-all
          fluid-property-all
          grid-all
          ic-bc-all
          material-property-all
          observation-all
          output-all
          region-all
          saturation-function-all
          solver-all
          strata-all
          time-all
          timestepper-all
          transport-condition-all
          ))

(defvar pflotran-all-re
  (regexp-opt pflotran-all 'words))

(defvar pflotran-keywords
  (append
   toplevel-keywords
   chemistry-keywords
   constraint-keywords
   fluid-property-keywords
   grid-keywords
   ic-bc-keywords
   material-property-keywords
   observation-keywords
   output-keywords
   region-keywords
   saturation-function-keywords
   solver-keywords
   strata-keywords
   time-keywords
   timestepper-keywords
   transport-condition-keywords
   ))

(defvar pflotran-keywords-re
  (regexp-opt pflotran-keywords 'words))

(defvar pflotran-sections-re
  (concat toplevel-sections-re
          (regexp-opt (append
                       end-section
                       chemistry-sections
                       constraint-sections
                       fluid-property-sections
                       grid-sections
                       ic-bc-sections
                       material-property-sections
                       observation-sections
                       output-sections
                       region-sections
                       saturation-function-sections
                       solver-sections
                       strata-sections
                       time-sections
                       timestepper-sections
                       transport-condition-sections
                       ))))

(defvar pflotran-sections
  (append
   toplevel-sections
   chemistry-sections
   constraint-sections
   fluid-property-sections
   grid-sections
   ic-bc-sections
   material-property-sections
   observation-sections
   output-sections
   region-sections
   saturation-function-sections
   solver-sections
   strata-sections
   time-sections
   timestepper-sections
   transport-condition-sections
   ))

(setq pflotran-sections (delete "" pflotran-sections))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Assign keywords to a font lock for syntax highlighting
;;
;; Notes:
;;
;;   * font-lock for sections and keywords go first and species names
;; go last so that the generic regexp for species names doesn't also
;; catch the keywords.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar integer-re "[ \t]+[0-9]+\\>")
(defvar float-re "\\<[-+]?[0-9]+\.\\([0-9]+\\)?[ed]?[-+]?[0-9]+\\>")
(defvar species-name-re ">?[A-Z][A-Za-z0-9\(\)_]*[-+]*")

;;font-lock-builtin-face  font-lock-keyword-face
(defvar pflotran-font-lock-defaults
  `((
     ( , pflotran-sections-re . font-lock-type-face)
     ( , pflotran-keywords-re . font-lock-keyword-face)
;;     ( , pflotran-all-re . font-lock-keyword-face)
     ( , integer-re . font-lock-constant-face)
     ( , float-re . font-lock-constant-face)
     ( , species-name-re . font-lock-function-name-face)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create per-line indenting rules
;;
;; Rules based on : http://emacswiki.org/emacs/ModeTutorial
;;
;; Indentation Rules:
;;
;;   1) start with indent = 0 at the begining of the buffer
;;
;;   2) if we are at an end of section marker, move indent back
;;
;;   3) if the previous line was an end of section, use the same indentation level
;;
;;   4) if the previous line was a section, then increase indentation level
;;
;;   5) if the previous line is a keyword, use the same indent
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar pflotran-tab-width 2)

(defvar end-section-re
  (concat "^[ \t]*" (regexp-opt end-section)))

(defvar sections-re
  "^[ \t]*")

(setq sections-re (concat sections-re (regexp-opt pflotran-sections)))

(defvar keywords-re
  "^[ \t]*")

(setq keywords-re (concat keywords-re (regexp-opt pflotran-keywords)))

(defvar debug-indent nil)

(defun pflotran-indent-line ()
  "Indent current line in pflotran input."
  (interactive)
  (beginning-of-line)
  (if (bobp) ; rule 1: begining of buffer (line)
      (progn
        (if debug-indent (print "indent rule 1 (begining of buffer)"))
        (indent-line-to 0))
    (let ((not-indented t) cur-indent)
      (if (looking-at end-section-re) ; rule 2: end section
          (progn
            (if debug-indent (print "indent rule 2 (current end)"))
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) pflotran-tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at end-section-re) ; rule 3
                (progn
                  (if debug-indent (print "indent rule 3 (previous end)"))
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[ \t]*$")
                  (progn
                    ;; can't tell what to do from a blank line, go back further
                    (if debug-indent (print "indent blank line"))
                    )
                (if (looking-at sections-re) ; rule 4
                    (progn
                      (if debug-indent (print "indent rule 4 (section)"))
                      (setq cur-indent (+ (current-indentation) pflotran-tab-width))
                      (setq not-indented nil))
                  (if (looking-at keywords-re)
                      (progn ; rule 5
                        (if debug-indent (print "indent rule 5 (keywords)"))
                        (setq not-indented nil)
                        (setq cur-indent (current-indentation))
                        )
                    (if (bobp)
                        (progn
                          ;; all the way back to the beginning of buffer, bail
                          (if debug-indent (print "indent returned to bobp"))
                          (setq not-indented nil))
                      ))))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyword completion
;;
;; Based on : http://ergoemacs.org/emacs/elisp_keyword_completion.html
;;
;; Algorithm notes:
;;
;;   1) when the begining of the keyword is empty, set it to empty
;;   string. try-completion will then show all keywords.
;;
;;   2) store possible completions in maxMaxResult
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pflotran-completions ()
  "Keyword completion for the word behind the cursor."
  (interactive)
  (let ((end-point (point))
        (begin-keyword (thing-at-point 'symbol))
        match-result)

    (when (not begin-keyword) (setq begin-keyword ""))  ;; note 1
    (setq match-result (try-completion begin-keyword pflotran-all))

    (cond ((eq match-result t))  ;; already a full keyword w/o another completion
          ((null match-result)  ;; no match
           (message "No completion for '%s'" begin-keyword)
           (ding))
          ((not (string= begin-keyword match-result)) ;; single match, use it
           (delete-region (- end-point (length begin-keyword)) end-point) ;; subtract the alreayd typed region
           (insert match-result))
          (t (message "Making completion list...")
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list
                (all-completions begin-keyword pflotran-all)
                begin-keyword))
             (message "Making completion list...%s" "done")))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Derive pflotran mode from fundamental mode, adding our custom
;; syntax and indenting rules
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode pflotran-mode fundamental-mode "PFloTran"
  "Major mode for editing pflotran input files"
  (setq font-lock-defaults pflotran-font-lock-defaults)
  (set (make-local-variable 'indent-line-function) 'pflotran-indent-line)
  (when pflotran-tab-width
    (setq tab-width pflotran-tab-width))
  (setq-default indent-tabs-mode nil)
  (global-set-key (kbd "M-TAB") 'pflotran-completions)
  ;; comment syntax
  ;(setq comment-start "skip")
  ;(setq comment-end "noskip")
  (modify-syntax-entry ?: "<" pflotran-mode-syntax-table)
  (modify-syntax-entry ?! "<" pflotran-mode-syntax-table)
  (modify-syntax-entry ?\n ">" pflotran-mode-syntax-table)
  ;; make some symbols word characters so they can be used in keywords
  ;; and species names
  (modify-syntax-entry ?_ "w" pflotran-mode-syntax-table)
  (modify-syntax-entry ?/ "w" pflotran-mode-syntax-table)
  (modify-syntax-entry ?+ "w" pflotran-mode-syntax-table)
  (modify-syntax-entry ?- "w" pflotran-mode-syntax-table)
  (modify-syntax-entry ?. "w" pflotran-mode-syntax-table)
  )

(provide 'pflotran-mode)

;;; pflotran.el ends here
