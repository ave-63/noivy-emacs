;; These functions all expect you to have the region surrounding
;; a sympy matrix, of the form:
;;
;; Matrix([[1, 2],
;; [3, 4]])
;;
;; They will then copy to the clipboard the latex code for either a matrix,
;; or a system of equations, or a set of vectors.
;;
;; This was all written by claude 4 opus, summer 2025.


;; Helper functions:

(defun mf--parse-matrix-from-region ()
  "Parse a SymPy matrix from the current region and return a list of row lists.
Returns nil if no valid matrix is found."
  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (clean-text (replace-regexp-in-string "\\s-+" " " region-text))
         (matrix-match (string-match "Matrix(\\[\\(.*\\)\\])" clean-text)))
    (when matrix-match
      (let* ((matrix-content (match-string 1 clean-text))
             (rows (split-string matrix-content "\\],\\s-*\\[" t)))
        (mapcar (lambda (row)
                  (mapcar 'string-trim
                          (split-string (replace-regexp-in-string "\\[\\|\\]" "" row)
                                ",\\s-*" t)))
                rows)))))

(defun mf--copy-to-clipboard (text success-message)
  "Copy TEXT to clipboard and show SUCCESS-MESSAGE."
  (kill-new text)
  (message success-message))

(defun mf--transpose-matrix (matrix)
  "Transpose a matrix represented as a list of row lists."
  (let ((num-cols (length (car matrix))))
    (cl-loop for col from 0 below num-cols
             collect (cl-loop for row in matrix
                              collect (nth col row)))))

(defun mf--format-equation-term (coef var-index is-first-term)
  "Format a single term in an equation given COEF and VAR-INDEX.
IS-FIRST-TERM determines whether to include a leading sign."
  (let ((var-name (format "x_%d" (1+ var-index)))
        (coef-str (string-trim coef)))
    (cond
     ((or (string= coef-str "0") (string= coef-str "0.0")) nil)
     ((string= coef-str "1")
      (if is-first-term var-name (concat "+ " var-name)))
     ((string= coef-str "-1")
      (concat (if is-first-term "-" "- ") var-name))
     ((string-match "^-" coef-str)
      (if is-first-term
          (format "%s%s" coef-str var-name)
        (format "- %s%s" (substring coef-str 1) var-name)))
     (t
      (if is-first-term
          (format "%s%s" coef-str var-name)
        (format "+ %s%s" coef-str var-name))))))

;; Main functions

(defun mf-matrix-to-latex-matrix ()
  "Convert a SymPy matrix in the region to LaTeX bmatrix format and copy to clipboard."
  (interactive)
  (let ((matrix (mf--parse-matrix-from-region)))
    (if matrix
        (let* ((latex-rows (mapcar (lambda (row)
                                     (mapconcat 'identity row " & "))
                                   matrix))
               (latex-matrix (concat "\\begin{bmatrix}\n"
                                     (mapconcat 'identity latex-rows " \\\\\n")
                                     "\n\\end{bmatrix}")))
          (mf--copy-to-clipboard latex-matrix "LaTeX matrix copied to clipboard!"))
      (message "No valid SymPy matrix found in region"))))

(defun mf-matrix-to-latex-vectors ()
  "Convert a SymPy matrix to LaTeX vector list using bbvec/bbbvec commands."
  (interactive)
  (let ((matrix (mf--parse-matrix-from-region)))
    (if matrix
        (let ((num-rows (length matrix)))
          (cond
           ((= num-rows 2)
            (let* ((columns (mf--transpose-matrix matrix))
                   (vectors (mapcar (lambda (col)
                                      (format "\\bbvec{%s}{%s}"
                                              (nth 0 col) (nth 1 col)))
                                    columns)))
              (mf--copy-to-clipboard 
               (format "\\left\\{ %s \\right\\}" (mapconcat 'identity vectors ", "))
               "LaTeX 2D vectors copied to clipboard!")))
           ((= num-rows 3)
            (let* ((columns (mf--transpose-matrix matrix))
                   (vectors (mapcar (lambda (col)
                                      (format "\\bbbvec{%s}{%s}{%s}"
                                              (nth 0 col) (nth 1 col) (nth 2 col)))
                                    columns)))
              (mf--copy-to-clipboard 
               (format "\\left\\{ %s \\right\\}" (mapconcat 'identity vectors ", "))
               "LaTeX 3D vectors copied to clipboard!")))
           (t
            (message "Matrix must have exactly 2 or 3 rows for vector conversion"))))
      (message "No valid SymPy matrix found in region"))))

(defun mf-matrix-to-system-of-eqns ()
  "Convert a SymPy augmented matrix to a LaTeX system of equations."
  (interactive)
  (let ((matrix (mf--parse-matrix-from-region)))
    (if matrix
        (let ((num-cols (length (car matrix))))
          (if (>= num-cols 2)
              (let* ((equations
                     (mapcar (lambda (row)
                               (let ((terms '())
                                     (rhs (car (last row))))
                                 ;; Process coefficients (all but last column)
                                 (cl-loop for i from 0 below (- num-cols 1)
                                          for coef = (nth i row)
                                          for term = (mf--format-equation-term 
                                                      coef i (null terms))
                                          when term do (push term terms))
                                 ;; Build equation
                                 (if terms
                                     (format "%s &= %s"
                                             (mapconcat 'identity (reverse terms) " ")
                                             (string-trim rhs))
                                   (format "0 &= %s" (string-trim rhs)))))
                             matrix))
                    (latex-system (concat "\\begin{aligned}[t]\n"
                                          (mapconcat 'identity equations " \\\\\n")
                                          "\n\\end{aligned}")))
                (mf--copy-to-clipboard latex-system 
                                         "LaTeX system of equations copied to clipboard!"))
            (message "Matrix must have at least 2 columns for equation conversion")))
      (message "No valid SymPy matrix found in region"))))
