;; to convert older lexdb .rev files to 4.0 spec
;; works by removing fourth field of dump file

(defun remove-orthkey-field nil
  (interactive)
  (replace-regexp "^\\([^\t]*\t[^\t]*\t[^\t]*\t\\)[^\t]*\t" "\\1"))