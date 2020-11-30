(defun mode-line-render (left right)
  "Return a string of `window-width' length containing left, and
   right aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

(setq-default
 mode-line-format
 '(:eval
   (mode-line-render

    (format-mode-line
     (list
      (propertize " " 'face `(:weight regular))
      "%b "
      '(:eval (if (and buffer-file-name (buffer-modified-p))
                  "(modified)"))))

    (format-mode-line
     (list
      '(:eval (if vc-mode
                  (concat vc-mode "  ")))
      (propertize "%3l:%2c "
	              'face `(:weight light)))))))
