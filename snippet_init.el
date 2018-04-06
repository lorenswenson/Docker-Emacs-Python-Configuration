;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

(defvar myPackages
  '(;;better-defaults
    ;; material-theme
    elpy
    auto-complete
    flycheck
    py-autopep8
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; (elpy-use-ipython)

(setq elpy-rpc-python-command "python3.6")
(setq python-shell-interpreter "~/dev/Docker-Emacs-Python-Configuration/docker_ipython.sh")

;; (setq python-shell-interpreter-interactive-arg "")

(setq python-shell-completion-native-enable nil)

;; Suppress warning: Warning (emacs): Python shell prompts cannot be detected.
;; https://github.com/jorgenschaefer/elpy/issues/733
(setq python-shell-prompt-detect-enabled nil)
;; (setq python-shell-prompt-detect-failure-warning nil)

(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

(eval-after-load "elpy"
  '(progn
     (define-key elpy-mode-map (kbd "s-<left>") 'elpy-nav-indent-shift-left)
     (define-key elpy-mode-map (kbd "s-<right>") 'elpy-nav-indent-shift-right)
     (define-key elpy-mode-map (kbd "s-<up>") 'elpy-nav-indent-shift-left)
     (define-key elpy-mode-map (kbd "s-<down>") 'elpy-nav-indent-shift-left)))

;; comint allows us to clear interactive buffers like ipython & eshell
(require 'comint)
;;(define-key comint-mode-map (kbd "M-") 'comint-next-input)
;;(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)
(define-key comint-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)

(defun clear-comint-buffer ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)
    (goto-char (point-max))))

(define-key inferior-python-mode-map (kbd "C-c M-o") 'clear-comint-buffer)
