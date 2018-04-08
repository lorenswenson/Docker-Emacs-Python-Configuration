(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'default-frame-alist '(background-color . "grey10"))
                                        ;(set-background-color "grey10" )


(global-set-key '[M-up] 'windmove-up)
(global-set-key '[M-down] 'windmove-down)
(global-set-key '[M-left] 'windmove-left)
(global-set-key '[M-right] 'windmove-right)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setf parse-sexp-ignore-comments t)
(setq transient-mark-mode t)
(show-paren-mode 1)
(setq use-dialog-box nil)
(line-number-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq scroll-step 1)
(setq scroll-conservatively 1)
(setq highlight-nonselected-windows nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(ido-mode t)
(setq-default fill-column 79)

(global-set-key (kbd "s-e") 'remove-outer-s-expression)
(global-set-key (kbd "s-E") 'undo-remove-outer-s-expression)

;; Add MELPA to get SLIME, STABLE VERSION
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(unless (package-installed-p company) (package-install company))
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
