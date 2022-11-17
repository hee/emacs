;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
	           (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf helm :ensure t)
(helm-mode 1)

(el-get-bundle syohex/emacs-helm-ag)
                                        ;(setq helm-ag-base-command "ag --nocolor --nogrou")
(global-set-key (kbd "C-c s") 'helm-ag)

(leaf magit :ensure t)

(leaf howm
  :ensure t
  :custom ((howmmenu-lang . 'ja)
	         (howm-directory . "~/share/howm")))
(when (require 'howm nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;;; markdown-mode
(leaf markdown-mode :ensure t)
(setq auto-mode-alist (cons '("\\.txt" . gfm-mode) auto-mode-alist))

(leaf js2-mode
  :ensure t
  :mode "\\.js\\'")

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
  :global-minor-mode global-flycheck-mode)
;;; Flycheck eslint
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
    )
  )

; javascript
(leaf add-node-modules-path :ensure t)
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook 'add-node-modules-path))

(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
;;; https://stackoverflow.com/questions/35162106/how-to-disable-js2-mode-syntax-checking-globally-in-spacemacs
(defvar js2-mode-show-parse-errors nil)
(defvar js2-mode-show-strict-warnings nil)

;; インデントの関数の再設定
(add-hook 'js2-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level);'indent-line-function)
            (setq js-indent-level 2)))

(defun web-mode-indent (num)
  (interactive "nIndent: ")
  (setq web-mode-markup-indent-offset num)
  (setq web-mode-css-indent-offset num)
  (setq web-mode-style-padding num)
  (setq web-mode-code-indent-offset num)
  (setq web-mode-script-padding num)
  (setq web-mode-block-padding num)
  (setq web-mode-attr-indent-offset num)
  (setq web-mode-comment-style num)
  )
(web-mode-indent 2)

; typescript
; mmm-mode
; [EmacsにおけるTypescript + React JSXの苦悩と良さげな設定について](https://qiita.com/nuy/items/ebcb25ad14f02ab72790)
(leaf typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook
          (lambda ()
            (interactive)
            (mmm-mode)
            )))

(leaf mmm-mode
  :ensure t
  :commands mmm-mode
  :mode (("\\.tsx\\'" . typescript-mode))
  :config
  (setq mmm-global-mode t)
  (setq mmm-submode-decoration-level 0)
  (mmm-add-classes
   '((mmm-jsx-mode
      :submode web-mode
      :face mmm-code-submode-face
      :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
      :front-offset -1
      :back ">\n?\s*)\n}\n"
      :back-offset 1
      )))
  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)


  (defun mmm-reapply ()
    (mmm-mode)
    (mmm-mode))

  (add-hook 'after-save-hook
            (lambda ()
              (when (string-match-p "\\.tsx?" buffer-file-name)
                (mmm-reapply)
                )))
  )

; flutter
(leaf dart-mode
  :ensure t
  :custom ((dart-format-on-save . t)
           (dart-sdk-path . "~/.local/flutter/bin/cache/dart-sdk/")))

(leaf lsp-mode
  :ensure t)
(leaf lsp-dart
  :ensure t
  :hook (dart-mode . lsp))
(leaf projectile
  :ensure t)
(leaf lsp-ui
  :ensure t)
(leaf company
  :ensure t)

; terraform
(leaf terraform-mode
  :ensure t
  :mode "\\.tf\\'")

;; あらゆるmajor modeを使えるようにする
(require 'generic-x)


(provide 'init)

;;; 行番号を表示する
(if (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;;行ハイライト
(global-hl-line-mode)
;;日本語info文字化け防止
(auto-compression-mode t)
(setq-default line-spacing 0) ;;行間設定
(setq-default tab-width 2 indent-tabs-mode nil) ;;; インデント時にタブを使わないでスペースを使う

;; pathを通す
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              "/usr/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              (expand-file-name "~/.composer/vendor/bin")
              ))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;バックアップファイル save dir
(add-to-list 'backup-directory-alist (cons "." "~/share/emacs_backups/"))
;; auto save設定
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/share/emacs_backups/") t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
 '(frame-title-format "%f" t)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(make-backup-files nil)
 '(package-selected-packages
   '(jsx-mode terraform-mode company lsp-ui projectile lsp-dart lsp-mode dart-mode web-mode php-mode markdown-mode magit macrostep leaf-tree leaf-convert js2-mode hydra howm helm-c-yasnippet helm-ag flycheck evil el-get blackout add-node-modules-path))
 '(show-paren-mode 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Ricty" :height 140))))
 '(cursor ((t (:background "#F8F8F2" :foreground "#272822"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "#75715E"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#A6E22E"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#F92672"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "#66d9ef"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "#E6DB74"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "#66d9ef"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#FD971F"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "#49483E"))))
 '(show-paren-match ((((class color) (background dark)) (:background "#3E3D32"))))
 '(variable-pitch ((t (:family "DejaVu Sans")))))

(put 'downcase-region 'disabled nil)

;; Emacs で全角スペース/タブ文字を可視化
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

