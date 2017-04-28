;; 隔離された環境にパッケージをインストール
; (例：パッケージを指定して起動)　emacs -q -l ~/path/to/somewhere/init.el
;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))



;;; base setting
(setq line-number-mode t) ;;カーソル行番号表示
(setq inhibit-startup-message t) ;;スタートアップメッセージ非表示
(setq make-backup-files nil) ;;バックアップファイルを作らない
(add-to-list 'backup-directory-alist (cons "." "~/share/emacs_backups/")) ;;バックアップファイル save dir
;; auto save設定
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/share/emacs_backups/") t)))
(setq frame-title-format "%f") ;;タイトルバーにフルパス表示
(auto-compression-mode t) ;;日本語info文字化け防止
(global-hl-line-mode) ;;行ハイライト
(show-paren-mode 1) ;;対応括弧ハイライト
(setq-default line-spacing 0) ;;行間設定
(setq-default tab-width 2 indent-tabs-mode nil) ;;; インデント時にタブを使わないでスペースを使う
(column-number-mode 1) ;;モードラインにカーソルがある位置の文字数を表示
(global-whitespace-mode 1)
(setq-default whitespace-style '(face tabs tab-mark)) ;; タブを可視化


;
; あらゆるmajor modeを使えるようにする
;
(require 'generic-x)

;;; 行番号を表示する
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d")

;
; el-get 設定
;
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;(el-get 'sync)


;;; Emacsのバージョンによってインストール先を変える
;(let ((versioned-dir (locate-user-emacs-file emacs-version)))
;  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
;	  package-user-dir (expand-file-name "elpa" versioned-dir)))

;(when (eq system-type 'darwin)
;    (setq ns-command-modifier (quote meta)))



;
; 利用パッケージ設定
;

; auto-complete
(el-get-bundle auto-complete)

; color theme
(el-get-bundle color-theme-railscasts)

; howm
(el-get-bundle howm)

; markdown-mode
(el-get-bundle markdown-mode)

; helm
(el-get-bundle helm)
(helm-mode 1)

; helm-ag
(el-get-bundle syohex/emacs-helm-ag)
(setq helm-ag-base-command "ag --nocolor --nogrou")
(global-set-key (kbd "C-c s") 'helm-ag)

; helm-c-yasnippet
(el-get-bundle helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")

; php-mode
(el-get-bundle php-mode)

; web-mode
(el-get-bundle web-mode)

; evil(mercurialのhgコマンドが必要)
(el-get-bundle evil)

;; rails development
; projectile
(el-get-bundle projectile)
(projectile-global-mode)
; projectile-rails
(el-get-bundle projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
; Rhtml-mode
(el-get-bundle rhtml-mode)

; flycheck ※el-getで入らないので、M-x pakage-install [Enter] flycheckなどで入れる
;(el-get-bundle melpa:flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

; swift-mode
; el-getで入らないので、M-x package-install [Enter] swift-modeなどで入れる
;(el-get-bundle melpa:swift-mode)

; emacs-jscs
;(el-get-bundle melpa:jscs)

(el-get-bundle codic)

; go-mode
(el-get-bundle go-mode)

;; js2-mode
(el-get-bundle mooz/js2-mode)
; jsxファイルに適用
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
(add-hook 'js2-jsx-mode-hook 'flycheck-mode)


;;;
;;; howm 設定
;;;
(setq howm-menu-lang 'ja)
;; howmメモの保存場所
(setq howm-directory "~/share/howm")
;; howm-mode
(when (require 'howm nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;;;
;;; markdown-mode
;;;
;(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.txt" . gfm-mode) auto-mode-alist))

;; ファイル内容を標準入力で渡すのではなく、ファイル名を引数として渡すように設定
;(defun markdown-custom ()
;  "markdown-mode-hook"
;  (setq markdown-command-needs-filename t)
;  )
;(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))


;;;
;;; php-mode
;;;
(add-hook 'php-mode-hook 'flycheck-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . php-mode))
(setq php-search-url "http://jp.php.net/ja/")
(setq php-manual-url "http://jp.php.net/manual/ja/")

;;;
;;; web-mode
;;;
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-engines-alist
        '(("php"    . "\\.ctp\\'"))
        )
  )
(add-hook 'web-mode-hook 'web-mode-hook)
  

;;;
;;; swift-mode flycheck用の設定
;;;
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'swift-mode-hook
  '(lambda()
     (add-to-list 'flycheck-checkers 'swift)
     (setq flycheck-swift-sdk-path
       (replace-regexp-in-string
        "\n+$" "" (shell-command-to-string
                   "xcrun --show-sdk-path --sdk macosx")))
  )
)

;;; 
;;; Flycheck eslint
;;;
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
  )
)


;;;
;;; GNU emacs用
;;;
(if window-system (progn

;;; font setting
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

(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))
))

;;
;; pathを通す
;;
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


;;
;; php-cs-fixerコマンドを叩ける用に設定
;; 参考：https://gist.github.com/shouhei/9005edbfa66470a3a592
;;
(defun php-cs-fixer ()
  (interactive)
  (setq filename (buffer-file-name (current-buffer)))
  (call-process "php-cs-fixer" nil nil nil "fix" filename "--fixers=-concat_without_spaces,-phpdoc_short_description,-pre_increment")
  (revert-buffer t t)
)

;; phpのファイルsave前にphp-cs-fixerを実行
;(add-to-list 'load-path (locate-user-emacs-file "el-get/php-cs-fixer"))
;(require 'php-cs-fixer)

;(add-hook 'before-save-hook 'php-cs-fixer-before-save)
