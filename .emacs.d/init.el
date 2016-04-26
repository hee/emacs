;; 隔離された環境にパッケージをインストール
; (例：パッケージを指定して起動)　emacs -q -l ~/path/to/somewhere/init.el
;
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

; flycheck
(el-get-bundle melpa:flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

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
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . php-mode))
(setq php-search-url "http://jp.php.net/ja/")
(setq php-manual-url "http://jp.php.net/manual/ja/")


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
